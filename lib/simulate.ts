import type { Amount, Box, BoxCandidate, EIP12UnsignedTransaction } from "@fleet-sdk/common";
import { FEE_CONTRACT } from "@fleet-sdk/core";
import { MockChain } from "@fleet-sdk/mock-chain";
import { SLong, estimateBoxSize } from "@fleet-sdk/serializer";
import { ONE, snapshot } from "../sdk/src/math.ts";
import { oraclePrice, readReactor } from "../sdk/src/reactor.ts";
import { type TxContext, adjustPegTx, betaDecayMinusTx, betaDecayPlusTx, fissionTx, fusionTx } from "../sdk/src/transactions.ts";

/**
 * Problems a node would reject but MockChain does not check: ERG must be preserved, a miner fee paid, every
 * output must hold the minimum value for its size, and creation heights must be plausible. `height` is the
 * current chain height; `minValuePerByte` is the blockchain parameter (360 nanoERG on mainnet).
 */
export function consensusIssues(tx: EIP12UnsignedTransaction, height: number, minValuePerByte = 360n): string[] {
  const issues: string[] = [];
  const total = (boxes: { value: Amount }[]) => boxes.reduce((sum, b) => sum + BigInt(b.value), 0n);
  if (total(tx.inputs) !== total(tx.outputs)) issues.push("ERG is not preserved");
  if (!tx.outputs.some((o) => o.ergoTree === FEE_CONTRACT)) issues.push("no miner-fee output");
  const newestInput = Math.max(...tx.inputs.map((i) => i.creationHeight));
  for (const o of tx.outputs) {
    const minimum = BigInt(estimateBoxSize(o)) * minValuePerByte;
    if (BigInt(o.value) < minimum) issues.push(`output below the minimum value of ${minimum}: ${o.value}`);
    if (o.creationHeight > height + 1) issues.push("output created in the future");
    if (o.creationHeight < newestInput) issues.push("output created before one of the inputs");
  }
  return issues;
}

/**
 * Exercises a reactor box before it is created for real. On a local mock chain holding that box and copies
 * of the real oracle and buyback boxes, a fresh wallet performs a fission, both beta decays, a fusion and,
 * after the oracle price moves out of the healthy range, an Adjust Peg. A reactor box with malformed
 * registers could never be spent, so this must succeed before the bootstrap is submitted.
 */
export function simulateReactor(
  reactor: BoxCandidate<Amount>,
  oracle: Box<Amount>,
  buyback: Box<Amount>,
  minFee: bigint,
  height: number,
): void {
  const chain = new MockChain({ height });
  const copy = (box: BoxCandidate<Amount>, registers = box.additionalRegisters) => ({
    value: box.value,
    ergoTree: box.ergoTree,
    assets: box.assets,
    additionalRegisters: registers,
    creationHeight: chain.height,
  });
  const user = chain.newParty("user");
  user.addBalance({ nanoergs: 100n * ONE });
  const reactorParty = chain.addParty(reactor.ergoTree, "reactor");
  reactorParty.addUTxOs(copy(reactor));
  const oracleParty = chain.addParty(oracle.ergoTree, "oracle");
  oracleParty.addUTxOs(copy(oracle));
  const buybackParty = chain.addParty(buyback.ergoTree, "buyback");
  buybackParty.addUTxOs(copy(buyback));

  const [, neutron, proton] = reactor.assets.map((a) => a.tokenId);
  const holding = (tokenId: string) =>
    user.utxos.toArray().flatMap((b) => b.assets).filter((a) => a.tokenId === tokenId).reduce((s, a) => s + a.amount, 0n);
  const ctx = (overrides: Partial<TxContext> = {}): TxContext => ({
    height: chain.height,
    minFee,
    reactor: reactorParty.utxos.at(0),
    oracle: oracleParty.utxos.at(0),
    inputs: user.utxos.toArray(),
    recipient: user.ergoTree,
    buyback: buybackParty.utxos.at(0),
    ...overrides,
  });
  const run = (label: string, build: () => ReturnType<typeof fissionTx>) => {
    try {
      chain.execute(build(), { signers: [user], throw: true });
    } catch (e) {
      throw new Error(`the new reactor would reject a ${label}: ${e instanceof Error ? e.message : e}`);
    }
  };

  run("fission", () => fissionTx(ctx(), ONE));
  run("beta decay +", () => betaDecayPlusTx(ctx(), holding(proton) / 4n));
  run("beta decay −", () => betaDecayMinusTx(ctx(), holding(neutron) / 4n));
  run("fusion", () => fusionTx(ctx(), ONE / 10n));

  // An oracle price that takes the fusion ratio to 1.2, out of the healthy range, so the peg must be adjusted.
  const price = oraclePrice(oracle);
  const { q } = snapshot(readReactor(reactorParty.utxos.at(0)).state, minFee, price);
  const higher = SLong((price * 12n * ONE) / (10n * q)).toHex();
  oracleParty.addUTxOs(copy(oracle, { ...oracle.additionalRegisters, R4: higher }));
  run("peg adjustment", () => adjustPegTx(ctx({ oracle: oracleParty.utxos.at(1) })));
}
