import { ErgoAddress } from "@fleet-sdk/core";
import { mockUTxO } from "@fleet-sdk/mock-chain";
import { SLong, parse } from "@fleet-sdk/serializer";
import { describe, expect, it } from "vitest";
import { bootstrapTransactions, deploymentRecord, planBootstrap, publicKeyOf } from "../lib/bootstrap.ts";
import { CONTRACT_SOURCE, compilerVersion } from "../lib/compile.ts";
import { type DeploymentConfig, bootstrapParameters, contractConstants, readDeployment } from "../lib/config.ts";
import { ONE, adjustedPrice, isHealthy, snapshot } from "../sdk/src/math.ts";
import { oraclePrice, readReactor } from "../sdk/src/reactor.ts";
import { renderDeployments, reproduce } from "../lib/records.ts";
import { simulateReactor } from "../lib/simulate.ts";
import { adjustPegTx, betaDecayMinusTx, betaDecayPlusTx, fissionTx, fusionTx } from "../sdk/src/transactions.ts";
import {
  BUYBACK_TREE,
  DOLLAR,
  GOLD,
  MIN_FEE,
  type Profile,
  type Tx,
  type World,
  expectAccepted,
  expectRejected,
  mutate,
  reactorTree,
  setup,
} from "./helpers.ts";

const shipped = { Gold: "../config/gold.json", Dollar: "../config/dollar.json" } as const;

// MockChain gives the outputs of executed transactions random box ids, so after the four chained bootstrap
// transactions (whose ids Fleet precomputes, as the node will) the deployer's party still lists the minted
// boxes. Follow-up transactions therefore use a fresh party.
function trader(w: World) {
  const party = w.chain.newParty("trader");
  party.addBalance({ nanoergs: 1_000n * ONE });
  return { party, ctx: (overrides = {}) => w.ctx({ inputs: party.utxos.toArray(), recipient: party.ergoTree, ...overrides }) };
}
const utf8 = (hex: string) => Buffer.from(parse<Uint8Array>(hex)).toString("utf8");

/** The shipped config of a profile, completed with test values for the fields that are still TODO. */
function testConfig(profile: Profile, w: World, seedErg = "10"): DeploymentConfig {
  const c = readDeployment(new URL(shipped[profile.name as keyof typeof shipped], import.meta.url));
  c.oracle.poolNft = profile.poolNft;
  c.oracle.buybackNft = profile.buybackNft;
  c.oracle.buybackContractAddress = ErgoAddress.fromErgoTree(BUYBACK_TREE).encode();
  c.treasuryAddress = w.treasury.address.encode();
  c.bootstrap.seedErg = seedErg;
  c.tokens.nft = { ticker: "TNFT", name: "Test Reactor NFT", description: "Identifies the test reactor box." };
  c.tokens.neutron = { ticker: "TNEU", name: "Test Neutron", description: "Pegged test token." };
  c.tokens.proton = { ticker: "TPRO", name: "Test Proton", description: "Leveraged test token." };
  return c;
}

function bootstrap(profile: Profile, seedErg?: string) {
  const w = setup(profile, { withoutReactor: true });
  const config = testConfig(profile, w, seedErg);
  const tree = reactorTree(contractConstants(config)).toHex();
  const price = oraclePrice(w.oracle.utxos.at(0));
  const plan = planBootstrap(bootstrapParameters(config), MIN_FEE, price, publicKeyOf(config.treasuryAddress), w.chain.height);
  const txs = bootstrapTransactions(config, plan, tree, w.user.utxos.toArray(), w.user.address.encode(), w.chain.height);
  return { w, config, plan, txs, tree };
}

describe.each([GOLD, DOLLAR])("Bootstrap ($name)", (profile) => {
  it("starts the reactor at q = 0.6, with protons worth 1 ERG and neutrons at the peg (N4)", () => {
    const { plan } = bootstrap(profile);
    const snap = snapshot(plan.state, MIN_FEE, profile.oraclePrice);
    expect(plan.price).toBe(adjustedPrice(profile.oraclePrice, profile.pegFactor));
    expect(Number(snap.q) / 1e9).toBeCloseTo(0.6, 6);
    expect(Number(((ONE - snap.qNorm) * snap.reserve) / snap.protonSupply) / 1e9).toBeCloseTo(1, 6);
    expect(Number((snap.qNorm * snap.reserve) / snap.neutronSupply) / Number(plan.price)).toBeCloseTo(1, 6);
    expect(plan.state.value).toBe(10n * ONE + MIN_FEE);
    expect(plan.state.pegFactor).toBe(profile.pegFactor);
  });

  it("mints the tokens with their EIP-4 metadata, creates the reactor box and burns the genesis tokens", () => {
    const { w, plan, txs, tree } = bootstrap(profile);
    for (const tx of [txs.mintNft, txs.mintNeutrons, txs.mintProtons, txs.createReactor]) expectAccepted(w, tx.toEIP12Object());

    const neutronMint = txs.mintNeutrons.outputs[0].additionalRegisters;
    expect(utf8(neutronMint.R4 as string)).toBe("TNEU");
    expect(utf8(neutronMint.R5 as string)).toBe("Test Neutron (TNEU): Pegged test token.");
    expect(utf8(neutronMint.R6 as string)).toBe("9");
    expect(utf8(txs.mintNft.outputs[0].additionalRegisters.R6 as string)).toBe("0");

    const reactor = w.reactor.utxos.at(0);
    expect(reactor.ergoTree).toBe(tree);
    expect(readReactor(reactor).state).toEqual(plan.state);
    expect(readReactor(reactor).tokens).toEqual(txs.tokens);
    // Only the reactor holds neutrons and protons: the genesis supply was burned.
    const holdings = (tokenId: string) =>
      txs.createReactor.outputs.flatMap((o) => o.assets).filter((a) => a.tokenId === tokenId).reduce((s, a) => s + BigInt(a.amount), 0n);
    expect(holdings(txs.tokens.neutron)).toBe(plan.state.neutrons);
    expect(holdings(txs.tokens.proton)).toBe(plan.state.protons);
  });

  it("creates a reactor that accepts every transaction type", () => {
    const { w, txs } = bootstrap(profile);
    for (const tx of [txs.mintNft, txs.mintNeutrons, txs.mintProtons, txs.createReactor]) expectAccepted(w, tx.toEIP12Object());
    const t = trader(w);
    const holding = (tokenId: string) =>
      t.party.utxos.toArray().flatMap((b) => b.assets).filter((a) => a.tokenId === tokenId).reduce((s, a) => s + a.amount, 0n);
    const run = (tx: Tx) => expectAccepted(w, tx, [t.party]);

    run(fissionTx(t.ctx(), ONE));
    run(betaDecayPlusTx(t.ctx(), holding(txs.tokens.proton) / 4n));
    run(betaDecayMinusTx(t.ctx(), holding(txs.tokens.neutron) / 4n));
    run(fusionTx(t.ctx(), ONE / 10n));

    // Adjust Peg needs an unhealthy reactor: publish an oracle price 70 % higher.
    w.oracle.addUTxOs({
      value: ONE,
      ergoTree: w.oracle.ergoTree,
      creationHeight: w.chain.height,
      assets: [{ tokenId: profile.poolNft, amount: 1n }],
      additionalRegisters: { R4: SLong((profile.oraclePrice * 17n) / 10n).toHex() },
    });
    const oracle = w.oracle.utxos.at(1);
    const { state } = readReactor(w.ctx().reactor);
    expect(isHealthy(snapshot(state, MIN_FEE, oraclePrice(oracle)).q)).toBe(false);
    run(adjustPegTx(t.ctx({ oracle })));
  });

  it.each(["0.1", "1", "10"])("passes the bootstrap script's pre-submission simulation with a %s ERG seed", (seedErg) => {
    const { w, txs } = bootstrap(profile, seedErg);
    const reactor = txs.createReactor.toEIP12Object().outputs[0];
    expect(() => simulateReactor(reactor, w.oracle.utxos.at(0), w.buyback.utxos.at(0), MIN_FEE, w.chain.height)).not.toThrow();
  });

  it("fails the pre-submission simulation with a malformed reactor", () => {
    const { w, txs } = bootstrap(profile);
    const reactor = txs.createReactor.toEIP12Object().outputs[0];
    const [oracle, buyback] = [w.oracle.utxos.at(0), w.buyback.utxos.at(0)];
    const malformed = { ...reactor, additionalRegisters: { ...reactor.additionalRegisters, R9: SLong(1_900_000n).toHex() } };
    expect(() => simulateReactor(malformed, oracle, buyback, MIN_FEE, w.chain.height)).toThrow(/would reject a fission/);
  });

  it("would create an unspendable reactor if R9 were a plain Long, so the script simulates first", () => {
    const { w, txs } = bootstrap(profile);
    for (const tx of [txs.mintNft, txs.mintNeutrons, txs.mintProtons]) expectAccepted(w, tx.toEIP12Object());
    const valid = txs.createReactor.toEIP12Object();
    // Creating a box is never checked by its own script, so the malformed reactor box is accepted …
    expectAccepted(w, mutate(valid, (tx) => void (tx.outputs[0].additionalRegisters.R9 = SLong(1_900_000n).toHex())));
    const malformed = w.reactor.utxos.at(0);
    // … but no transaction can ever spend it: build a fission for a well-formed twin, then spend the real box.
    const twin = mockUTxO({ ...malformed, additionalRegisters: valid.outputs[0].additionalRegisters });
    const t = trader(w);
    const fission = fissionTx(t.ctx({ reactor: twin }), ONE);
    const eip12 = { ...malformed, value: String(malformed.value), assets: malformed.assets.map((a) => ({ tokenId: a.tokenId, amount: String(a.amount) })) };
    expectRejected(w, mutate(fission, (tx: Tx) => void (tx.inputs[0] = { ...eip12, extension: tx.inputs[0].extension })), [t.party]);
  });

  it("records the deployment so that its contract can be reproduced and listed in Deployments.md (N3)", () => {
    const { config, plan, txs } = bootstrap(profile);
    const tree = reactorTree(contractConstants(config));
    const record = deploymentRecord(config, plan, profile.oraclePrice, tree, txs, "0".repeat(40), compilerVersion());
    expect(record.genesis.boxId).toBe(txs.createReactor.outputs[0].boxId);
    expect(record.transactions).toEqual([txs.mintNft.id, txs.mintNeutrons.id, txs.mintProtons.id, txs.createReactor.id]);
    expect(record.bootstrap?.burnedNeutrons).toBe(plan.genesis.neutrons.toString());
    expect(reproduce(record, CONTRACT_SOURCE)).toEqual([]);
    // The constants are segregated, so another _MinFee changes the tree and address but not the template.
    expect(reproduce({ ...record, constants: { ...record.constants, _MinFee: "2000000" } }, CONTRACT_SOURCE)).toEqual(["ErgoTree", "address"]);
    const md = renderDeployments([{ network: "mainnet", deployments: [record] }]);
    expect(md).toContain(`### ${config.deployment}`);
    expect(md).toContain(record.address);
    expect(md).toContain("9 decimals, emission 100000000 |");
  });
});
