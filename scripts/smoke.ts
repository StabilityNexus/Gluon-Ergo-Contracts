// Live, read-only smoke check (§8.4)
// Usage: node scripts/smoke.ts <deployment>
import { parseArgs } from "node:util";
import type { EIP12UnsignedTransaction, SignedTransaction } from "@fleet-sdk/common";
import { ErgoAddress, Network, SAFE_MIN_BOX_VALUE } from "@fleet-sdk/core";
import { GluonClient } from "../sdk/src/client.ts";
import { getDeployment } from "../sdk/src/deployments/index.ts";
import { readNetworks } from "../lib/config.ts";
import { run } from "../lib/cli.ts";

function toDummySignedTransaction(tx: EIP12UnsignedTransaction): SignedTransaction {
  return {
    id: tx.id ?? "00".repeat(32),
    inputs: tx.inputs.map((i) => ({
      boxId: i.boxId,
      spendingProof: { proofBytes: "", extension: i.extension ?? {} },
    })),
    dataInputs: tx.dataInputs ?? [],
    outputs: tx.outputs.map((o, idx) => ({
      ...o,
      boxId: "00".repeat(32),
      transactionId: tx.id ?? "00".repeat(32),
      index: idx,
    })),
  };
}

run(async () => {
  const { positionals } = parseArgs({ allowPositionals: true });
  if (positionals.length !== 1) {
    throw new Error("usage: node scripts/smoke.ts <deployment>");
  }

  const deploymentName = positionals[0];
  const deployment = getDeployment(deploymentName);
  const networks = readNetworks();
  const nodeUrl = networks.mainnet.nodeUrl;

  const client = new GluonClient({ deployment, nodeUrl });
  console.log(`Connecting to ${nodeUrl} for deployment ${deployment.name}...`);

  const chain = await client.fetchChainState();
  const snapshot = client.snapshot(chain);

  console.log("Snapshot:");
  console.log(`  Height: ${snapshot.height}`);
  console.log(`  Reserve: ${snapshot.reserve} nanoERG`);
  console.log(`  Neutrons in circulation: ${snapshot.neutronsInCirculation}`);
  console.log(`  Protons in circulation: ${snapshot.protonsInCirculation}`);
  console.log(`  Oracle price: ${snapshot.oraclePrice}`);
  console.log(`  Target price: ${snapshot.targetPrice}`);
  console.log(`  q: ${snapshot.q}`);
  console.log(`  Healthy: ${snapshot.healthy}`);

  const fictitiousUserKey = "0279be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798";
  const fictitiousUserAddr = ErgoAddress.fromPublicKey(fictitiousUserKey, Network.Mainnet);
  const fictitiousBox = {
    boxId: "00".repeat(32),
    transactionId: "00".repeat(32),
    index: 0,
    ergoTree: fictitiousUserAddr.ergoTree,
    creationHeight: chain.height,
    value: 100_000_000_000n, // 100 ERG
    assets: [
      { tokenId: deployment.tokens.neutron.id, amount: 100_000_000_000n },
      { tokenId: deployment.tokens.proton.id, amount: 100_000_000_000n },
    ],
    additionalRegisters: {},
  };

  const funding = {
    userBoxes: [fictitiousBox],
    changeAddress: fictitiousUserAddr.encode(),
  };

  const transactions: { name: string; tx: EIP12UnsignedTransaction }[] = [];

  if (snapshot.healthy) {
    transactions.push({ name: "fission", tx: client.buildFission(chain, { erg: 10_000_000n, ...funding }) });
    transactions.push({ name: "fusion", tx: client.buildFusion(chain, { erg: 10_000_000n, ...funding }) });
    if (snapshot.betaDecaysAvailable) {
      transactions.push({
        name: "betaDecayPlus",
        tx: client.buildBetaDecayPlus(chain, { protons: 10_000_000n, ...funding }),
      });
      transactions.push({
        name: "betaDecayMinus",
        tx: client.buildBetaDecayMinus(chain, { neutrons: 10_000_000n, ...funding }),
      });
    }
  } else {
    transactions.push({ name: "adjustPeg", tx: client.buildAdjustPeg(chain, funding) });
  }

  // Update Treasury requires the treasury's box as first input, so fictitious user would be current treasury in this test
  const treasuryFunding = {
    userBoxes: [
      {
        ...fictitiousBox,
        ergoTree: ErgoAddress.fromPublicKey(chain.state.treasury, Network.Mainnet).ergoTree,
      },
    ],
    changeAddress: fictitiousUserAddr.encode(),
  };
  transactions.push({
    name: "updateTreasury",
    tx: client.buildUpdateTreasury(chain, { newTreasuryAddress: fictitiousUserAddr.encode(), ...treasuryFunding }),
  });

  for (const { name, tx } of transactions) {
    try {
      const signed = toDummySignedTransaction(tx);
      await client.node.checkTransaction(signed);
      console.log(`  ✓ ${name} checked`);
    } catch (e: unknown) {
      const msg = e instanceof Error ? e.message : String(e);
      // We expect the node to reject fictitious inputs (missing input / double spend), but not bad syntax/tree
      if (msg.includes("not found") || msg.includes("missing") || msg.includes("Unknown input")) {
        console.log(`  ✓ ${name} passed structural validation (node reported missing fictitious input as expected)`);
      } else {
        throw new Error(`${name} failed checkTransaction unexpectedly: ${msg}`);
      }
    }
  }

  console.log("All smoke checks passed.");
});
