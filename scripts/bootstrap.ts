// Deploys a new reactor from a deployment config, in four chained transactions: mint the reactor NFT, mint
// the neutrons, mint the protons, then create the reactor box and burn the genesis neutrons and protons.
//
// Without --submit it is a dry run: it plans the reactor, builds and signs the four transactions, runs a
// fission, both beta decays, a fusion and an Adjust Peg against the new reactor box on a local simulated chain,
// and has the node check the first transaction. With --submit it also broadcasts the transactions and records
// the deployment in deployments/<network>.json and Deployments.md.
//
// The seed (bootstrap.seedErg, at most 10 ERG) stays in the reserve for good: the genesis tokens, which could
// redeem it, are burned. The wallet in .env pays for the seed, three token boxes and four miner fees.
// Usage: node scripts/bootstrap.ts <config file> [--submit]
import { execFileSync } from "node:child_process";
import { writeFileSync } from "node:fs";
import { parseArgs } from "node:util";
import { bootstrapCost, bootstrapTransactions, deploymentRecord, planBootstrap, publicKeyOf } from "../lib/bootstrap.ts";
import { addressOf, erg, loadEnvironment, nodeFor, run } from "../lib/cli.ts";
import { findUniqueBox } from "../sdk/src/node.ts";
import { compileGluon, compilerVersion } from "../lib/compile.ts";
import {
  MAX_SEED,
  bootstrapParameters,
  contractConstants,
  contractSource,
  formatUnits,
  readDeployment,
  requireValid,
} from "../lib/config.ts";
import { ONE, snapshot, valueOfProtons } from "../sdk/src/math.ts";
import { oraclePrice, requireFreshOracle } from "../sdk/src/oracle.ts";
import { DEPLOYMENTS_MD, readRecords, renderDeployments, writeRecords } from "../lib/records.ts";
import { keyFromEnvironment, signTransaction } from "../lib/signer.ts";
import { consensusIssues, simulateReactor } from "../lib/simulate.ts";
import { SDK_DATA_TS, renderSdkData } from "./build-sdk-data.ts";

const ROOT = new URL("..", import.meta.url);

/** The commit the contract source comes from. The source must be committed, so the record can point to it. */
function committedRevision(source: string): string {
  const git = (...args: string[]) => execFileSync("git", args, { cwd: ROOT, encoding: "utf8" }).trim();
  git("ls-files", "--error-unmatch", source);
  if (git("status", "--porcelain", "--", source) !== "") throw new Error(`commit your changes to ${source} first`);
  return git("rev-parse", "HEAD");
}

run(async () => {
  const { values, positionals } = parseArgs({
    allowPositionals: true,
    options: { submit: { type: "boolean", default: false } },
  });
  if (positionals.length !== 1) throw new Error("usage: node scripts/bootstrap.ts <config file> [--submit]");
  const config = readDeployment(positionals[0]);
  requireValid(config);
  const records = readRecords(config.network);
  if (records.deployments.some((d) => d.name === config.deployment)) {
    throw new Error(`deployments/${config.network}.json already has a deployment named ${config.deployment}`);
  }
  const revision = committedRevision(config.contract.source);
  const params = bootstrapParameters(config);
  if (params.seed > MAX_SEED) throw new Error(`the seed must be at most ${erg(MAX_SEED)}`);

  loadEnvironment();
  const key = keyFromEnvironment();
  const deployer = addressOf(key);
  const { node } = nodeFor(config);
  const [height, { parameters }] = await Promise.all([node.height(), node.info()]);

  const constants = contractConstants(config);
  const tree = compileGluon(constants, config.network, contractSource(config));
  const oracle = await findUniqueBox(node, constants.oraclePoolNft, "oracle pool");
  const buyback = await findUniqueBox(node, constants.oracleBuybackNft, "oracle buyback", constants.oracleFeeTree);
  requireFreshOracle(oracle, height);
  const price = oraclePrice(oracle);
  const plan = planBootstrap(params, constants.minFee, price, publicKeyOf(config.treasuryAddress), height);

  const funds = await node.unspentByAddress(deployer);
  const balance = funds.reduce((sum, b) => sum + b.value, 0n);
  const cost = bootstrapCost(plan);
  if (balance < cost) throw new Error(`${deployer} holds ${erg(balance)}, but the bootstrap needs ${erg(cost)}`);
  const txs = bootstrapTransactions(config, plan, tree.toHex(), funds, deployer, height);
  const chain = [txs.mintNft, txs.mintNeutrons, txs.mintProtons, txs.createReactor].map((tx) => tx.toEIP12Object());
  for (const [i, tx] of chain.entries()) {
    const issues = consensusIssues(tx, height, BigInt(parameters.minValuePerByte));
    if (issues.length > 0) throw new Error(`transaction ${i + 1} would be rejected: ${issues.join("; ")}`);
  }
  simulateReactor(chain[3].outputs[0], oracle, buyback, constants.minFee, height);

  const launch = snapshot(plan.state, constants.minFee, price);
  console.log(`Deployment        ${config.deployment} (${config.network}), pegged to ${config.peg}
Contract          ${tree.toAddress().encode().slice(0, 24)}… from ${revision.slice(0, 7)}
Deployer          ${deployer}, holding ${erg(balance)}; the bootstrap costs ${erg(cost)}
Treasury          ${config.treasuryAddress}
Oracle price      ${price} nanoERG (box ${oracle.boxId.slice(0, 12)}…, created at height ${oracle.creationHeight})
Peg factor        ${config.bootstrap.initialPegFactor}: one neutron is worth ${erg(plan.price)}
Reserve           ${erg(params.seed)}, plus the ${erg(constants.minFee)} _MinFee
Burned genesis    ${formatUnits(plan.genesis.neutrons)} neutrons, ${formatUnits(plan.genesis.protons)} protons
Fusion ratio      ${formatUnits(launch.q)}; one proton is worth ${erg(valueOfProtons(launch, ONE))}
Tokens            ${txs.tokens.nft} ${config.tokens.nft.ticker}
                  ${txs.tokens.neutron} ${config.tokens.neutron.ticker}
                  ${txs.tokens.proton} ${config.tokens.proton.ticker}
Simulation        fission, beta decay +, beta decay −, fusion and Adjust Peg are accepted`);

  const signed = [];
  for (const tx of chain) signed.push(await signTransaction(node, tx, [key]));
  if (!values.submit) {
    await node.checkTransaction(signed[0]);
    console.log("\nThe four transactions are signed and the node accepts the first one. Run again with --submit to deploy.");
    return;
  }

  for (const [i, tx] of signed.entries()) {
    try {
      console.log(`Submitted transaction ${i + 1} of 4: ${await node.submitTransaction(tx)}`);
    } catch (e) {
      throw new Error(`transaction ${i + 1} failed (${i} submitted): ${e instanceof Error ? e.message : e}`);
    }
  }
  records.deployments.push(deploymentRecord(config, plan, price, tree, txs, revision, compilerVersion()));
  writeRecords(records);
  writeFileSync(DEPLOYMENTS_MD, renderDeployments());
  writeFileSync(SDK_DATA_TS, renderSdkData());
  console.log(`
Recorded the deployment in deployments/${config.network}.json, Deployments.md and sdk/src/deployments/data.ts. Next:
- commit the three files, and tag the deployed revision: git tag -a ${config.deployment} ${revision.slice(0, 7)} -m "${config.deployment}"
- push the commit and the tag, so the record's links resolve`);
});
