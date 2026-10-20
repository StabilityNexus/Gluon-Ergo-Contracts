// Adjust Peg: while a reactor's fusion ratio is outside its healthy range [0.5, 0.98], anyone may move its peg
// factor by 1 % in the direction that brings the ratio back. The frontend and SDK do not offer this action.
//
// Without --submit it shows how many adjustments are needed and has the node check the first one. With
// --submit it broadcasts one adjustment; with --submit --repeat, all of them, each spending the reactor box
// the previous one created. The wallet in .env pays the miner fees.
// Usage: node scripts/adjust-peg.ts <config file> [--submit [--repeat]]
import { setTimeout as sleep } from "node:timers/promises";
import { parseArgs } from "node:util";
import type { Box } from "@fleet-sdk/common";
import { RECOMMENDED_MIN_FEE_VALUE, SAFE_MIN_BOX_VALUE } from "@fleet-sdk/core";
import { parse } from "@fleet-sdk/serializer";
import {
  addressOf,
  checkOrSubmit,
  loadEnvironment,
  nodeFor,
  run,
} from "../lib/cli.ts";
import { formatUnits, readDeployment } from "../lib/config.ts";
import { type ReactorState, pegAdjustments, snapshot } from "../sdk/src/math.ts";
import { type ErgoNode, findUniqueBox } from "../sdk/src/node.ts";
import { oraclePrice, requireFreshOracle } from "../sdk/src/oracle.ts";
import { readReactor } from "../sdk/src/reactor.ts";
import { selectInputs } from "../sdk/src/select.ts";
import { type DeploymentRecord, findDeployment } from "../lib/records.ts";
import { keyFromEnvironment, signTransaction } from "../lib/signer.ts";
import { adjustPegTx } from "../sdk/src/transactions.ts";

/** The reactor box, once the node has the box created by transaction `after` (if given) in its mempool. */
async function reactorBox(node: ErgoNode, record: DeploymentRecord, after?: string): Promise<Box<bigint>> {
  for (let attempt = 0; ; attempt++) {
    const box = await findUniqueBox(node, record.tokens.nft.id, "reactor", record.ergoTree);
    if (!after || box.transactionId === after) return box;
    if (attempt === 30) throw new Error(`the node does not show the reactor box created by ${after}`);
    await sleep(2000);
  }
}

run(async () => {
  const { values, positionals } = parseArgs({
    allowPositionals: true,
    options: { submit: { type: "boolean", default: false }, repeat: { type: "boolean", default: false } },
  });
  if (positionals.length !== 1 || (values.repeat && !values.submit)) {
    throw new Error("usage: node scripts/adjust-peg.ts <config file> [--submit [--repeat]]");
  }
  const config = readDeployment(positionals[0]);
  const record = findDeployment(config.network, config.deployment);
  const minFee = BigInt(record.constants._MinFee);
  const [, initialPeg] = parse<[bigint, bigint]>(record.genesis.registers.R9);
  const describe = (s: ReactorState, price: bigint) =>
    `fusion ratio ${formatUnits(snapshot(s, minFee, price).q)}, peg factor ${formatUnits(s.pegFactor)} ` +
    `(${formatUnits((s.pegFactor * 10_000n) / initialPeg, 2)} % of the initial ${formatUnits(initialPeg)})`;

  loadEnvironment();
  const key = keyFromEnvironment();
  const caller = addressOf(key);
  const { node } = nodeFor(config);

  let previous: string | undefined;
  for (;;) {
    const height = await node.height();
    const reactor = await reactorBox(node, record, previous);
    const oracle = await findUniqueBox(node, record.constants._OraclePoolNFT, "oracle pool");
    const { state } = readReactor(reactor);
    const price = oraclePrice(oracle);
    const steps = pegAdjustments(state, minFee, price);
    console.log(`${config.deployment}: ${describe(state, price)}`);
    if (steps.length === 0) {
      console.log("The reactor is in its healthy range, so no adjustment is needed.");
      return;
    }
    console.log(`${steps.length} adjustment(s) needed, which would reach ${describe(steps[steps.length - 1], price)}`);

    requireFreshOracle(oracle, height);
    const inputs = selectInputs(await node.unspentByAddress(caller), {
      nanoErgs: RECOMMENDED_MIN_FEE_VALUE + SAFE_MIN_BOX_VALUE,
    });
    const tx = adjustPegTx({ height, minFee, reactor, oracle, inputs, recipient: caller });
    previous = await checkOrSubmit(node, await signTransaction(node, tx, [key]), values.submit);
    if (!values.repeat || steps.length === 1) return;
  }
});
