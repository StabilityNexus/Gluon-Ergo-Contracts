// Update Treasury: moves a reactor's treasury, the single-signature key in R5 that receives the dev fees, to
// another single-signature address. Only the current treasury can do this: the wallet in .env must be the
// current treasury's, and it pays the miner fee. Without --submit, the node only checks the transaction.
// Usage: node scripts/update-treasury.ts <config file> <new treasury address> [--submit]
import { parseArgs } from "node:util";
import { AddressType, ErgoAddress, Network, RECOMMENDED_MIN_FEE_VALUE, SAFE_MIN_BOX_VALUE } from "@fleet-sdk/core";
import { publicKeyOf } from "../lib/bootstrap.ts";
import { addressOf, checkOrSubmit, loadEnvironment, nodeFor, run } from "../lib/cli.ts";
import { addressProblem, readDeployment } from "../lib/config.ts";
import { findUniqueBox } from "../sdk/src/node.ts";
import { readReactor } from "../sdk/src/reactor.ts";
import { selectInputs } from "../sdk/src/select.ts";
import { findDeployment } from "../lib/records.ts";
import { keyFromEnvironment, signTransaction } from "../lib/signer.ts";
import { updateTreasuryTx } from "../sdk/src/transactions.ts";

run(async () => {
  const { values, positionals } = parseArgs({
    allowPositionals: true,
    options: { submit: { type: "boolean", default: false } },
  });
  if (positionals.length !== 2) throw new Error("usage: node scripts/update-treasury.ts <config file> <new treasury address> [--submit]");
  const [path, newTreasury] = positionals;
  const config = readDeployment(path);
  const problem = addressProblem(newTreasury, config.network, [AddressType.P2PK], "a single-signature (P2PK) address");
  if (problem) throw new Error(`the new treasury ${problem}`);
  const record = findDeployment(config.network, config.deployment);

  loadEnvironment();
  const key = keyFromEnvironment();
  const signer = addressOf(key);
  const { node } = nodeFor(config);
  const height = await node.height();
  const reactor = await findUniqueBox(node, record.tokens.nft.id, "reactor", record.ergoTree);
  // Update Treasury does not use the oracle, but the contract reads data input 0 in every transaction.
  const oracle = await findUniqueBox(node, record.constants._OraclePoolNFT, "oracle pool");
  const { state } = readReactor(reactor);
  const current = ErgoAddress.fromPublicKey(state.treasury, Network.Mainnet).encode();
  if (signer !== current) throw new Error(`the wallet in .env (${signer}) is not the current treasury (${current})`);

  const inputs = selectInputs(
    await node.unspentByAddress(signer),
    { nanoErgs: RECOMMENDED_MIN_FEE_VALUE + SAFE_MIN_BOX_VALUE },
    { firstAddress: current },
  );
  const tx = updateTreasuryTx(
    { height, minFee: BigInt(record.constants._MinFee), reactor, oracle, inputs, recipient: signer },
    publicKeyOf(newTreasury),
  );
  console.log(`${config.deployment}: treasury ${current} → ${newTreasury}`);
  await checkOrSubmit(node, await signTransaction(node, tx, [key]), values.submit);
});
