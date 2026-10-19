import { existsSync } from "node:fs";
import type { Box, SignedTransaction } from "@fleet-sdk/common";
import { Network } from "@fleet-sdk/core";
import type { ErgoHDKey } from "@fleet-sdk/wallet";
import { type DeploymentConfig, type NetworkConfig, formatUnits, isTodo, readNetworks } from "./config.ts";
import { ErgoNode } from "../sdk/src/node.ts";

// Helpers shared by the scripts in scripts/.

/** Loads .env, which holds only secrets (N2). */
export function loadEnvironment(): void {
  if (existsSync(".env")) process.loadEnvFile(".env");
}

/** The node of the deployment's network, from config/networks.json. */
export function nodeFor(config: DeploymentConfig): { node: ErgoNode; network: NetworkConfig } {
  const network = readNetworks()[config.network];
  if (isTodo(network.nodeUrl)) throw new Error(`set the ${config.network} node URL in config/networks.json`);
  return { node: new ErgoNode(network.nodeUrl), network };
}

/** The mainnet P2PK address of a key. */
export function addressOf(key: ErgoHDKey): string {
  return key.address.encode(Network.Mainnet);
}

export function erg(nanoerg: bigint): string {
  return `${formatUnits(nanoerg)} ERG`;
}

/** Has the node validate a signed transaction and, if `submit`, broadcasts it. Returns the transaction id. */
export async function checkOrSubmit(node: ErgoNode, tx: SignedTransaction, submit: boolean): Promise<string> {
  if (!submit) {
    const id = await node.checkTransaction(tx);
    console.log(`The node accepts transaction ${id}. Run again with --submit to broadcast it.`);
    return id;
  }
  const id = await node.submitTransaction(tx);
  console.log(`Submitted transaction ${id}.`);
  return id;
}

/** Runs a script's main function, printing errors without a stack trace. */
export function run(main: () => Promise<void>): void {
  main().catch((e: unknown) => {
    console.error(`error: ${e instanceof Error ? e.message : String(e)}`);
    process.exit(1);
  });
}
