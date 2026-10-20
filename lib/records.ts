import { existsSync, readFileSync, writeFileSync } from "node:fs";
import { compile } from "@fleet-sdk/compiler";
import { blake2b256, sha256 } from "@fleet-sdk/crypto";
import type { ErgoTree } from "@fleet-sdk/core";
import { SByte, SColl, SConstant, SLong } from "@fleet-sdk/serializer";
import { type NetworkName, formatUnits } from "./config.ts";

// Deployment records: deployments/<network>.json is the machine-readable record of every reactor that has
// been deployed, and Deployments.md is generated from it (N3).

export const REPOSITORY_URL = "https://github.com/StabilityNexus/Gluon-Ergo-Contracts";
export const NETWORKS: NetworkName[] = ["mainnet"];
const EXPLORERS: Record<NetworkName, string> = {
  mainnet: "https://explorer.ergoplatform.com",
};

export interface TokenRecord {
  id: string;
  name: string;
  decimals: number;
  emission: string;
}

export interface DeploymentRecord {
  name: string;
  peg: string;
  status: "live" | "superseded" | "unused";
  network: NetworkName;
  contract: {
    /** Commit whose contract source compiles to `ergoTree`. */
    revision: string | null;
    /** Repository that has the commit, if it is not this one. */
    repository?: string;
    /** Path of the contract source in that commit. */
    source?: string;
    /** First and last commits whose contract source compiles to `ergoTree`, if more than one does. */
    revisionRange?: [string, string] | null;
    tag?: string | null;
    ergoTreeVersion: number;
    compiler?: string;
  };
  /** The contract's compile-time constants: decimal strings for numbers, hex for bytes. */
  constants: Record<string, string>;
  /** Address of the oracle-fee (buyback) contract given by `_OracleFeePk`. */
  oracleFeeAddress?: string;
  ergoTree: string;
  address: string;
  templateHash: { sha256: string; blake2b256: string };
  tokens: { nft: TokenRecord; neutron: TokenRecord; proton: TokenRecord };
  /** Treasury (R5) at genesis, as an address. */
  treasury: string;
  /** Parameters of the bootstrap script, for reactors it created. */
  bootstrap?: Record<string, string>;
  genesis: {
    transactionId: string;
    boxId: string;
    height: number | null;
    value: string;
    registers: Record<string, string>;
  };
  /** Ids of all transactions that created the deployment, in order. */
  transactions?: string[];
  notes?: string;
}

export interface Records {
  network: NetworkName;
  deployments: DeploymentRecord[];
}

const path = (network: NetworkName) => new URL(`../deployments/${network}.json`, import.meta.url);
export const DEPLOYMENTS_MD = new URL("../Deployments.md", import.meta.url);

export function readRecords(network: NetworkName): Records {
  return existsSync(path(network)) ? JSON.parse(readFileSync(path(network), "utf8")) : { network, deployments: [] };
}

export function writeRecords(records: Records): void {
  writeFileSync(path(records.network), `${JSON.stringify(records, null, 2)}\n`);
}

export function findDeployment(network: NetworkName, name: string): DeploymentRecord {
  const record = readRecords(network).deployments.find((d) => d.name === name);
  if (!record) throw new Error(`deployments/${network}.json has no deployment named ${name}`);
  return record;
}

export function templateHashes(tree: ErgoTree): { sha256: string; blake2b256: string } {
  const hex = (bytes: Uint8Array) => Buffer.from(bytes).toString("hex");
  return { sha256: hex(sha256(tree.template)), blake2b256: hex(blake2b256(tree.template)) };
}

/** A record's constants as compiler input: decimal strings are Long values, hex strings Coll[Byte] values. */
export function recordConstants(constants: Record<string, string>) {
  return Object.fromEntries(
    Object.entries(constants).map(([name, value]) => [name, /^\d+$/.test(value) ? SLong(BigInt(value)) : SColl(SByte, value)]),
  );
}

/**
 * Compiles `source` with a record's constants and ErgoTree version, and returns what does not match the
 * record: an empty list means the record is reproduced.
 */
export function reproduce(d: DeploymentRecord, source: string): string[] {
  const tree = compile(source, {
    version: d.contract.ergoTreeVersion as 0 | 1,
    segregateConstants: true,
    network: d.network,
    map: recordConstants(d.constants),
  });
  const hashes = templateHashes(tree);
  return [
    tree.toHex() !== d.ergoTree && "ErgoTree",
    tree.toAddress().encode() !== d.address && "address",
    (hashes.sha256 !== d.templateHash.sha256 || hashes.blake2b256 !== d.templateHash.blake2b256) && "template hash",
  ].filter((m) => m !== false);
}

/** Deployments.md, generated from the records of all networks. */
export function renderDeployments(all: Records[] = NETWORKS.map(readRecords)): string {
  const lines = [
    "# Deployments",
    "",
    "Every deployment of the Gluon reactor contract, with its contract address and the parameter values it",
    "was deployed with. This file is generated from `deployments/<network>.json` by",
    "`node scripts/render-deployments.ts`; do not edit it by hand. When you deploy, tag the deployed",
    "version in git; the bootstrap script records the deployment.",
  ];
  for (const records of all) {
    lines.push("", `## ${records.network[0].toUpperCase()}${records.network.slice(1)}`, "");
    if (records.deployments.length === 0) lines.push("No deployments yet.");
    for (const d of records.deployments) lines.push(...renderDeployment(d), "");
  }
  return `${lines.join("\n").replace(/\n+$/, "")}\n`;
}

function renderDeployment(d: DeploymentRecord): string[] {
  const explorer = EXPLORERS[d.network];
  const tx = (id: string) => `[\`${id.slice(0, 12)}…\`](${explorer}/en/transactions/${id})`;
  const token = (t: TokenRecord) =>
    `${t.name}: [\`${t.id.slice(0, 12)}…\`](${explorer}/en/token/${t.id}), ${t.decimals} decimals, emission ${formatUnits(BigInt(t.emission), t.decimals)}`;
  const repository = d.contract.repository ?? REPOSITORY_URL;
  const revision = d.contract.revision
    ? `[\`${d.contract.revision.slice(0, 7)}\`](${repository}/commit/${d.contract.revision})` +
      (d.contract.tag ? ` (tag \`${d.contract.tag}\`)` : "") +
      (d.contract.repository ? ` in ${d.contract.repository.replace("https://github.com/", "")}` : "")
    : "unknown";
  const rows: [string, string][] = [
    ["Status", d.status],
    ["Peg", d.peg],
    ["Contract revision", revision],
    ["ErgoTree", `version ${d.contract.ergoTreeVersion}, ${d.ergoTree.length / 2} bytes`],
    ["Template hash", `SHA-256 \`${d.templateHash.sha256}\``],
    ["Reactor NFT", token(d.tokens.nft)],
    ["Neutron", token(d.tokens.neutron)],
    ["Proton", token(d.tokens.proton)],
    ["Treasury at genesis", `\`${d.treasury}\``],
    [
      "Genesis",
      d.transactions && d.transactions.length > 1
        ? `transactions ${d.transactions.map(tx).join(", ")}`
        : `transaction ${tx(d.genesis.transactionId)}${d.genesis.height ? ` at height ${d.genesis.height}` : ""}`,
    ],
    ...Object.entries(d.constants).map(([name, value]): [string, string] => [
      `\`${name}\``,
      name === "_OracleFeePk" && d.oracleFeeAddress ? `ErgoTree of the address below (${value.length / 2} bytes)` : `\`${value}\``,
    ]),
    ...Object.entries(d.bootstrap ?? {}).map(([name, value]): [string, string] => [name, `\`${value}\``]),
    ...Object.entries(d.genesis.registers).map(([name, value]): [string, string] => [`Genesis ${name}`, describeRegister(value)]),
  ];
  const lines = [`### ${d.name}`, "", "| | |", "|---|---|", ...rows.map(([k, v]) => `| ${k} | ${v} |`)];
  if (d.notes) lines.push("", d.notes);
  lines.push("", "<details><summary>Contract address</summary>", "", "```", d.address, "```", "", "</details>");
  if (d.oracleFeeAddress) {
    lines.push("", "<details><summary>Oracle-fee contract address</summary>", "", "```", d.oracleFeeAddress, "```", "", "</details>");
  }
  return lines;
}

/** A register's type and value, e.g. "(Long, Long) `(0, 10000000000000000)`". */
function describeRegister(hex: string): string {
  const constant = SConstant.from<unknown>(hex);
  const value = (v: unknown): string =>
    v instanceof Uint8Array ? Buffer.from(v).toString("hex") : Array.isArray(v) ? `[${v.map(value).join(", ")}]` : String(v);
  const type = constant.type.toString().replace(/\bS(?=[A-Z])/g, "");
  const shown = type.startsWith("(") ? value(constant.data).replace(/^\[(.*)\]$/, "($1)") : value(constant.data);
  return `${type} \`${shown}\``;
}
