import { existsSync, readFileSync } from "node:fs";
import { AddressType, ErgoAddress, Network } from "@fleet-sdk/core";
import type { GluonConstants } from "./compile.ts";
import { ONE, Q_LOWER, Q_UPPER } from "../sdk/src/math.ts";

// Public parameters of deployments live in config/*.json (N2). Fields that are not decided yet hold a
// string starting with "TODO"; scripts refuse to run while a field they need is TODO.

export type NetworkName = "mainnet";

export interface NetworkConfig {
  nodeUrl: string;
  explorerUrl: string;
}

export interface TokenMetadata {
  /** Short name that wallets display: written to the token's EIP-4 name (R4). */
  ticker: string;
  /** Full name, written at the start of the EIP-4 description (R5). */
  name: string;
  description: string;
}

export interface DeploymentConfig {
  deployment: string;
  network: NetworkName;
  peg: string;
  contract: { source: string; ergoTreeVersion: number };
  minFee: string;
  oracle: { description: string; poolNft: string; buybackNft: string; buybackContractAddress: string };
  treasuryAddress: string;
  bootstrap: {
    seedErg: string;
    initialFusionRatio: string;
    initialProtonPriceErg: string;
    initialPegFactor: string;
    maxDevFeeErg: string;
    tokenEmission: string;
  };
  tokens: { nft: TokenMetadata; neutron: TokenMetadata; proton: TokenMetadata };
}

/** Largest allowed seed (D11). The genesis tokens are burned, so the seed stays in the reserve for good. */
export const MAX_SEED = 10n * ONE;
const MAX_LONG = 2n ** 63n - 1n;
const NETWORKS: Record<NetworkName, Network> = { mainnet: Network.Mainnet };

export function isTodo(value: unknown): boolean {
  return typeof value === "string" && value.trimStart().startsWith("TODO");
}

export function readNetworks(path = new URL("../config/networks.json", import.meta.url)): Record<NetworkName, NetworkConfig> {
  return JSON.parse(readFileSync(path, "utf8"));
}

/** Reads a deployment config and checks that every field is present and is a string (or the version, a number). */
export function readDeployment(path: string | URL): DeploymentConfig {
  const config = JSON.parse(readFileSync(path, "utf8")) as DeploymentConfig;
  const missing = leaves(TEMPLATE).filter(([p]) => typeof get(config, p) !== typeof get(TEMPLATE, p));
  if (missing.length > 0) throw new Error(`${path}: missing or invalid fields: ${missing.map(([p]) => p).join(", ")}`);
  return config;
}

/** Dotted paths of the fields that are still TODO. */
export function todoFields(config: DeploymentConfig): string[] {
  return leaves(config).filter(([, v]) => isTodo(v)).map(([p]) => p);
}

/** Throws if any field under the given paths (all fields by default) is still TODO. */
export function requireReady(config: DeploymentConfig, prefixes: string[] = [""]): void {
  const todo = todoFields(config).filter((p) => prefixes.some((prefix) => p.startsWith(prefix)));
  if (todo.length > 0) throw new Error(`${config.deployment}: these fields are still TODO: ${todo.join(", ")}`);
}

/** Problems with the fields that are set. TODO fields are not problems; see {@link todoFields}. */
export function validateDeployment(c: DeploymentConfig): string[] {
  const problems: string[] = [];
  const check = (path: string, test: (v: string) => string | undefined) => {
    const value = get(c, path);
    if (typeof value === "string" && !isTodo(value)) {
      const problem = test(value);
      if (problem) problems.push(`${path}: ${problem}`);
    }
  };
  const decimal = (min: bigint, max?: bigint, decimals = 9) => (v: string) => {
    const x = tryDecimal(v, decimals);
    if (x === undefined) return `"${v}" is not a decimal number with at most ${decimals} decimals`;
    if (x <= min) return `must be greater than ${formatUnits(min, decimals)}`;
    if (max !== undefined && x > max) return `must be at most ${formatUnits(max, decimals)}`;
  };
  const hex32 = (v: string) => (/^[0-9a-f]{64}$/.test(v) ? undefined : "must be 32 bytes of lowercase hex");
  const address = (types: AddressType[], what: string) => (v: string) => addressProblem(v, c.network, types, what);

  if (!(c.network in NETWORKS)) problems.push(`network: must be "mainnet"`);
  check("contract.source", (v) => (existsSync(new URL(`../${v}`, import.meta.url)) ? undefined : "file not found"));
  if (c.contract.ergoTreeVersion !== 0) problems.push("contract.ergoTreeVersion: must be 0");
  check("minFee", decimal(0n, MAX_LONG, 0));
  check("oracle.poolNft", hex32);
  check("oracle.buybackNft", hex32);
  check("oracle.buybackContractAddress", address([AddressType.P2S, AddressType.P2SH], "a contract address"));
  check("treasuryAddress", address([AddressType.P2PK], "a single-signature (P2PK) address"));
  check("bootstrap.seedErg", decimal(0n, MAX_SEED));
  check("bootstrap.initialFusionRatio", (v) => {
    const q = tryDecimal(v);
    return q !== undefined && q >= Q_LOWER && q <= Q_UPPER ? undefined : "must be within the healthy range [0.5, 0.98]";
  });
  check("bootstrap.initialProtonPriceErg", decimal(0n));
  check("bootstrap.initialPegFactor", decimal(0n));
  check("bootstrap.maxDevFeeErg", decimal(0n));
  check("bootstrap.tokenEmission", decimal(0n, MAX_LONG / ONE, 0));
  for (const token of ["nft", "neutron", "proton"] as const) {
    check(`tokens.${token}.ticker`, (v) => (/^\S{1,32}$/.test(v) ? undefined : "must be 1 to 32 characters without spaces"));
    check(`tokens.${token}.name`, (v) => (v.trim() ? undefined : "must not be empty"));
    check(`tokens.${token}.description`, (v) => (v.trim() ? undefined : "must not be empty"));
  }
  return problems;
}

/** What is wrong with `value` as an address of one of `types` (described by `what`) on `network`, if anything. */
export function addressProblem(value: string, network: NetworkName, types: AddressType[], what: string): string | undefined {
  let a: ErgoAddress;
  try {
    a = ErgoAddress.decode(value);
  } catch {
    return "is not a valid address";
  }
  if (a.network !== NETWORKS[network]) return `is not a ${network} address`;
  if (!types.includes(a.type)) return `must be ${what}`;
}

/** Throws if the config has problems, or if any field under `prefixes` is still TODO. */
export function requireValid(config: DeploymentConfig, prefixes?: string[]): void {
  const problems = validateDeployment(config);
  if (problems.length > 0) throw new Error(`${config.deployment}: ${problems.join("; ")}`);
  requireReady(config, prefixes);
}

/** The contract source a deployment compiles, from `contract.source` (a path relative to the repository root). */
export function contractSource(c: DeploymentConfig): string {
  return readFileSync(new URL(`../${c.contract.source}`, import.meta.url), "utf8");
}

/** The contract's compile-time constants for a deployment. */
export function contractConstants(c: DeploymentConfig): GluonConstants {
  requireValid(c, ["minFee", "oracle."]);
  return {
    minFee: BigInt(c.minFee),
    oracleFeeTree: ErgoAddress.decode(c.oracle.buybackContractAddress).ergoTree,
    oraclePoolNft: c.oracle.poolNft,
    oracleBuybackNft: c.oracle.buybackNft,
  };
}

export interface BootstrapParameters {
  /** Reserve to seed, in nanoERG. */
  seed: bigint;
  /** Initial fusion ratio, scaled by ONE. */
  fusionRatio: bigint;
  /** Initial price of one proton, in nanoERG. */
  protonPrice: bigint;
  /** Initial peg factor as stored in R9 (scaled by ONE). */
  pegFactor: bigint;
  /** Maximum dev fee as stored in R6, in nanoERG. */
  maxDevFee: bigint;
  /** Emission of neutrons and of protons, in base units. */
  emission: bigint;
}

export function bootstrapParameters(c: DeploymentConfig): BootstrapParameters {
  requireValid(c, ["bootstrap."]);
  const b = c.bootstrap;
  return {
    seed: parseDecimal(b.seedErg),
    fusionRatio: parseDecimal(b.initialFusionRatio),
    protonPrice: parseDecimal(b.initialProtonPriceErg),
    pegFactor: parseDecimal(b.initialPegFactor),
    maxDevFee: parseDecimal(b.maxDevFeeErg),
    emission: BigInt(b.tokenEmission) * ONE,
  };
}

/** Parses a decimal string, e.g. "0.001", into an integer scaled by 10^decimals (1000000 for 9 decimals). */
export function parseDecimal(value: string, decimals = 9): bigint {
  const x = tryDecimal(value, decimals);
  if (x === undefined) throw new Error(`"${value}" is not a decimal number with at most ${decimals} decimals`);
  return x;
}

function tryDecimal(value: string, decimals = 9): bigint | undefined {
  const match = /^(\d+)(?:\.(\d+))?$/.exec(value.trim());
  if (!match || (match[2] ?? "").length > decimals) return undefined;
  return BigInt(match[1] + (match[2] ?? "").padEnd(decimals, "0"));
}

export function formatUnits(value: bigint, decimals = 9): string {
  const s = value.toString().padStart(decimals + 1, "0");
  const fraction = s.slice(s.length - decimals).replace(/0+$/, "");
  return fraction ? `${s.slice(0, s.length - decimals)}.${fraction}` : s.slice(0, s.length - decimals);
}

// Shape of a deployment config, used to check that every field is present with the right type.
const TEMPLATE: DeploymentConfig = {
  deployment: "",
  network: "mainnet",
  peg: "",
  contract: { source: "", ergoTreeVersion: 0 },
  minFee: "",
  oracle: { description: "", poolNft: "", buybackNft: "", buybackContractAddress: "" },
  treasuryAddress: "",
  bootstrap: { seedErg: "", initialFusionRatio: "", initialProtonPriceErg: "", initialPegFactor: "", maxDevFeeErg: "", tokenEmission: "" },
  tokens: {
    nft: { ticker: "", name: "", description: "" },
    neutron: { ticker: "", name: "", description: "" },
    proton: { ticker: "", name: "", description: "" },
  },
};

function leaves(object: object, prefix = ""): [string, unknown][] {
  return Object.entries(object).flatMap(([key, value]) =>
    value !== null && typeof value === "object" ? leaves(value, `${prefix}${key}.`) : [[`${prefix}${key}`, value]],
  );
}

function get(object: unknown, path: string): unknown {
  return path.split(".").reduce<unknown>((o, key) => (o !== null && typeof o === "object" ? (o as Record<string, unknown>)[key] : undefined), object);
}
