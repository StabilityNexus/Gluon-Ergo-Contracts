import type { BoxCandidate, EIP12UnsignedTransaction } from "@fleet-sdk/common";
import { compile } from "@fleet-sdk/compiler";
import type { Amount, Box, ErgoTree } from "@fleet-sdk/core";
import { type KeyedMockChainParty, MockChain, type MockChainParty } from "@fleet-sdk/mock-chain";
import { SLong } from "@fleet-sdk/serializer";
import { expect } from "vitest";
import { type GluonConstants, compileGluon } from "../lib/compile.ts";
import { BUCKETS, ONE, type ReactorState, adjustedPrice, bucketStart, genesisSupplies } from "../sdk/src/math.ts";
import { type ReactorTokens, reactorAssets, reactorRegisters, readReactor } from "../sdk/src/reactor.ts";
import { consensusIssues } from "../lib/simulate.ts";
import type { TxContext } from "../sdk/src/transactions.ts";

/** Chain height when a test starts. Scripts see the height of the next block, HEIGHT + 1. */
export const HEIGHT = 1_900_000;
export const MIN_FEE = 1_000_000n;
export const EMISSION = 100_000_000n * ONE;
export const MAX_DEV_FEE = 10_000_000n * ONE;
export const TOKENS: ReactorTokens = { nft: "a1".repeat(32), neutron: "b2".repeat(32), proton: "c3".repeat(32) };

/** The two kinds of deployment: they differ in oracle, price unit and initial peg factor. */
export interface Profile {
  name: string;
  oraclePrice: bigint;
  pegFactor: bigint;
  poolNft: string;
  buybackNft: string;
}

export const GOLD: Profile = {
  name: "Gold",
  oraclePrice: 410_698_181_858_725n, // GoldPool on 2026-10-11, nanoERG per kg of gold
  pegFactor: ONE / 1000n, // 0.001: one neutron is pegged to one gram
  poolNft: "3c45f29a5165b030fdb5eaf5d81f8108f9d8f507b31487dd51f4ae08fe07cf4a",
  buybackNft: "d4".repeat(32), // the GoldPool's buyback NFT is still to be decided (D10)
};

export const DOLLAR: Profile = {
  name: "Dollar",
  oraclePrice: 3_048_107_555n, // USD Pool on 2026-10-11, nanoERG per USD
  pegFactor: ONE, // 1: one neutron is pegged to one USD
  poolNft: "6a2b821b5727e85beb5e78b4efb9f0250d59cd48481d2ded2c23e91ba1d07c66",
  buybackNft: "dcce07af04ea4f9b7979336476594dc16321547bcc9c6b95a67cb1a94192da4f",
};

export const PROFILES = [GOLD, DOLLAR];

/** Stand-in for the oracle buyback contract: anyone may spend it. The Gluon contract checks its successor. */
export const BUYBACK_TREE = compile("sigmaProp(true)").toHex();

export function constantsFor(profile: Profile): GluonConstants {
  return {
    minFee: MIN_FEE,
    oracleFeeTree: BUYBACK_TREE,
    oraclePoolNft: profile.poolNft,
    oracleBuybackNft: profile.buybackNft,
  };
}

const trees = new Map<string, ErgoTree>();

export function reactorTree(constants: GluonConstants): ErgoTree {
  const key = JSON.stringify(constants, (_, v) => (typeof v === "bigint" ? v.toString() : v));
  let tree = trees.get(key);
  if (!tree) {
    tree = compileGluon(constants);
    trees.set(key, tree);
  }
  return tree;
}

export function publicKey(party: KeyedMockChainParty): string {
  return Buffer.from(party.address.getPublicKeys()[0]).toString("hex");
}

/** A reactor with a 1,000 ERG reserve at fusion ratio `q` (default 0.6), with 400 protons in circulation. */
export function reactorState(profile: Profile, treasury: string, q = (6n * ONE) / 10n): ReactorState {
  const reserve = 1_000n * ONE;
  const price = adjustedPrice(profile.oraclePrice, profile.pegFactor);
  const neutrons = genesisSupplies(reserve, price, q).neutrons;
  const protons = genesisSupplies(reserve, price).protons;
  return {
    value: reserve + MIN_FEE,
    neutrons: EMISSION - neutrons,
    protons: EMISSION - protons,
    totalNeutrons: EMISSION,
    totalProtons: EMISSION,
    treasury,
    devFeeRepaid: 0n,
    maxDevFee: MAX_DEV_FEE,
    volumePlus: Array<bigint>(BUCKETS).fill(0n),
    volumeMinus: Array<bigint>(BUCKETS).fill(0n),
    lastBucketBlock: bucketStart(HEIGHT + 1),
    pegFactor: profile.pegFactor,
  };
}

export interface SetupOptions {
  /** Changes to the default reactor state (see {@link reactorState}). */
  state?: (s: ReactorState) => Partial<ReactorState>;
  /** Fusion ratio of the default reactor state. */
  q?: bigint;
  oraclePrice?: bigint;
  oracleNft?: string;
  /** Blocks between the oracle box's creation and the HEIGHT the scripts see. */
  oracleAge?: number;
  buybackNft?: string;
  /** A real buyback box (e.g. a mainnet fixture) to use instead of the stand-in. The reactor is compiled for it. */
  buybackBox?: Box<Amount>;
  /** Start without a reactor box, e.g. to bootstrap one. The reactor party still receives boxes of its tree. */
  withoutReactor?: boolean;
}

export interface World {
  chain: MockChain;
  profile: Profile;
  user: KeyedMockChainParty;
  treasury: KeyedMockChainParty;
  ui: KeyedMockChainParty;
  reactor: MockChainParty;
  oracle: KeyedMockChainParty;
  buyback: MockChainParty;
  /** The reactor state the world started with. */
  initial: ReactorState;
  /** A transaction context built from the current chain state. */
  ctx(overrides?: Partial<TxContext>): TxContext;
}

export function setup(profile: Profile, options: SetupOptions = {}): World {
  const chain = new MockChain({ height: HEIGHT });
  const treasury = chain.newParty("treasury");
  treasury.addBalance({ nanoergs: 10n * ONE });
  const base = reactorState(profile, publicKey(treasury), options.q);
  const initial = { ...base, ...options.state?.(base) };

  // The user holds every circulating neutron and proton, so that any action can be funded.
  const user = chain.newParty("user");
  user.addBalance({
    nanoergs: 100_000n * ONE,
    tokens: [
      { tokenId: TOKENS.neutron, amount: initial.totalNeutrons - initial.neutrons },
      { tokenId: TOKENS.proton, amount: initial.totalProtons - initial.protons },
    ],
  });
  const ui = chain.newParty("ui");
  const oracle = chain.newParty("oracle");
  const real = options.buybackBox;
  const buybackTree = real?.ergoTree ?? BUYBACK_TREE;
  const buyback = chain.addParty(buybackTree, "buyback");
  const constants = real
    ? { ...constantsFor(profile), oracleFeeTree: real.ergoTree, oracleBuybackNft: real.assets[0].tokenId }
    : constantsFor(profile);
  const tree = reactorTree(constants).toHex();
  const reactor = chain.addParty(tree, "reactor");

  if (!options.withoutReactor) {
    reactor.addUTxOs({
      value: initial.value,
      ergoTree: tree,
      creationHeight: HEIGHT - 10,
      assets: reactorAssets(initial, TOKENS),
      additionalRegisters: reactorRegisters(initial),
    });
  }
  oracle.addUTxOs({
    value: ONE,
    ergoTree: oracle.ergoTree,
    creationHeight: HEIGHT + 1 - (options.oracleAge ?? 5),
    assets: [{ tokenId: options.oracleNft ?? profile.poolNft, amount: 1n }],
    additionalRegisters: { R4: SLong(options.oraclePrice ?? profile.oraclePrice).toHex() },
  });
  buyback.addUTxOs(
    real
      ? real // as on chain: its box id is a hash of its content
      : {
          value: ONE,
          ergoTree: BUYBACK_TREE,
          creationHeight: HEIGHT - 100,
          assets: [{ tokenId: options.buybackNft ?? profile.buybackNft, amount: 1n }],
          additionalRegisters: {},
        },
  );

  return {
    chain,
    profile,
    user,
    treasury,
    ui,
    reactor,
    oracle,
    buyback,
    initial,
    ctx: (overrides = {}) => ({
      height: chain.height,
      minFee: MIN_FEE,
      reactor: reactor.utxos.at(0),
      oracle: oracle.utxos.at(0),
      inputs: user.utxos.toArray(),
      recipient: user.ergoTree,
      buyback: buyback.utxos.at(0),
      ...overrides,
    }),
  };
}

export type Tx = EIP12UnsignedTransaction;

function execute(w: World, tx: Tx, signers: KeyedMockChainParty[]): string | undefined {
  try {
    return w.chain.execute(tx, { signers, throw: true }) ? undefined : "rejected";
  } catch (e) {
    return e instanceof Error ? e.message : String(e);
  }
}

export function expectAccepted(w: World, tx: Tx, signers = [w.user]): void {
  expect(consensusIssues(tx, w.chain.height)).toEqual([]);
  expect(execute(w, tx, signers)).toBeUndefined();
}

export function expectRejected(w: World, tx: Tx, signers = [w.user]): void {
  const error = execute(w, tx, signers);
  expect(error).toBeDefined();
  // The contract must be what rejects the transaction, not token accounting.
  expect(error).not.toMatch(/burn|mint/i);
}

/** A copy of `tx` with `change` applied. */
export function mutate(tx: Tx, change: (copy: Tx) => void): Tx {
  const copy = structuredClone(tx);
  change(copy);
  return copy;
}

function addToken(output: BoxCandidate<string>, tokenId: string, delta: bigint): void {
  const asset = output.assets.find((a) => a.tokenId === tokenId);
  if (!asset) {
    output.assets.push({ tokenId, amount: delta.toString() });
    return;
  }
  const amount = BigInt(asset.amount) + delta;
  if (amount === 0n) output.assets = output.assets.filter((a) => a !== asset);
  else asset.amount = amount.toString();
}

/** Moves tokens between two outputs, so the transaction stays balanced. */
export function moveTokens(tx: Tx, tokenId: string, from: number, to: number, amount: bigint): void {
  addToken(tx.outputs[from], tokenId, -amount);
  addToken(tx.outputs[to], tokenId, amount);
}

/** Moves nanoERG between two outputs, so the transaction stays balanced. */
export function moveErg(tx: Tx, from: number, to: number, amount: bigint): void {
  tx.outputs[from].value = (BigInt(tx.outputs[from].value) - amount).toString();
  tx.outputs[to].value = (BigInt(tx.outputs[to].value) + amount).toString();
}

/** Index of the change output: the last output (other than OUTPUTS(1)) that pays to `tree`. */
export function changeIndex(tx: Tx, tree: string): number {
  const index = tx.outputs.findLastIndex((o, i) => i > 1 && o.ergoTree === tree);
  if (index < 0) throw new Error("the transaction has no change output");
  return index;
}

/** The reactor state an output encodes. */
export function stateOf(output: BoxCandidate<string>): ReactorState {
  return readReactor({ ...output, boxId: "", transactionId: "", index: 0 } as Box<Amount>).state;
}

/** Replaces the registers of OUTPUTS(0) with those of a modified reactor state. */
export function changeState(tx: Tx, change: (s: ReactorState) => Partial<ReactorState>): void {
  const s = stateOf(tx.outputs[0]);
  tx.outputs[0].additionalRegisters = reactorRegisters({ ...s, ...change(s) });
}
