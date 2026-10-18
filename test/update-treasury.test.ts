import { describe, expect, it } from "vitest";
import { reactorTx, updateTreasuryTx } from "../sdk/src/transactions.ts";
import {
  PROFILES,
  TOKENS,
  type Tx,
  type World,
  changeState,
  expectAccepted,
  expectRejected,
  moveErg,
  moveTokens,
  mutate,
  publicKey,
  setup,
  stateOf,
} from "./helpers.ts";

// The treasury is a single-signature key. The contract recognises Update Treasury by INPUTS(1): a box
// of the current treasury's address. Transaction layout: OUTPUTS(0) reactor, then change and miner fee.
const treasuryCtx = (w: World) => w.ctx({ inputs: w.treasury.utxos.toArray(), recipient: w.treasury.ergoTree });

const mutations: [string, (tx: Tx, w: World) => void][] = [
  ["the reactor's ERG value changes", (tx) => moveErg(tx, 0, 1, 1n)],
  ["the reactor's tokens change", (tx) => moveTokens(tx, TOKENS.proton, 0, 1, 1n)],
  ["R4 changes", (tx) => changeState(tx, (s) => ({ totalProtons: s.totalProtons + 1n }))],
  ["R6 changes", (tx) => changeState(tx, (s) => ({ devFeeRepaid: s.devFeeRepaid + 1n }))],
  ["R7 changes", (tx) => changeState(tx, (s) => ({ volumePlus: [1n, ...s.volumePlus.slice(1)] }))],
  ["R8 changes", (tx) => changeState(tx, (s) => ({ volumeMinus: [1n, ...s.volumeMinus.slice(1)] }))],
  ["R9 changes", (tx) => changeState(tx, (s) => ({ lastBucketBlock: s.lastBucketBlock + 720n }))],
  ["the reactor moves to another contract (it is immutable, 6e9704b)", (tx, w) => void (tx.outputs[0].ergoTree = w.user.ergoTree)],
  ["there is no oracle data input (see the contract header)", (tx) => void (tx.dataInputs = [])],
];

describe.each(PROFILES)("Update Treasury ($name)", (profile) => {
  it("accepts a new treasury key signed by the current treasury", () => {
    const w = setup(profile);
    const tx = updateTreasuryTx(treasuryCtx(w), publicKey(w.ui));
    expect(stateOf(tx.outputs[0]).treasury).toBe(publicKey(w.ui));
    expectAccepted(w, tx, [w.treasury]);
  });

  describe("rejects an update when", () => {
    it.each(mutations)("%s", (_, change) => {
      const w = setup(profile);
      expectRejected(w, mutate(updateTreasuryTx(treasuryCtx(w), publicKey(w.ui)), (tx) => change(tx, w)), [w.treasury]);
    });

    it("R5 does not change", () => {
      const w = setup(profile);
      expectRejected(w, reactorTx(treasuryCtx(w), w.initial), [w.treasury]);
    });

    it("INPUTS(1) is not a treasury box", () => {
      const w = setup(profile);
      expectRejected(w, reactorTx(w.ctx(), { ...w.initial, treasury: publicKey(w.user) }));
    });

    it("the treasury does not sign", () => {
      const w = setup(profile);
      expectRejected(w, updateTreasuryTx(treasuryCtx(w), publicKey(w.ui)), [w.user]);
    });
  });
});
