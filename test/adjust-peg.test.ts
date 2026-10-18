import { describe, expect, it } from "vitest";
import { ONE, isHealthy, pegAdjustments, snapshot } from "../sdk/src/math.ts";
import { readReactor } from "../sdk/src/reactor.ts";
import { adjustPegTx, reactorTx } from "../sdk/src/transactions.ts";
import {
  MIN_FEE,
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

const ABOVE = (12n * ONE) / 10n; // q = 1.2: the peg factor must drop by 1 %
const BELOW = (4n * ONE) / 10n; // q = 0.4: the peg factor must rise by 1 %

// Transaction layout: OUTPUTS(0) reactor, then the caller's change and the miner fee.
const mutations: [string, (tx: Tx, w: World) => void][] = [
  ["R9's bucket block changes", (tx) => changeState(tx, (s) => ({ lastBucketBlock: s.lastBucketBlock + 720n }))],
  ["the reactor's ERG value changes", (tx) => moveErg(tx, 0, 1, 1n)],
  ["the reactor's tokens change", (tx) => moveTokens(tx, TOKENS.neutron, 0, 1, 1n)],
  ["R4 changes", (tx) => changeState(tx, (s) => ({ totalNeutrons: s.totalNeutrons + 1n }))],
  ["R5 changes", (tx, w) => changeState(tx, () => ({ treasury: publicKey(w.user) }))],
  ["R6 changes", (tx) => changeState(tx, (s) => ({ devFeeRepaid: s.devFeeRepaid + 1n }))],
  ["R7 changes", (tx) => changeState(tx, (s) => ({ volumePlus: [1n, ...s.volumePlus.slice(1)] }))],
  ["R8 changes", (tx) => changeState(tx, (s) => ({ volumeMinus: [1n, ...s.volumeMinus.slice(1)] }))],
  ["the reactor moves to another contract", (tx, w) => void (tx.outputs[0].ergoTree = w.user.ergoTree)],
  ["there is no oracle data input", (tx) => void (tx.dataInputs = [])],
];

describe.each(PROFILES)("Adjust Peg ($name)", (profile) => {
  it("lowers the peg factor by 1 % when q > 0.98", () => {
    const w = setup(profile, { q: ABOVE });
    const tx = adjustPegTx(w.ctx());
    expect(stateOf(tx.outputs[0]).pegFactor).toBe((profile.pegFactor * 99n) / 100n);
    expectAccepted(w, tx);
  });

  it("raises the peg factor by 1 % when q < 0.50", () => {
    const w = setup(profile, { q: BELOW });
    const tx = adjustPegTx(w.ctx());
    expect(stateOf(tx.outputs[0]).pegFactor).toBe((profile.pegFactor * 101n) / 100n);
    expectAccepted(w, tx);
  });

  it.each([ABOVE, BELOW])("rounds a peg factor that is not a multiple of 100 down (q = %s)", (q) => {
    const w = setup(profile, { q, state: (s) => ({ pegFactor: s.pegFactor + 37n }) });
    expectAccepted(w, adjustPegTx(w.ctx()));
  });

  it("chains adjustments until the reactor is healthy again, as pegAdjustments predicts", () => {
    const w = setup(profile, { q: (103n * ONE) / 100n });
    const state = () => readReactor(w.ctx().reactor).state;
    const predicted = pegAdjustments(state(), MIN_FEE, profile.oraclePrice);
    let steps = 0;
    while (!isHealthy(snapshot(state(), MIN_FEE, profile.oraclePrice).q)) {
      expectAccepted(w, adjustPegTx(w.ctx()));
      steps++;
      expect(steps).toBeLessThan(10);
    }
    expect(steps).toBe(5);
    expect(predicted).toHaveLength(5);
    expect(predicted.at(-1)).toEqual(state());
    expect(pegAdjustments(state(), MIN_FEE, profile.oraclePrice)).toEqual([]);
  });

  describe("rejects an adjustment when", () => {
    it("the reactor is in the healthy range", () => {
      const w = setup(profile);
      const s = w.initial;
      expect(() => adjustPegTx(w.ctx())).toThrow(/healthy range/);
      expectRejected(w, reactorTx(w.ctx(), { ...s, pegFactor: (s.pegFactor * 99n) / 100n }));
      expectRejected(w, reactorTx(w.ctx(), { ...s, pegFactor: (s.pegFactor * 101n) / 100n }));
    });

    it("the factor moves by 2 %", () => {
      const w = setup(profile, { q: ABOVE });
      expectRejected(w, reactorTx(w.ctx(), { ...w.initial, pegFactor: (w.initial.pegFactor * 98n) / 100n }));
    });

    it("the factor moves in the wrong direction", () => {
      const w = setup(profile, { q: ABOVE });
      expectRejected(w, reactorTx(w.ctx(), { ...w.initial, pegFactor: (w.initial.pegFactor * 101n) / 100n }));
    });

    it.each(mutations)("%s", (_, change) => {
      const w = setup(profile, { q: ABOVE });
      expectRejected(w, mutate(adjustPegTx(w.ctx()), (tx) => change(tx, w)));
    });

    it("the oracle box holds another NFT", () => {
      const w = setup(profile, { q: ABOVE, oracleNft: "ee".repeat(32) });
      expectRejected(w, adjustPegTx(w.ctx()));
    });

    it("the oracle box is 35 blocks old", () => {
      const w = setup(profile, { q: ABOVE, oracleAge: 35 });
      expectRejected(w, adjustPegTx(w.ctx()));
    });
  });
});
