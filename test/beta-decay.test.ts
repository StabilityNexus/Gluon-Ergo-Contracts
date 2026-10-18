import { describe, expect, it } from "vitest";
import {
  BUCKETS,
  ONE,
  Q_LOWER,
  Q_UPPER,
  type ReactorState,
  betaDecayMinus,
  betaDecayPlus,
  bucketStart,
  snapshot,
  valueOfNeutrons,
  valueOfProtons,
} from "../sdk/src/math.ts";
import { type TxContext, betaDecayMinusTx, betaDecayPlusTx } from "../sdk/src/transactions.ts";
import {
  HEIGHT,
  MAX_DEV_FEE,
  MIN_FEE,
  PROFILES,
  type Profile,
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
} from "./helpers.ts";

// Beta decay + turns protons into neutrons; beta decay − turns neutrons into protons. Both record their
// value in a 14-bucket volume register (R7 for +, R8 for −) that sets the volume-dependent fee.
// Transaction layout: OUTPUTS(0) reactor, OUTPUTS(1) user, OUTPUTS(2) oracle fee (the recreated buyback
// box, whose input is the last input), OUTPUTS(3) dev fee, OUTPUTS(4) UI fee.
interface Direction {
  name: string;
  tx: (ctx: TxContext, amount: bigint) => Tx;
  result: typeof betaDecayPlus;
  tokenIn: string;
  tokenOut: string;
  /** The reactor's holding of the token going out. */
  out: "neutrons" | "protons";
  volume: "volumePlus" | "volumeMinus";
  other: "volumePlus" | "volumeMinus";
  /** A decay of 1 % of the circulating supply of the token going in. */
  amount: (s: ReactorState) => bigint;
  /** Value of `amount` units of the token going in, as the contract computes it. */
  worth: (s: ReactorState, profile: Profile, amount: bigint) => bigint;
  /** A fusion ratio close to the bound that this direction moves towards. */
  nearBound: bigint;
  pastBound: (q: bigint) => boolean;
}

const PLUS: Direction = {
  name: "Beta decay +",
  tx: betaDecayPlusTx,
  result: betaDecayPlus,
  tokenIn: TOKENS.proton,
  tokenOut: TOKENS.neutron,
  out: "neutrons",
  volume: "volumePlus",
  other: "volumeMinus",
  amount: (s) => (s.totalProtons - s.protons) / 100n,
  worth: (s, p, amount) => valueOfProtons(snapshot(s, MIN_FEE, p.oraclePrice), amount),
  nearBound: (97n * ONE) / 100n,
  pastBound: (q) => q > Q_UPPER,
};

const MINUS: Direction = {
  name: "Beta decay −",
  tx: betaDecayMinusTx,
  result: betaDecayMinus,
  tokenIn: TOKENS.neutron,
  tokenOut: TOKENS.proton,
  out: "protons",
  volume: "volumeMinus",
  other: "volumePlus",
  amount: (s) => (s.totalNeutrons - s.neutrons) / 100n,
  worth: (s, p, amount) => valueOfNeutrons(snapshot(s, MIN_FEE, p.oraclePrice), amount),
  nearBound: (51n * ONE) / 100n,
  pastBound: (q) => q < Q_LOWER,
};

/** Volume history of the last 14 buckets: 1..14 ERG for decays +, 0.5..7 ERG for decays −. */
const history = (s: ReactorState): Partial<ReactorState> => ({
  volumePlus: Array.from({ length: BUCKETS }, (_, i) => BigInt(i + 1) * ONE),
  volumeMinus: Array.from({ length: BUCKETS }, (_, i) => (BigInt(i + 1) * ONE) / 2n),
  lastBucketBlock: s.lastBucketBlock,
});

/** The fusion ratio after applying a decay of `amount`. */
function qAfter(d: Direction, w: World, s: ReactorState, amount: bigint): bigint {
  const { next } = d.result(s, MIN_FEE, w.profile.oraclePrice, amount, HEIGHT + 1);
  return snapshot(next, MIN_FEE, w.profile.oraclePrice).q;
}

describe.each(PROFILES)("$name", (profile) => {
  describe.each([PLUS, MINUS])("$name", (d) => {
    const mutations: [string, (tx: Tx, w: World) => void][] = [
      ["the user takes one extra unit of the token going out", (tx) => moveTokens(tx, d.tokenOut, 0, 1, 1n)],
      ["the user takes one unit too few of the token going out", (tx) => moveTokens(tx, d.tokenOut, 1, 0, 1n)],
      ["the user sends one unit too few of the token going in", (tx) => moveTokens(tx, d.tokenIn, 0, 1, 1n)],
      ["the reactor's ERG value changes", (tx) => moveErg(tx, 0, 1, 1n)],
      ["the volume register records 1 nanoERG more", (tx) => changeState(tx, (s) => ({ [d.volume]: [s[d.volume][0] + 1n, ...s[d.volume].slice(1)] }))],
      ["the other volume register changes", (tx) => changeState(tx, (s) => ({ [d.other]: [s[d.other][0] + 1n, ...s[d.other].slice(1)] }))],
      ["the volume register gets a 15th bucket", (tx) => changeState(tx, (s) => ({ [d.volume]: [...s[d.volume], 0n] }))],
      ["R9's bucket block is not the current bucket", (tx) => changeState(tx, (s) => ({ lastBucketBlock: s.lastBucketBlock - 720n }))],
      ["R9's peg factor changes", (tx) => changeState(tx, (s) => ({ pegFactor: s.pegFactor + 1n }))],
      ["R4 changes", (tx) => changeState(tx, (s) => ({ totalNeutrons: s.totalNeutrons + 1n }))],
      ["R5 changes", (tx, w) => changeState(tx, () => ({ treasury: publicKey(w.user) }))],
      ["R6 does not record the dev fee", (tx) => changeState(tx, (s) => ({ devFeeRepaid: s.devFeeRepaid - 1n }))],
      ["the oracle fee is 1 nanoERG short", (tx) => moveErg(tx, 2, 1, 1n)],
      ["the oracle fee goes to another script", (tx, w) => void (tx.outputs[2].ergoTree = w.user.ergoTree)],
      ["the buyback box is not the last input", (tx) => void tx.inputs.splice(1, 0, tx.inputs.pop() as Tx["inputs"][number])],
      ["the dev fee is 1 nanoERG short", (tx) => moveErg(tx, 3, 1, 1n)],
      ["the reactor moves to another contract", (tx, w) => void (tx.outputs[0].ergoTree = w.user.ergoTree)],
      ["there is no oracle data input", (tx) => void (tx.dataInputs = [])],
    ];

    it.each([0, 1, 13, 14, 30])("accepts a valid decay %i buckets after the last one", (elapsed) => {
      const w = setup(profile, {
        state: (s) => ({ ...history(s), lastBucketBlock: bucketStart(HEIGHT + 1) - BigInt(elapsed * 720) }),
      });
      expectAccepted(w, d.tx(w.ctx(), d.amount(w.initial)));
    });

    it.each([
      [false, false],
      [true, false],
      [false, true],
      [true, true],
    ])("accepts a valid decay with dev fee repaid = %s and UI fee = %s", (repaid, withUi) => {
      const w = setup(profile, { state: (s) => ({ ...history(s), devFeeRepaid: repaid ? MAX_DEV_FEE : 0n }) });
      const ctx = w.ctx(withUi ? { uiFeeKey: publicKey(w.ui) } : {});
      expectAccepted(w, d.tx(ctx, d.amount(w.initial)));
    });

    it("accepts a decay too small to owe an oracle fee (the dev fee is then at OUTPUTS(2))", () => {
      const w = setup(profile);
      let amount = 1n;
      while (d.worth(w.initial, profile, amount * 2n) < 1000n) amount *= 2n;
      // The contract needs the token going out to decrease, so the amount must release at least one unit.
      const released = (a: bigint) => w.initial[d.out] - d.result(w.initial, MIN_FEE, profile.oraclePrice, a, HEIGHT + 1).next[d.out];
      while (released(amount) === 0n) amount += 1n;
      expect(d.worth(w.initial, profile, amount)).toBeLessThan(1000n);
      const tx = d.tx(w.ctx(), amount);
      expect(tx.outputs[2].ergoTree).toBe(w.treasury.ergoTree);
      expectAccepted(w, tx);
    });

    describe("rejects a decay when", () => {
      it.each(mutations)("%s", (_, change) => {
        const w = setup(profile, { state: history });
        expectRejected(w, mutate(d.tx(w.ctx(), d.amount(w.initial)), (tx) => change(tx, w)));
      });

      it("the buyback box holds another NFT", () => {
        const w = setup(profile, { buybackNft: "ee".repeat(32) });
        expectRejected(w, d.tx(w.ctx(), d.amount(w.initial)));
      });

      it("the reactor is outside the healthy range before the decay", () => {
        const w = setup(profile, { q: d === PLUS ? (99n * ONE) / 100n : (49n * ONE) / 100n });
        expectRejected(w, d.tx(w.ctx(), d.amount(w.initial)));
      });

      it("the decay would leave the reactor outside the healthy range (Security Finding 1)", () => {
        const w = setup(profile, { q: d.nearBound });
        let amount = d.amount(w.initial);
        while (!d.pastBound(qAfter(d, w, w.initial, amount))) amount *= 2n;
        // The same decay, small enough to keep the reactor healthy, is accepted.
        const small = setup(profile, { q: d.nearBound });
        expectAccepted(small, d.tx(small.ctx(), d.amount(small.initial)));
        expectRejected(w, d.tx(w.ctx(), amount));
      });
    });
  });
});
