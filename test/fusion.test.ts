import { describe, it } from "vitest";
import { ONE } from "../sdk/src/math.ts";
import { fusionTx } from "../sdk/src/transactions.ts";
import {
  MAX_DEV_FEE,
  PROFILES,
  TOKENS,
  type Tx,
  type World,
  changeIndex,
  changeState,
  expectAccepted,
  expectRejected,
  moveErg,
  moveTokens,
  mutate,
  publicKey,
  setup,
} from "./helpers.ts";

// Transaction layout: OUTPUTS(0) reactor, OUTPUTS(1) user, OUTPUTS(2) dev fee; the user's remaining tokens
// are in the change output. The tokens owed are rounded down, so ERG deviations of a few nanoERG may be
// accepted; the ERG mutations below therefore move 0.01 ERG.
const CENT = ONE / 100n;
const mutations: [string, (tx: Tx, w: World) => void][] = [
  ["the user returns one nano-neutron too few", (tx) => moveTokens(tx, TOKENS.neutron, 0, 1, 1n)],
  ["the user returns one extra nano-neutron", (tx, w) => moveTokens(tx, TOKENS.neutron, changeIndex(tx, w.user.ergoTree), 0, 1n)],
  ["the user returns one nano-proton too few", (tx) => moveTokens(tx, TOKENS.proton, 0, 1, 1n)],
  ["the user returns one extra nano-proton", (tx, w) => moveTokens(tx, TOKENS.proton, changeIndex(tx, w.user.ergoTree), 0, 1n)],
  ["the user withdraws 0.01 ERG more", (tx) => moveErg(tx, 0, 1, CENT)],
  ["the user withdraws 0.01 ERG less", (tx) => moveErg(tx, 1, 0, CENT)],
  ["the dev fee is 1 nanoERG short", (tx) => moveErg(tx, 2, 1, 1n)],
  ["the dev fee goes to another address", (tx, w) => void (tx.outputs[2].ergoTree = w.user.ergoTree)],
  ["R6 does not record the dev fee", (tx) => changeState(tx, (s) => ({ devFeeRepaid: s.devFeeRepaid - 1n }))],
  ["R6 changes the maximum dev fee", (tx) => changeState(tx, (s) => ({ maxDevFee: s.maxDevFee + 1n }))],
  ["R4 changes", (tx) => changeState(tx, (s) => ({ totalProtons: s.totalProtons + 1n }))],
  ["R5 changes", (tx, w) => changeState(tx, () => ({ treasury: publicKey(w.user) }))],
  ["R7 changes", (tx) => changeState(tx, (s) => ({ volumePlus: [1n, ...s.volumePlus.slice(1)] }))],
  ["R8 changes", (tx) => changeState(tx, (s) => ({ volumeMinus: [1n, ...s.volumeMinus.slice(1)] }))],
  ["R9's bucket block changes", (tx) => changeState(tx, (s) => ({ lastBucketBlock: s.lastBucketBlock - 720n }))],
  ["R9's peg factor changes", (tx) => changeState(tx, (s) => ({ pegFactor: s.pegFactor - 1n }))],
  ["neutrons and protons swap places", (tx) => tx.outputs[0].assets.splice(1, 2, tx.outputs[0].assets[2], tx.outputs[0].assets[1])],
  ["the reactor moves to another contract", (tx, w) => void (tx.outputs[0].ergoTree = w.user.ergoTree)],
  ["there is no oracle data input", (tx) => void (tx.dataInputs = [])],
  ["another box precedes the oracle", (tx, w) => void tx.dataInputs.unshift(w.buyback.utxos.at(0) as never)],
];

describe.each(PROFILES)("Fusion ($name)", (profile) => {
  it.each([CENT, ONE, 100n * ONE])("accepts a valid fusion of %s nanoERG", (erg) => {
    const w = setup(profile);
    expectAccepted(w, fusionTx(w.ctx(), erg));
  });

  it("accepts a valid fusion that pays a UI fee", () => {
    const w = setup(profile);
    expectAccepted(w, fusionTx(w.ctx({ uiFeeKey: publicKey(w.ui) }), ONE));
  });

  it("accepts a valid fusion once the dev fee is fully repaid", () => {
    const w = setup(profile, { state: () => ({ devFeeRepaid: MAX_DEV_FEE }) });
    expectAccepted(w, fusionTx(w.ctx({ uiFeeKey: publicKey(w.ui) }), ONE));
  });

  describe("rejects a fusion when", () => {
    it.each(mutations)("%s", (_, change) => {
      const w = setup(profile);
      expectRejected(w, mutate(fusionTx(w.ctx(), ONE), (tx) => change(tx, w)));
    });

    it("the reactor is below the healthy range (q = 0.49)", () => {
      const w = setup(profile, { q: (49n * ONE) / 100n });
      expectRejected(w, fusionTx(w.ctx(), ONE));
    });

    it("the reactor is above the healthy range (q = 0.99)", () => {
      const w = setup(profile, { q: (99n * ONE) / 100n });
      expectRejected(w, fusionTx(w.ctx(), ONE));
    });

    it("the oracle box holds another NFT", () => {
      const w = setup(profile, { oracleNft: "ee".repeat(32) });
      expectRejected(w, fusionTx(w.ctx(), ONE));
    });

    it("the oracle box is 35 blocks old", () => {
      const w = setup(profile, { oracleAge: 35 });
      expectRejected(w, fusionTx(w.ctx(), ONE));
    });
  });
});
