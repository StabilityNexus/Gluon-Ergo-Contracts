import { describe, it } from "vitest";
import { ONE, type ReactorState } from "../sdk/src/math.ts";
import { reactorTx } from "../sdk/src/transactions.ts";
import { PROFILES, expectRejected, setup } from "./helpers.ts";

// Port of the Scala HackSpec: spending the reactor without performing any of its transaction types.
// Whatever the reactor releases goes to the attacker's change output.
const attacks: [string, (s: ReactorState) => Partial<ReactorState>][] = [
  ["take 1 ERG", (s) => ({ value: s.value - ONE })],
  ["take 1 proton", (s) => ({ protons: s.protons - ONE })],
  ["take 1 neutron", (s) => ({ neutrons: s.neutrons - ONE })],
  ["take 1 ERG and 1 proton", (s) => ({ value: s.value - ONE, protons: s.protons - ONE })],
  ["take 1 ERG and 1 neutron", (s) => ({ value: s.value - ONE, neutrons: s.neutrons - ONE })],
  ["take 1 proton and 1 neutron", (s) => ({ protons: s.protons - ONE, neutrons: s.neutrons - ONE })],
  ["take 1 ERG, 1 proton and 1 neutron", (s) => ({ value: s.value - ONE, protons: s.protons - ONE, neutrons: s.neutrons - ONE })],
  ["change the dev-fee record while keeping tokens and ERG", (s) => ({ devFeeRepaid: s.maxDevFee })],
  ["change the volume history while keeping tokens and ERG", (s) => ({ volumeMinus: s.volumeMinus.map(() => ONE) })],
  ["change the total supplies while keeping tokens and ERG", (s) => ({ totalNeutrons: s.totalNeutrons * 2n })],
];

describe.each(PROFILES)("Attacks on the reactor ($name)", (profile) => {
  it.each(attacks)("cannot %s", (_, attack) => {
    const w = setup(profile);
    expectRejected(w, reactorTx(w.ctx(), { ...w.initial, ...attack(w.initial) }));
  });
});
