import { readFileSync } from "node:fs";
import type { Amount, Box } from "@fleet-sdk/core";
import { describe, it } from "vitest";
import { betaDecayMinusTx, betaDecayPlusTx } from "../sdk/src/transactions.ts";
import { DOLLAR, type Tx, expectAccepted, expectRejected, mutate, setup } from "./helpers.ts";

// The Dollar deployment pays the oracle fee into the USD oracle's DORT buyback box. These tests use that
// box as it was on mainnet (fixture), so that the buyback contract's own script runs too.
const dortBuyback: Box<Amount> = JSON.parse(readFileSync(new URL("fixtures/dort-buyback-box.json", import.meta.url), "utf8"));

describe("Oracle fee paid to the live DORT buyback contract (Dollar)", () => {
  it.each([
    ["Beta decay +", betaDecayPlusTx, "protons"],
    ["Beta decay −", betaDecayMinusTx, "neutrons"],
  ] as const)("%s pays the oracle fee by topping the buyback box up", (_, build, token) => {
    const w = setup(DOLLAR, { buybackBox: dortBuyback });
    const circulating = token === "protons" ? w.initial.totalProtons - w.initial.protons : w.initial.totalNeutrons - w.initial.neutrons;
    expectAccepted(w, build(w.ctx(), circulating / 100n));
  });

  it.each([
    ["without the top-up context variable", (tx: Tx) => void (tx.inputs[tx.inputs.length - 1].extension = {})],
    ["if the buyback box's successor does not record the box it replaces", (tx: Tx) => void (tx.outputs[2].additionalRegisters = {})],
  ])("is rejected by the buyback contract %s", (_, change) => {
    const w = setup(DOLLAR, { buybackBox: dortBuyback });
    const tx = betaDecayMinusTx(w.ctx(), (w.initial.totalNeutrons - w.initial.neutrons) / 100n);
    expectRejected(w, mutate(tx, change));
  });
});
