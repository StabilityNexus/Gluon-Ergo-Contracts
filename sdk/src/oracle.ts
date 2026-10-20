import type { Amount, Box } from "@fleet-sdk/core";
import { parse } from "@fleet-sdk/serializer";
import { GluonError } from "./errors.ts";
import { MAX_ORACLE_AGE } from "./math.ts";

/** The price an oracle pool box publishes in R4. */
export function oraclePrice(box: Box<Amount>): bigint {
  const r4 = box.additionalRegisters.R4;
  if (!r4) throw new Error(`oracle box ${box.boxId} has no R4`);
  return parse<bigint>(r4);
}

/** The age of the oracle box in blocks relative to HEIGHT (height + 1). */
export function oracleAge(oracle: { creationHeight: number }, height: number): number {
  return height + 1 - oracle.creationHeight;
}

/** Throws ORACLE_STALE unless the contract would accept `oracle` in a transaction built at chain height `height`. */
export function requireFreshOracle(oracle: Box<Amount>, height: number): void {
  const age = oracleAge(oracle, height);
  if (age >= MAX_ORACLE_AGE || age < 0) {
    throw new GluonError(
      "ORACLE_STALE",
      `the oracle box ${oracle.boxId} is ${age} blocks old and the contract accepts at most ${MAX_ORACLE_AGE - 1}; wait for the oracle's next update`,
    );
  }
}
