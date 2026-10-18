import type { Amount, Box } from "@fleet-sdk/core";
import { ErgoAddress } from "@fleet-sdk/core";
import { mockUTxO } from "@fleet-sdk/mock-chain";
import { describe, expect, it } from "vitest";
import { bootstrapTransactions, deploymentRecord, planBootstrap, publicKeyOf } from "../lib/bootstrap.ts";
import { compilerVersion } from "../lib/compile.ts";
import { bootstrapParameters, contractConstants, readDeployment } from "../lib/config.ts";
import { deploymentFromRecord } from "../sdk/src/deployments/fromRecord.ts";
import { GluonError } from "../sdk/src/errors.ts";
import { crashCushion, protonLeverage } from "../sdk/src/math.ts";
import { oracleAge, oraclePrice, requireFreshOracle } from "../sdk/src/oracle.ts";
import { readReactor } from "../sdk/src/reactor.ts";
import { BUYBACK_TREE, GOLD, MIN_FEE, reactorTree, setup } from "./helpers.ts";

describe("SDK Math and Oracle (A3)", () => {
  it("protonLeverage is 1.5 at q = 0.6 and undefined from q = 0.99", () => {
    // q = 0.6 (600_000_000n): 0.6 / (1 - 0.6) = 1.5
    expect(protonLeverage(600_000_000n)).toBeCloseTo(1.5, 6);

    // q = 0.99 (990_000_000n): undefined
    expect(protonLeverage(990_000_000n)).toBeUndefined();

    // q = 1.0 (1_000_000_000n): undefined
    expect(protonLeverage(1_000_000_000n)).toBeUndefined();

    // q = 1.2 (1_200_000_000n): undefined
    expect(protonLeverage(1_200_000_000n)).toBeUndefined();
  });

  it("both crash cushions at q = 0.6, 0.98 and 1.2", () => {
    // At q = 0.6:
    // untilDepeg = 1 - 0.6 / 0.99 = 39 / 99 ≈ 0.393939
    // untilAdjustment = 1 - 0.6 / 0.98 = 38 / 98 ≈ 0.387755
    const c06 = crashCushion(600_000_000n);
    expect(c06.untilDepeg).toBeCloseTo(39 / 99, 6);
    expect(c06.untilAdjustment).toBeCloseTo(38 / 98, 6);

    // At q = 0.98:
    // untilDepeg = 1 - 0.98 / 0.99 = 1 / 99 ≈ 0.010101
    // untilAdjustment = 1 - 0.98 / 0.98 = 0
    const c098 = crashCushion(980_000_000n);
    expect(c098.untilDepeg).toBeCloseTo(1 / 99, 6);
    expect(c098.untilAdjustment).toBe(0);

    // At q = 1.2:
    // both clamped to 0
    const c12 = crashCushion(1_200_000_000n);
    expect(c12.untilDepeg).toBe(0);
    expect(c12.untilAdjustment).toBe(0);
  });

  it("readReactor throws for a wrong ErgoTree or a wrong token id", () => {
    const w = setup(GOLD, { withoutReactor: true });
    const config = readDeployment(new URL("../config/gold.json", import.meta.url));
    config.oracle.poolNft = GOLD.poolNft;
    config.oracle.buybackNft = GOLD.buybackNft;
    config.oracle.buybackContractAddress = ErgoAddress.fromErgoTree(BUYBACK_TREE).encode();
    config.treasuryAddress = w.treasury.address.encode();
    config.bootstrap.seedErg = "10";
    config.tokens.nft = { ticker: "TNFT", name: "Test Reactor NFT", description: "Identifies the test reactor box." };
    config.tokens.neutron = { ticker: "TNEU", name: "Test Neutron", description: "Pegged test token." };
    config.tokens.proton = { ticker: "TPRO", name: "Test Proton", description: "Leveraged test token." };

    const tree = reactorTree(contractConstants(config));
    const price = oraclePrice(w.oracle.utxos.at(0));
    const plan = planBootstrap(bootstrapParameters(config), MIN_FEE, price, publicKeyOf(config.treasuryAddress), w.chain.height);
    const txs = bootstrapTransactions(config, plan, tree.toHex(), w.user.utxos.toArray(), w.user.address.encode(), w.chain.height);
    const record = deploymentRecord(config, plan, price, tree, txs, "0".repeat(40), compilerVersion());
    const deployment = deploymentFromRecord(record);

    const validReactorBox = mockUTxO(txs.createReactor.outputs[0]);

    // Passes when deployment matches
    const parsed = readReactor(validReactorBox, deployment);
    expect(parsed.tokens).toEqual(txs.tokens);

    // Passes when called without deployment
    const parsedWithoutDep = readReactor(validReactorBox);
    expect(parsedWithoutDep.tokens).toEqual(txs.tokens);

    // Throws for wrong ErgoTree when deployment provided
    const wrongTreeBox = {
      ...validReactorBox,
      ergoTree: "0008cd" + "11".repeat(33),
    };
    expect(() => readReactor(wrongTreeBox, deployment)).toThrow(/wrong ErgoTree/);

    // But succeeds without deployment
    expect(() => readReactor(wrongTreeBox)).not.toThrow();

    // Throws for wrong token id when deployment provided
    const wrongTokenBox = {
      ...validReactorBox,
      assets: [
        { tokenId: "00".repeat(32), amount: 1n },
        validReactorBox.assets[1],
        validReactorBox.assets[2],
      ],
    };
    expect(() => readReactor(wrongTokenBox, deployment)).toThrow(/wrong token ids/);
  });

  it("oracleAge and requireFreshOracle at ages 34, 35 and -1", () => {
    const mockOracle = (creationHeight: number): Box<Amount> => ({
      boxId: "oracle-box-id",
      value: 1_000_000n,
      ergoTree: "0008cd",
      creationHeight,
      assets: [],
      additionalRegisters: {},
      transactionId: "tx-id",
      index: 0,
    });

    const oracle = mockOracle(100);

    // Age 34: height = 133 (HEIGHT = 134, age = 134 - 100 = 34)
    expect(oracleAge(oracle, 133)).toBe(34);
    expect(() => requireFreshOracle(oracle, 133)).not.toThrow();

    // Age 35: height = 134 (HEIGHT = 135, age = 135 - 100 = 35)
    expect(oracleAge(oracle, 134)).toBe(35);
    expect(() => requireFreshOracle(oracle, 134)).toThrowError(GluonError);
    try {
      requireFreshOracle(oracle, 134);
    } catch (e) {
      expect(e).toBeInstanceOf(GluonError);
      expect((e as GluonError).code).toBe("ORACLE_STALE");
      expect((e as GluonError).message).toContain("is 35 blocks old");
    }

    // Age -1: height = 98 (HEIGHT = 99, age = 99 - 100 = -1)
    expect(oracleAge(oracle, 98)).toBe(-1);
    expect(() => requireFreshOracle(oracle, 98)).toThrowError(GluonError);
    try {
      requireFreshOracle(oracle, 98);
    } catch (e) {
      expect(e).toBeInstanceOf(GluonError);
      expect((e as GluonError).code).toBe("ORACLE_STALE");
      expect((e as GluonError).message).toContain("is -1 blocks old");
    }
  });
});
