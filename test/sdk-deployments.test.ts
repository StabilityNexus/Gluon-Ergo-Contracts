import { execFileSync } from "node:child_process";
import { readFileSync, writeFileSync } from "node:fs";
import { fileURLToPath } from "node:url";
import { ErgoAddress } from "@fleet-sdk/core";
import { SLong } from "@fleet-sdk/serializer";
import { describe, expect, it } from "vitest";
import { bootstrapTransactions, deploymentRecord, planBootstrap, publicKeyOf } from "../lib/bootstrap.ts";
import { compilerVersion } from "../lib/compile.ts";
import { bootstrapParameters, contractConstants, readDeployment } from "../lib/config.ts";
import { SDK_DATA_TS, renderSdkData } from "../scripts/build-sdk-data.ts";
import { DEPLOYMENTS, deploymentFromRecord, getDeployment } from "../sdk/src/deployments/index.ts";
import { GluonError } from "../sdk/src/errors.ts";
import { oraclePrice } from "../sdk/src/reactor.ts";
import { BUYBACK_TREE, GOLD, MIN_FEE, reactorTree, setup } from "./helpers.ts";

describe("SDK Deployments (A2)", () => {
  it("a v3 record made the way test/bootstrap.test.ts makes one parses to the expected GluonDeployment, with its initial peg factor", () => {
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
    expect(deployment.name).toBe(config.deployment);
    expect(deployment.status).toBe("live");
    expect(deployment.peg).toBe(config.peg);
    expect(deployment.ergoTree).toBe(record.ergoTree);
    expect(deployment.address).toBe(record.address);
    expect(deployment.minFee).toBe(BigInt(record.constants._MinFee));
    expect(deployment.oraclePoolNft).toBe(record.constants._OraclePoolNFT);
    expect(deployment.oracleBuybackNft).toBe(record.constants._OracleBuybackNFT);
    expect(deployment.oracleFeeTree).toBe(record.constants._OracleFeePk);
    expect(deployment.tokens.nft).toEqual({
      id: record.tokens.nft.id,
      name: "TNFT",
      decimals: 0,
      emission: 1n,
    });
    expect(deployment.tokens.neutron).toEqual({
      id: record.tokens.neutron.id,
      name: "TNEU",
      decimals: 9,
      emission: BigInt(record.tokens.neutron.emission),
    });
    expect(deployment.tokens.proton).toEqual({
      id: record.tokens.proton.id,
      name: "TPRO",
      decimals: 9,
      emission: BigInt(record.tokens.proton.emission),
    });
    expect(deployment.initialPegFactor).toBe(GOLD.pegFactor);
  });

  it("a record whose R9 is a plain Long throws INVALID_DEPLOYMENT, as does a record without constants._MinFee", () => {
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

    // Record whose R9 is a plain Long
    const badR9Record = {
      ...record,
      genesis: {
        ...record.genesis,
        registers: {
          ...record.genesis.registers,
          R9: SLong(1_000_000n).toHex(),
        },
      },
    };
    expect(() => deploymentFromRecord(badR9Record)).toThrowError(GluonError);
    try {
      deploymentFromRecord(badR9Record);
    } catch (e) {
      expect(e).toBeInstanceOf(GluonError);
      expect((e as GluonError).code).toBe("INVALID_DEPLOYMENT");
    }

    // Record without constants._MinFee
    const { _MinFee: _, ...constantsWithoutMinFee } = record.constants;
    const badMinFeeRecord = {
      ...record,
      constants: constantsWithoutMinFee,
    };
    expect(() => deploymentFromRecord(badMinFeeRecord)).toThrowError(GluonError);
    try {
      deploymentFromRecord(badMinFeeRecord);
    } catch (e) {
      expect(e).toBeInstanceOf(GluonError);
      expect((e as GluonError).code).toBe("INVALID_DEPLOYMENT");
    }
  });

  it("run on the real deployments/mainnet.json, the generator keeps none of its four records", () => {
    const rendered = renderSdkData();
    expect(rendered).toContain("export default [];");
    expect(DEPLOYMENTS.length).toBe(0);
  });

  it("getDeployment of an unknown name throws UNKNOWN_DEPLOYMENT", () => {
    expect(() => getDeployment("unknown-deployment")).toThrowError(GluonError);
    try {
      getDeployment("unknown-deployment");
    } catch (e) {
      expect(e).toBeInstanceOf(GluonError);
      expect((e as GluonError).code).toBe("UNKNOWN_DEPLOYMENT");
    }
  });

  it("--check fails after a one-character change to data.ts", () => {
    const original = readFileSync(SDK_DATA_TS, "utf8");
    const scriptPath = fileURLToPath(new URL("../scripts/build-sdk-data.ts", import.meta.url));
    try {
      writeFileSync(SDK_DATA_TS, original + " ");
      expect(() =>
        execFileSync(process.execPath, [scriptPath, "--check"], {
          encoding: "utf8",
          stdio: "pipe",
        }),
      ).toThrow();
    } finally {
      writeFileSync(SDK_DATA_TS, original);
    }
  });

  it("implements every GluonErrorCode required by §5.4", () => {
    const expectedCodes = [
      "REACTOR_UNHEALTHY",
      "WOULD_LEAVE_HEALTHY_RANGE",
      "REACTOR_HEALTHY",
      "ORACLE_STALE",
      "BOX_AMBIGUOUS",
      "BOX_NOT_FOUND",
      "BUYBACK_UNAVAILABLE",
      "AMOUNT_TOO_SMALL",
      "AMOUNT_TOO_LARGE",
      "INSUFFICIENT_FUNDS",
      "INVALID_ADDRESS",
      "UNKNOWN_DEPLOYMENT",
      "INVALID_DEPLOYMENT",
      "NODE_ERROR",
    ] as const;

    for (const code of expectedCodes) {
      const err = new GluonError(code);
      expect(err.code).toBe(code);
      expect(err.message).toBe(code);
      expect(err.name).toBe("GluonError");
    }
  });

  it("throws INVALID_DEPLOYMENT on malformed records", () => {
    expect(() => deploymentFromRecord(null)).toThrowError(GluonError);
    expect(() => deploymentFromRecord({})).toThrowError(GluonError);
    expect(() => deploymentFromRecord("not an object")).toThrowError(GluonError);
    expect(() => deploymentFromRecord(123)).toThrowError(GluonError);
  });
});
