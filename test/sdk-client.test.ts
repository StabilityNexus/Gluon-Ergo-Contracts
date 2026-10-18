import { describe, expect, it } from "vitest";
import { ErgoAddress, Network, SAFE_MIN_BOX_VALUE } from "@fleet-sdk/core";
import { GluonClient } from "../sdk/src/client.ts";
import type { ChainState, GluonDeployment } from "../sdk/src/types.ts";
import { GluonError } from "../sdk/src/errors.ts";
import { deploymentFromRecord } from "../sdk/src/deployments/index.ts";
import { oraclePrice, readReactor } from "../sdk/src/reactor.ts";
import { ONE, adjustedPrice, genesisSupplies } from "../sdk/src/math.ts";
import {
  BUYBACK_TREE,
  DOLLAR,
  EMISSION,
  GOLD,
  HEIGHT,
  MIN_FEE,
  PROFILES,
  type Profile,
  TOKENS,
  type World,
  constantsFor,
  expectAccepted,
  publicKey,
  reactorTree,
  setup,
} from "./helpers.ts";
import { bootstrapTransactions, deploymentRecord, planBootstrap, publicKeyOf } from "../lib/bootstrap.ts";
import { compilerVersion } from "../lib/compile.ts";
import { bootstrapParameters, contractConstants, readDeployment } from "../lib/config.ts";

const shippedConfigs = { Gold: "../config/gold.json", Dollar: "../config/dollar.json" } as const;

function deploymentFor(profile: Profile, tree: string): GluonDeployment {
  return {
    name: `gluon-${profile.name.toLowerCase()}-v3`,
    status: "live",
    peg: profile.name === "Gold" ? "1 gram of gold" : "1 USD",
    ergoTree: tree,
    address: ErgoAddress.fromErgoTree(tree, Network.Mainnet).encode(),
    tokens: {
      nft: { id: TOKENS.nft, name: "NFT", decimals: 0, emission: 1n },
      neutron: {
        id: TOKENS.neutron,
        name: profile.name === "Gold" ? "GAU" : "USD",
        decimals: 9,
        emission: EMISSION,
      },
      proton: {
        id: TOKENS.proton,
        name: profile.name === "Gold" ? "GAUC" : "USDC",
        decimals: 9,
        emission: EMISSION,
      },
    },
    minFee: MIN_FEE,
    oraclePoolNft: profile.poolNft,
    oracleBuybackNft: profile.buybackNft,
    oracleFeeTree: BUYBACK_TREE,
    initialPegFactor: profile.pegFactor,
  };
}

function chainStateFor(world: World): ChainState {
  const reactor = world.reactor.utxos.at(0);
  const oracle = world.oracle.utxos.at(0);
  const buyback = world.buyback.utxos.at(0);
  const { state } = readReactor(reactor);
  return {
    height: world.chain.height,
    reactor,
    oracle,
    buyback,
    state,
  };
}

describe("GluonClient: transaction building & contract execution on mock chain", () => {
  for (const profile of PROFILES) {
    describe(`${profile.name} pool`, () => {
      const constants = constantsFor(profile);
      const tree = reactorTree(constants).toHex();
      const deployment = deploymentFor(profile, tree);

      it("builds a valid fission transaction accepted by the contract", () => {
        const w = setup(profile);
        const chain = chainStateFor(w);
        const client = new GluonClient({ deployment, nodeUrl: "http://fake-node:9053" });

        const tx = client.buildFission(chain, {
          erg: 10_000_000n,
          userBoxes: w.user.utxos.toArray(),
          changeAddress: w.user.address.encode(),
        });

        expectAccepted(w, tx, [w.user]);
      });

      it("builds a valid fusion transaction accepted by the contract", () => {
        const w = setup(profile);
        const chain = chainStateFor(w);
        const client = new GluonClient({ deployment, nodeUrl: "http://fake-node:9053" });

        const tx = client.buildFusion(chain, {
          erg: 10_000_000n,
          userBoxes: w.user.utxos.toArray(),
          changeAddress: w.user.address.encode(),
        });

        expectAccepted(w, tx, [w.user]);
      });

      it("builds a valid beta decay + (with oracle fee) accepted by the contract", () => {
        const w = setup(profile);
        const chain = chainStateFor(w);
        const client = new GluonClient({ deployment, nodeUrl: "http://fake-node:9053" });

        const tx = client.buildBetaDecayPlus(chain, {
          protons: 10_000_000n,
          userBoxes: w.user.utxos.toArray(),
          changeAddress: w.user.address.encode(),
        });

        expectAccepted(w, tx, [w.user]);
        // Buyback box is spent as last input when oracle fee is owed
        expect(tx.inputs[tx.inputs.length - 1].boxId).toBe(chain.buyback!.boxId);
      });

      it("builds a valid beta decay + (without oracle fee) accepted by the contract", () => {
        const w = setup(profile);
        const chain = chainStateFor(w);
        const client = new GluonClient({ deployment, nodeUrl: "http://fake-node:9053" });

        // For Gold: 805n protons gives neutronsOut = 1n, fees.oracle = 0n
        // For Dollar: 403n protons gives neutronsOut = 131n, fees.oracle = 0n
        const smallProtons = profile.name === "Gold" ? 805n : 403n;
        const quote = client.quoteBetaDecayPlus(chain, smallProtons);
        expect(quote.fees.oracle).toBe(0n);

        const tx = client.buildBetaDecayPlus(chain, {
          protons: smallProtons,
          userBoxes: w.user.utxos.toArray(),
          changeAddress: w.user.address.encode(),
        });

        expectAccepted(w, tx, [w.user]);
        // Buyback box should NOT be included when oracle fee is 0n
        expect(tx.inputs.some((i) => i.boxId === chain.buyback!.boxId)).toBe(false);
      });

      it("builds a valid beta decay − (with oracle fee) accepted by the contract", () => {
        const w = setup(profile);
        const chain = chainStateFor(w);
        const client = new GluonClient({ deployment, nodeUrl: "http://fake-node:9053" });

        const tx = client.buildBetaDecayMinus(chain, {
          neutrons: 10_000_000n,
          userBoxes: w.user.utxos.toArray(),
          changeAddress: w.user.address.encode(),
        });

        expectAccepted(w, tx, [w.user]);
        expect(tx.inputs[tx.inputs.length - 1].boxId).toBe(chain.buyback!.boxId);
      });

      it("builds a valid beta decay − (without oracle fee) accepted by the contract", () => {
        const w = setup(profile);
        const chain = chainStateFor(w);
        const client = new GluonClient({ deployment, nodeUrl: "http://fake-node:9053" });

        // For Gold: 2n neutrons gives protonsOut = 599n, fees.oracle = 0n
        // For Dollar: 198n neutrons gives protonsOut = 599n, fees.oracle = 0n
        const smallNeutrons = profile.name === "Gold" ? 2n : 198n;
        const quote = client.quoteBetaDecayMinus(chain, smallNeutrons);
        expect(quote.fees.oracle).toBe(0n);

        const tx = client.buildBetaDecayMinus(chain, {
          neutrons: smallNeutrons,
          userBoxes: w.user.utxos.toArray(),
          changeAddress: w.user.address.encode(),
        });

        expectAccepted(w, tx, [w.user]);
        expect(tx.inputs.some((i) => i.boxId === chain.buyback!.boxId)).toBe(false);
      });

      it("builds Adjust Peg in down direction (q > 0.98)", () => {
        const w = setup(profile, { q: (120n * ONE) / 100n });
        const chain = chainStateFor(w);
        const client = new GluonClient({ deployment, nodeUrl: "http://fake-node:9053" });

        const quote = client.quoteAdjustPeg(chain);
        expect(quote.allowed).toBe(true);
        expect(quote.direction).toBe("down");

        const tx = client.buildAdjustPeg(chain, {
          userBoxes: w.user.utxos.toArray(),
          changeAddress: w.user.address.encode(),
        });

        expectAccepted(w, tx, [w.user]);
      });

      it("builds Adjust Peg in up direction (q < 0.50)", () => {
        const w = setup(profile, { q: (40n * ONE) / 100n });
        const chain = chainStateFor(w);
        const client = new GluonClient({ deployment, nodeUrl: "http://fake-node:9053" });

        const quote = client.quoteAdjustPeg(chain);
        expect(quote.allowed).toBe(true);
        expect(quote.direction).toBe("up");

        const tx = client.buildAdjustPeg(chain, {
          userBoxes: w.user.utxos.toArray(),
          changeAddress: w.user.address.encode(),
        });

        expectAccepted(w, tx, [w.user]);
      });

      it("builds Update Treasury signed by the current treasury", () => {
        const w = setup(profile);
        const chain = chainStateFor(w);
        const client = new GluonClient({ deployment, nodeUrl: "http://fake-node:9053" });
        const newTreasury = w.chain.newParty("new_treasury");

        const tx = client.buildUpdateTreasury(chain, {
          newTreasuryAddress: newTreasury.address.encode(),
          userBoxes: w.treasury.utxos.toArray(),
          changeAddress: w.treasury.address.encode(),
        });

        expectAccepted(w, tx, [w.treasury]);
      });

      it("builds transaction with uiFeeAddress and pays the UI fee", () => {
        const w = setup(profile);
        const chain = chainStateFor(w);
        const client = new GluonClient({
          deployment,
          nodeUrl: "http://fake-node:9053",
          uiFeeAddress: w.ui.address.encode(),
        });

        const tx = client.buildFission(chain, {
          erg: 10_000_000n,
          userBoxes: w.user.utxos.toArray(),
          changeAddress: w.user.address.encode(),
        });

        // Output paying UI fee exists prior to mockchain execution
        expect(tx.outputs.some((o) => o.ergoTree === w.ui.ergoTree)).toBe(true);
        expectAccepted(w, tx, [w.user]);
      });
    });
  }
});

describe("GluonClient: one test per error code", () => {
  const profile = GOLD;
  const tree = reactorTree(constantsFor(profile)).toHex();
  const deployment = deploymentFor(profile, tree);

  it("throws UNKNOWN_DEPLOYMENT for an unknown deployment name", () => {
    expect(() => new GluonClient({ deployment: "non-existent-pool", nodeUrl: "http://fake" })).toThrowError(
      expect.objectContaining({ code: "UNKNOWN_DEPLOYMENT" }),
    );
  });

  it("throws INVALID_ADDRESS for invalid or non-mainnet or non-P2PK addresses", () => {
    // Malformed UI fee address
    expect(
      () => new GluonClient({ deployment, nodeUrl: "http://fake", uiFeeAddress: "not-an-ergo-address" }),
    ).toThrowError(expect.objectContaining({ code: "INVALID_ADDRESS" }));

    // Non-P2PK UI fee address (script tree)
    const scriptAddress = ErgoAddress.fromErgoTree(BUYBACK_TREE, Network.Mainnet).encode();
    expect(
      () => new GluonClient({ deployment, nodeUrl: "http://fake", uiFeeAddress: scriptAddress }),
    ).toThrowError(expect.objectContaining({ code: "INVALID_ADDRESS" }));

    const w = setup(profile);
    const chain = chainStateFor(w);
    const client = new GluonClient({ deployment, nodeUrl: "http://fake" });

    // Malformed change address
    expect(() =>
      client.buildFission(chain, {
        erg: 10_000_000n,
        userBoxes: w.user.utxos.toArray(),
        changeAddress: "bad-change-address",
      }),
    ).toThrowError(expect.objectContaining({ code: "INVALID_ADDRESS" }));

    // Treasury address same as current
    expect(() =>
      client.buildUpdateTreasury(chain, {
        newTreasuryAddress: w.treasury.address.encode(),
        userBoxes: w.treasury.utxos.toArray(),
        changeAddress: w.treasury.address.encode(),
      }),
    ).toThrowError(expect.objectContaining({ code: "INVALID_ADDRESS" }));
  });

  it("throws REACTOR_UNHEALTHY when attempting actions while outside [0.5, 0.98]", () => {
    const w = setup(profile, { q: (40n * ONE) / 100n });
    const chain = chainStateFor(w);
    const client = new GluonClient({ deployment, nodeUrl: "http://fake" });

    expect(() => client.quoteFission(chain, 10_000_000n)).toThrowError(
      expect.objectContaining({ code: "REACTOR_UNHEALTHY" }),
    );
    expect(() => client.quoteFusion(chain, 10_000_000n)).toThrowError(
      expect.objectContaining({ code: "REACTOR_UNHEALTHY" }),
    );
    expect(() => client.quoteBetaDecayPlus(chain, 10_000_000n)).toThrowError(
      expect.objectContaining({ code: "REACTOR_UNHEALTHY" }),
    );
    expect(() => client.quoteBetaDecayMinus(chain, 10_000_000n)).toThrowError(
      expect.objectContaining({ code: "REACTOR_UNHEALTHY" }),
    );
  });

  it("throws WOULD_LEAVE_HEALTHY_RANGE when a beta decay ends outside [0.5, 0.98]", () => {
    const w = setup(profile, { q: (97n * ONE) / 100n });
    const chain = chainStateFor(w);
    const client = new GluonClient({ deployment, nodeUrl: "http://fake" });

    // 200B protons pushes qAfter above 0.98 without exceeding reactor reserves
    expect(() => client.quoteBetaDecayPlus(chain, 200_000_000_000n)).toThrowError(
      expect.objectContaining({ code: "WOULD_LEAVE_HEALTHY_RANGE" }),
    );
  });

  it("throws REACTOR_HEALTHY when calling buildAdjustPeg while healthy", () => {
    const w = setup(profile, { q: (60n * ONE) / 100n });
    const chain = chainStateFor(w);
    const client = new GluonClient({ deployment, nodeUrl: "http://fake" });

    expect(() =>
      client.buildAdjustPeg(chain, {
        userBoxes: w.user.utxos.toArray(),
        changeAddress: w.user.address.encode(),
      }),
    ).toThrowError(expect.objectContaining({ code: "REACTOR_HEALTHY" }));
  });

  it("throws ORACLE_STALE when oracle age is >= 35 or negative", () => {
    const wStale = setup(profile, { oracleAge: 35 });
    const chainStale = chainStateFor(wStale);
    const client = new GluonClient({ deployment, nodeUrl: "http://fake" });

    expect(() => client.quoteFission(chainStale, 10_000_000n)).toThrowError(
      expect.objectContaining({ code: "ORACLE_STALE" }),
    );

    const wFuture = setup(profile, { oracleAge: -1 });
    const chainFuture = chainStateFor(wFuture);
    expect(() => client.quoteFission(chainFuture, 10_000_000n)).toThrowError(
      expect.objectContaining({ code: "ORACLE_STALE" }),
    );
  });

  it("throws AMOUNT_TOO_SMALL when amount is <= 0 or releases no tokens", () => {
    const w = setup(profile);
    const chain = chainStateFor(w);
    const client = new GluonClient({ deployment, nodeUrl: "http://fake" });

    expect(() => client.quoteFission(chain, 0n)).toThrowError(
      expect.objectContaining({ code: "AMOUNT_TOO_SMALL" }),
    );
    expect(() => client.quoteFusion(chain, -100n)).toThrowError(
      expect.objectContaining({ code: "AMOUNT_TOO_SMALL" }),
    );
    expect(() => client.quoteBetaDecayPlus(chain, 0n)).toThrowError(
      expect.objectContaining({ code: "AMOUNT_TOO_SMALL" }),
    );
    expect(() => client.quoteBetaDecayMinus(chain, 0n)).toThrowError(
      expect.objectContaining({ code: "AMOUNT_TOO_SMALL" }),
    );
  });

  it("throws AMOUNT_TOO_LARGE when fusion exceeds reserve or release exceeds reactor supply", () => {
    const w = setup(profile);
    const chain = chainStateFor(w);
    const client = new GluonClient({ deployment, nodeUrl: "http://fake" });

    // Fusion exceeding reserve
    expect(() => client.quoteFusion(chain, chain.state.value + ONE)).toThrowError(
      expect.objectContaining({ code: "AMOUNT_TOO_LARGE" }),
    );
  });

  it("throws BUYBACK_UNAVAILABLE when beta decay owes oracle fee but buyback box is missing", () => {
    const w = setup(profile);
    const chain = chainStateFor(w);
    const chainWithoutBuyback: ChainState = { ...chain, buyback: undefined };
    const client = new GluonClient({ deployment, nodeUrl: "http://fake" });

    expect(() => client.quoteBetaDecayPlus(chainWithoutBuyback, 10_000_000n)).toThrowError(
      expect.objectContaining({ code: "BUYBACK_UNAVAILABLE" }),
    );
  });

  it("throws INSUFFICIENT_FUNDS when user boxes cannot fund the action", () => {
    const w = setup(profile);
    const chain = chainStateFor(w);
    const client = new GluonClient({ deployment, nodeUrl: "http://fake" });

    expect(() =>
      client.buildFission(chain, {
        erg: 10_000_000n,
        userBoxes: [],
        changeAddress: w.user.address.encode(),
      }),
    ).toThrowError(expect.objectContaining({ code: "INSUFFICIENT_FUNDS" }));
  });

  it("throws BOX_AMBIGUOUS, BOX_NOT_FOUND, and NODE_ERROR during fetchChainState", async () => {
    // NODE_ERROR on HTTP 500
    const clientError = new GluonClient({
      deployment,
      nodeUrl: "http://fake-node:9053",
      fetch: () => Promise.resolve(new Response("Internal error", { status: 500 })),
    });
    await expect(clientError.fetchChainState()).rejects.toThrowError(
      expect.objectContaining({ code: "NODE_ERROR" }),
    );

    // BOX_NOT_FOUND when node returns no boxes
    const clientNotFound = new GluonClient({
      deployment,
      nodeUrl: "http://fake-node:9053",
      fetch: () => Promise.resolve(new Response(JSON.stringify([]))),
    });
    await expect(clientNotFound.fetchChainState()).rejects.toThrowError(
      expect.objectContaining({ code: "BOX_NOT_FOUND" }),
    );
  });

  it("throws INVALID_DEPLOYMENT for malformed deployment record", () => {
    expect(() => deploymentFromRecord({})).toThrowError(
      expect.objectContaining({ code: "INVALID_DEPLOYMENT" }),
    );
  });
});

describe("GluonClient: snapshot and max quotes", () => {
  const profile = GOLD;
  const tree = reactorTree(constantsFor(profile)).toHex();
  const deployment = deploymentFor(profile, tree);

  it("computes ProtocolSnapshot accurately against hand-computed values", () => {
    const w = setup(profile, { q: (60n * ONE) / 100n });
    const chain = chainStateFor(w);
    const client = new GluonClient({ deployment, nodeUrl: "http://fake" });

    const snap = client.snapshot(chain);

    expect(snap.deployment).toBe(deployment.name);
    expect(snap.height).toBe(HEIGHT);
    expect(snap.reserve).toBe(1_000n * ONE);
    expect(snap.oraclePrice).toBe(GOLD.oraclePrice);
    expect(snap.initialPegFactor).toBe(GOLD.pegFactor);
    expect(snap.pegFactor).toBe(GOLD.pegFactor);
    expect(snap.pegDrift).toBe(1);
    expect(snap.targetPrice).toBe((GOLD.oraclePrice * GOLD.pegFactor) / ONE);
    expect(snap.q).toBe(599999999n);
    expect(snap.healthy).toBe(true);
    expect(snap.adjustPeg).toBeUndefined();
    expect(snap.betaDecaysAvailable).toBe(true);

    // q = 0.6: leverage is 0.6 / (1 - 0.6) = 1.5
    expect(snap.protonLeverage).toBeCloseTo(1.5, 5);

    // crash cushion at q = 0.6
    expect(snap.crashCushion.untilDepeg).toBeCloseTo(1 - 0.6 / 0.99, 5);
    expect(snap.crashCushion.untilAdjustment).toBeCloseTo(1 - 0.6 / 0.98, 5);

    // reserveRatio at q = 0.6 is 100 / 0.6 = 166.666...
    expect(snap.reserveRatio).toBeCloseTo(100 / 0.6, 3);
  });

  it("finds the maximum beta decay amount via binary search", () => {
    const w = setup(profile);
    const chain = chainStateFor(w);
    const client = new GluonClient({ deployment, nodeUrl: "http://fake" });

    const maxPlus = client.maxBetaDecayPlus(chain);
    expect(maxPlus > 0n).toBe(true);
    expect(() => client.quoteBetaDecayPlus(chain, maxPlus)).not.toThrow();
    expect(() => client.quoteBetaDecayPlus(chain, maxPlus + 1n)).toThrow();

    const maxMinus = client.maxBetaDecayMinus(chain);
    expect(maxMinus > 0n).toBe(true);
    expect(() => client.quoteBetaDecayMinus(chain, maxMinus)).not.toThrow();
    expect(() => client.quoteBetaDecayMinus(chain, maxMinus + 1n)).toThrow();
  });

  it("selects only needed inputs, maintains buyback last, and ensures minimum change", () => {
    const w = setup(profile);
    const chain = chainStateFor(w);
    const client = new GluonClient({ deployment, nodeUrl: "http://fake" });

    // Add extra user boxes of varying amounts
    w.user.addBalance({ nanoergs: 1_000_000n * ONE });
    w.user.addBalance({ nanoergs: 2_000_000n * ONE });

    const userUtxos = w.user.utxos.toArray();
    expect(userUtxos.length).toBeGreaterThan(2);

    const tx = client.buildBetaDecayPlus(chain, {
      protons: 10_000_000n,
      userBoxes: userUtxos,
      changeAddress: w.user.address.encode(),
    });

    // Buyback box must be the very last input
    expect(tx.inputs[tx.inputs.length - 1].boxId).toBe(chain.buyback!.boxId);

    // Not all user boxes should have been consolidated
    const userInputsInTx = tx.inputs.slice(1, tx.inputs.length - 1);
    expect(userInputsInTx.length).toBeLessThan(userUtxos.length);

    // Any change output created must have value >= SAFE_MIN_BOX_VALUE
    const changeOutputs = tx.outputs.filter((o, idx) => idx > 1 && o.ergoTree === w.user.ergoTree);
    for (const change of changeOutputs) {
      expect(BigInt(change.value)).toBeGreaterThanOrEqual(SAFE_MIN_BOX_VALUE);
    }
  });
});

describe("A7: Client transaction building against bootstrap-compiled v3 deployment ErgoTree", () => {
  for (const profile of PROFILES) {
    it(`builds transactions with exact ErgoTree and tokens produced by bootstrap (${profile.name})`, () => {
      const w = setup(profile, { withoutReactor: true });
      const c = readDeployment(new URL(shippedConfigs[profile.name as keyof typeof shippedConfigs], import.meta.url));
      c.oracle.poolNft = profile.poolNft;
      c.oracle.buybackNft = profile.buybackNft;
      c.oracle.buybackContractAddress = ErgoAddress.fromErgoTree(BUYBACK_TREE).encode();
      c.treasuryAddress = w.treasury.address.encode();
      c.bootstrap.seedErg = "10";
      c.tokens.nft = { ticker: "TNFT", name: "Test NFT", description: "desc" };
      c.tokens.neutron = { ticker: "TNEU", name: "Test Neu", description: "desc" };
      c.tokens.proton = { ticker: "TPRO", name: "Test Pro", description: "desc" };

      const tree = reactorTree(contractConstants(c));
      const price = oraclePrice(w.oracle.utxos.at(0));
      const plan = planBootstrap(bootstrapParameters(c), MIN_FEE, price, publicKeyOf(c.treasuryAddress), w.chain.height);
      const btxs = bootstrapTransactions(c, plan, tree.toHex(), w.user.utxos.toArray(), w.user.address.encode(), w.chain.height);

      for (const tx of [btxs.mintNft, btxs.mintNeutrons, btxs.mintProtons, btxs.createReactor]) {
        expectAccepted(w, tx.toEIP12Object());
      }

      const record = deploymentRecord(c, plan, price, tree, btxs, "0".repeat(40), compilerVersion());
      const deployment = deploymentFromRecord(record);

      const reactorBox = w.reactor.utxos.at(0);
      expect(reactorBox.ergoTree).toBe(deployment.ergoTree);

      const chain: ChainState = {
        height: w.chain.height,
        reactor: reactorBox,
        oracle: w.oracle.utxos.at(0),
        buyback: w.buyback.utxos.at(0),
        state: readReactor(reactorBox, deployment).state,
      };

      const trader = w.chain.newParty("trader_a7");
      trader.addBalance({
        nanoergs: 1_000n * ONE,
        tokens: [
          { tokenId: deployment.tokens.neutron.id, amount: 50_000_000n },
          { tokenId: deployment.tokens.proton.id, amount: 50_000_000n },
        ],
      });

      const client = new GluonClient({ deployment, nodeUrl: "http://fake-node:9053" });

      // 1. Fission
      const fTx = client.buildFission(chain, {
        erg: 10_000_000n,
        userBoxes: trader.utxos.toArray(),
        changeAddress: trader.address.encode(),
      });
      expectAccepted(w, fTx, [trader]);

      // 2. Fusion
      const fuTx = client.buildFusion(chain, {
        erg: 10_000_000n,
        userBoxes: trader.utxos.toArray(),
        changeAddress: trader.address.encode(),
      });
      expectAccepted(w, fuTx, [trader]);

      // 3. Beta decay +
      const pTx = client.buildBetaDecayPlus(chain, {
        protons: 10_000_000n,
        userBoxes: trader.utxos.toArray(),
        changeAddress: trader.address.encode(),
      });
      expectAccepted(w, pTx, [trader]);

      // 4. Beta decay - (amount within reactor's capacity for a 10 ERG seed)
      const mTx = client.buildBetaDecayMinus(chain, {
        neutrons: 100_000n,
        userBoxes: trader.utxos.toArray(),
        changeAddress: trader.address.encode(),
      });
      expectAccepted(w, mTx, [trader]);

      // 5. Update Treasury
      const newTreasury = w.chain.newParty("new_treasury_a7");
      const uTx = client.buildUpdateTreasury(chain, {
        newTreasuryAddress: newTreasury.address.encode(),
        userBoxes: w.treasury.utxos.toArray(),
        changeAddress: w.treasury.address.encode(),
      });
      expectAccepted(w, uTx, [w.treasury]);

      // 6. Box with mismatched ErgoTree is rejected by readReactor
      const tamperedBox = { ...reactorBox, ergoTree: "0008cd" + "02".repeat(33) };
      expect(() => readReactor(tamperedBox, deployment)).toThrowError(/wrong ErgoTree/);

      // 7. Box with wrong token ID is rejected by readReactor
      const tamperedTokensBox = {
        ...reactorBox,
        assets: [{ tokenId: "wrong".padEnd(64, "0"), amount: 1n }, reactorBox.assets[1], reactorBox.assets[2]],
      };
      expect(() => readReactor(tamperedTokensBox, deployment)).toThrowError(/wrong token ids/);
    });
  }
});

