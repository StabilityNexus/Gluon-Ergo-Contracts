import type { Amount, Box, EIP12UnsignedTransaction } from "@fleet-sdk/common";
import { AddressType, ErgoAddress, Network, SAFE_MIN_BOX_VALUE } from "@fleet-sdk/core";
import {
  type ActionQuote,
  type AdjustPegQuote,
  type ChainState,
  type Funding,
  type GluonClientOptions,
  type GluonDeployment,
  type ProtocolSnapshot,
} from "./types.ts";
import { GluonError } from "./errors.ts";
import { getDeployment } from "./deployments/index.ts";
import { ErgoNode, findUniqueBox } from "./node.ts";
import { readReactor } from "./reactor.ts";
import { oracleAge, oraclePrice, requireFreshOracle } from "./oracle.ts";
import { selectInputs } from "./select.ts";
import {
  type TxContext,
  adjustPegTx,
  betaDecayMinusTx,
  betaDecayPlusTx,
  fissionTx,
  fusionTx,
  updateTreasuryTx,
} from "./transactions.ts";
import {
  ONE,
  Q_LOWER,
  Q_UPPER,
  betaDecayMinus,
  betaDecayPlus,
  crashCushion,
  elapsedBuckets,
  fission,
  fusion,
  isHealthy,
  pegAdjustments,
  protonLeverage,
  shiftVolumes,
  snapshot,
  valueOfNeutrons,
  valueOfProtons,
} from "./math.ts";

function validateChangeAddress(address: string): void {
  let addr: ErgoAddress;
  try {
    addr = ErgoAddress.fromBase58(address);
  } catch {
    throw new GluonError("INVALID_ADDRESS", `invalid change address: ${address}`);
  }
  if (addr.network !== Network.Mainnet) {
    throw new GluonError("INVALID_ADDRESS", `change address must be a mainnet address: ${address}`);
  }
}

function validateTreasuryAddress(address: string): string {
  let addr: ErgoAddress;
  try {
    addr = ErgoAddress.fromBase58(address);
  } catch {
    throw new GluonError("INVALID_ADDRESS", `invalid treasury address: ${address}`);
  }
  if (addr.network !== Network.Mainnet || addr.type !== AddressType.P2PK) {
    throw new GluonError(
      "INVALID_ADDRESS",
      `treasury address must be a mainnet single-signature (P2PK) address: ${address}`,
    );
  }
  return addr.ergoTree.slice(6);
}

export class GluonClient {
  readonly deployment: GluonDeployment;
  readonly node: ErgoNode;
  readonly minerFee: bigint;
  readonly uiFeeAddress?: string;
  private readonly uiFeeKey?: string;

  constructor(options: GluonClientOptions) {
    if (typeof options.deployment === "string") {
      this.deployment = getDeployment(options.deployment);
    } else {
      this.deployment = options.deployment;
    }

    this.minerFee = options.minerFee ?? 1_100_000n;
    this.node = new ErgoNode(options.nodeUrl, { fetch: options.fetch });

    if (options.uiFeeAddress) {
      let addr: ErgoAddress;
      try {
        addr = ErgoAddress.fromBase58(options.uiFeeAddress);
      } catch {
        throw new GluonError("INVALID_ADDRESS", `invalid UI-fee address: ${options.uiFeeAddress}`);
      }
      if (addr.network !== Network.Mainnet || addr.type !== AddressType.P2PK) {
        throw new GluonError(
          "INVALID_ADDRESS",
          `UI-fee address must be a mainnet single-signature (P2PK) address: ${options.uiFeeAddress}`,
        );
      }
      this.uiFeeAddress = options.uiFeeAddress;
      this.uiFeeKey = addr.ergoTree.slice(6);
    }
  }

  async fetchChainState(): Promise<ChainState> {
    const [height, reactor, oracle, buybackResult] = await Promise.all([
      this.node.height(),
      findUniqueBox(this.node, this.deployment.tokens.nft.id, "reactor", this.deployment.ergoTree),
      findUniqueBox(this.node, this.deployment.oraclePoolNft, "oracle pool"),
      findUniqueBox(
        this.node,
        this.deployment.oracleBuybackNft,
        "oracle buyback",
        this.deployment.oracleFeeTree,
      ).catch((e: unknown) => {
        if (e instanceof GluonError && e.code === "BOX_NOT_FOUND") return undefined;
        throw e;
      }),
    ]);

    const { state } = readReactor(reactor, this.deployment);

    return {
      height,
      reactor,
      oracle,
      buyback: buybackResult ?? undefined,
      state,
    };
  }

  snapshot(chain: ChainState): ProtocolSnapshot {
    const price = oraclePrice(chain.oracle);
    const snap = snapshot(chain.state, this.deployment.minFee, price);
    const healthy = isHealthy(snap.q);

    let adjustPegInfo: { direction: "down" | "up"; steps: number } | undefined;
    if (!healthy) {
      const direction: "down" | "up" = snap.q > Q_UPPER ? "down" : "up";
      const steps = pegAdjustments(chain.state, this.deployment.minFee, price).length;
      adjustPegInfo = { direction, steps };
    }

    const nDays = elapsedBuckets(chain.height + 1, chain.state.lastBucketBlock);
    const volumes = {
      plus: shiftVolumes(chain.state.volumePlus, nDays, 0n),
      minus: shiftVolumes(chain.state.volumeMinus, nDays, 0n),
    };

    const neutronDecimals = BigInt(this.deployment.tokens.neutron.decimals);
    const protonDecimals = BigInt(this.deployment.tokens.proton.decimals);

    return {
      deployment: this.deployment.name,
      height: chain.height,
      reserve: snap.reserve,
      neutronsInCirculation: snap.neutronSupply,
      protonsInCirculation: snap.protonSupply,
      oraclePrice: price,
      oracleAge: oracleAge(chain.oracle, chain.height),
      pegFactor: chain.state.pegFactor,
      initialPegFactor: this.deployment.initialPegFactor,
      pegDrift: Number(chain.state.pegFactor) / Number(this.deployment.initialPegFactor),
      pegUnitPrice: (price * this.deployment.initialPegFactor) / ONE,
      targetPrice: snap.price,
      q: snap.q,
      qNorm: snap.qNorm,
      healthy,
      adjustPeg: adjustPegInfo,
      neutronPrice: valueOfNeutrons(snap, 10n ** neutronDecimals),
      protonPrice: valueOfProtons(snap, 10n ** protonDecimals),
      reserveRatio: snap.q === 0n ? 0 : (100 * Number(ONE)) / Number(snap.q),
      protonLeverage: protonLeverage(snap.q),
      crashCushion: crashCushion(snap.q),
      volumes,
      devFee: { repaid: chain.state.devFeeRepaid, max: chain.state.maxDevFee },
      treasuryAddress: ErgoAddress.fromPublicKey(chain.state.treasury, Network.Mainnet).encode(),
      betaDecaysAvailable: chain.buyback !== undefined,
    };
  }

  quoteFission(chain: ChainState, erg: bigint): ActionQuote {
    requireFreshOracle(chain.oracle, chain.height);
    const price = oraclePrice(chain.oracle);
    const snap = snapshot(chain.state, this.deployment.minFee, price);

    if (!isHealthy(snap.q)) {
      throw new GluonError("REACTOR_UNHEALTHY", "the reactor is outside its healthy range");
    }
    if (erg <= 0n) {
      throw new GluonError("AMOUNT_TOO_SMALL", "fission amount must be greater than zero");
    }

    const result = fission(chain.state, this.deployment.minFee, erg, this.uiFeeKey !== undefined);
    const neutronsOut = chain.state.neutrons - result.next.neutrons;
    const protonsOut = chain.state.protons - result.next.protons;

    if (neutronsOut <= 0n || protonsOut <= 0n) {
      throw new GluonError("AMOUNT_TOO_SMALL", "fission amount is too small to release tokens");
    }
    if (result.next.neutrons < 0n || result.next.protons < 0n) {
      throw new GluonError("AMOUNT_TOO_LARGE", "reactor cannot release that many tokens");
    }

    let numFeeOutputs = 0n;
    if (result.fees.dev > 0n) numFeeOutputs++;
    if (result.fees.ui > 0n) numFeeOutputs++;
    const outputsMinFee = this.deployment.minFee * numFeeOutputs;
    const miner = this.minerFee;
    const total = result.fees.dev + result.fees.ui + outputsMinFee + miner;
    const qAfter = snapshot(result.next, this.deployment.minFee, price).q;

    return {
      ergIn: erg,
      ergOut: 0n,
      neutronsIn: 0n,
      neutronsOut,
      protonsIn: 0n,
      protonsOut,
      principal: erg,
      fees: {
        dev: result.fees.dev,
        ui: result.fees.ui,
        oracle: 0n,
        outputsMinFee,
        miner,
        total,
      },
      next: result.next,
      qAfter,
    };
  }

  quoteFusion(chain: ChainState, erg: bigint): ActionQuote {
    requireFreshOracle(chain.oracle, chain.height);
    const price = oraclePrice(chain.oracle);
    const snap = snapshot(chain.state, this.deployment.minFee, price);

    if (!isHealthy(snap.q)) {
      throw new GluonError("REACTOR_UNHEALTHY", "the reactor is outside its healthy range");
    }
    if (erg <= 0n) {
      throw new GluonError("AMOUNT_TOO_SMALL", "fusion amount must be greater than zero");
    }
    if (erg >= snap.reserve) {
      throw new GluonError("AMOUNT_TOO_LARGE", "fusion amount cannot exceed the reactor reserve");
    }

    const result = fusion(chain.state, this.deployment.minFee, erg, this.uiFeeKey !== undefined);
    const neutronsIn = result.next.neutrons - chain.state.neutrons;
    const protonsIn = result.next.protons - chain.state.protons;

    if (neutronsIn <= 0n || protonsIn <= 0n) {
      throw new GluonError("AMOUNT_TOO_SMALL", "fusion amount is too small to consume tokens");
    }

    let numFeeOutputs = 0n;
    if (result.fees.dev > 0n) numFeeOutputs++;
    if (result.fees.ui > 0n) numFeeOutputs++;
    const outputsMinFee = this.deployment.minFee * numFeeOutputs;
    const miner = this.minerFee;
    const total = result.fees.dev + result.fees.ui + outputsMinFee + miner;
    const qAfter = snapshot(result.next, this.deployment.minFee, price).q;

    return {
      ergIn: 0n,
      ergOut: erg,
      neutronsIn,
      neutronsOut: 0n,
      protonsIn,
      protonsOut: 0n,
      principal: erg,
      fees: {
        dev: result.fees.dev,
        ui: result.fees.ui,
        oracle: 0n,
        outputsMinFee,
        miner,
        total,
      },
      next: result.next,
      qAfter,
    };
  }

  quoteBetaDecayPlus(chain: ChainState, protons: bigint): ActionQuote {
    requireFreshOracle(chain.oracle, chain.height);
    const price = oraclePrice(chain.oracle);
    const snap = snapshot(chain.state, this.deployment.minFee, price);

    if (!isHealthy(snap.q)) {
      throw new GluonError("REACTOR_UNHEALTHY", "the reactor is outside its healthy range");
    }
    if (protons <= 0n) {
      throw new GluonError("AMOUNT_TOO_SMALL", "proton amount must be greater than zero");
    }

    const result = betaDecayPlus(
      chain.state,
      this.deployment.minFee,
      price,
      protons,
      chain.height + 1,
      this.uiFeeKey !== undefined,
    );
    const neutronsOut = chain.state.neutrons - result.next.neutrons;

    if (neutronsOut <= 0n) {
      throw new GluonError("AMOUNT_TOO_SMALL", "proton amount is too small to release neutrons");
    }
    if (neutronsOut > chain.state.neutrons) {
      throw new GluonError("AMOUNT_TOO_LARGE", "reactor cannot release that many neutrons");
    }

    const qAfter = snapshot(result.next, this.deployment.minFee, price).q;
    if (!isHealthy(qAfter)) {
      throw new GluonError(
        "WOULD_LEAVE_HEALTHY_RANGE",
        "beta decay would leave the reactor outside the healthy range",
      );
    }
    if (result.fees.oracle > 0n && !chain.buyback) {
      throw new GluonError("BUYBACK_UNAVAILABLE", "this beta decay owes an oracle fee, but no buyback box exists");
    }

    let numFeeOutputs = 0n;
    if (result.fees.oracle > 0n) numFeeOutputs++;
    if (result.fees.dev > 0n) numFeeOutputs++;
    if (result.fees.ui > 0n) numFeeOutputs++;
    const outputsMinFee = this.deployment.minFee * numFeeOutputs;
    const miner = this.minerFee;
    const total = result.fees.oracle + result.fees.dev + result.fees.ui + outputsMinFee + miner;
    const principal = valueOfProtons(snap, protons);

    return {
      ergIn: 0n,
      ergOut: 0n,
      neutronsIn: 0n,
      neutronsOut,
      protonsIn: protons,
      protonsOut: 0n,
      principal,
      fees: {
        dev: result.fees.dev,
        ui: result.fees.ui,
        oracle: result.fees.oracle,
        outputsMinFee,
        miner,
        total,
      },
      next: result.next,
      qAfter,
    };
  }

  quoteBetaDecayMinus(chain: ChainState, neutrons: bigint): ActionQuote {
    requireFreshOracle(chain.oracle, chain.height);
    const price = oraclePrice(chain.oracle);
    const snap = snapshot(chain.state, this.deployment.minFee, price);

    if (!isHealthy(snap.q)) {
      throw new GluonError("REACTOR_UNHEALTHY", "the reactor is outside its healthy range");
    }
    if (neutrons <= 0n) {
      throw new GluonError("AMOUNT_TOO_SMALL", "neutron amount must be greater than zero");
    }

    const result = betaDecayMinus(
      chain.state,
      this.deployment.minFee,
      price,
      neutrons,
      chain.height + 1,
      this.uiFeeKey !== undefined,
    );
    const protonsOut = chain.state.protons - result.next.protons;

    if (protonsOut <= 0n) {
      throw new GluonError("AMOUNT_TOO_SMALL", "neutron amount is too small to release protons");
    }
    if (protonsOut > chain.state.protons) {
      throw new GluonError("AMOUNT_TOO_LARGE", "reactor cannot release that many protons");
    }

    const qAfter = snapshot(result.next, this.deployment.minFee, price).q;
    if (!isHealthy(qAfter)) {
      throw new GluonError(
        "WOULD_LEAVE_HEALTHY_RANGE",
        "beta decay would leave the reactor outside the healthy range",
      );
    }
    if (result.fees.oracle > 0n && !chain.buyback) {
      throw new GluonError("BUYBACK_UNAVAILABLE", "this beta decay owes an oracle fee, but no buyback box exists");
    }

    let numFeeOutputs = 0n;
    if (result.fees.oracle > 0n) numFeeOutputs++;
    if (result.fees.dev > 0n) numFeeOutputs++;
    if (result.fees.ui > 0n) numFeeOutputs++;
    const outputsMinFee = this.deployment.minFee * numFeeOutputs;
    const miner = this.minerFee;
    const total = result.fees.oracle + result.fees.dev + result.fees.ui + outputsMinFee + miner;
    const principal = valueOfNeutrons(snap, neutrons);

    return {
      ergIn: 0n,
      ergOut: 0n,
      neutronsIn: neutrons,
      neutronsOut: 0n,
      protonsIn: 0n,
      protonsOut,
      principal,
      fees: {
        dev: result.fees.dev,
        ui: result.fees.ui,
        oracle: result.fees.oracle,
        outputsMinFee,
        miner,
        total,
      },
      next: result.next,
      qAfter,
    };
  }

  maxBetaDecayPlus(chain: ChainState): bigint {
    return this.findMaxQuote((amt) => this.quoteBetaDecayPlus(chain, amt), chain.state.totalProtons);
  }

  maxBetaDecayMinus(chain: ChainState): bigint {
    return this.findMaxQuote((amt) => this.quoteBetaDecayMinus(chain, amt), chain.state.totalNeutrons);
  }

  private findMaxQuote(quoteFn: (amount: bigint) => void, maxSupply: bigint): bigint {
    const tryQuote = (amount: bigint): boolean => {
      try {
        quoteFn(amount);
        return true;
      } catch {
        return false;
      }
    };

    let v = 0n;
    for (const testAmt of [10_000_000n, 1_000_000n, 100_000n, 10_000n, 1_000n, 805n, 403n, 100n, 10n, 1n]) {
      if (tryQuote(testAmt)) {
        v = testAmt;
        break;
      }
    }
    if (v === 0n) {
      let step = 1n;
      while (step <= maxSupply) {
        if (tryQuote(step)) {
          v = step;
          break;
        }
        step *= 2n;
      }
    }
    if (v === 0n) return 0n;

    let low = v;
    let high = maxSupply;
    let best = v;
    while (low <= high) {
      const mid = (low + high) / 2n;
      if (tryQuote(mid)) {
        best = mid;
        low = mid + 1n;
      } else {
        high = mid - 1n;
      }
    }
    return best;
  }

  quoteAdjustPeg(chain: ChainState): AdjustPegQuote {
    requireFreshOracle(chain.oracle, chain.height);
    const price = oraclePrice(chain.oracle);
    const snap = snapshot(chain.state, this.deployment.minFee, price);

    if (isHealthy(snap.q)) {
      return { allowed: false, steps: 0 };
    }

    const direction: "down" | "up" = snap.q > Q_UPPER ? "down" : "up";
    const steps = pegAdjustments(chain.state, this.deployment.minFee, price).length;
    const nextPegFactor =
      direction === "down" ? (chain.state.pegFactor * 99n) / 100n : (chain.state.pegFactor * 101n) / 100n;

    return { allowed: true, direction, steps, nextPegFactor };
  }

  buildFission(chain: ChainState, args: { erg: bigint } & Funding): EIP12UnsignedTransaction {
    validateChangeAddress(args.changeAddress);
    const quote = this.quoteFission(chain, args.erg);
    const neededErg = args.erg + SAFE_MIN_BOX_VALUE + quote.fees.total;
    const selectedBoxes = selectInputs(args.userBoxes, { nanoErgs: neededErg });

    const ctx: TxContext = {
      height: chain.height,
      minFee: this.deployment.minFee,
      reactor: chain.reactor,
      oracle: chain.oracle,
      inputs: selectedBoxes,
      recipient: args.changeAddress,
      uiFeeKey: this.uiFeeKey,
      buyback: chain.buyback,
    };
    return fissionTx(ctx, args.erg);
  }

  buildFusion(chain: ChainState, args: { erg: bigint } & Funding): EIP12UnsignedTransaction {
    validateChangeAddress(args.changeAddress);
    const quote = this.quoteFusion(chain, args.erg);
    const totalNeeded = SAFE_MIN_BOX_VALUE + quote.fees.total;
    const neededErg = totalNeeded > args.erg ? totalNeeded - args.erg : 0n;
    const selectedBoxes = selectInputs(args.userBoxes, {
      nanoErgs: neededErg,
      tokens: [
        { tokenId: this.deployment.tokens.neutron.id, amount: quote.neutronsIn },
        { tokenId: this.deployment.tokens.proton.id, amount: quote.protonsIn },
      ],
    });

    const ctx: TxContext = {
      height: chain.height,
      minFee: this.deployment.minFee,
      reactor: chain.reactor,
      oracle: chain.oracle,
      inputs: selectedBoxes,
      recipient: args.changeAddress,
      uiFeeKey: this.uiFeeKey,
      buyback: chain.buyback,
    };
    return fusionTx(ctx, args.erg);
  }

  buildBetaDecayPlus(chain: ChainState, args: { protons: bigint } & Funding): EIP12UnsignedTransaction {
    validateChangeAddress(args.changeAddress);
    const quote = this.quoteBetaDecayPlus(chain, args.protons);
    const neededErg = SAFE_MIN_BOX_VALUE + quote.fees.total;
    const selectedBoxes = selectInputs(args.userBoxes, {
      nanoErgs: neededErg,
      tokens: [{ tokenId: this.deployment.tokens.proton.id, amount: args.protons }],
    });

    const ctx: TxContext = {
      height: chain.height,
      minFee: this.deployment.minFee,
      reactor: chain.reactor,
      oracle: chain.oracle,
      inputs: selectedBoxes,
      recipient: args.changeAddress,
      uiFeeKey: this.uiFeeKey,
      buyback: chain.buyback,
    };
    return betaDecayPlusTx(ctx, args.protons);
  }

  buildBetaDecayMinus(chain: ChainState, args: { neutrons: bigint } & Funding): EIP12UnsignedTransaction {
    validateChangeAddress(args.changeAddress);
    const quote = this.quoteBetaDecayMinus(chain, args.neutrons);
    const neededErg = SAFE_MIN_BOX_VALUE + quote.fees.total;
    const selectedBoxes = selectInputs(args.userBoxes, {
      nanoErgs: neededErg,
      tokens: [{ tokenId: this.deployment.tokens.neutron.id, amount: args.neutrons }],
    });

    const ctx: TxContext = {
      height: chain.height,
      minFee: this.deployment.minFee,
      reactor: chain.reactor,
      oracle: chain.oracle,
      inputs: selectedBoxes,
      recipient: args.changeAddress,
      uiFeeKey: this.uiFeeKey,
      buyback: chain.buyback,
    };
    return betaDecayMinusTx(ctx, args.neutrons);
  }

  buildAdjustPeg(chain: ChainState, args: Funding): EIP12UnsignedTransaction {
    validateChangeAddress(args.changeAddress);
    const quote = this.quoteAdjustPeg(chain);
    if (!quote.allowed) {
      throw new GluonError("REACTOR_HEALTHY", "the reactor is in its healthy range, so Adjust Peg is not allowed");
    }

    const neededErg = this.minerFee + SAFE_MIN_BOX_VALUE;
    const selectedBoxes = selectInputs(args.userBoxes, { nanoErgs: neededErg });

    const ctx: TxContext = {
      height: chain.height,
      minFee: this.deployment.minFee,
      reactor: chain.reactor,
      oracle: chain.oracle,
      inputs: selectedBoxes,
      recipient: args.changeAddress,
      uiFeeKey: this.uiFeeKey,
      buyback: chain.buyback,
    };
    return adjustPegTx(ctx);
  }

  buildUpdateTreasury(
    chain: ChainState,
    args: { newTreasuryAddress: string } & Funding,
  ): EIP12UnsignedTransaction {
    validateChangeAddress(args.changeAddress);
    const newTreasuryPk = validateTreasuryAddress(args.newTreasuryAddress);
    if (newTreasuryPk === chain.state.treasury) {
      throw new GluonError("INVALID_ADDRESS", "the new treasury must differ from the current one");
    }

    const currentTreasuryAddr = ErgoAddress.fromPublicKey(chain.state.treasury, Network.Mainnet).encode();
    const neededErg = this.minerFee + SAFE_MIN_BOX_VALUE;
    const selectedBoxes = selectInputs(
      args.userBoxes,
      { nanoErgs: neededErg },
      { firstAddress: currentTreasuryAddr },
    );

    const ctx: TxContext = {
      height: chain.height,
      minFee: this.deployment.minFee,
      reactor: chain.reactor,
      oracle: chain.oracle,
      inputs: selectedBoxes,
      recipient: args.changeAddress,
      uiFeeKey: this.uiFeeKey,
      buyback: chain.buyback,
    };
    return updateTreasuryTx(ctx, newTreasuryPk);
  }
}
