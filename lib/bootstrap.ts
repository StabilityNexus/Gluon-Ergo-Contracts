import {
  type Amount,
  type Box,
  ErgoAddress,
  type ErgoTree,
  type ErgoUnsignedTransaction,
  OutputBuilder,
  RECOMMENDED_MIN_FEE_VALUE,
  SAFE_MIN_BOX_VALUE,
  TransactionBuilder,
} from "@fleet-sdk/core";
import { type BootstrapParameters, type DeploymentConfig, type TokenMetadata, contractConstants } from "./config.ts";
import { BUCKETS, type ReactorState, adjustedPrice, bucketStart, genesisSupplies, snapshot } from "../sdk/src/math.ts";
import { type ReactorTokens, reactorAssets, reactorRegisters } from "../sdk/src/reactor.ts";
import { type DeploymentRecord, type TokenRecord, templateHashes } from "./records.ts";

/** The state of a new reactor, derived from the bootstrap parameters and the current oracle price. */
export interface BootstrapPlan {
  /** Target price of one neutron at launch, in nanoERG: the oracle price times the initial peg factor. */
  price: bigint;
  /** Neutrons and protons in circulation at launch. They are burned, so nobody can redeem the seed (D11). */
  genesis: { neutrons: bigint; protons: bigint };
  /** The reactor box at launch. */
  state: ReactorState;
  /** Fusion ratio at launch, as the contract computes it (scaled by ONE). */
  q: bigint;
}

/**
 * Plans a reactor holding `seed` nanoERG of reserve whose fusion ratio is `fusionRatio` and whose protons
 * are worth `protonPrice` (N4). `height` is the current chain height.
 */
export function planBootstrap(
  p: BootstrapParameters,
  minFee: bigint,
  oraclePrice: bigint,
  treasury: string,
  height: number,
): BootstrapPlan {
  const price = adjustedPrice(oraclePrice, p.pegFactor);
  if (price === 0n) throw new Error("the oracle price times the peg factor rounds to zero");
  const genesis = genesisSupplies(p.seed, price, p.fusionRatio, p.protonPrice);
  if (genesis.neutrons === 0n || genesis.protons === 0n) {
    throw new Error("the seed is too small: no neutrons or no protons would be in circulation");
  }
  const state: ReactorState = {
    value: p.seed + minFee,
    neutrons: p.emission - genesis.neutrons,
    protons: p.emission - genesis.protons,
    totalNeutrons: p.emission,
    totalProtons: p.emission,
    treasury,
    devFeeRepaid: 0n,
    maxDevFee: p.maxDevFee,
    volumePlus: Array<bigint>(BUCKETS).fill(0n),
    volumeMinus: Array<bigint>(BUCKETS).fill(0n),
    lastBucketBlock: bucketStart(height + 1),
    pegFactor: p.pegFactor,
  };
  return { price, genesis, state, q: snapshot(state, minFee, oraclePrice).q };
}

/** Public key (hex) of a single-signature address. */
export function publicKeyOf(address: string): string {
  const [key] = ErgoAddress.decode(address).getPublicKeys();
  if (!key) throw new Error(`${address} is not a single-signature address`);
  return Array.from(key, (b) => b.toString(16).padStart(2, "0")).join("");
}

/**
 * EIP-4 metadata of a token: wallets display the name (R4), so it holds the ticker; the description (R5)
 * starts with the full name, as in "Gluon Stable Dollar (GSD): …" (N6).
 */
export function eip4(token: TokenMetadata, decimals: number) {
  return { name: token.ticker, description: `${token.name} (${token.ticker}): ${token.description}`, decimals };
}

export interface BootstrapTransactions {
  mintNft: ErgoUnsignedTransaction;
  mintNeutrons: ErgoUnsignedTransaction;
  mintProtons: ErgoUnsignedTransaction;
  createReactor: ErgoUnsignedTransaction;
  tokens: ReactorTokens;
}

/**
 * The four chained bootstrap transactions. Ergo mints one token per transaction (its id is the id of the
 * transaction's first input), and each mint goes to a box with its own EIP-4 registers, so the NFT cannot be
 * minted into the reactor box, whose registers the contract uses. The last transaction creates the reactor
 * box and burns the genesis neutrons and protons.
 */
export function bootstrapTransactions(
  config: DeploymentConfig,
  plan: BootstrapPlan,
  reactorTree: string,
  inputs: Box<Amount>[],
  changeAddress: string,
  height: number,
): BootstrapTransactions {
  const emission = plan.state.totalNeutrons;
  const mint = (from: Box<Amount>[], amount: bigint, token: TokenMetadata, decimals: number) =>
    new TransactionBuilder(height)
      .from(from)
      .to(new OutputBuilder(SAFE_MIN_BOX_VALUE, changeAddress).mintToken({ amount, ...eip4(token, decimals) }))
      .sendChangeTo(changeAddress)
      .payFee(RECOMMENDED_MIN_FEE_VALUE)
      .build();
  const mintNft = mint(inputs, 1n, config.tokens.nft, 0);
  const mintNeutrons = mint(mintNft.change, emission, config.tokens.neutron, 9);
  const mintProtons = mint(mintNeutrons.change, emission, config.tokens.proton, 9);
  const minted = [mintNft.outputs[0], mintNeutrons.outputs[0], mintProtons.outputs[0]];
  const tokens: ReactorTokens = {
    nft: minted[0].assets[0].tokenId,
    neutron: minted[1].assets[0].tokenId,
    proton: minted[2].assets[0].tokenId,
  };
  const createReactor = new TransactionBuilder(height)
    .from(minted, { ensureInclusion: true })
    .and.from(mintProtons.change)
    .to(
      new OutputBuilder(plan.state.value, reactorTree)
        .addTokens(reactorAssets(plan.state, tokens))
        .setAdditionalRegisters(reactorRegisters(plan.state)),
    )
    .burnTokens([
      { tokenId: tokens.neutron, amount: plan.genesis.neutrons },
      { tokenId: tokens.proton, amount: plan.genesis.protons },
    ])
    .sendChangeTo(changeAddress)
    .payFee(RECOMMENDED_MIN_FEE_VALUE)
    .build();
  return { mintNft, mintNeutrons, mintProtons, createReactor, tokens };
}

/** nanoERG the deployer needs: the seed plus the reactor's `_MinFee`, three mint boxes and four miner fees. */
export function bootstrapCost(plan: BootstrapPlan): bigint {
  return plan.state.value + 3n * SAFE_MIN_BOX_VALUE + 4n * RECOMMENDED_MIN_FEE_VALUE;
}

/**
 * The record of a bootstrapped deployment, for deployments/<network>.json (N3). `revision` is the commit
 * whose contract source was compiled into `tree`; `oraclePrice` is the price the plan was made with.
 */
export function deploymentRecord(
  config: DeploymentConfig,
  plan: BootstrapPlan,
  oraclePrice: bigint,
  tree: ErgoTree,
  txs: BootstrapTransactions,
  revision: string,
  compiler: string,
): DeploymentRecord {
  const constants = contractConstants(config);
  const reactor = txs.createReactor.outputs[0];
  const token = (id: string, metadata: TokenMetadata, decimals: number, emission: bigint): TokenRecord => ({
    id,
    name: metadata.ticker,
    decimals,
    emission: emission.toString(),
  });
  return {
    name: config.deployment,
    peg: config.peg,
    status: "live",
    network: config.network,
    contract: { revision, source: config.contract.source, ergoTreeVersion: tree.version, compiler },
    constants: {
      _MinFee: constants.minFee.toString(),
      _OracleFeePk: constants.oracleFeeTree,
      _OraclePoolNFT: constants.oraclePoolNft,
      _OracleBuybackNFT: constants.oracleBuybackNft,
    },
    oracleFeeAddress: config.oracle.buybackContractAddress,
    ergoTree: tree.toHex(),
    address: tree.toAddress().encode(),
    templateHash: templateHashes(tree),
    tokens: {
      nft: token(txs.tokens.nft, config.tokens.nft, 0, 1n),
      neutron: token(txs.tokens.neutron, config.tokens.neutron, 9, plan.state.totalNeutrons),
      proton: token(txs.tokens.proton, config.tokens.proton, 9, plan.state.totalProtons),
    },
    treasury: config.treasuryAddress,
    bootstrap: {
      ...config.bootstrap,
      oraclePrice: oraclePrice.toString(),
      burnedNeutrons: plan.genesis.neutrons.toString(),
      burnedProtons: plan.genesis.protons.toString(),
    },
    genesis: {
      transactionId: txs.createReactor.id,
      boxId: reactor.boxId,
      height: null,
      value: reactor.value.toString(),
      registers: { ...reactor.additionalRegisters },
    },
    transactions: [txs.mintNft.id, txs.mintNeutrons.id, txs.mintProtons.id, txs.createReactor.id],
  };
}
