import type { Amount, Box } from "@fleet-sdk/common";

export interface TokenInfo {
  id: string;
  name: string;
  decimals: number;
  emission: bigint;
}

export interface GluonDeployment {
  name: string; // "gluon-gold-v3", "gluon-dollar-v3"
  status: "live" | "superseded" | "unused"; // informational
  peg: string; // "1 gram of gold", "1 USD"
  ergoTree: string; // reactor contract
  address: string;
  tokens: { nft: TokenInfo; neutron: TokenInfo; proton: TokenInfo }; // token name = ticker (EIP-4 R4)
  minFee: bigint; // _MinFee
  oraclePoolNft: string; // _OraclePoolNFT
  oracleBuybackNft: string; // _OracleBuybackNFT
  oracleFeeTree: string; // _OracleFeePk: ErgoTree of the buyback contract
  initialPegFactor: bigint; // peg factor at genesis, ×10⁹: second element of the genesis R9
}

export interface GluonClientOptions {
  deployment: GluonDeployment | string; // a deployment, or the name of a bundled one
  nodeUrl: string; // HTTPS node serving the blockchain index API (/blockchain/…) and allowing browser requests
  uiFeeAddress?: string; // single-signature address of the optional 0.4 % UI fee (the Gluon UI sets none)
  minerFee?: bigint; // default 1_100_000n (Fleet's RECOMMENDED_MIN_FEE_VALUE)
  fetch?: typeof fetch; // injectable, for tests
}

export interface ReactorState {
  value: bigint;
  neutrons: bigint;
  protons: bigint; // value; tokens in the box
  totalNeutrons: bigint;
  totalProtons: bigint; // R4
  treasury: string; // R5: public key (hex)
  devFeeRepaid: bigint;
  maxDevFee: bigint; // R6
  volumePlus: bigint[];
  volumeMinus: bigint[]; // R7, R8
  lastBucketBlock: bigint;
  pegFactor: bigint; // R9
}

export interface ChainState {
  height: number; // node height; transactions are built for HEIGHT = height + 1
  reactor: Box<bigint>; // current reactor box, mempool-aware
  oracle: Box<bigint>; // oracle pool box, mempool-aware
  buyback?: Box<bigint>; // oracle buyback box, if one exists
  state: ReactorState; // parsed reactor
}

export interface Funding {
  userBoxes: Box<Amount>[]; // the wallet's UTXOs (EIP-12 boxes from get_utxos())
  changeAddress: string; // receives released tokens and change (EIP-12 get_change_address())
}

export interface ProtocolSnapshot {
  deployment: string;
  height: number; // node height; scripts see height + 1
  reserve: bigint; // box value − _MinFee, nanoERG: the total value locked
  neutronsInCirculation: bigint;
  protonsInCirculation: bigint;
  oraclePrice: bigint; // R4 of the oracle box: nanoERG per oracle unit (per kg of gold, per USD)
  oracleAge: number; // (height + 1) − the oracle box's creation height
  pegFactor: bigint; // R9, ×10⁹
  initialPegFactor: bigint;
  pegDrift: number; // pegFactor / initialPegFactor
  pegUnitPrice: bigint; // nanoERG per peg unit as defined at launch: oraclePrice × initialPegFactor / 10⁹ (per gram of gold, per USD)
  targetPrice: bigint; // nanoERG the contract targets per whole neutron: oraclePrice × pegFactor / 10⁹
  q: bigint; // fusion ratio, ×10⁹
  qNorm: bigint; // ratio the contract uses for prices, ×10⁹
  healthy: boolean; // 0.5 ≤ q ≤ 0.98
  adjustPeg?: { direction: "down" | "up"; steps: number }; // present when unhealthy
  neutronPrice: bigint; // nanoERG per whole neutron (10^decimals base units), at qNorm
  protonPrice: bigint; // nanoERG per whole proton
  reserveRatio: number; // percent: 100 / q
  protonLeverage?: number; // q / (1 − q), while q < 0.99 (Appendix A)
  crashCushion: { untilDepeg: number; untilAdjustment: number }; // fractions, 0.25 = 25 % (Appendix A)
  volumes: { plus: bigint[]; minus: bigint[] }; // 14 buckets of 720 blocks, index 0 = the bucket that contains HEIGHT
  devFee: { repaid: bigint; max: bigint };
  treasuryAddress: string;
  betaDecaysAvailable: boolean; // false when no buyback box exists
}

export interface ActionQuote {
  ergIn: bigint;
  ergOut: bigint; // nanoERG into or out of the reactor, before fees
  neutronsIn: bigint;
  neutronsOut: bigint;
  protonsIn: bigint;
  protonsOut: bigint;
  principal: bigint; // the value the fees are computed on, nanoERG
  fees: {
    dev: bigint;
    ui: bigint;
    oracle: bigint;
    outputsMinFee: bigint;
    miner: bigint;
    total: bigint;
  }; // outputsMinFee = _MinFee × number of fee outputs; total = the sum of all
  next: ReactorState; // the reactor after the transaction
  qAfter: bigint; // fusion ratio after it, ×10⁹
}

export interface AdjustPegQuote {
  allowed: boolean; // false when healthy
  direction?: "down" | "up";
  steps: number; // transactions needed to become healthy at the current oracle price
  nextPegFactor?: bigint;
}
