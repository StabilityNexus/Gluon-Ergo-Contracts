// BigInt mirror of the arithmetic in contracts/Gluon.es.
// Integer division makes results depend on evaluation order, so every expression below keeps the
// contract's order of operations. Amounts are in nanoERG and in token base units (9 decimals).

export const ONE = 1_000_000_000n;
export const Q_STAR = (99n * ONE) / 100n;
export const Q_UPPER = (98n * ONE) / 100n;
export const Q_LOWER = ONE / 2n;
export const PHI_FISSION = ONE / 1000n;
export const PHI_FUSION = ONE / 200n;
export const PHI_0 = ONE / 200n;
export const PHI_1 = ONE;
export const BLOCKS_PER_BUCKET = 720;
export const BUCKETS = 14;
export const FEE_DENOM = 1000n;
export const INITIAL_DEV_FEE = 5n;
export const ORACLE_FEE = 1n;
export const UI_FEE = 4n;
/** The oracle box must have been created fewer than this many blocks before HEIGHT. */
export const MAX_ORACLE_AGE = 35;

/** Contents of a reactor box: its value, the tokens it holds and its registers. */
export interface ReactorState {
  value: bigint;
  neutrons: bigint;
  protons: bigint;
  totalNeutrons: bigint; // R4._1
  totalProtons: bigint; // R4._2
  treasury: string; // R5: public key (hex) of the single-signature treasury
  devFeeRepaid: bigint; // R6._1
  maxDevFee: bigint; // R6._2
  volumePlus: bigint[]; // R7
  volumeMinus: bigint[]; // R8
  lastBucketBlock: bigint; // R9._1
  pegFactor: bigint; // R9._2
}

/** Values the contract derives from the input reactor box and the oracle price. */
export interface Snapshot {
  reserve: bigint;
  neutronSupply: bigint;
  protonSupply: bigint;
  price: bigint; // priceAdjusted: target price of one neutron, in nanoERG
  q: bigint; // fusion ratio, scaled by ONE
  qNorm: bigint; // normalized fusion ratio, scaled by ONE
}

export interface Fees {
  oracle: bigint;
  dev: bigint;
  ui: bigint;
}

export interface ActionResult {
  next: ReactorState;
  fees: Fees;
}

export function adjustedPrice(oraclePrice: bigint, pegFactor: bigint): bigint {
  return (oraclePrice * pegFactor) / ONE;
}

export function normalizedFusionRatio(q: bigint): bigint {
  const n = (ONE * q) / (q + ONE - Q_STAR);
  return n < q ? n : q;
}

export function isHealthy(q: bigint): boolean {
  return q >= Q_LOWER && q <= Q_UPPER;
}

export interface CrashCushion {
  untilDepeg: number;
  untilAdjustment: number;
}

/** Fraction by which ERG can fall against the peg before neutrons depeg or reactor leaves healthy range. */
export function crashCushion(q: bigint): CrashCushion {
  const qFraction = Number(q) / Number(ONE);
  return {
    untilDepeg: Math.max(0, 1 - qFraction / 0.99),
    untilAdjustment: Math.max(0, 1 - qFraction / 0.98),
  };
}

/** Proton leverage: q / (1 - q), defined while q < 0.99. */
export function protonLeverage(q: bigint): number | undefined {
  if (q >= (99n * ONE) / 100n) return undefined;
  const qFraction = Number(q) / Number(ONE);
  return qFraction / (1 - qFraction);
}

export function snapshot(s: ReactorState, minFee: bigint, oraclePrice: bigint): Snapshot {
  const reserve = s.value - minFee;
  const neutronSupply = s.totalNeutrons - s.neutrons;
  const protonSupply = s.totalProtons - s.protons;
  const price = adjustedPrice(oraclePrice, s.pegFactor);
  const q = (neutronSupply * price) / reserve;
  return { reserve, neutronSupply, protonSupply, price, q, qNorm: normalizedFusionRatio(q) };
}

export function valueOfProtons(snap: Snapshot, amount: bigint): bigint {
  const protonPrice = ((ONE - snap.qNorm) * snap.reserve) / snap.protonSupply;
  return (amount * protonPrice) / ONE;
}

export function valueOfNeutrons(snap: Snapshot, amount: bigint): bigint {
  const neutronPrice = (snap.qNorm * snap.reserve) / snap.neutronSupply;
  return (amount * neutronPrice) / ONE;
}

/** Fees owed on a transaction whose value (the contract's `principal`) is given in nanoERG. */
export function fees(s: ReactorState, principal: bigint, betaDecay: boolean, uiFee: boolean): Fees {
  const oracle = betaDecay ? (ORACLE_FEE * principal) / FEE_DENOM : 0n;
  const dev =
    s.devFeeRepaid < s.maxDevFee
      ? (((INITIAL_DEV_FEE * principal) / FEE_DENOM) * (s.maxDevFee - s.devFeeRepaid)) / s.maxDevFee
      : 0n;
  const ui = uiFee ? (UI_FEE * principal) / FEE_DENOM : 0n;
  return { oracle, dev, ui };
}

/** Fission: `erg` nanoERG go into the reserve; neutrons and protons come out. */
export function fission(s: ReactorState, minFee: bigint, erg: bigint, uiFee = false): ActionResult {
  const reserve = s.value - minFee;
  const neutronsOut = (erg * (s.totalNeutrons - s.neutrons) * (ONE - PHI_FISSION)) / reserve / ONE;
  const protonsOut = (erg * (s.totalProtons - s.protons) * (ONE - PHI_FISSION)) / reserve / ONE;
  const f = fees(s, erg, false, uiFee);
  return {
    next: {
      ...s,
      value: s.value + erg,
      neutrons: s.neutrons - neutronsOut,
      protons: s.protons - protonsOut,
      devFeeRepaid: s.devFeeRepaid + f.dev,
    },
    fees: f,
  };
}

/** Fusion: `erg` nanoERG come out of the reserve; neutrons and protons go in. */
export function fusion(s: ReactorState, minFee: bigint, erg: bigint, uiFee = false): ActionResult {
  const denominator = (s.value - minFee) * (ONE - PHI_FUSION);
  const neutronsIn = (erg * (s.totalNeutrons - s.neutrons) * ONE) / denominator;
  const protonsIn = (erg * (s.totalProtons - s.protons) * ONE) / denominator;
  const f = fees(s, erg, false, uiFee);
  return {
    next: {
      ...s,
      value: s.value - erg,
      neutrons: s.neutrons + neutronsIn,
      protons: s.protons + protonsIn,
      devFeeRepaid: s.devFeeRepaid + f.dev,
    },
    fees: f,
  };
}

/** First block of the volume bucket that contains `height`. */
export function bucketStart(height: number): bigint {
  return BigInt(Math.floor(height / BLOCKS_PER_BUCKET) * BLOCKS_PER_BUCKET);
}

/** Number of buckets elapsed since the last recorded one (the contract's nDays), capped at BUCKETS. */
export function elapsedBuckets(height: number, lastBucketBlock: bigint): number {
  const n = Number((BigInt(height) - lastBucketBlock) / BigInt(BLOCKS_PER_BUCKET));
  return n >= BUCKETS ? BUCKETS : n;
}

/** Shift a volume register by `nDays` buckets and add `amount` to the current bucket. */
export function shiftVolumes(volumes: bigint[], nDays: number, amount: bigint): bigint[] {
  if (nDays === 0) return [volumes[0] + amount, ...volumes.slice(1)];
  return [amount, ...Array<bigint>(nDays - 1).fill(0n), ...volumes.slice(0, BUCKETS - nDays)];
}

function sum(values: bigint[]): bigint {
  return values.reduce((a, b) => a + b, 0n);
}

/**
 * Beta decay +: `protons` go into the reactor; neutrons come out.
 * `height` is the HEIGHT the script will see, i.e. the height of the block that includes the transaction.
 */
export function betaDecayPlus(
  s: ReactorState,
  minFee: bigint,
  oraclePrice: bigint,
  protons: bigint,
  height: number,
  uiFee = false,
): ActionResult {
  const snap = snapshot(s, minFee, oraclePrice);
  const worth = valueOfProtons(snap, protons);
  const nDays = elapsedBuckets(height, s.lastBucketBlock);
  const volumePlus = shiftVolumes(s.volumePlus, nDays, worth);
  const volumeMinus = shiftVolumes(s.volumeMinus, nDays, 0n);
  const plus = sum(volumePlus);
  const minus = sum(volumeMinus);
  const volume = minus > plus ? 0n : plus - minus;
  const varPhiBeta = PHI_0 + (PHI_1 * volume) / snap.reserve;
  const ratio1 = (protons * (ONE - varPhiBeta)) / snap.protonSupply;
  const ratio2 = ((ONE - snap.qNorm) * snap.neutronSupply) / ONE;
  const neutronsOut = (ratio1 * ratio2) / snap.qNorm;
  const f = fees(s, worth, true, uiFee);
  return {
    next: {
      ...s,
      neutrons: s.neutrons - neutronsOut,
      protons: s.protons + protons,
      volumePlus,
      volumeMinus,
      lastBucketBlock: bucketStart(height),
      devFeeRepaid: s.devFeeRepaid + f.dev,
    },
    fees: f,
  };
}

/** Beta decay −: `neutrons` go into the reactor; protons come out. `height` as for {@link betaDecayPlus}. */
export function betaDecayMinus(
  s: ReactorState,
  minFee: bigint,
  oraclePrice: bigint,
  neutrons: bigint,
  height: number,
  uiFee = false,
): ActionResult {
  const snap = snapshot(s, minFee, oraclePrice);
  const worth = valueOfNeutrons(snap, neutrons);
  const nDays = elapsedBuckets(height, s.lastBucketBlock);
  const volumeMinus = shiftVolumes(s.volumeMinus, nDays, worth);
  const volumePlus = shiftVolumes(s.volumePlus, nDays, 0n);
  const plus = sum(volumePlus);
  const minus = sum(volumeMinus);
  const volume = plus > minus ? 0n : minus - plus;
  const varPhiBeta = PHI_0 + (PHI_1 * volume) / snap.reserve;
  const ratio1 = (neutrons * (ONE - varPhiBeta)) / snap.neutronSupply;
  const ratio2 = (snap.qNorm * snap.protonSupply) / ONE;
  const protonsOut = (ratio1 * ratio2) / (ONE - snap.qNorm);
  const f = fees(s, worth, true, uiFee);
  return {
    next: {
      ...s,
      neutrons: s.neutrons + neutrons,
      protons: s.protons - protonsOut,
      volumePlus,
      volumeMinus,
      lastBucketBlock: bucketStart(height),
      devFeeRepaid: s.devFeeRepaid + f.dev,
    },
    fees: f,
  };
}

/**
 * Adjust Peg: when the fusion ratio is outside the healthy range, anyone may move the peg factor by 1 %.
 * Returns undefined when the reactor is healthy and the transaction is therefore not allowed.
 */
export function adjustPeg(s: ReactorState, minFee: bigint, oraclePrice: bigint): ReactorState | undefined {
  const { q } = snapshot(s, minFee, oraclePrice);
  if (q > Q_UPPER) return { ...s, pegFactor: (s.pegFactor * 99n) / 100n };
  if (q < Q_LOWER) return { ...s, pegFactor: (s.pegFactor * 101n) / 100n };
  return undefined;
}

/**
 * The states a reactor goes through while Adjust Peg is repeated until the reactor is healthy again: one
 * state per transaction, none if the reactor is already healthy.
 */
export function pegAdjustments(s: ReactorState, minFee: bigint, oraclePrice: bigint, limit = 10_000): ReactorState[] {
  const states: ReactorState[] = [];
  for (let next = adjustPeg(s, minFee, oraclePrice); next; next = adjustPeg(next, minFee, oraclePrice)) {
    if (states.length === limit) throw new Error(`Adjust Peg does not make the reactor healthy within ${limit} steps`);
    states.push(next);
  }
  return states;
}

/**
 * Circulating supplies for a new reactor with reserve `reserve` and neutron target price `price` (nanoERG),
 * such that it starts at fusion ratio `q0` (scaled by ONE) with one proton worth `protonPrice` (nanoERG).
 */
export function genesisSupplies(
  reserve: bigint,
  price: bigint,
  q0 = (6n * ONE) / 10n,
  protonPrice = ONE,
): { neutrons: bigint; protons: bigint } {
  return {
    neutrons: (q0 * reserve) / price,
    protons: ((ONE - q0) * reserve) / protonPrice,
  };
}
