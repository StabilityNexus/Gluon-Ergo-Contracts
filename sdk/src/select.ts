import type { Amount, Box } from "@fleet-sdk/core";
import { ErgoAddress, SAFE_MIN_BOX_VALUE } from "@fleet-sdk/core";
import { GluonError } from "./errors.ts";

export interface TargetToken {
  tokenId: string;
  amount: bigint;
}

export interface SelectionTarget {
  nanoErgs: bigint;
  tokens?: TargetToken[];
}

export interface SelectionOptions {
  firstAddress?: string;
}

/**
 * Selects the user's inputs to cover the required ERG and tokens.
 * Priority:
 * 1. For each token, select boxes holding it (largest token amount first) until covered.
 * 2. If ERG is not covered, add boxes by largest value until covered.
 * 3. If firstAddress is given, prioritize boxes matching that address.
 * 4. Ensure change is either 0 or at least SAFE_MIN_BOX_VALUE, adding boxes if needed.
 * 5. Throws INSUFFICIENT_FUNDS if requirements cannot be met.
 */
export function selectInputs<T extends Amount = bigint>(
  userBoxes: Box<T>[],
  target: SelectionTarget,
  options?: SelectionOptions,
): Box<T>[] {
  let firstTree: string | undefined;
  if (options?.firstAddress) {
    try {
      firstTree = ErgoAddress.fromBase58(options.firstAddress).ergoTree;
    } catch {
      firstTree = options.firstAddress;
    }
  }

  const selectedBoxes: Box<T>[] = [];
  const selectedBoxIds = new Set<string>();

  const isFirst = (b: Box<T>): boolean => firstTree !== undefined && b.ergoTree === firstTree;

  const addBox = (box: Box<T>) => {
    if (!selectedBoxIds.has(box.boxId)) {
      selectedBoxIds.add(box.boxId);
      selectedBoxes.push(box);
    }
  };

  // Consolidate target tokens
  const requiredTokens = new Map<string, bigint>();
  for (const t of target.tokens ?? []) {
    if (t.amount > 0n) {
      requiredTokens.set(t.tokenId, (requiredTokens.get(t.tokenId) ?? 0n) + BigInt(t.amount));
    }
  }

  // 1. Cover each required token
  for (const [tokenId, requiredAmount] of requiredTokens.entries()) {
    // Available boxes holding this token
    const tokenCandidates = userBoxes
      .filter((b) => b.assets.some((a) => a.tokenId === tokenId))
      .sort((a, b) => {
        const aFirst = isFirst(a);
        const bFirst = isFirst(b);
        if (aFirst && !bFirst) return -1;
        if (!aFirst && bFirst) return 1;

        const aAmt = BigInt(a.assets.find((ast) => ast.tokenId === tokenId)?.amount ?? 0n);
        const bAmt = BigInt(b.assets.find((ast) => ast.tokenId === tokenId)?.amount ?? 0n);
        return aAmt < bAmt ? 1 : aAmt > bAmt ? -1 : 0;
      });

    let accumulatedToken = 0n;
    for (const b of selectedBoxes) {
      accumulatedToken += BigInt(b.assets.find((ast) => ast.tokenId === tokenId)?.amount ?? 0n);
    }

    for (const b of tokenCandidates) {
      if (accumulatedToken >= requiredAmount) break;
      if (!selectedBoxIds.has(b.boxId)) {
        addBox(b);
        accumulatedToken += BigInt(b.assets.find((ast) => ast.tokenId === tokenId)?.amount ?? 0n);
      }
    }

    if (accumulatedToken < requiredAmount) {
      throw new GluonError(
        "INSUFFICIENT_FUNDS",
        `insufficient funds for token ${tokenId}: needed ${requiredAmount}, available ${accumulatedToken}`,
      );
    }
  }

  // 2. Cover ERG requirement
  let accumulatedErg = selectedBoxes.reduce((acc, b) => acc + BigInt(b.value), 0n);

  if (accumulatedErg < target.nanoErgs) {
    const ergCandidates = userBoxes
      .filter((b) => !selectedBoxIds.has(b.boxId))
      .sort((a, b) => {
        const aFirst = isFirst(a);
        const bFirst = isFirst(b);
        if (aFirst && !bFirst) return -1;
        if (!aFirst && bFirst) return 1;

        const aVal = BigInt(a.value);
        const bVal = BigInt(b.value);
        return aVal < bVal ? 1 : aVal > bVal ? -1 : 0;
      });

    for (const b of ergCandidates) {
      if (accumulatedErg >= target.nanoErgs) break;
      addBox(b);
      accumulatedErg += BigInt(b.value);
    }

    if (accumulatedErg < target.nanoErgs) {
      throw new GluonError(
        "INSUFFICIENT_FUNDS",
        `insufficient funds for ERG: needed ${target.nanoErgs}, available ${accumulatedErg}`,
      );
    }
  }

  // 3. Change requirement:
  // If change exists (either excess ERG > 0 or unspent tokens), it must be >= SAFE_MIN_BOX_VALUE.
  const hasTokenChange = (): boolean => {
    const tokenTotals = new Map<string, bigint>();
    for (const b of selectedBoxes) {
      for (const a of b.assets) {
        tokenTotals.set(a.tokenId, (tokenTotals.get(a.tokenId) ?? 0n) + BigInt(a.amount));
      }
    }
    for (const [tokenId, total] of tokenTotals.entries()) {
      const targetAmt = requiredTokens.get(tokenId) ?? 0n;
      if (total > targetAmt) return true;
    }
    return false;
  };

  let changeErg = accumulatedErg - target.nanoErgs;
  const tokenChange = hasTokenChange();

  if (changeErg > 0n || tokenChange) {
    if (changeErg < SAFE_MIN_BOX_VALUE) {
      const additionalCandidates = userBoxes
        .filter((b) => !selectedBoxIds.has(b.boxId))
        .sort((a, b) => {
          const aFirst = isFirst(a);
          const bFirst = isFirst(b);
          if (aFirst && !bFirst) return -1;
          if (!aFirst && bFirst) return 1;

          const aVal = BigInt(a.value);
          const bVal = BigInt(b.value);
          return aVal < bVal ? 1 : aVal > bVal ? -1 : 0;
        });

      for (const b of additionalCandidates) {
        if (changeErg >= SAFE_MIN_BOX_VALUE) break;
        addBox(b);
        accumulatedErg += BigInt(b.value);
        changeErg = accumulatedErg - target.nanoErgs;
      }

      if (changeErg < SAFE_MIN_BOX_VALUE) {
        throw new GluonError(
          "INSUFFICIENT_FUNDS",
          `insufficient funds for change box: change is ${changeErg} nanoERG, need at least ${SAFE_MIN_BOX_VALUE}`,
        );
      }
    }
  }

  // If firstAddress was requested, ensure any matching boxes are placed first
  if (options?.firstAddress) {
    selectedBoxes.sort((a, b) => {
      const aFirst = isFirst(a);
      const bFirst = isFirst(b);
      if (aFirst && !bFirst) return -1;
      if (!aFirst && bFirst) return 1;
      return 0;
    });
  }

  return selectedBoxes;
}
