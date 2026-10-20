# Remediation Review: Findings from the `GluonWBoxGuardScript.es` Refactoring Audit (October 2026)

**Date:** 2026-10-11
**Previous audit:** [GluonWBoxGuardScript-refactoring-audit-2026-10.md](GluonWBoxGuardScript-refactoring-audit-2026-10.md) (audited up to `e50f98c`)
**Files reviewed:**
- [`modules/gluonw-base/src/main/resources/ErgoContracts/GluonW/Ergs/BoxGuardScripts/GluonWBoxGuardScript.es`](https://github.com/StabilityNexus/Gluon-Ergo-Contracts/blob/2cc695d0136013412951bf9fdc71877d69b66e30/modules/gluonw-base/src/main/resources/ErgoContracts/GluonW/Ergs/BoxGuardScripts/GluonWBoxGuardScript.es)
- [`modules/gluonw-base/src/main/scala/gluonw/common/GluonWAlgorithm.scala`](https://github.com/StabilityNexus/Gluon-Ergo-Contracts/blob/2cc695d0136013412951bf9fdc71877d69b66e30/modules/gluonw-base/src/main/scala/gluonw/common/GluonWAlgorithm.scala)

**Commit range:** `e50f98c` (the previous audit's HEAD) → `2cc695d` (current HEAD of `main`)

---

## Preamble

### Purpose and scope

This is a **remediation review**, not a new full audit. It checks whether the commits made after the previous audit fix the findings that the audit left open at `e50f98c`:

- **Bugs:** BUG-03, BUG-05, BUG-13, BUG-14, BUG-15, BUG-16, BUG-17
- **Comment defects:** DOC-03, DOC-04, DOC-05, DOC-06

It also checks whether those remediation commits introduced any new bugs (regressions). The review covers the 8 commits from `21bcb07` to `2cc695d`, all dated 2026-10-11. Together they change only the two files listed above.

Findings that the previous audit had already marked as fixed (BUG-01, 02, 04, 06–12; PRE-01 to PRE-07; DOC-01, DOC-02) were not re-examined. The only exception is a check that the remediation commits did not undo any of those fixes.

### Methodology

- Each remediation commit's diff was reviewed manually against the finding it claims to fix.
- The ad-hoc static checks from the previous audit were run again on a snapshot of the contract after every remediation commit:
  - scope-aware identifier resolution (undefined names, names used before they are declared, names used out of scope);
  - a missing-comma heuristic for multi-line argument lists;
  - detection of stray trailing `.`;
  - bracket balance.
- The diff statistics for `e50f98c..2cc695d` were checked to confirm no other files or regions were touched.

### Limitations

- As in the previous audit, **no ErgoScript or Scala compiler was run**: sbt is not installed in the review environment, and no tests were executed. "Compiles" and "does not compile" are conclusions from static analysis.
- **Independence note:** the off-chain change in `2cc695d` (fixing BUG-03) is the edit that Claude, the AI model producing this report, made at the user's request in the same session. The user then committed it. This review therefore checks that edit against the stated requirement (fee slope = 1), but it is not an independent review of it.

### AI model and effort level

This review was carried out by **Claude Opus 5.5** (model ID `claude-opus-5-5`) running in **Claude Code** (VS Code extension), at **effort level: high**. This is the same model and effort level as the previous audit. The findings have not been reviewed by a human auditor.

### Identifiers

Finding IDs from the previous audit (`BUG-nn`, `DOC-nn`) are reused unchanged. New bugs found in this review continue the previous numbering, starting at **BUG-18**.

---

## Summary

| Finding | Severity | Status at `2cc695d` | Remediating commit | Notes |
|---|---|---|---|---|
| BUG-03 | High | ✅ Fixed | `2cc695d` | Off-chain `phi1` changed to 1, matching the contract (for both beta decays). |
| BUG-05 | **Critical** | ⚠️ **Partially fixed** | `21bcb07` | Fixed in fission, BetaDecay+ and BetaDecay−. In fusion the replacement identifier is misspelled (**BUG-18**), so the script does not compile. |
| BUG-13 | Compile | ✅ Fixed | `ff08367` | Stray `.` removed. |
| BUG-14 | Compile | ✅ Fixed | `bf24e62` | `uiFeesPayout` → `uiFeePayout`. |
| BUG-15 | Compile | ✅ Fixed | `45086b0` | Leftover `fees` line deleted. |
| BUG-16 | Compile | ✅ Fixed | `e60542c` | `NEUTRONS_TOTAL_SUPPLY` and `PROTONS_TOTAL_SUPPLY` re-declared. |
| BUG-17 | Compile | ✅ Fixed | `438885e` | Declaration renamed to `__outVolumeMinusValidated`. |
| DOC-03 | Doc | ✅ Fixed | `1560de4` | Heading now reads "BetaDecayMinus". |
| DOC-04 | Doc | ✅ Fixed | `1560de4` | Fusion equation now uses `phiFusion`. |
| DOC-05 | Doc | ✅ Fixed | `1560de4` | Comment now says "at most 34 blocks". |
| DOC-06 | Doc | ✅ Fixed | `1560de4` | "circuluting" → "circulating". |
| **BUG-18** | Compile | ❌ **New, not fixed** | — | Introduced by `21bcb07`: undefined identifier `MaxDevFeeThreshold` in the fusion branch. |

**Overall:** 10 of the 11 open findings are fully fixed. BUG-05 is fixed in three of its four locations. The fusion location was changed to `MaxDevFeeThreshold`, an identifier that does not exist (BUG-18). As a result, **the contract at `2cc695d` still does not compile**. BUG-18 is the only remaining static-analysis finding. It is a one-word fix, and fixing it also completes the BUG-05 remediation.

---

## Per-commit analysis

### 1. `21bcb07` — Fix BUG-05 from refactoring audit
Adds `val cSameR6MaxDevFeeThreshold: Boolean = MAX_DEV_FEE_THRESHOLD == OUT_MAX_DEV_FEE_THRESHOLD` (line 175). In the four branches where `cSameR6` was wrong, it replaces `cSameR6` with this new check. `cSameR6` itself remains, correctly, in Adjust Peg and Update Treasury, where R6 must not change.

The new check allows `R6._1` (total dev fees repaid) to change while keeping `R6._2` (maximum threshold) fixed. That restores the baseline semantics, and `devFeeRepaidValueAdded` in `cFees` still constrains `R6._1` exactly. The new check duplicates `maxDevFeeThresholdSame` in `cFees` (line 240), but this is harmless. `MAX_DEV_FEE_THRESHOLD` and `OUT_MAX_DEV_FEE_THRESHOLD` are declared at the top level before line 175, so they are in scope.

#### Previous findings fixed
- **BUG-05: partially fixed.** Correct in fission (line 281), BetaDecay+ (line 439) and BetaDecay− (line 550).

#### Bugs introduced
- **BUG-18 (Compile).** In the fusion branch (line 309 at HEAD), `cSameR6` was replaced with `MaxDevFeeThreshold` instead of `cSameR6MaxDevFeeThreshold`. No declaration of `MaxDevFeeThreshold` exists, so the script cannot compile. This is the same "incomplete renaming" pattern as BUG-07 and BUG-17. *Fix:* change `MaxDevFeeThreshold` to `cSameR6MaxDevFeeThreshold` on that line. That also completes BUG-05.

### 2. `ff08367` — Fix BUG-13 from refactoring audit
Removes the trailing `.` from `val uiFee: BigInt = 4L.toBigInt` (line 107). No other changes.
#### Previous findings fixed
- **BUG-13: fixed.**
#### Bugs introduced
None.

### 3. `bf24e62` — Fix BUG-14 from refactoring audit
Changes `uiFeesPayout` to `uiFeePayout` in the `uiFeesPaid` condition (line 245). `uiFeePayout` is declared earlier in the same block.
#### Previous findings fixed
- **BUG-14: fixed.**
#### Bugs introduced
None.

### 4. `45086b0` — Fix BUG-15 from refactoring audit
Deletes the unused line `val uiFees: (Coll[Byte], BigInt) = … fees(2) else fees(1)`. No remaining code refers to `uiFees` or `fees`.
#### Previous findings fixed
- **BUG-15: fixed.**
#### Bugs introduced
None.

### 5. `e60542c` — Fix BUG-16 from refactoring audit
Adds `val NEUTRONS_TOTAL_SUPPLY: Long = GLUON_BOX.R4[(Long, Long)].get._1` and `val PROTONS_TOTAL_SUPPLY: Long = GLUON_BOX.R4[(Long, Long)].get._2` (lines 67–68). They come after `GLUON_BOX` is declared and before their use in `supplyNeutrons` and `supplyProtons`. The values are the same as the baseline's `ASSET_TOTAL_SUPPLY_REGISTER._1` and `._2`.
#### Previous findings fixed
- **BUG-16: fixed.**
#### Bugs introduced
None.

### 6. `438885e` — Fix BUG-17 from refactoring audit
In the BetaDecay+ branch, renames the declaration `__OUT_VOLUME_MINUSValidated` to `__outVolumeMinusValidated` (line 394), which matches its use at line 440.
#### Previous findings fixed
- **BUG-17: fixed.**
#### Bugs introduced
None.

### 7. `1560de4` — Fix DOC-03, DOC-04, DOC-05 and DOC-06 from refactoring audit
Comment-only changes. Apart from whitespace inside one comment line, the code is unchanged.
#### Previous findings fixed
- **DOC-03: fixed.** The heading at line 150 now reads "BetaDecayMinus".
- **DOC-04: fixed.** The fusion equation at line 286 now uses `phiFusion`.
- **DOC-05: fixed.** The comment at line 185 now says "at most 34 blocks". The "(~70 min)" approximation is unchanged; 34 blocks is about 68 min at 2 min per block.
- **DOC-06: fixed.** The typo at line 131 is corrected.
#### Bugs introduced
None.

### 8. `2cc695d` — Fix BUG-03 from refactoring audit
In `GluonWAlgorithm.varPhiBeta`, changes `val phi1: Long = (precision * 0.5).toLong` to `val phi1: Long = precision`, i.e. a slope of 1 (line 99). This is the decided resolution: keep the on-chain slope of 1 and align the off-chain code with it. `varPhiBeta` is the only off-chain implementation of this fee, and both beta-decay paths call it (lines 280 and 313). So this fixes BUG-03 for BetaDecay− and also removes the baseline mismatch for BetaDecay+ noted in the previous audit's appendix.
#### Previous findings fixed
- **BUG-03: fixed.** See the independence note in the preamble.
#### Bugs introduced
None.

---

## Open findings at `2cc695d`

| ID | Severity | Introduced in | Location (HEAD) | Description | Suggested fix |
|---|---|---|---|---|---|
| **BUG-18** | Compile | `21bcb07` — Fix BUG-05 from refactoring audit | `GluonWBoxGuardScript.es`, line 309 (fusion branch) | Undefined identifier `MaxDevFeeThreshold`; the script does not compile | Replace it with `cSameR6MaxDevFeeThreshold` |
| BUG-05 (residual) | Critical (until BUG-18 is fixed) | `2f43edd` | line 309 | The fusion branch's part of the BUG-05 fix only takes effect once BUG-18 is fixed | Same as BUG-18 |

All other findings left open by the previous audit are resolved. No comment defects remain open.

Before deployment, the maintainers should fix BUG-18, compile the contract, and run the test suites (`FissionTxSpec`, `FusionTxSpec`, `BetaDecayPlusSpec`, `BetaDecayMinusSpec`, `AdjustPegTxSpec`, `GluonWFeesSpec`). This review is based on static analysis alone.

---

## Appendix: observations outside the scope of this review

These are not findings of the previous audit and were not introduced by the remediation commits. They are recorded because they affect whether the BUG-03 fix achieves its purpose, which is that off-chain–built transactions are accepted on-chain.

1. **The off-chain fusion ratio differs from the on-chain one.** `GluonWConstants.fusionRatio` (`GluonWAlgorithm.scala`, lines 66–78) computes `min(S_n·P_t / R, qStar)` with `qStar = 0.66`. It does not apply the peg factor. The contract computes `q = S_n · priceAdjusted / R`, where `priceAdjusted` includes `PEG_FACTOR`, and then `qNorm = min(one·q / (q + one − qStar), q)` with `qStar = 0.99`. Every amount the off-chain code derives from the fusion ratio is therefore likely to differ from what the contract expects. That includes proton and neutron prices, beta-decay output amounts, and the volumes used in `varPhiBeta`. Even with the `phi1` values now aligned, off-chain–built beta-decay transactions may still be rejected. The off-chain code should be reconciled with the contract.
2. The pre-existing issue noted in the previous audit's appendix (every transaction type reads `INPUTS(1)` and `CONTEXT.dataInputs(0)` at the top level) is unchanged.
