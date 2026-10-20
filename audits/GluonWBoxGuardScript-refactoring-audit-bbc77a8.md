# Refactoring Audit: Commit `bbc77a8` of `GluonWBoxGuardScript.es`

**Date:** 2026-10-11
**Audited file:** [`modules/gluonw-base/src/main/resources/ErgoContracts/GluonW/Ergs/BoxGuardScripts/GluonWBoxGuardScript.es`](https://github.com/StabilityNexus/Gluon-Ergo-Contracts/blob/bbc77a86ea555899890fb9f4f09dfcb039abdad2/modules/gluonw-base/src/main/resources/ErgoContracts/GluonW/Ergs/BoxGuardScripts/GluonWBoxGuardScript.es)
**Audited commit:** `bbc77a8` — "Refactor to remove redundant check" (Zahnentferner, 2026-10-11 05:19:59 +0300)
**Parent commit:** `186ba5c` — "Fix BUG-18 from refactoring audit remediation review"

**Related reports:**
- [GluonWBoxGuardScript-refactoring-audit-2026-10.md](GluonWBoxGuardScript-refactoring-audit-2026-10.md) (refactoring audit)
- [GluonWBoxGuardScript-remediation-review-2026-10.md](GluonWBoxGuardScript-remediation-review-2026-10.md) (remediation review)

---

## Preamble

### Scope

This audit covers **only** the single commit `bbc77a8`, which is described as a refactoring. It checks:

1. whether the commit preserves the contract's logic, meaning the set of transactions the script accepts;
2. whether it introduces bugs, with particular attention to missing commas and other punctuation, and to undefined identifiers left behind by incomplete renamings;
3. whether it fixes any bugs.

The parent commit `186ba5c` is **not** audited here. It is used only as the "before" state of the file. One defect present in that parent is mentioned below, because `bbc77a8` removes it.

### Methodology

- The commit's diff was reviewed manually.
- Each removed check was compared with the conditions that remain in every affected branch, to decide whether the change preserves logic.
- The ad-hoc static checks from the earlier reports were run on the file both before the commit (at `186ba5c`) and after it (at `bbc77a8`):
  - scope-aware identifier resolution;
  - a missing-comma heuristic for multi-line argument lists;
  - detection of stray trailing `.`;
  - bracket balance.
- A word-level diff confirmed that the commit changes nothing beyond the lines discussed below.

### Limitations

No ErgoScript compiler was run, because sbt is not installed in the audit environment, and no tests were executed. The conclusions come from static analysis and manual review.

### AI model and effort level

This audit was carried out by **Claude Opus 5.5** (model ID `claude-opus-5-5`) running in **Claude Code** (VS Code extension), at **effort level: low** (reasoning-effort setting 15). The findings have not been reviewed by a human auditor.

### Identifiers

IDs continue the numbering of the earlier reports. The last ID used there was BUG-18, so the next new ID is **BUG-19**.

---

## Commit analysis

### `bbc77a8` — Refactor to remove redundant check

**Change.** The commit deletes the top-level check `val cSameR6MaxDevFeeThreshold: Boolean = MAX_DEV_FEE_THRESHOLD == OUT_MAX_DEV_FEE_THRESHOLD`. It removes that check from the final `allOf` of the fission, fusion, BetaDecay+ and BetaDecay− branches, and adds the comment `// cFees also checks that R6._2 remains the same` next to `cFees` in each of those branches.

**Logic preserved: yes.**
- In each of the four branches, the removed conjunct is identical to `maxDevFeeThresholdSame = OUT_MAX_DEV_FEE_THRESHOLD == MAX_DEV_FEE_THRESHOLD` (line 239). That check is part of `cFees` (lines 255–258), and `cFees` is a conjunct in all four branches (lines 281, 309, 442 and 553).
- `MAX_DEV_FEE_THRESHOLD` and `OUT_MAX_DEV_FEE_THRESHOLD` are the `._2` components of the input and output R6 (lines 72–77). So the removed conjunct and `maxDevFeeThresholdSame` express exactly the same condition, and removing the duplicate does not change which transactions are accepted.
- R6 remains fully constrained in these branches. `devFeeRepaidValueAdded` constrains `R6._1` and `maxDevFeeThresholdSame` constrains `R6._2`.
- The Adjust Peg and Update Treasury branches are untouched and still require all of R6 to be unchanged through `cSameR6`, as they should.

**Punctuation and naming.**
- In fission and fusion, `cFees` is the last element and is followed directly by `)))`. In BetaDecay+ and BetaDecay−, `cFees,` is still followed by `cOracle`.
- The comment is placed after the comma in each case, so it doesn't interfere with parsing.
- No reference to the deleted identifier remains.
- Static checks on `bbc77a8` report **no findings**.

#### Bugs introduced
None.

#### Bugs fixed
- **BUG-19 (Compile; introduced in `186ba5c`, outside this audit's scope).** In the parent commit, the attempted fix for BUG-18 replaced the undefined `MaxDevFeeThreshold` in the fusion branch with `c6SameR6MaxDevFeeThreshold`. That is another misspelling: the declared name was `cSameR6MaxDevFeeThreshold`. So the contract still did not compile at `186ba5c`, and BUG-18 had not actually been fixed. `bbc77a8` removes this conjunct from the fusion branch altogether. The undefined identifier is gone, which fixes BUG-19 and resolves BUG-18 as a side effect.
- **BUG-05 (from the refactoring audit): now fully resolved.** With the fusion branch corrected, none of the four branches requires all of R6 to be unchanged. `R6._1` is constrained only by the dev-fee accounting and `R6._2` by `maxDevFeeThresholdSame`, which is the intended behaviour.

---

## Unresolved bugs

**None.** The audited commit introduces no bugs. With this commit, none of the findings from the earlier reports remains open (all BUG-nn and DOC-nn items), and the static checks report no remaining issues in the file. `bbc77a8` is the first state since the start of the October refactoring in which the static checks find nothing.

Before deployment, the contract should still be compiled and the test suites run (`FissionTxSpec`, `FusionTxSpec`, `BetaDecayPlusSpec`, `BetaDecayMinusSpec`, `AdjustPegTxSpec`, `GluonWFeesSpec`), because this audit is based on static analysis alone. The off-chain/on-chain fusion-ratio mismatch noted in the appendix of the remediation review is unaffected by this commit and is still open.
