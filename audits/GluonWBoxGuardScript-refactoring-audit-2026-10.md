# Audit Report: Refactoring of `GluonWBoxGuardScript.es` (October 2026)

**Date:** 2026-10-10
**Audited file:** [`modules/gluonw-base/src/main/resources/ErgoContracts/GluonW/Ergs/BoxGuardScripts/GluonWBoxGuardScript.es`](https://github.com/StabilityNexus/Gluon-Ergo-Contracts/blob/e50f98c22184eb9662899da30f8e41d33549a183/modules/gluonw-base/src/main/resources/ErgoContracts/GluonW/Ergs/BoxGuardScripts/GluonWBoxGuardScript.es)
**Commit range:** `8216578` (baseline, last commit before 2026-10-01) → `e50f98c` (HEAD of `main`)

---

## Preamble

### Scope

This audit covers every commit made since 2026-10-01 that modifies the Gluon reactor box guard script `GluonWBoxGuardScript.es`. That is 57 commits, including one merge commit, from `6e9704b` (2026-10-02) to `e50f98c` (2026-10-10). Most of these commits were meant as behaviour-preserving refactorings: renames, moving declarations, removing duplication, reformatting and rewording comments. A few were meant to fix bugs.

For each commit the audit checks:

1. whether the contract's logic (the set of transactions the script accepts) was preserved;
2. whether the commit introduced new bugs, in particular syntax errors such as missing commas or stray punctuation, and undefined variables left behind by incomplete renamings or by moving declarations out of scope;
3. whether the commit fixed bugs, either bugs introduced by earlier commits in the range or bugs already present in the baseline.

**Out of scope:** the economic design of the protocol, other contracts in the repository, the off-chain Scala code (consulted only to cross-check a constant), and a full security audit of the baseline contract. Pre-existing issues in the baseline are reported only if a commit in the range fixed them or if they bear directly on a finding. A few that came up incidentally are listed in the appendix.

### Methodology

- A snapshot of the file was extracted at the baseline and after each commit (58 versions). Each commit's diff was reviewed manually in chronological order.
- Ad-hoc static checks were run on every snapshot to catch the mechanical bug classes that the request highlighted:
  - a **scope-aware identifier resolver**, which flags any identifier that is used where no declaration is in scope (undefined, used before declaration, or declared only in a sibling or inner block);
  - a **missing-comma heuristic** for elements of parenthesised argument lists (`Coll(...)`, `allOf(...)`) that span several lines;
  - detection of **stray trailing `.`** and of trailing commas before a closing parenthesis;
  - bracket balance.
- Large mechanical commits (bulk renames) were also checked with word-level diffs, to confirm they did nothing beyond the renaming.
- For commits labelled "comments only", the code with comments stripped was compared before and after the commit to confirm no code changed.
- Commits whose message says "comments" but which also change code were reviewed as code changes.
- The merge commit was verified by re-applying the side-branch patch to the first parent and comparing the result with the merge output (identical).

### Limitations

- **No ErgoScript compiler was run.** The repository's sbt toolchain is not installed in the audit environment, so nothing was compiled or tested. Every "does not compile" finding comes from static analysis. Where a finding depends on details of ErgoScript semantics (for example, whether the parser accepts trailing commas, or how `Coll.slice` treats out-of-range bounds), this is stated explicitly.
- Statements that a refactoring "preserves logic" mean that the set of accepted transactions is unchanged, assuming each version compiles. Evaluation cost (JIT costing) was not analysed.

### AI model and effort level

This audit was carried out by **Claude Opus 5.5** (model ID `claude-opus-5-5`) running in **Claude Code** (VS Code extension), at **effort level: high**. Findings have not yet been reviewed by a human auditor.

### Identifier conventions

| Prefix | Meaning |
|---|---|
| `BUG-nn` | Code bug **introduced** by a commit in the audited range (numbered in order of introduction). |
| `PRE-nn` | Bug **already present in the baseline** (`8216578`) and fixed by a commit in the range. |
| `DOC-nn` | A comment that is wrong or misleading, introduced in the range (no effect on execution). |

Severity: **Critical** = funds or protocol operation affected; **High** = significant behavioural deviation; **Compile** = syntax or name-resolution error (the contract cannot be compiled or deployed, but it cannot be exploited either); **Low** = robustness or consistency; **Doc** = comment only.

---

## Summary of findings

| ID | Severity | Introduced in | Fixed in | Short description |
|---|---|---|---|---|
| BUG-01 | Compile | `6e9704b` | `fd4aa70` | Missing comma in the Update Treasury `Coll(...)` |
| BUG-02 | Compile | `5c4169d` | `a17d1fa` | Stray `.` after `IN_GLUONW_PROTONS_TOKEN._2` |
| BUG-03 | High | `5c4169d` | **Not fixed** | BetaDecay− fee slope `Phi1` silently changed from 0.5 to 1 |
| BUG-04 | Compile | `763b192` | `b8fd21a` | `cSameContract` used before its declaration |
| BUG-05 | **Critical** | `2f43edd` | **Not fixed** | Fission, fusion and beta decays require all of R6 to be unchanged, which contradicts dev-fee accounting |
| BUG-06 | Compile | `c1e6166` | `15d5b60` (merged in `e50f98c`) | Missing comma after `newMultisig != TREASURY_MULTISIG` |
| BUG-07 | Compile | `c1e6166` | `84986a6` | Undefined `validUpdateTreasuryMultisigTx` (incomplete rename) |
| BUG-08 | Compile | `d2d9b4a` | `3e9a126` | Three missing commas after the inlined persisted-value checks |
| BUG-09 | Compile | `7efb6e8` | `1b5bb1b` | `oracleFeesToBePaid` used out of scope |
| BUG-10 | Compile | `e11d039` | `66d07f3` | Dev-fee register variables moved into an inner block but still used outside it |
| BUG-11 | Compile | `b76aa0a` | `66d07f3` | `oracleFee` and `feeDenom` used outside the block that declares them |
| BUG-12 | High | `b76aa0a` | `66d07f3` | Fission and fusion treated as owing an oracle fee, shifting the expected output indices |
| BUG-13 | Compile | `66d07f3` | **Not fixed** | Stray `.` in `val uiFee: BigInt = 4L.toBigInt.` |
| BUG-14 | Compile | `66d07f3` | **Not fixed** | Undefined `uiFeesPayout` (typo for `uiFeePayout`) |
| BUG-15 | Compile | `66d07f3` | **Not fixed** | Leftover reference to the removed `fees` collection |
| BUG-16 | Compile | `4af0bae` | **Not fixed** | `NEUTRONS_TOTAL_SUPPLY` and `PROTONS_TOTAL_SUPPLY` no longer declared |
| BUG-17 | Compile | `6f859e3` | **Not fixed** | `__outVolumeMinusValidated` undefined in BetaDecay+ (partial revert of renaming) |
| PRE-01 | High | baseline | `6e9704b` (complete in `fd4aa70`) | Update Treasury did not require the script to be preserved (mutable contract) |
| PRE-02 | Compile (probable) | baseline | `75e8700` | Trailing commas inside `Coll(...)` literals |
| PRE-03 | High | baseline | `71d1956` | UI-fee check in beta decays used the oracle-fee tuple |
| PRE-04 | High | baseline | `66d07f3` | Fission/fusion with a UI fee expected the dev-fee output at the wrong index |
| PRE-05 | High | baseline | `66d07f3` | UI-fee output index assumed a dev-fee output was always present |
| PRE-06 | Compile (probable) | baseline | `66d07f3` | `emptyFees` declared as `(Coll[Byte], Long)` but given a `BigInt` |
| PRE-07 | Low | baseline | `a2e2081` | BetaDecay+ `nDays` was not capped at `buckets` (inconsistent with BetaDecay−) |
| DOC-01 | Doc | `5c4169d` | `15d5b60` | Fission comment says "Protons increase" |
| DOC-02 | Doc | `5c4169d` | `15d5b60` | `isFusionTx` headed "# Fission" |
| DOC-03 | Doc | `5c4169d` | **Not fixed** | `isBetaDecayMinusTx` headed "BetaDecayPlus" |
| DOC-04 | Doc | `1463d52` | **Not fixed** | Fusion equation comment uses `PhiFission` instead of `PhiFusion` |
| DOC-05 | Doc | `abfb531` | **Not fixed** | "Oracle delay is at most 35 blocks", but the check is `< 35` |
| DOC-06 | Doc | `15d5b60` | **Not fixed** | Typo "circuluting" |

**Overall:** most of the refactoring preserved logic, and several real baseline bugs in fee handling were found and fixed (PRE-03, PRE-04, PRE-05). However, **the contract at HEAD (`e50f98c`) does not compile**: BUG-13 to BUG-17 are unresolved. Even once those are fixed, it contains one **critical logic bug (BUG-05)** that would block every fission, fusion and beta-decay transaction while dev fees are still owed. It also contains one **unannounced change to a fee parameter (BUG-03)**.

---

## Per-commit analysis

### 1. `6e9704b` — Make contract immutable (2026-10-02)
Adds `IN_GLUONW_BOX.propositionBytes == OUT_GLUONW_BOX.propositionBytes` to the Update Treasury self-recreation check. This is an intentional logic change.

#### Bugs introduced
- **BUG-01 (Compile).** The new line was appended after `(IN_GLUONW_BOX.R9[(Long, Long)].get == OUT_GLUONW_BOX.R9[(Long, Long)].get)` without adding a comma to that preceding line. The `Coll(...)` literal no longer parses.

#### Bugs fixed
- **PRE-01 (High).** Before this commit, the treasury multisig could spend the reactor box into an output with an arbitrary script, so the contract was mutable by the treasury. The intended fix only takes effect once BUG-01 is fixed in `fd4aa70`.

### 2. `5c4169d` — Cosmetic changes (2026-10-02)
This commit reformats comments, rewrites the header, adds TODOs, joins short `Coll` argument lists onto single lines, and inlines `_outsideRange` as `!isHealthyRange` and `oneMinusFusionRatio` inside `getProtonVolume`. Apart from the items listed below, logic is preserved.

#### Bugs introduced
- **BUG-02 (Compile).** `val _protonsInCirculation: Long = PROTONS_TOTAL_SUPPLY - IN_GLUONW_PROTONS_TOKEN._2.   // ...` has a stray trailing `.`. The parser then tries to read the next line's `val` as a member name, which is a syntax error.
- **BUG-03 (High, behavioural change in a "cosmetic" commit).** In the BetaDecay− branch, `val Phi1 = precision / 2` was changed to `val Phi1 = precision`, and the comment was changed from "Phi1 is 0.5" to "Phi1 is 1". This doubles the volume-dependent part of the BetaDecay− fee `VarPhiBeta = Phi0 + Phi1·volume/R`. The off-chain implementation `GluonWAlgorithm.varPhiBeta` (`modules/gluonw-base/src/main/scala/gluonw/common/GluonWAlgorithm.scala`, line 99) still uses `phi1 = 0.5` for **both** beta decays. Off-chain–built BetaDecay− transactions will therefore be rejected whenever the net volume is non-zero. (BetaDecay+ already used `Phi1 = 1` in the baseline, so it already disagreed with the off-chain code.) The intended value must be confirmed against the specification, and the on-chain and off-chain code aligned. Commit `1463d52` later turned the value into a single shared parameter, so the change is now permanent.
- **DOC-01.** The comment on the fission token check reads "Check Protons increase". Protons in the box *decrease* in a fission.
- **DOC-02.** `isFusionTx` is introduced with the heading "# Fission: Merges protons and neutrons…".
- **DOC-03.** `isBetaDecayMinusTx` is introduced with the heading "# BetaDecayPlus: Transmutes Neutrons to Protons".

#### Bugs fixed
None. BUG-01 is still present.

### 3. `fd4aa70` — Add missing comma (2026-10-02)
#### Bugs introduced
None.
#### Bugs fixed
- **BUG-01.** The comma is added. PRE-01's fix now takes effect.

### 4. `29080d8` — Rename precision to one (2026-10-02)
Renames `__precision` to `__one` and `precision` to `one`. The scope check confirms that every occurrence was renamed. Logic is preserved.
#### Bugs introduced
None.
#### Bugs fixed
None.

### 5. `1e7b8fa` — Rename alpha to pegFactor (2026-10-02)
Renames `inAlpha`/`outAlpha` to `inPegFactor`/`outPegFactor` and the `_alpha*` locals to `_pegFactor*`. The rename is complete. Logic is preserved.
#### Bugs introduced
None.
#### Bugs fixed
None.

### 6. `2e634be` — Rename functions (2026-10-02)
Renames `getProtonVolume`/`getNeutronVolume` to `valueOfProtons`/`valueOfNeutrons`, along with their parameters and call-site locals. The rename is complete. Logic is preserved.
#### Bugs introduced
None.
#### Bugs fixed
None.

### 7. `6a7d89f` — Remove unnecessary TODO. (2026-10-02)
Comment-only change; no code changed (verified).
#### Bugs introduced
None.
#### Bugs fixed
None.

### 8. `e9e9588` — Remove code duplication of Pt, normalizedPt and one (2026-10-02)
Promotes the top-level `__one`, `__Pt` and `__normalizedPt` to `one`, `Pt` and `normalizedPt`, and deletes the duplicate inner declarations. The inner `Pt` read `CONTEXT.dataInputs(0).R4`, which is the same box as `ORACLE_BOX`, so the definitions are equivalent. No leftover references to the old names remain. Logic is preserved.
#### Bugs introduced
None.
#### Bugs fixed
None.

### 9. `95773e2` — Improve comments (No code changes) (2026-10-02)
Comment-only change; no code changed (verified).
#### Bugs introduced
None.
#### Bugs fixed
None.

### 10. `59b828c` — Move most variable declarations to a common location (2026-10-03)
Moves `_neutronsInCirculation`, `_protonsInCirculation`, `SNeutrons`, `SProtons`, `RErg`, `qStar`, `q` and `fusionRatio` from the fission/fusion/beta-decay block to the top level. The moved line still carries BUG-02. The moved expressions are now evaluated for all transaction types. No new failure modes were found: the only divisions are by `RErg`, which the top-level `__q` already divided by, and by `q + one − qStar`, which is always positive. Logic is preserved.
#### Bugs introduced
None.
#### Bugs fixed
None.

### 11. `a17d1fa` — Remove duplicated variables (2026-10-03)
Merges the duplicate variables `__SNeutrons`/`SNeutrons`, `__RErg`/`RErg` and `__q`/`q`, whose definitions are identical. Renames `__qStarUpper`/`__qStarLower` to `qUpperThreshold`/`qLowerThreshold` and updates all uses, including in Adjust Peg. Logic is preserved.
#### Bugs introduced
None.
#### Bugs fixed
- **BUG-02.** The line containing the stray `.` was rewritten as `val SProtons: BigInt = (PROTONS_TOTAL_SUPPLY - IN_GLUONW_PROTONS_TOKEN._2).toBigInt`.

### 12. `bb80005` — Organize variable definitions (No logic changes. Just rearrangements.) (2026-10-03)
Only reorders the top-level declarations. Every declaration still comes before its first use. Logic is preserved.
#### Bugs introduced
None.
#### Bugs fixed
None.

### 13. `72168bb` — Move validity checks out of transaction definitions… (2026-10-03)
Removes `__gluonWBoxPersistedValueCheck` and `isHealthyRange` from the four `is…Tx` classifiers and adds them to each branch's final `allOf`. The persisted check is now listed twice in each branch; that duplication is harmless and is removed in `06546e5`. **The set of accepted transactions is unchanged.** The four token/value patterns are mutually exclusive, and any transaction that matches one of them violates `_tokensSame`/`_valueSame`. Such a transaction therefore could never have passed the Adjust Peg or Update Treasury branches, which it previously fell through to when the healthy-range or persisted checks failed. It is still rejected, now inside the matching branch.
#### Bugs introduced
None.
#### Bugs fixed
None.

### 14. `1463d52` — Move fee parameters to the parameters section (2026-10-03)
Introduces the top-level parameters `PhiFission`, `PhiFusion`, `Phi0` and `Phi1`, and deletes the per-branch definitions. The values match the previous ones, including the BetaDecay− `Phi1 = one` introduced by BUG-03, which is now shared by both beta decays. Logic is preserved relative to the parent commit.
#### Bugs introduced
- **DOC-04.** The fusion equation comment now reads `… ==> M (1 - PhiFission) [Ergs]`. Fusion uses `PhiFusion`.
#### Bugs fixed
None.

### 15. `3b01478` — Remove some tautological validity checks from fission and fusion (2026-10-03)
Removes `ErgsActualValue == ErgsExpectedValue` from fission and fusion. Both sides were defined by the same expression as `M`, so the check was always true. Logic is preserved.
#### Bugs introduced
None.
#### Bugs fixed
None.

### 16. `2b4689d` — Move preservation checks to their own section… (2026-10-03)
Moves `_tokensSame`, `_valueSame`, `_r4Same` … `_r9Block1Same` from the Adjust Peg branch to the top level. Logic is preserved.
#### Bugs introduced
None.
#### Bugs fixed
None.

### 17. `763b192` — Rename preservation checks (2026-10-03)
Renames `_xSame` to `cSameX`, adds `cSameContract`, and uses it in `__gluonWBoxPersistedValueCheck`, Adjust Peg and Update Treasury.
#### Bugs introduced
- **BUG-04 (Compile).** `__gluonWBoxPersistedValueCheck`, at about line 141, now references `cSameContract`, which is declared about 50 lines later (line 192). ErgoScript block-local `val`s cannot be referenced before their declaration.
#### Bugs fixed
None.

### 18. `b8fd21a` — Move a preservation check to its correct location (2026-10-03)
#### Bugs introduced
None.
#### Bugs fixed
- **BUG-04.** `__gluonWBoxPersistedValueCheck` is moved below the preservation checks.

### 19. `2f43edd` — Reuse checks (2026-10-03)
Introduces `cSameTokenIdentifiers` and `cSameR9PegFactor`, and rewrites `__gluonWBoxPersistedValueCheck` as `allOf(Coll(cSameTokenIdentifiers, cSameContract, cSameR4, cSameR5, cSameR6, cSameR9PegFactor))`.
#### Bugs introduced
- **BUG-05 (Critical).** The original persisted-value check required only `IN.R6._2 == OUT.R6._2` (the maximum dev-fee threshold). `cSameR6` requires **all of R6** to be unchanged, including `R6._1`, the total dev fees repaid. However, `devFeeRepaidValueAdded` (later part of `cFees`) requires `OUT.R6._1 − IN.R6._1 == devFeePayout`. The two conditions can both hold only when `devFeePayout == 0`. Consequently, as long as `DEV_FEE_REPAID < MAX_DEV_FEE_THRESHOLD`, **every fission, fusion, BetaDecay+ and BetaDecay− transaction is rejected**, except dust-sized ones whose dev fee rounds to zero. Because R6._1 can then never grow, the condition is permanent: the reactor is effectively frozen for its four main operations. Commits `d2d9b4a`, `3e9a126` and `a03753b` carried the check forward when inlining it, and it is still present at HEAD (lines 171, 278, 306, 436 and 547). *Suggested fix:* remove `cSameR6` from the four branches. R6 is already fully constrained there by `devFeeRepaidValueAdded` and `maxDevFeeThresholdSame` inside `cFees`.
#### Bugs fixed
None.

### 20. `a659654` — Reuse defined checks (2026-10-03)
Replaces the explicit register comparisons in Update Treasury with `cSameValue`, `cSameTokens`, `cSameR4`, `cSameR6`, `cSameR7` and `cSameR8`. These are equivalent. Logic is preserved.
#### Bugs introduced
None.
#### Bugs fixed
None.

### 21. `81a6180` — Define and use cSameR9 (2026-10-03)
Equivalent replacement. Logic is preserved.
#### Bugs introduced
None.
#### Bugs fixed
None.

### 22. `c1e6166` — Simplify update treasury transaction validity checks (2026-10-03)
Flattens the nested `validUpdateTreasuryMultisigTx` and `validSelfRecreation` blocks into a single `allOf`. The intended logic is the same.
#### Bugs introduced
- **BUG-06 (Compile).** `newMultisig != TREASURY_MULTISIG // R5 is the only register that changes` has no trailing comma before the next element, `cSameR6, …`.
- **BUG-07 (Compile).** The value is declared as `validUpdateTreasuryTx`, but the result line still reads `sigmaProp(validUpdateTreasuryMultisigTx) && TREASURY_MULTISIG`. The rename is incomplete and leaves an undefined identifier.
#### Bugs fixed
None.

### 23. `84986a6` — Simplify update treasury transaction (2026-10-03)
Inlines the `allOf` directly into `sigmaProp(...) && TREASURY_MULTISIG`.
#### Bugs introduced
None. BUG-06 is carried over unchanged.
#### Bugs fixed
- **BUG-07.** The undefined identifier no longer appears.

### 24. `9694f24` — Simplify adjust peg transaction (2026-10-03)
Inlines `_pegFactorDecreased` and `_pegFactorIncreased` into `pegFactorCorrect` and reorders the checks. Logic is preserved.
#### Bugs introduced
None.
#### Bugs fixed
None.

### 25. `ab8ea28` — Rename GluonW to Gluon everywhere (2026-10-03)
A word-level diff confirms this is a pure rename (`IN_GLUONW_*` to `IN_GLUON_*`, `OUT_GLUONW_*` to `OUT_GLUON_*`, and `GluonWBox` to `GluonBox` in comments), plus the removal of a stale comment. The rename is complete. Logic is preserved.
#### Bugs introduced
None.
#### Bugs fixed
None.

### 26. `f4f09a8` — Add comments (2026-10-03)
Comment-only change; no code changed (verified).
#### Bugs introduced
None.
#### Bugs fixed
None.

### 27. `06546e5` — Remove duplicated checks (2026-10-03)
Removes the duplicate `__gluonWBoxPersistedValueCheck` introduced in `72168bb`. Logic is preserved.
#### Bugs introduced
None.
#### Bugs fixed
None.

### 28. `d2d9b4a` — Replace a big check by its definition… (2026-10-03)
Inlines `__gluonWBoxPersistedValueCheck` into the four branches. This carries BUG-05 into each branch.
#### Bugs introduced
- **BUG-08 (Compile).** In the fusion, BetaDecay+ and BetaDecay− branches, the inlined line `cSameTokenIdentifiers, cSameContract, cSameR4, cSameR5, cSameR6, cSameR9PegFactor` has no trailing comma before the next element, `isHealthyRange,`. That makes three syntax errors. The fission branch is correct.
#### Bugs fixed
None.

### 29. `3e9a126` — Reorder checks for the sake of uniformity (2026-10-03)
Reorders the arguments of the four `allOf` lists. The set of conjuncts is unchanged. Logic is preserved.
#### Bugs introduced
None.
#### Bugs fixed
- **BUG-08.** As a side effect of the reordering, every list element is now correctly comma-separated.

### 30. `6280ed9` — Remove TODOs (2026-10-03)
Comment-only change; no code changed (verified).
#### Bugs introduced
None.
#### Bugs fixed
None.

### 31. `abfb531` — Simplify oracle check (2026-10-03)
Replaces `__oracleCheck` with `cOracle`: `oracleDelay < 35 && oracleDelay >= 0` together with `ORACLE_BOX.tokens(0)._1 == _OraclePoolNFT`. This is equivalent to the previous check, and all uses were renamed. Logic is preserved.
#### Bugs introduced
- **DOC-05.** The comment says "Oracle delay is at most 35 blocks", but `oracleDelay < 35` allows at most 34 blocks. (The baseline comment said "within … 35 blocks".)
#### Bugs fixed
None.

### 32. `fb23190` — Move auxiliary functions (2026-10-03)
Moves `valueOfProtons`, `valueOfNeutrons` and `sum` to the top level. These are functions and are only evaluated when called. Logic is preserved.
#### Bugs introduced
None.
#### Bugs fixed
None.

### 33. `0c81fbc` — Simplify aux functions (2026-10-03)
Inlines result variables. Logic is preserved.
#### Bugs introduced
None.
#### Bugs fixed
None.

### 34. `75e8700` — Eliminate unnecessary line breaks… (2026-10-03)
Compacts the `fees` construction and the `…FeesPaid` blocks. The same values are produced. Logic is preserved.
#### Bugs introduced
None.
#### Bugs fixed
- **PRE-02 (probable compile error).** The baseline `fees` construction contained `Coll(..., (_optUIFeeAddress.get.propBytes, uiFeePayout),)` and two similar trailing commas. ErgoScript's parser derives from an older Scala grammar and is not believed to accept trailing commas. This is unverified, because no compiler was available. The rewrite removes them.

### 35. `4c91b99` — Eliminate unnecessary line breaks (2026-10-03)
Rewrites `principal` as a chain of single-line `if/else` expressions. The branches are equivalent. Logic is preserved.
#### Bugs introduced
None.
#### Bugs fixed
None.

### 36. `d969922` — Simplify comments (2026-10-03)
Comment-only change; no code changed (verified). DOC-01 is still present: the comment now reads "Protons increase".
#### Bugs introduced
None.
#### Bugs fixed
None.

### 37. `7efb6e8` — Unnest oracleFeesPaid (2026-10-03)
Moves `oracleFeesPaid` out of the `feesPaid` block and adds it to `__feesCheck`. The resulting conjunction is the same.
#### Bugs introduced
- **BUG-09 (Compile).** The moved `oracleFeesPaid` references `oracleFeesToBePaid`, which is still declared *inside* the `feesPaid` block. It is out of scope.
#### Bugs fixed
None.

### 38. `1b5bb1b` — Unnest devFeesPaid and uiFeesPaid (2026-10-03)
Moves `oracleFeesToBePaid`, `devFeesPaid` and `uiFeesPaid` to the enclosing scope and lists all three in `__feesCheck`. Logic is preserved.
#### Bugs introduced
None.
#### Bugs fixed
- **BUG-09.** `oracleFeesToBePaid` is now declared before, and in scope of, `oracleFeesPaid`.

### 39. `25c6c07` — Remove unused variable declaration (2026-10-03)
Removes the unused `uiFeesToBePaid`. Logic is preserved.
#### Bugs introduced
None.
#### Bugs fixed
None.

### 40. `71d1956` — Fix a bug related to UI Fees (2026-10-03)
This is an intentional logic change.
#### Bugs introduced
None.
#### Bugs fixed
- **PRE-03 (High).** In the baseline, `uiFeesPaid` always checked `fees(1)`. For beta decays, `fees(1)` is the **oracle** fee tuple, so the UI-fee output had to pay the oracle address (`_OracleFeePk`) the oracle-fee amount. Beta-decay transactions with a UI fee were therefore unusable as intended. The fix selects `fees(2)` for beta decays and `fees(1)` for fission and fusion.

### 41. `3d24269` — Improve style (2026-10-03)
Whitespace and comment changes only; no code changed (verified).
#### Bugs introduced
None.
#### Bugs fixed
None.

### 42. `a03753b` — Eliminate duplicate code (2026-10-03)
Replaces `__validVolumeHandling` together with `cSameR9PegFactor` by `cSameR7, cSameR8, cSameR9` in fission and fusion. This is equivalent: `cSameR9` is the conjunction of last-bucket-block preservation and peg-factor preservation. Logic is preserved. BUG-05 is still present.
#### Bugs introduced
None.
#### Bugs fixed
None.

### 43. `e11d039` — Move aliases related to dev fee close to where they are used. (2026-10-03)
Moves `ASSET_MAX_DEV_FEE_THRESHOLD`, `DEV_FEE_REPAID`, `MAX_DEV_FEE_THRESHOLD`, `OUT_DEV_FEE_REPAID` and `OUT_MAX_DEV_FEE_THRESHOLD` into the `fees: Coll[...] = { ... }` block.
#### Bugs introduced
- **BUG-10 (Compile).** `devFeeRepaidValueAdded` and `maxDevFeeThresholdSame` are declared *outside* the `fees` block but reference `DEV_FEE_REPAID`, `OUT_DEV_FEE_REPAID`, `MAX_DEV_FEE_THRESHOLD` and `OUT_MAX_DEV_FEE_THRESHOLD`, which are now local to that block. All four are out of scope.
#### Bugs fixed
None.

### 44. `f0202fb` — Rename variables (2026-10-03)
Renames:
- `SNeutrons`, `SProtons`, `RErg` to `supplyNeutrons`, `supplyProtons`, `reserve`
- `Pt`, `normalizedPt`, `fusionRatio` to `price`, `priceAdjusted`, `qNorm`
- `Phi*` to `phi*`
- `BLOCKS_PER_VOLUME_BUCKET`, `BUCKETS` to `blocksPerVolumeBucket`, `buckets`

The renames are complete (no new unresolved identifiers). Logic is preserved. BUG-10 is still present.
#### Bugs introduced
None.
#### Bugs fixed
None.

### 45. `c6d24ed` — Rename variable (2026-10-03)
Renames `isHealthyRange` to `isHealthy`. The rename is complete. Logic is preserved.
#### Bugs introduced
None.
#### Bugs fixed
None.

### 46. `b76aa0a` — Make oracleFeesPaid independent of the fees Coll (2026-10-03)
Hoists `principal`, `oracleFeePayout` and `oracleFeeAddressAndPayout` out of the `fees` block. Defines `oracleFeesToBePaid = oracleFeePayout > 0` and makes `oracleFeesPaid` use `_OracleFeePk` and `oracleFeePayout` directly.
#### Bugs introduced
- **BUG-11 (Compile).** The hoisted `oracleFeePayout = (oracleFee * principal) / feeDenom` references `oracleFee` and `feeDenom`, which are still declared inside the `fees` block, later and in an inner scope.
- **BUG-12 (High).** `oracleFeesToBePaid` no longer depends on the transaction type. Previously it was `fees(1)._2 > 0`, which is the oracle fee for beta decays. It is now `oracleFeePayout > 0`, which is true for **fission and fusion** too whenever `principal ≥ 1000` nanoERG. `devFeesPaid` and `uiFeesPaid` use this flag to pick the output index. As a result, fission and fusion transactions would expect the dev fee at `OUTPUTS(3)` and the UI fee at `OUTPUTS(4)`, even though there is no oracle output, and `OUTPUTS(2)` would go unchecked. This is masked by BUG-10 and BUG-11 (the script does not compile), and is fixed in the next commit.
#### Bugs fixed
None.

### 47. `66d07f3` — Simplify code by eliminating fees Coll. Also fix a bug related to OUTPUTS position calculation. (2026-10-03)
Removes the `fees` collection entirely. The fee parameters and the dev-fee register aliases move to the enclosing scope. `oracleFeesToBePaid` becomes `(isBetaDecayPlusTx || isBetaDecayMinusTx) && oracleFeePayout > 0`. A `devFeesToBePaid` flag is introduced, and the UI output index is computed from both flags.
#### Bugs introduced
- **BUG-13 (Compile).** `val uiFee: BigInt = 4L.toBigInt.    // Optional UI fee: 0.4%` has a stray trailing `.`, the same kind of mistake as BUG-02. The parser then tries to read the next line's `val` as a member name. Still present at HEAD, line 104.
- **BUG-14 (Compile).** The new condition `if (_optUIFeeAddress.isDefined && uiFeesPayout > 0)` uses `uiFeesPayout`, which is never declared. The declared value is `uiFeePayout`. Still present at HEAD, line 241.
- **BUG-15 (Compile).** The leftover line `val uiFees: (Coll[Byte], BigInt) = if (isBetaDecayPlusTx || isBetaDecayMinusTx) fees(2) else fees(1)` still references `fees`, which this commit deleted. `uiFees` is also no longer used. Still present at HEAD, line 242.
#### Bugs fixed
- **BUG-10.** The dev-fee aliases are now in the enclosing `if` block, in scope of `devFeeRepaidValueAdded`.
- **BUG-11.** `feeDenom`, `devFee`, `oracleFee` and `uiFee` are now declared before `oracleFeePayout` in the same scope.
- **BUG-12.** `oracleFeesToBePaid` is again false for fission and fusion.
- **PRE-04 (High).** In the baseline, `oracleFeesToBePaid = fees(1)._2 > 0`. For fission and fusion, `fees(1)` is the **UI** fee tuple. Whenever a fission or fusion paid a UI fee, the dev-fee output was therefore expected at `OUTPUTS(3)` and the UI output at `OUTPUTS(4)`, leaving `OUTPUTS(2)` unconstrained.
- **PRE-05 (High).** In the baseline, the UI output index was `if (!oracleFeesToBePaid) OUTPUTS(3) else OUTPUTS(4)`, which assumes a dev-fee output always exists. Once dev fees are fully repaid, or when the dev fee rounds to 0, the UI output would have been looked up one position too far. The new three-way index computation is correct.
- **PRE-06 (probable compile error).** The baseline declared `val emptyFees: (Coll[Byte], Long) = (Coll(1.toByte), 0L.toBigInt)`: the declared type is `Long` but the value is a `BigInt`. It was then placed in a `Coll[(Coll[Byte], BigInt)]`. This is a type error and is eliminated together with the collection.

### 48. `4af0bae` — Rename Register definitions (2026-10-03)
Renames the box and register aliases:
- `IN_GLUON_BOX` to `GLUON_BOX`
- `IN_GLUON_*_TOKEN` to `*_TOKEN`
- `inVolumePlus`/`outVolumePlus` to `VOLUME_PLUS`/`OUT_VOLUME_PLUS`, and likewise for the minus volumes
- `inR9` to `REGISTER_9`
- `inLastBucketBlock` to `LAST_BUCKET_BLOCK`
- `inPegFactor` to `PEG_FACTOR`

It also moves the dev-fee aliases to the top level.

*Observation:* the rename was applied to substrings of longer identifiers too. For example, `outVolumePlusExpectedValue` became `OUT_VOLUME_PLUSExpectedValue`, and `__outVolumeMinusValidated` became `__OUT_VOLUME_MINUSValidated`. Because declarations and uses were renamed consistently, this did not break anything at this commit. It is reverted, partly, in `6f859e3`.
#### Bugs introduced
- **BUG-16 (Compile).** The declarations `ASSET_TOTAL_SUPPLY_REGISTER`, `NEUTRONS_TOTAL_SUPPLY` and `PROTONS_TOTAL_SUPPLY` were deleted. However, `supplyNeutrons` and `supplyProtons` still use `NEUTRONS_TOTAL_SUPPLY` and `PROTONS_TOTAL_SUPPLY`, which are now undefined. Still present at HEAD, lines 107–108. *Suggested fix:* define `val NEUTRONS_TOTAL_SUPPLY: Long = GLUON_BOX.R4[(Long, Long)].get._1` and `val PROTONS_TOTAL_SUPPLY: Long = GLUON_BOX.R4[(Long, Long)].get._2`, or inline them.
#### Bugs fixed
None.

### 49. `81f6de3` — Improve comments (2026-10-03)
**Despite its message, this commit also changes code:**
- the fee parameters `feeDenom`, `initialDevFee` (renamed from `devFee`), `oracleFee` and `uiFee` move to the top-level parameter section, carrying BUG-13 with them;
- the dev-fee formula is inlined as `((initialDevFee * principal) / feeDenom) * (MAX − REPAID) / MAX`, which evaluates in the same order as before;
- `sum` becomes a single expression;
- the unused `oracleFeeAddressAndPayout` is removed;
- `devFeeRepaidValueAdded` and `maxDevFeeThresholdSame` are moved;
- `__feesCheck` is renamed to `cFees`, and every use is renamed.

Logic is preserved.
#### Bugs introduced
None.
#### Bugs fixed
None.

### 50. `6f859e3` — Fix spurious renamings (2026-10-03)
Reverts most of the substring renames from `4af0bae`.
#### Bugs introduced
- **BUG-17 (Compile).** In the BetaDecay+ branch, the declaration `val __OUT_VOLUME_MINUSValidated: Boolean = …` was **not** reverted, but its use in the final `allOf` was changed to `__outVolumeMinusValidated`. That name is declared only in the BetaDecay− branch, which is a sibling scope, so it is undefined in BetaDecay+. Still present at HEAD (declaration at line 391, use at line 437). This is the "incomplete renaming" pattern.
#### Bugs fixed
None. The substring renames from `4af0bae` were cosmetic and had not broken the code.

### 51. `1aca03e` — Improve comments (2026-10-03)
Comment-only change; no code changed (verified).
#### Bugs introduced
None.
#### Bugs fixed
None.

### 52. `a2a3d10` — Remove intermediary variables that were only being used once (2026-10-03)
Inlines `oneMinusPhiBeta` and `oneMinusFusionRatio` in both beta decays. The expressions are identical. Logic is preserved.
#### Bugs introduced
None.
#### Bugs fixed
None.

### 53. `9b6c23e` — Reorder some lines to bring variable definitions closer to where they are used (2026-10-03)
Moves `currentBlockNumber` and the `…ActualValue` declarations. Every declaration still comes before its first use. Logic is preserved.
#### Bugs introduced
None.
#### Bugs fixed
None.

### 54. `53ceba6` — Add a TODO about a possible bug (2026-10-03)
Comment-only change; no code changed (verified). The TODO correctly flags PRE-07.
#### Bugs introduced
None.
#### Bugs fixed
None.

### 55. `15d5b60` — fix: comments + missing comma fix (2026-10-07, side branch based on `53ceba6`)
Fixes comments, adds the missing comma, and drops a redundant `.toBigInt` on a `BigInt` in `valueOfProtons`, which is equivalent.
#### Bugs introduced
- **DOC-06.** Typo "circuluting" in the fission comment (line 128).
#### Bugs fixed
- **BUG-06.** Comma added after `newMultisig != TREASURY_MULTISIG`.
- **DOC-01.** The fission comment now says "Protons decrease".
- **DOC-02.** The fusion heading now says "# Fusion".

### 56. `a2e2081` — Fix bug related to overflow of nDays (2026-10-10, `main`, based on `53ceba6`)
Caps BetaDecay+ `nDays` at `buckets`, in the same way as BetaDecay−, and removes the TODO.
#### Bugs introduced
None.
#### Bugs fixed
- **PRE-07 (Low).** BetaDecay+ previously used the uncapped `nDays`. If ErgoScript's `Coll.slice` clamps out-of-range bounds the way Scala does, the uncapped value gives the same result. In that case this fix is mainly about consistency and robustness, and it is required if `slice` rejects a negative `until`. *Note:* despite the commit message, this change does not prevent overflow. The `Long → Int` conversion (`.toInt`) happens *before* the cap, in both branches. A conversion overflow would require `LAST_BUCKET_BLOCK` to be very far below `HEIGHT`, which is practically unreachable.

### 57. `e50f98c` — Merge commit '15d5b60…' (2026-10-10)
Merges the side branch `15d5b60` into `main`. The merge was verified by applying the `53ceba6..15d5b60` patch to `a2e2081`, which reproduces the merge result exactly. No merge errors.
#### Bugs introduced
None.
#### Bugs fixed
- **BUG-06**, **DOC-01** and **DOC-02** reach `main` through the merge.

---

## Unresolved bugs at HEAD (`e50f98c`)

The following bugs introduced in the audited range are **not fixed** at HEAD. Line numbers refer to the file at `e50f98c`.

| ID | Severity | Introduced in | Location (HEAD) | Description | Suggested fix |
|---|---|---|---|---|---|
| **BUG-05** | **Critical** | `2f43edd` — Reuse checks | lines 171, 278, 306, 436, 547 | `cSameR6` requires all of R6 to be unchanged in fission, fusion and both beta decays. This contradicts `devFeeRepaidValueAdded`, so every such transaction with a non-zero dev fee is rejected and the reactor is frozen until the dev fees are repaid, which can then never happen. | Remove `cSameR6` from these four branches. `cFees` already constrains R6 through `devFeeRepaidValueAdded` and `maxDevFeeThresholdSame`. |
| **BUG-03** | High | `5c4169d` — Cosmetic changes | lines 97–98 (`phi0`, `phi1`), used at lines 403 and 514 | BetaDecay− fee slope changed from 0.5 to 1. This disagrees with the off-chain `GluonWAlgorithm.varPhiBeta`, which uses 0.5 for both beta decays. | Confirm the intended value(s) against the specification and align the on-chain and off-chain code. |
| **BUG-13** | Compile | `66d07f3` — Simplify code by eliminating fees Coll… | line 104 | Stray `.` in `4L.toBigInt.` | Delete the trailing `.` |
| **BUG-14** | Compile | `66d07f3` | line 241 | Undefined `uiFeesPayout` | Rename to `uiFeePayout` |
| **BUG-15** | Compile | `66d07f3` | line 242 | References the deleted `fees` collection; `uiFees` is unused | Delete the line |
| **BUG-16** | Compile | `4af0bae` — Rename Register definitions | lines 107–108 | `NEUTRONS_TOTAL_SUPPLY` and `PROTONS_TOTAL_SUPPLY` are undefined | Re-declare them from `GLUON_BOX.R4[(Long, Long)]` |
| **BUG-17** | Compile | `6f859e3` — Fix spurious renamings | lines 391 and 437 | BetaDecay+ declares `__OUT_VOLUME_MINUSValidated` but uses `__outVolumeMinusValidated` | Rename the declaration at line 391 to `__outVolumeMinusValidated` |

Comment defects that are still present:

| ID | Introduced in | Location (HEAD) | Description |
|---|---|---|---|
| DOC-03 | `5c4169d` — Cosmetic changes | line 147 | `isBetaDecayMinusTx` is headed "## BetaDecayPlus" |
| DOC-04 | `1463d52` — Move fee parameters to the parameters section | line 283 | Fusion equation comment uses `phiFission`; it should be `phiFusion` |
| DOC-05 | `abfb531` — Simplify oracle check | line 181 | "at most 35 blocks", but `< 35` means at most 34 |
| DOC-06 | `15d5b60` — fix: comments + missing comma fix | line 128 | Typo "circuluting" |

**State of HEAD:** because of BUG-13 to BUG-17, the script at `e50f98c` does not compile. Fixing those five compile errors alone is **not sufficient**: BUG-05 must be fixed before deployment, and BUG-03 must be resolved one way or the other. The table below shows, from static analysis, which compile errors are open after each commit. It may help bisect test failures. It ignores the probable baseline issues PRE-02 (open until `75e8700`) and PRE-06 (open until `66d07f3`).

| After commit(s) | Open compile-level bugs |
|---|---|
| `6e9704b` | BUG-01 |
| `5c4169d` | BUG-01, BUG-02 |
| `fd4aa70` … `59b828c` | BUG-02 |
| `a17d1fa` … `2b4689d` | none |
| `763b192` | BUG-04 |
| `b8fd21a` … `81a6180` | none |
| `c1e6166` | BUG-06, BUG-07 |
| `84986a6` … `06546e5` | BUG-06 |
| `d2d9b4a` | BUG-06, BUG-08 |
| `3e9a126` … `d969922` | BUG-06 |
| `7efb6e8` | BUG-06, BUG-09 |
| `1b5bb1b` … `a03753b` | BUG-06 |
| `e11d039` … `c6d24ed` | BUG-06, BUG-10 |
| `b76aa0a` | BUG-06, BUG-10, BUG-11 |
| `66d07f3` | BUG-06, BUG-13, BUG-14, BUG-15 |
| `4af0bae`, `81f6de3` | BUG-06, BUG-13 – BUG-16 |
| `6f859e3` … `53ceba6`, `a2e2081` | BUG-06, BUG-13 – BUG-17 |
| `15d5b60`, `e50f98c` (HEAD) | BUG-13 – BUG-17 |

---

## Appendix: pre-existing issues observed but outside the audit scope

These issues were already in the baseline and are unchanged at HEAD. They are listed for completeness and were not analysed in depth.

1. **Every transaction type evaluates `INPUTS(1)` and `CONTEXT.dataInputs(0)` at the top level** (`isUpdateTreasury` and `ORACLE_BOX`, together with the oracle-derived `price`, `q`, …). In practice, every transaction therefore needs at least two inputs and at least one data input. This contradicts the header comment, which says Update Treasury has "DataInputs: None".
2. **BetaDecay+ `Phi1`** was already 1 in the baseline, while the off-chain code uses 0.5. See BUG-03.
3. **Process observations.** Two commits carried changes their messages did not describe. `5c4169d` ("Cosmetic changes") changed a fee parameter (BUG-03), and `81f6de3` ("Improve comments") moved and renamed code. Keeping behavioural changes out of commits labelled cosmetic or comment-only, and compiling and running the test suite before each commit, would have caught all of the compile-level bugs above at the commit that introduced them.
