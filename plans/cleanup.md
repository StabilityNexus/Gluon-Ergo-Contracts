# Gluon-Ergo-Contracts: cleanup plan

*Proposal for the Gluon maintainers.*
- *Written 2026-10-11 and revised the same day with the maintainers' decisions D1–D11 and notes N1–N7.*
- *Based on `main` at `00e45b6`. Nothing in the repository has been changed yet.*
- *Update 2026-10-12: the cleanup was carried out, and the maintainers then decided against any testnet deployment, so the testnet tooling this plan describes (`config/testnet/`, `scripts/testnet-oracle.ts`, `deployments/testnet.json`, the testnet rehearsal) was removed on the branch `cleanup/remove-testnet`. Where this plan mentions testnet, that part is superseded.*

## 1. Summary

**Today.** The repository is mostly three things: a Scala/Play REST backend (its README is still that of an "Ergo-Play-Boilerplate"), a Scala off-chain library, and Scala deployment utilities.

- The one file that matters, the Gluon contract, sits at `modules/gluonw-base/src/main/resources/ErgoContracts/GluonW/Ergs/BoxGuardScripts/GluonWBoxGuardScript.es`.
- Gluon-Ergo-UI and Gluon-Ergo-SDK have never called the backend, in any branch or commit (§9.5).

**Target.** A small repository with:

- the contract, renamed to `contracts/Gluon.es`;
- tests that execute the contract (`test/`);
- deployment scripts (`scripts/`);
- public configuration files (`config/`);
- deployment records, both machine-readable (`deployments/*.json`) and human-readable (`Deployments.md`);
- the existing audit reports;
- the auxiliary files of StabilityNexus/Template-Repo.

The repository will have no license for now.

**Deployments.**

- The live v2 Gold reactor stays as it is; there is no migration.
- Two new v3 reactors will be bootstrapped from scratch, one pegged to gold and one to the US dollar, each with its own oracle.
- Each bootstrap seeds a configurable amount of ERG, at most 10 ERG. It burns the genesis tokens and computes the token amounts so that, at launch:
  - the fusion ratio q is 0.6;
  - one proton is worth 1 ERG;
  - the peg factor is 0.001 for Gold and 1 for Dollar.
- The contract no longer divides the oracle price by 1000; the peg factor carries the unit instead.
  - The GoldPool reports nanoERG per kg, so Gold's 0.001 pegs one neutron to one gram.
  - The USD Pool reports nanoERG per USD, so Dollar's 1 pegs one neutron to one dollar.

**Stack** (decided): TypeScript with Fleet SDK; vitest; Node 24 LTS with npm.

- `@fleet-sdk/compiler` compiles the contract.
- `@fleet-sdk/mock-chain` executes transactions locally. It uses `sigmastate-js`, the JavaScript build of the interpreter the Ergo node runs.
- `@fleet-sdk/core` builds the scripts' transactions.

**Verified with throwaway experiments** (§9.3):

1. Fleet's compiler reproduces the live Gold reactor's ErgoTree byte for byte.
2. The v3 contract compiles with the compiler Fleet ships, but not with the newer compiler the node runs (Sigma ≥ 6.0.3). The cause is two wrong `Long` annotations; the fix leaves the compiled tree unchanged.
3. In mock-chain, a valid fission and a valid peg adjustment are accepted, and changing any amount by one unit is rejected.
4. Update Treasury fails without an oracle box as data input, although the contract header says it needs none.
5. With `/ 1000` removed, the contract still compiles with both compiler generations.
   - A Gold reactor with peg factor 0.001 prices one neutron at one gram of gold.
   - A Dollar reactor with peg factor 1 prices one neutron at one USD.
   - Each accepts a valid fission, rejects one-unit deviations, and accepts a correct −1 % Adjust Peg.

**Still TODO before the v3 deployments** (§8). These values stay marked TODO in the config files:

- the Gold oracle-fee destination, since its buyback NFT was burned;
- the treasury address;
- the token metadata;
- the seed amounts.

## 2. Decisions

### 2.1 Decided

| ID | Decision | Where it shows up |
|---|---|---|
| D1 | `StabilityNexus/Gluon-Ergo-Contracts` is canonical. The StabilityNexus fork is ignored. | Step 0; audit permalinks; README links |
| D2 | TypeScript + Fleet SDK | §3, §5, §6 |
| D3 | Node 24 LTS + npm + vitest | §3; CI |
| D4 | No license for now | PR 4 deletes the CC0 `LICENSE`; no `LICENSE.md` is added |
| D5 | Fix the contract header | PR 2 |
| D6 | No v2 → v3 migration. v2 Gold continues. Two new v3 deployments, Gold and Dollar, each with its own oracle. | §6; the migration script and tests are dropped |
| D7 | Initial peg factor 1, refined by D9: 1 for Dollar, 0.001 for Gold. The treasury address is a TODO in the config files. | §6.1, §6.3 |
| D8 | Ship `scripts/adjust-peg.ts` | §6.4 |
| D9 | **Price scale.** Remove the hard-coded division from the contract: `price = R4 / 1000` becomes `price = R4`. The contract then computes `priceAdjusted = price × pegFactor / 10⁹`. Initial peg factors:<ul><li>Dollar: 1. The USD Pool already reports nanoERG per USD.</li><li>Gold: 0.001. The GoldPool reports nanoERG per kg, and 0.001 pegs one neutron to one gram.</li></ul> | PR 2 (contract change), §6.1, §6.3; verified in §9.3 |
| D10 | The Gold oracle-fee destination stays a clear TODO in `config/gold.json`. The Gold oracle's buyback NFT `119a068a…` was burned (§9.4). | §6.1, §8 |
| D11 | The genesis tokens are burned. The bootstrap refuses seeds above 10 ERG, so that nobody seeds a large amount expecting to receive the tokens. | §6.3 |
| N1 | The contract is renamed to `Gluon.es` | PR 1 |
| N2 | `.env` holds only secrets. Public parameters come from committed config files. | §6.1, §6.2 |
| N3 | `Deployments.md` in the root, in addition to `deployments/*.json` | §6.5 |
| N4 | Bootstrap takes the seed ERG from config; the token amounts give q = 0.6 and a proton price of 1 ERG | §6.3 |
| N5 | The treasury is a single-signature address, despite the name `TREASURY_MULTISIG` | §6.1, §6.4 |
| N6 | Token name, ticker and description come from config, following Ergo conventions (EIP-4) | §6.3 |
| N7 | Config files for the planned v3 Gold and Dollar deployments | §6.1; PR 3 |

### 2.2 Still open

No decisions remain open. The values still marked TODO in the config files are listed in §8.

## 3. Target repository

```
contracts/
  Gluon.es                       the contract (git mv + rename; history preserved)
config/                          public parameters (N2)
  networks.json                  node and explorer URLs per network
  gold.json                      planned v3 Gold deployment (N7)
  dollar.json                    planned v3 Dollar deployment (N7)
  testnet/                       rehearsal configs (stand-in oracle and buyback)
lib/                             code shared by tests and scripts
  config.ts                      load and validate config; refuse TODO values a script needs
  compile.ts                     compile Gluon.es for a deployment config, with pinned options
  math.ts                        BigInt mirror of the contract formulas, incl. bootstrap amounts
  transactions.ts                builders: bootstrap, fission, fusion, beta decay ±, adjust peg, update treasury
  node.ts                        minimal client for the Ergo node REST API
test/
  helpers.ts                     mock-chain fixtures and consensus checks
  *.test.ts                      see §5
scripts/                         see §6.4
deployments/
  mainnet.json                   machine-readable records of every deployment
  testnet.json
Deployments.md                   human-readable list, generated from deployments/*.json (N3)
audits/                          existing reports + AuditReport-KB.pdf
skills/                          security-review and security-remediation (unchanged)
public/                          stability.svg (template) + gluon.svg (from Gluon-Ergo-UI)
.github/workflows/ci.yml
.coderabbit.yml  .gitignore  .env.example  .nvmrc  package.json  package-lock.json  tsconfig.json
README.md  COPYRIGHT.md  Contributors.md  DCO.md      (no license file, D4)
```

**Dependencies.** Pin exact versions (`save-exact`) and commit the lockfile, because the ErgoTree depends on the compiler version. Fleet itself pins its engine, `sigmastate-js` 0.4.6, exactly.

- Runtime:
  - `@fleet-sdk/compiler` 0.12.0
  - `@fleet-sdk/core` 0.12.0
  - `@fleet-sdk/serializer` 0.11.0
  - `@fleet-sdk/wallet` 0.12.0
- Dev:
  - `@fleet-sdk/mock-chain` 0.12.0
  - `vitest`
  - `typescript`
- Optional dev:
  - `fast-check`
  - `sigmastate-js` 0.6.3 under an npm alias, for the forward-compatibility compile check

There is no build step: Node 24 runs TypeScript directly by stripping types. Avoid syntax that cannot simply be stripped, such as `enum`.

**Compile options.** Compile as ErgoTree **version 0** with segregated constants (header `0x10`), like every Gluon reactor on chain. Write `version: 0` explicitly in the code, because Fleet's default (version 1) produces a different tree and address. The reasons are in §9.6.

## 4. Execution plan

### Step 0: housekeeping (no PR)

1. Tag the current `main`, for example `scala-backend-final`, and push the tag. The removed code then stays one click away.
2. Delete the remote branch `fix`; its tip `1c25a67` is already in `main`.
3. Keep tags `v1` and `v2`. `v2` is the contract running the live Gold reactor (§9.3).
4. Optional: rename the repository to drop "Backend". GitHub redirects the old URL.

### PR 1: remove obsolete code, move and rename the contract

This PR only deletes and moves files. No working test is lost, because the existing tests cannot run (§9.2).

```sh
mkdir contracts
git mv modules/gluonw-base/src/main/resources/ErgoContracts/GluonW/Ergs/BoxGuardScripts/GluonWBoxGuardScript.es contracts/Gluon.es
git mv AuditReport-KB.pdf audits/
git rm -r app conf modules project build.sbt .scalafmt.conf fmt.sh Dockerfile .dockerignore \
  docker-compose.yml docker-compose.dev.yml Procfile postman.yaml ergo_config_template.json \
  docs CHANGELOG.md pull_request_template.md
rm -rf modules   # an ignored .DS_Store would otherwise keep the directory alive
```

- **Audit links.** Rewrite the 4 relative links (`../modules/...`) in 3 audit reports as permalinks to the commits they audited, for example `https://github.com/StabilityNexus/Gluon-Ergo-Contracts/blob/369fb81cc09664d25fd29d512ac89a44d6a2306c/modules/gluonw-base/…/GluonWBoxGuardScript.es`. An audit describes a file at one commit, so a permalink is the right reference. It also survives the move and the rename.
- **`.gitignore`.** Replace it with a Node/TypeScript one: `node_modules/`, `.env`, `coverage/`, `.DS_Store`. Keep `unremediated-security-reviews/`.
- **Not carried over:**
  - the off-chain pricing code, which belongs in the SDK;
  - the Play API and its Postman collection;
  - the testnet ids in `reference.conf`, which no longer exist on the current testnet (§9.4).

  The mainnet constants stay reachable through the tag; PR 3 records them.

### PR 2: toolchain, contract fixes, tests, CI

- **Toolchain:**
  - `package.json` with the pinned dependencies and the scripts `test`, `typecheck` and `verify`;
  - `.nvmrc` with `24`;
  - a strict `tsconfig.json` with `noEmit`;
  - `lib/`.
- **Contract changes.** Each change gets a test, and the compile check shows whether the ErgoTree moved.
  1. Change `val currentBlockNumber: Long = CONTEXT.HEIGHT` to `Int` at lines 325 and 458. The ErgoTree is unchanged.
  2. Header (D5): Update Treasury needs the oracle box as data input. In the header's terms, "DataInputs: GoldOracle", not "None".
  3. **D9: remove the division by 1000.** `val price: BigInt = ORACLE_BOX.R4[Long].get.toBigInt / 1000` becomes `… .toBigInt`.
     - Update the comments. R9's description should say that the peg factor (scaled by 10⁹) converts the oracle's unit into the price of one neutron, for example 0.001 for a per-kg gold oracle.
     - This changes the ErgoTree, so the review in §8 must cover it.
- **Tests:** see §5.
- **CI (`.github/workflows/ci.yml`):**
  - Triggers: push and pull request.
  - Checkout, then `setup-node` with `.nvmrc`. Pin both actions by commit SHA.
  - `npm ci --ignore-scripts`, `npm run typecheck`, `npm test`.
  - `npm run verify`, which:
    - recompiles every deployment config and diffs against `deployments/*.json`;
    - checks that `Deployments.md` is up to date;
    - compiles with `sigmastate-js` 0.6.3 and asserts identical bytes.
  - Optional: a scheduled read-only job that fetches each live reactor by NFT and compares its ErgoTree with the record.

### PR 3: configuration, scripts, deployment records

- `config/networks.json`, `config/gold.json` and `config/dollar.json`, as in §6.1 (N7).
- The scripts in §6.4 and `lib/node.ts`.
- **`deployments/mainnet.json`, seeded with the existing deployments** (§6.5). Gather each entry from chain data and git history: contract revision, constants, ErgoTree, address, NFT, tokens, creation transaction. Then generate `Deployments.md` from it.
- **Testnet rehearsal.** `testnet-oracle.ts` creates a stand-in oracle and buyback. `config/testnet/*.json` points at them. Then run the bootstrap and every transaction type against both a Gold-like and a Dollar-like test deployment.

### PR 4: README and auxiliary files

See §7. This PR also deletes the CC0 `LICENSE` (D4).

### Definition of done

- `git ls-files` shows only `contracts/`, `config/`, `lib/`, `test/`, `scripts/`, `deployments/`, `audits/`, `skills/`, `public/`, `.github/`, and the root files listed in §3.
- On a clean machine, after `npm ci`, `npm test` passes without network access. Mock-chain needs no node.
- `npm run verify` passes, and CI is green.
- `config/*.json` validates. TODO values remain only where §8 says so.
- The README follows the template, and no unintended `TODO` markers remain.

## 5. Test plan

**Principles**

- **Compile the real source.** Each test file compiles `contracts/Gluon.es` once, with the pinned options and the constants of a deployment config.
- **Cover both deployments.** Every scenario runs for both the Gold and the Dollar configuration (`describe.each`), because their oracle constants and initial peg factors (0.001 and 1) differ.
- **Execute locally.** Every transaction runs in `MockChain`, with no network.
- **One valid transaction per action.** It is built by `lib/transactions.ts`. Mutation tests change exactly one thing and must be rejected.
- **Do not test the contract against a copy of itself.** Expected amounts come from `lib/math.ts`. Add hand-computed examples, invariant (property) tests and one-unit mutations.
- **Add consensus checks.** MockChain is not a consensus validator. It does not check:
  - ERG conservation;
  - minimum box value;
  - the miner-fee output;
  - creation-height rules.

  A helper in `test/helpers.ts` checks these for every transaction that a test expects to succeed.
- **Mind the height offset.** Scripts see `HEIGHT = chain.height + 1`. Set oracle ages and volume buckets relative to that.
- **Name regression tests after the audit item they guard** (`BUG-05`, `Security Finding 1`, …).

**Scenario matrix**

| File | Must accept | Must reject |
|---|---|---|
| `compile.test.ts` | Both deployment configs compile. Records in `deployments/*.json` match. `sigmastate-js` 0.6.3 produces identical bytes. | Drift: CI fails if the contract or a config changes without regenerating the records. |
| `config.test.ts` | The shipped configs are well-formed: 32-byte NFT ids, decodable addresses, consistent fields. | A required TODO value makes the corresponding script refuse to run. |
| `bootstrap.test.ts` | <ul><li>At several oracle prices and seeds of up to 10 ERG, the bootstrap amounts give q = 0.6 and a proton price of 1 ERG, within one base unit of rounding.</li><li>Under the Gold config, one neutron is priced at the GoldPool price per gram (R4 / 1000). Under the Dollar config, it is priced at the USD Pool price (R4).</li><li>The genesis tokens are burned in the creation transaction.</li><li>The registers are encoded as the contract expects.</li><li>The minted tokens carry the EIP-4 metadata from config.</li><li>The new reactor box then accepts a fission, a fusion, both beta decays and an Adjust Peg.</li></ul> | <ul><li>A seed above 10 ERG, or of zero, makes the script refuse to run.</li><li>A malformed register, for example R9 as a `Long`, leaves the box unspendable. The test demonstrates this, which is why the script simulates before submitting.</li></ul> |
| `fission.test.ts` | Several sizes. With and without UI fee (context variable 0). Dev fee owed. Dev fee fully repaid, so no dev-fee output. | <ul><li>±1 on neutrons, protons or ERG</li><li>Wrong dev-fee amount or recipient</li><li>`R6._1` not incremented, or `R6._2` changed</li><li>Any change to R4, R5, R7, R8 or R9</li><li>Swapped token id</li><li>Changed contract</li><li>Unhealthy reactor</li><li>Oracle with the wrong NFT, stale (≥ 35 blocks), or not at `dataInputs(0)`</li></ul> |
| `fusion.test.ts` | Same structure as fission | Same structure as fission |
| `beta-decay-plus.test.ts`, `beta-decay-minus.test.ts` | <ul><li>Volume-bucket shifts for `nDays` = 0, 1, 13 and ≥ 14</li><li>Fee `φβ = φ0 + volume/R` at several volumes</li><li>Oracle-fee output at `OUTPUTS(2)`, with the buyback box as the last input</li><li>Every combination of dev fee and UI fee</li></ul> | <ul><li>±1 on amounts</li><li>Wrong `R9._1`</li><li>Peg factor changed</li><li>A trade that ends outside the healthy range (`isHealthyAfter`; regression for Security Finding 1)</li><li>Wrong oracle-fee amount, recipient script or NFT</li><li>Buyback box not the last input</li></ul> |
| `adjust-peg.test.ts` | −1 % when q > 0.98. +1 % when q < 0.50. Non-default peg factors, including Gold's 0.001 (stored as 1,000,000) → 990,000 / 1,010,000 (rounding). Chained steps until the reactor is healthy. | Reactor inside the healthy range. Wrong magnitude or direction. `R9._1` changed. Any other change to a register, token, value or the contract. Fake or stale oracle. |
| `update-treasury.test.ts` | New R5 signed by the current single-signature treasury, with `INPUTS(1)` = a box of the treasury address and the oracle box as data input. | R5 unchanged. No treasury box at `INPUTS(1)`. Missing signature. Contract, value, tokens, R4 or R6–R9 changed (immutability; regression for `6e9704b`). No data input (documents D5). |
| `adversarial.test.ts` | — | Port of `HackSpec`: taking ERG, protons, neutrons or any combination out of the reactor. Changing registers while keeping tokens and ERG. |
| `properties.test.ts` (optional, `fast-check`) | Random valid states and amounts | Any single-unit perturbation. Fission followed by fusion must never return more ERG than was deposited. |

**What happens to the existing Scala tests**

| Scala spec | Fate |
|---|---|
| `FissionTxSpec`, `FusionTxSpec`, `BetaDecayPlusSpec`, `BetaDecayMinusSpec`, `HackSpec`, and the on-chain half of `AdjustPegTxSpec` | Their scenarios are carried over to the matrix above. |
| `OracleHackSpec` | It defines a fixture but asserts nothing. The oracle cases above cover its idea. |
| `GluonWAlgorithmSpec`, `GluonWCalculatorSpec`, `GluonWFeesSpec`, `GluonWBoxSpec`, and the off-chain half of `AdjustPegTxSpec` | Dropped. They test the deleted Scala off-chain model; pricing belongs in the SDK. |

## 6. Configuration and deployment

### 6.1 Public configuration files (N2, N7)

`config/networks.json` holds public endpoints only:

```json
{
  "mainnet": { "nodeUrl": "https://node.sigmaspace.io", "explorerUrl": "https://api.ergoplatform.com" },
  "testnet": { "nodeUrl": "TODO", "explorerUrl": "https://api-testnet.ergoplatform.com" }
}
```

The scripts need a node with the indexed API. On 2026-10-11, `node.sigmaspace.io`, `node.ergopool.io` and `ergo-node.eutxo.de` all served it.

`config/gold.json`:

```json
{
  "deployment": "gluon-gold-v3",
  "network": "mainnet",
  "contract": { "source": "contracts/Gluon.es", "ergoTreeVersion": 0 },
  "minFee": "1000000",
  "oracle": {
    "description": "GoldPool: R4 = nanoERG per kg of gold. The initial peg factor of 0.001 converts it to a price per gram, so 1 neutron = 1 gram.",
    "poolNft": "3c45f29a5165b030fdb5eaf5d81f8108f9d8f507b31487dd51f4ae08fe07cf4a",
    "buybackNft": "TODO: the GoldPool buyback NFT 119a068a… was burned (2026-06-30). Set the NFT of the new buyback box here.",
    "buybackContractAddress": "TODO: set the address of the contract that guards the new buyback box here."
  },
  "treasuryAddress": "TODO: single-signature address, to be provided.",
  "bootstrap": {
    "seedErg": "TODO: at most 10. The genesis tokens are burned, so the seed stays in the reserve for good.",
    "initialFusionRatio": "0.6",
    "initialProtonPriceErg": "1",
    "initialPegFactor": "0.001",
    "maxDevFeeErg": "10000000",
    "tokenEmission": "100000000"
  },
  "tokens": {
    "nft":     { "ticker": "TODO", "name": "TODO", "description": "TODO" },
    "neutron": { "ticker": "TODO", "name": "TODO", "description": "TODO" },
    "proton":  { "ticker": "TODO", "name": "TODO", "description": "TODO" }
  }
}
```

`config/dollar.json` is identical except for these fields:

```json
{
  "deployment": "gluon-dollar-v3",
  "oracle": {
    "description": "USD Pool: R4 = nanoERG per 1 USD. With the initial peg factor of 1, 1 neutron = 1 USD.",
    "poolNft": "6a2b821b5727e85beb5e78b4efb9f0250d59cd48481d2ded2c23e91ba1d07c66",
    "buybackNft": "dcce07af04ea4f9b7979336476594dc16321547bcc9c6b95a67cb1a94192da4f",
    "buybackContractAddress": "TBHrt7kYkmxqwSo2EEPDQkTn…"
  },
  "bootstrap": {
    "initialPegFactor": "1"
  }
}
```

The file itself holds the full 910-character address of buyback box `6ec9976986a5…`.

**How the fields are used**

| Field | Use |
|---|---|
| `minFee`, `oracle.poolNft`, `oracle.buybackNft` | Compiled into `_MinFee`, `_OraclePoolNFT` and `_OracleBuybackNFT`. The values match those compiled into the live v2 Gold reactor and the second pre-v3 Dollar reactor. The exception is Gold's buyback NFT, which is TODO (D10). |
| `oracle.buybackContractAddress` | Decoded to its ErgoTree and compiled into `_OracleFeePk`. |
| `oracle` (on-chain checks) | Before every run, the scripts check that the pool box exists and is younger than 35 blocks, and that a box at the buyback contract holds the buyback NFT. They also print the pool NFT's on-chain name ("GoldPool NFT", "USD Pool NFT") and the implied price of one neutron, so the operator can confirm the right oracle. |
| `treasuryAddress` | A single-signature P2PK address (N5). Its key goes into R5, and its ErgoTree is what Update Treasury expects at `INPUTS(1)`. It stays TODO until it is provided (D7). |
| `bootstrap.*` | Inputs to §6.3.<br>`seedErg` must be greater than 0 and at most 10 (D11).<br>`initialPegFactor` is 0.001 for Gold and 1 for Dollar (D9). R9._2 stores it multiplied by 10⁹ (1,000,000 and 1,000,000,000), so the config validator requires that product to be a whole number. For Gold, 0.001 therefore means "no adjustment", and Adjust Peg moves it to 0.00099, 0.00101 and so on.<br>`maxDevFeeErg` and `tokenEmission` default to the v2 Gold values: 10,000,000 ERG and 100,000,000 tokens. |
| `tokens.*` | EIP-4 metadata (N6, §6.3). Tickers should not collide with existing Gluon tokens: v2 Gold uses "GluonW GAU" and "GluonW GAUC"; the unused pre-v3 Dollar reactors use "GSD"/"GLYD" and "USDG"/"USDGYL". |

**Rehearsal configs.** `config/testnet/gold.json` and `config/testnet/dollar.json` have the same shape. They point at the stand-in oracle and buyback that `testnet-oracle.ts` creates.

### 6.2 Secrets (N2)

`.env` (with `.env.example` committed) holds only:

- `MNEMONIC`: the signing key of whoever runs a script (deployer, treasury or keeper);
- optionally `MNEMONIC_PASSWORD`.

Everything else comes from `config/`.

### 6.3 Bootstrap (N4, N6, D7, D9, D11)

**Inputs**

| Symbol | Meaning |
|---|---|
| R | `seedErg` in nanoERG, with 0 < `seedErg` ≤ 10 ERG. This is the reserve; the reactor box holds R + `minFee`, since the contract's reserve is `value − _MinFee`. |
| f | `initialPegFactor` × 10⁹: the value stored in R9._2 (Gold 1,000,000; Dollar 1,000,000,000) |
| p | The adjusted oracle price, computed exactly as the contract does: ⌊R4 · f / 10⁹⌋. It is in nanoERG per whole neutron (10⁹ base units). |
| q₀ | 0.6 |
| π₀ | 1 ERG, i.e. 10⁹ nanoERG |
| E | `tokenEmission` × 10⁹ base units |
| H | The current height |

**Amounts in circulation**

- Neutrons: Sₙ = ⌊q₀ · R · 10⁹ / p⌋ base units.
- Protons: Sₚ = ⌊(1 − q₀) · R · 10⁹ / π₀⌋ = ⌊0.4 · R⌋ base units.

**Why this gives the targets.** The contract's ratio is q = Sₙ · p / R = 0.6.

- For q ≤ 0.99 the normalized ratio equals q, so neutrons start exactly at the peg: q · R / Sₙ = p.
- One proton is worth (1 − q) · R / Sₚ = 1 ERG.
- Rounding down moves both values by at most one base unit's worth.

**Example.** Seed 10 ERG, with the oracle prices of 2026-10-11:

| Deployment | Peg factor | p | Neutrons in circulation | Protons in circulation |
|---|---|---|---|---|
| Gold | 0.001 | 410.70 ERG per gram | ≈ 0.014609 | 4 |
| Dollar | 1 | 3.048108 ERG per USD | ≈ 1.968434 | 4 |

For Gold, p is the GoldPool price divided by 1000, rounded down to the nanoERG. For Dollar it is exactly the USD Pool price.

**Reactor box**

| Field | Value |
|---|---|
| Value | R + `minFee` |
| Tokens | NFT 1; neutrons E − Sₙ; protons E − Sₚ |
| R4 | (E, E) |
| R5 | treasury SigmaProp |
| R6 | (0, `maxDevFeeErg` × 10⁹) |
| R7, R8 | 14 zeros each |
| R9 | (⌊H/720⌋·720, f) |

**Genesis tokens (D11).** The Sₙ neutrons and Sₚ protons are burned in the creation transaction: they are simply left out of its outputs. The contract still counts them as circulating, so the seeded ERG stays in the reserve for good, backing tokens that nobody holds. Hence the 10 ERG cap.

**Transactions.** Four, chained:

1. Mint the NFT (emission 1, 0 decimals).
2. Mint the neutrons (emission E, 9 decimals).
3. Mint the protons (emission E, 9 decimals).
4. Create the reactor box and burn the genesis tokens.

Each mint goes to a deployer box that carries its EIP-4 registers. The NFT cannot be minted directly into the reactor box, because its EIP-4 metadata would occupy R4–R6, which the reactor uses. Fleet requires burning to be explicit (`burnTokens`), so an accidental burn cannot happen.

**Token metadata (EIP-4).** EIP-4 defines R4 = name, R5 = description and R6 = decimals; it has no ticker field. Wallets display R4, so the bootstrap writes:

- R4 = ticker;
- R5 = "‹name› (‹ticker›): ‹description›";
- R6 = decimals.

The pre-v3 Dollar tokens follow the same pattern: name "GSD", description "Gluon Stable Dollar (GSD): Stablecoin targeting 1 USD value.". The decimals must be 9 for neutrons and protons, because the contract prices 10⁹ base units as one unit of the peg.

**Safety.**

- The script refuses to run while a required field is TODO, or if `seedErg` is outside (0, 10].
- Before submitting, it simulates a fission, a fusion, both beta decays and an Adjust Peg against the new reactor box. The contract offers no other way out, so a malformed register would make the reactor unusable from its first transaction.
- It then runs every transaction through the node's `/transactions/check`.

### 6.4 Scripts

| Script | Purpose |
|---|---|
| `compile.ts` | Compile a deployment config and write the ErgoTree, address and template hashes to `deployments/<network>.json`. `--check` verifies instead of writing (CI). |
| `bootstrap.ts` | The fresh v3 deployment described in §6.3: seed of at most 10 ERG, genesis tokens burned |
| `update-treasury.ts` | v3 Update Treasury. `INPUTS(1)` is a box of the current treasury address, the oracle box is a data input, and the treasury key comes from `.env`. |
| `adjust-peg.ts` | Permissionless 1 % peg adjustment (D8). `--repeat` chains steps until q is healthy. The signer only pays the fee. It reports the factor relative to the deployment's initial factor, so Gold's 0.001 → 0.00099 shows as −1 %. |
| `render-deployments.ts` | Regenerate `Deployments.md` from `deployments/*.json`. `--check` is used in CI. |
| `testnet-oracle.ts` | Testnet only: mint a stand-in oracle-pool NFT and buyback NFT, then publish a fresh price box and a buyback box for rehearsals. |

**Common design**

- **Invocation:** `node scripts/<name>.ts --config config/<deployment>.json`.
- **Read-only by default.** A script:
  1. fetches the live boxes it needs;
  2. builds the unsigned transaction;
  3. executes it in a `MockChain` seeded with exactly those boxes;
  4. sends it to the node's `POST /transactions/check`;
  5. prints the reactor before and after (value, tokens, R4–R9, q);
  6. writes the unsigned transaction as EIP-12 JSON.
- **Signing and submission are explicit flags** (`--sign`, `--submit`). The key comes from `.env`.
  - Fleet's `Prover` signs transactions whose inputs are all plain P2PK boxes: the mints and the bootstrap.
  - Transactions that spend a reactor box need script reduction: Adjust Peg and Update Treasury. They use `sigmastate-js`'s `ProverBuilder`, which mock-chain also uses, with a real state context from `/blocks/lastHeaders/10`.
  - The treasury is a single key (N5), so no multisig signing flow is needed.

### 6.5 Deployment records (N3)

- **`deployments/<network>.json`** is the machine-readable record, written by the scripts. Per deployment it holds:
  - name, network and status;
  - contract revision (git tag or commit);
  - compiler package, engine and options;
  - every constant (type, hex, value);
  - ErgoTree, P2S address, and template hashes (SHA-256, which the public explorer indexes, and BLAKE2b-256);
  - oracle pool NFT, buyback NFT and buyback address;
  - token ids, tickers, decimals and emission;
  - treasury;
  - bootstrap parameters (seed ERG, q₀, proton price, initial peg factor, max dev fee), the oracle price used, and the burned genesis amounts;
  - transaction ids, reactor box id and height.
- **`Deployments.md`** is the human-readable list, generated from the JSON by `render-deployments.ts`. It has one section per deployment with its addresses and parameter values. The org's EVM contracts template has the same file and the same rule: tag every deployed version.
- **Initial entries** (PR 3):

| Deployment | Status | Contract revision | Reactor NFT | Notes |
|---|---|---|---|---|
| Gold, pre-v2 contract | Moved to v2 around height 1,397,584 (Nov 2024) | v1.x (to identify) | `797e331d…` | Old address prefix `E81MR2gG…`, taken from the UI's history code. Constants to reproduce from git history. |
| Gold v2 | Live | tag `v2` (= `d3f20c3`) | `797e331d…` | Address `U2Jtp6oe…`. The tree was reproduced byte for byte. Beta decays have been impossible since 2026-04-05 (§9.4). |
| Dollar (pre-v3), first build | Unused (seeded with dust) | to identify (2,226-byte tree) | `b2ec9f85…` | Tokens USDG / USDGYL; created at height 1,667,232. One neutron is priced at $0.001, because the price is divided by 1000 and there is no peg factor (D9). |
| Dollar (pre-v3), second build | Unused (seeded with dust) | `2329331` | `9fcb277e…` | Tokens GSD / GLYD; created at height 1,785,348. Same $0.001 pricing. |

Old testnet deployments are not recorded, because their ids no longer exist on the current testnet.

## 7. README and auxiliary files

**Template-Repo** (HEAD `f46a083`, 2026-02-11) contains exactly 7 files and no `.github/`.

**Org defaults.** GitHub applies issue forms and PR templates from the owning org's `.github` repository, but only to repositories that have none of their own. For this repository that is `StabilityNexus/.github` (D1):

- its issue forms are identical to StabilityNexus's;
- its PR template is a variant revised on 2026-03-04.

| Template file | Action |
|---|---|
| `README.md` | Rewrite in the template's style (outline below). |
| `COPYRIGHT.md` | Add verbatim. Like the template itself, it refers to a `LICENSE.md` that will not exist (D4). |
| `DCO.md` | Add verbatim. |
| `Contributors.md` | Add it, with the Discord line pointing to the Gluon channel. Contributors add their own rows, because the table holds personal contact details. |
| `.coderabbit.yml` | Add, then adapt: path rules for `contracts/**/*.es` (the template has Solidity rules but none for ErgoScript); keep the TypeScript and test rules; drop the Next.js and React Native rules. It only takes effect if CodeRabbit is enabled for the StabilityNexus org. |
| `.gitignore` | Do not copy it; it is GitHub's LaTeX list. Keep the Node/TypeScript one from PR 1. |
| `public/stability.svg` | Add verbatim. Also add `public/gluon.svg` from Gluon-Ergo-UI (`public/logo/gluon.svg`) as the project logo. |
| Org PR and issue templates | PR 1 deletes the root `pull_request_template.md`, so the StabilityNexus defaults apply. Optionally, add an Ergo-specific PR template modeled on `StabilityNexus/Template-Repo-EVM-Contracts`, with checks such as: accept and reject tests added; records regenerated; transaction-layout changes coordinated with the SDK. |

**README outline**

1. **Header (template):**
   - the `readme-top` anchor;
   - logo row: `stability.svg`, `gluon.svg`;
   - badge `Stability_Nexus-/Gluon_Ergo_Contracts`, linking to the Gluon app (URL to confirm);
   - the six social badges.
2. **Title and description:** `<h1>` plus one or two sentences, with links to the Gluon paper, the SDK and the UI.
3. **Project Maturity:**
   - Keep the checklist and tick what is true, for example "deployed to Ergo" (v2 Gold).
   - Delete the mobile-app and web-frontend items.
4. **Tech Stack:** ErgoScript, TypeScript, Fleet SDK (compiler, mock-chain), vitest, Node 24.
5. **Getting Started:** prerequisites (Node 24), clone `https://github.com/StabilityNexus/Gluon-Ergo-Contracts.git`, `npm ci`, `npm test`.
6. **Repository Structure:** a short tree.
7. **The Contract:**
   - **Box layout:** tokens and R4–R9. R5 holds the treasury's SigmaProp; the code calls it `TREASURY_MULTISIG`, but deployments use a single-signature address.
   - **Transactions:** a table of the six transaction types with their inputs, data inputs, outputs and context variable. Include the data-input requirement (§9.3) and the conditional positions of the oracle, dev and UI fee outputs. This is the interface the SDK implements.
   - **The two pegs:**
     - Gold uses the GoldPool oracle, which reports per kg, with an initial peg factor of 0.001. One neutron is then one gram, and for Gold a factor of 0.001 means "no adjustment".
     - Dollar uses the USD Pool oracle with an initial peg factor of 1.
   - **Changes from v2 to v3:**
     - R9 = `(LastBucketBlock, PegFactor)`;
     - peg-adjusted price. The oracle price is no longer divided by 1000; the peg factor carries the unit;
     - the healthy range and the post-trade health check;
     - qStar 0.99 with the normalized formula;
     - phi1 = 1;
     - oracle check on all four user actions.

     This list replaces the 2022 `CHANGELOG.md` stub.
8. **Configuration and Deployment:**
   - `config/` holds the public parameters;
   - `.env` holds only the mnemonic;
   - the scripts;
   - the dry-run → check → sign → submit flow.
9. **Deployments:** a link to `Deployments.md` and `deployments/*.json`.
10. **Security & Audits:** links to `audits/`, and private disclosure through Discord or Telegram (wording from the EVM template).
11. **Contributing:** the template text, with `npm run typecheck` and `npm test` in place of the template's npm commands.
12. **Footer:** `© 2025 The Stable Order.`

There is no License section (D4).

## 8. Before the v3 deployments

The cleanup does not depend on any of these, but `bootstrap.ts` must not run on mainnet until the items for that deployment are resolved.

| # | Item | Gold | Dollar |
|---|---|---|---|
| 1 | Oracle-fee destination (D10) | **Blocked**: TODO in `config/gold.json` until the GoldPool has a new buyback box | Ready (dortBuyback alive) |
| 2 | Price scale (D9) | Ready (peg factor 0.001) | Ready (peg factor 1) |
| 3 | Treasury address (D7) | TODO | TODO |
| 4 | Token metadata and seed amount, at most 10 ERG (N6, N4, D11) | TODO | TODO |
| 5 | Review of the final v3 source, including the PR 2 changes (notably the removal of `/ 1000`) | Required: v3 is immutable | Required |
| 6 | Testnet rehearsal (PR 3) | Required | Required |

**Not blocking the deployment, but needed for users.** The SDK and UI must support v3 before users can use the new reactors through the UI. They also need to become configurable per deployment: today they hard-code the v2 Gold ids and implement v2 rules (§9.5).

**Unchanged by this plan.** On the live v2 Gold reactor, beta decays with a non-zero oracle fee have been impossible since 2026-04-05, because its buyback NFT was burned (§9.4).

## 9. Findings and evidence

### 9.1 What is in the repository (74 tracked files)

| Path | What it is | Fate |
|---|---|---|
| `modules/gluonw-base/src/main/resources/…/GluonWBoxGuardScript.es` | The contract (v3.0) | Keep → `contracts/Gluon.es` |
| `app/`, `conf/`, `Procfile`, `Dockerfile`, `.dockerignore`, `docker-compose*.yml`, `postman.yaml` | Play REST backend and its packaging | Delete |
| `modules/common/`, `modules/gluonw-base/src/main/scala/` | Off-chain Scala library: box wrappers, pricing, transaction builders, config, explorer client | Delete. `lib/transactions.ts` re-implements the layouts, including Adjust Peg, which has no other implementation. |
| `…/tools/BoxCreation.scala`, `ergo_config_template.json`, `docs/Redeployment.md` | Deployment by editing constants in a Scala `App` and running it | Replaced by `config/` and `scripts/` |
| `modules/gluonw-base/src/test/` | ScalaTest suite, 3,785 lines | Replaced by `test/` (§5) |
| `build.sbt`, `project/`, `.scalafmt.conf`, `fmt.sh` | sbt build | Delete |
| `GluonWTxProxyContract.es`, `DummyErgoScript.es`, `ErgoScriptTemplate.es`, `SampleBox.box.ergo.yml` | An empty stub, a test dummy, a template, and a leftover from a lending project | Delete |
| `README.md`, `CHANGELOG.md`, `pull_request_template.md` | Boilerplate: "Ergo-Play-Boilerplate", "Build exle-dot", "Everything!" | Rewrite or delete (§7) |
| `AuditReport-KB.pdf` | External audit report | Move → `audits/` |
| `audits/`, `skills/` | Audit and security-review reports (Oct 2026), and review skills | Keep. Gluon-EVM uses the same skills. |
| `LICENSE` | CC0 1.0 | Delete (D4) |

### 9.2 Why the Scala build is not worth reviving

- **A dependency that no longer exists.**
  - Main and test code depend on `io.github.ergo-lend:edge:0.1.14-SNAPSHOT` for box wrappers, registers, `Tx` and the node client.
  - It came from Sonatype OSSRH snapshot repositories, which shut down in June 2025. Their URLs now return 404, and the group is not on Maven Central.
  - Its source (Ergo-Lend/edge, last push 2024-03-20) would have to be vendored.
- **An obsolete toolchain:**
  - a Bintray/JCenter resolver;
  - sbt 1.7.2 and Scala 2.13.9, too old for current JDKs (this machine has JDK 25, which Scala supports only from 2.13.17);
  - Play 2.8.8, which is end of life;
  - ergo-appkit 5.0.3 (latest 6.0.1) and sigma-state 5.0.8 (latest 6.0.7).
- **The tests need a live third-party node.** `GluonWBase` gets its context from `edge`'s `BaseClient`, which points at the hard-coded `213.239.193.208:9052`.
- **The tests' off-chain model has drifted from the contract.**
  - `GluonWAlgorithm` still uses `qStar = 0.66` and `min(q, qStar)`.
  - The contract uses `qStar = 0.99` and the normalized `qNorm` (changed in `2329331`).
  - Beta-decay tests built on that model cannot match the contract.
- **The refactored contract was never run.** The October audit reports state that no compiler was available, so it was never compiled or executed.

### 9.3 Experiments

Setup: Fleet SDK 0.12.0, whose compiler and mock-chain run on `sigmastate-js` 0.4.6, on Node 23 and bun 1.2.17. The compiler checks also used `sigmastate-js` 0.6.3. All scripts were throwaway and ran in the session's scratch directory.

1. **Reproducibility.** The contract at tag `v2`, compiled as ErgoTree v0 with segregated constants and the mainnet constants from `reference.conf`, equals the live Gold reactor's ErgoTree byte for byte (2,869 bytes). Those constants are:
   - `_MinFee` = 1,000,000;
   - the buyback ErgoTree;
   - pool NFT `3c45f29a…`;
   - buyback NFT `119a068a…`.
2. **v3 compiles with `sigmastate-js` 0.4.6:** 2,999 bytes as v0, 3,001 bytes as v1.
   - With 0.6.3 (Sigma 6.0.7, the node's generation) it **fails to compile**. Since Sigma 6.0.3 the compiler checks declared types, and `val currentBlockNumber: Long = CONTEXT.HEIGHT` (lines 325 and 458) declares an `Int` as `Long`.
   - With `Int`, both compilers emit identical bytes, which are also identical to the unpatched tree under 0.4.6.
3. **Execution** (expected amounts computed with BigInt, mirroring the contract):

   | Transaction | Result |
   |---|---|
   | Fission of 10 ERG (dev fee at `OUTPUTS(2)`) | Accepted |
   | Fission with one extra nano-neutron | Rejected |
   | Fission with one nano-proton too few | Rejected |
   | Fission with the dev fee 1 nanoERG short | Rejected |
   | Adjust Peg −1 % when q > 0.98 | Accepted |
   | Adjust Peg −2 % | Rejected |
   | Adjust Peg +1 % | Rejected |
4. **Update Treasury needs a data input.**
   - The header says "DataInputs: None", but the transaction fails:
     - without a data input (`ArrayIndexOutOfBoundsException`);
     - with one that has no R4 (`None.get`);
     - with one that has an `R4: Long` but no token.
   - It passes with the oracle box as data input.
   - Cause: the top-level `ORACLE_BOX`, `price` and `cOracle` are evaluated eagerly.
   - Two further consequences: every Gluon transaction needs at least 2 inputs, because `INPUTS(1)` is read at the top level; and the output reserve must stay above zero, because `qOut` divides by it.
   - The live v2 contract behaves the same way.
5. **Contract without `/ 1000` (D9).**
   - The contract was patched (`price = R4` plus the `Int` fix). It compiles with `sigmastate-js` 0.6.3 and with Fleet's 0.4.6.
   - Both reactors were bootstrapped per §6.3 with a 10 ERG seed.

   | | Gold (peg factor 0.001, stored as 1,000,000) | Dollar (peg factor 1, stored as 10⁹) |
   |---|---|---|
   | Oracle R4 | 410,698,181,858,725 (per kg) | 3,048,107,555 (per USD) |
   | Adjusted price of one neutron | 410,698,181,858 nanoERG (one gram) | 3,048,107,555 nanoERG (one USD) |
   | Genesis q / proton price | 0.59999998 / 1.00000005 ERG | 0.599999999 / 1.000000002 ERG |
   | Fission of 1 ERG | Accepted | Accepted |
   | Same fission with one extra nano-neutron | Rejected | Rejected |
   | Adjust Peg −1 % after the price rises 70 % (q > 0.98) | Accepted (1,000,000 → 990,000) | Accepted (10⁹ → 0.99 × 10⁹) |
   | Adjust Peg −2 % | Rejected | Rejected |

   The raw oracle `price` is used only to compute `priceAdjusted`, so the peg factor fully determines the unit.

### 9.4 Chain state (queried 2026-10-11)

**Gold v2 reactor `19e094973f9f…6deb`**

- Value 12,714.49 ERG; about 45.65 GAU and 16.11 GAUC in circulation; the current box was created at height 1,778,037.
- R5 is the single key of `9huM5LVa7b8uAUGgzDGDCHUdRe4L1j4SQ6kHXC1JVVbL1RSNCmd`.
- Under v2, that key can move the whole reserve to any script, through the Update Treasury branch, which does not pin the output contract. v3 removes that power.

**GoldPool oracle**

- NFT `3c45f29a…` ("GoldPool NFT"); reward token GORT.
- R4 = nanoERG per kg of gold: 410,698,181,858,725 at height 1,892,135, i.e. 410.70 ERG per gram.

**Gold buyback (`119a068a…`, "gortBuyback")**

- No unspent box holds it, and no box remains at its contract address.
- It left the contract for a wallet at height 1,757,086 (2026-04-05). Its last holder box was spent without re-emitting it around height 1,818,488 (2026-06-30).

**USD Pool oracle**

- NFT `6a2b821b…` ("USD Pool NFT"); reward token DORT.
- R4 = nanoERG per 1 USD: 3,048,107,555 at height 1,892,130, i.e. 3.048 ERG per USD.

**Dollar buyback (`dcce07af…`, "dortBuyback")**

- Three tokens were issued "so 3 buyback boxes can coexist".
- One sits in the buyback-contract box `6ec9976986a5…`; its ErgoTree is 662 bytes, and the address is 910 characters long.
- The other two are in a wallet. That cannot be abused, because the contract also checks the output script.
- The tree of the second pre-v3 Dollar reactor (`9fcb277e…`) contains exactly this contract, the USD Pool NFT and this buyback NFT.

**Pre-v3 Dollar reactors (both unused)**

- `9fcb277e…` (GSD/GLYD) and `b2ec9f85…` (USDG/USDGYL).
- Both were seeded with dust: 2 ERG and 0.01 tokens in the first case, 1 ERG in the second.
- Their R9 is a `Long`.

**Testnet.** The Gluon NFT `6540859e…` and oracle NFT `d94bfac4…` from `reference.conf` do not exist on the current testnet (explorer 404; height ≈ 593 k).

### 9.5 Gluon-Ergo-SDK and Gluon-Ergo-UI

- **The backend is unused.**
  - No branch or commit of either repository calls the REST API (`/api/v1/fission`, `transmute`, `mint`, …).
  - The SDK builds transactions locally with `ergo-lib-wasm` 0.23 and talks only to an Ergo node.
  - The backend's own mainnet node, `213.239.193.208:9053`, is unreachable.
- **The SDK implements v2 semantics:**
  - qStar 0.66 clamp and phi1 0.5;
  - R9 written and read as a `Long`;
  - no peg factor and no health checks;
  - no Adjust Peg and no Update Treasury;
  - the dev-fee recipient is hard-coded instead of read from R5;
  - the v2 Gold ids are module constants, so it cannot target another deployment.

  Neither repository has tests.
- **The deployed UI uses a vendored copy of the SDK.** Its `mainnet` branch, which the README says Vercel deploys, vendors SDK 1.0.0. `main` uses npm `gluon-ergo-sdk` 1.0.3.
- **What this repository gives the SDK:**
  - `deployments/*.json` and `Deployments.md`;
  - the transaction-layout specification in the README;
  - the v2 → v3 change list;
  - optionally, golden test vectors exported from the tests.

### 9.6 Current practice for Ergo contract repositories

- **Protocol.** Sigma 6.0 (EIP-50) has been active on mainnet since height 1,628,160 (2025-10-06); nodes run 6.0.7.
  - The 6.0 features (UnsignedBigInt, new methods) need ErgoTree v3.
  - Trees of versions 0–2 keep their 5.x semantics by soft-fork design; only their costing changed.
- **Toolchain status:**
  - Fleet SDK 0.12.0 (2025-07-26) is the latest release; there has been none in about 14 months. Its compiler and mock-chain are thin wrappers (about 100 and 400 lines) over `sigmastate-js` 0.4.6, which predates 6.0. Open PRs move it to `sigmastate-js` 0.6 and add a script prover.
  - `sigmastate-js` 0.6.3 (2026-09-29) corresponds to Sigma 6.0.7.
  - ergo-appkit is at 6.0.1 (2026-08-27).
  - `ergo-lib-wasm` 0.28.0 (2024-08) is what Nautilus 1.3.3 and the Gluon SDK/UI use; sigma-rust 0.29 is unreleased.
  - ergo-playgrounds, Kiosk and the ergoscript-compiler CLI are stale.
- **What maintained repositories do:**
  - **TypeScript projects**, including StabilityNexus's BenefactionPlatform-Ergo and Bountiful-BountyPlatform-Ergo: `contracts/*.es`, Fleet `compile` with a map of named constants, and mock-chain tests under vitest.
  - **JVM projects**, such as ChainCash: appkit 6 with offline mocked clients.
  - **Most mature artifact pipeline:** Rosen Bridge's, which publishes per-network JSON and TypeScript packages from release CI.
- **ErgoTree version: use v0, not v3.**
  - `ergo-lib-wasm` 0.28 cannot deserialize v2/v3 trees, so users of Nautilus, the SDK and the UI could not spend such a box.
  - Mock-chain 0.12 cannot evaluate v3.
  - The contract uses no 6.0 feature, and v0 matches the deployed reactors.
  - v1 would also work, but it changes the address and gains nothing.
- **Testing practice:**
  - Compile the real source with the real constants.
  - For every spending path, write one positive case and negative cases that each break exactly one condition.
  - Control HEIGHT explicitly.
  - Add consensus-style assertions.
  - Optionally add property tests.
- **Deployment practice:**
  - Pin the compiler.
  - Commit one artifact per network: ErgoTree, address, template hash, constants and compiler version.
  - Dry-run by default and use the node's `/transactions/check`.
  - Never commit a key.
  - Rehearse on testnet.
- **CI practice:**
  - Pin actions by commit SHA.
  - `npm ci` against the lockfile.
  - Run the tests and an artifact drift check.
  - Compile with the newest compiler as a forward-compatibility check; it would have caught the `Long`/`Int` annotation.
  - Optionally schedule an on-chain parity check.

## 10. Risks

| Risk | Mitigation |
|---|---|
| Fleet releases slowly, and its engine (`sigmastate-js` 0.4.6) predates Sigma 6.0. | Tests use only stable APIs: `compile`, `MockChain` and the builders. The forward-compatibility compile in CI catches compiler drift. If Fleet stalls, call `sigmastate-js` directly; it exposes the compiler and prover that Fleet wraps. |
| The interpreter in the tests and the node's interpreter differ in version. | Every mainnet transaction goes through `/transactions/check`. Each transaction type is rehearsed on testnet. |
| MockChain accepts transactions that a node would reject. | The consensus helper in tests (§5) and `/transactions/check` in scripts (§6.4). |
| Tests check the contract against a transcription of itself. | Hand-computed examples, invariant and property tests, and one-unit mutations. |
| A compiler or option change silently alters an address. | Exact version pins, `version: 0` written explicitly, and `npm run verify` in CI. |
| A bootstrap mistake leaves an unusable reactor: v3 is immutable and has no exit outside its rules. The seed itself is small and burned by design (D11). | Config validation and TODO checks; the 10 ERG cap; simulating every action on the new box; `/transactions/check`; testnet rehearsal. |
| Gold's peg factor of 0.001 means "no adjustment" (it converts the per-kg oracle price to grams). Off-chain code could misread it as a 99.9 % devaluation. | State it in the config, the README and `Deployments.md`, and record the initial factor in `deployments/*.json`. The SDK and UI must treat the factor relative to that recorded value. |
| Gold's factor is stored as 1,000,000 (six significant digits), so each ±1 % step can round. | The rounding is at most one unit per step, i.e. one part in a million. It is negligible, and the contract defines it exactly: integer arithmetic on `PEG_FACTOR × 99 / 100`. |
| The oracle price moves between building and inclusion. | q₀ = 0.6 is exact only at the price used. A drift is harmless, because 0.6 sits well inside the healthy range [0.50, 0.98]. |
| v3 is non-upgradeable by design (`6e9704b`). A future fix would need a user-driven move to a new deployment. | Extra review effort before mainnet: the tests above, and a review of the final source (§8). |
| Key handling. | Only the mnemonic lives in `.env` (N2). Scripts are read-only by default and sign and submit only on explicit flags. |

## Key sources

- **Fleet SDK:**
  - https://github.com/fleet-sdk/fleet (`packages/compiler`, `packages/mock-chain`)
  - https://fleet-sdk.github.io/docs/
- **Token metadata (EIP-4):** https://github.com/ergoplatform/eips/blob/master/eip-0004.md
- **Sigma 6.0:**
  - https://docs.ergoplatform.com/dev/protocol/sigma-6/
  - https://github.com/ergoplatform/eips/pull/100
  - https://github.com/ergoplatform/sigmastate-interpreter/releases
- **Ergo tooling overview:** https://docs.ergoplatform.com/dev/scs/ergoscript-tooling/
- **Org templates and examples:**
  - https://github.com/StabilityNexus/Template-Repo
  - https://github.com/StabilityNexus/Template-Repo-EVM-Contracts
  - https://github.com/StabilityNexus/.github
  - https://github.com/StabilityNexus/BenefactionPlatform-Ergo
- **Other examples:**
  - https://github.com/rosen-bridge/contract
  - https://github.com/ChainCashLabs/chaincash
- **Infrastructure:** https://central.sonatype.org/pages/ossrh-eol/ (OSSRH end of life)
