<!-- Don't delete it -->
<div name="readme-top"></div>

<!-- Organization Logo -->
<div align="center" style="display: flex; align-items: center; justify-content: center; gap: 16px;">
  <img alt="Stability Nexus" src="public/stability.svg" width="175">
  <img alt="Gluon" src="public/gluon.svg" width="175" />
</div>

&nbsp;

<!-- Organization Name -->
<div align="center">

[![Static Badge](https://img.shields.io/badge/Stability_Nexus-/Gluon_Ergo_Contracts-228B22?style=for-the-badge&labelColor=FFC517)](https://stability.nexus/)

<!-- Correct deployed url to be added -->

</div>

<!-- Organization/Project Social Handles -->
<p align="center">
<!-- Telegram -->
<a href="https://t.me/StabilityNexus">
<img src="https://img.shields.io/badge/Telegram-black?style=flat&logo=telegram&logoColor=white&logoSize=auto&color=24A1DE" alt="Telegram Badge"/></a>
&nbsp;&nbsp;
<!-- X (formerly Twitter) -->
<a href="https://x.com/StabilityNexus">
<img src="https://img.shields.io/twitter/follow/StabilityNexus" alt="X (formerly Twitter) Badge"/></a>
&nbsp;&nbsp;
<!-- Discord -->
<a href="https://discord.gg/YzDKeEfWtS">
<img src="https://img.shields.io/discord/995968619034984528?style=flat&logo=discord&logoColor=white&logoSize=auto&label=Discord&labelColor=5865F2&color=57F287" alt="Discord Badge"/></a>
&nbsp;&nbsp;
<!-- Medium -->
<a href="https://news.stability.nexus/">
  <img src="https://img.shields.io/badge/Medium-black?style=flat&logo=medium&logoColor=black&logoSize=auto&color=white" alt="Medium Badge"></a>
&nbsp;&nbsp;
<!-- LinkedIn -->
<a href="https://linkedin.com/company/stability-nexus">
  <img src="https://img.shields.io/badge/LinkedIn-black?style=flat&logo=LinkedIn&logoColor=white&logoSize=auto&color=0A66C2" alt="LinkedIn Badge"></a>
&nbsp;&nbsp;
<!-- Youtube -->
<a href="https://www.youtube.com/@StabilityNexus">
  <img src="https://img.shields.io/youtube/channel/subscribers/UCZOG4YhFQdlGaLugr_e5BKw?style=flat&logo=youtube&logoColor=white&logoSize=auto&labelColor=FF0000&color=FF0000" alt="Youtube Badge"></a>
</p>

---

<div align="center">
<h1>Gluon Ergo Contracts</h1>
</div>

[Gluon](https://eprint.iacr.org/2025/1372) is a cryptocurrency stabilization protocol. This repository contains its
smart contract on [Ergo](https://ergoplatform.org/), the tests that execute it, and the scripts that deploy and operate
its reactors: Gluon Gold, pegged to one gram of gold, and Gluon Dollar, pegged to one US dollar. The
[Gluon Ergo SDK](sdk/) (published to npm as `@stabilitynexus/gluon-ergo-sdk`), located in `sdk/`, and the
[Gluon Ergo UI](https://github.com/StabilityNexus/Gluon-Ergo-UI), in its own repository, build users' transactions
for the reactors.

---

## Tech Stack

### Smart Contract

- ErgoScript, compiled to ErgoTree version 0
- [Ergo](https://ergoplatform.org/), with prices from [oracle pools](https://github.com/ergoplatform/oracle-core)

### Tests and Scripts

- TypeScript, run directly by Node.js 24 (there is no build step)
- [Fleet SDK](https://fleet-sdk.github.io/docs/): compiler, mock chain, transaction builder and wallet
- [sigmastate-js](https://github.com/ergoplatform/sigmastate-interpreter), the JavaScript build of the interpreter
  that Ergo nodes run
- [Vitest](https://vitest.dev/)

---

## Getting Started

### Prerequisites

- Node.js 24 (the version in `.nvmrc`)
- npm

### Installation

#### 1. Clone the Repository

```bash
git clone https://github.com/StabilityNexus/Gluon-Ergo-Contracts.git
cd Gluon-Ergo-Backend-Contracts
```

#### 2. Install Dependencies

```bash
npm ci
```

#### 3. Run the Tests

```bash
npm test
```

The tests compile [`contracts/Gluon.es`](contracts/Gluon.es) and execute every transaction type on a simulated chain,
so they need neither a node nor network access. `npm run typecheck` checks the TypeScript code, and `npm run verify`
checks the deployment configs and records (see [Deployments](#deployments)).

---

## Repository Structure

```
contracts/Gluon.es   the Gluon reactor contract
config/              public deployment parameters: the node, Gold and Dollar
lib/                 deployment tooling: compilation, configuration, records, signing and simulation
sdk/                 the SDK, published to npm as @stabilitynexus/gluon-ergo-sdk
test/                contract tests, on a simulated chain
scripts/             deployment and operation scripts
deployments/         records of every deployment, per network
Deployments.md       the same records, for humans (generated)
audits/              audit and security review reports
skills/              security review skills for coding agents
public/              logos
```

---

## The Contract

Each deployment is one box, the reactor, guarded by [`contracts/Gluon.es`](contracts/Gluon.es). It holds the reserve
of ERG and the neutrons and protons that are not in circulation. Neutrons are the stablecoin, pegged to an asset such
as one gram of gold. Protons are worth the rest of the reserve, so they absorb the swings of the price of ERG.

The fusion ratio `q = neutrons in circulation × price of one neutron / reserve` is the share of the reserve that backs
the neutrons. Fission, fusion and beta decays are allowed only while `q` is in the healthy range [0.5, 0.98], and beta
decays must also leave it there.

### Box Layout

| | Contents |
|---|---|
| Value | the reserve plus `_MinFee`, in nanoERG |
| Token 0 | the reactor NFT |
| Token 1 | the neutrons not in circulation |
| Token 2 | the protons not in circulation |
| R4 | `(Long, Long)`: total supply of neutrons and of protons |
| R5 | `SigmaProp`: the treasury, which receives the dev fees. The contract calls it `TREASURY_MULTISIG`, but deployments use a single-signature key. |
| R6 | `(Long, Long)`: dev fees paid so far, and the maximum, in nanoERG |
| R7 | `Coll[Long]`: value of the beta decays + in each of the last 14 buckets of 720 blocks (about a day each), in nanoERG |
| R8 | `Coll[Long]`: the same for beta decays − |
| R9 | `(Long, Long)`: first block of the current bucket, and the peg factor scaled by 10⁹ |

The compile-time constants are `_MinFee`, `_OraclePoolNFT`, `_OracleBuybackNFT` and `_OracleFeePk` (the ErgoTree of the
oracle's buyback contract). Each deployment sets them in its config.

### Transactions

Every transaction spends the reactor as `INPUTS(0)` and recreates it as `OUTPUTS(0)`, under the same contract. It
also has the oracle pool box as data input 0. Except in Update Treasury, that box must hold `_OraclePoolNFT` as its
first token and must have been created less than 35 blocks before the block that includes the transaction. Update
Treasury does not use the oracle, but the contract reads data input 0 and `INPUTS(1)` in every transaction, so every
transaction needs both.

| Transaction | Reactor | Other inputs | Outputs after the reactor | Context variables |
|---|---|---|---|---|
| Fission | ERG in; neutrons and protons out, in proportion to their supplies | boxes paying the ERG | `OUTPUTS(1)`: the user's box; then the fee outputs | Optional UI-fee key, as variable 0 of the reactor input |
| Fusion | neutrons and protons in, in proportion to their supplies; ERG out | boxes holding the neutrons and protons | as for fission | as for fission |
| Beta Decay + | protons in, neutrons out; R7, R8 and R9's bucket block updated | boxes holding the protons; the oracle buyback box last, if an oracle fee is due | as for fission | as for fission, plus what the buyback contract requires (see the oracle fee below) |
| Beta Decay − | neutrons in, protons out; R7, R8 and R9's bucket block updated | boxes holding the neutrons; the oracle buyback box last, if an oracle fee is due | as for fission | as for beta decay + |
| Adjust Peg | peg factor × 0.99 if `q` > 0.98, or × 1.01 if `q` < 0.5; nothing else changes | a box paying the miner fee | none required | none |
| Update Treasury | new key in R5; nothing else changes | `INPUTS(1)`: a box of the current treasury's address; the treasury signs | none required | none |

Anyone may submit any of these transactions, except Update Treasury, which only the current treasury can sign.

**Fees.** Each transaction of the first four types moves a value, its principal: the ERG paid or withdrawn, or the
value in ERG of the protons or neutrons that decay. Fission keeps 0.1 % of it in the reserve, fusion 0.5 %, and a beta
decay 0.5 % plus the net value of the beta decays in the same direction over the last 14 buckets, relative to the
reserve. Up to three fees are paid out, each in its own output, which receives exactly the fee plus `_MinFee`. These
outputs follow `OUTPUTS(1)` in this order, and a fee of zero has no output:

1. **Oracle fee**, 0.1 % of a beta decay. It goes to the oracle buyback box, which the transaction spends as its last
   input and recreates with the same contract (`_OracleFeePk`) and first token (`_OracleBuybackNFT`), and with the fee
   plus `_MinFee` added to its value. Oracle-core v2 buyback contracts, such as the USD oracle's, also require
   variable 0 = `1` on the buyback input and the id of the spent buyback box in R4 of its successor.
2. **Dev fee**, 0.5 %, decreasing linearly to zero as the dev fees paid (R6) approach their maximum. It goes to the
   treasury (R5) and is added to R6.
3. **UI fee**, 0.4 %, if the reactor input sets variable 0 to a key. It goes to that key.

[`lib/transactions.ts`](lib/transactions.ts) builds all six transactions, and [`lib/math.ts`](lib/math.ts) mirrors the
contract's integer arithmetic. Off-chain code such as the SDK must follow the same layouts and arithmetic.

### The Two Pegs

The contract computes the price of one neutron as `oracle price × peg factor / 10⁹`. The initial peg factor therefore
sets the unit:

| | Gluon Gold | Gluon Dollar |
|---|---|---|
| Oracle pool | GoldPool, in nanoERG per kg of gold | USD Pool, in nanoERG per US dollar |
| Initial peg factor | 0.001 (stored as 1000000) | 1 (stored as 1000000000) |
| One neutron at launch | one gram of gold | one US dollar |

For Gold, a peg factor of 0.001 means no adjustment. Adjust Peg changes the factor by 1 % at a time, so compare it
with the initial factor, which `deployments/` records.

### Changes from v2 to v3

The live Gold reactor runs v2 (tag [`v2`](https://github.com/StabilityNexus/Gluon-Ergo-Contracts/tree/v2)).
In v3, [`contracts/Gluon.es`](contracts/Gluon.es):

- stores `(LastBucketBlock, PegFactor)` in R9, instead of `LastBucketBlock` alone;
- multiplies the oracle price by the peg factor, instead of dividing it by 1000;
- adds Adjust Peg;
- allows fission, fusion and beta decays only in the healthy range, and checks that beta decays stay in it;
- normalizes the fusion ratio as `min(q / (q + 1 − q*), q)` with q* = 0.99, instead of `min(q, 0.66)`;
- sets the slope of the beta decay fee, φ₁, to 1 instead of 0.5;
- checks the oracle in all four user transactions, not only in beta decays;
- requires Update Treasury to recreate the reactor under the same contract, which v2 did not check.

---

## Configuration and Deployment

Public parameters live in [`config/`](config/): [`networks.json`](config/networks.json) names the mainnet node and explorer,
and each deployment has a config, such as [`config/gold.json`](config/gold.json) and
[`config/dollar.json`](config/dollar.json). Values that are not decided yet start with `TODO`, and the scripts refuse
to use them. The only secret is the mnemonic of the wallet that signs, in `.env`:

```bash
cp .env.example .env   # then set MNEMONIC
```

| Script | Purpose |
|---|---|
| `node scripts/compile.ts <config>` | Print the contract a config compiles to: ErgoTree, address and template hash |
| `node scripts/bootstrap.ts <config> [--submit]` | Deploy a reactor |
| `node scripts/adjust-peg.ts <config> [--submit [--repeat]]` | Adjust the peg of an unhealthy reactor, once or until it is healthy |
| `node scripts/update-treasury.ts <config> <address> [--submit]` | Move the treasury to another single-signature address |
| `node scripts/render-deployments.ts` | Regenerate `Deployments.md` from `deployments/` |

Scripts that send transactions do a dry run by default: they build and sign the transactions and have the node check
them, without broadcasting. Only `--submit` broadcasts.

### Deploying a Reactor

The bootstrap mints the reactor NFT, the neutrons and the protons, then creates the reactor and burns the neutrons and
protons in circulation at genesis. The seed (at most 10 ERG) therefore stays in the reserve for good. The token amounts
follow from the config's `initialFusionRatio` and `initialProtonPriceErg`: in the shipped configs, the reactor starts
at a fusion ratio of 0.6, with one proton worth 1 ERG.

1. Set every `TODO` value in the deployment's config: the treasury, the seed and the token metadata.
2. Commit the contract source: the deployment record points to the commit it was compiled from.
3. Run `node scripts/bootstrap.ts <config>`. Besides the node's check, the dry run simulates a fission, both beta
   decays, a fusion and an Adjust Peg against the new reactor box.
4. Run it again with `--submit`. It records the deployment in `deployments/<network>.json` and `Deployments.md`.
   Commit both, and tag the deployed commit.

---

## Deployments

[Deployments.md](Deployments.md) lists every reactor deployed so far, with its contract address, its tokens and the
parameters it was deployed with. [`deployments/`](deployments/) holds the same records as JSON, for programs. On
mainnet, the v2 Gluon Gold reactor is live; the v3 Gold and Dollar reactors are planned. The v3 Gold deployment
needs a new oracle buyback box first, because the GoldPool's buyback NFT was burned.

`npm run verify`, which CI runs, recompiles each recorded contract from the commit it was compiled from and checks
that it yields the recorded ErgoTree, address and template hash. It also checks that every config is valid and that
`Deployments.md` is up to date.


---

## SDK

[`sdk/`](sdk/) holds the TypeScript SDK, published to npm as [`@stabilitynexus/gluon-ergo-sdk`](https://www.npmjs.com/package/@stabilitynexus/gluon-ergo-sdk). It reads reactor boxes, computes quotes and analytics, and builds EIP-12 transactions for Gluon v3 reactors on Ergo mainnet.

- **Build**: `npm run build:sdk` compiles the SDK TypeScript source to `sdk/dist/`.
- **Packaging check**: `npm run check:sdk` validates package structure, browser safety, and type declarations.
- **Releases**: Automated via `.github/workflows/release.yml` on pushing a tag matching `sdk-v<version>` (where the tag matches `sdk/package.json`), using npm trusted publishing (OIDC with provenance).

### License

The SDK in `sdk/` is licensed under the [GNU General Public License v3.0](sdk/LICENSE) (`GPL-3.0-only`). As decided in the repository cleanup (decision CD4), this license applies to the SDK package only; the repository as a whole has no global license file.
---

## Security & Audits

[`audits/`](audits/) holds the reports. They name the contract by its former path, `GluonWBoxGuardScript.es`, at the
commits they reviewed:

- [`AuditReport-KB.pdf`](audits/AuditReport-KB.pdf): audit by Krasavice Blasen (November 2024);
- [`GluonWBoxGuardScript-refactoring-audit-2026-10.md`](audits/GluonWBoxGuardScript-refactoring-audit-2026-10.md):
  audit of the October 2026 refactoring of the contract;
- [`GluonWBoxGuardScript-remediation-review-2026-10.md`](audits/GluonWBoxGuardScript-remediation-review-2026-10.md):
  review of the remediations of that audit's findings;
- [`GluonWBoxGuardScript-refactoring-audit-bbc77a8.md`](audits/GluonWBoxGuardScript-refactoring-audit-bbc77a8.md):
  audit of a later refactoring commit, `bbc77a8`;
- [`sec_review_2026-10-11T02-51-42Z_369fb81.md`](audits/sec_review_2026-10-11T02-51-42Z_369fb81.md) and
  [its remediations](audits/sec_review_2026-10-11T02-51-42Z_369fb81_remediations.md): security review of v3.

[`skills/`](skills/) holds the security-review and security-remediation skills for coding agents.

> Found a vulnerability? Please **do not open a public issue**. Contact the Stability Nexus team privately via [Discord](https://discord.gg/YzDKeEfWtS) or [Telegram](https://t.me/StabilityNexus).

---

## Contributing

We welcome contributions of all kinds! To contribute:

1. Fork the repository and create your feature branch (`git checkout -b feature/AmazingFeature`).
2. Commit your changes (`git commit -m 'Add some AmazingFeature'`).
3. Run the development workflow commands to ensure code quality:
   - `npm run typecheck`
   - `npm test`
   - `npm run verify`
4. Push your branch (`git push origin feature/AmazingFeature`).
5. Open a Pull Request for review.

If you encounter bugs, need help, or have feature requests:

- Please open an issue in this repository providing detailed information.
- Describe the problem clearly and include any relevant logs or screenshots.

We appreciate your feedback and contributions!

© 2025 The Stable Order.
