# Gluon v3 migration: the SDK in the contracts repository, and a v3-only static UI on GitHub Pages

*Plan for the maintainers of the contracts repository
([StabilityNexus/Gluon-Ergo-Contracts](https://github.com/StabilityNexus/Gluon-Ergo-Contracts)) and of
[StabilityNexus/Gluon-Ergo-UI](https://github.com/StabilityNexus/Gluon-Ergo-UI), written on 2026-10-13. It is meant to
be executed step by step by a developer or a coding agent, and it is self-contained. The maintainers chose it over two
other routes, an SDK in a repository of its own and a library inside the UI, because it keeps one copy of the code that
must match the contract, next to the contract and its tests; those plans and their comparison are in this repository's
history (commit `7f60beb`). The npm setup it relies on is in `references/npm-guide.md`.*

## 0. Scope, and how to use this plan

**Scope.** This plan does two things:
1. **Part A, in the contracts repository:** it turns the TypeScript there, which already implements every v3 rule and
   is tested against the contract, into the **SDK 2.0.0**, published from that repository as a new npm package that the
   maintainers own, **`@stability-nexus/gluon-ergo-sdk`**.
2. **Part B, in the UI repository:** it builds a new version of the UI on that package, as a **static, client-side site
   hosted on GitHub Pages**. Gluon Gold v3 and Gluon Dollar v3 each get their own deployment, in their own fork of the
   repository, and the two forks differ only in their env/config files.

- **The SDK supports the v3 contract only, on Ergo mainnet only**, with no backward compatibility with 1.x, in its API or
  in its behavior. It is licensed GPL-3.0 (`"license": "GPL-3.0-only"`).
- **The SDK repository and the old package are not changed.** `StabilityNexus/Gluon-Ergo-SDK` and `gluon-ergo-sdk`
  1.0.0 to 1.0.3 stay as they are, and nothing in 1.x is fixed, changed or deprecated. The SDK 2.x is a new package,
  `@stability-nexus/gluon-ergo-sdk`, in the npm organization `stability-nexus`; its versions start at 2.0.0, continuing
  the 1.x line under the new name.
- **The new UI knows v3 deployments and SDK 2.x only**: no v2 code path, no use of SDK 1.x, no testnet support.
- **The legacy UI is the `mainnet` branch** of the UI repository. It keeps being deployed to Vercel by its existing
  project, and nothing in this plan fixes or improves it.
- **No server code, no secrets, no price feed and no UI fee** in the new UI (UD6, UD9, UD10).
- **No testnet.** The first mainnet bootstrap, with a small seed, is the first live test.
- Where this plan describes what the current code does, it says what replaces it. It never says what to change in the
  legacy UI or in SDK 1.x.

**How to use this plan.**
- There are two repositories and two sequences of steps:
  - **Part A** (§4 to §8), in the contracts repository: steps **A1 to A7**. Each is a branch merged into `main` once its
    checks pass. (The maintainers of that repository merge branches locally; open pull requests instead if your
    workflow uses them.)
  - **Part B** (§9 to §15), in the UI repository: steps **B1 to B7**, each a pull request into `main` of upstream.

  §16 says how they interleave: the UI starts against a fake client, and depends on a published pre-release from A6 on.
- **npm.** The package's one-time setup (the organization, the first version, the trusted publisher) is in
  `references/npm-guide.md`; step A6 follows it.
- **Starting points.** The contracts repository at `main` once its testnet tooling is removed (branch
  `cleanup/remove-testnet`); the file sizes and the test count below are from that state. The UI repository at `main`,
  `e4e30ab`, which Step 0 (§9) tags `pre-v3`.
- Facts marked with a date were checked on that date.

## 1. Summary

The Gluon reactor contract is now at v3. Two v3 reactors will be bootstrapped on Ergo mainnet from the contracts
repository: Gluon Gold, pegged to one gram of gold, and Gluon Dollar, pegged to one US dollar. v3 adds two things users
will see:
- a **healthy range** for the fusion ratio: outside it, fission, fusion and transmutations are refused;
- **Adjust Peg**, a transaction anyone can submit while the reactor is outside its healthy range. It moves the peg
  factor by 1 % per transaction until the reactor is healthy again, and the neutron's target price is the oracle price
  times the current peg factor.

The contracts repository already holds TypeScript for every v3 rule: the contract's arithmetic (`lib/math.ts`), the
register layout (`lib/reactor.ts`), builders for all six transactions (`lib/transactions.ts`) and a node client that
copes with a stale blockchain index and with mempool races (`lib/node.ts`). Its 375 tests execute those builders against
the real contract on a simulated chain. This plan:
- makes that code the source of the SDK 2.0.0, **`@stability-nexus/gluon-ergo-sdk`**, by moving it into a package
  folder, `sdk/`, rather than copying it, and adds what a front end needs: bundled deployment records, node access that
  works in browsers, input selection, error codes and a client;
- keeps the contract, its tests, the deployment records and the SDK in one repository, so that a change to any of them
  is tested against the others in one CI run;
- publishes the SDK from that repository's CI, with npm trusted publishing;
- builds the new UI on the SDK: a static site that reads the chain from the visitor's browser, shows the reactor's
  health, gates trading accordingly and offers Adjust Peg, published from two forks of the UI repository that differ
  only in one env file.

## 2. Decisions

### 2.1 The SDK and the contracts repository

- **CD1. The SDK is the contracts repository's code, moved, not copied.** `lib/math.ts`, `lib/reactor.ts`,
  `lib/transactions.ts` and `lib/node.ts` move to `sdk/src/` with `git mv`, so that their history follows them, and the
  scripts and tests import them from there. There is one implementation of each v3 rule, and the contract tests
  exercise it.
- **CD2. One package, in a workspace.** `sdk/` is an npm workspace of the repository, with its own `package.json`,
  build, `README.md`, `CHANGELOG.md` and `LICENSE`. The rest of the repository (the contract, the tooling, the tests and
  the records) stays an unpublished project.
- **CD3. A new package, owned by the maintainers.** The SDK is `@stability-nexus/gluon-ergo-sdk`, in the npm
  organization `stability-nexus`: v3 only, mainnet only, with no 1.x API. Its versions start at 2.0.0 and follow
  `gluon-ergo-sdk` 1.x, which stays on npm and in its own repository, untouched. `references/npm-guide.md` sets up the
  organization, the package and its trusted publisher.
- **CD4. GPL-3.0 for the package only.** `sdk/LICENSE` holds the GPL-3.0 text and `sdk/package.json` declares
  `GPL-3.0-only`. The repository as a whole keeps having no license file, as decided in its cleanup (decision D4 of
  `cleanup.md`); the maintainers confirmed this split on 2026-10-13. One license for the whole repository would be a
  separate decision: it would mean moving `sdk/LICENSE` to the root.
- **CD5. Browser-safe by construction.** The package's source uses no Node module, no `Buffer` and no `process`. Its
  build compiles it without Node's type definitions, so any such use is a compile error (tried on 2026-10-12 with
  TypeScript 7.0.2, the repository's version).
- **CD6. Tooling stays out of the package.** `lib/bootstrap.ts`, `cli.ts`, `compile.ts`, `config.ts`, `records.ts`,
  `signer.ts` and `simulate.ts` stay in `lib/`. They may import the SDK's source; the SDK never imports them.
- **CD7. One implementation for the scripts too.** The scripts use the SDK's box lookup, oracle-age check and input
  selection, and the versions in `lib/cli.ts` (`uniqueBox`, `requireFreshOracle`, `selectFunds`) are removed.
- **CD8. Deployments are generated from the records.** `sdk/src/deployments/data.ts` is generated from
  `deployments/mainnet.json` by a script of the repository, committed, regenerated by the bootstrap script and checked
  by CI, as `Deployments.md` is today.
- **CD9. CI publishes, from a tag.** A tag `sdk-v<version>` runs a release workflow that tests, builds, checks and
  publishes the package with npm trusted publishing (OIDC). No npm token is stored anywhere. Only the first version is
  published by hand, because npm can add a trusted publisher only to a package that exists (`references/npm-guide.md`).
- **CD10. The public API is that of §5.4, and no more.** The repository's scripts and tests import modules from
  `sdk/src` directly, so internal functions, such as the low-level transaction builders and the node client, need not
  be exported.

### 2.2 The UI

- **UD1. One build, one deployment.** `NEXT_PUBLIC_GLUON_DEPLOYMENT` names a deployment bundled in the SDK. Nothing in
  the UI chooses between deployments.
- **UD2. One SDK client, in a React context.** `GluonProvider` owns the client and the latest chain state and snapshot.
  It polls every 30 s while the tab is visible, refreshes on demand and after every submitted transaction, and keeps
  the last good state while a refresh fails. Components read it with `useGluon()`; nothing else instantiates the SDK.
- **UD3. Everything on-chain comes from the deployment:** token ids, decimals, tickers (the token's EIP-4 name),
  contract and oracle. Presentation (colors, labels) is in code, keyed by deployment name (§12.3).
- **UD4. The SDK computes; the UI formats.** Quotes and transactions come from `client.quote*` and `client.build*`.
  Amounts stay `bigint` in base units until display.
- **UD5. The UI never offers what the contract would reject:** health, oracle age and the buyback box gate every action.
- **UD6. Static and client-side only.** The site is exported to plain files (`output: "export"`) and published on
  GitHub Pages. All data comes from the visitor's browser: the Ergo node and the explorer API, both of which allow
  browser requests. There is no server code and there are no secrets.
- **UD7. Fail fast.** The build validates the configuration, so a fork with a missing or wrong `.env.production` fails
  its build instead of publishing the wrong reactor.
- **UD8. The wallet must be on mainnet.**
- **UD9. No external price feed.** The site shows no USD value of ERG, and nothing depends on CoinGecko or its API key.
  Removed: `pages/api/getErgPrice.ts`, the `COINGECKO_API_KEY` variable, the `MyStats` portfolio component, the ERG
  price card and its polling in `GluonStats`, and the USD amounts of the fee panel. Prices stay in ERG and in the
  pegged asset, from the oracle.
- **UD10. No UI fee.** The contract allows an optional 0.4 % fee for the interface that builds a transaction. These
  sites charge none: the UI creates its SDK client without `uiFeeAddress`, so quotes have `fees.ui = 0` and no
  transaction has an interface-fee output. There is no fee address to configure.

## 3. Background

### 3.1 Deployments the SDK serves

| Deployment | Recorded when |
|---|---|
| `gluon-gold-v3` | the v3 Gold reactor is bootstrapped |
| `gluon-dollar-v3` | the v3 Dollar reactor is bootstrapped |

Each deployment gets a record in `deployments/mainnet.json` of the contracts repository when its bootstrap script runs
with `--submit`. Until then, the SDK is developed and tested against synthetic records and the simulated chain (§8).

`deployments/mainnet.json` also holds the records of reactors that are not v3: `gluon-gold-pre-v2`, `gluon-gold-v2`,
`gluon-dollar-pre-v3-a` and `gluon-dollar-pre-v3-b`. The SDK does not serve them. The script that generates its data
(A2) drops every record whose genesis R9 is not a `(Long, Long)` pair, which is what tells a v3 reactor from every
earlier one.

### 3.2 The v3 contract in brief

All arithmetic is integer arithmetic on BigInt, with truncating division, in the contract's order of operations.
`ONE = 10⁹`. Appendix A has every formula; the contracts repository's README ("The Contract") has the full layout.

- **Price.** The target price of one neutron is `oracle R4 × peg factor / 10⁹`, with the peg factor in R9. The initial
  peg factor fixes the unit: 0.001 for the GoldPool, which quotes nanoERG per kg of gold, so that one neutron is one
  gram; 1 for the USD Pool, which quotes nanoERG per USD.
- **Healthy range.** Fission, fusion and beta decays need the fusion ratio `q` to be within [0.5, 0.98]. Beta decays
  must also leave it there. Prices use `qNorm = min(q / (q + 1 − 0.99), q)`.
- **Adjust Peg.** Anyone may submit it while `q` is outside the healthy range. It multiplies the peg factor by 99/100
  if `q > 0.98` and by 101/100 if `q < 0.5`, and changes nothing else.
- **Update Treasury.** The current treasury, a single-signature key in R5, replaces R5 with another key. The
  transaction must have a box of the treasury's address as `INPUTS(1)`.
- **Oracle.** Fission, fusion, beta decays and Adjust Peg require the oracle pool box as data input 0, holding
  `_OraclePoolNFT` as its first token and created fewer than 35 blocks before the block that includes the transaction.
- **Positions.** Every transaction spends the reactor as `INPUTS(0)` and recreates it as `OUTPUTS(0)`. It has the
  oracle pool box as data input 0 and at least two inputs: the contract reads data input 0 and `INPUTS(1)` in every
  transaction, whether or not the transaction uses them.
- **Fees.** Fission and fusion move a principal value of ERG; beta decays move the value of the protons or neutrons.
  Up to three fees are paid in separate outputs after `OUTPUTS(1)`, in this order, and a fee of zero has no output:
  oracle fee 0.1 % (beta decays only), dev fee 0.5 % decaying to zero, UI fee 0.4 % (only if the reactor input sets
  context variable 0 to a key). Each output receives exactly its fee plus `_MinFee`.

### 3.3 Oracle buyback top-ups

A beta decay that owes an oracle fee spends the oracle's buyback box as its **last input** and recreates it as
`OUTPUTS(2)` with the same script and first token and with the fee plus `_MinFee` added to its value. Oracle-core v2
buyback contracts, such as the USD oracle's DORT buyback (`dcce07af…`), also require:
- context variable 0 = `Int 1` (serialized `0402`) on the buyback input;
- R4 of the new box = `Coll[Byte]` holding the id of the box it replaces.

Without both, they reject the transaction. The contracts repository verified this against the live box
(`test/oracle-fee.test.ts`).

## 4. Part A, the contracts repository: what is there today

Everything the SDK must do is already implemented once, for the deployment scripts, and most of it is tested against
the contract:

| File today | Lines | What it holds | In this plan |
|---|---|---|---|
| `lib/math.ts` | 272 | BigInt mirror of the v3 arithmetic, in the contract's order of operations | moves to `sdk/src/math.ts` (A1); gains `crashCushion` and `protonLeverage` (A3) |
| `lib/reactor.ts` | 72 | reading and writing the reactor's registers; the oracle price | moves to `sdk/src/reactor.ts` (A1); `readReactor` can check a deployment's tree and tokens, and `oraclePrice` moves to `oracle.ts` (A3) |
| `lib/transactions.ts` | 177 | builders for all six transactions: output layout, fee outputs, buyback top-up, UI-fee variable, position assertions | moves to `sdk/src/transactions.ts` (A1); otherwise unchanged: the client calls it (A5) |
| `lib/node.ts` | 164 | node client: mempool-aware box lookups checked against the UTXO set; transaction check and submission | moves to `sdk/src/node.ts` (A1); becomes browser-safe and gains the unique-box lookup (A4) |
| `lib/cli.ts` | 88 | helpers of the scripts | stays; its box lookup, oracle-age check and fund selection move to the SDK (A3, A4) |
| `lib/simulate.ts` | 91 | `consensusIssues()`, and the trades the bootstrap simulates before it deploys | stays |
| `lib/bootstrap.ts`, `compile.ts`, `config.ts`, `records.ts`, `signer.ts` | 739 | deployment tooling | stay |
| `test/*.ts` | 1 185 | 375 tests: most execute transactions against the real contract on Fleet's mock chain | stay; they import the SDK's source from A1 on; each step adds tests |
| `deployments/mainnet.json`, `Deployments.md` | | the records of every reactor | the source of the SDK's bundled deployments (A2) |

What the SDK needs and the repository does not have yet: deployment records as a module that browsers can load, node
access that works in every browser, token-aware input selection, error codes a UI can show, quotes and the statistics
of the dashboard, and a client that ties them together.

## 5. Part A: target design of the SDK

### 5.1 Layout

```
contracts/Gluon.es        the contract
config/                   public deployment parameters
deployments/mainnet.json  the records of every reactor; the SDK's deployments are generated from it
lib/                      deployment tooling, never published: bootstrap, cli, compile, config, records, signer, simulate
scripts/                  bootstrap, adjust-peg, update-treasury, compile, verify, render-deployments, and new:
                          check-sdk (A1), build-sdk-data (A2), smoke (A5)
test/                     contract tests and SDK tests; they import ../sdk/src/*.ts
sdk/                      the package @stability-nexus/gluon-ergo-sdk, an npm workspace
  package.json  README.md  CHANGELOG.md  LICENSE  tsconfig.build.json
  src/
    index.ts              public exports (§5.4)
    types.ts              GluonDeployment, ChainState, ProtocolSnapshot, ActionQuote, …
    errors.ts             GluonError and its codes
    deployments/
      fromRecord.ts       parses one record of deployments/mainnet.json
      data.ts             generated by scripts/build-sdk-data.ts: the v3 records
      index.ts            DEPLOYMENTS, getDeployment()
    math.ts               from lib/math.ts
    reactor.ts            from lib/reactor.ts
    oracle.ts             oracle price and age
    node.ts               from lib/node.ts: node client and box lookups
    select.ts             input selection
    transactions.ts       from lib/transactions.ts
    client.ts             GluonClient
  dist/                   build output, ignored by git
```

- **The direction of imports.** `lib/`, `scripts/` and `test/` may import `sdk/src/*.ts`. Nothing in `sdk/src` imports
  from outside `sdk/src`, except the packages it declares as dependencies. The build enforces the Node part of this
  (CD5), and `scripts/check-sdk.ts` checks the rest (§5.2).
- **TypeScript that Node runs directly.** The repository runs its scripts with Node 24's type stripping, so `sdk/src`
  follows the rules `lib/` follows today: relative imports carry the `.ts` extension, and only erasable TypeScript
  syntax is used (the root `tsconfig.json` sets `allowImportingTsExtensions` and `erasableSyntaxOnly`).
  `sdk/package.json` has `"type": "module"`, which Node needs to run `sdk/src/*.ts` as ES modules.

### 5.2 The package

`sdk/package.json`, as A1 creates it (A4 adds `json-bigint`):

```json
{
  "name": "@stability-nexus/gluon-ergo-sdk",
  "version": "2.0.0-next.0",
  "description": "Read, quote and build transactions for Gluon v3 reactors on Ergo",
  "license": "GPL-3.0-only",
  "type": "module",
  "sideEffects": false,
  "exports": {
    ".": { "types": "./dist/index.d.ts", "import": "./dist/index.js" },
    "./package.json": "./package.json"
  },
  "types": "./dist/index.d.ts",
  "files": ["dist", "CHANGELOG.md"],
  "repository": {
    "type": "git",
    "url": "git+https://github.com/StabilityNexus/Gluon-Ergo-Contracts.git",
    "directory": "sdk"
  },
  "publishConfig": { "access": "public", "tag": "next" },
  "scripts": { "build": "tsc -p tsconfig.build.json" },
  "dependencies": {
    "@fleet-sdk/common": "0.10.0",
    "@fleet-sdk/core": "0.12.0",
    "@fleet-sdk/serializer": "0.11.0"
  }
}
```

- **`repository`** names the repository that runs the release workflow, with `"directory": "sdk"`: npm requires
  `repository.url` to match the repository that publishes. If the repository moves to another organization, update it
  and the trusted publisher (`references/npm-guide.md` §10).
- **Dependencies** are the packages that `sdk/src` imports, at exact versions, the same ones the root `package.json`
  uses. `@fleet-sdk/common` is imported for types only, but the emitted declarations name it, so consumers need it
  installed. npm always packs `package.json`, `README.md` and `LICENSE`; `files` adds the rest.
- **`publishConfig`**: `access` is `public`, because a scoped package is private unless published with public
  access, and `tag` is `next`, so that pre-releases never take `latest` (§7).

`sdk/tsconfig.build.json`:

```json
{
  "extends": "../tsconfig.json",
  "compilerOptions": {
    "noEmit": false,
    "rootDir": "src",
    "outDir": "dist",
    "declaration": true,
    "rewriteRelativeImportExtensions": true,
    "target": "ES2022",
    "lib": ["ES2023", "DOM"],
    "types": []
  },
  "include": ["src"]
}
```

It keeps the root's strictness and changes what a published build needs: JavaScript and declarations in `dist/`,
relative `./x.ts` imports rewritten to `./x.js` in the JavaScript, the DOM library for `fetch`, and **no Node types**
(`"types": []`), which makes any use of a `node:` module, `Buffer` or `process` a compile error. This configuration was
tried on 2026-10-12 against the four modules A1 moves, with TypeScript 7.0.2: it emits them without changes to their
source, and it rejects a file that uses `node:fs` or `Buffer`. The emitted declarations keep `./x.ts` specifiers;
TypeScript 5.9.3 and 7.0.2 resolve them for a consumer, with `moduleResolution` set to `bundler` or `nodenext` and
`skipLibCheck` off (also tried on 2026-10-12).

**Root `package.json`:** `"workspaces": ["sdk"]`, and two scripts:
`"build:sdk": "npm run build -w @stability-nexus/gluon-ergo-sdk"` and `"check:sdk": "node scripts/check-sdk.ts"`.

**`scripts/check-sdk.ts`** runs after the build, in CI and in the release workflow. It fails unless:
1. `sdk/dist/index.js` and `sdk/dist/index.d.ts` exist;
2. no file in `sdk/dist` imports a `node:` module or names `Buffer` or `process`;
3. every package that a file in `sdk/dist` imports, in `.js` and `.d.ts` files, is in the `dependencies` of
   `sdk/package.json`, at the same exact version as in the root `package.json` when the root has it too;
4. `npm pack --dry-run --json -w @stability-nexus/gluon-ergo-sdk` lists only `package.json`, `README.md`, `LICENSE`,
   `CHANGELOG.md` and files under `dist/`;
5. the packed tarball, installed with its dependencies into an empty temporary folder, can be imported by Node
   (`import("@stability-nexus/gluon-ergo-sdk")`), and a small TypeScript file that uses the package's types compiles
   against it.

Check 5 needs the npm registry; the others work offline.

### 5.3 Design requirements

Each requirement follows from the v3 contract or from how the chain behaves. Most are met by the code that A1 moves.

| # | Requirement | Why | Today | Step |
|---|---|---|---|---|
| R1 | Reactor ErgoTree, token ids, oracle and buyback NFTs, `_MinFee`, buyback contract and initial peg factor come from a deployment record. Nothing deployment-specific is a module constant. | Several reactors, each with its own constants | Met: the builders take everything from the boxes and their arguments; the records exist | A2 bundles the records |
| R2 | v3 arithmetic, in the contract's order of operations: peg-adjusted price, `qNorm`, φ₁ = 1, healthy range, Adjust Peg (Appendix A) | The contract checks amounts to the unit | Met: `math.ts` | A3 adds two derived values |
| R3 | R9 is a `(Long, Long)` pair, written and read as such (Appendix B) | v3 box layout | Met: `reactor.ts` | |
| R4 | Amounts and fees are `bigint` end to end | A fee one nanoERG off makes the contract reject the transaction; JavaScript numbers lose precision above 2⁵³ | Met | |
| R5 | A fee output exists only if its fee is above zero, in the order oracle, dev, UI. The buyback box is spent only if the oracle fee is above zero. The dev fee goes to the treasury in R5. The UI fee sets context variable 0 on the reactor input. | Output positions are checked by index | Met: `transactions.ts`, tested | |
| R6 | The buyback top-up of §3.3 | Oracle-core v2 buyback contracts | Met, tested against the live DORT buyback box (`test/oracle-fee.test.ts`) | |
| R7 | Box lookups are verified against the UTXO set and the mempool, and an ambiguous result is an error | The node's blockchain index can list spent boxes as unspent, and the oracle pool box can have competing successors in the mempool (both seen on 2026-10-11) | Met for Node: `node.ts` and `uniqueBox` of `lib/cli.ts` | A4: in browsers, with error codes |
| R8 | Scripts see `HEIGHT = height + 1`; outputs get creation height `height` | Volume buckets, `lastBucketBlock` and the oracle-age check use `HEIGHT` | Met | |
| R9 | The reactor is `INPUTS(0)`, the buyback box is the last input, and only the inputs a transaction needs are spent | Positions are checked by index; a wallet's UTXOs should not all be consolidated | Positions met and asserted; the scripts select ERG only (`selectFunds`) | A4: `selectInputs`, for ERG and tokens |
| R10 | Addresses are encoded and checked for mainnet; the treasury and UI-fee addresses are single-signature | A wallet can be switched to another network, and would then sign transactions the chain rejects | Met in the scripts (`addressProblem` of `lib/config.ts`) | A5: in the client |
| R11 | Health and oracle age are checked before a transaction is built, with errors a UI can show | The contract only answers "rejected" | Partly: the scripts check the oracle's age, and the builders throw plain errors | A3, A5: `GluonError` codes |
| R12 | The tests execute the real contract | The contract is the specification | Met: 375 tests on Fleet's mock chain | each step adds tests |

### 5.4 Public API

`Box`, `Amount` and `EIP12UnsignedTransaction` are Fleet's types (`@fleet-sdk/common`).

```ts
// deployments
export interface TokenInfo { id: string; name: string; decimals: number; emission: bigint }
export interface GluonDeployment {
  name: string;                    // "gluon-gold-v3", "gluon-dollar-v3"
  status: "live" | "superseded" | "unused"; // informational
  peg: string;                     // "1 gram of gold", "1 USD"
  ergoTree: string;                // reactor contract
  address: string;
  tokens: { nft: TokenInfo; neutron: TokenInfo; proton: TokenInfo };  // token name = ticker (EIP-4 R4)
  minFee: bigint;                  // _MinFee
  oraclePoolNft: string;           // _OraclePoolNFT
  oracleBuybackNft: string;        // _OracleBuybackNFT
  oracleFeeTree: string;           // _OracleFeePk: ErgoTree of the buyback contract
  initialPegFactor: bigint;        // peg factor at genesis, ×10⁹: second element of the genesis R9
}
export const DEPLOYMENTS: readonly GluonDeployment[];   // every bundled v3 record
export function getDeployment(name: string): GluonDeployment;       // throws UNKNOWN_DEPLOYMENT
export function deploymentFromRecord(record: unknown): GluonDeployment; // throws INVALID_DEPLOYMENT

// client
export interface GluonClientOptions {
  deployment: GluonDeployment | string;  // a deployment, or the name of a bundled one
  nodeUrl: string;                 // HTTPS node serving the blockchain index API (/blockchain/…) and allowing browser requests
  uiFeeAddress?: string;           // single-signature address of the optional 0.4 % UI fee (the Gluon UI sets none)
  minerFee?: bigint;               // default 1_100_000n (Fleet's RECOMMENDED_MIN_FEE_VALUE)
  fetch?: typeof fetch;            // injectable, for tests
}

export interface ReactorState {    // the contents of a reactor box (Appendix B)
  value: bigint; neutrons: bigint; protons: bigint;       // value; tokens in the box
  totalNeutrons: bigint; totalProtons: bigint;            // R4
  treasury: string;                                       // R5: public key (hex)
  devFeeRepaid: bigint; maxDevFee: bigint;                // R6
  volumePlus: bigint[]; volumeMinus: bigint[];            // R7, R8
  lastBucketBlock: bigint; pegFactor: bigint;             // R9
}

export interface ChainState {
  height: number;                  // node height; transactions are built for HEIGHT = height + 1
  reactor: Box<bigint>;            // current reactor box, mempool-aware
  oracle: Box<bigint>;             // oracle pool box, mempool-aware
  buyback?: Box<bigint>;           // oracle buyback box, if one exists
  state: ReactorState;             // parsed reactor
}

export class GluonClient {
  readonly deployment: GluonDeployment;
  constructor(options: GluonClientOptions);
  fetchChainState(): Promise<ChainState>;

  snapshot(chain: ChainState): ProtocolSnapshot;
  quoteFission(chain: ChainState, erg: bigint): ActionQuote;
  quoteFusion(chain: ChainState, erg: bigint): ActionQuote;
  quoteBetaDecayPlus(chain: ChainState, protons: bigint): ActionQuote;
  quoteBetaDecayMinus(chain: ChainState, neutrons: bigint): ActionQuote;
  maxBetaDecayPlus(chain: ChainState): bigint;   // largest proton amount whose quote succeeds
  maxBetaDecayMinus(chain: ChainState): bigint;  // largest neutron amount whose quote succeeds
  quoteAdjustPeg(chain: ChainState): AdjustPegQuote;

  buildFission(chain: ChainState, args: { erg: bigint } & Funding): EIP12UnsignedTransaction;
  buildFusion(chain: ChainState, args: { erg: bigint } & Funding): EIP12UnsignedTransaction;
  buildBetaDecayPlus(chain: ChainState, args: { protons: bigint } & Funding): EIP12UnsignedTransaction;
  buildBetaDecayMinus(chain: ChainState, args: { neutrons: bigint } & Funding): EIP12UnsignedTransaction;
  buildAdjustPeg(chain: ChainState, args: Funding): EIP12UnsignedTransaction;
  buildUpdateTreasury(chain: ChainState, args: { newTreasuryAddress: string } & Funding): EIP12UnsignedTransaction;
}

export interface Funding {
  userBoxes: Box<Amount>[];        // the wallet's UTXOs (EIP-12 boxes from get_utxos())
  changeAddress: string;           // receives released tokens and change (EIP-12 get_change_address())
}

export interface ProtocolSnapshot {
  deployment: string;
  height: number;                  // node height; scripts see height + 1
  reserve: bigint;                 // box value − _MinFee, nanoERG: the total value locked
  neutronsInCirculation: bigint; protonsInCirculation: bigint;
  oraclePrice: bigint;             // R4 of the oracle box: nanoERG per oracle unit (per kg of gold, per USD)
  oracleAge: number;               // (height + 1) − the oracle box's creation height
  pegFactor: bigint;               // R9, ×10⁹
  initialPegFactor: bigint;
  pegDrift: number;                // pegFactor / initialPegFactor
  pegUnitPrice: bigint;            // nanoERG per peg unit as defined at launch: oraclePrice × initialPegFactor / 10⁹
                                   // (per gram of gold, per USD)
  targetPrice: bigint;             // nanoERG the contract targets per whole neutron: oraclePrice × pegFactor / 10⁹
  q: bigint;                       // fusion ratio, ×10⁹
  qNorm: bigint;                   // ratio the contract uses for prices, ×10⁹
  healthy: boolean;                // 0.5 ≤ q ≤ 0.98
  adjustPeg?: { direction: "down" | "up"; steps: number };  // present when unhealthy
  neutronPrice: bigint;            // nanoERG per whole neutron (10^decimals base units), at qNorm
  protonPrice: bigint;             // nanoERG per whole proton
  reserveRatio: number;            // percent: 100 / q
  protonLeverage?: number;         // q / (1 − q), while q < 0.99 (Appendix A)
  crashCushion: { untilDepeg: number; untilAdjustment: number }; // fractions, 0.25 = 25 % (Appendix A)
  volumes: { plus: bigint[]; minus: bigint[] }; // 14 buckets of 720 blocks, index 0 = the bucket that contains HEIGHT
  devFee: { repaid: bigint; max: bigint };
  treasuryAddress: string;
  betaDecaysAvailable: boolean;    // false when no buyback box exists
}

export interface ActionQuote {
  ergIn: bigint; ergOut: bigint;   // nanoERG into or out of the reactor, before fees
  neutronsIn: bigint; neutronsOut: bigint; protonsIn: bigint; protonsOut: bigint;
  principal: bigint;               // the value the fees are computed on, nanoERG
  fees: { dev: bigint; ui: bigint; oracle: bigint; outputsMinFee: bigint; miner: bigint; total: bigint };
                                   // outputsMinFee = _MinFee × number of fee outputs; total = the sum of all
  next: ReactorState;              // the reactor after the transaction
  qAfter: bigint;                  // fusion ratio after it, ×10⁹
}

export interface AdjustPegQuote {
  allowed: boolean;                // false when healthy
  direction?: "down" | "up";
  steps: number;                   // transactions needed to become healthy at the current oracle price
  nextPegFactor?: bigint;
}

// pure functions, for analytics over historical boxes (the UI's history and fee charts)
export { readReactor, reactorRegisters } from "./reactor.ts";
export { oraclePrice, oracleAge } from "./oracle.ts";
export * as math from "./math.ts";  // snapshot, fees, fission, fusion, betaDecayPlus/Minus, adjustPeg, pegAdjustments, shiftVolumes

export class GluonError extends Error { readonly code: GluonErrorCode }
export type GluonErrorCode =
  | "REACTOR_UNHEALTHY"            // fission, fusion or beta decay while q is outside [0.5, 0.98]
  | "WOULD_LEAVE_HEALTHY_RANGE"    // a beta decay that would end outside the range
  | "REACTOR_HEALTHY"              // Adjust Peg while healthy
  | "ORACLE_STALE"                 // oracle box 35 or more blocks old at HEIGHT
  | "BOX_AMBIGUOUS"                // several unspent boxes hold the token (competing mempool updates): retry after the next block
  | "BOX_NOT_FOUND"
  | "BUYBACK_UNAVAILABLE"          // a beta decay owes an oracle fee but no buyback box exists
  | "AMOUNT_TOO_SMALL"             // the transaction would move no tokens
  | "AMOUNT_TOO_LARGE"             // the reactor cannot pay out or release that amount
  | "INSUFFICIENT_FUNDS"           // the wallet's boxes cannot cover the amounts and fees
  | "INVALID_ADDRESS"              // not a mainnet address, or not single-signature where that is required
  | "UNKNOWN_DEPLOYMENT"           // no bundled deployment with that name
  | "INVALID_DEPLOYMENT"           // a record that is malformed or not a v3 reactor
  | "NODE_ERROR";                  // the node did not answer, or answered with an error
```

A `ChainState` is a plain object, so callers can also build one from historical boxes. Nothing else is exported
(CD10): the transaction builders of `transactions.ts`, the node client and the input selection are internal, and
the repository's scripts and tests import them from `sdk/src`.

### 5.5 Rules the implementation must respect

- **Heights.** `ChainState.height` is the node's current height. Everything the contract evaluates with `HEIGHT` uses
  `height + 1`: bucket shifts, `lastBucketBlock` and the oracle-age check. Outputs get creation height `height`.
- **Lookups.** Find the reactor, oracle and buyback boxes as `lib/node.ts` and `uniqueBox` of `lib/cli.ts` do today
  (A4 moves both into the SDK):
  1. Ask the blockchain index for unspent boxes holding the token, with `includeUnconfirmed=true&excludeMempoolSpent=true`.
  2. Keep only the boxes that `POST /utxo/withPool/byIds` also returns: this drops stale index entries.
  3. Drop boxes that `GET /transactions/unconfirmed/inputs/byBoxId/{id}` reports as spent in the mempool.
  4. Require the token to be the box's first token. For the reactor and the buyback, also require the deployment's
     ErgoTree (`ergoTree`, `oracleFeeTree`).
  5. Require exactly one box. Several mean competing mempool updates: throw `BOX_AMBIGUOUS`. None: `BOX_NOT_FOUND`
     (for the buyback box, leave `buyback` undefined instead).

  The user's own boxes come from the wallet, not from these lookups. The client runs in browsers, so step 2 is a
  cross-origin POST with a JSON body, which the browser preflights. Both public nodes tested on 2026-10-12
  (`node.sigmaspace.io`, `node.ergopool.io`) allow it (`access-control-allow-origin: *`, `content-type` allowed). §15.6
  has a command to check any other node.
- **Inputs.** Select the user's inputs with the `selectInputs` function of `sdk/src/select.ts`, then add them to the
  transaction builder with `ensureInclusion`, in this fixed order: the reactor, the selected user boxes, the buyback box
  (last). Fleet places boxes it selects itself after the ones marked `ensureInclusion` (`BoxSelector.select` in
  `@fleet-sdk/core` 0.12), which would move the buyback box ahead of the user's boxes. `transactions.ts` asserts
  both positions; keep those assertions.
  `selectInputs(userBoxes, { nanoErgs, tokens }, options?)`:
  - for each required token, take the boxes holding it, largest amount first, until it is covered;
  - then add boxes by largest value until the ERG requirement (outputs plus miner fee) is covered;
  - for Update Treasury, take boxes of the treasury's address first;
  - leave either no change at all, or change worth at least `SAFE_MIN_BOX_VALUE` (1 000 000 nanoERG, enough for a
    change box with a few dozen tokens), adding boxes until it is;
  - throw `INSUFFICIENT_FUNDS` if the requirements cannot be met.

  Fleet 0.12 cannot fix the change itself: with every input fixed by `ensureInclusion` it has no box to add, and it
  turns an ERG-only change below the minimum box value into an output that nodes reject (`consensusIssues()` reports it).
- **Fees.** Use `fees()` of `math.ts` exactly. Create a fee output only when its fee is above zero. Spend the
  buyback box only when the oracle fee is above zero. Read the treasury from R5. Set the UI-fee variable only when
  `uiFeeAddress` is set.
- **Health.** Refuse fission, fusion and beta decays while unhealthy (`REACTOR_UNHEALTHY`). Refuse a beta decay whose
  `qAfter` is outside [0.5, 0.98] (`WOULD_LEAVE_HEALTHY_RANGE`). `maxBetaDecayPlus` and `maxBetaDecayMinus` search for
  the largest amount whose quote succeeds, within what the reactor can release (`AMOUNT_TOO_LARGE` otherwise).
- **Oracle age.** Throw `ORACLE_STALE` when `height + 1 − oracle.creationHeight` is 35 or more, or negative. This
  applies to every transaction except Update Treasury.
- **Mainnet only.** Encode and decode addresses for Ergo mainnet. Reject a UI-fee, treasury or change address that is
  not a mainnet address, and a UI-fee or treasury address that is not P2PK (`INVALID_ADDRESS`). The constructor
  validates `uiFeeAddress`.

## 6. Part A: work plan

Each step is a branch of the contracts repository, merged into `main` when these pass: `npm run typecheck`, `npm test`
and `npm run verify`, and, from A1 on, `npm run build:sdk` and `npm run check:sdk`. No step changes what a deployment
script does, unless the step says so. Before any real deployment, the bootstrap's dry run (`node scripts/bootstrap.ts
<config>`, without `--submit`) remains the check, as the repository's README says.

### A1: The package, and the move

1. **Move the four modules**, with their history:

   ```sh
   mkdir -p sdk/src
   git mv lib/math.ts lib/reactor.ts lib/transactions.ts lib/node.ts sdk/src/
   ```
2. **Fix the imports.** In `lib/*.ts`, `"./math.ts"` becomes `"../sdk/src/math.ts"`, and the same for `reactor`,
   `transactions` and `node`. In `scripts/*.ts` and `test/*.ts`, `"../lib/math.ts"` becomes `"../sdk/src/math.ts"`, and
   the same for the other three. The moved files import each other with `./` and need no change. `npm run typecheck`
   reports any import that was missed.
3. **The package's files:** `sdk/package.json` and `sdk/tsconfig.build.json` as in §5.2; `sdk/LICENSE` with the text of
   https://www.gnu.org/licenses/gpl-3.0.txt; a short `sdk/README.md` ("2.0.0 is in development; it supports Gluon v3
   reactors only") and `sdk/CHANGELOG.md`, both completed in A6; and `sdk/src/index.ts`, which for now exports `math`
   (`export * as math from "./math.ts"`), `readReactor` and `reactorRegisters`. Later steps complete it to §5.4.
4. **The workspace.** In the root `package.json`, add `"workspaces": ["sdk"]` and the two scripts of §5.2. Run
   `npm install`, and commit the updated `package-lock.json`. In `tsconfig.json`, add `"sdk/src"` to `include`. In
   `.gitignore`, add `sdk/dist/`.
5. **`scripts/check-sdk.ts`**, with the checks of §5.2.
6. **CI.** In `.github/workflows/ci.yml`, after `npm run verify`, add `npm run build:sdk` and `npm run check:sdk`.
7. **README.** In "Repository Structure", add `sdk/` ("the SDK, published to npm as @stability-nexus/gluon-ergo-sdk"),
   and describe `lib/` as deployment tooling.

**Done when** the five checks pass, the 375 tests pass with no change other than their import paths, `sdk/dist` holds
the four modules and `index`, and `git log --follow sdk/src/math.ts` shows the file's earlier history.

### A2: Deployments as data, and the error type

1. `sdk/src/errors.ts`: `GluonError` and every code of §5.4.
2. `sdk/src/deployments/fromRecord.ts`: `deploymentFromRecord(record: unknown): GluonDeployment`. It reads a record of
   the shape of `DeploymentRecord` (`lib/records.ts`), without importing that file:

   | `GluonDeployment` field | Record field |
   |---|---|
   | `name`, `status`, `peg`, `ergoTree`, `address` | same names |
   | `tokens.{nft,neutron,proton}` | `tokens.*`, with `emission` as `BigInt` |
   | `minFee` | `BigInt(constants._MinFee)` |
   | `oraclePoolNft`, `oracleBuybackNft`, `oracleFeeTree` | `constants._OraclePoolNFT`, `constants._OracleBuybackNFT`, `constants._OracleFeePk` |
   | `initialPegFactor` | second element of the decoded `genesis.registers.R9` |

   It throws `INVALID_DEPLOYMENT` for a record whose R9 is not a pair, or that lacks a field.
3. `scripts/build-sdk-data.ts`: reads `deployments/mainnet.json`; keeps the v3 records, those whose
   `genesis.registers.R9` decodes (Fleet serializer `parse`) to a pair; keeps the fields that `fromRecord` reads; and
   writes `sdk/src/deployments/data.ts`, in a stable order and format, starting with the line
   `// Generated by scripts/build-sdk-data.ts from deployments/mainnet.json. Do not edit.`, then
   `export default [ … ];`. With `--check`, it writes nothing and fails if the file is out of date. While no v3
   deployment is recorded, the list is empty.
4. `sdk/src/deployments/index.ts`: `DEPLOYMENTS`, every record of `data.ts` parsed, and `getDeployment(name)`, which
   throws `UNKNOWN_DEPLOYMENT` for a name it does not know.
5. **Wire it into the tooling.** `scripts/bootstrap.ts` regenerates `data.ts` right after it writes `Deployments.md`, and
   its closing message lists the three files to commit. `scripts/verify.ts` fails when `data.ts` is out of date, as it
   does for `Deployments.md`.
6. **Tests** (`test/sdk-deployments.test.ts`):
   - a v3 record made the way `test/bootstrap.test.ts` makes one (`deploymentRecord()` of `lib/bootstrap.ts`) parses to
     the expected `GluonDeployment`, with its initial peg factor;
   - a record whose R9 is a plain `Long` throws `INVALID_DEPLOYMENT`, as does a record without `constants._MinFee`;
   - run on the real `deployments/mainnet.json`, the generator keeps none of its four records;
   - `getDeployment` of an unknown name throws `UNKNOWN_DEPLOYMENT`;
   - `--check` fails after a one-character change to `data.ts`.

**Done when** these tests pass and `npm run verify` checks `data.ts`.

### A3: Math, the reactor and the oracle

1. `math.ts`: add `crashCushion(q)` and `protonLeverage(q)` (Appendix A).
2. `reactor.ts`: `readReactor(box, deployment?)`. Given a deployment, it also checks the box's ErgoTree and token ids
   against it. The check that R5 holds a 33-byte key stays. Calls without a deployment, as in the scripts and the
   tests, behave as before.
3. `oracle.ts`: `oraclePrice(box)` moves here from `reactor.ts`. New: `oracleAge(oracle, height)`, which is
   `height + 1 − oracle.creationHeight`, and `requireFreshOracle(oracle, height)`, which throws `ORACLE_STALE` when the
   age is 35 or more, or negative. It replaces `requireFreshOracle` of `lib/cli.ts`: delete that one, and import the
   SDK's in `scripts/bootstrap.ts` and `scripts/adjust-peg.ts`. The message stays.
4. **Tests**, with hand-computed expectations: `protonLeverage` is 1.5 at q = 0.6 and undefined from q = 0.99; both
   crash cushions at q = 0.6, 0.98 and 1.2; `readReactor` throws for a wrong ErgoTree or a wrong token id; `oracleAge`
   and `requireFreshOracle` at ages 34, 35 and −1.

**Done when** these tests pass and `lib/cli.ts` no longer has `requireFreshOracle`.

### A4: Node access, box lookup and input selection

1. **`node.ts`, browser-safe:**
   - parse responses, and write request bodies, with `json-bigint` (`useNativeBigInt: true`), instead of the
     source-text reviver of `JSON.parse` and `JSON.rawJSON`, which recent JavaScript engines have and older browsers
     lack. Add `json-bigint` 1.0.0 to the dependencies of `sdk/package.json`, and `@types/json-bigint` to the root's
     development dependencies. (On 2026-10-12, json-bigint 1.0.0 parsed `12345678901234567890` to a `bigint` and wrote
     it back as a JSON number.)
   - The constructor takes `{ fetch?, timeoutMs? }`: an injectable `fetch` (default `globalThis.fetch`) and a timeout
     (default 25 s, with `AbortSignal.timeout`).
   - Every failure (a network error, a timeout, an HTTP error, a body that does not parse) throws `GluonError`
     `NODE_ERROR`, with the request and the node's message.
2. **Box lookup.** `uniqueBox` moves from `lib/cli.ts` to `node.ts` as `findUniqueBox(node, tokenId, what, ergoTree?)`,
   which follows the lookup rules of §5.5 and throws `BOX_AMBIGUOUS` when several boxes qualify and `BOX_NOT_FOUND` when
   none does, with today's messages. The scripts import it from the SDK.
3. **`select.ts`:** `selectInputs(userBoxes, { nanoErgs, tokens }, options?)`, as §5.5 ("Inputs") specifies, with an
   option to take the boxes of one address first. It replaces `selectFunds` of `lib/cli.ts`, which selects ERG only:
   `scripts/adjust-peg.ts` and `scripts/update-treasury.ts` call `selectInputs` with no tokens, and Update Treasury
   takes the treasury's boxes first.
4. **Tests:**
   - with an injected `fetch` that serves recorded node answers: a stale index entry is dropped; a box spent in the
     mempool is dropped; two live candidates give `BOX_AMBIGUOUS`; a box with the wrong ErgoTree is ignored; an HTTP
     error and a timeout give `NODE_ERROR`; amounts above 2⁵³ survive parsing and writing;
   - `selectInputs`: only the needed boxes are chosen, tokens first and then ERG; the boxes of the given address come
     first; change below `SAFE_MIN_BOX_VALUE` gets another box; `INSUFFICIENT_FUNDS` when the boxes run out.

**Done when** these tests pass, and `lib/cli.ts` no longer has `uniqueBox` or `selectFunds`.

### A5: The client

1. **`client.ts`:** `GluonClient` of §5.4, built on A2 to A4 and on the builders that A1 moved:
   - `fetchChainState()` fetches, in parallel, the height, the reactor (`findUniqueBox` with the deployment's NFT and
     ErgoTree), the oracle pool box (its NFT) and the buyback box (its NFT and `oracleFeeTree`; when there is none,
     `buyback` is undefined), then reads the reactor with `readReactor(reactor, deployment)`;
   - quotes come from `math.ts`, after the checks of §5.5 (health, oracle age, amounts), which throw `GluonError`;
   - each `build*` does the same checks, selects the inputs with `selectInputs`, and calls the matching builder of
     `transactions.ts` with a `TxContext` whose `recipient` is the caller's `changeAddress`, whose `uiFeeKey` is the
     public key of `uiFeeAddress` when one is set, and whose `inputs` are the selected boxes. The builders' output
     layout, fee order, buyback top-up and position assertions stay as they are;
   - `snapshot()` fills every `ProtocolSnapshot` field (Appendix A). `volumes` are shifted to the current `HEIGHT` with
     `shiftVolumes(…, nDays, 0n)`, so that a bucket more than a day old is no longer shown as today's.
     `betaDecaysAvailable` is false when `chain.buyback` is undefined.
2. **`types.ts` and `index.ts`:** the API of §5.4, and nothing more (CD10).
3. **Tests** (`test/sdk-client.test.ts`), on the mock chain of `test/helpers.ts`, for both pegs (a per-kg Gold oracle with
   peg factor 0.001, a per-USD Dollar oracle with peg factor 1):
   - each `build*` of the client is accepted by the real contract and passes `consensusIssues()`: fission, fusion, both
     beta decays (with and without an oracle fee), Adjust Peg in both directions, Update Treasury, and a client with a
     `uiFeeAddress`;
   - one test per error code;
   - `snapshot()` against hand-computed values; `maxBetaDecayPlus` and `maxBetaDecayMinus` return the largest amount
     whose quote succeeds;
   - input selection through the client: only the needed boxes are spent, the buyback box stays last, and no change box
     is below the minimum value.
4. **`scripts/smoke.ts <deployment>`:** the live, read-only check of §8.4.

**Done when** these tests pass. The existing contract tests keep testing the builders directly.

### A6: Documentation and the first pre-release

1. `sdk/README.md`: installation; choosing a deployment; the snapshot, quote and build flow; an EIP-12 signing example
   with Nautilus; the error codes. At the top: **2.x supports Gluon v3 reactors only; reactors of earlier contract
   versions need 1.x** (`npm install gluon-ergo-sdk@1`, the old package). Link the repository's README ("The
   Contract") and `Deployments.md`.
2. `sdk/CHANGELOG.md`: the first entry.
3. The repository's `README.md`: a short "SDK" section (what it is, `npm run build:sdk`, and how releases work, §7);
   the license note of CD4; and, in the introduction, the SDK is in `sdk/`, while the UI is in its own repository.
4. `.github/workflows/release.yml` (§7).
5. The one-time setup of §7, as `references/npm-guide.md` describes: the npm organization, the first version
   (`2.0.0-next.0`, published by hand), the trusted publisher, the lock-down and the GitHub environment.
6. Set `sdk/package.json` to `2.0.0-next.1`, commit, tag `sdk-v2.0.0-next.1`, and push the commit and the tag. The
   workflow publishes.

**Done when** `npm view @stability-nexus/gluon-ergo-sdk@next version` prints `2.0.0-next.1`, and the package's npm page
shows that version's provenance from this repository.

### A7: After each v3 deployment

1. `node scripts/bootstrap.ts <config> --submit` records the deployment and regenerates `Deployments.md` and
   `sdk/src/deployments/data.ts` (A2). Commit the three files, and tag the deployed revision, as the bootstrap's closing
   message says.
2. Add a client test (A5) that builds each transaction against a reactor box with the new deployment's exact ErgoTree.
3. Bump the version to the next pre-release, add a CHANGELOG entry, commit, tag `sdk-v<version>`, and push. The UI then
   moves to it (§16).
4. After the first mainnet v3 deployment passes the live checks of §8.5, release 2.0.0 (§7).

## 7. Part A: releases

- **Versions.** Pre-releases are `2.0.0-next.N`, under the `next` dist-tag (`publishConfig.tag`), and the UI installs
  them by exact version. `gluon-ergo-sdk` 1.x is another package, which nothing here affects.
- **2.0.0** is released when the first mainnet v3 deployment is recorded, bundled and live-checked (§8.5), and becomes
  `latest`: remove `"tag": "next"` from `publishConfig` in the commit that gets the tag `sdk-v2.0.0`
  (`references/npm-guide.md` §8).
- **One way to publish:** push a tag `sdk-v<version>`, and the release workflow publishes. Nobody runs `npm publish` by
  hand, except once, for the first version (`references/npm-guide.md` §4).
- **The UI depends on an exact version**, never on a range.

`.github/workflows/release.yml`:

```yaml
name: Release the SDK

on:
  push:
    tags: ["sdk-v*"]

permissions:
  contents: read

concurrency:
  group: release-sdk
  cancel-in-progress: false

jobs:
  publish:
    runs-on: ubuntu-24.04
    timeout-minutes: 20
    environment: npm
    permissions:
      contents: read
      id-token: write   # npm trusted publishing (OIDC), which also signs the provenance
    steps:
      - uses: actions/checkout@3d3c42e5aac5ba805825da76410c181273ba90b1 # v7.0.1
        with:
          # scripts/verify.ts recompiles each deployment from the commit it was deployed from.
          fetch-depth: 0
      - uses: actions/setup-node@949feb2413d6458794dcd2491c4babbbce0c15c1 # v7.1.0
        with:
          node-version-file: .nvmrc
          registry-url: https://registry.npmjs.org
      - name: The tag must match the package's version
        run: test "$GITHUB_REF_NAME" = "sdk-v$(node -p "require('./sdk/package.json').version")"
      - name: npm 11.20.0 (trusted publishing needs 11.5.1 or newer)
        run: npm install --global npm@11.20.0 && npm --version
      - run: npm ci --ignore-scripts
      - run: npm run typecheck
      - run: npm test
      - run: npm run verify
      - run: npm run build:sdk
      - run: npm run check:sdk
      - run: npm publish -w @stability-nexus/gluon-ergo-sdk
```

Notes:
- Nothing is published if the tag does not match `sdk/package.json`, or if any check fails.
- Trusted publishing needs npm 11.5.1 or newer, Node 22.14.0 or newer, a GitHub-hosted runner, and the
  `id-token: write` permission; npm then generates the provenance attestation itself (npm's documentation, read on
  2026-10-13). The workflow installs npm 11.20.0, which runs on Node 24. The tag check was tried locally on 2026-10-12.
- No npm token is stored in GitHub.
- The actions are pinned to the same commits as the repository's `ci.yml`.

**One-time setup, in A6:** follow `references/npm-guide.md`. In short:
1. Create the npm organization `stability-nexus`, with at least two owners who use 2FA (guide §2, §3).
2. Publish the first version, `2.0.0-next.0`, by hand from a clean checkout (guide §4): npm can add a trusted
   publisher only to a package that exists.
3. Add the trusted publisher: organization `StabilityNexus`, repository `Gluon-Ergo-Backend-Contracts`, workflow
   filename `release.yml`, environment `npm` (guide §5). It must then be used within 2 days, so add it when the first
   release from CI is ready.
4. Set the package's publishing access to "Require two-factor authentication and disallow tokens" (guide §6).
5. In the contracts repository, create the GitHub environment `npm`, limited to the tags `sdk-v*`, and optionally with
   the maintainers as required reviewers (guide §7).

Optionally, have every release approved on npm with 2FA before it goes live (staged publishing, guide §9).

## 8. Part A: tests

### 8.1 What exists

The 375 tests of `test/` (Vitest): config validation, compilation, bootstrap planning and records, and, for both pegs,
every transaction type executed against the real contract on Fleet's mock chain, with one-unit mutations that the
contract must reject, the live DORT buyback box (`test/fixtures/dort-buyback-box.json`), and `consensusIssues()` for the
rules the mock chain does not check. From A1 on they test the SDK's source, unchanged.

### 8.2 What each step adds

Listed in A2 to A5: the parsing and generation of deployments; the new formulas; the reactor and oracle checks; node
access with an injected `fetch`; input selection; the client's transactions against the real contract; one test per
error code; the snapshot.

### 8.3 Package checks

`npm run check:sdk` (§5.2), in CI and before every publication.

### 8.4 Live, read-only checks

`node scripts/smoke.ts <deployment>`:
1. fetches the chain state;
2. prints the snapshot;
3. builds every allowed transaction with a fictitious funding box;
4. sends each to the node's `/transactions/check`.

Expect "missing input" errors only; any other rejection is a bug. The node rejects the fictitious input before it
evaluates any script, so this checks structure and serialization against live boxes, not the contract's rules (the
mock-chain tests cover those). The bootstrap's dry run did the same on 2026-10-11.

### 8.5 Live checks on the first mainnet deployment

The first mainnet bootstrap is also the first live test, and a small seed (at most 10 ERG, which the bootstrap script
enforces) keeps its cost low. Once the deployment is recorded and bundled:
1. run `scripts/smoke.ts` against it (§8.4);
2. run a fission, a fusion and both beta decays for real, with small amounts, from a wallet.

Adjust Peg can only be run live when a reactor actually leaves its healthy range, which takes an oracle price move of
tens of percent. Until then, the mock-chain tests are its test.

## 9. Part B, the UI repository: Step 0, the legacy UI is `mainnet`

Do this first, before any change for v3.

**Facts** (checked on 2026-10-12):
- `mainnet` is `98e902d9697a681e115d3b0534d3c80923ef12b9` (2026-03-01). The README says Vercel deploys it. It vendors
  the SDK in `lib/gluon-gold-sdk` (a `file:` dependency) and is 52 commits behind `main`.
- `main` is `e4e30abc753372a6e438bcc98f1e476661986bc0` (2026-09-03). It depends on npm `gluon-ergo-sdk ^1.0.2`, locked
  at 1.0.3.

**Decision.** `mainnet` is the legacy branch. No new branch is created for the legacy UI. `main` becomes the v3 line.

- **`mainnet` stays as it is**, deployed to Vercel by the existing project. Optionally protect it on GitHub (Settings →
  Branches): block force pushes and deletion. Never merge `main` into it.
- **Check the existing Vercel project(s)** (Settings → Environments → Production → Branch Tracking; older dashboards:
  Settings → Git → Production Branch). Each must track `mainnet`. If one tracks `main`, switch it to `mainnet` before
  B1 lands, or the first v3 commit would be deployed over the legacy site.
- **Tag the last commit of the old design on `main`**, so that porting has a stable reference:

  ```sh
  git clone https://github.com/StabilityNexus/Gluon-Ergo-UI.git && cd Gluon-Ergo-UI
  git tag pre-v3 e4e30abc753372a6e438bcc98f1e476661986bc0
  git push origin pre-v3
  ```

  Where this plan says **port**, take the file from `pre-v3` (`git show pre-v3:<path>`): it is the newest version of the
  same design as `mainnet`.
- If GitHub Pages is enabled on the upstream repository, disable it (Settings → Pages). Upstream does not deploy.

## 10. Part B: the two deployments

| Deployment | Serves | Repository (a fork of upstream) | Site |
|---|---|---|---|
| Gold | `gluon-gold-v3` | `StabilityNexus/Gluon-Ergo-UI-Gold` | https://gold.gluon.stability.nexus/ |
| Dollar | `gluon-dollar-v3` | `StabilityNexus/Gluon-Ergo-UI-Dollar` | https://dollar.gluon.stability.nexus/ |

(Without a custom domain, a site would be at `https://stabilitynexus.github.io/<repository>/` and would need
`NEXT_PUBLIC_BASE_PATH`; the build supports that for previews and as a fallback.)

GitHub Pages publishes one site per repository, so each deployment has a repository of its own. They work like this:

- **Upstream** is `StabilityNexus/Gluon-Ergo-UI`. It holds the shared code on `main` and the legacy UI on `mainnet`. It
  has no deployment configuration and does not publish a site.
- **Each fork** is a fork of upstream. It has the same code as upstream `main`, plus **one file of its own: the
  committed `.env.production`**. That file, and the repository's Pages settings, are the only differences between the
  Gold fork and the Dollar fork. Changes to the code are made in upstream and merged into both forks (§15.8).
- **One build serves exactly one deployment**, chosen by `NEXT_PUBLIC_GLUON_DEPLOYMENT`. There is no selector and no
  code for several deployments in one site.
- A later v3 reactor (another peg) is added by: a new SDK release that bundles its record (A7), an entry in the UI's
  presentation config (§12.3), and a new fork (§15.3).

## 11. Part B: what is reused and what is replaced

The new version keeps the current app's layout, components and charts. What changes is the data layer, which assumes
the v2 Gold reactor throughout, and the hosting model. This table says what the current code does and what replaces
it; none of it is a fix list for the legacy UI.

| Area | Current code (`main`) | New version |
|---|---|---|
| SDK | `gluon-ergo-sdk` 1.x; `new sdk.Gluon()` in 7 places, each with the SDK's built-in reactor | `@stability-nexus/gluon-ergo-sdk` 2.0.0; one `GluonClient` in `GluonProvider` |
| Reactor and token ids | hard-coded in `lib/constants/token.ts` and 6 other files | `deployment.tokens` |
| Units and labels | oracle price ÷ 1000, a `0.66` cushion, "GAU"/"GAUC" literals | `ProtocolSnapshot` fields; tickers from the token names |
| Amounts | JavaScript numbers (`Number(...)`) | `bigint`, with `amounts.ts` |
| Wallet UTXOs | all passed to the SDK | the SDK selects what it needs |
| Environment | `NEXT_PUBLIC_DEPLOYMENT` (a network), two node variables, about 25 presentation variables, a server-side API key | §12.2: a few `NEXT_PUBLIC_*` variables in `.env.production`; presentation in code; no secrets |
| USD price of ERG | `/api/getErgPrice` (CoinGecko, with an API key), used by `MyStats`, `GluonStats` and `FeeBreakdownPanel` | none: removed (UD9) |
| Server code | API routes in `src/pages/api/`, and a CSP set by `headers()` in `next.config.ts` | none: a static export (§12.1) |
| History and fee charts | assume a `Long` R9, two contract addresses and only four kinds of transaction | v3 records; Adjust Peg and Update Treasury transitions |
| wasm | `ergo-lib-wasm-*`, used by `NodeService.getCtx` | none |
| Hosting | `vercel.json` (npm), and a GitHub starter workflow for Pages that looks for `bun.lockb` while the repository has `bun.lock` | the workflows of §15.4 |
| Site identity | hard-coded `https://gluon.fi` (canonical link) and `https://www.gluon.gold/` (SEO tags) | `NEXT_PUBLIC_SITE_URL` |
| Tests | none | `bun test`, with a fake SDK client |

**Reused as it is:** the layout, navigation and theme machinery, UI primitives and icons, `ErgoProvider`, the wallet
connector, the home page, the `resolveAssetPath` helper, IndexedDB and transaction-listener utilities, and the
presentation code of the charts.

## 12. Part B: target design of the UI

The UI's decisions, UD1 to UD10, are in §2.2.

### 12.1 Changes that make the site static

1. **`next.config.ts`:**

   ```ts
   import type { NextConfig } from "next";

   const nextConfig: NextConfig = {
     output: "export",                       // `next build` writes the site to out/
     trailingSlash: true,                    // /swap/ is out/swap/index.html, which GitHub Pages serves
     images: { unoptimized: true },          // the image optimizer needs a server
     basePath: process.env.NEXT_PUBLIC_BASE_PATH || "",  // "/<repo>" for https://<owner>.github.io/<repo>/
     reactStrictMode: true,
   };
   export default nextConfig;
   ```

   Remove the wasm webpack settings, `transpilePackages` and `headers()`: a static export supports no custom headers,
   rewrites or redirects.
2. **Delete `src/pages/api/`** (`getErgPrice.ts`, `hello.ts`): API routes need a server. Add no `getServerSideProps`,
   `getInitialProps` or middleware.
3. **Content-Security-Policy as a meta tag** in `_document.tsx`, since the host sends no headers:
   `<meta httpEquiv="Content-Security-Policy" content="script-src 'self' 'unsafe-inline'; object-src 'none'; base-uri 'self'" />`.
   (`'unsafe-inline'` stays for the inline theme script. A meta tag cannot set `frame-ancestors`.)
4. **Asset paths.** Pass every absolute path to a file in `public/` through `resolveAssetPath(path,
   process.env.NEXT_PUBLIC_BASE_PATH)`, including the `<link>` tags of `_document.tsx` (manifest, apple-touch-icon).
   Make the paths in `public/manifest.json` relative (`"start_url": "./"`, `"src": "logo/gluon.png"`), so that the
   manifest works under any base path.
5. **Site identity.** Delete the hard-coded `<link rel="canonical" href="https://gluon.fi">` of `_document.tsx` and the
   default `url` (`https://www.gluon.gold/`) of `SEO.tsx`. Build the canonical link and the `og:url`, `og:image` and
   `twitter:*` URLs from `NEXT_PUBLIC_SITE_URL` (an absolute URL, which social crawlers need); omit them if it is unset.
6. **`public/.nojekyll`:** an empty file, so that the exported `_next/` folder is never processed by Jekyll.
7. **`src/pages/404.tsx`:** a short "page not found" page; GitHub Pages serves the exported `404.html` for unknown URLs.
8. **Scripts** (`package.json`): `"dev": "next dev --webpack"`, `"build": "next build --webpack"`,
   `"typecheck": "tsc --noEmit"`, `"lint": "eslint ."` (Next.js 16 removed `next lint`), `"test": "bun test"`.
9. Delete `vercel.json` and `.github/workflows/nextjs.yml`.

### 12.2 Configuration

The configuration is the `.env.production` file of each fork. Every value is public: it ends up in the browser bundle.
There are no secrets.

| Variable | Required | Meaning |
|---|---|---|
| `NEXT_PUBLIC_GLUON_DEPLOYMENT` | yes | The deployment this site serves, e.g. `gluon-gold-v3`. Must be a deployment of the SDK's `DEPLOYMENTS`. |
| `NEXT_PUBLIC_NODE_URL` | yes | HTTPS URL of an Ergo node that serves the blockchain index API (`/blockchain/…`) and allows browser requests (CORS). `http://localhost…` is accepted for development. |
| `NEXT_PUBLIC_SITE_URL` | recommended | The site's public URL, with its base path and a trailing slash, e.g. `https://gold.gluon.stability.nexus/`. Used for the canonical link and the social tags. |
| `NEXT_PUBLIC_BASE_PATH` | no | `/<repository name>` when the site is served from `https://<owner>.github.io/<repository>/`. Empty (the default, and the setting of both sites) when it has its own domain. |
| `NEXT_PUBLIC_EXPLORER_URL` | no | Base URL for transaction and token links. Default `https://explorer.ergoplatform.com`; links are `…/en/transactions/<id>`. |
| `NEXT_PUBLIC_EXPLORER_API_URL` | no | Explorer API for the history charts. Default `https://api.ergoplatform.com/api/v1`. |
| `NEXT_PUBLIC_YIELD_CHART_MIN_APY` | no | Threshold of the yield chart (`GaucYieldChart.tsx:209`); unchanged. |

Upstream has `.env.example`, with the same variables, and no `.env.production` (Appendix D). Next.js loads
`.env.production` automatically during `next build`; for development, copy `.env.example` to `.env.local`.

**Validation.** `src/config/env.ts` parses and validates the variables when it is imported, and throws
`Gluon UI configuration error: …` with a message that names the variable: the deployment name exists in the SDK; the
node URL is HTTPS (or localhost); `NEXT_PUBLIC_BASE_PATH` is empty or starts with `/` and does not end with one.
`pages/_app.tsx` imports it, so `next build` fails when the configuration is invalid, because every page is
pre-rendered. At build time it also logs `Gluon UI: <deployment>, node <url>`.

### 12.3 Presentation

`src/config/presentation.ts` holds what a site looks like, per deployment name:

```ts
export interface Presentation {
  protocolName: string;   // navbar and page titles: "GLUON GOLD"
  pegAsset: string;       // "Gold" | "USD": also keys the transmute action types
  pegUnit: string;        // "g of gold" | "USD": one neutron targets "1 <pegUnit>" at the initial peg factor
  favicon: string;        // "/logo/gluon.png" | "/logo/gluon-green.png"
  theme: { primary: string; glow: string; stableToken: string; volatileToken: string; /* … */ };
                          // the 17 keys of today's tokenConfig.theme
}
export const PRESENTATION: Record<string, Presentation> = {
  "gluon-gold-v3": { /* … */ },
  "gluon-dollar-v3": { /* … */ },
};
```

Copy the theme values and favicons from `.env.gold` (Gold) and `.env.dollar` (Dollar) as they are on `pre-v3`.
Tickers are not here: the neutron and proton tickers are `deployment.tokens.neutron.name` and `.proton.name`, which the
bootstrap script sets from the contracts repository's config.

`src/config/tokenConfig.ts` keeps its exported shape, so that components keep working, but is built from the deployment
and its presentation instead of from environment variables.

### 12.4 Modules

| Module | Role |
|---|---|
| `src/config/env.ts` (new) | §12.2 |
| `src/config/presentation.ts` (new), `src/config/tokenConfig.ts` (rebuilt) | §12.3 |
| `src/lib/providers/GluonProvider.tsx` (new) | The context of UD2: `{ deployment, presentation, client, chain?, snapshot?, status: "loading" \| "ready" \| "error", error?: GluonError, refresh() }` |
| `src/lib/hooks/useGluon.ts` (new) | `useGluon()` reads the context |
| `src/lib/components/blockchain/reactor/ReactorHealth.tsx` (new) | Fusion ratio, healthy band, peg factor and drift, oracle age (§12.5) |
| `src/lib/components/blockchain/reactor/AdjustPegPanel.tsx` (new) | The Adjust Peg flow (§12.5) |
| `src/lib/components/blockchain/reactor/NetworkGuard.tsx` (new) | The wallet-network check (§12.5) |
| `src/lib/utils/amounts.ts` (new) | `toBaseUnits(text, decimals): bigint` and `formatUnits(value, decimals): string`, without floating point |
| `src/lib/utils/error-handler.ts` | Maps `GluonError.code` to the messages of §12.6 |

### 12.5 User experience

**Prices and the peg.** Show:
- "1 ⟨neutron ticker⟩ targets ⟨`pegDrift`⟩ ⟨`pegUnit`⟩", for example "0.990 g of gold" after one downward adjustment,
  with its ERG price `targetPrice`;
- the oracle's price of one peg unit (`pegUnitPrice`);
- the market prices `neutronPrice` and `protonPrice`.

Never claim "pegged to 1 USD" once the peg factor has drifted. All amounts are in ERG, in tokens, or in the pegged
asset: there is no USD value of ERG (UD9).

**Reactor health.** `ReactorHealth` shows the fusion ratio `q` as a percentage on a bar with the healthy band 50–98 %,
the peg factor and its drift ("peg factor 99.0 % of initial"), and the oracle price's age in blocks. The swap card
follows this table:

| State | Condition | What the swap card does |
|---|---|---|
| Healthy | `snapshot.healthy` | Normal. Transmutation MAX buttons use `maxBetaDecayPlus/Minus`. |
| Unhealthy, q above 98 % | `adjustPeg.direction = "down"` | Fission, fusion and transmutations disabled. Banner: "The neutrons in circulation are worth more than 98 % of this reactor's reserve. Trading resumes after the peg factor is lowered, 1 % per step (⟨steps⟩ steps at the current price). Anyone can do this." Show the Adjust Peg panel. |
| Unhealthy, q below 50 % | `adjustPeg.direction = "up"` | Same, with "less than 50 %" and "raised" |
| Oracle stale | `oracleAge ≥ 35` | Actions disabled. "Waiting for the next oracle update." Retried on the next poll. |
| Buyback unavailable | `!betaDecaysAvailable` | Transmutations disabled: "The oracle's buyback box is unavailable." Fission and fusion work as usual. |
| Ambiguous state | last refresh failed with `BOX_AMBIGUOUS` | Actions disabled. "The reactor or the oracle is being updated; retrying." |
| Node unreachable | last refresh failed with `NODE_ERROR` | Actions disabled, last good data shown, "Reconnecting to the node." |

**Adjust Peg flow.** `AdjustPegPanel`, shown only when unhealthy:
1. Show the direction, the steps needed (`quoteAdjustPeg`) and the cost: one miner fee per step, paid by the user, with
   no protocol fee.
2. Button "Adjust peg (step 1 of N)": `client.buildAdjustPeg(chain, { userBoxes, changeAddress })`, then the wallet's
   `sign_tx` and `submit_tx`, then `refresh()`.
3. After the refresh shows the new reactor box (mempool-aware; compare the reactor's `transactionId` with the submitted
   id, retrying every 2 s for up to 60 s), offer the next step. Stop when healthy. Do not sign several steps at once:
   each step needs the box the previous one created.
4. Errors: `REACTOR_HEALTHY` means someone else finished it ("the reactor is healthy again"). `ORACLE_STALE` and
   `BOX_AMBIGUOUS`: retry later. A wallet rejection: stop silently.

**Wallet network.** `NetworkGuard` reads the wallet's change address. A mainnet P2PK address starts with `9`. If it does
not, show a blocking banner ("Your wallet is not connected to Ergo mainnet. Switch it to mainnet.") and disable
actions. The SDK also throws `INVALID_ADDRESS` for such an address.

**No UI fee (UD10).** The client is created without `uiFeeAddress`, and the fee breakdown has no interface-fee row.

### 12.6 Error messages

| `GluonError.code` | Message |
|---|---|
| `REACTOR_UNHEALTHY` | "The reactor is outside its healthy range. Trading resumes after the peg is adjusted." |
| `WOULD_LEAVE_HEALTHY_RANGE` | "This amount would push the reactor out of its healthy range. The maximum is ⟨max⟩." |
| `REACTOR_HEALTHY` | "The reactor is healthy; no peg adjustment is needed." |
| `ORACLE_STALE` | "The oracle price is too old. Waiting for the next oracle update (usually a few minutes)." |
| `BOX_AMBIGUOUS` | "The reactor or the oracle is being updated right now. Please retry in a minute." |
| `BOX_NOT_FOUND` | "Could not find the reactor or oracle on the network. Check the node." |
| `BUYBACK_UNAVAILABLE` | "Transmutations are paused: the oracle's buyback box is unavailable." |
| `AMOUNT_TOO_SMALL` | "The amount is too small to produce any tokens." |
| `AMOUNT_TOO_LARGE` | "The reactor cannot pay out or release that amount." |
| `INSUFFICIENT_FUNDS` | "Not enough ERG or tokens in your wallet for this transaction and its fees." |
| `INVALID_ADDRESS` | "Your wallet is not connected to Ergo mainnet." |
| `NODE_ERROR` | "Could not reach the Ergo node. Retrying." |
| `UNKNOWN_DEPLOYMENT`, `INVALID_DEPLOYMENT` | configuration errors: caught at build time (UD7); at run time, a generic error |

## 13. Part B: work plan

Each step is one pull request into `main` of the upstream repository. Run `bun run typecheck`, `bun run lint`,
`bun test` and `bun run build` before merging each one. Where a step says **port**, start from the file on `pre-v3`
(`git show pre-v3:<path> > <path>`) and change its data layer; the presentation code usually stays.

### B1: Foundation

1. **Dependencies.** Add `@stability-nexus/gluon-ergo-sdk` at an exact `2.0.0-next.N` (until A6 publishes a
   pre-release, a local build: §16). Remove `gluon-ergo-sdk` 1.x, `ergo-lib-wasm-browser`, `ergo-lib-wasm-nodejs` and
   `@fleet-sdk/core` (nothing in `src` uses it directly). Add `.nvmrc` with `24` and `"engines": { "node": ">=24" }`.
   Regenerate `bun.lock`.
2. **Static export.** Apply §12.1: `next.config.ts`, the deletion of `src/pages/api/`, the CSP meta tag, asset paths,
   site identity, `public/.nojekyll`, `404.tsx`, the scripts, and the deletion of `vercel.json` and `nextjs.yml`.
3. **Remove what depends on the SDK 1.x API, and the CoinGecko features.** The SDK's API changes completely, so delete
   every file that imports `gluon-ergo-sdk`, `ergo-lib-wasm-*` or the two `useGluon*` hooks, and the routes that render
   them. They are ported back in B2 to B5 (as of `e4e30ab`), except `MyStats.tsx`, which is removed for good (UD9):
   - swap: `components/blockchain/swap/*`, `functions/reactor/*`, `pages/swap/index.tsx`;
   - dashboard: `components/blocks/dashboard/*`, `pages/reactor/index.tsx`;
   - history and fees: `hooks/useGluonFeeBreakdown.ts`, `hooks/useGluonTransactionHistory.ts`.

   Hide the navigation entries of those pages (`TopNavbar.tsx`, `BottomNavbar.tsx`, `ReactorSidebar.tsx`) until their
   PR. `pages/history` (the user's own transactions, from IndexedDB) does not depend on the SDK and stays.
4. **Configuration.** Add `src/config/env.ts` (§12.2) and `src/config/presentation.ts` (§12.3), import `env.ts` in
   `pages/_app.tsx`, and rebuild `src/config/tokenConfig.ts`. Delete `.env.gold` and `.env.dollar`; rewrite
   `.env.example` (Appendix D); in `.gitignore`, drop the exceptions for the two deleted files and keep `.env*` ignored
   (the forks add `.env.production` with `git add -f`, §15.3).
5. **Provider and utilities.** Add `GluonProvider` and `useGluon` (§12.4) and mount the provider in
   `lib/providers/Providers.tsx`. Add `amounts.ts`. In `lib/constants/token.ts`, keep `ACTION_TYPES` and remove
   `TOKEN_ADDRESS`; `erg-converter.ts` takes decimals from the deployment. Rewrite `lib/utils/node-service.ts` without
   wasm, keeping what the transaction listener uses (height, unconfirmed-transaction lookups).
6. **CI and publishing.** Add `.github/workflows/ci.yml` and `.github/workflows/pages.yml` as in §15.4, with the actions
   pinned by commit SHA. Add `bun run build` to `ci.yml` as soon as the SDK bundles a deployment.
7. **Test harness.** `bunfig.toml` with `[test] preload = ["./test/setup.ts"]`, where `test/setup.ts` registers jsdom's
   window and document as globals (Bun's runner has no DOM). `test/fakeClient.ts` implements the SDK's `GluonClient`
   surface (§5.4) with fixed `ChainState`, snapshots and quotes, and records the calls it receives.
8. **Tests.** `env.ts`: valid and invalid deployment names, a non-HTTPS node, a malformed base path. `GluonProvider`,
   with the fake client: polling, keeping the last good state on error, pausing while the tab is hidden. `amounts.ts`:
   round trips for 0, 3 and 9 decimals and for values above 2⁵³.

**Done when** the checks above pass; `bun run build` writes `out/` (once a deployment is bundled); and the exported
home page renders with the configured deployment's presentation and tickers.

### B2: The swap card

**Port** `ReactorSwap.tsx`, `FeeBreakdownPanel.tsx`, `functions/reactor/*` and `pages/swap/index.tsx`.

1. `ReactorSwap` takes `client`, `chain` and `snapshot` from `useGluon()`; it initializes nothing.
2. Each `calculate*Amounts` becomes a call to `client.quote*`: parse input with `toBaseUnits(text, decimals)` using the
   token's `decimals` from the deployment, format with `formatUnits`, and never convert an amount to `number`. Fusion's
   maximum ERG comes from the wallet's balances and `quoteFusion` (take the token ratio for 1 ERG, then verify the
   result with `quoteFusion`).
3. Each `handle*Swap` becomes: `refresh()`; `client.build*(chain, { …, userBoxes: await getUtxos(), changeAddress: await
   getChangeAddress() })`, both from `useErgo()` (`ErgoProvider.tsx:330-349,378-388`); `sign_tx`, then `submit_tx`;
   `refresh()`. The SDK selects the inputs it needs.
4. Token ids, decimals and tickers come from `deployment.tokens`: `defaultTokens` (`utils.ts`) becomes a function of the
   deployment; drop the `gau`/`gauc` legacy fields. `ACTION_TYPES` stays, keyed by `pegAsset`.
5. Health gating (§12.5): disable the controls per the state table; MAX for transmutations = min(balance,
   `maxBetaDecayPlus/Minus`).
6. The fee panel shows `quote.fees` (dev, oracle, `outputsMinFee`, miner) **in ERG only**: drop the `ergPriceUSD`
   property and the USD amounts, and show no interface fee (UD10).
7. `error-handler.ts` maps the codes of §12.6; explorer links use `…/en/transactions/<id>`.
8. Tests, with the fake client: each state of §12.5 enables and disables the right controls; each `handle*Swap` calls
   `build*` with the wallet's UTXOs and change address; the MAX buttons.

**Done when**, on the first mainnet deployment (§14.2), a small fission and fusion succeed from the UI. Until a
deployment exists, the fake-client tests are the check.

### B3: Reactor health, Adjust Peg and the network guard

1. `ReactorHealth`, `AdjustPegPanel` and `NetworkGuard` (§12.5), on the swap page; `ReactorHealth` also on the reactor
   dashboard when B4 adds it.
2. Tests, with the fake client: a healthy reactor shows no panel; an unhealthy one shows the direction and steps; one
   step calls `buildAdjustPeg`, signs, submits, and waits for the new reactor box; `REACTOR_HEALTHY` after a step ends
   the flow; a wallet rejection leaves the panel usable; a non-mainnet address blocks actions.

**Done when** those tests pass. Adjust Peg can only be run live when a real reactor leaves its healthy range (§14.2).

### B4: Dashboard

**Port** `GluonStats.tsx`, `ProtonAPYBadge.tsx`, `ReserveRatioChart.tsx`, `GaucLeverageChart.tsx`,
`GaucYieldChart.tsx`, `CumulativeFeesChart.tsx`, `VolumeChart.tsx`, `GluonExplainer.tsx` and `pages/reactor/index.tsx`,
reading `snapshot` from `useGluon()`. **Do not port `MyStats.tsx`**, and leave out the ERG price card, the ERG price
polling and the USD figures of `GluonStats.tsx` (UD9).

| Statistic | Source |
|---|---|
| Price of one peg unit | `pegUnitPrice` |
| Target price of one neutron, with the peg factor's drift | `targetPrice`, `pegFactor`, `pegDrift` |
| Neutron and proton price | `neutronPrice`, `protonPrice` |
| Total value locked | `reserve` |
| Fusion ratio | `q` |
| Reserve ratio | `reserveRatio` |
| Price crash cushion | `crashCushion.untilDepeg`; also `untilAdjustment` ("fall before trading pauses") |
| Proton leverage | `protonLeverage`, shown as "—" when undefined |
| Circulating supplies | `neutronsInCirculation`, `protonsInCirculation` |
| Volumes of 1, 7 and 14 days, and per-day series | `volumes.plus`, `volumes.minus` (sum the first 1, 7 or 14 buckets) |
| Health, and the oracle's age | `healthy`, `oracleAge` (the `ReactorHealth` component) |

Also in this PR:
- Token names come from the deployment, and the peg-price card's label from the presentation.
- `GluonExplainer.tsx` gets a paragraph on the healthy range and Adjust Peg.
- Tests: `GluonStats` with fixed snapshots for Gold at peg drift 1 and 0.99, and for Dollar.

**Done when** the dashboard shows the right figures for the first mainnet deployment.

### B5: History and fee charts

**Port** `useGluonTransactionHistory.ts` and `useGluonFeeBreakdown.ts`, and the charts that use them.

1. **Parameterize by the deployment:** the reactor NFT, token ids and decimals, and the oracle pool NFT. A v3 reactor
   keeps one contract and one NFT for its whole life, so there is no address lineage and no migration handling.
2. **Explorer API** from `NEXT_PUBLIC_EXPLORER_API_URL` (or its default). Include the deployment's name in the cache
   keys (for example `gluon_box_history_v10:<deployment>`), so that deployments served from one origin, such as
   `localhost` in development, do not share caches.
3. **Decode** explorer boxes into the node's box shape (registers as plain hex strings), then use the SDK's pure
   functions: `readReactor(box, deployment)`, `oraclePrice` and `math.*`. Price each historical box with its own peg
   factor and the oracle box nearest its height.
4. **Classify** each transition by the box's deltas: fission, fusion and beta decays as in the current hook, plus
   **Adjust Peg** (value, tokens, R4–R8 and R9's first element unchanged; the peg factor changed) and **Update Treasury**
   (only R5 changed). The last two carry no fees; plot them as markers rather than as "unknown".
5. Reconstruct fees per transition with `math.*`, in the categories the hook has today (dev fee, dilution, oracle fee),
   in ERG.
6. Tests with recorded explorer responses of a real v3 reactor, as fixtures: one transition of each type that has
   occurred, with the expected category and fee amounts; for Adjust Peg, a hand-built box sequence.

**Done when** the charts render for the first mainnet deployment.

### B6: Documentation and clean-up

1. `README.md`: what the app is; the configuration (§12.2); running locally (`.env.local`); the two deployments and how
   the forks relate to upstream (§10, §15); Adjust Peg in a few sentences. Replace the badge link, which points to another
   project, with the addresses of the two sites. No CoinGecko step.
2. `docs/github-pages.md`: §15 of this plan, with the final repository names and domains.
3. Check that nothing of the old version is left: `src` has no literal token tickers, no `0.66`, no kg-to-gram `÷ 1000`,
   no `gluon-gold-v2`, no `COINGECKO`, no `uiFeeAddress`, no import from `gluon-ergo-sdk` (1.x), no
   `NEXT_PUBLIC_DEPLOYMENT` or `NEXT_PUBLIC_NODE` (without `_URL`), no `/api/` call, and `package.json` has no unused
   dependency.
4. Run the static checks of §14.1 locally.

### B7: The two deployments

Done by the maintainers once SDK 2.0.0 is released with both records (or with the record of the deployment being
published first):

1. In upstream, bump `@stability-nexus/gluon-ergo-sdk` to `2.0.0` (exact) and merge.
2. Create the forks and publish the sites: §15.3 and §15.5, once per fork.
3. Verify each site: §15.7.

## 14. Part B: test plan

### 14.1 Automated and static checks

- The tests listed in B1 to B5, run by `bun test` against the fake client, and checked by CI.
- **The exported site, locally:** with a valid `.env.local`, run `bun run build`, then serve `out/` with any static
  server (for example `bunx serve out`) and open it. Check that the home page, `/swap/`, `/reactor/` and `/history/`
  load, that a direct visit to a deep link works, and that an unknown URL shows the 404 page.
- **A base path:** build with `NEXT_PUBLIC_BASE_PATH=/test`, copy `out/` to a folder named `test` inside an empty
  directory, serve that directory, and check that the logo, favicon, manifest and chunks load under `/test/`.
- **No server code:** `out/` contains only files, and nothing in `src` calls `/api/`.

### 14.2 Mainnet checks

There is no testnet. The first mainnet bootstrap, with a small seed, is the first live test; after it, with the
deployment recorded and bundled, and with small amounts:

1. A fission, a fusion and both transmutations. Check the received amounts against the quote, and the fee outputs in the
   explorer.
2. The MAX buttons, including a transmutation capped by the healthy range.
3. The dashboard and the charts.
4. The wallet-network banner, with a wallet that is not on mainnet.

**Adjust Peg and oracle staleness** can only be seen when they actually happen: the oracle price must move by tens of
percent for a reactor to leave its healthy range, and the oracle must miss updates to become stale. Until then, the
fake-client tests are their check here, and the SDK's mock-chain tests cover the transactions themselves.

## 15. GitHub Pages guide

### 15.1 How the sites are published

Upstream holds the code; each fork builds and publishes one site (§10). A push to a fork's `main` runs the workflow of
§15.4: it type-checks, tests, builds the static site with the fork's `.env.production`, and deploys it to the fork's
GitHub Pages site. The workflow does nothing in upstream.

### 15.2 Before you start

1. **Rights.** You can create repositories in the `stability-nexus` organization and change their settings.
2. **The code.** Upstream `main` has B1 to B6 merged and depends on `@stability-nexus/gluon-ergo-sdk` 2.0.0, which
   bundles the deployment you are publishing. This prints its address, from a checkout of upstream with `bun install`
   done:

   ```sh
   node -e "import('@stability-nexus/gluon-ergo-sdk').then(s => console.log(s.getDeployment('gluon-gold-v3').address))"
   ```
3. **A node** for `NEXT_PUBLIC_NODE_URL` that passes the checks of §15.6.
4. **DNS access** for `stability.nexus`, to add one `CNAME` record per site (§15.3, step 7) and, recommended, the `TXT`
   record that verifies `gluon.stability.nexus` for the organization.
5. The repositories are public: GitHub Pages is available for public repositories on every plan.

### 15.3 Create a fork and publish its site

Repeat for each deployment (Gold, Dollar).

1. **Fork.** On `StabilityNexus/Gluon-Ergo-UI`: **Fork → Create a new fork**. Owner `stability-nexus`; repository name
   `Gluon-Ergo-UI-Gold` (Gold) or `Gluon-Ergo-UI-Dollar` (Dollar); tick **Copy the `main` branch only**, so that the
   legacy `mainnet` branch is not copied.
   (GitHub allows a fork in the same organization as its parent, under a different name. `gh repo fork` does the same;
   see `gh repo fork --help` for the name and branch options of your version.)
2. **Enable workflows.** Forks start with workflows disabled: Actions tab → **I understand my workflows, go ahead and
   enable them**.
3. **Pages source.** Settings → Pages → Build and deployment → Source: **GitHub Actions**.
4. **Deployment branch.** Settings → Environments → `github-pages`: deployment branches limited to `main`. GitHub
   creates this environment when Pages is set up with Actions or on the first run; if it is not there yet, do this step
   after step 6.
5. **The fork's one file.** Clone the fork, create `.env.production` as in §15.5, and commit it. The file is ignored by
   `.gitignore`, so add it with `git add -f .env.production`. Push to `main`.
6. **Watch the run.** Actions → *Deploy to GitHub Pages*: the `build` job type-checks, tests and builds; the `deploy`
   job publishes. The deployment's URL appears on the run's summary and in Settings → Pages.
7. **Custom domain.** Settings → Pages → Custom domain: `gold.gluon.stability.nexus` (Gold) or
   `dollar.gluon.stability.nexus` (Dollar), and save. At the DNS provider, add a `CNAME` record from that name to
   `stabilitynexus.github.io` (the organization's address, without the repository name). When GitHub's DNS check
   passes, tick **Enforce HTTPS**. With this publishing method a `CNAME` file in the repository is ignored: the setting
   is what counts. `NEXT_PUBLIC_SITE_URL` in `.env.production` already holds the new address (§15.5).

   **Recommended: verify the domain first.** As an owner of the organization: organization Settings → Pages → Add a
   domain → `gluon.stability.nexus`, then create the `TXT` record GitHub shows (named
   `_github-pages-challenge-StabilityNexus.gluon.stability.nexus`) and click Verify. A verified domain can be used only
   by repositories of the organization, so nobody else can attach `gold.gluon.stability.nexus` or
   `dollar.gluon.stability.nexus` to another repository, for instance if a `CNAME` record is ever left dangling.
   Verification covers a domain's immediate subdomains only, which is why the domain to verify is `gluon.stability.nexus`
   (verifying `stability.nexus` would not cover `gold.gluon.stability.nexus`). Do not add a wildcard DNS record.
8. Run the verification of §15.7.

### 15.4 The workflows

Both files are in upstream from B1 and reach the forks by merge. The actions are pinned by commit SHA (their newest
releases on 2026-10-12); update the pins when updating the actions.

`.github/workflows/pages.yml`:

```yaml
name: Deploy to GitHub Pages

on:
  push:
    branches: [main]
  workflow_dispatch:

permissions:
  contents: read

concurrency:
  group: pages
  cancel-in-progress: false

jobs:
  build:
    # Only the deployment forks publish: upstream has no .env.production.
    if: github.repository != 'StabilityNexus/Gluon-Ergo-UI'
    runs-on: ubuntu-24.04
    timeout-minutes: 20
    steps:
      - uses: actions/checkout@3d3c42e5aac5ba805825da76410c181273ba90b1 # v7.0.1
      - uses: actions/setup-node@949feb2413d6458794dcd2491c4babbbce0c15c1 # v7.1.0
        with:
          node-version-file: .nvmrc
      - uses: oven-sh/setup-bun@0c5077e51419868618aeaa5fe8019c62421857d6 # v2.2.0
      - run: bun install --frozen-lockfile
      - run: bun run typecheck
      - run: bun test
      - run: bun run build            # reads .env.production; writes the static site to out/
      - uses: actions/upload-pages-artifact@fc324d3547104276b827a68afc52ff2a11cc49c9 # v5.0.0
        with:
          path: out

  deploy:
    needs: build
    runs-on: ubuntu-24.04
    permissions:
      pages: write
      id-token: write
    environment:
      name: github-pages
      url: ${{ steps.deployment.outputs.page_url }}
    steps:
      - id: deployment
        uses: actions/deploy-pages@368f82528645a54fb793d4d04e342629a3f51346 # v5.0.1
```

`.github/workflows/ci.yml` (runs in upstream and in the forks):

```yaml
name: CI

on:
  push:
  pull_request:

permissions:
  contents: read

concurrency:
  group: ${{ github.workflow }}-${{ github.ref }}
  cancel-in-progress: true

jobs:
  check:
    runs-on: ubuntu-24.04
    timeout-minutes: 20
    steps:
      - uses: actions/checkout@3d3c42e5aac5ba805825da76410c181273ba90b1 # v7.0.1
      - uses: actions/setup-node@949feb2413d6458794dcd2491c4babbbce0c15c1 # v7.1.0
        with:
          node-version-file: .nvmrc
      - uses: oven-sh/setup-bun@0c5077e51419868618aeaa5fe8019c62421857d6 # v2.2.0
      - name: Upstream must not contain deployment configuration
        if: github.repository == 'StabilityNexus/Gluon-Ergo-UI'
        run: test ! -f .env.production
      - name: Use the example configuration where a repository has none
        run: test -f .env.production || cp .env.example .env.production
      - run: bun install --frozen-lockfile
      - run: bun run typecheck
      - run: bun run lint
      - run: bun test
      - run: bun run build
```

Notes:
- The `build` job is skipped in upstream, so `deploy`, which needs it, is skipped too.
- Pushes to a fork run both workflows. The CI check also builds, with the fork's own `.env.production`.
- If the upstream repository is renamed, update the repository name in the two `if:` conditions.
- The newest actions may have moved on; check each action's releases before pinning a newer SHA.

### 15.5 The `.env.production` of each fork

These files are the only difference between the forks.

Gold (`StabilityNexus/Gluon-Ergo-UI-Gold`):

```sh
NEXT_PUBLIC_GLUON_DEPLOYMENT=gluon-gold-v3
NEXT_PUBLIC_NODE_URL=https://node.sigmaspace.io
NEXT_PUBLIC_SITE_URL=https://gold.gluon.stability.nexus/
```

Dollar (`StabilityNexus/Gluon-Ergo-UI-Dollar`):

```sh
NEXT_PUBLIC_GLUON_DEPLOYMENT=gluon-dollar-v3
NEXT_PUBLIC_NODE_URL=https://node.sigmaspace.io
NEXT_PUBLIC_SITE_URL=https://dollar.gluon.stability.nexus/
```

Neither file sets `NEXT_PUBLIC_BASE_PATH`: both sites have their own domain. Without a custom domain (a preview, or a
fallback), a Gold site would be served from `https://stabilitynexus.github.io/Gluon-Ergo-UI-Gold/`, and its
`.env.production` would set `NEXT_PUBLIC_BASE_PATH=/Gluon-Ergo-UI-Gold` and
`NEXT_PUBLIC_SITE_URL=https://stabilitynexus.github.io/Gluon-Ergo-UI-Gold/`. A fork whose `.env.production` is missing
or invalid fails its build (UD7).

### 15.6 Check the node

Run these for the value of `NEXT_PUBLIC_NODE_URL`, with `ORIGIN` set to the site's origin
(`https://gold.gluon.stability.nexus` or `https://dollar.gluon.stability.nexus`):

```sh
NODE=https://node.sigmaspace.io
ORIGIN=https://gold.gluon.stability.nexus
# 1. The blockchain index is on and in sync (indexedHeight equals fullHeight):
curl -s "$NODE/blockchain/indexedHeight"
# 2. Browsers may call the node. The SDK makes one cross-origin POST with a JSON body, which browsers preflight:
curl -s -D - -o /dev/null -X OPTIONS "$NODE/utxo/withPool/byIds" -H "Origin: $ORIGIN" \
  -H "Access-Control-Request-Method: POST" -H "Access-Control-Request-Headers: content-type" \
  | grep -i "^HTTP\|access-control"
# 3. A plain GET carries the CORS header too:
curl -s -D - -o /dev/null -H "Origin: $ORIGIN" "$NODE/info" | grep -i "^HTTP\|access-control-allow-origin"
```

Expected: JSON with equal heights; `HTTP/2 200` with an `access-control-allow-origin` of `*` or `$ORIGIN` and an
`access-control-allow-headers` that includes `Content-Type`; the same `access-control-allow-origin` on the GET. On
2026-10-12, `https://node.sigmaspace.io` and `https://node.ergopool.io` passed all three from the origin
`https://stabilitynexus.github.io`, and the explorer API at `https://api.ergoplatform.com/api/v1` returned
`access-control-allow-origin: *` for GET requests. A `*` does not depend on the origin; run the checks again with the
sites' own origins before launch.

### 15.7 Verify a published site

1. The workflow run is green, and its summary shows the site's URL.
2. Open the site: the name, colors and favicon are the deployment's; the token tickers are the on-chain names (compare
   with `Deployments.md` of the contracts repository); the dashboard numbers load.
3. In the page source: the CSP meta tag is present, and the canonical link and `og:url` are the site's address.
4. In the browser's developer tools (Network): requests go to the node's URL and the explorer API, with no CORS errors
   and no request to any other host.
5. Open a deep link directly (`/swap/`): it loads. Open an unknown path: the 404 page appears.
6. Connect Nautilus. With a wallet that is not on mainnet, the wallet-network banner appears.
7. The mainnet checks of §14.2, with small amounts.

### 15.8 Keep the forks in sync

In each fork, once: `git remote add upstream https://github.com/StabilityNexus/Gluon-Ergo-UI.git`. For each change:

```sh
git fetch upstream
git switch main
git merge upstream/main      # GitHub's "Sync fork" button does the same
git push origin main         # the push publishes the site
```

- **The rule that keeps merges conflict-free:** a fork never edits a shared file. Its only file of its own is
  `.env.production`, which upstream never contains (the upstream CI fails if it does). Make every change in upstream
  first.
- After an SDK bump, merge into **both** forks and verify each site (§15.7).
- When upstream adds a required variable, add it to each fork's `.env.production`; until then the fork's build fails.

### 15.9 Day-two operations

- **Change a fork's configuration:** edit `.env.production` and push.
- **Roll back:** `git revert` the offending merge in the fork and push. The workflow publishes the reverted state.
- **Change a site's domain:** Settings → Pages → Custom domain, the DNS record, and `NEXT_PUBLIC_SITE_URL`.
- **Add a deployment (another v3 reactor):** an SDK release that bundles its record (A7), an entry in
  `presentation.ts`, a new fork (§15.3).
- **Retire a site:** disable Pages in the fork (Settings → Pages) and archive the repository.

### 15.10 Troubleshooting

| Symptom | Likely cause and fix |
|---|---|
| The workflow does not run in a fork | Workflows are disabled in new forks: enable them (Actions tab). |
| `pages.yml` shows both jobs skipped | The repository is upstream, or its name is the one in the `if:` condition. |
| The `deploy` job fails | Settings → Pages → Source must be **GitHub Actions**; and the `github-pages` environment must allow `main`. |
| The build fails with `Gluon UI configuration error: …` | `.env.production` is missing or invalid: the message names the variable. |
| The site loads but images, styles or scripts return 404 | The base path is wrong: set `NEXT_PUBLIC_BASE_PATH=/<repository>` for a `github.io/<repository>/` address, or leave it empty for a custom domain. |
| A direct visit to a page returns 404 | `trailingSlash: true` and the exported `404.html` are required (§12.1); check that `out/swap/index.html` exists. |
| The custom domain shows a certificate warning or 404 | DNS has not propagated, the `CNAME` target is wrong (it must not include the repository name), or **Enforce HTTPS** is not available yet. |
| The site shows an initialization error | The node is unreachable or blocks browsers: run §15.6. |
| An old version is still shown | GitHub's cache can take a few minutes: hard-reload, and check the run's commit. |

### 15.11 Limits and terms

- **Size and traffic:** a published Pages site is limited to 1 GB, with a soft bandwidth limit of about 100 GB a month.
  The 10-builds-an-hour limit does not apply to sites published with a custom workflow. This site is a few megabytes.
- **Terms:** GitHub's terms for Pages say it is not intended as a host for a site whose main purpose is facilitating
  commercial transactions or offering commercial software as a service. A protocol front end could be read either way.
  The maintainers have decided that Pages is acceptable for these sites (§18). **If GitHub objects,** nothing else
  changes: `out/` is a plain static site, and moving a fork to another static host replaces only the `deploy` job of
  §15.4.

### 15.12 The legacy site

The Vercel project(s) that serve the current UI keep deploying `mainnet` and are outside this plan (Step 0).

## 16. Coordination between the two repositories

**Order of work.**
1. A1 to A5 build the SDK in the contracts repository. They need no v3 deployment: the tests use synthetic records and
   the simulated chain, with the Dollar deployment's final constants.
2. B1 can start at the same time. Its tests run against `test/fakeClient.ts`, which implements the API of §5.4 with
   fixed data; the package itself is needed only to type-check and build, and until A6 publishes a pre-release a local
   build serves (below).
3. A6 publishes the first pre-releases: `2.0.0-next.0` by hand, `2.0.0-next.1` from CI. From then on, the UI depends
   on an exact published version.
4. The first mainnet bootstrap records `gluon-dollar-v3` or `gluon-gold-v3`. A7 publishes a pre-release that bundles it,
   and the UI can run live (§8.5, §14.2).
5. After the live checks, A7 publishes 2.0.0, and B7 creates the forks.

**Developing the UI against an unreleased SDK:**

```sh
# in the contracts repository
npm run build:sdk && npm pack -w @stability-nexus/gluon-ergo-sdk --pack-destination /tmp/gluon-sdk
# in the UI repository
bun add /tmp/gluon-sdk/stabilitynexus-gluon-ergo-sdk-<version>.tgz
```

Bun installs a local tarball (tried with Bun 1.2.17 on 2026-10-12), but CI cannot reach it: before a pull request is
merged, replace it with a published version, `bun add --exact @stability-nexus/gluon-ergo-sdk@<version>`.

**How a change reaches the sites:**

| Event | Contracts repository | UI repository and forks |
|---|---|---|
| A new deployment is recorded | `bootstrap.ts --submit` regenerates the data; commit and tag the deployment; bump the SDK's version, tag `sdk-v<version>`; CI publishes (A7) | bump `@stability-nexus/gluon-ergo-sdk` to the exact new version in upstream, then merge into both forks (§15.8) |
| A bug in the SDK | fix it, with a test; bump, tag; CI publishes | bump, then merge into both forks |
| A UI change | nothing | the change in upstream, then merge into both forks |

**Who owns what.** The maintainers of the contracts repository own the SDK and its releases, and, as owners of the npm
organization `stability-nexus`, its package. The UI's maintainers own the UI and the two forks.

## 17. Definition of done

**Contracts repository.**
- A1 to A6 are merged into `main`, and CI is green: type check, tests, `verify`, the SDK's build and `check:sdk`.
- `@stability-nexus/gluon-ergo-sdk` 2.0.0 is published by the release workflow, with provenance, and bundles at least
  the first mainnet v3 deployment; the package's publishing access disallows tokens (`references/npm-guide.md` §6).
- The scripts use the SDK: `lib/cli.ts` has no `uniqueBox`, `requireFreshOracle` or `selectFunds`.
- The SDK repository, `StabilityNexus/Gluon-Ergo-SDK`, and the old package, `gluon-ergo-sdk` 1.x, are unchanged.

**UI repository.**
- B1 to B6 are merged in upstream, upstream's CI is green, and upstream depends on `@stability-nexus/gluon-ergo-sdk`
  2.0.0, exactly.
- Both forks exist, each with its `.env.production`, and each site passes §15.7 at its own address (§10).
- `mainnet` is unchanged since Step 0, and the existing Vercel project still deploys it.
- The code has no v2 path, no use of SDK 1.x, no testnet support, no server code, no CoinGecko dependency and no UI fee
  (B6, step 3).

## 18. Decisions and open items

**Decided by the maintainers (2026-10-12).**
1. The SDK supports the v3 contract only, on mainnet only, under GPL-3.0; 1.x is not changed.
2. No testnet deployment, ever.
3. The legacy UI is the `mainnet` branch, which keeps being deployed to Vercel; the new UI is published on GitHub Pages.
4. The forks are `StabilityNexus/Gluon-Ergo-UI-Gold` and `StabilityNexus/Gluon-Ergo-UI-Dollar`, and their sites are
   https://gold.gluon.stability.nexus/ and https://dollar.gluon.stability.nexus/.
5. There is no UI fee (UD10).
6. GitHub Pages is acceptable for these sites (§15.11).
7. Porting starts from `pre-v3` (`main` at `e4e30ab`).
8. Proton leverage is shown as `q / (1 − q)`, as in the legacy UI (Appendix A).
9. `CODEOWNERS` is enforced. That decision protects contract-sensitive code in the UI repository, in the route where
   the library lives there. In this route the UI holds none: the SDK is in the contracts repository, whose maintainers
   change it directly. If the contracts repository starts taking outside pull requests, protect `sdk/`, `contracts/`,
   `deployments/` and `.github/` there the same way.

**Decided by the maintainers (2026-10-13).**
1. The SDK is a new npm package that the maintainers own, `@stability-nexus/gluon-ergo-sdk`, set up as
   `references/npm-guide.md` describes; the old `gluon-ergo-sdk` keeps 1.x.
2. `sdk/` is licensed GPL-3.0-only, and the rest of the contracts repository keeps having no license file (CD4).
3. The maintainers control the DNS of `stability.nexus`, for the records of §15.3, step 7.
4. The sites use `https://node.sigmaspace.io` as their node.

**Defaults that apply unless the maintainers decide otherwise.**
1. 2.0.0 is released, and becomes `latest`, once the first mainnet v3 deployment is recorded, bundled and live-checked
   (§7).
2. The deployment records are bundled in the package (CD8); `deploymentFromRecord` serves any other record.
3. One Adjust Peg step per transaction, rebuilt from fresh mempool-aware state, as `scripts/adjust-peg.ts --repeat`
   does; the SDK does not chain steps offline.
4. `nodeUrl` has no default: the caller passes an HTTPS node.
5. The SDK repository gets no change. A note in its README pointing to the new package would change no 1.x code; it is
   the maintainers' call.

**Good to know.**
- If the name `stability-nexus` is already taken on npm, the organization needs another name, and `@stability-nexus`
  changes everywhere (`references/npm-guide.md` §1).
- The contracts repository is `StabilityNexus/Gluon-Ergo-Contracts`, while the UI and its forks are in
  `stability-nexus`. The package's `repository` field and its trusted publisher name the contracts repository; if it
  moves, update both (`references/npm-guide.md` §10).

## Appendix A: Formulas

Notation: integer division truncates; `ONE = 10⁹`; `R = value − _MinFee`; `Sn`, `Sp` = neutrons and protons in
circulation (total supply in R4 minus the amount in the box); `HEIGHT = height + 1`. Exactly as `math.ts` (today `lib/math.ts`).

```
P      = oraclePrice × pegFactor / ONE                    target price of one whole neutron, nanoERG
q      = Sn × P / R
qNorm  = min(ONE × q / (q + ONE − 990_000_000), q)
healthy = 500_000_000 ≤ q ≤ 980_000_000

Fission (M nanoERG in):   neutronsOut = M × Sn × (ONE − 1_000_000) / R / ONE      (protons: the same with Sp)
Fusion (M nanoERG out):   neutronsIn  = M × Sn × ONE / (R × (ONE − 5_000_000))   (protons: the same with Sp)

valueOfProtons(a)  = a × ((ONE − qNorm) × R / Sp) / ONE
valueOfNeutrons(a) = a × (qNorm × R / Sn) / ONE

fees(principal):
  oracle = principal × 1 / 1000                              beta decays only
  dev    = (5 × principal / 1000) × (max − repaid) / max     if repaid < max, else 0     (R6 = (repaid, max))
  ui     = 4 × principal / 1000                              if a UI key is set
principal = M for fission and fusion; valueOfProtons(M) for beta decay +; valueOfNeutrons(M) for beta decay −
All four actions: devFeeRepaid' = devFeeRepaid + dev.

nDays = min((HEIGHT − lastBucketBlock) / 720, 14)
shift(v, n, x) = n = 0 ? [v0 + x, v1 … v13] : [x, 0 × (n − 1), v0 … v(13 − n)]
Beta decay + (M protons in):
  plus' = shift(R7, nDays, valueOfProtons(M)); minus' = shift(R8, nDays, 0)
  volume = max(0, Σplus' − Σminus');  φβ = 5_000_000 + ONE × volume / R
  neutronsOut = (M × (ONE − φβ) / Sp) × ((ONE − qNorm) × Sn / ONE) / qNorm
Beta decay − (M neutrons in):
  minus' = shift(R8, nDays, valueOfNeutrons(M)); plus' = shift(R7, nDays, 0)
  volume = max(0, Σminus' − Σplus')
  protonsOut = (M × (ONE − φβ) / Sn) × (qNorm × Sp / ONE) / (ONE − qNorm)
Both beta decays: lastBucketBlock' = HEIGHT / 720 × 720;  qAfter = Sn' × P / R must be healthy.

Adjust Peg: q > 980_000_000 → pegFactor × 99 / 100;  q < 500_000_000 → pegFactor × 101 / 100.
```

**Derived values** for `ProtocolSnapshot`:

```
pegUnitPrice  = oraclePrice × initialPegFactor / ONE
neutronPrice  = valueOfNeutrons(10^neutron.decimals)        per whole token
protonPrice   = valueOfProtons(10^proton.decimals)
reserveRatio  = 100 / q                                      percent, with q as a fraction
crashCushion.untilDepeg      = max(0, 1 − q / 0.99)          fraction by which ERG can fall against the peg before
                                                             neutrons are worth less than the peg
crashCushion.untilAdjustment = max(0, 1 − q / 0.98)          fall before the reactor leaves its healthy range
protonLeverage = q / (1 − q)                                 while q < 0.99
```

**Why `protonLeverage = q / (1 − q)`.** Let `e` be the price of ERG in the pegged asset and `N` the neutron supply's
value in that asset, so that `q = N / (R·e)`. Measured in ERG, a proton is worth `(R − N/e) / Sp = R·(1 − q) / Sp`. The
elasticity of that value to `e` is `(N/e) / (R − N/e) = q / (1 − q)`. At `q = 0.6` a proton's price in ERG rises 1.5 %
for each 1 % that ERG rises against the pegged asset. This is the figure the legacy UI shows. Within the healthy range
`qNorm = q`, so the formula holds there and up to `q = 0.99`.

## Appendix B: Reactor box encoding

| | Type | Meaning |
|---|---|---|
| Value | | reserve + `_MinFee` |
| Token 0 / 1 / 2 | | reactor NFT (1) / neutrons not in circulation / protons not in circulation |
| R4 | `(Long, Long)` | total supply of neutrons, of protons |
| R5 | `SigmaProp` | treasury key: serialized `08cd` + a 33-byte public key |
| R6 | `(Long, Long)` | dev fees repaid, maximum (nanoERG) |
| R7 / R8 | `Coll[Long]` | beta decay + / − value per bucket: 14 buckets of 720 blocks |
| R9 | `(Long, Long)` | first block of the current bucket; peg factor ×10⁹ |

Serialized type bytes: `Long` = `05`, `(Long, Long)` = `59`, `Coll[Long]` = `11`, `SigmaProp` = `08`. The context variable for
the oracle-core top-up is `{ "0": "0402" }`. The UI-fee variable is `{ "0": SSigmaProp(SGroupElement(key)) }` on the
reactor input.

## Appendix C: File checklists

**Contracts repository (Part A).**

| Path | Action | Step |
|---|---|---|
| `sdk/src/math.ts`, `reactor.ts`, `transactions.ts`, `node.ts` | moved from `lib/` with `git mv` | A1 |
| imports in `lib/`, `scripts/`, `test/` | point to `sdk/src/` | A1 |
| `sdk/package.json`, `sdk/tsconfig.build.json`, `sdk/src/index.ts`, `sdk/LICENSE` | new | A1 |
| `sdk/README.md`, `sdk/CHANGELOG.md` | new; completed | A1, A6 |
| `package.json`, `package-lock.json`, `tsconfig.json`, `.gitignore` | workspace and scripts; `sdk/src` included; `sdk/dist/` ignored | A1 |
| `scripts/check-sdk.ts` | new | A1 |
| `.github/workflows/ci.yml` | build and check the SDK | A1 |
| `sdk/src/errors.ts`, `sdk/src/deployments/fromRecord.ts`, `index.ts` | new | A2 |
| `sdk/src/deployments/data.ts` | generated | A2, then A7 |
| `scripts/build-sdk-data.ts` | new | A2 |
| `scripts/bootstrap.ts`, `scripts/verify.ts` | regenerate and check the SDK's data | A2 |
| `sdk/src/oracle.ts` | new; `oraclePrice` moves here | A3 |
| `lib/cli.ts` | loses `requireFreshOracle` (A3), `uniqueBox` and `selectFunds` (A4) | A3, A4 |
| `scripts/bootstrap.ts`, `adjust-peg.ts`, `update-treasury.ts` | use the SDK's oracle check, box lookup and input selection | A3, A4 |
| `sdk/src/select.ts` | new | A4 |
| `sdk/src/client.ts`, `sdk/src/types.ts` | new | A5 |
| `scripts/smoke.ts` | new | A5 |
| `test/sdk-*.test.ts` | new | A2 to A5 |
| `README.md` | the SDK, the layout, the license note | A1, A6 |
| `.github/workflows/release.yml` | new | A6 |

**UI repository (Part B).**

| Path | Action | Step |
|---|---|---|
| `package.json`, `bun.lock` | `@stability-nexus/gluon-ergo-sdk` (a pre-release, then 2.0.0); remove `gluon-ergo-sdk` 1.x, ergo-lib-wasm, `@fleet-sdk/core`; `engines`; scripts | B1 |
| `.nvmrc` | new (`24`) | B1 |
| `next.config.ts` | static export (§12.1) | B1 |
| `vercel.json`, `.github/workflows/nextjs.yml` | delete | B1 |
| `.github/workflows/ci.yml`, `pages.yml` | new (§15.4) | B1 |
| `.env.gold`, `.env.dollar` | delete (values move to `presentation.ts`) | B1 |
| `.env.example`, `.gitignore` | rewrite; drop the two exceptions | B1 |
| `src/pages/api/*` | delete | B1 |
| `src/pages/_document.tsx`, `src/lib/components/layout/SEO.tsx`, `public/manifest.json` | CSP meta, asset paths, site identity | B1 |
| `public/.nojekyll`, `src/pages/404.tsx` | new | B1 |
| `src/config/env.ts`, `src/config/presentation.ts` | new | B1 |
| `src/config/tokenConfig.ts` | rebuilt from deployment and presentation | B1 |
| `src/lib/providers/GluonProvider.tsx`, `src/lib/hooks/useGluon.ts` | new | B1 |
| `src/lib/providers/Providers.tsx` | mount `GluonProvider` | B1 |
| `src/lib/utils/amounts.ts`, `erg-converter.ts` | new; decimals from the deployment | B1 |
| `src/lib/utils/node-service.ts` | rewrite without wasm | B1 |
| `src/lib/constants/token.ts` | keep `ACTION_TYPES`; remove `TOKEN_ADDRESS` | B1 |
| `bunfig.toml`, `test/setup.ts`, `test/fakeClient.ts` | new | B1 |
| files that use the SDK 1.x API (B1, step 3) | delete in B1, then port | B1, B2 to B5 |
| `src/lib/components/blocks/dashboard/MyStats.tsx` | delete, not ported (UD9) | B1 |
| `components/blockchain/reactor/ReactorHealth.tsx`, `AdjustPegPanel.tsx`, `NetworkGuard.tsx` | new | B3 |
| `README.md`, `docs/github-pages.md` | rewrite; new | B6 |
| forks: `.env.production` | new, one per fork | B7 |

## Appendix D: `.env.example` of the UI

```sh
# The deployment this site serves: a v3 deployment bundled in @stability-nexus/gluon-ergo-sdk.
NEXT_PUBLIC_GLUON_DEPLOYMENT=gluon-gold-v3

# HTTPS Ergo node that serves the blockchain index API and allows browser requests.
NEXT_PUBLIC_NODE_URL=https://node.sigmaspace.io

# The site's public URL, with its base path and a trailing slash.
NEXT_PUBLIC_SITE_URL=https://gold.gluon.stability.nexus/

# Optional.
# NEXT_PUBLIC_BASE_PATH=/Gluon-Ergo-UI-Gold
# NEXT_PUBLIC_EXPLORER_URL=
# NEXT_PUBLIC_EXPLORER_API_URL=
# NEXT_PUBLIC_YIELD_CHART_MIN_APY=
```

For development, copy it to `.env.local`. A fork commits its own `.env.production` instead; upstream never does.

## Appendix E: Documentation used

- Custom workflows for GitHub Pages, and custom domains: https://docs.github.com/en/pages
- Verifying a custom domain for GitHub Pages: https://docs.github.com/en/pages/configuring-a-custom-domain-for-your-github-pages-site/verifying-your-custom-domain-for-github-pages
- GitHub Pages limits: https://docs.github.com/en/pages/getting-started-with-github-pages/github-pages-limits
- GitHub's terms for additional products (Pages): https://docs.github.com/en/site-policy/github-terms/github-terms-for-additional-products-and-features
- Forks within one organization: https://github.blog/changelog/2022-06-27-improved-innersource-collaboration-and-enterprise-fork-policies/
- Next.js static exports: https://nextjs.org/docs/app/guides/static-exports
- Next.js 16 requirements and the removal of `next lint`: https://nextjs.org/docs/app/guides/upgrading/version-16
- Setting up the npm package and its trusted publisher: `references/npm-guide.md`, next to this plan
- npm trusted publishing: https://docs.npmjs.com/trusted-publishers
- npm workspaces: https://docs.npmjs.com/cli/v11/using-npm/workspaces
- TypeScript's `rewriteRelativeImportExtensions`: https://www.typescriptlang.org/tsconfig/#rewriteRelativeImportExtensions
