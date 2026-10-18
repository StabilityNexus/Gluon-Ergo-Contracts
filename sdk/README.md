# @stabilitynexus/gluon-ergo-sdk

> **Important**: 2.x supports Gluon v3 reactors only. Reactors of earlier contract versions need 1.x (`npm install gluon-ergo-sdk@1`, the legacy package).
>
> See [The Contract](../README.md#the-contract) for the on-chain mechanics and [Deployments.md](../Deployments.md) for live reactor deployments.

A TypeScript SDK for reading, quoting, and building transactions for [Gluon v3](https://eprint.iacr.org/2025/1372) stabilization reactors on Ergo mainnet.

- **Zero Node builtins or WASM**: browser-safe by construction.
- **Contract-tested**: off-chain math and transaction layouts match the v3 contract directly.
- **Typed deployments**: bundled deployment configurations with compile-time parameters.
- **Structured errors**: typed `GluonError` with actionable error codes.

---

## Installation

```bash
npm install @stabilitynexus/gluon-ergo-sdk@next
```

Peer dependencies: none. The SDK bundles and exports everything needed to read reactor boxes and build EIP-12 transactions.

---

## Choosing a Deployment

Deployments are identified by name:

```ts
import { DEPLOYMENTS, getDeployment, type GluonDeployment } from "@stabilitynexus/gluon-ergo-sdk";

// List all bundled v3 deployments
console.log(DEPLOYMENTS.map((d) => d.name));

// Get a deployment by name (throws UNKNOWN_DEPLOYMENT if not found)
const goldDeployment = getDeployment("gluon-gold-v3");
```

Custom or runtime deployment records can also be parsed and validated:

```ts
import { deploymentFromRecord } from "@stabilitynexus/gluon-ergo-sdk";

// Parses and validates a deployment record (throws INVALID_DEPLOYMENT if invalid)
const deployment = deploymentFromRecord(record);
```

---

## Client: Snapshot, Quote, and Build Flow

`GluonClient` is the primary entry point:

```ts
import { GluonClient } from "@stabilitynexus/gluon-ergo-sdk";

const client = new GluonClient({
  deployment: "gluon-gold-v3", // or pass a GluonDeployment object directly
  nodeUrl: "https://node.sigmaspace.io",
  // Optional:
  // uiFeeAddress: "9...", // single-signature P2PK address for 0.4 % UI fee
  // minerFee: 1_100_000n, // defaults to 1_100_000n (Fleet recommended min fee)
});
```

### 1. Fetching Chain State

`fetchChainState()` queries the node in parallel for the current height, the reactor box (verified by NFT and ErgoTree), the oracle pool box, and the optional buyback box:

```ts
const chain = await client.fetchChainState();
```

### 2. Protocol Snapshot & Analytics

`snapshot()` computes full state analytics against current oracle prices, including health status, leverage, cushion metrics, and volume history:

```ts
const snap = client.snapshot(chain);

console.log(`Reserve: ${snap.reserve} nanoERG`);
console.log(`Fusion ratio (q): ${snap.q}`);
console.log(`Reactor healthy: ${snap.healthy}`); // 0.50 <= q <= 0.98

if (snap.adjustPeg) {
  console.log(`Adjust Peg needed: direction ${snap.adjustPeg.direction}, ${snap.adjustPeg.steps} steps remaining`);
}
```

### 3. Quotes

Quotes compute expected inputs, outputs, fees, and the resulting reactor state and fusion ratio:

```ts
// Fission (deposit ERG, mint neutrons and protons)
const fissionQuote = client.quoteFission(chain, 10_000_000_000n);

// Fusion (redeem neutrons and protons, withdraw ERG)
const fusionQuote = client.quoteFusion(chain, 10_000_000_000n);

// Transmutation + (decay protons into neutrons)
const plusQuote = client.quoteBetaDecayPlus(chain, 100_000_000n);

// Transmutation - (decay neutrons into protons)
const minusQuote = client.quoteBetaDecayMinus(chain, 100_000_000n);

// Adjust Peg quote (when reactor is outside healthy range)
const pegQuote = client.quoteAdjustPeg(chain);

// Maximum beta decay amounts allowed by current reactor reserves and health bounds
const maxProtons = client.maxBetaDecayPlus(chain);
const maxNeutrons = client.maxBetaDecayMinus(chain);
```

### 4. Building Transactions

Builders select necessary boxes from user funding UTXOs and construct standard EIP-12 unsigned transactions:

```ts
const userBoxes = await ergo.get_utxos();
const changeAddress = await ergo.get_change_address();

const funding = { userBoxes, changeAddress };

// Build Fission
const tx = client.buildFission(chain, { erg: 10_000_000_000n, ...funding });
```

---

## EIP-12 Signing with Nautilus

```ts
// 1. Build transaction using GluonClient
const tx = client.buildFission(chain, {
  erg: 10_000_000_000n,
  userBoxes: await ergo.get_utxos(),
  changeAddress: await ergo.get_change_address(),
});

// 2. Sign transaction via EIP-12 wallet
const signedTx = await ergo.sign_tx(tx);

// 3. Submit transaction
const txId = await ergo.submit_tx(signedTx);
console.log(`Transaction submitted: ${txId}`);
```

---

## Error Codes

Every expected error thrown by the SDK is an instance of `GluonError` with a structured `code` property:

| Error Code | Description |
|---|---|
| `REACTOR_UNHEALTHY` | Fission, fusion, or beta decay attempted while fusion ratio $q$ is outside healthy range `[0.5, 0.98]`. |
| `WOULD_LEAVE_HEALTHY_RANGE` | A beta decay whose post-transaction ratio `qAfter` would fall outside `[0.5, 0.98]`. |
| `REACTOR_HEALTHY` | Adjust Peg attempted while the reactor is healthy. |
| `ORACLE_STALE` | Oracle box is 35 or more blocks old (or has a future creation height). |
| `BOX_AMBIGUOUS` | Multiple live unspent candidate boxes found (competing mempool updates). |
| `BOX_NOT_FOUND` | Required reactor or oracle pool box could not be found. |
| `BUYBACK_UNAVAILABLE` | A beta decay owes an oracle fee, but no live buyback box exists. |
| `AMOUNT_TOO_SMALL` | Action amount is zero or too small to release any tokens. |
| `AMOUNT_TOO_LARGE` | Requested amount exceeds reactor reserves or capacity. |
| `INSUFFICIENT_FUNDS` | Provided user boxes cannot cover required nanoERGs, tokens, and miner/fee outputs. |
| `INVALID_ADDRESS` | Address is not an Ergo mainnet address, or not single-signature (P2PK) where required. |
| `UNKNOWN_DEPLOYMENT` | Deployment name not found among bundled deployments. |
| `INVALID_DEPLOYMENT` | Deployment record is malformed, lacks required fields, or is not a v3 reactor. |
| `NODE_ERROR` | Node query failed (network failure, timeout, HTTP error status, or malformed JSON). |

Example error handling:

```ts
import { GluonError } from "@stabilitynexus/gluon-ergo-sdk";

try {
  client.quoteFission(chain, 10_000_000n);
} catch (error) {
  if (error instanceof GluonError) {
    switch (error.code) {
      case "REACTOR_UNHEALTHY":
        console.error("Trading paused: reactor is outside healthy range");
        break;
      case "ORACLE_STALE":
        console.error("Oracle price data is stale; wait for next oracle pool block");
        break;
      default:
        console.error(`Gluon error: ${error.code} - ${error.message}`);
    }
  }
}
```

---

## Historical Analytics & Pure Functions

For indexers, charts, and analytics without a client instance:

```ts
import { readReactor, reactorRegisters, oraclePrice, oracleAge, math } from "@stabilitynexus/gluon-ergo-sdk";

// Parse any reactor box directly
const { state, tokens } = readReactor(box);

// Calculate snapshot metrics directly from state
const snap = math.snapshot(state, minFee, price);
```

---

## License

This package is licensed under the [GNU General Public License v3.0](LICENSE) (`GPL-3.0-only`).
