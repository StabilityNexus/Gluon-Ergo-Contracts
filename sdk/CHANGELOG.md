# Changelog

## 2.0.0-next.1 (2026-10-18)

- Initial pre-release of `@stabilitynexus/gluon-ergo-sdk` supporting Gluon v3 reactors on Ergo mainnet.
- Integrated `GluonClient` with deployment-aware reactor box parsing, mempool lookups, and fresh oracle verification.
- Analytics and snapshot engine (`snapshot()`) providing reserve ratios, leverage, crash cushions, and shifted daily volume buckets.
- High-level quotes and transaction builders for fission, fusion, transmutations (beta decays), Adjust Peg, and Update Treasury.
- Automatic input selection and change handling conforming to v3 transaction layouts.
- Pure historical analytics functions (`readReactor`, `reactorRegisters`, `oraclePrice`, `oracleAge`, `math`).
- Structured `GluonError` error codes for clear UX and error handling.
- Browser-safe zero-Node-dependency build without WASM requirements.
