# Remediations of Security Review Findings

Review date and time: 2026-10-11T02:51:42Z

## Remediations

### Remediation of Finding 1: Beta decays can push the reactor out of the healthy range, letting holders force permissionless peg adjustments

- [ ] This remediation implements the security review's recommendation for this finding.
- [x] This remediation addresses the finding in a way that differs from the security review's recommendation.

Remediation commits:
- [bf83391](https://github.com/StabilityNexus/Gluon-Ergo-Contracts/commit/bf83391bce4108e0366700c4eeea13dd257d58d5)

Description: Added a post-transaction health check, `isHealthyAfter`. It computes the fusion ratio `qOut` from the output box's neutron supply and reserve at the current adjusted price, and requires it to be within `[0.50, 0.98]`. BetaDecay+ and BetaDecay− now require both `isHealthy` (before the trade) and `isHealthyAfter` (after the trade), so a beta decay can no longer move the reactor outside the healthy range. This implements item 1 of the review's recommendation. Item 2, rate-limiting Adjust Peg, was not implemented: Adjust Peg still has no cooldown. Fission and fusion keep only the before-trade check; comments added in the commit state that they do not change the ratio.

### Remediation of Finding 2: Oracle data input is not authenticated in fission and fusion, so the healthy-range gate can be bypassed

- [x] This remediation implements the security review's recommendation for this finding.
- [ ] This remediation addresses the finding in a way that differs from the security review's recommendation.

Remediation commits:
- [503e86a](https://github.com/StabilityNexus/Gluon-Ergo-Contracts/commit/503e86a0bf98b10134c4925427e59bc4b1c2f393)
- [16e33af](https://github.com/StabilityNexus/Gluon-Ergo-Contracts/commit/16e33af207aca62b07eadfe26ad20e730073acbb)

Description: Added `cOracle` to the fission and fusion conjunctions (`503e86a`). Both operations now require data input 0 to be the oracle pool box, with the correct pool NFT and at most 34 blocks old, so the price that `isHealthy` relies on can no longer be forged. `16e33af` removed a trailing comma that `503e86a` had left after `cOracle` in the fission branch.

## Non-remediated findings

None.
