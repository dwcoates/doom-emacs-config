# Streaming reveal window: vetting register

## 1. The vendor's fragment cadence is steady enough per model to predict

- **Assumption.** For one model and one block kind, the gap between
  consecutive streamed fragments is stable enough that a recency-weighted
  average of the last 25 predicts the next gap well.
- **Affected.** `frontend.v1.FeedResponseRevealWindow.expected_gap_ms` and
  the daemon's 25-sample, 0.85-decay window. If the cadence is bimodal or
  heavy-tailed, the weighting or the window size changes, not the contract.
- **How to verify.** Log the gap per fragment (the daemon records
  `daemon.revealpace.observed` at DEBUG with `gap_ms`, `model`, `kind`),
  collect a day of live use, and compare each sample against the window's
  prediction just before it.
- **Status.** OPEN.
