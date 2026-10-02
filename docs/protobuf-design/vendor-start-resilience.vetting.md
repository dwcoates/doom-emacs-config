# Vetting register — vendor-start resilience

## V1. A refused vendor start can be retried on the same shim

- Assumption: after StartSession answers `vendor_start_failed`, a second
  StartSession on the same shim process can succeed.
- Affected: the daemon retry loop in `Fleet.startSession`; if false, every
  retry must relaunch the shim instead.
- Verify: shim unit test that drives a start whose liveness probe times out,
  then a second start whose probe answers, on one engine instance.
- Status: CONFIRMED 2026-10-02 (shim tests "a start retried after its liveness
  probe timed out", fresh and resume, in test/engine/session.test.ts; no
  production change needed). A fresh start that wrote no rows mints a new id
  on retry; the first id was never announced, so nothing observes it.

## V2. A network outage presents as a retryable failure

- Assumption: a transient network failure during vendor bring-up surfaces as
  silence (liveness or init timeout), an early process/stream end, or an
  error result with a network/5xx/overloaded status — all labeled retryable.
- Affected: the shim's retryable/rejected labeling; a network failure that
  arrives as some other error result would be labeled REJECTED and not retried.
- Verify: forward pass — for each retryable classification, find the SDK path
  that produces it; reverse pass — enumerate every way the SDK reports a
  start error and check each lands in a deliberate bucket (the reverse pass
  finds silent omissions).
- Status: VERIFIED WITH FINDINGS 2026-10-02. Forward pass: liveness timeout,
  refused supportedModels, init silence, early end/throw, child exit, and
  error results with 408/429/5xx or network wording are RETRYABLE. Reverse
  pass found network outages that would be REJECTED: an error result with no
  status whose text matches the auth pattern ("OAuth token refresh failed:
  fetch failed", "API Error: Connection error."), unlisted no-status network
  wording, and a SessionStart hook that blocks because it failed offline.
  Mitigation: the start settles on supportedModels, which makes no API call,
  so an outage normally shows as silence or a child exit (retryable).
  Orchestrator ruling: with no HTTP status, network wording wins over auth
  wording (resilience is the goal); a spawn error with a structured errno
  (ENOENT/EACCES) is REJECTED because the errno is structured evidence, not
  text inference; an exit-coded refused resume stays RETRYABLE (telling it
  apart would mean inferring from stderr text; the ten-minute cap bounds it).
