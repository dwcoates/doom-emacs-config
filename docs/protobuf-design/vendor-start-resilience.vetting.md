# Vetting register — vendor-start resilience

## V1. A refused vendor start can be retried on the same shim

- Assumption: after StartSession answers `vendor_start_failed`, a second
  StartSession on the same shim process can succeed.
- Affected: the daemon retry loop in `Fleet.startSession`; if false, every
  retry must relaunch the shim instead.
- Verify: shim unit test that drives a start whose liveness probe times out,
  then a second start whose probe answers, on one engine instance.
- Status: OPEN (deferred into the implementation wave).

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
- Status: OPEN (deferred into the implementation wave).
