# Owner 17 (section H, failure arms) — STATE

Branch overhaul/int-play-17, rebased onto overhaul/integration 3c35dbbd5.
Owner is now opus-medium (the fable owner is gone); edits are made directly.

## Authored (796a13cbc, rebased)
- e2e/playtest_17_failure_arms_test.go — TestPlaytestFailureArms: one world, one
  loop over 29 rows (17 `!fail-*`, 12 `!api-*`). Per row: submit, await the
  newest root-feed `turnEnded` row by `[data-arm]` + exact `.turn-ended-cause`
  + exact `.turn-ended-vendor`, await the fate of the work in flight, await a
  settled tab arm, capture. Artifacts under playtest/17-failure-arms/.

## Runs
- run1 (scratch owner-17/run1.log): RED at row 20 (`!api-401`), 19 captures
  taken. Root cause found and fixed — see below.

## Defect fixed at the source
The vendor's failure reaches the feed resolver TWICE: the sidecar tailing the
session transcript's `system:api_error` line, and the shim's own stream
terminal. `resolver.addEvidence` attached the first to whatever turn was in
flight, and `erroredOutcome` appended it to the headline — so an `!api-*` turn
drew its arm's sentence alone or with " (a vendor request failed mid-turn and
the turn went on: …)" depending on a few-millisecond schedule. Measured in
run1: evidence lost by 17ms on `api-429`, won by 3ms on `api-401`.
Fixed in daemon/internal/resolve/feed: evidence carries its origin
(`turnEvidenceLine`), and a terminal never restates the api failure it ended
on. A DIFFERENT mid-turn api failure still rides.
- unit: internal/resolve/feed/turnended_test.go — three tests (both producer
  orderings; the specific negative).
- integration: integration/feed_test.go — two tests through the real daemon.

## Reviewed
All 19 run1 PNGs read against MANIFEST.md; every one matches its sentence.

## Filed for others (not fixed here)
- daemon integration `TestAParkedWorkspacesFooterIsIdleAndTheIndicatorReportsNoFault`
  failed once under the full `-parallel 8` suite (35.10s, "stream ended before
  the revived footer back on idle.done", plus an undeclared warn
  `daemon.sessionwatcher.turn_end_withheld: a terminal arrived before the main
  agent was named"). 3/3 green in isolation at 2.4s. Load-sensitive, and
  unrelated to this branch's change (its terminal is a SUCCESS frame).
- The sandbox image `latest` was rebuilt mid-flight by another owner
  (89ea5c14318a -> 8096de2293eb) while the lead's brief named the old digest.
- Every playtest picture's Emacs frame is ~1280x835 on a 1280x1024 screen, so
  the bottom ~19% of every capture is bare X root. Shared substrate
  (`playbook.prepareFrame`), not section H.

## Next
1. run2 in flight; expect all 29 rows.
2. Two consecutive green runs, then delete this file in the last commit.
