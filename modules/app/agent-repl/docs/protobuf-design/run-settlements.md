# GetRunSettlements: a settled run is never tracked again

A detached run that settled can never be tracked again, or concluded LOST, by
any sidecar process, including one started after the settle. The settle is
durable in exactly one place, the run's `detached_work` row, and the sidecar
had no verb to read it.

## The defect (2026-09-30, workspace footer-activity-updates)

- Background Bash run `bs38ysohm`, spawning call
  `toolu_01XEyqbMGgxwBUbC65ofDkHx`, finished normally.
  - At 18:55:30 the sidecar read `EXIT=0` and logged `lost-policy` "run settled
    by a terminal read from its own file".
  - The store took the file-plane terminal: ledger key
    `bash:toolu_01XEyqbMGgxwBUbC65ofDkHx:terminal`, write_seq 351590, at source
    offset 1118, in the same transaction as the cursor advance to 1118.
- Two deploys restarted the sidecar (19:18:34 and 19:22:33).
  - Each new process re-claimed the spool, resumed it at cursor 1118 (its end,
    past the marker), read nothing, and logged "tracking detached run
    kind=shell-spool last_activity_ms=1790808930576" (19:19:07, 19:23:04).
- At 19:27:05 the silence window concluded `went_silent` and minted a LOST
  terminal (write_seq 373625).
  - The store's file-plane supersession let it replace the real terminal.
  - `detached_work.ended_at_ms` became 1790810825213, the LOST instant.
- Root cause: every way a run settles lived only in the sidecar's memory.
  - The four ways are `settling`, `concluded`, `stopped`, and a LOST
    conclusion.
  - The restarted process re-derived tracking from files and cursors, and a
    terminator behind the cursor is never read again.
  - The same was true of an agent run its notification concluded and a run a
    person stopped.

## Landed shape

1. `store.v1` rpc `GetRunSettlements`, under Recovery in `service.proto`
   (`endpoint_get_run_settlements.proto`).
   - Request: `repeated string run_ids`.
     - Each is a spawning call's activity id, which is the `detached_work`
       row's `origin_unit`.
     - It must be non-empty, every id must be non-empty, and it is unscoped,
       like `GetShellRunClaims`.
   - Success: `repeated RunSettlement settled`, where `RunSettlement { string
     run_id = 1; int64 ended_at_ms = 2; }`.
   - Failure: `detail` plus `oneof kind { invalid_request{field} |
     storage_failure }`, mirroring `GetShellRunClaims`.
   - No terminal arm: nothing acts on how the run ended, only on whether it did.
2. Why absence means "not settled".
   - The store answers a run only when every row its origin unit locates has
     ended.
   - A run with no row yet, or with a live row, is absent.
   - "Not settled" is the honest reading of both: the file plane can observe a
     spool before the stream plane announces it, so "no row" is ordinary.
   - Absence is never an error and never a guess that the run ended.
   - A store that cannot answer is the failure arm, never an empty success.
3. Store guard (lead approval, 2026-09-30): a file-plane LOST terminal over a
   run whose row has already ended is refused whole.
   - It answers `invalid_request` naming `entries[i]`, at site
     `lost_over_settled`.
   - The original terminal and `ended_at_ms` stand.
   - It is recorded once at ERROR as an invariant violation.
4. Sidecar (`settle.go`).
   - A watched detached run is queued, and every `watchTargets` pass asks once
     for all queued runs.
   - A run the record holds as ended is settled (`stale.SettledByRecord`, one
     `lost-policy` INFO record); any other is observed.
   - A store failure leaves the runs untracked, with one `run-settlements`
     ERROR, until the next pass.
   - A sweep's conclusions are asked about before any LOST is minted.
   - Every in-process settle goes through `settleRun` into the tracker's
     settled set, and observing a settled run panics.
   - The corrupted `bs38ysohm` row is not hand-repaired.
