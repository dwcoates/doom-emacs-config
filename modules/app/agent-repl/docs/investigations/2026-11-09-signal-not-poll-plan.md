# Signals, never timers: the daemon and its tests (2026-11-09)

Owner request (main workspace): remove every place, in production or test
code, where time stands in for synchronization; consolidate the daemon test
harness so far fewer daemons start. Implemented by `main (doom)` itself in
workspace `daemon-signal-not-poll`, landed with `/emergency-merge-queue-bypass`
once everything is done (owner instruction, repeated 2026-11-09).

## Landed

The first batch landed off-queue as `0.380.0` (2026-11-09); the second batch
landed the same way once everything below was done.

- `internal/kernelevent`: process exit (kqueue/pidfd), path watch
  (kqueue vnode/inotify), `Follow`, `Merge`, `Source`.
- Production polls removed: heldingress (250ms), deploy service restarts (1s),
  rollout manifest/arm/serving-release/adoption/carry follow, spawn address
  report, readiness probe, shimclient adopted exits, workspace lock release,
  root watch (1s), startingshim (20ms), the session watcher's refused-watch
  reopen timer (`shim.v1` `expected` on `WatchBash`/`WatchAgent`: the shim
  waits for an announced handle's rows), `selectionfollow`'s 50ms
  data_version read (the state file's commit signal), persistent wifi's 5s
  re-read (`netwatch` and the power preference files), and the write
  barrier's 1ms quiet tick (h2 frame headers on each counted connection,
  `publish.StreamPushes`).
- Test barrier (`AwaitIdle`, `internal/inflight`): every negative probe waits
  on it; addressed `elisp` pushes and `publish.Changes` wakes are counted.
  What it leaves to records by contract is in `daemon/AGENTS.md`.
- Test polls removed: every harness log and file wait, the fake shim's
  subscribed push (`push_session_update_when_subscribed`) and call counts
  (`await_count`), daemon.addr waits, feed re-opens (on the tail), routed
  record counts, session faults, and every unit-test negative window and log
  spin (`dlog.TestLogger.AwaitCount`, records the code now writes).
- Daemon starts: the scenario goldens run on one shared daemon's views.

## Kept, with the reason

- Retry pacing of failed external work: `rollout.retryAdoption`'s backoff, the
  handover reoffer delay, the hotspot failover's schedule, `RetryAccept`'s
  backoff. Each paces a retry of work that failed; none stands in for a
  signal.
- Samplers and schedules: CPU governor, process metrics, web footprint,
  repository divergence, the lock watchdog, the log cap scan, the daily and
  hourly sweeps, the pull-request poll (GitHub, owner ruling 2026-10-30).
- The wsm template database: a fresh in-memory state database opens in ~1ms
  after the process's first open (measured 2026-11-09); the first open's
  ~29ms is driver setup a template cannot skip.
- The harness's process-state polls and the 26 unverified tier-audit
  candidates: rows in `org/todo.md`.
