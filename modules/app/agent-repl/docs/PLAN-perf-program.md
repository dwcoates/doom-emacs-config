# PLAN: the perf program (2026-10-12)

The owner's standing program for agent-repl performance: user-perceived
latency first, then CPU and memory (battery and OS resources), then test-run
cost. This file is the single place the pending work, the working rules and
the state of every stream are written down; it is updated at every landing.

## Working rules (owner, 2026-10-12; for this session and across compactions)

- Every landing on master is a CHERRY-PICK of a complete change's commits from
  its branch. No merge commits, no hand edits on master.
- The merge queue is bypassed for this program's own work (owner permission:
  the work is on the queue's speed and the runtime it deploys).
- Every complete change landed is followed by a FULL hard bounce of
  agent-repl, Emacs included (`/manage-agent-repl-runtime hard-bounce`).
- Every bounce is a perf checkpoint: measure workspace startup time, webapp
  load time per workspace (first and repeat switch), and the other UX
  latencies in the perceptibility table below; remediate regressions and the
  worst offenders as we go.
- Every bounce is also a merge-queue recovery check: snapshot the queue, the
  running merge and every workspace with a turn in flight BEFORE the bounce;
  after it, verify every queued item continued where it left off and every
  agent interrupted mid-turn was resumed with its "continue" prompt. Any gap
  dispatches a fix agent.
- Branches land as they resolve; each must pass its own suites repeatedly and
  the full suite (`bin/test-all.sh`) before its pick, with every failure
  either fixed or shown (with evidence) to belong to another session's change.
- Logging runs at `info`. A measurement that needs `debug` is a logging gap
  to fix, not a reason to raise the level.
- Mocking is approved (owner, 2026-10-12) for test-run write volume: the
  daemon's test logs, test SQLite state and the store as a dependency. The
  store's own suites keep the real store.
- Measurements report numbers (bytes, CPU-seconds, ms), never "less".

## Perceptibility budgets (rank latencies by how far over budget they are)

| Interaction | Budget |
|---|---|
| Keystroke echo / input | 50ms |
| Switch to an already-loaded workspace | 100ms |
| First switch to a never-shown workspace | 100ms (owner-flagged; measured 1s+) |
| Workspace open | 300ms |
| Prompt submit to first visible output | 1s |
| Background deploy / bounce | invisible unless it blocks input |

## Streams

Status values: queued, running, landed (commit), blocked (reason).

### A. Merge-queue rollout hold — running (branch `rollout-hold`)
- `UpdateMergeQueue` arms `hold_rollouts` / `release_rollouts` (+ errors
  `rollouts_already_held`, `rollouts_not_held`); the hold is durable across
  restarts and handovers; while held a landing logs "rollout held" at INFO and
  tells no deploy.
- `/merge-queue hold-rollouts` / `release-rollouts` + awareness note.
- Also fixes the master-red elisp pin `merge-queue-action-arms-pinned`.
- After landing: hard bounce, then HOLD rollouts for the profiling program.
- The queue's existing `pause` / `resume` stay as they are (kept, unused).

### B. Test-run write volume (measured: a full run on the disk writes 25-34GB)
- B1 fake git hard-linked, not copied — landed (da3219969): ~3GB per daemon suite.
- B2 daemon test logs over a pipe to the harness, in memory; written to disk
  only for a failed test — running (branch `perf-logpipe`).
- B3 in-memory SQLite for test daemons that never restart on the same state;
  restart/handover tests keep the file, decided mechanically — running
  (branch `perf-memstate`).
- B4 store mock built once per suite where the store is a dependency —
  running (branch `perf-memstate`).
- B5 attribute the remaining ~half of writes my sampler cannot see (root,
  kernel, short-lived processes): needs a root `fs_usage` capture (owner runs
  the prepared script).
- B6 the SSD fallback when the RAM disk cannot be made should refuse to run
  rather than write ~30GB to the SSD — queued (owner to rule).

### C. Profiling everywhere + the profiling skill — running (branch `perf-profiling`)
- Runtime start/stop of CPU and heap profiling in the daemon, shim-store,
  shim-sidecar, the shim (node), the webapp and Emacs, all into
  `~/.cache/agent-repl/profiles/<session>/`; off costs nothing.
- `bin/profile-agent-repl.sh start|stop|status|report`: per-process CPU and
  RSS sampling, top frames per runtime, and the UX-latency table from INFO log
  edges ranked against the budgets above; a `--startup` mode for the startup
  assessment (D2).
- `/profile` deleted; `/profile-agent-repl` created with the living list of
  perf-sensitive areas (standard entry format) and the rule to add or update
  an entry whenever the owner shows interest in an area.

### D. User-perceived latency (UX first)
- D1 first switch to a never-shown workspace takes 1s+ (owner-flagged):
  diagnosis running (branch `perf-diagnosis`); then (a) preload pages before
  the switch, one at a time in idle time, likely-next first, never freezing
  Emacs; (b) load only the most recent feed page per workspace and remove any
  other first-load boot work or daemon wait that blocks the page.
- D2 startup assessment (lead only): launch a real Emacs.app, time launch to
  every workspace loaded, then switch through every workspace timing first vs
  repeat switch and page load. Run at every bounce checkpoint.
- D3 workspace open, switch, submit-to-first-output: measured at every
  checkpoint from the collector's table; worst-over-budget first.

### E. CPU and memory (battery)
- E1 live daemon RSS ~509MB: attribution running (branch `perf-diagnosis`).
- E2 sidebar re-render logs every row on every render (26% of daemon records
  in tests; noise in debug windows): log a row's decisions only when they
  change; same for the feed's per-row decisions (11%) — proposed, owner
  ruling pending.
- E3 whatever the collector ranks highest in CPU-seconds and RSS during the
  owner's normal use.

### F. Master reds — running (branch `master-reds`)
- background-harness scan flags a plist comment from 8ade38a7a.
- ERT `itest-verbs-show-sends-the-show-arm-then-selects` timeout under load.
- shim "durable log sink poisoned > states the loss on WatchSession" under load.
- daemon `TestWatchFeedTailsExactlyAfterTheOpenedPageWithNoGapOrOverlap`
  re-delivered a row under load (possible production feed race).
- `TestAMergeRunningThroughAHandoverResumesInTheSuccessorAndLands` log-await
  deadline (known open flake).

### G. Test speed (earlier goal, stopped by the owner; kept for reference)
- Best clean full run 202.6s vs the 479s baseline (58%); coverage within 0.4
  points. RAM-disk I/O throttling of utility-tier I/O is the largest known
  lever (37% of daemon-suite slot-seconds); the root hunt needs `fs_usage`.
- The disk manager (diskarbitrationd, pid 362) is wedged; full runs fall back
  to the SSD until the owner restarts it
  (`sudo launchctl kickstart -k system/com.apple.diskarbitrationd` or reboot).

## Landed log

| Date | Commit | Change |
|---|---|---|
| 2026-10-12 | da3219969 | fake git hard-linked into each daemon's PATH |
| 2026-10-12 | 839a72d9f | idle sweep cannot hibernate a shim whose start is under way |
