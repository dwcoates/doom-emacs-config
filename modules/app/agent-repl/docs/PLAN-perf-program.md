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
  agent-repl, Emacs included (`/agent-repl-runtime hard-bounce`).
- Every bounce is a perf checkpoint: measure workspace startup time, webapp
  load time per workspace (first and repeat switch), and the other UX
  latencies in the perceptibility table below; remediate regressions and the
  worst offenders as we go.
- Every bounce is also a merge-queue recovery check: snapshot the queue, the
  running merge and every workspace with a turn in flight BEFORE the bounce;
  after it, verify every queued item continued where it left off and every
  agent interrupted mid-turn was resumed with its "continue" prompt. Any gap
  dispatches a fix agent.
- Branches land as they resolve. BEFORE every pick (owner, 2026-10-12): the
  branch is rebased onto master and its suites are run and fixed THERE, so no
  merge-queue fixer ever repairs this session's breakage. Full `bin/test-all.sh`
  runs are avoided while the host's one suite slot is contended; the touched
  systems' suites run directly instead.
- The live Emacs runtime is shared with doom-26 (startup and workspace-switch
  perf), which has PRIORITY: ask doom-26 before every hard bounce and wait for
  its go; never touch the runtime while it holds it. Startup/switch findings
  go to doom-26 for awareness; this session no longer investigates that area.
- The merge queue is PAUSED and its rollouts HELD; the owner resumes the queue.
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
- `/agent-repl-merge-queue hold-rollouts` / `release-rollouts` + awareness note.
- Also fixes the master-red elisp pin `merge-queue-action-arms-pinned`.
- After landing: hard bounce, then HOLD rollouts for the profiling program.
- The queue's existing `pause` / `resume` stay as they are (kept, unused).

### A2. Merge queue: top two race to land (owner, 2026-10-12) — landed (5620e6c75)
- A full pause (578ea375c): a paused queue advances nothing; a pause suspends
  what runs and the resume takes it on at its recorded step.
- Window 3 -> 2 landed (8e28c0471).
- Next: both of the top two rebase onto the TARGET (not chained), both run the
  full sequence; whichever passes first lands; before the post-landing
  restart the other is stopped, the queue advances, and the loser (now first)
  and the new second both restart from a rebase onto the target.
- Recovery must respect the window (resumed runs currently bypass it).
- Recovery resumes agents a stand-down cut mid-turn, under the hood (landed
  fb7b94832 + the Serving regression fix dcec14621); verified only once a
  bounce cuts an agent mid-turn (the debt is recorded by the daemon being
  stood down, so the first fixed bounce could not exercise it).

### I. Merge queue latency (analysis only, owner) — done (docs/investigations/2026-10-12-merge-queue-latency.md)
- ~75-80% raw inefficiency, ~15-20% restart handling. Top causes: master red
  2h+ (other sessions' landings), the ONE host suite slot behind every
  session's full runs at load 25-95, this session's ~60 direct picks moving
  master under passed gates, restart-cut gates rerunning from scratch,
  auth-failed fix turns counting as attempts. Of 19 failed/cut gate runs:
  11 load flakes, 6 master red, 2 restart cuts, 1 genuine conflict.
- Landed from it: the repair brief runs only the failing tests until they
  pass (5b362ef54), with a fallback handing an unrunnable-alone test to an
  investigating workspace (10727f9c4). Other levers await the owner.

### H. Test-writing guidance (owner, 2026-10-12) — running (branch `write-tests-skill`)
- `/agent-repl-tests` skill: test-perf pitfalls and how to avoid them without any
  coverage loss; a living document updated at every important perf finding.
- A performance-lessons section in each agent-repl system's AGENTS.md, also a
  living document.

### B. Test-run write volume (measured: a full run on the disk writes 25-34GB)
- B1 fake git hard-linked, not copied — landed (da3219969): ~3GB per daemon suite.
- B2 daemon test logs over a pipe to the harness — REJECTED and reverted
  (4e5d4c563): the one collector in the test process is the bottleneck
  under load, even with non-blocking daemon writes (logpipe-v2: 5-8 failures
  per copy against master's 1 in three under three-copy load). Recorded in
  /agent-repl-tests pitfall 2 (abbddd6e9). Cut record volume instead.
- B3 in-memory SQLite for test daemons that never restart on the same state;
  restart/handover tests keep the file, enforced at runtime — done on
  `perf-memstate` (570 of 790 integration daemon starts on memory; rollout
  and boot unit suites ~0 writes), landing next.
- B4 store mock: measured, nothing heavy left (store is already faked
  in-process where it is a dependency). Next lever found: integration.test
  itself writes ~550MB per run (per-daemon copies of the fake webapp dist and
  prompts dir: share or hard-link them).
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
- `/profile` deleted; `/agent-repl-profile` created with the living list of
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

- D4 startup: after the 13:40 hard bounce the daemon served in 1.6s, but no
  workspace got its go-ahead for 29s: the stale-sidecar restart's launchd
  kickstart took 29s (13:40:43.18 -> 13:41:12.20) with the sidecar itself
  silent; all 26 sessions were up 54s after Emacs relaunched. Fix running
  (branch `startup-sidecar`): kickstart cost, whether bring-ups must wait,
  why the sidecar reads stale on every bounce, the Emacs relaunch -600.
- D5 first show of a hidden page (diagnosis, e3436892c): switch -> painted
  p50 679ms / p90 1,477ms / max 2,619ms because the hidden page replays its
  push backlog (p50 23, max 153 records, ~27ms each) before it shows. Fix
  running (branch `hidden-page-coalesce`): hidden pages coalesce pushes per
  key and draw the latest state once on show; INFO records for resume,
  coalesced count, draw cost, page bytes. No visible change.
- D6 every opened workspace keeps a live hidden page: 32 WebKit processes,
  2.85GB RSS, 28.5% CPU in one snapshot (~89MB per page). Proposal: a small
  LRU of live pages (current + 2-3 likely next) warmed one at a time in idle
  time; parked pages load cold. OWNER RULING NEEDED (visible: a parked
  workspace's first switch is a cold load).
- D7 Emacs spends 80-180ms of elisp before each webview swap; ~118ms of it in
  the slowest case is unrecorded: add records, then cut it.

### E. CPU and memory (battery)
- E1 live daemon memory (diagnosis, e3436892c): footprint 551MB, peak 682MB,
  547MB Go heap. Retained by durable feed rows: wsm.db 332MB, 1,001 rows,
  ~327MB, of which 219 merge "tests" rows hold ~300MB (every suite's output
  inline, avg 1.4MB, max 9.6MB, re-marshalled and pushed to every reader on
  every counted test line, never retired). Fix candidates, ranked: (1) move
  suite output out of the row into the archive the gate already writes (row
  keeps counts, a tail and a link) — OWNER RULING NEEDED if the bubble shows
  the full output today; (2) retire durable rows of settled merge bubbles;
  (3) restore a workspace's durable rows only when a reader opens its feed;
  (4) cap Go heap headroom (SetMemoryLimit/GOGC) once the live set is small;
  (5) Go memory stats in daemon health at INFO. Also: logs dir 5.8GB with no
  seen retention policy; layout backups beside wsm.db ~1GB.
- E2 sidebar re-render logged every row on every render (26% of test daemon
  records): landed (40482a061). The feed's row_decision records are per-event
  branch choices logged once each, not render repeats; nothing to dedupe.
- E4 merge tests rows: each suite keeps a 40-line tail in its row and the
  full output is read from the gate's archive when shown
  (GetMergeTestSuiteOutput), drawn once after a brief busy state (owner:
  option A). Branch `tests-archive` on top of the unlanded
  `merge-queue-bubble-tabs`: settled run row 830KB -> 3KB, wsm.db+WAL
  1.55MB -> 0.71MB per 20k-line run. Lands with/after that branch.
- E3 whatever the collector ranks highest in CPU-seconds and RSS during the
  owner's normal use — PARKED (owner, 2026-10-12): UX first; and the
  collector needs the live runtime, which doom-26 holds.

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
| 2026-10-12 | e3436892c | investigation: first switch and daemon memory |
| 2026-10-12 | db277cff6 | merge pump reads a run's lease/chain base under the run's lock (data race) |
| 2026-10-12 | 8e28c0471 | merge queue window 3 -> 2 (head + one behind) |
| 2026-10-12 | 55a22a48e | hidden pages hold the newest roster instead of redrawing per push |
| 2026-10-12 | (11 picks) | rollout hold endpoint + /agent-repl-merge-queue hold-rollouts, release-rollouts |
| 2026-10-12 | (3 picks) | daemon test logs over a pipe, in memory |
| 2026-10-12 | (5 picks) | in-memory test state (DurableState opt-in) |
| 2026-10-12 | (17 picks) | profiling in every runtime + /agent-repl-profile |
| 2026-10-12 | 5b362ef54, 10727f9c4 | repair brief: failing tests only, until they pass; unrunnable-alone fallback |
| 2026-10-12 | dcec14621, fb7b94832 | Serving regression fix; under-the-hood resume of cut turns |
| 2026-10-12 | 578ea375c | a paused queue advances nothing |
| 2026-10-12 | 02afdccf7 (5 picks) | boot bring-ups beside the sidecar restart; sidecar spool glob; test build reports isolated |
| 2026-10-12 | 5620e6c75 | the top two race to land |
| 2026-10-12 | 40482a061 | sidebar logs a row's decisions only when they change |
| 2026-10-12 | 4e5d4c563 (4 reverts) | test log pipe reverted (stalled test daemons under load) |
| 2026-10-12 | 5ced835a4 | the shutdown tests' held gate is released when the test ends |
| 2026-10-12 | 64a42cf32 | race tests drain the queue instead of fixed pump counts (flake) |
| 2026-10-12 | abbddd6e9 | the log pipe recorded as a rejected approach |
| 2026-10-12 | 056137c2c (5 picks) | flake fixes: fake git atomic save; exit waits for landing deploys; opened-shim hang-up; adopted-shim stop race; prompt held across a shim death |
| 2026-10-12 | fd33737d7 | vendor starts capped daemon-wide (max(2, NumCPU/4)) |

## Bounce checkpoints

| When | Recovery | Startup | Notes |
|---|---|---|---|
| 2026-10-12 19:07 (after fd33737d7) | the turn the 18:52 bounce cut (daemon-run-tests-skill) RESUMED under the hood at 19:08:13; owed debt paid | 17 sessions up 25s after the bounce, 1 vendor retry | the 18:52 bounce had failed every session: a network outage (owner) plus every vendor starting at once kept load at 160 for the 10-min retry window; the cap now bounds it |
| 2026-10-12 15:34 (after 02afdccf7) | paused queue: 2 in-progress merges recovered as pending resumes, none started | daemon serving +0.9s; every workspace's go-ahead +1.8s (was ~30s); first session +3.5s; all 26 up 14.6s after the Emacs relaunch (was 54s) | -600 relaunch persists; sidecar kickstart still 29s (dead /tmp/artr mount) but no longer blocking |
| 2026-10-12 14:27 (after 8e28c0471) | 4 interrupted merges resumed at their steps (recovery resumed 3 in-progress merges although the window is now 2: resumed runs bypass the window); 4 agents interrupted mid-turn were NOT resumed with "continue" -> fix dispatched (branch `bounce-continue`) | daemon served 1.3s after boot; sidecar kickstart 29s again; first tabs +30s | Emacs relaunch -600 again |
| 2026-10-12 13:39 (after 839a72d9f) | 3 merges suspended at `tests`, all resumed at their step; requesters re-raised; chain rebuilt; no turns in flight | daemon served 1.6s after boot; first session +33s (29s sidecar kickstart); 26 sessions up 54s after Emacs relaunch | Emacs relaunch first attempt failed (-600), second succeeded |
