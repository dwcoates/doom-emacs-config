# STOPPING POINT — daemon rebuild (overhaul/daemon), 2026-09-02

Supersedes the 2026-08-31 wind-down file entirely: everything that file
queued has landed. A cold resume picks up from here without re-derivation.

Governing documents, in reading order: `daemon/ARCHITECTURE.md` (package map,
seams, every landed ruling — read it first and in full), `daemon/AGENTS.md`
(flags, env contracts, test knobs, the wait-bound table, the conventions),
`daemon/integration/SPEC.md` (the integration suite's specification and its
"Settled behaviors"), `daemon/ERROR-ARMS.md` (the refusal-arm ledger), and
`docs/overhaul/daemon.md` (the durable rulings record, current through the
landing 7 relay).

## Board

- Branch `overhaul/daemon`, worktree `~/.config/doom-overhaul/daemon`.
- Unit: `cd modules/app/agent-repl/daemon && go build ./... && go vet ./... &&
  AGENT_REPL_FORBID_VENDOR_CALLS=1 go test ./... -count=1` — 42 packages, 3741
  tests, ~9 s, vet clean, `gofmt -l` clean.
- Integration: `TMPDIR=/tmp AGENT_REPL_FORBID_VENDOR_CALLS=1 go test -tags
  integration ./integration/... -count=1 -timeout 180s` — 539 tests, ~105 s.
  `TMPDIR=/tmp` is REQUIRED on macOS: `t.TempDir()` otherwise roots the state
  under `/var/folders/...` and `<state>/sock/<workspace-id>.sock` exceeds the
  103-byte unix socket path limit, so the daemon refuses the state root at boot
  before anything else runs.
- staticcheck (`./...` and `-tags integration ./...`) clean of U1000; one
  pre-existing S1016 style note in `internal/vocab/api.go`, ruled out of scope.
- Sweep leaked fixtures ONLY when no agent is running a suite:
  `pkill -9 -f agent-repl-integration-bin; pkill -9 -f
  '/private/tmp/ar[A-Za-z0-9]*/state'`, then remove `/private/tmp/ar*`,
  `/private/tmp/agent-repl-integration-bin-*` and
  `/private/tmp/agent-repl-*-{daemon,shim}-*.log` (find, not glob; /tmp is a
  symlink, so use /private/tmp).

## What is built

shimclient (spawn/supervise/redial/adopt), gitclient (fake-git tests only),
wsm (17 tables incl. the merge ledger, host_session_id, layout v3, Promote()),
dlog, sessionwatcher (routes to the five resolvers plus the lifecycle/link/
diagnostics sinks; AwaitFree/AwaitTurnEnd), resolve/{feed,footer,topbar,
sidebar,holds}, prompthandler + promptqueue + classifier (-fake heuristic,
vendor guard), the merge orchestrator (two methods, ledger id minted at
enqueue, parked route, test gate), drain, rollout (handover, adopt rendezvous,
relaunch engine, intent manifest, staleness bounce once per observed stamp),
the workspace verbs (create incl. one-shot/fork/parent, open/close/kill/nuke/
restart, tasks, priority, relays), account (determined config dir, transcript
porting), login (one pty per account root), externalbrowser, command-file
ingress (general verbs, quarantine at parse and at apply), health, the boot
sequence, the server (47 rpcs, publishers, asset origin, flush-on-accept, arm
mapping), the cmd graph wiring, and the deploy chain (`bin/deploy-all.sh` →
`(agent-repl-runtime-restart-await)`; `agent-shim/wire` deleted). Integration
harness + fakeshim + fakegit + 14 suites.

## Deviations from docs/overhaul/daemon.md (all recorded there or in ARCHITECTURE.md)

1. Merge recovery re-queues at the FRONT and re-runs from the queue tab
   (daemon.md override, 2026-08-29).
2. Shim relaunch: an interim sequential order was implemented because the shim
   held the workspace lock at startup. The project lead ruled the shim takes
   both locks inside StartSession (landed on overhaul/shim be119abbf); the
   engine is FLIPPED BACK to the prescribed prelaunch-then-wait and the
   fakeshim locks at StartSession. daemon.md "Ruling relay: shim relaunch vs
   the workspace lock".
3. `server.Deps.SessionFacts` is REQUIRED in `cmd/claude-repld/graph.go`
   (buildGraph refuses when unwired) and produced by
   `workspace.Fleet.HostSessionFacts` (host_session_id minted at session
   creation, persisted in wsm.Session; generation = fleet spawn count). The
   earlier `unwiredSessionFacts` null object is deleted.
4. Host arm publishing: `WatchHostWorkspace` uses a per-workspace STATE topic
   beside the event topic (compose-then-subscribe, so a late subscriber gets
   the state); `Server.PublishHostWorkspace` is exported and called by the
   fleet (session up/stopped/installed/resumed, link changes) and by merge
   publish/forget, deduping by `proto.Equal`. A workspace with a session record
   this daemon does not operate composes
   `existing{terminal|live{shim_attached:false}}` from the durable record.
5. Watch* transport-closed refusals (unknown/mismatched/unowned workspace,
   unminted/expired feed token, no login open) log INFO
   `daemon.refusal.transport_closed` via `server.TransportClosed`, never the
   unlanded_arm WARN. Typed LANDED refusals in workspace/close/login log INFO
   `daemon.refusal.typed`. Only `server.UnlandedArm` logs WARN
   `daemon.refusal.unlanded_arm`.
6. Connectivity truth per hop landed late: the server states host and web
   stream liveness to the footer and the topbar on every open/close edge;
   connected iff shim link + host + web are live.
7. Hibernation is a PARK: the host stays `live{shim_attached:false}` and the
   roster keeps an idle arm. A revival-pending hold whose bring-up FAILS is a
   loud DROP (daemon_hold.proto) — tombstoned, WARN per turn.
8. Fork: the daemon mints a fresh vendor session id, copies the parent's
   transcript under it and resumes; the parent is untouched (no shim fork arm).
9. Adoption (crash boot, handover) was attach-only against the durable record;
   landing 7's `session_started` re-announce makes it a pure attach.
10. Adopt* calls WAIT for the rendezvous (all participants succeed together);
    `not_yet_adopted` only when the caller's own context expires first.
11. Interrupt `confirm_required` counts live AGENTS only (a detached shell
    never raises the challenge).
12. `/clear` and `/compact` run as the turn they mint (`command_acted` is for
    `/model <arg>`); unknown slash text falls through to the vendor; a
    duplicate idempotency_key answers `duplicate_submission`.
13. ShimBuildSHA: the stamp file first
    (`agent-shim/claude/shim/dist/.built-sha`), `SHIM_BUILD_SHA` when absent; a
    present-but-blank stamp refuses. Flagged fragile: a built stamp silently
    outranks a test env.
14. The default-model fallback (`AGENT_REPL_DEFAULT_MODEL`, else `opus`) is
    retired by landing 7's optional `StartSessionFresh.model`.
15. Log-sink eviction on close evicts the HANDLE (the link and its target stay
    on disk); dlog durable targets live under `<state>/logs/`, never the OS
    temp dir.

## Dead-code pass (2026-09-02, ruled by TEAMLEAD.md "Dead code is hunted programmatically")

Three tools, run by a sonnet-medium agent in its own worktree: `staticcheck`
(U1000, both tag sets), `go test -coverprofile` across unit and integration,
and `golang.org/x/tools/cmd/deadcode` whole-program reachability from
`./cmd/claude-repld`.

METHODOLOGY FINDING, binding on anyone who repeats this: the coverprofile is
NOT a dead-code oracle for this daemon. The integration tests exec the real
`claude-repld`, the fakeshim and the fakegit as separate OS subprocesses, and
`go test -coverprofile` only instruments in-process code, so nearly all of
`cmd/claude-repld` and `internal/*` reads as a flat 0% no matter how
thoroughly the 534 integration tests drive it (only 7 functions in the whole
`internal/` tree show a nonzero integration hit, all harness-called setup
helpers). Whole-program reachability (`deadcode`) is the oracle that works
here; coverage is signal only for packages unit tests drive in-process. To make
coverage meaningful a future pass would have to build the daemon and the fakes
with `-cover` and collect `GOCOVERDIR`.

DELETED:

- `internal/resolve/topbar`: `SetPermissionModePicker` (method + interface
  declaration). No production caller; `OnSessionStarted` already serves the
  fixed switchable set directly.
- `internal/server`: `unwiredSessionFacts` and its method — the retired null
  object; `server.New` refuses when `Deps.SessionFacts` is nil and `graph.go`
  always wires `hostSessionFacts{fleet}`.
- `integration/harness`: `AllowAllWarnings` (`"*"`) and the branch honoring it.
  Zero callers; deleting the branch makes every test's `ExpectWarnings` list
  exact, which is the intent.
- `internal/daemonaddr`: `Read`/`read`. `daemon.addr` is never read back in
  production — a joining successor learns the incumbent's address from
  `--joining <addr>` on argv (rollout/spawn.go).
- `internal/envc`: `Contracts.Owned()` and `Contracts.ChildEnv()`. Every spawn
  site builds its env by hand (`shimclient/supervisor.go` `spawnEnv`).
- `internal/feedid`: `SubagentRowKey`, `PlanRowKey`, `PlanBubbleKey`,
  `MergeHeadRowKey`. Production constructs the identical `RowKey{}` literal
  inline at every real call site.
- `internal/merge`: `SelfRepoDir` (graph.go duplicates the
  `AGENT_REPL_SELF_REPO_DIR` override inline); `testgate.go`
  `suiteStateRunning` (never named).
- `internal/prompts`: `LoadAndSplice`. Every real call site does Load then
  `.Splice()`.
- `internal/resolve/holds`: `MergeDequeueOffer` and `MergeDequeueHeadline` —
  stale, not even a live duplicate: `internal/merge/tabs.go` `dequeueOffer`
  composes a different, workspace-name-specific sentence.
- `internal/resolve/feed`: `planState.turn` (never assigned, never read).
- `integration/harness`: `syncBuffer` + `Daemon.stderr` (a superseded
  pipe-based stderr capture; the real path writes to a file),
  `WriteDefaultShimProfile`, `WriteTranscript`, `Daemon.String()`,
  `IndexOfVerb`, `LogSubjects`, `SetDirty`, `ConflictedFiles`,
  `ExpectKillSession`, `ExpectHibernate`, `ExpectWatchBash`, `LockFiles`,
  `Repo.Commit`, `Repo.Head`.
- Test-only unused symbols and write-only fields across `internal/boot`,
  `internal/merge`, `internal/server`, `internal/sessionwatcher`,
  `internal/resolve/topbar` and `integration/support_test.go`.

KEPT WITH REASON (zero apparent coverage, but live):

- `internal/dlog/testlogger.go` (22 symbols) — test-only logging
  infrastructure used across dozens of `_test.go` files; unreachable from main
  by construction.
- `promptqueue.queue.waitForClassifications` — the WaitGroup test-sync helper,
  22 call sites across 5 files.
- The footer's `WithClock`, `WithMomentaryDwell`, `WithTokenAlarmThreshold`,
  `WithRateLimitNewsworthyThreshold`, `WithFeedIDEncoder` and the topbar's
  `WithClock`, `WithWarningCap` — documented test-injectable functional
  options; production always wants the real values.
- `shimclient.WithBackoff`, `WithKillGrace`, `WithLockProbe` — tested options.
- `sessionlock.SessionLockPath` — the contract's canonical home; the daemon
  deliberately never probes the session lock.
- `sessionlock.ProbeWithLog` — a tested logging wrapper.
- `vocab.OneofArmNames` — a reflection helper backing `RenderColors.AssertXArms`.
- `merge.Refused` and `workspace.AsRefusal` — the canonical error→arm
  extractors, exercised by 15 and 9 test sites.
- `resolve/footer.statusName` — the suite's status-to-string adapter.
- `internal/prompts.Wrap` (+`MetaOpen`/`MetaClose`) — the canonical sentinel
  scheme ARCHITECTURE.md describes, backing 13+ `StripSentinels` cases.

The last four rows are live-code/dead-code PAIRS: the canonical helper survives
only because a caller bypasses it with a near-duplicate. No tool can tell
"genuinely superseded" from "designed-to-be-shared-but-bypassed"; those were
read by hand and routed to a deduplication remediation (see below).

## Tests over 1 s (re-profiled at 971145abf)

Unit: none over 1 s.

| test | duration | why |
|---|---|---|
| TestFlushOnAcceptAcrossWatchKinds | 2.62s | table over every Watch* kind, one real daemon per kind |
| TestFooterLoadingStatusIsRetiredByADaemonSideDwell | 1.72s | real daemon-side dwell |
| TestFooterInterruptedStatusIsRetiredByADaemonSideDwell | 1.72s | real daemon-side dwell |
| TestSubscriptionInvariantAcrossWatchKinds | 1.08s | table over every Watch* kind, one real daemon per kind |
| TestAFileWithOneInvalidEntryAppliesNothing | 1.08s | command-file poll cadence + 500ms negative probe |

Run 8's eight 5 s rows are gone: each was over 1 s only because it was RED and
rode out `DefaultTimeout`. Nothing here justifies a looser bound; the AGENTS.md
wait-bound table stands unchanged.

## Skips that stay (each names its hook)

- `TestAnAbandonedQueuedMergeHasNoReachableCause` — `FeedMergeError` needs
  distinct evict/dequeue/abandon causes (only failed|abandoned exist).
- `TestADisplacedUserTurnIsResubmittedExactlyOnceAcrossADaemonBounce` — no
  crash-window hook.
- The Watch* kinds with no two-step driver inside
  `TestSubscriptionInvariantAcrossWatchKinds` (`WatchHostWorkspace`
  deliberately).

## Standing rules for whoever resumes

- Implementers are `opus-low`; rote, fully specified writes are `sonnet-medium`;
  nothing else. Fable only for fresh-context adversarial audits, and that loop
  is CLOSED at three rounds unless the project lead reopens it.
- No real git in any test: a fake `gitclient.Git` above the leaf, a scripted
  fake `git` executable first on PATH for the leaf itself, fixture data in the
  integration harness. No `git init`, no temp repositories, anywhere.
- No vendor calls: `AGENT_REPL_FORBID_VENDOR_CALLS=1` in every test process.
- No `time.Sleep` for synchronization. Timeouts stay TIGHT: `DefaultTimeout` is
  5 s and is never raised; a longer wait gets its own row in the AGENTS.md
  wait-bound table with a one-line justification.
- Zero WARNING records on green paths; `ExpectWarnings` lists are exact.
- Never weaken error handling; never amend a test without a stated contract
  reason.
- The daemon lead never edits `.proto` files: proto needs go to the project
  lead as concrete arm/field proposals.
- Implementer worktrees are `~/.config/doom-overhaul/daemon-agents/<slug>` on
  `overhaul/daemon-<slug>`, hand-created from the tip, merged back by the lead
  and deleted. A silent agent is not a dead agent: SendMessage round-trip
  before reaping one.
- Every scratch file in the shared scratchpad is prefixed `daemon-`.
- The implementers' common brief is at scratchpad
  `DAEMON-COMMON-BRIEF-overhaul-daemon.md`.

## Pointers

- Lead notes (chronological, every ruling): scratchpad `daemon-lead-notes.md`.
- Audit critiques (all folded in): scratchpad `daemon-audit{1,2,3}-critiques.md`.
- Run logs: scratchpad `daemon-integration-run{1..9}.log`; the >1 s profile:
  scratchpad `daemon-slow-tests.md`.
