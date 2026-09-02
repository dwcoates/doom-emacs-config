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
  integration ./integration/... -count=1 -timeout 180s` — 539 pass, 4 skip,
  ZERO reds and zero undeclared warnings, 105-133 s.
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

## Landing 7 (merged and adapted, 2026-09-02)

Protos ab7e681f2, bindings c10714a41, relay in `docs/overhaul/daemon.md`
"Landing 7 relay". All five adaptations landed:

1. `SubmitPromptError.bubble_refused{detail, kind}` replaces `server.UnlandedArm`
   for both bubble refusals. `workspace.ArmShimAgentBusy` names the shim's
   `UpdateAgentFailure.agent_busy`; `server.bubbleRefused` maps
   not_deliverable and agent_busy onto the landed arm through a new
   `nestedArm` field value, and `setArm` gained nested-oneof selection that
   REFUSES an arm the message does not carry (it degrades to `UnlandedArm`,
   loudly, rather than arriving with an empty kind). The by-design red is
   green.
2. `StartSessionFresh.model` is optional. The `DefaultModel` fallback,
   `AGENT_REPL_DEFAULT_MODEL` and the built-in `opus` are deleted, including
   the AGENTS.md env row; `shimclient/validate.go` no longer requires the
   field. The effective model is read from `SessionStarted.effective_model`.
3. `WatchSessionResponse` is a `oneof frame`. `shimclient.WatchSession` yields
   the response; an unset frame RAISES. `sessionwatcher` splits fact uptake
   into `applySessionStartedLocked` + `adoptLiveWorkLocked`, ignores a repeat
   at DEBUG (never a WARN) and records an armless frame at ERROR.
   `Session.Started` is optional and `Fleet.watchInstalled` no longer
   synthesizes a `SessionStarted` from the durable record: adoption is a PURE
   ATTACH. The fake shim re-announces on every watch.
4. `CloseWorkspaceBlocked` carries all five fields. `closeBlocker` computes
   every blocker instead of returning at the first, so all four counts ride
   the refusal and the leading reason only picks the sentence; `summary` IS
   `footer.CloseBlocked.Detail`, so the footer's activity line and the
   refusal share one composer.
5. `FeedMergeAbandoned.summary` carries the abandon cause.

ERROR-ARMS.md: the landing-6 bubble row, the "agent_busy has no producer" gap,
the "CloseWorkspaceBlocked is an EMPTY message" note and both landing-7
proposals are deleted. No remaining row lost its producer.

## Duplication remediation (2026-09-02)

The dead-code pass's live-code/dead-code pairs, resolved:

- `internal/server/refuse.go` routes through `workspace.AsRefusal` and
  `merge.Refused`. `merge.Refused` was RESHAPED from `(string, bool)` to
  `(*RefusalError, bool)` to match `workspace.AsRefusal`: the arm-only shape
  dropped `RefusalError.Reason`, which `refuse.go` needs for the arm's own
  text and detail, and that is precisely why it had no production caller.
  Behavior at every call site is unchanged.
- `internal/workspace/oneshot.go` uses `prompts.Wrap`/`MetaOpen`/`MetaClose`;
  the duplicate local sentinels are deleted. There is no import cycle
  (`internal/prompts` has no internal dependencies). The emitted bytes are
  pinned against the raw cross-system literal, not against the constant, so
  the wrapper cannot drift from what the webapp and the elisp side strip.
- Every workspace lock probe in `internal/boot` and `internal/workspace` goes
  through `sessionlock.ProbeWithLog`: held/free at DEBUG, could-not-tell at
  ERROR, an underivable lock path at ERROR (it previously returned silently).
  No `ExpectWarnings` list changed.
- `shimclient.WithLockProbe` is WIRED in `cmd/claude-repld/graph.go`. It was a
  real defect: `witnessAdoptedDeath` returned early on a nil probe, so an
  adopted shim whose socket was gone was redialed forever, no `ExitInfo` was
  ever published and the workspace stayed wedged on a process that was not
  there. The graph already owned the identical probe and handed it to rollout;
  it simply never handed it to the supervisor. Only `StateFree` is death —
  `StateUnknown` is not, and a probe error is surfaced and redialing
  continues, so the AGENTS.md "a probe that could not tell is never read as
  free" rule holds.
- `RenderColors.AssertRosterStatusArms`/`AssertMergeGlyphArms` are called by
  `sidebar.New` (the hand-rolled copy it replaced checked `merge_glyphs` in
  one direction only, so this ADDS a failure path, pinned).
  `AssertFooterStatusArms`/`AssertFooterAllowanceArms` are called by
  `footer.New`, which now takes the vocabulary at all — the footer owns every
  `FooterStatus.status` and `FooterAllowance.status` arm, and had no check
  whatsoever. `vocab.OneofArmNames` stays test-only infrastructure, and now
  also pins the hardcoded `statusArms`/`allowanceArms` lists against their
  proto oneofs so an arm landing in the contract cannot escape the assertion.
  No vocabulary drift was found.

## Remaining items

- RESOLVED (project lead's ruling, 2026-09-02).
  `TestAnAbandonedQueuedMergeHasNoReachableCause` is un-skipped, renamed
  `TestKillingAWorkspaceAbandonsItsQueuedMergeWithTheCloseAsTheCause`. The two
  missing producers were built: `merge.OnWorkspaceClosed`, called by both
  teardown verbs (`CloseWorkspace` still refuses outright while a merge is
  queued, so Kill/Nuke are the one door such a workspace leaves through), and
  `recoverWaiting`, which abandons a merge the restart cannot put back on its
  queue under the daemon-shutdown cause. `AbandonCause` declares all four
  causes beside the one sentence each draws, and `publishAbandoned` records
  the abandonment at INFO keyed by the cause. No proto changed.
- RESOLVED (project lead's ruling, 2026-09-02).
  `TestADisplacedUserTurnIsResubmittedExactlyOnceAcrossADaemonBounce` is
  un-skipped, and a second test covers the double-boot edge. Both blockers were
  built: `merge.recoverDisplaced`, a boot sweep that resubmits every turn still
  marked displaced under `PROMPT_ORIGIN_MERGE_DISPLACED_TURN_RESUME`, and
  `merge.Deps.PauseAfterCapture`, a test-only pause (nil in production, wired
  only from `AGENT_REPL_MERGE_PAUSE_AFTER_CAPTURE`) that holds a run in the
  window between the capture and everything that would close it. Exactly-once
  is arbitrated by the database: `wsm.ClaimDisplacedTurn` clears the mark under
  `WHERE displaced = 1` and reports whether THIS caller took the record, so the
  merge's own release and the sweep can never both put one turn back. No proto
  changed. The original reading, kept for the trace:
- `TestADisplacedUserTurnIsResubmittedExactlyOnceAcrossADaemonBounce` stays
  skipped, and the earlier reading of WHY was wrong. The blocker is not the
  missing crash-window hook: THE BOUNCE-CROSSING RESUBMISSION HAS NO PRODUCER.
  `wsm.Turn.Displaced` is written by `workspace.CaptureDisplaced` and read back
  by nothing outside `wsm/turns.go`'s own scan and insert. `merge/recover.go`
  re-runs an interrupted merge, but that second run's `CaptureDisplaced` finds
  nothing in flight (the first run already killed the turn), so
  `resubmitDisplaced` no-ops. A turn displaced by a merge that then crashes
  stays marked displaced forever and is never put back. Un-skipping needs a
  boot-time recovery that resubmits every still-marked displaced turn exactly
  once with `PROMPT_ORIGIN_MERGE_DISPLACED_TURN_RESUME` and clears the mark in
  the same step. Behavior question for the project lead, not a test-hook ask.
- The merge ENDS the displaced user turn (KillTurn) after capturing it, then
  resubmits exactly once at lease release. daemon.md says only "captured
  durably and resubmitted exactly once"; the rationale for ending it is that a
  merge driving the session under a still-live user turn is incoherent. Owed
  to the project lead as an FYI confirmation.
- ShimBuildSHA resolution is flagged fragile: a built stamp file silently
  outranks a test `SHIM_BUILD_SHA` env.
- Coverage is not a dead-code oracle here until the daemon and the fakes are
  built with `-cover` and `GOCOVERDIR` is collected (see the methodology
  finding above).

## The integration warning sweep is unconditional (2026-09-02)

The zero-WARN-on-green-paths ruling was enforced only for tests that opted in:
the sweep was installed by the FIRST `ExpectWarnings` call, so a test that
never called it got no warning assertion at all and could emit any number of
WARN records and stay green. Two tests were doing exactly that.

`StartDaemon` now registers the sweep for EVERY harness daemon with an empty
declared set, and `ExpectWarnings` only widens an already-armed sweep. It is
registered before the daemon's own kill and cancel cleanups so it runs last
and reads a complete log, and it reports through `t.Errorf`, so it still runs
after a test has already failed for another reason — one run reports every
problem rather than hiding the warnings behind the first failure. There is no
escape hatch; `AllowAllWarnings` stays deleted.

Arming it turned 73 tests red in one run. Every one was triaged: the records
that are evidence on a failure path the test deliberately drives (shim death,
link sever, staged merge conflict, an abandoned queued merge, deliberately
corrupted state rows, provoked refusals, fed anomalies, restart consequences)
are DECLARED, 135 declarations across the suite, each with its reason. One was
a real defect and was fixed in production rather than declared away:
`daemon.merge.answer_dequeue` moved from WARN to DEBUG, because both arms of
`AnswerDequeue` are the user working an offer the daemon itself raised and the
keep arm was already DEBUG — the work actually abandoned still gets
`dropQueued`'s own WARN, so no coverage was lost. No other record was moved,
weakened, or deleted.

Also fixed: `drain_rollout_test.go` compared a millisecond-truncated wire
stamp against a nanosecond-precision deadline, so a drain that fired inside
the deadline's own millisecond read as strictly before it. Both bounds are now
compared in milliseconds, the wire's own precision. That closed
`TestScheduledDrainFiresAndAnnouncesShutdownWithTheScheduledDrainCause`, which
runs 5/5 green.

## Intermittents closed (2026-09-02)

Three failures that were measured at the SAME rate before and after the
landing-7 work, so all three were pre-existing, not regressions:

1. `TestARevivalTimeHeldPromptCarriesTheSessionStartingHoldAndRefusesRelease`
   (3/20) was a DAEMON correctness bug, not a timing artifact. A hibernation's
   lease is dropped as the hibernation completes, while the workspace it parked
   still has no shim. `promptqueue.OnLeaseChanged` read "no lease stands" as
   "nothing holds this prompt": it un-stamped the surviving `session_starting`
   holds and delivered them in line, reviving inside `deliverHeld`. A release
   landing between the un-stamp and the revival found no hold stamp and no
   session watcher, and answered the UNTYPED `UpdateHeldPromptError.no_session`
   about a workspace that was in fact still coming up — an unlanded generic arm
   where the landed typed `release_refused` describes the state exactly.
   `OnLeaseChanged` no longer un-stamps a hold on a session-less workspace: the
   hold stays `session_starting` and the bring-up is handed to the same
   background revival a fresh submission takes, and `Release` additionally
   consults the bring-up itself, so the window between a hold's release and its
   delivery still answers `release_refused`. 0/20 after.
2. `TestAdoptWebWorkspaceRefusesParticipantNotExpectedForAClientNotOpenAtAnnouncement`
   failed only under full-suite load: it subscribed to the daemon stream AFTER
   triggering the handover. The daemon-level push topic replays only its own
   process's latest value to a new subscriber, and the incumbent tears that
   process down as the handover completes, so under load the subscription
   opened too late to ever see `shutdown_announced`. Fixed in the test's
   driving — subscribe first, then trigger.
3. `TestALandedMergesLedgerRecordsEachTabsInterval` (3/40 under GOMAXPROCS=1)
   was NOT the millisecond-truncation shape it resembled. A landing tears the
   merged workspace's worktree down, and `OpenFeed`/`WatchFeed` resolve the
   workspace's log sink by stat-ing that directory; the test watched the root
   feed after `MergeWorkspace`, racing the teardown. Same fix: subscribe before
   enqueueing. 0/40 after.

Nothing was fixed with a sleep, a retry, or a widened timeout.

FOLLOW-UP OWED: the subscribe-after-trigger shape appears at roughly ten more
sites in `integration/merge_test.go` (`root := f.watchRootFeed()` after
`MergeWorkspace`). Only the one named test was fixed; the rest are latent
instances of the same defect and deserve a sweep.

## The four integration skips at this tip

1. `TestAnAbandonedQueuedMergeHasNoReachableCause` — BEHAVIOR NOT IMPLEMENTED,
   not a prerequisite. `internal/merge/queue.go` documents THREE distinct ends
   (evict, dequeue, abandon), but `dropQueued` has only TWO callers: Evict (the
   operator's) and the dequeue release (the user's). No call site anywhere
   raises a queued merge's OWN give-up. Landing 7's `FeedMergeAbandoned.summary`
   did NOT un-skip it, and neither would the distinct `FeedMergeError`
   per-cause arms: a PRODUCER has to exist first. To un-skip: implement the
   self-abandon end (the "workspace closed" and "daemon shutdown" causes the
   landing-7 relay names have no production call site in `internal/merge`),
   then the per-cause arms. This is a behavior question for the project lead,
   not a proto ask.
2. `TestADisplacedUserTurnIsResubmittedExactlyOnceAcrossADaemonBounce` — the
   BEHAVIOR IS **NOT** IMPLEMENTED (corrected 2026-09-02; see "Remaining
   items"). Nothing reads `wsm.Turn.Displaced` back, so no bounce ever
   resubmits. The paragraph below records the SECOND blocker, the missing test
   hook, which only matters once the behavior exists. Original text:
   the skip is a missing TEST HOOK. The merge captures
   the displaced turn durably, ends it (KillTurn), and resubmits exactly once
   at lease release. What cannot be constructed is a deterministic crash inside
   the narrow window between `CaptureDisplaced` and either the merge's own next
   `Queue.Submit` or a clean run's near-instant finish: there is no knob to
   freeze a merge run mid-method, and the only park point the harness offers is
   a scripted conflict, which requires the conflict brief's own `Queue.Submit`
   to go through — exactly the call this scenario would need held. To un-skip:
   a test-only pause point in the merge run between capture and resubmit.
3. `TestSubscriptionInvariantAcrossWatchKinds/WatchHostWorkspace` — DELIBERATE,
   and it should stay skipped. The invariant under test is what a LATE
   subscriber is replayed, which is a property of retained VIEWS. The only
   lightweight two-step driver this suite has for that kind is `OpenInEditor`,
   a one-shot host RELAY — sent once to whoever is listening, replayed to
   nobody by design. Driving it here would assert the opposite of the contract.
4. `TestSubscriptionInvariantAcrossWatchKinds/WatchWebWorkspace` — DELIBERATE,
   and structurally unreachable. Its only push arm is `transferred`, fired once
   per handover, and by the time it fires the workspace's standing on THIS
   daemon is already `transferring_away`, so `resolveStreamRef` refuses every
   further per-workspace open before it reaches the topic. There is no window
   in which "subscribe late on the daemon holding the published transferred
   view" exists at all, so the late-subscribe half cannot be demonstrated
   without contradicting the refusal contract.

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
