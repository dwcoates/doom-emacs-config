> SUPERSEDED, 2026-09-02. Every item this handoff queued has landed: the
> confirming integration run, the dead-code pass, landing 7's five adaptations
> and a duplication remediation. The six landing-7 sentinels in the table below
> are LANDED, not owed. Read
> `docs/overhaul/reports/STOP-daemon.md` for the current state; this file is
> kept as the record of how the tip got here.

# DAEMON HANDOFF — overhaul/daemon, 2026-09-02

For an opus-medium agent with none of the outgoing lead's context. Read this,
then `daemon/ARCHITECTURE.md`, `daemon/AGENTS.md`, `daemon/ERROR-ARMS.md`,
`daemon/integration/SPEC.md`, and `docs/overhaul/daemon.md` (the durable
rulings record, current through landing 6 plus the 2026-09-02 shim-lock
section). The lead's running notes are at
`/private/tmp/claude-501/-Users-dodgecoates--config-doom/6a1b0e3a-9be5-4fdb-b4f7-d5c97ccdec15/scratchpad/daemon-lead-notes.md`
(shared scratchpad; every scratch file is prefixed `daemon-`).

## State at handoff

- Branch `overhaul/daemon`, worktree `~/.config/doom-overhaul/daemon`. Tip is
  named in the handoff message; `cd modules/app/agent-repl/daemon && go build
  ./... && go vet ./... && go test ./...` is GREEN (3712 unit tests, ~9 s).
- Integration suite: `TMPDIR=/tmp AGENT_REPL_FORBID_VENDOR_CALLS=1 go test
  -tags integration ./integration/... -count=1 -timeout 180s` — 534 tests,
  ~140 s. TMPDIR=/tmp is REQUIRED on macOS (unix-socket path budget).
- Sweep leaked fixtures ONLY when no agent is running a suite, keyed on state
  dirs: `pkill -9 -f agent-repl-integration-bin; pkill -9 -f
  '/private/tmp/ar[A-Za-z0-9]*/state'`, then remove `/private/tmp/ar*`,
  `/private/tmp/agent-repl-integration-bin-*`, and
  `/private/tmp/agent-repl-*-{daemon,shim}-*.log` (find, not glob; /tmp is a
  symlink so use /private/tmp).
- Run logs (verbose, `-json`-free): scratchpad `daemon-integration-run{1..8}.log`.
  The >1 s profile table: scratchpad `daemon-slow-tests.md`.

## (a) What was built vs docs/overhaul/daemon.md, and every deviation

Built, all landed and unit-green: shimclient (spawn/supervise/redial/adopt),
gitclient (fake-git tests only), wsm (17 tables incl. merge ledger,
host_session_id, layout v3, Promote()), dlog, sessionwatcher (routes to the
five resolvers + lifecycle/link/diagnostics sinks; AwaitFree/AwaitTurnEnd),
resolve/{feed,footer,topbar,sidebar,holds}, prompthandler + promptqueue +
classifier (-fake heuristic; vendor guard), merge orchestrator (two methods,
ledger id minted at enqueue, parked route, test gate), drain, rollout
(handover, adopt rendezvous, relaunch engine, intent manifest, staleness
bounce once per observed stamp), workspace verbs (create incl. one-shot/fork/
parent, open/close/kill/nuke/restart, tasks, priority, relays), account
(determined config dir, transcript porting), login (one pty per account root),
externalbrowser, commandfile ingress (general verbs, quarantine at parse and
apply), health, boot sequence, server (47 rpcs, publishers, asset origin,
flush-on-accept, arm mapping), cmd graph wiring, deploy chain
(`bin/deploy-all.sh` → `(agent-repl-runtime-restart-await)`; `agent-shim/wire`
deleted). Integration harness + fakeshim + fakegit + 14 suites (534 tests).

Deviations / overrides (all recorded in daemon.md or ARCHITECTURE.md):

1. Merge recovery re-queues at the FRONT and re-runs from the queue tab (daemon.md override 2026-08-29).
2. Shim relaunch: an INTERIM sequential order (stand down → reap → launch) was implemented because the shim held the workspace lock at startup; the project lead ruled (a) the shim takes both locks inside StartSession (landed on overhaul/shim be119abbf), and the engine was FLIPPED BACK to the prescribed prelaunch-then-wait; the fakeshim locks at StartSession. daemon.md "Ruling relay: shim relaunch vs the workspace lock".
3. `server.Deps.SessionFacts` (host arm identity facts) was first a named null object (`unwiredSessionFacts`) so server.New never failed boot; it is now REQUIRED in `cmd/claude-repld/graph.go` (buildGraph refuses when unwired) and produced by `workspace.Fleet.HostSessionFacts` (host_session_id minted at session creation, persisted in wsm.Session; generation = fleet spawn count).
4. Host arm publishing: `WatchHostWorkspace` uses a per-workspace STATE topic beside the event topic (compose-then-subscribe so a late subscriber gets the state); `Server.PublishHostWorkspace(ctx, ws)` is exported and called by the fleet (session up/stopped/installed/resumed, link changes) and by merge publish/forget; it dedupes by proto.Equal. A workspace with a session record this daemon does not operate composes `existing{terminal|live{shim_attached:false}}` from the durable record.
5. Watch* transport-closed refusals (unknown/mismatched/unowned workspace, unminted/expired feed token, no login open) log INFO `daemon.refusal.transport_closed` via `server.TransportClosed`, never the unlanded_arm WARN; typed LANDED refusals in workspace/close/login log INFO `daemon.refusal.typed`; only `server.UnlandedArm` logs WARN `daemon.refusal.unlanded_arm`.
6. CONNECTIVITY TRUTH PER HOP landed late: the server states host/web stream liveness to footer and topbar on every open/close edge; connected iff shim link + host + web are live.
7. Hibernation is a PARK: host stays live{shim_attached:false}, roster keeps an idle arm; a revival-pending hold whose bring-up FAILS is a loud DROP (daemon_hold.proto) — tombstoned, WARN per turn.
8. Fork: the daemon mints a fresh vendor session id, copies the parent's transcript under it, resumes; the parent is untouched (no shim fork arm exists).
9. Adoption (crash boot / handover) is attach-only: no StartSession on an already-started shim; facts come from the durable record until landing 7's `session_started` re-announce lands.
10. Adopt* calls WAIT for the rendezvous (all participants succeed together); not_yet_adopted on Adopt* only when the caller's context expires first.
11. Interrupt confirm_required counts live AGENTS only (a detached shell never raises the challenge).
12. /clear and /compact run as the turn they mint (command_acted is for /model <arg>); unknown slash text falls through to the vendor; duplicate idempotency_key → duplicate_submission.
13. ShimBuildSHA: stamp file first (`agent-shim/claude/shim/dist/.built-sha`), `SHIM_BUILD_SHA` env when absent; a present-but-blank stamp refuses. Flagged fragile (a built stamp silently outranks a test env).
14. Default model when CreateWorkspaceRequest.model is unset: `AGENT_REPL_DEFAULT_MODEL`, fallback "opus" (landing-7 candidate below removes the need).
15. Log-sink eviction on close evicts the HANDLE (link and target stay on disk); dlog durable targets live under `<state>/logs/` (moved from os.TempDir by remed8).

Landing-7 sentinels (all unlanded; every one answers via `server.UnlandedArm`
naming the arm, WARN `daemon.refusal.unlanded_arm`, rows in ERROR-ARMS.md):

| candidate | where in ERROR-ARMS.md | site |
| --- | --- | --- |
| `SubmitPromptError.bubble_refused{kind: not_deliverable|agent_busy}` | "Landing 6 batch, opened by the server handlers" table | `server.bubbleRefused` (internal/server/prompt.go) |
| shim `UpdateAgentFailure.agent_busy` (producer for kind agent_busy) | same row's note | none; `TestASecondSubmitWhileATurnRunsOnTheSameAgentThroughTheBubblePath…` is RED by design |
| shim `StartSessionFresh.model` optional ("SDK default") | lead notes; NOT yet an ERROR-ARMS row — add one | workspace/sessions.go DefaultModel fallback |
| shim `WatchSession.session_started` re-announce on every new watch | lead notes; add a NOTE row | Fleet.Adopt/Install attach-only |
| `CloseWorkspaceBlocked` composed-reason fields | first table's trailing note | workspace close blockers |
| `FeedMergeError` distinct evict/dequeue/abandon causes (only failed|abandoned today) | integration/SPEC.md "owed to landing 7"; add an ERROR-ARMS NOTE | merge terminal; `TestAnAbandonedQueuedMergeHasNoReachableCause` skipped |

Other unlanded rows still in ERROR-ARMS: `brief_missing` (one-shot finish),
the shim-name propagation rows on Interrupt/Answer* (`not_deliverable`,
`unknown_agent`, `no_open_ask`, `answer_mismatch`, `no_session`,
`unknown_work`, `live`, `not_the_open_turn`, `unspecified`). Report any that
end up with no producer; the project lead retires them.

## (b) Current red list (after remed8 merged at e56fb80c5)

remed8 completed every item and its full run showed EXACTLY ONE failure:
`TestASecondSubmitWhileATurnRunsOnTheSameAgentThroughTheBubblePathAnswersTheDaemonFaultRefusal`
— RED BY DESIGN until the shim's `UpdateAgentFailure.agent_busy` lands
(landing 7). Never "fix" it. The three flakes (layout-table race,
millisecond-equality assertion, admission pump dying on one repo's git
failure) are fixed. The lead did NOT re-run the full suite after the merge
(halt directive): FIRST ACTION for the new lead is one full run at
`-timeout 180s` on the tip to confirm 1 red / 0 unexpected WARN, then the
>1 s re-profile.

Skips that stay (each names its hook): `TestAnAbandonedQueuedMergeHasNoReachableCause`
(landing-7 per-cause arms; also `FeedMergeAbandoned` needs a `summary` field —
proposal recorded in ERROR-ARMS.md), `TestADisplacedUserTurnIsResubmittedExactlyOnceAcrossADaemonBounce`
(no crash-window hook), and the Watch* kinds with no two-step driver inside
`TestSubscriptionInvariantAcrossWatchKinds` (`WatchHostWorkspace` deliberately).

Decision needing the project lead's confirmation (remed8): the merge now ENDS
the displaced user turn (KillTurn) after capturing it, then resubmits exactly
once at lease release; daemon.md says only "captured durably and resubmitted
exactly once". Rationale: a merge driving the session under a still-live user
turn is incoherent. Escalate as an FYI in the final report.

Run-8 diagnoses (all fixed by remed8; kept for the record): drain schedule not
republished at boot; corrupt creation_jobs folded into "geometry gone";
FeedMergeAbandoned had no producer; NukeWorkspace git error not wrapped;
shim_refused unreachable; Fleet.Shim liveness by map presence; dlog targets in
os.TempDir (now `<state>/logs/`); feed tail retention unwired (now
`--feed-tail-retention` / `AGENT_REPL_FEED_TAIL_RETENTION`); displaced turn
never ended and worktree removed before resubmit; refusing lease released
holds at turn end; Diagnostics arm never reached the roster (degraded); idle
sweep wrote per-workspace records to the run log; answered cold gate never
retired; merge pump died on one start error. New test-only knob:
`AGENT_REPL_FAKE_SHIMS` (turn fake ON only) so a suite reaches a real vendor
call site with a live session.

## (c) Remaining queue, in order

1. Confirm the tip with one full run at `-timeout 180s` (expected: exactly the
   agent_busy red, zero unexpected WARN/ERROR; `ExpectWarnings` lists are exact,
   `AllowAllWarnings` is retired); fix anything else that shows; re-profile
   (>1 s table) once.
2. Dead-code pass — dispatch ONE `sonnet-medium` agent in its own worktree
   (never opus-low, never yourself): `staticcheck` (U1000; install via `go
   install honnef.co/go/tools/cmd/staticcheck@latest`) plus `go test
   -coverprofile` across unit AND integration (`-tags integration
   -coverpkg=./...`); every zero-coverage production function is DELETED or
   named in the final report with the reason it is live and pinned by a unit
   test; ruled-dead files still on disk are defects. Rule text: TEAMLEAD.md
   "Dead code is hunted programmatically" (overhaul/integration 7deb982cc,
   amendment 25bf69341). Known candidates: `topbar.SetPermissionModePicker`
   (no caller), any `unwiredSessionFacts` remnant.
3. Rewrite `docs/overhaul/reports/STOP-daemon.md` for the new state (board,
   red list, landing-7 ledger, resume queue) — the current one describes the
   2026-08-31 pause.
4. Final report to the project lead per COMMON.md: commit range, every suite
   and result, overrides, escalations (landing-7 ledger), the dead-code
   deletion list and kept-with-reason list, the >1 s table, what was left out
   and why.

## (d) Standing rules (binding)

- Your own dispatches: `opus-low` for implementation, `sonnet-medium` for the
  dead-code pass and fully specified mechanical writes; fable only for
  fresh-context adversarial audits (the audit loop is CLOSED at three rounds
  unless the project lead reopens it).
- No real git in any test (fake `git` executable / fake `gitclient.Git`); no
  vendor calls (`AGENT_REPL_FORBID_VENDOR_CALLS=1`, `--fake`); no
  `time.Sleep` for synchronization; tight timeouts (harness default 5 s,
  `HandoverChainTimeout` 15 s, package 180 s — never raise the default; a
  longer per-site bound needs a one-line justification).
- Implementer worktrees: `~/.config/doom-overhaul/daemon-agents/<slug>` on
  `overhaul/daemon-<slug>`, hand-created from the tip (`git worktree add -b`),
  never the Agent tool's own worktree; you merge each back and delete it.
  A silent agent is not a dead agent: SendMessage round-trip before reaping.
- Never edit `.proto` files or generated bindings; route proto needs to the
  project lead as concrete arm/field proposals. Never edit
  `~/.config/doom` (the integration checkout) or master. No pushes, no PRs.
- Every scratch file in the shared scratchpad is prefixed `daemon-`.
- Implementers get the common brief at scratchpad
  `DAEMON-COMMON-BRIEF-overhaul-daemon.md` (read it; it carries the file
  boundary, gate, and report conventions).

## (e) Pointers

- Lead notes (chronological, incl. every ruling): scratchpad `daemon-lead-notes.md`.
- Audit critiques (all folded in): scratchpad `daemon-audit{1,2,3}-critiques.md`.
- `daemon/ERROR-ARMS.md` (ledger), `daemon/AGENTS.md` (flags/env/knobs/bound table),
  `daemon/ARCHITECTURE.md`, `daemon/integration/SPEC.md` (incl. "Settled behaviors").
- Run logs: scratchpad `daemon-integration-run{1..8}.log`; >1 s table: `daemon-slow-tests.md`.
- Common brief for implementers: scratchpad `DAEMON-COMMON-BRIEF-overhaul-daemon.md`.
