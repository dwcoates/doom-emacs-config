# Daemon architecture and seams (overhaul rebuild)

This is the daemon teamlead's binding package map and seam catalog for the
rebuilt daemon. Every implementation agent reads it before touching code.
The contract is `proto/src/` (its comments are the documentation); the
architecture prescriptions are `docs/overhaul/daemon.md`; the standing
conventions are `docs/overhaul/prompts/TEAMLEAD.md`. This document only
fixes what those leave open: where code lives, what each package exports,
and how the packages meet.

Nothing from the old daemon tree is copied. Three areas may be consulted via
`git show e1eb8ca18:modules/app/agent-repl/daemon/<path>` as behavioral
reference only: `internal/workspace/merge/` (git edge cases),
`internal/sessioncontroller/classify.go` (hold/classification policy),
`internal/errclass/` (the errclass taxonomy).

## Module and toolchain

- Module `claude-repld` at `daemon/`, Go 1.24 toolchain (`go 1.23` in
  go.mod is fine). Dependencies: `connectrpc.com/connect`,
  `golang.org/x/net/http2` (+ `h2c`), `modernc.org/sqlite`,
  `github.com/google/uuid`, `github.com/creack/pty` (login pty),
  `google.golang.org/protobuf`. Replace directives stay:
  `agentrepl/proto => ../proto/gen/go`, `agentrepl/logging =>
  ../agent-shim/logging/go`, `agentrepl/wire => ../agent-shim/wire`.
- `proto/gen/go` does not require connect in its own go.mod; the daemon's
  go.mod carries `connectrpc.com/connect` directly, which satisfies the
  generated `*connect` packages under Go's pruned module graph.
- store.v1 is never imported (the codegen gate enforces it).
- Build: `cd daemon && go build ./... && go vet ./... && go test ./...`.

## Package map

```
daemon/
  cmd/claude-repld/main.go        boot only: flags, env contracts, pprof (before deps), log
                                  surfaces, state root, addr claim + daemon.addr, WSM open,
                                  joining mode, wire the graph, serve, orderly exit
  internal/
    dlog/          canonical JSONL logging API + log surfaces (per-workspace symlinked sinks,
                   run log w/ backups + cap, terminal mirror decoupled, non-closeable borrow
                   handle, shared-fd shim sink for fd 3, ClientLog persistence)
    envc/          the four env contracts (-fake, AGENT_REPL_FORBID_VENDOR_CALLS,
                   AGENT_REPL_STATE_DIR, AGENT_REPL_OWNED) + vendor guard Check(site)
    stateroot/     $AGENT_REPL_STATE_DIR resolution + layout (paths below)
    pprofsurface/  opt-in local-only profiling listener
    daemonaddr/    the loopback listener claim + daemon.addr write/remove (atomic)
    vocab/         readers + assertions for proto/vocab/render-colors.json and paint-classes.json
    paint/         ANSI escape parser -> paint spans; syntax highlighter -> paint spans
    feedid/        FeedId encode/decode (no table)
    prompts/       prompts/ directory reader: header parse, placeholder validation, splice
    wsm/           the state client (SQLite; the durable fact inventory; lease policy metadata)
    sessionlock/   shim-held kernel lock PROBES (path derivation + flock probe); daemon never holds
    shimclient/    spawn/supervise/redial/adopt + every shim.v1 verb + watch streams + occupancy mutex
    gitclient/     git leaf: create/merge no-ff/revert/remove/nuke/identity/landed range/env hygiene
    account/       config-dir determination (path under $MULTI_REPO_ROOT), .claude.json email, transcript porting
    externalbrowser/  pinned external browser launch (OpenExternal)
    login/         per-account pty running `claude /login`, scrollback, attach/resize/close
    publish/       generic Topic[T] with the never-miss-never-end-stale subscription invariant
    sessionwatcher/ one per live workspace/shim: owns every shim watch; connectivity truth; routes
    resolve/
      feed/        feed resolver (row synthesis, prose fold, output address, pages, tail tokens)
      footer/      footer resolver
      topbar/      topbar resolver
      sidebar/     sidebar (roster) resolver
      holds/       hold-tray resolver
    prompthandler/ SubmitPrompt body (command recognition, mirror, forward to queue)
    promptqueue/   the one delivery path; holds; classifier call; interject; parked ledger; drain on turn end
    classifier/    headless vendor run (guarded) + -fake keyword heuristic
    merge/         merge orchestrator (per-repo queue, two methods, tabs, lease, test gate, briefs, ledger)
    drain/         shutdown schedule + idle sweep (hibernation policy incl. Hibernate directive)
    rollout/       self-reload trigger consumer: daemon handover, adopt rendezvous, shim relaunch engine,
                   build-staleness bounce, asset origin, intent manifest, reload_webapp push
    workspace/     workspace verbs (create standard + one-shot, register, open, close, kill, nuke,
                   restart{force}, select, priority), task verbs, cold gate answer, SetModel/SetPermissionMode
    health/        DaemonHealth / SessionHealth answers (unhealthy is an answer) + fault records
    commandfile/   the command-file ingress ($AGENT_REPL_STATE_DIR/output/workspace_commands_*.json)
                   mapped onto the same internal paths as the rpcs
    server/        Connect handlers (validation via base functions, delegation), publishers wiring,
                   static asset origin, unowned-workspace refusal, h2c + HTTP/1.1
    boot/          boot sequence and adoption reconciliation (surviving shims, intent manifest, holds restore)
  integration/     the integration suite (build tag `integration`): real daemon in-process against a
                   FAKE shim.v1 server, fake git repos, temp state root, fake store socket path
```

Package dependency direction (a package may import only what is at or
below it in this list): proto gen, dlog, envc, stateroot, vocab, paint,
feedid, prompts, publish  <  wsm, sessionlock, shimclient, gitclient,
account, externalbrowser, login  <  sessionwatcher, resolve/*  <
prompthandler, promptqueue, classifier, merge, drain, rollout, workspace,
health, commandfile  <  server, boot  <  cmd. The shim client and git
client know no other daemon package. The prompt queue, merge orchestrator
and drain controller never import each other; they meet at wsm (the
lease) and at the shim client.

## State root layout (`$AGENT_REPL_STATE_DIR`, default `~/.claude-emacs`)

- `daemon.addr` — `127.0.0.1:<port>\n`, written atomically after the
  listener is bound, removed on orderly exit. A joining daemon writes it only
  once it owns every workspace.
- `wsm.db` — the fresh WSM database (old `state.db` is abandoned in place).
- `logs/daemon.run.log` (+ rotated backups) — the restart-scoped run log.
- `sock/<workspace-id>.sock` — per-workspace shim UDS. Keep the path short
  (macOS `sun_path` is 104 bytes): the state root may be long, so the daemon
  refuses at boot if `sock/` plus a 16-char name would overflow, loudly.
- `intent/manifest.json` — the stand-down intent manifest (pid + intent per
  session), written by the outgoing daemon, reconciled by the incoming one.
- `output/workspace_commands_*.json` — the command-file ingress.
- Per-workspace durable log sinks are symlinks at
  `<workspace>/.claude/emacs/{daemon,shim,webapp,sidecar}.log` per
  `logging-contract.md`; targets live under the OS temp dir.

## Shim-held kernel locks (probe only)

Directory `~/.cache/agent-repl/run/`. Workspace lock:
`workspace-<md5hex(filepath.Clean(absDir))[:8]>.lock`. Session lock:
`session-<vendor_session_id>.lock` (ASSUMED — cross-system, flagged to the
project lead; the daemon's probes and rollout wait use the WORKSPACE lock
only, so the session-lock spelling is not load-bearing for the daemon). A
probe is `open + flock(LOCK_EX|LOCK_NB)`: success (then unlock) means free;
EWOULDBLOCK means a live shim holds it; any other error means "could not
tell" and is never read as free.

## Shim spawn (the common contract, verbatim)

`node <module>/agent-shim/claude/shim/dist/main.js --listen <uds> --store-socket <store uds> --log-fd 3 [--fake]`,
env `CLAUDE_CONFIG_DIR=<account root>`, `AGENT_REPL_OWNED=1`,
`AGENT_REPL_STATE_DIR`, `SHIM_BUILD_SHA`, and in tests
`AGENT_REPL_FORBID_VENDOR_CALLS=1`; cwd is the workspace dir; fd 3 is the
already-open shim log sink (never a pipe to the daemon's stderr). Process
group discipline; stderr captured in a ring buffer as failure evidence.
Readiness = WatchSession connected and the first pushed `diagnostics` arm
says healthy. Session facts travel only in StartSession.

## Seams (the interfaces the foundation lands; leaves implement; peers consume)

The foundation commit lands each of these as Go interfaces (or concrete
types with stubbed methods returning `errNotImplemented`) so wave-1 leaves
and wave-2 peers build in parallel. Signatures below are the intent; the
foundation agent may refine names and add context parameters, but must not
change responsibilities. Every method takes `context.Context` first.

### wsm (`internal/wsm`)

```go
type DB interface {
  Close() error
  ReadOnly() bool
  // registry
  RegisterWorkspace(dir string, facts RegisterFacts) (Workspace, bool /*created*/, error)   // idempotent by normalized dir; mints WorkspaceID + RepoID
  Workspace(id WorkspaceID) (Workspace, error); WorkspaceByDir(dir string) (Workspace, error)
  ListWorkspaces() ([]Workspace, error); ListRepositories() ([]Repository, error)
  SetClosed(id WorkspaceID, closed bool) error; SetCurrent(id WorkspaceID, at time.Time) error; Current() (*WorkspaceID, error)
  SetPriority(id WorkspaceID, p *Priority) error; SetAttention(id WorkspaceID, on bool) error
  SetMergedAt(id WorkspaceID, at time.Time) error; Forget(id WorkspaceID) error  // nuke
  // creation jobs (merge geometry + configured actions + materialization)
  PutCreationJob(CreationJob) error; CreationJob(id WorkspaceID) (CreationJob, bool, error)
  // session facts
  PutSession(Session) error; Session(id WorkspaceID) (Session, bool, error)
  SetSessionTerminal(id WorkspaceID, t SessionTerminal) error; TouchEngagement(id WorkspaceID, at time.Time) error
  // occupancy lease (policy metadata; the in-memory mutex is shimclient's)
  AcquireLease(id WorkspaceID, holder LeaseHolder, policy LeasePolicy) (Lease, error)  // refuses if held
  ReleaseLease(leaseID LeaseID) error; Lease(id WorkspaceID) (Lease, bool, error); SetLeasePolicy(leaseID LeaseID, p LeasePolicy) error
  // held prompts (the ONE durable hold store)
  PutHeldPrompt(HeldPrompt) error; UpdateHeldPromptClassification(turn TurnID, c Classification) error
  UpdateHeldPromptHold(turn TurnID, h *HoldKind) error; TombstoneHeldPrompt(turn TurnID, why Tombstone) error
  HeldPrompts(id WorkspaceID) ([]HeldPrompt, error); AllHeldPrompts() ([]HeldPrompt, error)  // corrupt row => error, nothing loaded
  // turns (durable origin, displaced capture, idempotency)
  PutTurn(Turn) error; CloseTurn(turn TurnID, at time.Time, how TurnClose) error; OpenTurns(id WorkspaceID) ([]Turn, error)
  ClaimIdempotencyKey(id WorkspaceID, key string, turn TurnID) (existing *TurnID, err error)
  // orphan close: everything without a terminal, one transaction
  CloseOrphans(id WorkspaceID, at time.Time) (OrphanReport, error)
  // tasks
  CreateTask(title string) (Task, error); UpdateTask(TaskID, TaskChange) error; Tasks() ([]Task, error)
  AssignWorkspaceTask(id WorkspaceID, task *TaskID) error
  // merge-lease ledger
  OpenMergeLedger(id WorkspaceID, lease LeaseID) error; RecordTabInterval(lease LeaseID, TabInterval) error; MergeLedger(id WorkspaceID) ([]MergeLedgerEntry, error)
  // faults (open/closed with persisted resolved-at)
  OpenFault(Fault) (FaultID, error); CloseFault(FaultID, at time.Time) error; OpenFaults(scope FaultScope) ([]Fault, error)
  // drain schedule
  PutDrainSchedule(DrainSchedule) error; ClearDrainSchedule() error; DrainSchedule() (*DrainSchedule, error)
  // serving ownership (handover)
  ClaimServing(id WorkspaceID, daemon InstanceID) error; Serving(id WorkspaceID) (*InstanceID, error); ReleaseServing(id WorkspaceID, daemon InstanceID) error
}
```

Invariants: one `*sql.DB` with `SetMaxOpenConns(1)`; open DSN carries
`_pragma=busy_timeout(5000)&_pragma=journal_mode(WAL)&_txlock=immediate`
(the old open path, copied); layout version table; a newer layout refuses
to open; `OpenReadOnly` uses `mode=ro&_pragma=query_only(1)`; every load
is all-or-nothing (a corrupt row fails the whole read).

### shimclient (`internal/shimclient`)

```go
type Spec struct{ WorkspaceID; WorkspaceDir, UDSPath, StoreSocket, ConfigDir, ShimBuildSHA, NodeBin, MainJS string; Fake bool; LogSink *os.File /*fd 3*/; ForbidVendor bool }
type Supervisor interface {
  Spawn(ctx, Spec) (Client, error)            // spawn + dial + wait for first healthy diagnostics; a dead process ends bring-up with exit+stderr
  Adopt(ctx, WorkspaceID, UDSPath string) (Client, error)  // crash-boot / handover: dial a running shim, no spawn
}
type Client interface {
  // verbs (all lease-checked by callers; the client only guards occupancy)
  StartSession(ctx, *shimv1.StartSessionRequest) (*shimv1.StartSessionResponse, error)
  WatchSession(ctx) (Stream[*conversationv1.SessionUpdate], error)
  SetSessionModel / SetSessionPermissionMode / Hibernate / KillSession / StartTurn / UpdateAgent / KillTurn / StopBash / DetachForeground / ReadHistory
  WatchAgent(ctx, *shimv1.WatchAgentRequest) (Stream[*shimv1.WatchAgentResponse], error)
  WatchBash(ctx, work *conversationv1.DetachedWorkId) (Stream[*conversationv1.AgentBash], error)
  // occupancy: the in-memory guard behind the WSM lease row
  Occupy(holder string) (release func(), err error)
  // supervision
  Exited() <-chan ExitInfo   // closed with exit decoding + stderr ring when the process is gone
  Kill(attr KillAttribution) error; Detach()  // Detach: stop supervising, keep the process (handover)
  Connectivity() <-chan LinkState  // dialing|connected|redialing|dead — evidence-driven redial forever
  PID() int
}
type Stream[T any] interface{ Recv() (T, error); Close() }   // Recv returns io.EOF only on a producer-side end; the consumer decides whether that is a transport failure
```

Workflow verbs (GetWorkflow/WatchWorkflow/StopWorkflow) are NOT exposed this
wave (kicked).

### gitclient (`internal/gitclient`)

```go
type Git interface {
  DefaultBranch(ctx, repoDir string) (string, error)
  ResolveRef(ctx, repoDir, ref string) (sha string, err error)
  CreateWorktree(ctx, repoDir, branch, baseRef, worktreeDir string) error
  RemoveWorktree(ctx, repoDir, worktreeDir string) error
  Nuke(ctx, repoDir, worktreeDir, branch string) error         // force both
  CommonDir(ctx, dir string) (string, error)                    // canonicalized (symlinks resolved)
  SameRepo(ctx, a, b string) (bool, error)
  MergeNoFF(ctx, targetDir, sourceBranch, message string) (MergeOutcome, error)  // Landed{Commit} | Conflicted{Files}
  ConflictedFiles(ctx, dir string) ([]string, error); AbortMerge(ctx, dir string) error
  RevertMerge(ctx, targetDir, mergeCommit string) error
  LandedRange(ctx, targetDir, mergeCommit string) ([]Commit, error)  // second-parent history
  ChangedPaths(ctx, dir, rangeSpec string) ([]string, error)
  IsClean(ctx, dir string) (bool, error); CurrentBranch(ctx, dir string) (string, error)
}
```

Every invocation: `git -C dir ...`, inherited `GIT_DIR`/`GIT_WORK_TREE`/
`GIT_INDEX_FILE`/`GIT_COMMON_DIR`/`GIT_PREFIX`/`GIT_OBJECT_DIRECTORY`/
`GIT_ALTERNATE_OBJECT_DIRECTORIES` stripped; local only (never fetch/push).
Failures carry the git stdout+stderr as evidence.

### publish (`internal/publish`)

```go
type Topic[T any] struct{ ... }
func (t *Topic[T]) Publish(v T)                      // whole view; no-op when proto.Equal to the last
func (t *Topic[T]) Subscribe(ctx) <-chan T           // delivers the latest published value first (if any), then every later value in order, never skipping; closes only on ctx cancel
func (t *Topic[T]) Latest() (T, bool)
```
The feed's per-feed row stream is `feed.Tail` (below), which is this
guarantee's feed spelling with a token-pinned start.

### Output address and lease (shared vocabulary in `internal/wsm` types)

```go
type OutputAddress struct{ Feed feedid.Feed; Parent *feedid.Ref }   // set by a lease holder; the feed resolver applies it to every row the session produces
type LeaseHolder int  // HolderMerge | HolderRestart | HolderDrain | HolderHibernate
type LeasePolicy int  // PolicyRefuse (merge) | PolicyHold (restart, drain) | PolicyParked (merge parked: route to resolution agent)
```

### sessionwatcher (`internal/sessionwatcher`)

One instance per live workspace. Constructor takes the shim client, the
five sinks, and a `LifecycleSink`. It opens WatchSession immediately, opens
WatchAgent for `turn_in_flight`, and one watch per `live_work` item; opens a
watch for every detached-work announcement; reaps at terminals.

```go
type FeedSink interface {
  OnPrompt(ws WorkspaceID, agent AgentId, *conversationv1.AgentPrompt, addr OutputAddress)
  OnActivity(ws, agent AgentId, *conversationv1.AgentActivity, addr OutputAddress)
  OnQuestion(ws, agent, *conversationv1.AgentQuestion, addr); OnPermission(ws, agent, *conversationv1.AgentPermission, addr)
  OnAgentTerminal(ws, agent, turn *TurnID, success *AgentSuccess, failure *AgentFailure, addr)
  OnDetachedWork(ws, agent, *conversationv1.AgentDetachedWork, addr); OnBash(ws, work DetachedWorkId, *conversationv1.AgentBash, addr)
  OnSessionUpdate(ws, *conversationv1.SessionUpdate)   // query_died, compacting, identity_rotated affect rows
  OnHistoryPage(ws, agent, *conversationv1.HistoryPage, addr)  // the opening page of a watch (catch-up)
}
type FooterSink interface { OnActivity(...); OnQuestion(...); OnPermission(...); OnAgentTerminal(...); OnDetachedWork(...); OnBash(...); OnSessionUpdate(...); OnLink(ws, LinkState) }
type TopbarSink interface { OnSessionStarted(ws, *SessionStarted); OnSessionUpdate(ws, *SessionUpdate); OnActivity(ws, agent, *AgentActivity) /*unmodeled warnings*/; OnLink(ws, LinkState) }
type SidebarSink interface { OnSessionStarted(ws, *SessionStarted); OnAgentTerminal(...); OnActivity(...); OnDetachedWork(...); OnPermission(...); OnSessionUpdate(...); OnLink(ws, LinkState) }
type HoldsSink   interface { /* the tray is fed by the prompt queue and merge orchestrator, not the watcher */ }
type LifecycleSink interface { OnTurnEnded(ws, turn TurnID, how TurnClose); OnLiveWorkChanged(ws, live LiveWorkSet); OnNotification(ws, HostNotification) }
```
The watcher owns the per-workspace `LinkState` (the daemon↔shim hop of
connectivity truth) and exposes `Connected() bool` and `LiveWork()`.
Freeness = no in-flight turn AND empty live-work set, answered here.

### resolvers (`internal/resolve/*`)

Each resolver holds in-memory accumulation per workspace, publishes only
complete views, and exposes `Topic(ws) *publish.Topic[*View]`. Inputs are
the sink interfaces above plus daemon-fact setters:

- feed: `Tail(ws, feed feedid.Feed) *feed.Tail` (row upserts, token-pinned),
  `OpenPage(ws, feed, reader ReaderID) (FeedPage, token)`, `NextPage(ws, feed, reader)`,
  `SetOutputAddress(ws, *OutputAddress)`, `UpsertSynthesized(ws, feed, FeedRow)`
  (merge tabs, cold gate, separations, mirrored user prompts),
  `RetireRow(ws, feed, FeedId)`. Page walks are per reader (connection),
  never persisted. The mirror of an accepted prompt is a `user_prompt` row
  stamped with the minted TurnId, drawn with the metaprompt sentinel spans
  stripped (the full text stays on the record).
- footer: status tree resolution + R1 dwell retirement of `interrupted`/
  `loading` (a daemon-side timer that re-pushes the successor; nothing on
  the wire ticks), live-work chips, tokens cell + panels always populated,
  `SetMerge(ws, MergeFacts)`, `SetClosing(ws, *CloseBlocked)`, `SetColdGate`,
  `SetInterrupting(ws, bool)`.
- topbar: title/session line from WSM naming + session facts, model
  selector from the catalog + last-writer-wins model fact (shim order),
  connectivity glyph/tone from the render-colors vocabulary, warnings
  (accounting, unmodeled, detached unmodeled, session faults, degraded
  windows), context chip from `context_usage`, account from the config
  root's `.claude.json`.
- sidebar: both groupings, priority order, attention marker (set on
  notification, cleared on SelectWorkspace), recently merged, `current`,
  status arm from session/merge/link facts; `SetMerge(ws, MergeFacts)`,
  `SetSelected(ws)`, `SetRegistry(...)` on any WSM change.
- holds: `SetHeldPrompts(ws, []HeldPrompt)`, `SetOffer(ws, *HeldOffer)`;
  heading composed here.

### promptqueue (`internal/promptqueue`)

```go
type Queue interface {
  Submit(ctx, Submission{WS, Turn TurnID, Said *UserSaid, Origin PromptOrigin, Target *feedid.Ref /*bubble composer*/}) (Disposition, error)  // Delivered | Held{kind} | Refused{arm}
  Release(ctx, ws, turn) error; Drop(ctx, ws, turn) error         // UpdateHeldPrompt
  SubmitSessionAct(ctx, ws, Act) error                             // /clear, /compact, model change (SetModel + /model), permission mode
  OnTurnEnded(ws, turn, how)                                       // from LifecycleSink: pop + deliver next
  OnLeaseChanged(ws)                                               // re-evaluate holds vs policy
  RestoreHolds(ctx) error                                          // boot: all-or-nothing
}
```
Classifier: `classifier.Judge(ctx, running, incoming) (Verdict, error)`;
guarded by `envc.VendorGuard.Check("classifier")`; `-fake` uses the keyword
heuristic; the explicit-interrupt fast path ("stop", "abort", "cancel",
"halt", "wait") bypasses the model. Interject re-spec: the interrupting
prompt moves to the semantic head before teardown; the footer's
waiting·interrupting fires the moment the interrupt registers; delivery
waits for the real turn end; a failed interrupt strips the jump and stamps
classification_error.

### merge (`internal/merge`)

```go
type Orchestrator interface {
  Enqueue(ctx, ws) error                         // MergeWorkspace + command-file merge; refuses pre-state (no layout facts, deleted session, already queued/merging)
  Pause(ctx) error; Resume(ctx) error; Evict(ctx, ws) error
  AnswerDequeue(ctx, ws, keep bool) error        // AnswerHeldOffer
  OnInterrupt(ctx, ws)                           // raises the dequeue offer while queued
  Facts(ws) (MergeFacts, bool)                   // for the footer/sidebar
  Resume(ctx) error                              // boot: resume or loudly fail in-flight merges
}
```
Two methods keyed by `gitclient.SameRepo(target, daemonCheckout)`: Emacs
repo = pre-prompt → no-ff merge → conflicts (agent, once per conflict
commit, then parked) → tests (`bin/test-all.sh --suites <selected>`, no
flake re-run, output archived) → fixes (agent loop until pass or escalation
record, then parked) → rollout bounce → post-prompt; every other repo =
pre-prompt → post-prompt. Tabs are FeedMergeTab rows on the merge bubble's
sub-feed (`feedid.Feed{Merge{leaseID}}`), append-only, round-numbered.
Briefs are read from `prompts/` at use time (`merge-conflict-resolve.md`,
`merge-test-failure-resolve.md`). Post-merge worktree removal after
terminal publication. The displaced user turn is captured durably and
resubmitted exactly once at lease release. Self-reload trigger:
`rollout.Trigger(landed []Commit)` only after release + terminal.

### rollout (`internal/rollout`)

`Controller` with: `Trigger(landed)` (classify by subsystem prefix, invoke
`bin/deploy-all.sh --no-bounce` once, then per-subsystem action),
`Handover(ctx)` (spawn successor with `--joining`, announce on WatchDaemon,
per-workspace transfer at freeness with quiesce + intent manifest +
`transferred` pushes, adoption window timing, wait-forever with 10-minute
holdout warnings), `AdoptHost(ws)`/`AdoptWeb(ws)` (rendezvous against the
expected-participant set recorded at announcement; completes when all have
called; headless = zero participants), `RelaunchShim(ws, reason)` (the one
engine for self-merge shim change and the build-staleness bounce: prelaunch
inert → wait freeness → restart-pending hold → stand down old (Hibernate is
NOT used here; graceful KillSession{force:false} then reap) → reap gate →
StartSession(resume) → drain holds), `ReloadWebapp(ws)` push. Asset origin:
`server` serves `webapp/dist` with the entry point re-stat'd per request
and `Cache-Control: no-store` on it only.

### workspace (`internal/workspace`)

The verbs, each a function delegating to wsm + gitclient + shimclient +
queue + resolvers: `Register`, `Create` (standard + one-shot; slug from the
initial prompt via the naming rule; branch; worktree; layout facts recorded;
registration only after materialization; consent check for ungated modes;
fork = the daemon ports the parent's transcript into the child's config
root project dir before StartSession(resume)), `Open` (spawn on mount
semantics), `Close` (requires quiet: no turn, no live work, no held prompts,
no queued merge; refusal manifests in the footer), `Kill`, `Nuke`, `Restart`
(delegates to rollout.RelaunchShim; force = KillSession{force:true} first;
owns the reload_webapp push when needed), `Select`, `SetPriority`, tasks,
`AnswerColdGate`, `SetModel`/`SetPermissionMode` (through the queue's
session-act path; ungated mode needs consent recorded at creation),
`Interrupt` (turn → KillTurn{force:confirm} with the confirm_required
challenge when detached agents are live; detached → decode FeedId →
UpdateAgent.stop / StopBash; all_agents → fan-wide), `AnswerPermission`,
`AnswerQuestion`, `OpenExternal`, login verbs, `ClientLog`, health.

## Cross-cutting conventions (binding)

- Validation: one `validate<Message>` base function per message where
  validation lives once; every message-typed field / oneof arm gets its own
  dedicated function delegating to the child's base; unset non-optional
  fields and unset oneofs are errors. Requests failing validation answer a
  Connect `InvalidArgument` error naming the field. Stream pushes are built
  by resolvers that never emit a partial view.
- Refusals whose error arm does not exist yet: answer a Connect error
  (`connect.CodeFailedPrecondition` for state refusals, `CodeNotFound` for
  unknown ids) whose message is exactly `intended arm: <RpcName>Error.<arm_name>: <reason>`,
  and log the intended arm at WARNING with operation
  `daemon.refusal.unlanded_arm`. Collect every such site in
  `daemon/ERROR-ARMS.md` (rpc, arm name, condition) — the teamlead batches
  them to the project lead.
- Logging: only `internal/dlog`. Every logical branch logs (DEBUG on the
  ordinary path, WARN for warnings, ERROR for errors), with `operation`
  names of the form `daemon.<package>.<verb>` and structured `context`.
  Workspace-bound records go to the workspace sink; failing to resolve the
  workspace is an invariant violation, never a global write.
- Identifier spaces are never conflated: vendor AgentId (which agent),
  vendor tool-use id / activity id (which unit), daemon TurnId (which turn),
  daemon WorkspaceID / FeedId.
- Presence never sentinels; clocks ship instants; pushes are whole views,
  event-driven, deduplicated by `proto.Equal`.
- Tests: table-driven, Arrange/Act/Assert, one file per source file
  (`foo_test.go` beside `foo.go`), one edge case per test, no `time.Sleep`
  for synchronization (channels, WaitGroups, or injected clocks). Every
  production package ships its own unit tests and returns green.
- No real vendor calls anywhere: every test sets
  `AGENT_REPL_FORBID_VENDOR_CALLS=1`; the classifier and any exec site
  check `envc.VendorGuard`.
- Workflow is kicked: workflow rpcs and arms are answered/ignored with a
  typed not-implemented refusal (transport-layer, intended arm
  `<Rpc>Error.not_implemented`); no watch is ever opened for a workflow.
- Q1–Q4 defaults: `/agents` and `/help` are NOT recognized (fall through as
  prompts); no OpenInEditor; panels answer in SubmitPromptSuccess only;
  no held-prompt accept action.

## FeedId scheme (`internal/feedid`)

```go
type Feed struct{ Root bool; Agent *AgentId; Merge *LeaseID }
type Ref  struct{ WS WorkspaceID; Feed Feed; Row RowKey }
type RowKey struct{ Kind RowKind; ID string; Sub string }   // Kind: prompt|activity|turn_ended|permission|question|separation|cold_gate|merge_head|merge_tab|detached_subagent|detached_shell|synth
func Encode(Ref) *frontendv1.FeedId; func Decode(*frontendv1.FeedId) (Ref, error)
```
Encoding is a versioned, delimiter-safe, base64url string; the same input
yields the same id across pushes, restarts and replays. A subagent bubble
row (`Kind=activity, ID=<spawn unit>, Sub=<created agent id>`) decodes to
the sub-feed address `Feed{Agent: created}`; a merge head decodes to
`Feed{Merge: lease}`. The plan bubble keys on `Kind=activity, ID=plan:<agent>:<episode>`.

## Paint classes (`proto/vocab/paint-classes.json`)

The closed inventory of `paint_class` names for `FeedCodeSpan` and
`FeedMergeTestSpan`. ANSI parsing maps SGR codes to `ansi-*` classes
(bold, dim, italic, underline, fg-<name>, bg-<name>, bright variants);
syntax highlighting emits the highlight classes (keyword, string, comment,
number, type, function, operator, punctuation, variable, constant, attribute,
tag, heading, link, emphasis, strong, added, removed, meta). `paint` asserts
its emitted classes are in the vocabulary file.
