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
- `proto/gen/go/go.mod` requires `connectrpc.com/connect v1.17.0` (landed by
  the project lead); the daemon pins the same version.
- `agent-shim/wire` is NOT imported by the rebuild; once nothing in the
  daemon imports it, the daemon lead deletes `agent-shim/wire` and its
  `bin/test-all.sh` roster entry (the store lead drops its dependency).
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
    ids/           the identity newtypes shared by every package (WorkspaceID, RepoID, InstanceID,
                   LeaseID, TurnID, TaskID, FaultID) — a leaf below wsm and feedid; both alias them
    feedid/        FeedId encode/decode (no table)
    apiresponses/  the unit -> API RESPONSE filing every token reconciliation runs over
                   (usage rides exactly one unit per response; shared by footer + topbar)
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
    sessioncommand/ the ONE parse of text as a slash command (the SessionCommand enum's spec
                   option) and the context-cut predicate; prompthandler and promptqueue both use it
    prompthandler/ SubmitPrompt body (command recognition, mirror, forward to queue)
    promptqueue/   the one delivery path; holds; classifier call; interject; parked ledger; drain on turn end
    classifier/    the routing question, asked through headless/ + -fake keyword heuristic
    headless/      the ONE exec site for the daemon's own `claude -p` calls:
                   the classifier's routing question and the workspace naming
                   call. Guard, binary resolution, stdin discipline, deadline
    merge/         merge orchestrator (per-repo queue, two methods, tabs, lease, test gate, briefs, ledger)
    drain/         shutdown schedule + idle sweep (hibernation policy incl. Hibernate directive)
    worktreereap/  the landed-worktree reaper: a daily low-priority sweep that removes linked
                   worktrees whose changes landed on the default branch (see AGENTS.md)
    newsdigest/    the daily Claude news digest: reads the watched sources, keeps what is new, has
                   Sonnet condense it (through headless/), stands it over every webview's feed until
                   dismissed; a durable daily cadence, one run at a time across processes (see AGENTS.md)
    agentreplsession/ agent-repl's session (since the later of an agent-repl login and the editor's
                   start), durable in wsm; the topbar's
                   connectivity dropdown states it (see AGENTS.md)
    runecap/       the one rune bound for every composed model prompt
    flock/         the one held non-blocking exclusive kernel lock (merge repo lock, reaper sweep
                   lock, news digest run lock)
    clock/         the one injectable Now/After clock every waiting package takes (drain, rollout,
                   startingshim, worktreereap alias it)
    rollout/       self-reload trigger consumer: daemon handover, adopt rendezvous, shim relaunch engine,
                   build-staleness bounce, asset origin, intent manifest, reload_webapp push
    workspace/     workspace verbs (create standard + one-shot, register, open, close, kill, nuke,
                   forget, restart{force}, select, priority), task verbs, cold gate answer,
                   SetModel/SetPermissionMode/SetEffort
    health/        DaemonHealth / SessionHealth answers (unhealthy is an answer) + fault records
    commandfile/   the command-file ingress ($AGENT_REPL_STATE_DIR/output/workspace_commands_*.json)
                   mapped onto the same internal paths as the rpcs
    heldingress/   the held-prompt ingress ($AGENT_REPL_STATE_DIR/held-prompts/held_*.json): prompts a
                   client could not hand to a live daemon, submitted through the prompt handler under
                   their own idempotency keys once one serves
    intakegate/    whether THIS daemon takes the two on-disk intakes: only the daemon that serves (not a
                   successor still joining, not an incumbent handing over)
    server/        Connect handlers (validation via base functions, delegation), publishers wiring,
                   static asset origin, unowned-workspace refusal, h2c + HTTP/1.1
    boot/          boot sequence and adoption reconciliation (surviving shims, intent manifest, holds restore)
  integration/     the integration suite (build tag `integration`): real daemon in-process against a
                   FAKE shim.v1 server, fake git repos, temp state root, fake store socket path
```

Package dependency direction (a package may import only what is at or
below it in this list): proto gen, dlog, envc, stateroot, vocab, paint,
feedid, prompts, publish, apiresponses, flock, clock, sessioncommand, runecap  <  wsm, sessionlock, shimclient, gitclient,
account, externalbrowser, login  <  sessionwatcher, resolve/*  <
prompthandler, promptqueue, classifier, merge, drain, rollout, workspace,
health, intakegate  <  commandfile, heldingress, worktreereap, newsdigest, agentreplsession  <  server, boot  <  cmd. The shim client and git
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
- `held-prompts/held_*.json` — the held-prompt ingress ("heldingress" below);
  `held-prompts/quarantine/` keeps a malformed entry where a person can read it.
- `logs/agent-repl-<workspace-log-id>-<sink>-*.log` — the per-workspace durable
  sink TARGETS. The sinks themselves are symlinks at
  `<workspace>/.claude/emacs/{daemon,shim,webapp,sidecar}.log` per
  `logging-contract.md`, pointing here. They are NOT in the OS temp dir: a
  durable log a person is asked to read must not live where the system may
  sweep it, must not move with TMPDIR, and must not leave one orphan per run in
  a directory nothing owns.

## Shim-held kernel locks (probe only)

RULED (project lead, 2026-08-29). Directory `~/.cache/agent-repl/run/`.
Workspace lock `workspace-<md5hex(filepath.Clean(absDir))[:8]>.lock`, taken
by the shim at startup with flock(LOCK_EX). Session lock
`session-<vendor-session-id>.lock`, taken inside StartSession (the shim
pre-mints the id on fresh). The daemon probes the WORKSPACE lock only —
`open + flock(LOCK_EX|LOCK_NB)`, released immediately — for boot adoption
and the rollout's transfer wait. Success means free; EWOULDBLOCK means a
live shim holds it; any other error means "could not tell" and is never
read as free.

## Shim spawn (the common contract, verbatim)

`node <module>/agent-shim/claude/shim/dist/main.js --listen <uds> --store-socket <store uds> --log-fd 3 [--fake]`
(the store socket is ALWAYS passed explicitly: `-store-socket` flag beats
env `AGENT_REPL_STORE_SOCKET` beats the default
`~/.cache/agent-repl/sock/store.sock`),
env = the daemon's OWN environment passed through (never an allowlist) with
`CLAUDE_CONFIG_DIR=<account root>`, `AGENT_REPL_OWNED=1`,
`AGENT_REPL_STATE_DIR`, `SHIM_BUILD_SHA`, `AGENT_REPL_SESSION_ID=<HostSessionId.value>`
(log correlation only) set/overridden, and in tests
`AGENT_REPL_FORBID_VENDOR_CALLS=1` (which also forces `--fake` onto the argv);
cwd is the workspace dir; fd 3 is the
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
  SetMergedAt(id WorkspaceID, at time.Time) error
  // nuke's durable half AND the whole of forget: the workspace's rows, plus the repository's
  // when no other workspace references it (nothing else ever deleted a repository record).
  Forget(id WorkspaceID) (ForgetReport, error)
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
  ClaimIdempotencyKey(id WorkspaceID, key string, turn TurnID) (IdempotencyClaim, error)  // minted | accepted (duplicate) | redriven (unaccepted claim rebound to turn)
  AcceptIdempotencyKey(id WorkspaceID, key string, turn TurnID) error  // the queue took the bound turn's submission
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
(the old open path, copied); layout version table; an OLDER layout is
MIGRATED FORWARD by the ordered list in `internal/wsm/migrate.go` (the state
is the user's data, so a schema change never throws it away), and only a
layout this build cannot interpret refuses to open — a NEWER one, because a
downgrade is not a migration, or one no chain of migrations reaches;
`OpenReadOnly` uses `mode=ro&_pragma=query_only(1)` and therefore refuses an
older layout rather than migrating it; every load is all-or-nothing (a corrupt
row fails the whole read).

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
  WatchSession(ctx) (Stream[*shimv1.WatchSessionResponse], error)  // frame oneof: update | session_started (re-announced once per watch)
  SetSessionModel / SetSessionPermissionMode / SetSessionEffort / Hibernate / KillSession / StartTurn / UpdateAgent / KillTurn / StopBash / DetachForeground / ReadHistory
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
  MainWorktree(ctx, dir string) (string, error)                 // the repository's dir; a bare repo is an error
  RepositoryOf(ctx, dir string) (string, ok bool, err error)    // MainWorktree's PROBE half: outside every repo is an ordinary false, never an error record
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
  never persisted. THE FEED BEGINS AT THE NEWEST SEPARATION: a `cleared` or
  `compacted` divider bounds DELIVERY, so the first page starts at it, `next`
  answers at_start there, and the rows above it are neither served nor pushed
  (`feed/pages.go` `deliverable`). Nothing is retired and no feedid is reminted
  — the store's book keeps every pointer — and the bound is read off the row
  order at every page rather than remembered, so a replay and the live plane
  cannot disagree about it. A compaction's surviving account rides the divider
  row itself (`FeedContextCutCompacted.summary`), which is why the bound keeps
  the summary without keeping the conversation. `compaction_failed` cut nothing
  and the worktree arms cut no context, so neither bounds anything. A history
  replay holds its publications until its page is placed, and a fork's
  inherited past rides its own plane and is never pushed (see AGENTS.md, "The
  feed never serves a row the newest cut withholds"). The mirror of an accepted prompt is a `user_prompt` row
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
Headless: `headless.Client.Run(ctx, Request) (Response, error)` — the daemon's
own one-shot vendor run, `claude -p` with the question on STDIN so it never
rides an argv. Two callers: the classifier (site `classifier`, plain text) and
the workspace naming call (site `workspace_naming`, `--model haiku`, the JSON
envelope). `headless.ResolveBin` never answers empty, which is what the
classifier's old empty-binary hole was.

Classifier: `classifier.Judge(ctx, running, incoming) (Verdict, error)`;
guarded by `envc.VendorGuard.Check("classifier")`; `-fake` uses the keyword
heuristic; the explicit-interrupt fast path ("stop", "abort", "cancel",
"halt", "wait") bypasses the model. Interject re-spec: the interrupting
prompt moves to the semantic head before teardown; the footer's
waiting·interrupting fires the moment the interrupt registers; delivery
waits for the real turn end; a refused interrupt strips the jump and returns
the prompt to hold_for_turn_end. No failure on the classifier path is ever
stamped classification_error (the tray's "unclassified"): a running turn the
store cannot show as open, a failed classifier call, and a refused interrupt
each resolve to hold_for_turn_end and are logged; every verdict is recorded at
INFO.

### merge (`internal/merge`)

```go
type Orchestrator interface {
  Enqueue(ctx, ws, by Requester) error           // MergeWorkspace (RequestedByUser) + command-file merge (RequestedByAgent); refuses pre-state (no layout facts, deleted session, already queued/merging)
  Pause(ctx, scope) error; Unpause(ctx, scope) error
  Evict(ctx, ws) error                           // a waiting merge leaves the queue; a RUNNING or PARKED one is abandoned
  AnswerDequeue(ctx, ws, keep bool) error        // AnswerHeldOffer; releasing a running merge abandons it
  OnInterrupt(ctx, ws)                           // raises the dequeue offer while queued or running
  OnWorkspaceClosed(ctx, ws)                     // abandons the workspace's merge, waiting or running
  RouteParked(ctx, ws, turn, said) error         // a parked merge's guidance: delivered to the workspace's own session and answered
  RetireConcluded(ctx, ws)                       // a failed/merged state retires once the workspace moves on
  Facts(ws) (MergeFacts, bool)                   // for the footer/sidebar
  Drain(ctx); Recover(ctx) error                 // orderly exit suspends mid-step merges; boot resumes them from their progress record
}
```
Two methods keyed by `gitclient.SameRepo(target, daemonCheckout)`. Every other
repo = pre-prompt → post-prompt. The Emacs repo = pre-prompt → ATTEMPTS →
post-prompt, where one attempt is: a detached scratch tree of the queue's own
at the target's tip (`<state>/merge-trees/<lease>-<n>`) → no-ff merge there →
conflicts (the agent brings ITS OWN branch up to date, once per source tip,
then parked) → tests (the tree's own `bin/test-all.sh --suites <selected>`,
no flake re-run, output archived; a gate that failed to run parks at once) →
fixes (the agent commits a repair on its own branch; loop until pass or
escalation record, then parked) → a fast-forward of the target to the tested
commit (a target that moved is merged onto again). The target is never a
working tree for the merge, so a failed, parked or abandoned merge leaves it
untouched. A branch already contained in the target concludes as merged with
nothing to land.

A repository's SLOT (`slot.go`) is the one right to make a tree, run the
gate and move the target; the admission pump is its only grantor and the
repository's kernel lock travels with it. A PARKED run yields its slot, so
the merges behind it proceed; once its guidance turn has ended it waits for
the slot again and makes its merge afresh on the new tip. Every end of a run
-- landed, failed, abandoned, stopped -- goes through the one teardown, which
releases the lease, the queue entry, the open ledger intervals, the tree and
the slot. A repair that changes the merge machinery (`daemon/internal/merge/`,
`bin/test-all.sh`) is refused and parks. Every brief and every guidance is
addressed to the merging workspace's own session.

Tabs are FeedMergeTab rows on the merge bubble's sub-feed
(`feedid.Feed{Merge{leaseID}}`), append-only, round-numbered. Every bubble row
(head and tabs) is published through `feed.Resolver.UpsertDurable`, which
records it in wsm's `durable_feed_rows` at its order key, so a new daemon
(restart or handover) draws the bubble again where it stood; no store replays
it. A held prompt keeps the requester open past the landing
(`wsm.bindMergeHold`). Briefs are read
from `prompts/` at use time (`merge-conflict-resolve.md`,
`merge-test-failure-resolve.md`). Post-merge worktree removal after terminal
publication. A user-requested merge captures the displaced user turn durably
and resubmits it exactly once at lease release; an agent-requested one waits
for the turn instead. A landing on the daemon's own checkout tells the deploy
ONCE (`Trigger.Landed(landed []Commit)`, which `deploy.Deployer` implements)
only after release + terminal, and never waits on the build.

### rollout (`internal/rollout`)

`Controller` with: `HandOver(ctx, force)` (spawn successor with `--joining`,
announce on WatchDaemon, intent manifest, then ask the prompt queue's BOUNCE
REGISTRY for every workspace's transfer at once — each taken on its own
freeness edge, or at once when forced — with quiesce + `transferred` pushes,
adoption window timing, and holdout warnings), `AdoptHost(ws)`/`AdoptWeb(ws)`
(rendezvous against the expected-participant set recorded at announcement;
completes when all have called; headless = zero participants),
`BounceShim(ws, reason, force, done)` (the one shim-bounce engine, run by the
registry: prelaunch inert → restart-pending hold → stand down old (Hibernate
is NOT used here; `KillSession{force}` then reap) → reap gate →
StartSession(resume) → the queue delivers what it held), `ShimReported(ws,
build)` / `CheckStaleness(ws, force)` (a shim's reported content hash against
the installed bundle's; a stale shim goes to the registry), and the
`ReloadWebapp(ws)` push. The DEPLOY (`internal/deploy`) builds, judges and
installs, and acts through this controller; see daemon/AGENTS.md "Deploy". Asset origin:
`server` serves `webapp/dist` with the entry point re-stat'd per request
and `Cache-Control: no-store` on it only. Image origin (`internal/imageorigin`,
mounted at `/feed-images/`): a prompt's `ImageBlock{path}` names a file on
THIS host, so the feed resolver registers the path and draws the origin's URL
as the `src`. Registering is the only way a path becomes servable, so the
origin serves exactly the images some conversation carried and an arbitrary
host path has no id a client could ask for.

### workspace (`internal/workspace`)

The verbs, each a function delegating to wsm + gitclient + shimclient +
queue + resolvers: `Register`, `RegisterRepository` (a repository ON ITS OWN,
with no workspace: resolved from ANY path inside it through `RepositoryOf`,
idempotent by the resolved main-worktree dir, answering whether the registry
already held it; it republishes the roster so the new repository's EMPTY
section draws), `Create` (standard + one-shot; slug from the
initial prompt via the naming rule; branch; worktree; layout facts recorded;
registration only after materialization; consent check for ungated modes;
fork = the daemon ports the parent's transcript into the child's config
root project dir before StartSession(resume)), `Open` (spawn on mount
semantics), `Close` (requires quiet: no turn, no live work, no held prompts,
no queued merge; refusal manifests in the footer), `Kill`, `Nuke`, `Forget`
(registration's undo: the record goes, and its repository's record with it when
nothing else references that repository; NO files are touched. Refuses an open
workspace, a workspace that is not quiet, and one others were spawned from.
Reachable today only through the command-file ingress -- the rpc is unlanded,
see ERROR-ARMS.md), `Restart`
(delegates to rollout.RelaunchShim; force = KillSession{force:true} first;
owns the reload_webapp push when needed), `Select`, `SetPriority`, tasks,
`AnswerColdGate`, `SetModel`/`SetPermissionMode` (through the queue's
session-act path; ungated mode needs consent recorded at creation),
`SetEffort` (NOT through the queue: straight to the shim's SetSessionEffort,
which itself waits for the running turn, so no row ever stands for it; a new
shim is put back at the picked level by `Fleet.sessionUp`),
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
- GIT IS NEVER CALLED DURING TESTING (user directive, binding): packages
  above the git client test against a fake `gitclient.Git`; the integration
  harness scripts git facts as fixtures; the git-client leaf tests against a
  scripted fake `git` executable first on PATH; the merge test gate is a
  scripted fake script. No `git init`, no temp repositories in any test.
- No real vendor calls anywhere: every test sets
  `AGENT_REPL_FORBID_VENDOR_CALLS=1`; the classifier and any exec site
  check `envc.VendorGuard`. THE SHIM SPAWN IS THE ONE SITE THAT DOES NOT
  REFUSE: the shim is the daemon's own process and runs the whole real shim
  over a scripted SDK under `--fake`, so the guard forces fake mode on the
  spawn (`shimclient.fakeMode`) rather than refusing it. Refusing made a
  workspace impossible to create under the guard, and forcing the fake is
  strictly stronger: the child cannot reach the vendor whatever the caller
  asked for, and the shim's own guard still throws at `createRealQuery`.
- Workflow is kicked: workflow rpcs and arms are answered/ignored with a
  typed not-implemented refusal (transport-layer, intended arm
  `<Rpc>Error.not_implemented`); no watch is ever opened for a workflow.
- LANDED at overhaul/integration 80a7a0322 (merged): see docs/overhaul/daemon.md
  "Kickoff increments and rulings". Additions beyond the list below:
  `RequestCommandSupport{workspace, command}` → `{WorkspaceRef}` composes the
  add-support brief from `prompts/add-support-slash-command.md` (loud on
  absence) and creates via the ordinary standard form with initial_prompt;
  `SubmitPromptRequest.origin` is REQUIRED (UNSPECIFIED refused; persisted
  onto the turn via StartTurn); `UpdateHeldPrompt.accept` is legal only on a
  `hold_for_turn_end` verdict (flips HeldPromptAccepted, re-pushes the tray);
  `SessionFault.kind` (store_unreachable | converter_defect |
  log_sink_poisoned | keepalive_failed | vendor_query_failed) feeds the
  topbar's session_fault warnings; every shim.v1 failure now carries a
  typed `kind`/`cause` arm the daemon switches on. Panel/refusal rows mint
  FeedIds from (workspace, a per-workspace monotonically increasing
  synthesized sequence) so re-pushes upsert. Roster `closed = true` on
  merged, closed AND killed rows; a nuked row leaves the roster. Emacs
  launches `daemon/bin/claude-repld` with NO argv (state via env); the
  joining successor is spawned with `-joining <incumbent address>`
  (documented in daemon/AGENTS.md).
- RULINGS (project lead landing, 2026-08-29):
  - Q1: `/agents` and `/help` ARE recognized and never forwarded to the shim;
    they answer as `command_refused` (the add-support card, "not supported").
    AgentsPanelView / HelpPanelView stay unproduced.
  - Q2: WEB LINK verb `OpenInEditor{workspace, path, optional line}` + a
    `WatchHostWorkspaceResponse.open_in_editor{path, optional line}` push:
    the daemon validates the workspace and relays the click onto that
    workspace's host stream; no ack, no command loop.
  - Q3: recognized command panels and command refusals are MIRRORED into the
    ROOT FEED as synthesized NON-DURABLE rows — `FeedRow.command_panel` (a
    panel oneof over the six views) and `FeedRow.command_refused{literal,
    composed reason, add-support offer marker}`; resolver memory only, never
    stored, never replayed after restart. SubmitPromptSuccess keeps
    `command_panel` and gains `command_refused`.
  - Q4: superseded — `accept` landed (see above).
  - Login: `WatchLoginTerminal` is a SERVER stream (request {workspace};
    output bytes|closed) plus unary `SendLoginInput{workspace, oneof
    keystrokes|resize}`.
  - `TopbarView.permission_mode_picker{current, options[{mode, display_name}]}`:
    the daemon serves the switchable set it will accept for the session;
    SetPermissionMode validates against what it served.
  - `AgentUpdate` gains page-line arms `context_cut` (ContextCut: /clear,
    compaction, compaction_failed — the feed draws the separation divider
    from it) and `api_error` (ApiRequestFailed as MID-TURN evidence, never a
    terminal — the feed/footer draw it as retry/notice evidence).
  - shim.v1 failure `kind` oneofs now carry the shim's real arms; the daemon
    switches on them (respelled into the feed/footer/fault vocabularies).
  - WorkspaceRef echo: clients echo the FULL ref; the daemon keys on `id` and
    REFUSES a ref whose `dir` disagrees with the registry (intended arm
    `<Rpc>Error.workspace_ref_mismatch` until landed). The webview URL is
    `http://<daemon.addr>/?workspace=<id>&dir=<normalized dir>`.

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

## Landing 3 (LANDED at overhaul/integration 87234e51c, merged at c0a4cf153 — see docs/overhaul/daemon.md "Landing 3 relay"; the error-arm batch is landing 4)

Build against these shapes behind your OWN seam types now; swap to the
generated arms when the landing merges (one place each):
- `FeedTurnEndedErrored.headline` — a daemon-composed per-arm sentence (the
  feed resolver composes it; the client's sentence table dies).
- `FeedToolCallReturned.form.none` — a returned call with nothing to draw;
  never `text{""}`.
- `FeedToolCallInput.form` = command | path | query — the daemon states the
  input line's drawn form (shell line vs muted path vs query).
- `HostNotificationKind.question_asked{header}` — a blocked question's
  notification (route as agent_addressed until it lands).
- `DetachedLost{file_vanished | went_silent | swept_up}` as `lost` arms on
  AgentBashInterrupted.cause, AgentSubagentFailure.cause and
  AgentFailure.failure — the feed resolver maps them to FeedShellLost /
  FeedSubagentLost; the sessionwatcher routes them as ordinary terminals.
- `FeedColdGateResolvedCompact.scope` (SessionCompactScope) — the resolved
  trace names what was summarized; the feed resolver carries model + scope.
- Ungated permission modes are exactly `bypass` and `dont_ask`; `auto` keeps
  a gate and needs no creation consent.
- One-shot FINISH execution (self_merge / open_pr on the turn that concludes
  with the success marker) is the prompt queue's turn-terminal hook calling
  a workspace verb; the workspace verbs only record the finish action.
- The error-arm batch (every `<Rpc>Error` arm incl. transferring_away /
  not_yet_adopted, and the DaemonFault / SessionFault / HostFault kind arms)
  is collected in ERROR-ARMS.md and sent by the teamlead once the server
  handlers expose the sites.

## Deploy (daemon-owned, 2026-09-23)

- There is no deploy chain script: the daemon builds into staging with
  `bin/build-frontend.sh --out`, judges staleness by content hash, installs,
  and restarts what is out of date (`internal/deploy`; daemon/AGENTS.md
  "Deploy").
- `build-frontend.sh` without `--out` builds `daemon/bin/claude-repld` from
  `./cmd/claude-repld` for Emacs's cold start.
- `agent-shim/wire` is deleted with the rewrite (nothing in the daemon
  imports it) and its `bin/test-all.sh` roster entry dropped.

## Standing-stream mechanics (connect-go v1.17.0, system-wide rule)

- SERVER: every Watch* handler (WatchFeed, WatchFooter, WatchTopbar,
  WatchWorkspaceRoster, WatchDaemonHolds, WatchHostWorkspace, WatchDaemon,
  WatchWebWorkspace, WatchLoginTerminal) FLUSHES its response headers the
  moment it accepts the stream (`stream.ResponseHeader()` set, then an
  explicit flush via the underlying `http.Flusher` / `connect` send of the
  headers before the first frame), so acceptance is observable before the
  first published view arrives; a refused open is a Connect error before
  any frame.
- CLIENT (the shim client): a standing watch is ended by CANCELLING its
  context, never by `Close` alone — `ServerStreamForClient.Close` drains
  the body and blocks forever on a stream that never ends. `CallServerStream`
  returns once headers arrive (before the first frame under early flush);
  refusals surface at the first `Receive`, so the opening frame is consumed
  as the open's answer (WatchSession's first `diagnostics` push).
- IMPLEMENTATION of flush-on-accept in connect-go: response headers are sent
  lazily (first Send or handler return), so every Watch* handler must act:
  when a most-recently-published view exists, the subscription invariant's
  first Send happens immediately; when none exists yet, flush headers via a
  ResponseWriter wrapper installed on the h2c mux that `accept`s (status 200
  + the request's streaming content type, flushed) the moment the
  subscription is registered, swallows connect-go's later WriteHeader
  (warning if the status disagrees), and implements `Unwrap()` so
  `http.NewResponseController` still reaches the real writer; unary is
  untouched; stream-request validation runs BEFORE registration so refusals
  stay refusals. A proven copy (read-only reference): branch
  overhaul/elisp-integration, worktree
  /Users/dodgecoates/.config/doom-overhaul/elisp-agents/integration, file
  modules/app/agent-repl/lisp/testsupport/fakedaemon/accept.go (+ test).
  Verify in the integration suite: a Watch* open on a workspace with no
  published view returns headers before any frame.
- Live-work items restored from `SessionStarted.live_work` have NO
  announcing agent: the feed/footer/sidebar sinks receive a nil agent and
  MUST place such items on the ROOT feed (root-feed fallback), never drop
  them (ruled by the daemon lead; a shim-side re-announce with the
  `created` origin is requested upstream).

## Cross-system literals the daemon consumes (pinned)

- METAPROMPT SENTINELS (Emacs `agent-repl--meta-wrap`, lisp/core.el): an
  injected span is `<!--agent-repl:meta-->` + text + `<!--/agent-repl:meta-->`.
  `prompts.StripSentinels` removes every such span (and the whitespace it
  leaves) from the DRAWN prompt text only; the record keeps the full text.
  The daemon wraps its own injected spans (one-shot decoration, add-support
  briefs, merge briefs) with the same markers.
- `.claude.json` (per account root): the daemon READS only
  `oauthAccount.emailAddress` (absent → logged out; malformed file → error)
  and NEVER writes the file; the project entry the CLI keeps under
  `projects.<cwd>` is not read or written by the daemon — transcript porting
  moves files under `<root>/projects/<encoded cwd>/` only.

## Landing 4 (staged on overhaul/landing-4; lands with the ERROR-ARMS batch)

Rulings already binding; code swaps to the generated arms when it lands:
- `SessionStarted.live_work` items are ALWAYS `created`-origin on
  re-adoption (ruled on the shim); the sessionwatcher's ERROR + skip on a
  `detached`-origin live item is the correct contract-violation handling.
- `DetachedWorkId.value == the unit's AgentActivityId.value` (same bytes; a
  subagent's is also its AgentId) — so a `created`-origin MONITOR is retired
  by the monitor's own `ended`/`failure` frame whose activity id equals the
  handle. Monitors stay in freeness. (Sessionwatcher remediation at landing
  4: key the created-monitor reap by that equality.)
- `SessionUpdate.context_budget_warning` (tag 24) is RETIRED; the arm becomes
  `AgentUpdate.context_budget_warning = 7 {text}` — a page line, sidecar-
  produced, arriving via WatchAgent. Route it to the footer from the agent
  plane (sessionwatcher: new AgentUpdate arm → FooterSink; delete the
  WatchSession routing; footer: unchanged consumer). Since RETIRED with the
  footer's `context_budget` kind (owner ruling, 2026-10-06): tag 7 is
  reserved, nothing routes it.
- Restored live-work items route to the root feed (agreed).
- The landing-4 batch also carries the ERROR-ARMS.md arms and the four e2e
  seam answers (arm names; the merge test-gate invocation; the .claude.json
  key path; the metaprompt sentinels — the last two are pinned above).

## Handover: the web side never redials (project lead ruling)

- On `transferring_away{address}` / `transferred{address}` the WEBAPP does
  not dial the successor; Emacs reloads the webview at the successor's
  address and the FRESH page calls `AdoptWebWorkspace` ONCE AT BOOT, before
  opening any view stream. The host side is unchanged (Emacs calls
  `AdoptHostWorkspace` on the announcement).
- Consequences for `rollout.AdoptWeb` and the server handler:
  `no_transfer_announced{}` is the ORDINARY answer on every non-handover
  page boot — logged at INFO at most, never WARN/ERROR, never a fault;
  `not_yet_adopted{}` is answered while adoption is in progress (the page
  retries with backoff); the successor's expected-participant count for the
  web side is satisfied by the reloaded page's adopt call, not by a
  surviving stream (record the web participant as "expected" from the old
  daemon's snapshot, and mark it satisfied by the first AdoptWebWorkspace
  from any connection).
- ON A JOINING SUCCESSOR THE REFUSAL IS NOT IMMEDIATE. `Handover` announces
  the successor's address BEFORE it writes the intent manifest, so a
  participant that dials the announced address at once can arrive ahead of
  the arm. `rollout.rendezvousCall` HOLDS such a call (`awaitArm`) until the
  manifest is read, bounded by `Deps.AdoptionWindow`; once a manifest is in
  hand the transfer set is known and a workspace it does not name is refused
  at once. A daemon that is not joining never waits, so the ordinary page
  boot above is unchanged.
- Vocab merge note: `footer_allowance` also landed on overhaul/integration
  (8c56dece8) directly; when overhaul/daemon merges into integration the
  render-colors.json conflict resolves to the daemon's version.

## FooterAllowance sourcing (project lead ruling, supersedes the landing-4 adaptation)

- `SessionUpdate.account_usage` is NOT retired. FIGURES (utilization,
  resets_at) come from account_usage: five_hour → `session`, seven_day →
  `weekly`; sampled at a cadence and complete from the first sample.
- The VERDICT (`FooterAllowance.status` arm) comes from
  `SessionUpdate.rate_limit_status`, matched by window: five_hour →
  session; seven_day / seven_day_opus / seven_day_sonnet /
  seven_day_overage_included → weekly; `overage` → the `overage`
  allowance, its own cell since 2026-09-13. It
  stays UNSET until a rate-limit event for that window has been seen — an
  unset status oneof is LEGAL ("no vendor verdict observed yet").
- The allowance line draws as soon as a usage sample exists; the verdict
  arm joins when it arrives.
- A per-seat sample (`SessionAccountUsage.seat_spend`) draws the seat's
  spend instead (`FooterActivityEnduring.seat_spend`) and clears the windows;
  a subscription sample ends the seat mode. While the account is per seat a
  rate-limit event files no window (owner ruling, 2026-10-06: the line is by
  billing mode, never both).
- A rate-limit event carrying a utilization for the same window that is
  NEWER than the last sample wins for the figure.
- Footer remediation owed: replace the "both windows from rate_limit_status"
  rule with the above (tests per bullet).

## Merge orchestrator rulings (project lead, on its report)

- Pause/Resume scope: landing 5 adds `optional RepositoryRef repository` to
  UpdateMergeQueuePause/Resume (UNSET = every repository); until it lands
  the daemon-wide switch is correct.
- `gitclient.Git` gains `Commit(ctx, dir, message string) (sha string, err
  error)` (`commit --no-edit -m <message>`; the fake-git tests cover argv
  only) — the merge orchestrator completes a resolved conflict through it.
- `prompts/merge-conflict-resolve.md` and `merge-test-failure-resolve.md`
  still describe the retired rebase-worktree/cherry-pick flow: the prompts
  agent rewrites their BODIES for the no-ff-merge-in-target flow with the
  placeholder sets unchanged.
- Recovery re-queues an in-flight merge at the FRONT of its repo queue
  rather than re-entering a tab: accepted as an override (recorded in
  docs/overhaul/daemon.md). SUPERSEDED 2026-10-06 (owner): a merge always
  resumes at the step its durable progress record names, in the same bubble;
  see daemon/AGENTS.md "A MERGE ALWAYS RESUMES WHERE IT LEFT OFF".
- Terminal ordering post-prompt → terminal → release → worktree removal →
  displaced turn → rollout trigger: accepted.

## Landing 5 (LANDED at 081dbbba8, merged at 05b460c4b) — remediations owed

- FEED (resume the feed agent): (1) `AgentBashOutput.not_observed` (and an
  AgentBashInterrupted whose output is not_observed) maps to an UNSET
  `FeedShell.spool` — never an empty `FeedShellSpool{text:""}` — with the
  settled arm exactly as the record states it (completed/cancelled/lost);
  pin with a resolver test; no new frontend element. (2) A vendor-
  synthesized notice sets `FeedResponse.notice{heading}` instead of
  prepending a heading to the prose; the prose stays verbatim.
- MERGE (resume the merge agent): UpdateMergeQueuePause/Resume gained
  `optional workspace.v1.RepositoryRef repository` — UNSET = every
  repository (the current daemon-wide switch is the unset case); a set ref
  scopes the pause/resume to that repo's queue; typed refusal when the ref
  is unknown.

## The canonical token-figure format (daemon-wide; the webapp copies it)

One formatter, `internal/figures.Tokens(n uint64) string` (extraction from
the three per-resolver copies is owed in the feed remediation; footer and
topbar swap imports):
- n < 1000 → unscaled decimal digits ("0", "999").
- otherwise scale by the RENDERED unit: k = n/1000, M = n/1,000,000 —
  rendered with EXACTLY ONE fractional digit (strconv.FormatFloat 'f' 1,
  round-to-nearest with binary-float ties), then a trailing ".0" trimmed.
  One fractional digit applies at EVERY scaled magnitude ("1.2k", "12.3k",
  "182.4k", "1.2M") — the proto's own examples ("18.2k", "142.3k") fix this;
  there is no drop-the-fraction-from-ten rule.
- UNIT SELECTION IS BY THE RENDERED VALUE: a count whose k-rendering would
  reach "1000k" (n ≥ 999,950) renders "1M" instead; same rule at every
  boundary.
- Examples: 0→"0", 999→"999", 1000→"1k", 1200→"1.2k", 12340→"12.3k",
  182000→"182k", 999949→"999.9k", 999950→"1M", 1200000→"1.2M".
- Suffixes composed by the call site ("18.2k in", "12.4k tok") wrap this
  value; the formatter emits only the figure.

## commandfile

The command-file ingress sweeps `$AGENT_REPL_STATE_DIR/output/workspace_commands_*.json`
and maps every entry onto the same internal path as the equivalent rpc. A file
is ONE request: an entry that does not validate, or whose directory cannot be
resolved, applies nothing from the whole file, which retires to `quarantine/`
at WARN `daemon.commandfile.quarantine` with the refused entry and field in its
cause.

EVERY DIRECTORY FIELD (`git_root`, `project_dir`, `dir`, `source_dir`,
`evict_dir`, `repository_dir`) goes through
`dirpath.Absolute` before anything reads it. A leading `~` expands to the home
directory the daemon resolved at boot, because the `/create-or-update-workspace`
skill's contract says a leading `~` is expanded downstream. `~user` and any path
still relative afterwards are refused. None is resolved against the daemon's
working directory, which is wherever launchd started it:
`filepath.Abs("~/.config/doom")` once named `/Users/me/~/.config/doom`, and
the create that carried it was quarantined as an unknown repository
(2026-09-28).

THE DECODE IS STRICT: a field this reader does not declare quarantines the
file. `encoding/json` drops one without a word, and for a month that dropped
every create field the `/create-or-update-workspace` skill writes beyond
`name`, `git_root` and `prompt`. A create-only field on any other verb is
refused for the same reason. A create maps onto the `CreateSpec` the
`CreateWorkspace` rpc fills for the same request:

| field | `CreateSpec` |
|---|---|
| `git_root` | `RepoDir`: a registered repository's main checkout, or the repository of the registered workspace whose worktree it is (the skill's default `git_root` is the source workspace's own path); anything else is left for the verb to refuse as `unknown_repository` |
| `source_ws` `{name, path}` | `Parent`: none when `path` is the repository's main checkout, else the registered workspace at `path`, which must be of the same repository; `name` is display only |
| `fork_from` | `ForkFrom` and `Parent`: the ONE open workspace of the repository with that name. A fork is always from the parent (`CreateWorkspaceParent.fork`), so a `source_ws` naming a different workspace is refused, and so is a base beside it |
| `base_commit`, `base_ref` | `BaseRef`; the two spellings are refused together |
| `model` | `Model`, blank as unset |
| `priority` | `Priority`: `p05`, `p1`, `p2`, `p3` |
| `before_ws_merge`, `postprocessing_prompt` | `MergeActions.Before`, `MergeActions.After` |

### The merge queue's controls

Three entry types mirror `UpdateMergeQueue`'s three arms and call the same
orchestrator entry points (`merge.Orchestrator.Evict`, `Pause`, `Unpause`).
Each names its REQUESTER by `project_dir` (or `dir`, or a `workspace` id), as
a `merge` entry does; that is the workspace whose agent wrote it.

| type | fields | maps onto |
|---|---|---|
| `merge_evict` | `evict_dir` optional: another workspace's worktree root | `Evict` of the workspace at `evict_dir`, else of the requester |
| `merge_pause` | `repository_dir` optional: a repository's main checkout | `Pause` of that repository's queue, else of every repository |
| `merge_resume` | `repository_dir` optional, as for a pause | `Unpause`, scoped as a pause is |

`evict_dir` is refused on any other type, and `repository_dir` on any but a
pause or a resume. The OUTCOME is the file's fate plus one INFO
`daemon.commandfile.merge_queue` record whose `path` is the file and whose
`outcome` is `evicted`, `not_queued`, `paused` or `resumed`. EVICTING A
WORKSPACE WITH NOTHING ON THE QUEUE IS AN ANSWER, NOT A FAILURE: the rpc's
`no_such_queued_merge` applies the file with `outcome: not_queued`. Every other
refusal (`already_paused`, `not_paused`, `unknown_repository`, an unregistered
requester or `evict_dir`) quarantines the file at WARN
`daemon.commandfile.entry` with the arm in its `cause`.

## heldingress

A client whose `SubmitPrompt` the daemon did not answer — no daemon, a stuck
one, a handover refusal — never holds the prompt in its own memory. It writes
ONE file per prompt into `$AGENT_REPL_STATE_DIR/held-prompts/`, under a
dot-prefixed temporary name renamed into place, named
`held_<UTC %Y%m%dT%H%M%S.%N>_<md5hex(dir)[:8]>_<idempotency key>.json` so
name order is write order and a client can count a workspace's waiting prompts
from the names alone:

```json
{
  "version": 1,
  "project_dir": "/abs/path/of/the/workspace/worktree",
  "idempotency_key": "the key of the SubmitPrompt attempt it re-drives",
  "origin": "PROMPT_ORIGIN_USER_SENT",
  "said": { "content": { "blocks": [ { "text": { "text": "..." } } ] } },
  "queued_at": "2026-09-28T12:00:00.000000000Z",
  "delivery": "SUBMIT_PROMPT_DELIVERY_DEFERRED"
}
```

`said` is the request's `UserSaid` in protojson and `origin` the
`PromptOrigin` value name. `delivery` is optional and mirrors
`SubmitPromptRequest.delivery`: absent is the ordinary delivery, and a deferred
prompt (`SPC j RET`) names `SUBMIT_PROMPT_DELIVERY_DEFERRED`, so the queue holds
it for the running turn's end unjudged, exactly as the rpc would. Unknown
fields, another version, a relative directory, a missing key, an unspecified
origin or a delivery this reader does not honor make the file malformed.

The daemon sweeps the directory at start and every 250ms, in name order, and
hands each entry to `prompthandler.Handler.Submit` — the rpc's own body — under
the entry's key. So the queue holds, classifies and delivers it by its ordinary
rules and it appears in the held tray like any held prompt. An entry leaves the
directory in exactly two ways:

- its prompt was ACCEPTED (INFO `daemon.heldingress.ingest`) or the key was
  already accepted, by this daemon or an earlier one (INFO
  `daemon.heldingress.dedupe`) — the file is removed only after that answer,
  and the workspace's host state is then re-pushed so a client re-counting the
  directory reads the removal;
- it is malformed: moved to `quarantine/` at WARN.

A crash before the removal leaves the file; the next sweep resubmits it under
the same key and the durable claim answers it as a duplicate, so a crash
mid-ingest neither loses nor duplicates. A refusal (a merge in flight, a cold
gate, no session, a move sealed toward another daemon, an unregistered
directory, a fault) leaves the entry and every later entry for the same
workspace, so order is kept; it is retried after a delay doubling from the
interval to 10s, recorded once PER KIND at its level (INFO for the queue's
answers, WARN for an unregistered directory, ERROR for a fault) and at DEBUG on
each retry of a kind already stated. The submission carries
`prompthandler.WithRedrive`, so the queue records its own side of a standing
refusal at DEBUG: the ingress's record is the one that says it.

ONLY THE DAEMON THAT SERVES SWEEPS (`intakegate`, answered by
`rollout.Controller.ServesIntake`): not a successor still joining, and not an
incumbent whose handover or restart has begun. The ingress calls the handler
directly, so the server's handover refusals never reach it; without the gate a
joining successor would revive a session its incumbent still runs. Between the
incumbent's stop and the successor's start nothing is taken and every entry
waits on disk. A sweep is also EXCLUSIVE across daemons, under the kernel lock
`held-prompts/.sweep.lock` (`flock`), because a sweep the incumbent began before
its handover can still be submitting when the successor's gate opens, and an
entry submitted by both before either accepted its key would be delivered
twice. The command-file ingress takes the same gate; its per-file exclusivity is
the claim rename, and a claim lost to the other daemon is DEBUG, never an
ERROR.
