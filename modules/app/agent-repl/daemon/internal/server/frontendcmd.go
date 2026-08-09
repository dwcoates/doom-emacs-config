// frontendcmd.go binds the daemon's frontend.v1 command surface: the
// FrontendCommand handler that routes each inbound command to the module that
// owns it (design §14.2 point 1, §5.4), and the SSM-backed StateProvider the
// frontend server snapshots on every (re)connect.
//
// The handler dispatches through NARROW injected interfaces (PromptRouter,
// MergeRunner, WorkspaceLifecycle) rather than reaching into the modules
// directly, so the routing is unit-testable and the concrete bindings (the
// per-session shimclient, the merge.Driver, the Emacs workspace-command
// channel) are assembled by WireAgentShim / main.go. Every path surfaces its
// failure as a CommandAck error via the returned error — never a silent drop.
package server

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"path/filepath"
	"sync"
	"sync/atomic"
	"time"

	corev1 "agentrepl/proto/agentshim/core/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/errclass"
	"claude-repld/internal/frontend"
	"claude-repld/internal/protocol"
	"claude-repld/internal/registry"
	"claude-repld/internal/sessioncontroller"
	"claude-repld/internal/ssm"
	"claude-repld/internal/workspace/geometry"
	"claude-repld/internal/workspace/merge"

	"google.golang.org/protobuf/types/known/structpb"
)

// PromptRouter forwards conversation control to a workspace's session shim
// (the per-session shimclient). A workspace with no live shim connection must
// return a loud error, never a silent no-op — the frontend renders the failed
// CommandAck.
type PromptRouter interface {
	// SubmitPrompt carries the COMMAND'S OWN request id through to the session controller:
	// it is what the daemon's immediate prompt receipt is keyed on, and what
	// the durable transcript line is later stamped with, so a frontend
	// reconciles the two onto one bubble.
	SubmitPrompt(ctx context.Context, workspace, requestID, text, permissionMode string, promptOrigin corev1.PromptOrigin) error
	// Interrupt carries the COMMAND'S OWN request id through to the session
	// controller for the same reason SubmitPrompt does: it is the only id the
	// user's client, the daemon's command log and the shim's control exchange
	// can be reconciled on. The wire itself travels under a daemon-minted
	// control id that appears in no caller's records.
	Interrupt(ctx context.Context, workspace, requestID string) error
	AnswerPermission(ctx context.Context, workspace, permissionRequestID string, allow bool, denyMessage string, updatedInput *structpb.Struct) error
	SetModel(ctx context.Context, workspace, model string) (string, error)
	// ResolveMergeConflict drives the workspace's OWN session to resolve a
	// cherry-pick merge.Coordinator parked on a conflict, returning once that
	// resolution turn has ended.
	//
	// It belongs on THIS interface because it is a prompt submitted to that same
	// session — the one prompt path the merge lease admits — so the fleet that
	// serves the user's prompts is necessarily the fleet the merge drives.
	ResolveMergeConflict(ctx context.Context, res merge.ConflictResolution) error
	// ResolveMergeTestFailure drives the workspace's OWN session to fix a test
	// suite a cherry-picked commit broke on the target, returning once that
	// resolution turn has ended. It is the test-gate sibling of
	// ResolveMergeConflict and lives here for the same reason.
	ResolveMergeTestFailure(ctx context.Context, res merge.TestFailureResolution) error
	// RunMergeBeforeAction drives the workspace's OWN session to run the
	// before_ws_merge action it was created with, returning once that turn has
	// ended. Third sibling of the two above, here for the same reason: it is a
	// prompt on the one path the merge lease admits.
	RunMergeBeforeAction(ctx context.Context, act merge.BeforeAction) error
	// RunMergeAfterAction drives the workspace's OWN session to run the
	// postprocessing action it was created with, once every commit has landed,
	// returning when that turn has ended. Fourth sibling, here for the same
	// reason: it is a prompt on the one path the merge lease admits.
	RunMergeAfterAction(ctx context.Context, act merge.AfterAction) error
}

// SessionRestarter hard-restarts one workspace's session: stop its shim, bring
// the same record back up. Satisfied by *sessioncontroller.Manager.
type SessionRestarter interface {
	RestartSession(ctx context.Context, workspace string) error
}

// SessionHibernator is the user-facing hibernation surface: the forced sleep
// and the two revival modes. Satisfied by *sessioncontroller.Manager.
//
// It is separate from SessionRestarter because a restart and a hibernation are
// opposite intents about the same shim — one replaces the process and keeps the
// session usable, the other stops it and gates the session behind a choice —
// and a single interface carrying both would let a caller reach for the wrong
// one by autocomplete.
type SessionHibernator interface {
	HibernateWorkspace(workspace string) error
	ReviveSession(ctx context.Context, workspace string, mode sessioncontroller.ReviveMode) error
}

// SessionHealthRouter proves a named session's existing daemon-to-shim path.
// It is intentionally separate from PromptRouter: a health probe must never
// lazily create or revive a shim just because a frontend asked whether one is
// ready.
type SessionHealthRouter interface {
	Health(ctx context.Context, workspace, sessionID, requestID string) (*corev1.HealthStatus, error)
}

// DaemonHealthChecker reports whether every daemon-global boot dependency is
// ready.  The HTTP /healthz route and frontend command use the same checker,
// so startup cannot have two competing definitions of operational.
type DaemonHealthChecker interface {
	DaemonHealth() (healthy bool, reason string)
}

// MergeRunner runs (or resumes) a workspace merge against a FULLY RESOLVED
// geometry.
//
// It takes a whole merge.Request rather than a workspace name because resolving
// the three coordinates is the command handler's job (see resolveMergeGeometry),
// and by the time a request reaches this port there is exactly one answer on it.
// The runner never re-derives anything: a second resolution point is a second
// owner of the map, which is the failure this whole subsystem was reorganized
// to make unrepresentable.
// It is merge.Coordinator, NOT merge.Driver. Enqueue returns as soon as the
// request is durably on its repository's queue, because a merge that has to
// wait its turn cannot report a terminal outcome inline — the outcome arrives
// as pushed merge state instead. Calling merge.Driver straight from the command
// handler is what let two merge_workspace commands cherry-pick into one target
// worktree at the same time.
type MergeRunner interface {
	// Enqueue admits a cherry-pick merge onto its repository's queue and
	// returns the position it landed at.
	Enqueue(ctx context.Context, req merge.Request) (merge.Position, error)
	// Resume continues a human-resolved conflict (the
	// conflict_resolved_continue handoff, §9.3).
	Resume(ctx context.Context, req merge.Request) error
	// Abandon gives up the workspace's in-flight merge, if any. Closing a
	// workspace whose merge is parked on a conflict is the abandonment the
	// merge.Lease contract names — the lease must release on it like on any
	// other terminal phase — and abandonment has no command of its own on the
	// wire, so close_workspace routes here before the lifecycle close.
	Abandon(ctx context.Context, workspace string) (bool, error)
	// Evict takes the workspace's WAITING merges off their repositories'
	// queues, reporting how many entries it dropped. It is the queue half of an
	// interrupt: a merge waiting its turn is work the user asked to stop, and
	// no other command on the wire reaches it.
	//
	// A merge already in flight is NOT evicted — the coordinator's head holds
	// the shim lease and may be mid-cherry-pick — so zero is the ordinary
	// answer and never an error.
	Evict(ctx context.Context, workspace string) (int, error)
}

// MergeGeometrySource answers a workspace's recorded merge geometry: its source
// branch, its own worktree, and the worktree its commits land in.
//
// It exists because the daemon now OWNS that map. Emacs used to compute all
// three and ride them on every merge_workspace command; two owners of one map
// is how a merge landed against a target the daemon had never heard of, so the
// command is now a bare request keyed by workspace and the answer comes from
// here. Satisfied by *geometry.Store.
//
// A workspace with no record is reported as found=false and NEVER as a
// synthesized answer. Refusing the merge with an explanation is the whole point:
// a cherry-pick run against a guessed target writes commits into a repository
// nobody asked for.
type MergeGeometrySource interface {
	Lookup(ctx context.Context, workspace string) (geometry.Record, bool, error)
}

// WorkspaceLifecycle closes/opens a workspace. Bound to the Emacs
// workspace-command channel (workspacecmd) at stitch.
type WorkspaceLifecycle interface {
	Close(ctx context.Context, workspace string) error
	// Open makes a workspace ready to render and drive, CREATING its session
	// when it has none. opts carries the run preferences for a session it
	// starts and is unread when the workspace already has one.
	Open(ctx context.Context, workspace string, opts WorkspaceOpenOpts) error
	// OpenDriveable is Open for a caller that will DRIVE the session it opens
	// (merge.SessionBringUp): it returns only once the shim has handshaked, so
	// the caller's next send cannot race the shim's boot, and it reports
	// merge.ErrNoSession for a workspace that has no session record at all.
	//
	// It sits on this interface rather than behind a type assertion so a
	// lifecycle that cannot serve a merge is a COMPILE error instead of a
	// wiring-time one.
	OpenDriveable(ctx context.Context, workspace string) error
	// OpenForMerge is the merge-only direct revival boundary. It makes the
	// workspace driveable after retiring a durable hibernation gate when one
	// exists, without treating a user revival mode as implicit.
	OpenForMerge(ctx context.Context, workspace string) error
}

// WorkspaceCreationBridge is the server-side seam for daemon-owned workspace
// creation.  It deliberately speaks only frontend.v1 messages: the concrete
// adapter may compose workspace/create.Manager, its durable store, and the
// inbox without making this transport/server package import any of them.
//
// A nil bridge is an explicit unsupported capability: the command handler
// returns a loud Nack rather than claiming a workspace or host action exists.
type WorkspaceCreationBridge interface {
	MarkWorkspaceMaterialized(ctx context.Context, jobID string) error
	CompleteHostAction(ctx context.Context, actionID string, ok bool, failure string) error
	SessionPublicationDecision(worktreePath, sessionID string) (SessionPublicationDecision, error)
	SubscribeSessionPublicationReleases() (<-chan SessionPublicationRelease, func())
	SnapshotHostWork() WorkspaceHostWorkSnapshot
	SubscribeWorkspaceAvailable() (<-chan *frontendv1.WorkspaceAvailable, func())
	SubscribeHostActions() (<-chan *frontendv1.HostAction, func())
}

// SessionPublicationDecision is the creation subsystem's durable verdict for
// one session-scoped frontend frame.  A denied decision names the create job
// that has not received the host's WorkspaceMaterialized acknowledgement.
type SessionPublicationDecision struct {
	JobID        string
	WorktreePath string
	SessionID    string
	Materialized bool
}

// SessionPublicationRelease is emitted after the durable materialization
// acknowledgement and before the creation worker may release an initial
// prompt.  The server uses it to publish fresh authoritative state rather than
// replaying pre-materialization frames.
type SessionPublicationRelease struct {
	JobID        string
	WorktreePath string
	SessionID    string
	// Open flips the bridge's in-memory publication decision while the frontend
	// holds its writer-side publication lock. It must run before Snapshot.
	Open func() error
	// Completion acknowledges that the server has enqueued the authoritative
	// post-materialization snapshot. The creation worker waits for it before
	// submitting the initial prompt, making snapshot-before-prompt structural.
	Completion chan error
}

// WorkspaceHostWorkSnapshot is the durable host-only subset a reconnecting
// Emacs receives.  Its proto representation lets frontend.Server enforce its
// ClientKind boundary without knowing how the creation store persists jobs.
type WorkspaceHostWorkSnapshot struct {
	WorkspaceAvailable []*frontendv1.WorkspaceAvailable
	HostActions        []*frontendv1.HostAction
}

// Resyncer replays a workspace's retained conversation deltas from the
// requested seq INCLUSIVE (design §5.4), the conversation-delta half of a
// frontend resync the StateSnapshot re-send does not cover. The implementation
// raises that start to the newest clear or compaction when there is one, so a
// frontend is never served history one of those already discarded. Satisfied by
// *sessioncontroller.Manager.
type Resyncer interface {
	ResyncForGeneration(workspace, expectedSessionID, expectedGenerationID string, fromSeq uint64) error
}

// SessionCreateDeleter is the daemon-core session-lifecycle surface behind the
// createSession/deleteSession UDS commands (the same core POST /sessions and
// DELETE /sessions/{id} use). *Server satisfies it, but it is constructed AFTER
// WireAgentShim, so main injects the late-bound *SessionCommandBinding.
type SessionCreateDeleter interface {
	// CreateSession returns the durable session id alongside an error when
	// registration succeeded but bring-up failed. The command boundary uses
	// that id in exact-resume continuity evidence.
	CreateSession(ctx context.Context, opts CreateOpts) (string, error)
	DeleteSession(sessionID string) error
}

// DaemonViewSource supplies the daemon-identity frame for the connect snapshot
// (boot id, protocol version, binary mtime, version). *Server satisfies it via
// the same late-bound binding.
type DaemonViewSource interface {
	DaemonView() *frontendv1.DaemonView
}

// SessionCommands is the combined daemon-core surface the frontend command
// handler and snapshot provider need. *SessionCommandBinding satisfies it via
// its late-bound *Server target.
type SessionCommands interface {
	SessionCreateDeleter
	DaemonViewSource
}

// SessionCommandBinding is the late-bound bridge from the frontend command
// handler and snapshot provider to the daemon core. The *Server that satisfies
// SessionCommands is constructed AFTER WireAgentShim (it needs the
// frontend.Server WireAgentShim builds), so main injects this holder and calls
// SetTarget once the Server exists — the same late-bind shape as PushForwarder.
type SessionCommandBinding struct {
	Logf   func(string, ...any)
	target atomic.Pointer[Server]
}

var _ SessionCommands = (*SessionCommandBinding)(nil)

// SetTarget binds the *Server the holder delegates to. Called once by main,
// after New, before any frontend client can connect.
func (b *SessionCommandBinding) SetTarget(s *Server) { b.target.Store(s) }

func (b *SessionCommandBinding) logMiss(what string) {
	if b.Logf != nil {
		b.Logf("server: session-command binding %s before SetTarget — daemon core not yet wired", what)
	}
}

// CreateSession delegates to the bound Server, or fails loudly when the binding
// has no target yet (a construction-order bug, never a normal runtime state).
func (b *SessionCommandBinding) CreateSession(ctx context.Context, opts CreateOpts) (string, error) {
	s := b.target.Load()
	if s == nil {
		b.logMiss("CreateSession")
		return "", fmt.Errorf("server: session-create binding not wired")
	}
	return s.CreateSession(ctx, opts)
}

// DeleteSession delegates to the bound Server, failing loudly when unbound.
func (b *SessionCommandBinding) DeleteSession(sessionID string) error {
	s := b.target.Load()
	if s == nil {
		b.logMiss("DeleteSession")
		return fmt.Errorf("server: session-delete binding not wired")
	}
	return s.DeleteSession(sessionID)
}

// DaemonView delegates to the bound Server. An unbound binding logs the miss and
// returns nil (a snapshot with no daemon block); in production the binding is
// always set before any client connects.
func (b *SessionCommandBinding) DaemonView() *frontendv1.DaemonView {
	s := b.target.Load()
	if s == nil {
		b.logMiss("DaemonView")
		return nil
	}
	return s.DaemonView()
}

// commandHandler implements frontend.CommandHandler by routing each command to
// the owning module. Every dependency is required; a nil one is a construction
// error (surfaced by newCommandHandler) rather than a nil-deref at dispatch.
type commandHandler struct {
	prompts PromptRouter
	merges  MergeRunner
	// mergeGeometry answers a bare merge request's three coordinates. Nil makes
	// every name-only merge a loud failing ack: the daemon owns the map, and a
	// daemon that cannot read it must say so rather than cherry-pick blind.
	mergeGeometry MergeGeometrySource
	// mergeStates records the merge phases this handler owns (merge_enqueuing
	// on receipt, merge_failed on a refused enqueue). Nil makes a merge command
	// a loud failing ack: an unmarked merge attempt is invisible until the
	// coordinator gets to it, which is the hole merge_enqueuing closes.
	mergeStates merge.StateSink
	lifecycle   WorkspaceLifecycle
	// resyncer replays conversation deltas on a resync; nil-safe (Resync then
	// documents the snapshot-only behavior rather than swallowing).
	resyncer Resyncer
	// sessions backs the createSession/deleteSession commands. Required.
	sessions SessionCreateDeleter
	// resumes resolves which conversation a CONTINUE create lands on. Nil is a
	// loud failing ack for that mode; see CommandHandlerConfig.Resumes.
	resumes ConversationResumeResolver
	// shutdown begins the daemon's graceful teardown, told whether to stop the
	// session shims on the way out (false PRESERVES them, which is the
	// default; see server.ShutdownAll). Nil makes the shutdown command a loud
	// failing ack (the capability is unconfigured), never a silent no-op.
	shutdown func(stopShims bool, cause sessioncontroller.StopCause)
	// queues backs the queue force/accept/cancel commands (E4). Nil makes each
	// of them a loud failing ack rather than a silent no-op, same as shutdown.
	queues QueueController
	// schedules backs the schedule/cancel scheduled-shutdown commands. Nil is a
	// loud unsupported capability: a caller told its bounce was scheduled when
	// nothing took a lease would wait for a shutdown that is never coming.
	schedules ShutdownScheduleController
	// logTargets releases a closed workspace's log descriptors, binding their
	// lifetime to the workspace's. Nil holds them for the daemon's lifetime.
	logTargets WorkspaceLogTargetEvictor
	// workspaceViews retains the three resolved per-workspace views and the
	// memoized branch behind the topbar's title. The close releases them beside
	// the log targets, which is what binds every per-workspace retention to the
	// same owner's death. It is bound AFTER construction (WireAgentShim builds
	// the publisher on top of the frontend server this handler is handed to),
	// exactly as the snapshot provider's binding is. Nil holds the retentions
	// for the daemon's lifetime, which is what a focused harness wants and what
	// production must never be.
	workspaceViews *WorkspaceViews
	// restarts backs the restartSession command. Nil is a loud unsupported
	// capability, never a success-shaped no-op.
	restarts SessionRestarter
	// hibernations backs hibernateWorkspace and reviveSession. Nil is a loud
	// unsupported capability, never a success-shaped no-op.
	hibernations SessionHibernator
	// workspaceCreation owns durable create jobs and the file-inbox actions.
	// Nil is a loud unsupported capability, never a success-shaped no-op.
	workspaceCreation WorkspaceCreationBridge
	health            SessionHealthRouter
	daemonHealth      DaemonHealthChecker
	// turns and liveTasks are the interrupt confirm gate's two facts (I1),
	// each read from the authority that already owns it rather than
	// re-derived here. Both required for the interrupt command; an unwired one
	// makes it a loud failing ack instead of an unchallenged stop.
	turns     TurnStateSource
	liveTasks LiveTaskSource
	logf      func(string, ...any)
	// establishMu guards establishing, the in-flight create-plus-establish
	// round per workspace cwd. This is the NARROWEST point the coalescing can
	// sit at: the create core is shared with POST /sessions, and the frontend
	// command is the only caller whose contract is "acked means established".
	// See createestablish.go.
	establishMu  sync.Mutex
	establishing map[string]*sessionEstablishment
	// establishTimeout bounds one create-plus-establish round. Zero means
	// createEstablishTimeout.
	establishTimeout time.Duration
	clientLogs       ClientLogWriter
}

// ClientLogWriter owns persistence of a canonical browser source record. The
// command handler deliberately knows neither targets nor file paths.
type ClientLogWriter interface {
	PersistClientLog(workspace, requestID string, cmd *frontendv1.ClientLogCmd) error
}

type ClientLogSessionIdentity struct {
	AgentReplSessionID string
	ClaudeSessionID    string
}

// ClientLogIdentityResolver supplies daemon-authoritative session identity for
// a workspace when the browser record carries session attribution.
type ClientLogIdentityResolver interface {
	ResolveClientLogIdentity(workspace string) (ClientLogSessionIdentity, bool)
}

// TurnStateSource reports whether a workspace's session has a turn IN FLIGHT,
// as observed off the shim's own TurnStarted/TurnEnded stream. Satisfied by
// *sessioncontroller.Manager, which is where that observation already lives.
type TurnStateSource interface {
	TurnActive(workspace string) (bool, error)
}

// LiveTaskSource reports a workspace's live subagent-task count, and whether
// the workspace is known at all. Satisfied by *progress.Manager, which already
// adopts the count off the SSM's WorkspaceState and carries it to
// ProgressView — so the gate and the footer answer with the same number.
type LiveTaskSource interface {
	LiveTasks(workspace string) (int64, bool)
}

// CommandHandlerConfig collects the independently optional capabilities used
// by focused unit harnesses. Production WireAgentShim supplies every field.
type CommandHandlerConfig struct {
	// Schedules backs the scheduled-shutdown commands. Nil is a loud
	// unsupported capability, never a success-shaped no-op.
	Schedules         ShutdownScheduleController
	WorkspaceCreation WorkspaceCreationBridge
	// MergeGeometry answers a bare merge request's three coordinates. Nil is a
	// loud failing ack for a name-only merge, NEVER a guessed geometry.
	MergeGeometry MergeGeometrySource
	// MergeStates records the merge phases the COMMAND HANDLER itself owns:
	// merge_enqueuing on receipt, and merge_failed when the enqueue is refused.
	// Nil is a loud failing ack for a merge, never a merge run without its
	// first mark — a merge attempt nothing can see is precisely the state
	// merge_enqueuing exists to end.
	MergeStates merge.StateSink
	Health      HealthConfig
	Interrupt   InterruptGateConfig
	// Restarts backs the restartSession command. Nil is a loud unsupported
	// capability.
	Restarts SessionRestarter
	// Hibernations backs the hibernateWorkspace and reviveSession commands.
	// Nil is a loud unsupported capability, never a success-shaped no-op: a
	// caller told its workspace was hibernated when nothing stopped would
	// render a revival gate over a session that is still running.
	Hibernations SessionHibernator
	// EstablishTimeout bounds one createSession establishment round. Zero takes
	// createEstablishTimeout, which is the only value production uses; it is
	// injectable so a harness can prove the DEADLINE nack without waiting out a
	// bound sized for a loaded machine.
	EstablishTimeout time.Duration
	// Resumes answers which conversation a RESUME_MODE_CONTINUE create should
	// land on. Nil is a loud failing ack for that mode, NEVER a quiet fresh
	// start: silently starting fresh on top of an intact conversation is the
	// exact failure this resolver was introduced to end, and an unwired
	// resolver must not be able to reproduce it.
	Resumes ConversationResumeResolver
	// LogTargets releases a closed workspace's log descriptors. Nil leaves them
	// held for the daemon's lifetime, which is what a harness with no target
	// manager wants and what production must never be.
	LogTargets WorkspaceLogTargetEvictor
}

// WorkspaceLogTargetEvictor releases the log targets of a workspace that has
// been closed, reporting how many it released. Satisfied by
// *dlog.TargetManager.
//
// IT EXISTS TO BIND A RESOURCE TO A SCOPE. A workspace's log descriptors used
// to live from their first write until the daemon exited, so a long-lived
// daemon held one set per workspace it had ever touched, closed or not.
type WorkspaceLogTargetEvictor interface {
	EvictWorkspace(workspace dlog.Workspace) (int, error)
}

// ConversationResumeResolver resolves a workspace to the conversation a create
// should continue, and reports the conversation a live session landed on.
// Implemented by ConversationResolver; an interface so a harness can drive the
// create path without a registry on disk.
type ConversationResumeResolver interface {
	ResolveResume(configDir, cwd string) (string, bool)
	// ObservedClaudeSessionID reports the vendor uuid currently on sessionID's
	// record, for OBSERVABILITY only — it rides the create ack so a client can
	// attribute its logs before the first pushed SessionView. Empty when
	// unknown, which is a fine answer: an unattributed client log is accepted,
	// where a MISattributed one is rejected.
	ObservedClaudeSessionID(sessionID string) string
	// ConversationEvidence reports the daemon's durable answer to "has this
	// workspace ever had a Claude conversation?". It is consulted only when
	// ResolveResume names nothing, and it is what decides between a legitimate
	// fresh start and the resume ladder's terminal fault. See freshgate.go.
	ConversationEvidence(configDir, cwd string) conversationEvidence
}

// InterruptGateConfig supplies the interrupt confirm gate's two facts. Both
// are required for the interrupt command to run at all; a harness that omits
// them gets a loud failing ack rather than a stop that skipped the gate.
type InterruptGateConfig struct {
	Turns     TurnStateSource
	LiveTasks LiveTaskSource
}

type HealthConfig struct {
	Router SessionHealthRouter
	Daemon DaemonHealthChecker
}

var _ frontend.CommandHandler = (*commandHandler)(nil)

func (h *commandHandler) WorkspaceMaterialized(ctx context.Context, _ string, requestID string, cmd *frontendv1.WorkspaceMaterializedCmd) error {
	h.logf("frontend cmd: workspace_materialized request_id=%s job_id=%s", requestID, cmd.GetJobId())
	if h.workspaceCreation == nil {
		return fmt.Errorf("server: workspace_materialized is unavailable: workspace creation manager is not wired")
	}
	return h.workspaceCreation.MarkWorkspaceMaterialized(ctx, cmd.GetJobId())
}

func (h *commandHandler) HostActionCompleted(ctx context.Context, _ string, requestID string, cmd *frontendv1.HostActionCompletedCmd) error {
	h.logf("frontend cmd: host_action_completed request_id=%s action_id=%s ok=%t error=%s", requestID, cmd.GetActionId(), cmd.GetOk(), cmd.GetError())
	if h.workspaceCreation == nil {
		return fmt.Errorf("server: host_action_completed is unavailable: workspace creation manager is not wired")
	}
	return h.workspaceCreation.CompleteHostAction(ctx, cmd.GetActionId(), cmd.GetOk(), cmd.GetError())
}

// newCommandHandler validates its dependencies and returns the handler. The
// resyncer is optional (nil-safe) and shutdown is optional (an unconfigured
// shutdown fails the command loudly); the three routers and the
// session-lifecycle binding are required.
func newCommandHandler(prompts PromptRouter, merges MergeRunner, lifecycle WorkspaceLifecycle, resyncer Resyncer, sessions SessionCreateDeleter, shutdown func(stopShims bool, cause sessioncontroller.StopCause), queues QueueController, logf func(string, ...any), configs ...CommandHandlerConfig) (*commandHandler, error) {
	switch {
	case prompts == nil:
		return nil, fmt.Errorf("server: frontend command handler needs a PromptRouter")
	case merges == nil:
		return nil, fmt.Errorf("server: frontend command handler needs a MergeRunner")
	case lifecycle == nil:
		return nil, fmt.Errorf("server: frontend command handler needs a WorkspaceLifecycle")
	case sessions == nil:
		return nil, fmt.Errorf("server: frontend command handler needs a SessionCreateDeleter")
	}
	if logf == nil {
		logf = func(string, ...any) {}
	}
	if len(configs) > 1 {
		return nil, fmt.Errorf("server: frontend command handler received %d configs; expected at most one", len(configs))
	}
	var config CommandHandlerConfig
	if len(configs) == 1 {
		config = configs[0]
	}
	return &commandHandler{
		prompts: prompts, merges: merges, lifecycle: lifecycle, resyncer: resyncer,
		sessions: sessions, shutdown: shutdown, queues: queues,
		workspaceCreation: config.WorkspaceCreation,
		mergeGeometry:     config.MergeGeometry,
		mergeStates:       config.MergeStates,
		health:            config.Health.Router, daemonHealth: config.Health.Daemon,
		turns: config.Interrupt.Turns, liveTasks: config.Interrupt.LiveTasks, logf: logf,
		restarts:         config.Restarts,
		hibernations:     config.Hibernations,
		establishTimeout: config.EstablishTimeout,
		resumes:          config.Resumes,
		schedules:        config.Schedules,
		logTargets:       config.LogTargets,
	}, nil
}

// DaemonHealth returns the daemon-global readiness assertion.  A false answer
// is a completed check, not a command failure: callers wait for this correlated
// view and retry only when the daemon reports what remains unavailable.
func (h *commandHandler) DaemonHealth(_ context.Context, _ string, requestID string, _ *frontendv1.DaemonHealthCmd) (*frontendv1.DaemonHealthView, error) {
	if requestID == "" {
		return nil, fmt.Errorf("frontend cmd: daemon_health requires a request_id")
	}
	if h.daemonHealth == nil {
		return nil, fmt.Errorf("frontend cmd: daemon_health request_id=%s: no daemon health checker wired", requestID)
	}
	healthy, reason := h.daemonHealth.DaemonHealth()
	h.logf("frontend cmd: daemon_health request_id=%s healthy=%v reason=%q", requestID, healthy, reason)
	return &frontendv1.DaemonHealthView{RequestId: requestID, Healthy: healthy, Reason: reason}, nil
}

// SessionHealth returns a correlated assertion for precisely the session
// carried by the command.  Any missing controller, mismatched session, transport
// failure, or unhealthy shim is encoded as healthy=false so Emacs has one
// honest result type to wait on during restore.
func (h *commandHandler) SessionHealth(ctx context.Context, workspace, requestID string, cmd *frontendv1.SessionHealthCmd) (*frontendv1.SessionHealthView, error) {
	view := &frontendv1.SessionHealthView{RequestId: requestID, Workspace: workspace, SessionId: cmd.GetSessionId()}
	if requestID == "" {
		return nil, fmt.Errorf("frontend cmd: session_health requires a request_id")
	}
	if err := checkWorkspaceKey("session_health", workspace); err != nil {
		view.Reason = err.Error()
		h.logf("frontend cmd: session_health request_id=%s healthy=false reason=%q", requestID, view.Reason)
		return view, nil
	}
	if view.GetSessionId() == "" {
		view.Reason = "frontend cmd: session_health requires a session_id"
		h.logf("frontend cmd: session_health ws=%s request_id=%s healthy=false reason=%q", workspace, requestID, view.Reason)
		return view, nil
	}
	if h.health == nil {
		view.Reason = "frontend cmd: session health router is not wired"
		h.logf("frontend cmd: session_health ws=%s session=%s request_id=%s healthy=false reason=%q", workspace, view.GetSessionId(), requestID, view.Reason)
		return view, nil
	}
	actual, err := h.health.Health(ctx, workspace, view.GetSessionId(), requestID)
	if isUnwiredWorkspace(err) {
		view.Reason = err.Error()
		h.logf("frontend cmd: session_health ws=%s session=%s request_id=%s — the workspace has no live session controller; skipping the probe (healthy=false, nothing to act on)",
			workspace, view.GetSessionId(), requestID)
		return view, nil
	}
	if err != nil {
		view.Reason = err.Error()
		h.logf("frontend cmd: session_health ws=%s session=%s request_id=%s healthy=false reason=%q", workspace, view.GetSessionId(), requestID, view.Reason)
		return view, nil
	}
	if actual.GetRequestId() != requestID {
		view.Reason = fmt.Sprintf("frontend cmd: session health response request_id mismatch got=%q want=%q", actual.GetRequestId(), requestID)
		h.logf("frontend cmd: session_health ws=%s session=%s request_id=%s healthy=false reason=%q", workspace, view.GetSessionId(), requestID, view.Reason)
		return view, nil
	}
	view.Healthy = actual.GetHealthy()
	view.Reason = actual.GetReason()
	h.logf("frontend cmd: session_health ws=%s session=%s request_id=%s healthy=%v component=%s reason=%q", workspace, view.GetSessionId(), requestID, view.GetHealthy(), actual.GetComponent(), view.GetReason())
	return view, nil
}

// checkWorkspaceKey rejects a prompt-plane workspace key that is not an
// absolute path.
//
// Every session-routed command is keyed by the session's CWD: SessionLocator
// matches records on CWD == workspace and sessioncontroller maps live session controllers under
// that same string. A frontend that sends a DISPLAY NAME instead ("doom" rather
// than "/Users/…/.config/doom") therefore matches nothing — and without this
// check the miss surfaces as `workspace "doom" has no live session to drive`,
// indistinguishable from a genuinely dead session. That ambiguity is what let
// the 2026-07-25 name-keyed regression read as a session-startup failure
// instead of the wire-contract violation it was. Naming the real defect costs
// one check.
func checkWorkspaceKey(command, workspace string) error {
	if !filepath.IsAbs(workspace) {
		return fmt.Errorf("server: %s workspace key %q is not an absolute path — session-routed commands must be keyed by the session cwd, not a display name", command, workspace)
	}
	return nil
}

func (h *commandHandler) SubmitPrompt(ctx context.Context, workspace, requestID string, cmd *frontendv1.SubmitPromptCmd) error {
	h.logf("frontend cmd: submit_prompt ws=%s request_id=%s prompt_origin=%s", workspace, requestID, cmd.GetPromptOrigin())
	if err := checkWorkspaceKey("submit_prompt", workspace); err != nil {
		return err
	}
	if err := validatePromptOrigin(cmd.GetPromptOrigin()); err != nil {
		h.logf("frontend cmd: submit_prompt REFUSED ws=%s request_id=%s prompt_origin=%d error=%v", workspace, requestID, cmd.GetPromptOrigin(), err)
		return err
	}
	return h.prompts.SubmitPrompt(ctx, workspace, requestID, cmd.GetText(), cmd.GetPermissionMode(), cmd.GetPromptOrigin())
}

func validatePromptOrigin(origin corev1.PromptOrigin) error {
	if origin == corev1.PromptOrigin_PROMPT_ORIGIN_UNSPECIFIED {
		return fmt.Errorf("frontend submit_prompt requires a non-UNSPECIFIED prompt_origin")
	}
	if _, ok := corev1.PromptOrigin_name[int32(origin)]; !ok {
		return fmt.Errorf("frontend submit_prompt received unknown prompt_origin %d", origin)
	}
	return nil
}

// Interrupt stops the workspace's turn, subject to THE CONFIRM GATE.
//
// Interrupting a LIVE TURN never asks: the user can see the turn running and
// asked for it to stop. What deserves a second keystroke is the other case —
// no turn in flight, but subagent tasks still working — because there the
// visible thing the user meant to stop is already over and the thing they
// would actually stop is work they may not have in mind at all.
//
// The challenge is returned as a TYPED error rather than as a refusal: it is
// not a failure, and the ack it becomes carries neither `failure` nor `error`
// (see frontend.InterruptConfirmRequired). NOTHING is delivered to the shim on
// that path; the client asks the user and resends with confirm_agents.
//
// No turn and no live tasks DELIVERS anyway. The shim answers
// ALREADY_COMPLETE atomically and remains the sole authority on the verdict —
// the daemon's view of "no turn" is an observation, not a ruling, and refusing
// on it would let a stale observation swallow a stop.
//
// IT ALSO EVICTS THE WORKSPACE'S WAITING MERGES, and that is the second half of
// what a stop means here. A merge queued behind another one is work this
// workspace is doing that the user can see, cannot prompt past (the composer is
// gated on the merge lease), and had no way to call off: the stop went to a shim
// with no turn to end, and the merge ran anyway when its turn came. Nothing else
// on the wire reaches a queued merge, so the interrupt reaches it.
//
// EVICTION HAPPENS AFTER THE GATE AND BEFORE THE SHIM. After the gate because a
// challenged interrupt performs NOTHING — the whole point of the challenge is
// that the user has not decided yet — and before the shim because the queue is
// the part that can still be stopped cleanly, where the shim's verdict is its
// own to make.
//
// NEITHER HALF SWALLOWS THE OTHER'S FAILURE. A queue that could not be evicted
// must not cancel the stop the user asked for, and a shim that refused the stop
// must not hide an eviction that did happen, so both run and both errors travel
// back joined on the one ack.
func (h *commandHandler) Interrupt(ctx context.Context, workspace, requestID string, cmd *frontendv1.InterruptCmd) error {
	h.logf("frontend cmd: interrupt ws=%s request_id=%s confirm_agents=%v", workspace, requestID, cmd.GetConfirmAgents())
	if err := checkWorkspaceKey("interrupt", workspace); err != nil {
		return err
	}
	if !cmd.GetConfirmAgents() {
		challenge, err := h.interruptChallenge(workspace, requestID)
		if err != nil {
			return err
		}
		if challenge != nil {
			return challenge
		}
	}
	evictErr := h.evictQueuedMerges(ctx, workspace, requestID)
	return errors.Join(evictErr, h.prompts.Interrupt(ctx, workspace, requestID))
}

// evictQueuedMerges performs the interrupt's queue half and reports its failure.
//
// The count is logged rather than returned because it is not the command's
// answer: an interrupt acks the stop, not the number of merges that came off a
// queue. What the number IS for is the record — a merge that vanished from a
// user's queue has to be findable in the log — so both the eviction and the
// ordinary nothing-was-queued case are written down.
func (h *commandHandler) evictQueuedMerges(ctx context.Context, workspace, requestID string) error {
	evicted, err := h.merges.Evict(ctx, workspace)
	if err != nil {
		h.logf("frontend cmd: interrupt merge eviction FAILED ws=%s request_id=%s evicted=%d: %v — the merges it could not drop are still queued and still run when their turn comes",
			workspace, requestID, evicted, err)
		return fmt.Errorf("frontend cmd: interrupt ws=%s: evict queued merges: %w", workspace, err)
	}
	if evicted > 0 {
		h.logf("frontend cmd: interrupt EVICTED %d queued merge(s) ws=%s request_id=%s", evicted, workspace, requestID)
	}
	return nil
}

// interruptChallenge decides whether an unconfirmed interrupt must be
// challenged, returning the challenge or nil to deliver.
//
// Both facts come from the daemon's EXISTING authorities and neither is
// re-derived here: the turn boundary from the session controller that observes it off the
// shim's own stream, and the live-task count from the resolver that already
// carries it to ProgressView. An unwired source is a construction bug and
// fails loudly rather than silently skipping the gate — a skipped gate would
// stop working subagents with no question asked, which is precisely what it
// exists to prevent.
func (h *commandHandler) interruptChallenge(workspace, requestID string) (error, error) {
	if h.turns == nil || h.liveTasks == nil {
		return nil, fmt.Errorf("server: interrupt confirm gate is not wired (turn source=%t live-task source=%t); refusing to stop workspace %q without the check the contract requires",
			h.turns != nil, h.liveTasks != nil, workspace)
	}
	active, err := h.turns.TurnActive(workspace)
	if err != nil {
		return nil, err
	}
	if active {
		return nil, nil
	}
	tasks, known := h.liveTasks.LiveTasks(workspace)
	if !known {
		// Nothing has ever reported this workspace's task count, so there is no
		// live work on record to ask about. Loud, because the alternative
		// reading — silently treating an unknown as zero — is how a gate stops
		// engaging without anyone noticing.
		h.logf("frontend cmd: interrupt ws=%s request_id=%s has no progress view; no live subagent work on record, delivering unchallenged", workspace, requestID)
		return nil, nil
	}
	if tasks <= 0 {
		return nil, nil
	}
	return &frontend.InterruptConfirmRequired{LiveTasks: tasks}, nil
}

func (h *commandHandler) AnswerPermission(ctx context.Context, workspace, requestID string, cmd *frontendv1.PermissionAnswerCmd) error {
	h.logf("frontend cmd: answer_permission ws=%s request_id=%s permission_request_id=%s allow=%v",
		workspace, requestID, cmd.GetPermissionRequestId(), cmd.GetAllow())
	if err := checkWorkspaceKey("answer_permission", workspace); err != nil {
		return err
	}
	return h.prompts.AnswerPermission(ctx, workspace, cmd.GetPermissionRequestId(), cmd.GetAllow(), cmd.GetDenyMessage(), cmd.GetUpdatedInput())
}

// SetModel relays the user's explicit request to the live shim.  It returns
// only the shim-confirmed selection so the frontend ack cannot echo intent as
// state.
func (h *commandHandler) SetModel(ctx context.Context, workspace, requestID string, cmd *frontendv1.SetModelCmd) (string, error) {
	h.logf("frontend cmd: set_model ws=%s request_id=%s requested=%q", workspace, requestID, cmd.GetModel())
	if err := checkWorkspaceKey("set_model", workspace); err != nil {
		return "", err
	}
	requested := registry.NormalizeModel(cmd.GetModel())
	if requested == "" {
		return "", fmt.Errorf("frontend cmd: set_model ws=%s request_id=%s has an empty model", workspace, requestID)
	}
	selected, err := h.prompts.SetModel(ctx, workspace, requested)
	selected = registry.NormalizeModel(selected)
	if err != nil {
		if selected != "" {
			h.logf("frontend cmd: set_model REJECTED ws=%s request_id=%s requested=%q shim_selected=%q: %v", workspace, requestID, cmd.GetModel(), selected, err)
			return selected, err
		}
		return "", err
	}
	if selected == "" {
		return "", fmt.Errorf("frontend cmd: set_model ws=%s request_id=%s got an empty shim-confirmed model", workspace, requestID)
	}
	h.logf("frontend cmd: set_model CONFIRMED ws=%s request_id=%s requested=%q selected=%q", workspace, requestID, cmd.GetModel(), selected)
	return selected, nil
}

// resolveMergeGeometry fills in a merge request's three coordinates from THE
// daemon's own record, which is the only map there is.
//
// A caller used to be able to state all three on the command instead. That path
// is retired with MergeWorkspaceCmd's source_branch/source_dir/target_dir (see
// the reservation in frontend.proto): two owners of one map is how a
// cherry-pick reached a target the daemon had never heard of, and a caller that
// wants a merge run against a repository the daemon did not create records the
// geometry through geometry.Store rather than smuggling it past the map.
//
// An unrecorded workspace is refused with an explanation naming the workspace
// and what to do about it. It is NEVER guessed at.
func (h *commandHandler) resolveMergeGeometry(ctx context.Context, req merge.Request) (merge.Request, error) {
	if h.mergeGeometry == nil {
		h.logf("frontend cmd: merge_workspace REFUSED ws=%s name=%q reason=no-geometry-source", req.Workspace, req.Name)
		return req, fmt.Errorf("server: merge_workspace for %s cannot be resolved: the daemon's merge-geometry record is not wired", req.Workspace)
	}
	rec, found, err := h.mergeGeometry.Lookup(ctx, req.Workspace)
	if err != nil {
		h.logf("frontend cmd: merge_workspace REFUSED ws=%s name=%q reason=geometry-lookup-failed: %v", req.Workspace, req.Name, err)
		return req, fmt.Errorf("server: merge_workspace for %s: read recorded geometry: %w", req.Workspace, err)
	}
	if !found {
		h.logf("frontend cmd: merge_workspace REFUSED ws=%s name=%q reason=no-recorded-geometry", req.Workspace, req.Name)
		return req, fmt.Errorf("server: merge_workspace for %s has no recorded merge geometry: the daemon records a workspace's source branch, source worktree, and merge target when it CREATES the workspace, and derives them at boot for older ones. A workspace with neither is one whose branch or worktree git cannot answer for (a detached HEAD, or a worktree that no longer exists), and merging it would mean guessing which repository to write commits into", req.Workspace)
	}
	req.SourceBranch, req.SourceDir, req.TargetDir = rec.SourceBranch, rec.SourceDir, rec.TargetDir
	h.logf("frontend cmd: merge_workspace geometry RESOLVED ws=%s name=%q origin=%s source_branch=%q source_dir=%s target_dir=%s",
		req.Workspace, req.Name, rec.Origin, rec.SourceBranch, rec.SourceDir, rec.TargetDir)
	return req, nil
}

// markMergeEnqueuing records merge_enqueuing for a merge attempt the moment the
// command arrives.
//
// A FAILURE HERE REFUSES THE MERGE. The mark is the attempt's only trace until
// the coordinator records something of its own, so a merge run without it is a
// merge the user cannot see and the next boot cannot sweep. Proceeding past a
// failed mark would put the system back in exactly the state this phase exists
// to end, so the command nacks and nothing is enqueued.
func (h *commandHandler) markMergeEnqueuing(workspace, name, requestID string) error {
	if h.mergeStates == nil {
		h.logf("frontend cmd: merge_workspace REFUSED ws=%s name=%q request_id=%s reason=no-merge-state-sink", workspace, name, requestID)
		return fmt.Errorf("server: merge_workspace for %s cannot be recorded: the daemon's merge state sink is not wired", workspace)
	}
	cause := fmt.Sprintf("merge command received (request_id=%s)", requestID)
	if err := h.mergeStates.RecordMergeTransition(workspace, merge.PhaseMergeEnqueuing, cause); err != nil {
		h.logf("frontend cmd: merge_workspace merge_enqueuing record FAILED ws=%s name=%q request_id=%s: %v — the merge is REFUSED rather than run unmarked",
			workspace, name, requestID, err)
		return fmt.Errorf("server: merge_workspace for %s: record %s: %w", workspace, merge.PhaseMergeEnqueuing, err)
	}
	h.logf("frontend cmd: merge_workspace ENQUEUING ws=%s name=%q request_id=%s", workspace, name, requestID)
	return nil
}

// failMergeAttempt records merge_failed for an attempt that never reached the
// queue, and returns the nack the caller propagates.
//
// BOTH HALVES ARE REQUIRED. The nack is the frontend's answer to the command it
// sent; the transition is what stops the workspace from sitting on
// merge_enqueuing forever for every other surface that reads pushed state. A
// failure to RECORD the transition is joined onto the returned error rather
// than replacing it: the caller's original refusal is still the reason the
// merge did not happen.
func (h *commandHandler) failMergeAttempt(workspace, name, requestID, cause string, nack error) error {
	if err := h.mergeStates.RecordMergeTransition(workspace, merge.PhaseMergeFailed, cause); err != nil {
		h.logf("frontend cmd: merge_workspace merge_failed record FAILED ws=%s name=%q request_id=%s cause=%q: %v — the workspace stays at merge_enqueuing with nothing to advance it",
			workspace, name, requestID, cause, err)
		return errors.Join(nack, fmt.Errorf("server: merge_workspace for %s: record %s: %w", workspace, merge.PhaseMergeFailed, err))
	}
	h.logf("frontend cmd: merge_workspace FAILED ws=%s name=%q request_id=%s cause=%q", workspace, name, requestID, cause)
	return nack
}

// MergeWorkspace runs a merge, or resumes one on the conflict_resolved_continue
// handoff (§9.3).
//
// THE DAEMON OWNS THE GEOMETRY MAP. Every merge request is keyed by workspace
// alone, and the three coordinates come from the record the daemon wrote when
// it created that workspace (or derived at boot for a pre-cutover one). Emacs
// used to compute all three and ride them on the command; two owners of one map
// is how a merge landed against a target the daemon had never heard of, so
// those fields are retired from the wire entirely.
//
// A request still missing a coordinate is refused by merge.Request's own
// validation, which is the single place that decides what a runnable merge is.
//
// The non-resume path ENQUEUES rather than merges: merge.Coordinator owns the
// repository's queue, and this call returns once the request is durably on it.
// A merge that had to wait reports its outcome through pushed merge state, so
// an ok ack here means "accepted and recorded", never "already merged".
func (h *commandHandler) MergeWorkspace(ctx context.Context, workspace, requestID string, cmd *frontendv1.MergeWorkspaceCmd) error {
	// The envelope's workspace is the daemon's KEY (the session cwd), exactly
	// as it is for every other command, and the display name rides its own
	// field. Keying merge state on the name instead is what filed a merge's
	// rows under a workspace nothing else knew about.
	req := merge.Request{
		Workspace: workspace,
		Name:      cmd.GetWorkspaceName(),
	}
	// THE RESUME PATH BRANCHES FIRST, before the enqueuing mark below. A
	// conflict_resolved_continue is the continuation of a merge that is already
	// on the queue and already holds its lease; marking it "enqueuing" would
	// walk a live merge_conflict backwards to the weakest phase on the axis.
	if cmd.GetConflictResolvedContinue() {
		req, err := h.resolveMergeGeometry(ctx, req)
		if err != nil {
			return err
		}
		h.logf("frontend cmd: merge_workspace RESUME ws=%s name=%q request_id=%s source_branch=%q source_dir=%q target_dir=%q",
			workspace, req.Name, requestID, req.SourceBranch, req.SourceDir, req.TargetDir)
		return h.merges.Resume(ctx, req)
	}

	// THE FIRST THING A MERGE ATTEMPT DOES IS BECOME VISIBLE, and it happens
	// HERE — before the geometry is resolved and before the coordinator is
	// asked for anything. Everything below this line can fail, and every one of
	// those failures used to leave the user with a command that vanished: no
	// phase was pushed until merge_queued or merging, so a merge refused for an
	// unresolvable geometry, or a daemon that died before the durable write,
	// left zero trace in any UI.
	if err := h.markMergeEnqueuing(workspace, req.Name, requestID); err != nil {
		return err
	}
	req, err := h.resolveMergeGeometry(ctx, req)
	if err != nil {
		return h.failMergeAttempt(workspace, req.Name, requestID, "merge geometry unresolvable: "+err.Error(), err)
	}
	h.logf("frontend cmd: merge_workspace ws=%s name=%q request_id=%s source_branch=%q source_dir=%q target_dir=%q",
		workspace, req.Name, requestID, req.SourceBranch, req.SourceDir, req.TargetDir)
	pos, enqueueErr := h.merges.Enqueue(ctx, req)
	if enqueueErr != nil {
		h.logf("frontend cmd: merge_workspace ENQUEUE FAILED ws=%s name=%q request_id=%s: %v", workspace, req.Name, requestID, enqueueErr)
		return h.failMergeAttempt(workspace, req.Name, requestID, "merge enqueue refused: "+enqueueErr.Error(), enqueueErr)
	}
	h.logf("frontend cmd: merge_workspace ENQUEUED ws=%s name=%q request_id=%s repo=%q index=%d depth=%d",
		workspace, req.Name, requestID, pos.Repo, pos.Index, pos.Depth)
	return nil
}

func (h *commandHandler) CloseWorkspace(ctx context.Context, workspace, requestID string, _ *frontendv1.CloseWorkspaceCmd) error {
	h.logf("frontend cmd: close_workspace ws=%s request_id=%s", workspace, requestID)
	// A close is also the abandonment of any merge the workspace has parked on
	// a conflict: the lease must not outlive the workspace it was taken over,
	// and there is no separate abandon command on the wire. An abandon failure
	// refuses the close loudly — closing anyway would leave the lease standing
	// over a workspace nobody can prompt or resolve.
	abandoned, err := h.merges.Abandon(ctx, workspace)
	if err != nil {
		h.logf("frontend cmd: close_workspace ABANDON FAILED ws=%s request_id=%s: %v", workspace, requestID, err)
		return err
	}
	if abandoned {
		h.logf("frontend cmd: close_workspace ABANDONED the workspace's parked merge ws=%s request_id=%s", workspace, requestID)
	}
	if err := h.lifecycle.Close(ctx, workspace); err != nil {
		return err
	}
	// THE LOG TARGETS GO WITH THE WORKSPACE. They are released only after the
	// close itself succeeded — a refused close leaves a workspace that is still
	// live, and taking its descriptors away would break the writers still using
	// them. A release failure is reported, never swallowed: descriptors that
	// could not be closed are exactly the leak this eviction exists to end.
	h.evictWorkspaceLogTargets(workspace, requestID)
	// THE RESOLVED VIEWS GO WITH THE WORKSPACE TOO, and for a second reason
	// beyond the snapshot carrying a topbar nothing runs: the publisher
	// memoizes this workspace's branch, and a workspace re-created at the same
	// path would inherit its dead predecessor's under a genuinely current fence.
	h.forgetWorkspaceViews(workspace, requestID)
	return nil
}

// forgetWorkspaceViews releases a closed workspace's retained resolved views.
//
// Like the log-target eviction it runs only after the close itself succeeded —
// a refused close leaves a workspace that is still live, and dropping its
// topbar would blank a surface still being driven — and an unwired publisher
// says so rather than passing for a release that happened.
func (h *commandHandler) forgetWorkspaceViews(workspace, requestID string) {
	if h.workspaceViews == nil {
		h.logf("frontend cmd: close_workspace resolved views RETAINED ws=%s request_id=%s — no view publisher is wired, so this workspace's topbar, breakdown, gate and memoized branch stay held for the daemon's lifetime",
			workspace, requestID)
		return
	}
	h.workspaceViews.Forget(workspace)
	h.logf("frontend cmd: close_workspace resolved views RELEASED ws=%s request_id=%s", workspace, requestID)
}

// evictWorkspaceLogTargets releases a closed workspace's log descriptors.
//
// It is loud in every direction: an unwired evictor says so (the daemon always
// wires one, so an absent one is a wiring defect a harness may accept and
// production may not), an unresolvable workspace says so, and a close failure
// says so. None of them fails the close, which has already happened.
func (h *commandHandler) evictWorkspaceLogTargets(workspace, requestID string) {
	if h.logTargets == nil {
		h.logf("frontend cmd: close_workspace log targets RETAINED ws=%s request_id=%s — no log target evictor is wired, so this workspace's descriptors stay held for the daemon's lifetime",
			workspace, requestID)
		return
	}
	resolved, err := dlog.WorkspaceFromDirectory(workspace)
	if err != nil {
		h.logf("frontend cmd: close_workspace log target eviction SKIPPED ws=%s request_id=%s: %v — the workspace identity the targets are keyed by could not be resolved",
			workspace, requestID, err)
		return
	}
	evicted, err := h.logTargets.EvictWorkspace(resolved)
	if err != nil {
		h.logf("frontend cmd: close_workspace log target eviction FAILED ws=%s request_id=%s evicted=%d: %v — descriptors this workspace held may still be open",
			workspace, requestID, evicted, err)
		return
	}
	h.logf("frontend cmd: close_workspace log targets RELEASED ws=%s request_id=%s evicted=%d", workspace, requestID, evicted)
}

func (h *commandHandler) OpenWorkspace(ctx context.Context, workspace, requestID string, cmd *frontendv1.OpenWorkspaceCmd) error {
	h.logf("frontend cmd: open_workspace ws=%s request_id=%s permission_mode=%s config_dir=%s fake=%v%s",
		workspace, requestID, cmd.GetPermissionMode(), cmd.GetConfigDir(), cmd.GetFake(),
		protocol.UngatedNote("a session this open starts", cmd.GetPermissionMode(), cmd.GetAllowUngated()))
	return h.lifecycle.Open(ctx, workspace, WorkspaceOpenOpts{
		PermissionMode: cmd.GetPermissionMode(),
		ConfigDir:      cmd.GetConfigDir(),
		Fake:           cmd.GetFake(),
		AllowUngated:   cmd.GetAllowUngated(),
	})
}

// Resync drives the conversation-delta replay half of a frontend resync (the
// frontend server independently re-sends the StateSnapshot). It routes to the
// per-session controller's retained-ring replay from the requested seq, inclusive
// and floored at the newest clear or compaction.
//
// WHENEVER A FRONTEND CONNECTS, IT GETS THE CONVERSATION. There is no longer an
// unwired skip here. A workspace with no live session controller is served from
// its DURABLE store history instead (sessioncontroller/durablereplay.go), so a
// webview mounting after a daemon bounce receives the same feed a live
// workspace's does. The skip that used to sit here is what left that webview
// showing a correct footer over an empty conversation, and quieting the failure
// is precisely what made the gap invisible: EVERY failure of the replay now
// nacks, because a frontend cannot tell silence from an empty conversation.
//
// A nil resyncer is a construction error, not a degraded mode: the command
// exists, so something must answer it.
func (h *commandHandler) Resync(_ context.Context, workspace, requestID string, cmd *frontendv1.ResyncCmd) error {
	// The client echoes ONE token. It held two identities in agreement before,
	// which is exactly what the fence removed; the daemon still needs both,
	// because its eligibility ladder distinguishes a stale session under a
	// current generation from a stale generation, and it reads them back
	// through the inverse of the function that minted them.
	sessionID, generationID := ssm.SplitFence(cmd.GetFence())
	h.logf("frontend cmd: resync ws=%s request_id=%s session=%s generation=%s from_seq=%d", workspace, requestID, sessionID, generationID, cmd.GetFromSeq())
	if h.resyncer == nil {
		h.logf("frontend cmd: resync ws=%s request_id=%s session=%s generation=%s from_seq=%d FAILED — no resyncer is wired, so the conversation replay cannot be served at all (the snapshot half alone would render an empty feed)",
			workspace, requestID, sessionID, generationID, cmd.GetFromSeq())
		return fmt.Errorf("frontend cmd: resync ws=%s request_id=%s: no resyncer wired for the conversation replay", workspace, requestID)
	}
	if err := h.resyncer.ResyncForGeneration(workspace, sessionID, generationID, cmd.GetFromSeq()); err != nil {
		h.logf("frontend cmd: resync ws=%s request_id=%s session=%s generation=%s from_seq=%d FAILED: %v", workspace, requestID, sessionID, generationID, cmd.GetFromSeq(), err)
		return classifyStaleFenceResync(err)
	}
	return nil
}

// staleFenceResync carries the REMEDY for a resync the eligibility ladder
// refused because the fence the client echoed is no longer the workspace's.
//
// THE CLASSIFICATION IS NOT CHANGED HERE, deliberately. `ErrSessionSuperseded`
// already classifies as `reconnect_superseded`, and that arm is exactly this
// case: the contract defines it as a view whose replay would have come from a
// generation it never saw. It is the more specific true statement than
// `workspace_not_live`.
//
// THE RESYNC IS THE ONLY FENCE-DRIVEN COMMAND REFUSAL THIS DAEMON HAS. The
// resync is the one command that echoes a fence at all, so it is the one place
// a stale fence can refuse anything. `workspace_not_live` is minted from
// `ErrNotLiveSession`, whose single live producer refuses a hibernate whose
// named SESSION is not the one controlling the workspace — a session-identity
// mismatch, not a fence comparison. If a driving command (a submit, an
// interrupt) ever grows a fence, its stale-fence refusal is a different
// statement from this one and gets its own classification then; nothing here
// anticipates it.
//
// What this adds is the one thing the ladder cannot know: `remedy`, the action
// to offer. A resync is a VIEW's only recovery mechanism, so a refused one
// leaves the client permanently behind unless it is told to remount — which is
// the sentence below. It reaches the arm through the same door
// SessionResumeFailureDetailer uses: the funnel classifies, and the refusing
// site supplies the evidence only it holds.
type staleFenceResync struct{ err error }

func (e *staleFenceResync) Error() string { return e.err.Error() }
func (e *staleFenceResync) Unwrap() error { return e.err }

// FailureRemedy is the action offered to a client whose resync was refused.
// The daemon composes it because the ladder's verdict is what decides there is
// an action at all; the wording is host-neutral because the daemon does not
// know which frontend is asking.
func (e *staleFenceResync) FailureRemedy() string {
	return "reload this view: its replay would have come from a session generation it never saw"
}

// classifyStaleFenceResync attaches that remedy, leaving every other error —
// and the superseded sentinel's own classification — exactly as it was.
func classifyStaleFenceResync(err error) error {
	if err == nil || !errors.Is(err, errclass.ErrSessionSuperseded) {
		return err
	}
	return &staleFenceResync{err: err}
}

// isUnwiredWorkspace reports whether err is the "this workspace has no live
// shim controller" refusal.
//
// It reads UNWIRED rather than the word it used to carry. `dormant` is no longer
// a state: the render vocabulary split it into `severed` (the substrate broke)
// and `hibernated` (nothing is wrong), and this predicate deliberately covers
// BOTH plus every other reason a session controller might be absent. It is a fact about the
// controller, not a claim about which of the two closed halves the workspace is on.
//
// IT IS CLASSIFIED BY CALLER, not by shape. The refusal is the same fact
// either way, but what it MEANS to the user is not: Emacs fans background
// machinery — resyncs, kept health probes, sweep passes — across every
// workspace it knows about, and after a daemon bounce most of them are
// legitimately unwired. Reporting each of those as a failure put dozens of
// error-shaped lines and nacks in front of a user with nothing to act on.
//
// So BACKGROUND callers treat it as the calm, expected answer it is, and
// DIRECT user-initiated commands (interrupt, the queue controls) keep the loud
// nack — there the refusal IS the user's feedback, and quieting it would leave
// a pressed control doing nothing with no explanation. Nothing here weakens the
// loud path for a genuine caller error: any other failure still propagates.
func isUnwiredWorkspace(err error) bool {
	return err != nil && errors.Is(err, errclass.ErrNoLiveSessionController)
}

// CreateSession lives in createestablish.go: the command is the daemon's
// establishment gate, and its ack condition is the whole reason that file
// exists.

// DeleteSession marks the command's session terminal and stops its shim.
func (h *commandHandler) DeleteSession(_ context.Context, workspace, requestID string, cmd *frontendv1.DeleteSessionCmd) error {
	h.logf("frontend cmd: delete_session ws=%s request_id=%s session=%s", workspace, requestID, cmd.GetSessionId())
	return h.sessions.DeleteSession(cmd.GetSessionId())
}

// RestartSession HARD-RESTARTS the workspace's session (sessionsession controller's
// RestartSession): stop whatever shim is serving it — including a survivor of a
// previous daemon, reached by the pid it announced — then bring the SAME
// session record back up, so the conversation resumes under a fresh process.
//
// Synchronous, unlike Shutdown: the ack is the user's only report of whether
// their session came back, so a restart that failed must nack rather than
// return ok and leave them looking at a dead workspace.
func (h *commandHandler) RestartSession(ctx context.Context, workspace, requestID string, _ *frontendv1.RestartSessionCmd) error {
	h.logf("frontend cmd: restart-session ws=%s request_id=%s", workspace, requestID)
	if h.restarts == nil {
		return fmt.Errorf("server: session restart not supported by this daemon")
	}
	if err := h.restarts.RestartSession(ctx, workspace); err != nil {
		// CLASSIFIED, like create and open. A restart that cannot find its
		// conversation's transcript is a continuity failure, and returning it
		// raw made it `internal.unclassified`: the daemon knew the exact cause
		// and the user got an unexplained refusal to open their workspace.
		return restartResumeEstablishment().classify(
			fmt.Errorf("server: restarting the session for workspace %q: %w", workspace, err))
	}
	h.logf("frontend cmd: restart-session ws=%s request_id=%s COMPLETE", workspace, requestID)
	return nil
}

// HibernateWorkspace stops the workspace's shim and marks its session
// hibernated, so the next prompt meets the revival gate rather than silently
// paying a bring-up.
//
// Synchronous, and for RestartSession's reason: the ack is the user's only
// report. A hibernate that was refused — because a turn is live, or the merge
// lease is held — must NACK rather than return ok and leave the user believing
// they reclaimed 500MB that is still in use.
func (h *commandHandler) HibernateWorkspace(_ context.Context, workspace, requestID string, _ *frontendv1.HibernateWorkspaceCmd) error {
	h.logf("frontend cmd: hibernate-workspace ws=%s request_id=%s", workspace, requestID)
	if h.hibernations == nil {
		return fmt.Errorf("server: workspace hibernation not supported by this daemon")
	}
	if err := h.hibernations.HibernateWorkspace(workspace); err != nil {
		return fmt.Errorf("server: hibernating workspace %q: %w", workspace, err)
	}
	h.logf("frontend cmd: hibernate-workspace ws=%s request_id=%s COMPLETE", workspace, requestID)
	return nil
}

// ReviveSession brings a hibernated session back under the user's chosen mode.
//
// THE ACK IT PRODUCES IS AN ACCEPTANCE, not a completion. Under compact_first
// the session is up and `/compact` is submitted when this returns, and the
// compaction runs on inside the daemon with the session still gated until it
// lands (sessioncontroller/revive.go).
//
// THE MODE IS READ FROM THE ONEOF AND AN UNSET ONE IS A NACK. The wire makes
// "no decision" unrepresentable precisely so the daemon never has to invent a
// default, and inventing one here would spend the user's context budget on a
// choice they were being asked to make.
func (h *commandHandler) ReviveSession(ctx context.Context, workspace, requestID string, cmd *frontendv1.ReviveSessionCmd) error {
	var mode sessioncontroller.ReviveMode
	switch cmd.GetMode().(type) {
	case *frontendv1.ReviveSessionCmd_CompactFirst:
		mode = sessioncontroller.ReviveModeCompactFirst
	case *frontendv1.ReviveSessionCmd_Direct:
		mode = sessioncontroller.ReviveModeDirect
	}
	h.logf("frontend cmd: revive-session ws=%s request_id=%s mode=%s", workspace, requestID, mode)
	if h.hibernations == nil {
		return fmt.Errorf("server: session revival not supported by this daemon")
	}
	if mode == sessioncontroller.ReviveModeUnset {
		return fmt.Errorf("server: reviving workspace %q: the command carries no revival mode; the choice between compacting first and resuming as-is is the user's and the daemon has no default for it", workspace)
	}
	if err := h.hibernations.ReviveSession(ctx, workspace, mode); err != nil {
		return fmt.Errorf("server: reviving the session for workspace %q: %w", workspace, err)
	}
	h.logf("frontend cmd: revive-session ws=%s request_id=%s mode=%s ACCEPTED", workspace, requestID, mode)
	return nil
}

// Shutdown begins the daemon's graceful teardown — the same func POST /shutdown
// drives — asynchronously, so the ok CommandAck is delivered before the process
// exits. An unconfigured shutdown is a loud failing ack (the capability is
// absent), never a silent no-op.
func (h *commandHandler) Shutdown(_ context.Context, workspace, requestID string, cmd *frontendv1.ShutdownCmd) error {
	stopShims := cmd.GetStopShims()
	h.logf("frontend cmd: shutdown ws=%s request_id=%s stop_shims=%v", workspace, requestID, stopShims)
	if h.shutdown == nil {
		return fmt.Errorf("server: shutdown not supported by this daemon")
	}
	go h.shutdown(stopShims, sessioncontroller.StopCauseDaemonShutdown())
	return nil
}

// ClientLog persists frontend evidence only in the workspace's webapp.log. It
// never mutates daemon state or duplicates the record into daemon.log.
// QueueController is the queue half of the frontend command surface (E4).
// Satisfied by *sessioncontroller.Manager. Every method reports an unknown entry id
// as an error: the user asked for something specific, and pretending to have
// done it would be worse than saying it is gone.
type QueueController interface {
	ForceQueueEntry(workspace, entryID string) error
	AcceptQueueEntry(workspace, entryID string) error
	CancelQueueEntry(workspace, entryID string) error
}

// ForceQueueEntry runs the interject sequence for a held prompt, user-initiated.
func (h *commandHandler) ForceQueueEntry(_ context.Context, workspace, requestID string, cmd *frontendv1.QueueForceCmd) error {
	h.logf("frontend cmd: queue_force ws=%s request_id=%s entry=%s", workspace, requestID, cmd.GetEntryId())
	if h.queues == nil {
		return fmt.Errorf("server: prompt queue not supported by this daemon")
	}
	return h.queues.ForceQueueEntry(workspace, cmd.GetEntryId())
}

// AcceptQueueEntry confirms a held prompt's classification (view state only).
func (h *commandHandler) AcceptQueueEntry(_ context.Context, workspace, requestID string, cmd *frontendv1.QueueAcceptCmd) error {
	h.logf("frontend cmd: queue_accept ws=%s request_id=%s entry=%s", workspace, requestID, cmd.GetEntryId())
	if h.queues == nil {
		return fmt.Errorf("server: prompt queue not supported by this daemon")
	}
	return h.queues.AcceptQueueEntry(workspace, cmd.GetEntryId())
}

// CancelQueueEntry drops a held prompt; it is never delivered.
func (h *commandHandler) CancelQueueEntry(_ context.Context, workspace, requestID string, cmd *frontendv1.QueueCancelCmd) error {
	h.logf("frontend cmd: queue_cancel ws=%s request_id=%s entry=%s", workspace, requestID, cmd.GetEntryId())
	if h.queues == nil {
		return fmt.Errorf("server: prompt queue not supported by this daemon")
	}
	return h.queues.CancelQueueEntry(workspace, cmd.GetEntryId())
}

// ScheduleShutdown takes the daemon-global drain lease instead of bouncing now.
//
// Synchronous, unlike Shutdown: the ack carries whether the lease was actually
// TAKEN, and a caller that was told ok while a rival schedule already stood
// would sit waiting for a bounce belonging to someone else's intent.
func (h *commandHandler) ScheduleShutdown(_ context.Context, workspace, requestID string, cmd *frontendv1.ScheduleShutdownCmd) error {
	h.logf("frontend cmd: schedule_shutdown ws=%s request_id=%s stop_shims=%v cause=%q",
		workspace, requestID, cmd.GetStopShims(), cmd.GetCause())
	if h.schedules == nil {
		return fmt.Errorf("server: scheduled shutdown is not supported by this daemon")
	}
	scheduleID, err := h.schedules.Schedule(cmd.GetStopShims(), cmd.GetCause())
	if err != nil {
		return err
	}
	h.logf("frontend cmd: schedule_shutdown ws=%s request_id=%s schedule_id=%s TAKEN", workspace, requestID, scheduleID)
	return nil
}

// CancelScheduledShutdown releases the drain lease. A stale schedule id is a
// loud nack from the engine and is surfaced verbatim.
func (h *commandHandler) CancelScheduledShutdown(_ context.Context, workspace, requestID string, cmd *frontendv1.CancelScheduledShutdownCmd) error {
	h.logf("frontend cmd: cancel_scheduled_shutdown ws=%s request_id=%s schedule_id=%s",
		workspace, requestID, cmd.GetScheduleId())
	if h.schedules == nil {
		return fmt.Errorf("server: scheduled shutdown is not supported by this daemon")
	}
	return h.schedules.Cancel(cmd.GetScheduleId())
}

func (h *commandHandler) ClientLog(_ context.Context, workspace, requestID string, cmd *frontendv1.ClientLogCmd) error {
	if h.clientLogs == nil {
		return fmt.Errorf("server: client-log persistence is not wired")
	}
	if err := h.clientLogs.PersistClientLog(workspace, requestID, cmd); err != nil {
		return fmt.Errorf("server: persist client log: %w", err)
	}
	return nil
}

// targetClientLogWriter is the daemon-owned browser persistence boundary. It
// validates the complete browser record before daemon attribution overwrites
// workspace and request facts the browser cannot authoritatively know.
type targetClientLogWriter struct {
	targets  *dlog.TargetManager
	identity ClientLogIdentityResolver
	terminal io.Writer
	verbose  bool
}

// NewTargetClientLogWriter constructs the explicit runtime target dependency.
func NewTargetClientLogWriter(targets *dlog.TargetManager, identity ClientLogIdentityResolver, terminal io.Writer, verbose bool) (ClientLogWriter, error) {
	if targets == nil {
		return nil, fmt.Errorf("server: client log writer needs target manager")
	}
	if identity == nil {
		return nil, fmt.Errorf("server: client log writer needs session identity resolver")
	}
	if terminal == nil {
		return nil, fmt.Errorf("server: client log writer needs terminal")
	}
	return &targetClientLogWriter{targets: targets, identity: identity, terminal: terminal, verbose: verbose}, nil
}

func (w *targetClientLogWriter) PersistClientLog(workspace, requestID string, cmd *frontendv1.ClientLogCmd) error {
	if cmd == nil {
		return errors.New("client log command is required")
	}
	if err := checkWorkspaceKey("client_log", workspace); err != nil {
		return err
	}
	if requestID == "" {
		return errors.New("client log request ID is required")
	}
	if cmd.GetContext() == nil {
		return errors.New("client log canonical record context is required")
	}
	raw, err := json.Marshal(cmd.GetContext().AsMap())
	if err != nil {
		return fmt.Errorf("encode client log canonical record: %w", err)
	}
	record, err := dlog.ParseForwardedRecord(raw)
	if err != nil {
		return err
	}
	if record.Runtime != dlog.RuntimeWebapp {
		return fmt.Errorf("client log runtime must be webapp, got %q", record.Runtime)
	}
	if record.ConnectionID == "" {
		return errors.New("client log webapp connection ID is required")
	}
	if record.Message != cmd.GetMessage() {
		return errors.New("client log command message does not match canonical record")
	}
	if record.Level != clientLogLevel(cmd.GetLevel()) {
		return fmt.Errorf("client log command level does not match canonical record")
	}
	ws, err := dlog.WorkspaceFromDirectory(workspace)
	if err != nil {
		return fmt.Errorf("resolve client log workspace: %w", err)
	}
	if (record.WorkspaceDirectory == "") != (record.WorkspaceID == "") {
		return errors.New("client log source workspace attribution is incomplete")
	}
	if record.WorkspaceDirectory != "" && (record.WorkspaceDirectory != ws.Directory || record.WorkspaceID != ws.ID) {
		return fmt.Errorf("client log source workspace attribution disagrees with command workspace")
	}
	identity, identityKnown := w.identity.ResolveClientLogIdentity(workspace)
	if record.AgentReplSessionID != "" || record.ClaudeSessionID != "" {
		if !identityKnown {
			return errors.New("client log source session attribution cannot be verified for command workspace")
		}
		if record.AgentReplSessionID != "" && record.AgentReplSessionID != identity.AgentReplSessionID {
			return fmt.Errorf("%w: agent-repl session ID got=%q want=%q workspace=%q request_id=%q",
				errclass.ErrClientLogIdentityStale, record.AgentReplSessionID, identity.AgentReplSessionID, workspace, requestID)
		}
		if record.ClaudeSessionID != "" && record.ClaudeSessionID != identity.ClaudeSessionID {
			return fmt.Errorf("%w: Claude session ID got=%q want=%q agent_repl_session_id=%q workspace=%q request_id=%q",
				errclass.ErrClientLogIdentityStale, record.ClaudeSessionID, identity.ClaudeSessionID, identity.AgentReplSessionID, workspace, requestID)
		}
	}
	logger, err := w.targets.OpenWorkspaceRuntimeLogger(ws, dlog.RuntimeWebapp, w.terminal, w.verbose)
	if err != nil {
		return fmt.Errorf("open webapp target: %w", err)
	}
	forwarded := dlog.ForwardedIdentity{RequestID: requestID}
	if identityKnown {
		forwarded.AgentReplSessionID = identity.AgentReplSessionID
		forwarded.ClaudeSessionID = identity.ClaudeSessionID
	}
	return logger.PersistForwarded(ws, dlog.RuntimeWebapp, record, forwarded)
}

func clientLogLevel(level frontendv1.ClientLogLevel) dlog.Level {
	switch level {
	case frontendv1.ClientLogLevel_CLIENT_LOG_LEVEL_INFO:
		return dlog.LevelInfo
	case frontendv1.ClientLogLevel_CLIENT_LOG_LEVEL_WARN:
		return dlog.LevelWarn
	case frontendv1.ClientLogLevel_CLIENT_LOG_LEVEL_ERROR:
		return dlog.LevelError
	default:
		return ""
	}
}

// FileDiagnosticPersister is injected into the session controller without making
// it depend on daemon target management.
type FileDiagnosticPersister interface {
	PersistFileDiagnostic(workspace, agentReplSessionID string, ev *corev1.Event, diagnostic *corev1.FilePlaneDiagnostic) error
}

type targetFileDiagnosticPersister struct {
	targets  *dlog.TargetManager
	terminal io.Writer
	verbose  bool
}

// NewTargetFileDiagnosticPersister builds the daemon-owned sidecar routing
// boundary. Its targets are distinct from daemon, shim, and webapp targets.
func NewTargetFileDiagnosticPersister(targets *dlog.TargetManager, terminal io.Writer, verbose bool) (FileDiagnosticPersister, error) {
	if targets == nil {
		return nil, errors.New("server: file diagnostic persister needs target manager")
	}
	if terminal == nil {
		return nil, errors.New("server: file diagnostic persister needs terminal")
	}
	return &targetFileDiagnosticPersister{targets: targets, terminal: terminal, verbose: verbose}, nil
}

func (p *targetFileDiagnosticPersister) PersistFileDiagnostic(workspace, agentReplSessionID string, ev *corev1.Event, diagnostic *corev1.FilePlaneDiagnostic) error {
	if ev == nil || diagnostic == nil {
		return errors.New("sidecar diagnostic event and payload are required")
	}
	if ev.GetSessionId() == "" {
		return errors.New("sidecar diagnostic Claude session ID is required")
	}
	ws, err := dlog.WorkspaceFromDirectory(workspace)
	if err != nil {
		return fmt.Errorf("resolve sidecar diagnostic workspace: %w", err)
	}
	logger, err := p.targets.OpenWorkspaceRuntimeLogger(ws, dlog.RuntimeSidecar, p.terminal, p.verbose)
	if err != nil {
		return fmt.Errorf("open sidecar target: %w", err)
	}
	context := make(map[string]any, len(diagnostic.GetContext().GetFields())+1)
	for key, value := range diagnostic.GetContext().AsMap() {
		context[key] = value
	}
	if diagnostic.GetSourcePath() != "" {
		context["source_path"] = diagnostic.GetSourcePath()
	}
	record := dlog.Record{
		Timestamp: dlog.NewStamp(time.UnixMilli(ev.GetProducedAtMs())), Runtime: dlog.RuntimeSidecar,
		Level: dlog.Level(diagnostic.GetLevel()), Verbosity: dlog.Verbosity(diagnostic.GetVerbosity()),
		Operation: diagnostic.GetOperation(), Message: diagnostic.GetMessage(), Context: context,
		PID: int(diagnostic.GetSourcePid()),
	}
	return logger.PersistForwarded(ws, dlog.RuntimeSidecar, record, dlog.ForwardedIdentity{
		AgentReplSessionID: agentReplSessionID, ClaudeSessionID: ev.GetSessionId(), RequestID: ev.GetRequestId(),
	})
}

// ssmSnapshotProvider implements frontend.StateProvider from the SSM's
// resolved per-workspace state plus per-session metadata from the registry
// (model/slug/title where the daemon has them, design §14.2 point 1).
type ssmSnapshotProvider struct {
	ssm      *ssm.Manager
	sessions SessionMetaSource
	// inits supplies the retained SessionInitView of every live session (S9),
	// so a (re)connecting frontend sources its slash-command/tools/model menus
	// from the snapshot. Nil-safe: a nil source leaves snapshot.inits empty.
	inits SessionInitSource
	// catalogs supplies every live session's DETACHED WORK — the complete task
	// roster and the open bubbles folded to date — so reconnect restores or
	// clears both before later deltas.
	catalogs TaskCatalogSource
	// queues supplies each live session's held-prompt queue (E4). Nil-safe: a
	// nil source leaves snapshot.queues empty.
	queues QueueSource
	// daemon supplies the DaemonView (boot id / protocol version / binary
	// mtime / version) carried on every connect snapshot. Nil-safe: a nil
	// source leaves snapshot.daemon unset rather than nil-derefing.
	daemon DaemonViewSource
	// shutdownSchedule supplies the daemon-global drain lease carried on every
	// connect snapshot, so a client that connects MID-DRAIN sees the lease
	// without waiting for an edge — the edge may never come, because a drain
	// waiting on one long turn is silent for as long as that turn runs. Nil-safe
	// only for focused unit construction; production always supplies it.
	shutdownSchedule ShutdownScheduleSource
	// progress supplies each workspace's resolved ProgressView (F1), so a
	// (re)connecting frontend's footer is populated before the next change
	// pushes. Nil-safe: a nil source leaves snapshot.progress empty.
	progress ProgressSource
	// workspaceViews supplies the three RESOLVED per-workspace views the
	// snapshot carries: the topbar, the token-breakdown menu and the revival
	// gate. It is the SAME retention the pushes advance, so a client that
	// connects between two pushes adopts exactly the view the last push
	// delivered. Nil-safe: a nil publisher leaves the three fields empty.
	workspaceViews *WorkspaceViews
	// workspaceCreation supplies retained daemon-owned work for the Emacs host.
	// frontend.Server removes these fields for all GUI client kinds; retaining
	// them here makes a reconnecting host drain the durable queue before relying
	// on best-effort live publications.
	workspaceCreation WorkspaceCreationBridge
	// logf records the complete snapshot shape at the reconnect boundary. It is
	// optional only for focused unit construction outside WireAgentShim.
	logf dlog.Logf
}

var _ frontend.StateProvider = (*ssmSnapshotProvider)(nil)

// SessionMetaSource supplies the SessionView metadata the SSM does not carry
// (model, slug, title). Bound to the session registry / live session map at
// stitch. Returning an empty slice is valid (no sessions yet).
type SessionMetaSource interface {
	SessionViews() []*frontendv1.SessionView
}

// SessionInitSource supplies the retained SystemInit of every live session as
// SessionInitViews (S9), for the connect snapshot's inits. Satisfied by
// *sessioncontroller.Manager. Returning an empty slice is valid (no inits yet).
type SessionInitSource interface {
	SessionInits() []*frontendv1.SessionInitView
}

// TaskCatalogSource supplies every live session's DETACHED WORK for the
// connect/resync snapshot: the authoritative task roster, and the open bubbles
// folded to date. Satisfied by *sessioncontroller.Manager.
//
// BOTH HALVES ON ONE INTERFACE because they are two views of one thing — the
// roster names the detached work, the bubbles are what that work produced —
// and the same object has always answered for both. They were two interfaces
// and two config fields, and the daemon's own e2e harness supplied the roster
// and forgot the bubbles for the whole life of the feature: the snapshot side
// is nil-safe by design, so every reconnect served zero bubbles with a live
// bubble outstanding and nothing said so. One source makes supplying half of it
// unrepresentable.
//
// Empty results are significant on both halves: an empty catalog clears stale
// frontend roster state, and contributing no bubbles is how a frontend learns
// its previous ones are gone.
type TaskCatalogSource interface {
	TaskCatalogs() []*frontendv1.TaskCatalog
	AsyncBubbles() []*frontendv1.AsyncBubble
}

// QueueSource supplies every live session's held-prompt queue (E4) for the
// connect/resync snapshot. Satisfied by *sessioncontroller.Manager. A session with an
// empty queue still contributes its empty view, so a reconnecting frontend is
// TOLD the queue is empty rather than left to assume it.
type QueueSource interface {
	QueueViews() []*frontendv1.QueueView
}

// ProgressSource supplies every workspace's resolved ProgressView (F1) for the
// connect/resync snapshot, so a frontend's progress footer starts populated
// rather than blank until the next change. Satisfied by *progress.Manager.
type ProgressSource interface {
	Snapshot() []*frontendv1.ProgressView
}

// ShutdownScheduleSource supplies the daemon-global drain lease for the
// connect/resync snapshot. Satisfied by *ShutdownScheduler. Its answer is never
// absent: an idle daemon reports the idle arm, which is a real value.
type ShutdownScheduleSource interface {
	View() *frontendv1.ShutdownScheduleView
}

// ShutdownScheduleController is the command half of the drain lease. Satisfied
// by *ShutdownScheduler. Every method reports a refusal as an error: a schedule
// that was not taken, or a cancel that hit nothing, must never ack ok.
type ShutdownScheduleController interface {
	Schedule(stopShims bool, cause string) (string, error)
	Cancel(scheduleID string) error
}

// QueueBackend is the daemon's whole prompt-queue surface: the command half
// and the snapshot half. Satisfied by *sessioncontroller.Manager, which owns both.
type QueueBackend interface {
	QueueController
	QueueSource
}

// Snapshot assembles a StateSnapshot from the SSM's workspace states and the
// session metadata source. A failed SSM read yields the sessions-only snapshot
// with the failure loud-logged by the SSM; it never blocks the connect.
func (p *ssmSnapshotProvider) Snapshot() *frontendv1.StateSnapshot {
	snap := &frontendv1.StateSnapshot{}
	if p.ssm != nil {
		if states, err := p.ssm.Snapshot(); err == nil {
			snap.Workspaces = states
		}
	}
	if p.sessions != nil {
		snap.Sessions = p.sessions.SessionViews()
	}
	if p.inits != nil {
		snap.Inits = p.inits.SessionInits()
	}
	if p.catalogs != nil {
		snap.Catalogs = p.catalogs.TaskCatalogs()
		snap.AsyncBubbles = refuseWorkspacelessBubbles(p.catalogs.AsyncBubbles(), p.logf)
	}
	if p.queues != nil {
		snap.Queues = p.queues.QueueViews()
	}
	if p.daemon != nil {
		snap.Daemon = p.daemon.DaemonView()
	}
	if p.progress != nil {
		snap.Progress = p.progress.Snapshot()
	}
	if p.shutdownSchedule != nil {
		snap.ShutdownSchedule = p.shutdownSchedule.View()
	}
	if p.workspaceViews != nil {
		snap.Topbars = p.workspaceViews.Topbars()
		snap.TokenBreakdowns = p.workspaceViews.TokenBreakdowns()
		snap.WorkspaceGates = p.workspaceViews.WorkspaceGates()
	}
	if p.workspaceCreation == nil {
		panic("server: snapshot provider requires workspace creation bridge")
	}
	publicationAllowed := sessionPublicationGate(p.workspaceCreation, p.logf)
	snap.Workspaces = filterPublishedSessionViews(snap.Workspaces, publicationAllowed, p.logf)
	snap.Sessions = filterPublishedSessionViews(snap.Sessions, publicationAllowed, p.logf)
	// The FENCED views carry no session id. The materialization latch is a
	// per-WORKSPACE decision and still holds them back exactly as before; the
	// gate simply has no session to name in its record, which it already
	// tolerates for every sessionless frame.
	snap.Inits = filterPublishedWorkspaceViews(snap.Inits, publicationAllowed, p.logf)
	snap.Catalogs = filterPublishedWorkspaceViews(snap.Catalogs, publicationAllowed, p.logf)
	snap.Queues = filterPublishedWorkspaceViews(snap.Queues, publicationAllowed, p.logf)
	snap.Progress = filterPublishedWorkspaceViews(snap.Progress, publicationAllowed, p.logf)
	snap.Topbars = filterPublishedWorkspaceViews(snap.Topbars, publicationAllowed, p.logf)
	snap.TokenBreakdowns = filterPublishedWorkspaceViews(snap.TokenBreakdowns, publicationAllowed, p.logf)
	snap.WorkspaceGates = filterPublishedWorkspaceViews(snap.WorkspaceGates, publicationAllowed, p.logf)
	// A bubble is a per-workspace family like any other, and the latch holds it
	// back for the same reason: its label, its command line and its spooled
	// output are the contents of work running in a workspace the client has not
	// been told exists yet. It carries no session id of its own — the workspace
	// IS its routing key — so it asks the gate the fenced question.
	snap.AsyncBubbles = filterPublishedWorkspaceViews(snap.AsyncBubbles, publicationAllowed, p.logf)
	hostWork := p.workspaceCreation.SnapshotHostWork()
	snap.WorkspaceAvailable = hostWork.WorkspaceAvailable
	snap.HostActions = hostWork.HostActions
	if p.logf != nil {
		taskCount := 0
		for _, catalog := range snap.GetCatalogs() {
			taskCount += len(catalog.GetTasks())
		}
		p.logf("frontend: connect snapshot workspaces=%d sessions=%d catalogs=%d tasks=%d async_bubbles=%d inits=%d queues=%d progress=%d workspace_available=%d host_actions=%d daemon=%t",
			len(snap.GetWorkspaces()), len(snap.GetSessions()), len(snap.GetCatalogs()), taskCount, len(snap.GetAsyncBubbles()),
			len(snap.GetInits()), len(snap.GetQueues()), len(snap.GetProgress()), len(snap.GetWorkspaceAvailable()), len(snap.GetHostActions()), snap.GetDaemon() != nil)
	}
	return snap
}

type sessionPublicationView interface {
	GetWorkspace() string
	GetSessionId() string
}

// workspacePublicationView is a FENCED view: it carries a workspace and no
// session identity.
type workspacePublicationView interface {
	GetWorkspace() string
}

// refuseWorkspacelessBubbles drops any async bubble that names no workspace,
// loudly.
//
// It is DEFENCE IN DEPTH, not the primary guard: frontend.OpenAsyncBubble
// refuses to mint a workspace-less bubble at all, so reaching this is a daemon
// defect. It is still checked here because the workspace is the ONLY routing
// key a snapshot has for a bubble, and one that slipped through would be
// delivered to every scoped client — a cross-workspace leak that the scope pass
// downstream cannot detect, since an unroutable bubble and a correctly-routed
// one are indistinguishable to it.
//
// It refuses rather than panics: the bubble is one piece of detached work, and
// a connect snapshot that aborts costs the client its whole session view.
func refuseWorkspacelessBubbles(bubbles []*frontendv1.AsyncBubble, logf func(string, ...any)) []*frontendv1.AsyncBubble {
	filtered := make([]*frontendv1.AsyncBubble, 0, len(bubbles))
	for _, bubble := range bubbles {
		if bubble.GetWorkspace() == "" {
			if logf != nil {
				logf("server: REFUSING async bubble %q from the connect snapshot — it names no workspace, which is the only routing key a snapshot has for a bubble, so it would reach every scoped client; the bubble is omitted rather than leaked",
					bubble.GetId())
			}
			continue
		}
		filtered = append(filtered, bubble)
	}
	return filtered
}

// filterPublishedWorkspaceViews is filterPublishedSessionViews for the fenced
// views, asking the same gate the same per-workspace question with no session
// to name.
func filterPublishedWorkspaceViews[T workspacePublicationView](views []T, allow func(workspace, sessionID string) (bool, error), logf func(string, ...any)) []T {
	filtered := make([]T, 0, len(views))
	for _, view := range views {
		allowed, err := allow(view.GetWorkspace(), "")
		if err != nil {
			if logf != nil {
				logf("server: SESSION PUBLICATION INVARIANT VIOLATION snapshot workspace=%q session=%q error=%v", view.GetWorkspace(), "", err)
			}
			panic(fmt.Sprintf("server: session publication snapshot invariant workspace=%q session=%q: %v", view.GetWorkspace(), "", err))
		}
		if !allowed {
			continue
		}
		filtered = append(filtered, view)
	}
	return filtered
}

func filterPublishedSessionViews[T sessionPublicationView](views []T, allow func(workspace, sessionID string) (bool, error), logf func(string, ...any)) []T {
	filtered := make([]T, 0, len(views))
	for _, view := range views {
		allowed, err := allow(view.GetWorkspace(), view.GetSessionId())
		if err != nil {
			if logf != nil {
				logf("server: SESSION PUBLICATION INVARIANT VIOLATION snapshot workspace=%q session=%q error=%v", view.GetWorkspace(), view.GetSessionId(), err)
			}
			panic(fmt.Sprintf("server: session publication snapshot invariant workspace=%q session=%q: %v", view.GetWorkspace(), view.GetSessionId(), err))
		}
		if !allowed {
			continue
		}
		filtered = append(filtered, view)
	}
	return filtered
}
