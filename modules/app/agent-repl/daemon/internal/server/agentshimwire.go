// agentshimwire.go assembles the daemon's agent-shim frontend surface (design
// §9.1 ADD, §14.2): the session-state manager (SSM), the workspace-merge
// merge.Driver whose transitions feed the SSM, and the frontend.Server that snapshots
// SSM state on connect and pushes every SSM state change as a WorkspaceState
// frame. WireAgentShim builds them wired together; main.go opens listeners on
// the returned Server and closes the handle on shutdown.
package server

import (
	"context"
	"fmt"
	"time"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/frontend"
	"claude-repld/internal/progress"
	"claude-repld/internal/sessioncontroller"
	"claude-repld/internal/ssm"
	"claude-repld/internal/workspace/merge"
)

// AgentShimConfig injects everything WireAgentShim binds. SSM is opened by the
// caller (main) and injected so its lifecycle — and the per-session controller that
// also feeds it — is owned in one place; the three routers back the frontend
// command handler; Sessions supplies SessionView metadata for snapshots.
type AgentShimConfig struct {
	// SSM is the session-state manager, opened and owned by the caller (main).
	// Required: the frontend snapshot and the merge-transition push loop both
	// read/write it. WireAgentShim does NOT close it — main does (the same SSM
	// is shared with the per-session controller, so one owner closes it once).
	SSM *ssm.Manager
	// Progress is the progress-footer resolver (F1), a sibling of the SSM. It is
	// created and owned by the caller for the same reason the SSM is: the
	// per-session controller folds events into it too. Required.
	Progress *progress.Manager
	// Prompts routes prompt/interrupt/permission to the session shim.
	Prompts PromptRouter
	// Turns reports whether a workspace has a turn in flight, for the interrupt
	// confirm gate (I1). Required, and it must be the same controller fleet as
	// Prompts: the gate decides whether there is a turn to stop, and reading a
	// different fleet's answer would gate one session on another's liveness.
	Turns TurnStateSource
	// Health routes correlated session health checks to the existing live shim
	// connection.  It must be the same controller fleet as Prompts. It is also what
	// createSession's establishment gate proves the new session on, so an
	// unwired Health makes every create a loud nack rather than an unprovable
	// ok (see createestablish.go).
	Health SessionHealthRouter
	// Restarts hard-restarts one workspace's session (the restartSession
	// command). Nil makes that command a loud failing ack.
	Restarts SessionRestarter
	// Hibernations backs hibernateWorkspace and reviveSession — the user-forced
	// sleep and the two revival modes. Nil is a loud unsupported capability.
	Hibernations SessionHibernator
	// EstablishTimeout bounds one createSession establishment round. Zero takes
	// the package default; only a harness sets it.
	EstablishTimeout time.Duration
	// Resumes resolves which conversation a create continues, from the daemon's
	// own registry and the transcripts on disk. Required: without it every
	// ordinary create is a loud refusal, because the alternative — quietly
	// starting a fresh conversation over an intact one — is the data-loss this
	// resolver exists to prevent. See ConversationResolver.
	Resumes ConversationResumeResolver
	// DaemonHealth supplies the one daemon-global readiness assertion shared by
	// the HTTP health route and frontend health command.
	DaemonHealth DaemonHealthChecker
	// Lifecycle closes/opens workspaces (the Emacs workspace-command channel).
	Lifecycle WorkspaceLifecycle
	// SessionDeaths reports whether a workspace's newest session was DELETED,
	// which is the one thing Lifecycle's bring-up cannot tell from a
	// hibernation. Required: without it a merge of a deleted workspace
	// resurrects the very session the user destroyed. See sessiondeaths.go.
	SessionDeaths SessionDeaths
	// Sessions supplies SessionView metadata (model/slug/title) for snapshots.
	Sessions SessionMetaSource
	// Inits supplies the retained SystemInit of every live session as
	// SessionInitViews for the connect snapshot (S9). Nil-safe: a nil source
	// leaves snapshot.inits empty. Satisfied by *sessioncontroller.Manager.
	Inits SessionInitSource
	// Catalogs supplies every live session's complete detached-task roster for
	// connect/resync snapshots. Satisfied by *sessioncontroller.Manager.
	Catalogs TaskCatalogSource
	// Queues is the prompt-queue backend (E4): the force/accept/cancel command
	// half and the snapshot half. Nil makes each queue command a loud failing
	// ack and leaves snapshot.queues empty.
	Queues QueueBackend
	// SessionCommands is the late-bound daemon-core surface (session
	// create/delete + DaemonView) the command handler and snapshot provider
	// need. Required: main injects a *SessionCommandBinding and calls SetTarget
	// once the *Server exists (WireAgentShim runs first, so the Server does not
	// exist yet here — hence the late-bind holder).
	SessionCommands SessionCommands
	// Resyncer replays a workspace's retained conversation deltas on a frontend
	// resync (design §5.4). Nil-safe: a nil Resyncer makes Resync a documented
	// no-op (the server still re-sends the StateSnapshot independently).
	Resyncer Resyncer
	// RequestShutdown begins the daemon's graceful teardown, backing the
	// shutdown FrontendCommand. Its argument is ShutdownCmd.stop_shims: false
	// (the default) PRESERVES the session shims so the next daemon reattaches
	// to them. Nil makes the shutdown command a loud failing ack (the
	// capability is unconfigured).
	RequestShutdown func(stopShims bool, cause sessioncontroller.StopCause)
	// WorkspaceCreation owns durable workspace-create jobs and retained host
	// actions. Required: it receives create/materialized/completion commands
	// and supplies/publishes the host-only work that Emacs renders. It is kept
	// behind the server-local bridge so this transport package never imports the
	// workspace creation implementation or store.
	WorkspaceCreation WorkspaceCreationBridge
	// ClientLogs persists canonical browser records to the webapp workspace
	// target. A missing writer makes that command fail loudly.
	ClientLogs ClientLogWriter
	// LogTargets releases a closed workspace's log descriptors, binding their
	// lifetime to the workspace's rather than to the daemon's. Nil retains
	// them, which only a harness with no target manager should do.
	LogTargets WorkspaceLogTargetEvictor
	// MergeLease is the shim exclusivity claim merge.Coordinator holds across
	// every merge it drives. Required: without it a cherry-pick would run into
	// a session the user is still prompting, so an unbound lease is a broken
	// merge subsystem rather than a merge subsystem without a nicety. It is
	// implemented in internal/ssm and injected by main.
	MergeLease merge.Lease
	// MergeQueue is merge.Coordinator's DURABLE request channel. Required, and
	// injected rather than constructed here because the SSM's merge.Lease binds
	// the SAME instance: two queues over one directory would put two merge
	// subsystems behind one manager's leases.
	MergeQueue merge.DurableQueue
	// MergeGeometry is THE daemon's workspace -> merge-geometry map, and it is
	// what a BARE merge_workspace command (the only kind Emacs sends) resolves
	// through. main always supplies it, opened over the shared state store.
	//
	// Nil is a loud unsupported capability, exactly as it is for the other
	// optional handler bindings: every bare merge is refused with "the daemon's
	// merge-geometry record is not wired". It is left optional because a caller
	// that states all three coordinates on the command needs no map at all —
	// that is how the integration suites drive merges against fixture
	// repositories the daemon never created — and a nil map can therefore never
	// silently produce a guessed target.
	MergeGeometry MergeGeometrySource
	// ShutdownSchedules is the DURABLE half of the scheduled-shutdown drain
	// lease. Supplying it is what turns the scheduled-shutdown commands on: a
	// nil store leaves them loud unsupported-capability nacks, because a lease
	// nothing records could be erased by a crash while every client kept waiting
	// for a bounce nothing was driving. main always supplies it, opened over the
	// shared state store; focused harnesses leave it nil.
	ShutdownSchedules ShutdownScheduleStore
	// DrainHolds is the session fleet the drain lease binds itself to, and the
	// authority on which workspaces hold the drain open. Required whenever
	// ShutdownSchedules is set. Satisfied by *sessioncontroller.Manager.
	DrainHolds DrainHoldSource
	// DrainEvidence is the durable evidence a RESTORED lease seeds its
	// unresolved set from — the registry plus the parked-connection and
	// session-lock probes. Required whenever ShutdownSchedules is set: without
	// it a lease restored mid-drain would read the not-yet-wired fleet as
	// quiescent and bounce over every surviving mid-turn shim. Satisfied by
	// RegistryDrainEvidence.
	DrainEvidence DrainEvidenceSource
	// CommandLatency persists one lifecycle-timing record per completed
	// frontend command: its ack and processing durations, and the command
	// queue depth at receipt. main always supplies it; a focused harness that
	// leaves it nil gets a loud construction line from frontend.New and no
	// telemetry, never a silently timed daemon.
	CommandLatency frontend.CommandLatencyRecorder
	// AckWarnThreshold is the ack latency at which a command's lifecycle
	// record is raised from debug to warn. Non-positive uses the frontend
	// package's default.
	AckWarnThreshold time.Duration
	// Logf is the daemon logger. Nil discards.
	Logf func(string, ...any)
	// Warnf is the daemon logger's WARN channel, carried to the frontend
	// server so a refused command or a connect served without its snapshot is
	// not recorded at info. Nil leaves the frontend server on Logf.
	Warnf func(string, ...any)
	// LogVerbosef persists frequent lease-success diagnostics without forcing
	// them onto the daemon terminal. Required so freshness cannot be opaque or
	// inflate normal console volume.
	LogVerbosef func(string, ...any)
}

// AgentShim is the assembled frontend surface. Server is mounted by main.go
// (ServeUDS + ServeWS); Close tears down the push loop and the frontend server.
// The SSM is injected and owned by main (Close does NOT close it), because the
// same SSM instance also backs the per-session controller.
type AgentShim struct {
	Server   *frontend.Server
	SSM      *ssm.Manager
	Progress *progress.Manager
	Merge    *merge.Driver
	// MergeCoordinator owns the per-repository merge queue that drives Merge.
	// Close stops its drains; a merge in flight keeps its durable queue entry
	// so the next daemon resumes it.
	MergeCoordinator *merge.QueueCoordinator
	// MergeDispatch routes the daemon's OWN merge ingress (the "merge" verb in
	// a workspace command file) through the same command path a frontend merge
	// takes. main binds it into the workspace-command inbox.
	MergeDispatch *MergeDispatch
	// ShutdownScheduler is the daemon-global drain lease, or nil when the
	// capability is unconfigured. main calls Restore on it once, at boot.
	ShutdownScheduler *ShutdownScheduler

	cancelPush                       func()
	cancelProgress                   func()
	cancelWorkspaceAvailable         func()
	cancelHostActions                func()
	cancelSessionPublicationReleases func()
	logf                             func(string, ...any)
}

// hostWorkPublisher is the narrow frontend surface the durable creation
// manager needs. *frontend.Server satisfies it; isolating the forwarding loop
// here keeps it testable without opening a real listener.
// Both methods report the number of HOST clients the frame reached, because
// host-only work delivered to zero host clients is lost work and the forwarding
// loops below are the only place that can say so.
type hostWorkPublisher interface {
	PushWorkspaceAvailable(*frontendv1.WorkspaceAvailable) int
	PushHostAction(*frontendv1.HostAction) int
}

// forwardWorkspaceAvailable is the daemon's MATERIALIZATION REQUEST — the line
// that asks the editor to render a freshly created workspace.
//
// It logs the request and its delivery separately on purpose. A workspace that
// never materializes has exactly two explanations — the request never reached
// the editor, or the editor received it and did nothing — and one line saying
// "pushed" could not distinguish them. That ambiguity cost a whole afternoon:
// the daemon had bounced, the running editor never re-established its HOST
// connection, and every materialization request went to a client set containing
// only GUI streams, which are barred from host-only frames. The push was logged,
// the delivery was zero, and nothing anywhere said so.
func forwardWorkspaceAvailable(logf func(string, ...any), publisher hostWorkPublisher, values <-chan *frontendv1.WorkspaceAvailable) {
	for available := range values {
		if available == nil {
			panic("server: workspace creation bridge published nil WorkspaceAvailable")
		}
		logf("server: MATERIALIZATION REQUESTED job_id=%s workspace=%s worktree=%q session=%s", available.GetJobId(), available.GetFinalName(), available.GetWorktreePath(), available.GetSessionId())
		delivered := publisher.PushWorkspaceAvailable(available)
		if delivered == 0 {
			logf("server: MATERIALIZATION REQUEST UNDELIVERED job_id=%s workspace=%s host_clients=0 — no Emacs host is connected, so this request reached nobody; the job stays awaiting_emacs and is re-requested until a host answers", available.GetJobId(), available.GetFinalName())
			continue
		}
		logf("server: materialization request delivered job_id=%s workspace=%s host_clients=%d", available.GetJobId(), available.GetFinalName(), delivered)
	}
}

func forwardHostActions(logf func(string, ...any), publisher hostWorkPublisher, values <-chan *frontendv1.HostAction) {
	for action := range values {
		if action == nil {
			panic("server: workspace creation bridge published nil HostAction")
		}
		logf("server: host-work push host_action action_id=%s action_type=%T", action.GetActionId(), action.GetAction())
		if delivered := publisher.PushHostAction(action); delivered == 0 {
			logf("server: HOST ACTION UNDELIVERED action_id=%s action_type=%T host_clients=0 — the action stays pending and rides the next host connect's snapshot", action.GetActionId(), action.GetAction())
		}
	}
}

func sessionPublicationGate(bridge WorkspaceCreationBridge, logf func(string, ...any)) func(workspace, sessionID string) (bool, error) {
	return func(workspace, sessionID string) (bool, error) {
		decision, err := bridge.SessionPublicationDecision(workspace, sessionID)
		if err != nil {
			return false, err
		}
		if decision.Materialized {
			return true, nil
		}
		logf("server: session publication HELD job_id=%q worktree=%q session=%q frame_session=%q reason=awaiting_workspace_materialization", decision.JobID, decision.WorktreePath, decision.SessionID, sessionID)
		return false, nil
	}
}

// mergeSink adapts the SSM to merge.StateSink: every merge-state transition the
// merge.Driver emits is appended to the SSM's per-workspace log (§9.2, §4.6). A sink
// failure propagates so the merge.Driver aborts loudly rather than losing state.
type mergeSink struct{ mgr *ssm.Manager }

func (s mergeSink) RecordMergeTransition(ws string, phase merge.Phase, cause string) error {
	return s.mgr.ApplyMergeTransition(ws, string(phase), cause)
}

// RecordMergeStatus is the same append, carrying the phase-level MergeStatus the
// merge PIPELINE published with it. Both ends of the pair go through one SSM call
// so a frame can never carry the phase word without the progress behind it.
func (s mergeSink) RecordMergeStatus(ws string, phase merge.Phase, cause string, status *frontendv1.MergeStatus) error {
	return s.mgr.ApplyMergeStatus(ws, string(phase), cause, status)
}

var (
	_ merge.StateSink  = mergeSink{}
	_ merge.StatusSink = mergeSink{}
)

// mergePhases adapts the SSM to merge.PhaseSource: the boot sweep's read of
// which workspaces are still pinned on a merge phase. It is the same log
// mergeSink writes to, read back — which is the point: merge_enqueuing has no
// durable queue entry, so the pushed state IS the record that the attempt
// happened, and the only place a boot can find one that died.
type mergePhases struct{ mgr *ssm.Manager }

func (p mergePhases) WorkspacesAtPhase(phase merge.Phase) ([]string, error) {
	return p.mgr.WorkspacesAtMergePhase(phase)
}

var _ merge.PhaseSource = mergePhases{}

// mergeConflictResolver adapts the PromptRouter to merge.ConflictResolver, the
// port merge.Coordinator drives a parked conflict through.
//
// IT IS DERIVED FROM Prompts RATHER THAN INJECTED SEPARATELY, and that is the
// point. The resolution prompt is admissible only against the session the merge
// lease was taken over, and that lease is taken over the fleet Prompts routes
// to. A second injection point could be bound to a different controller fleet —
// resolving a conflict on one daemon's session while another holds the lease —
// which this makes unrepresentable rather than merely unlikely.
type mergeConflictResolver struct{ prompts PromptRouter }

func (r mergeConflictResolver) Resolve(ctx context.Context, res merge.ConflictResolution) error {
	return r.prompts.ResolveMergeConflict(ctx, res)
}

// mergeBeforeActionRunner adapts the PromptRouter to merge.BeforeActionRunner.
// It is derived from Prompts for exactly the reason mergeConflictResolver is:
// the action is admissible only against the session the merge lease was taken
// over, and that lease is taken over the fleet Prompts routes to.
type mergeBeforeActionRunner struct{ prompts PromptRouter }

func (r mergeBeforeActionRunner) Run(ctx context.Context, act merge.BeforeAction) error {
	return r.prompts.RunMergeBeforeAction(ctx, act)
}

var _ merge.BeforeActionRunner = mergeBeforeActionRunner{}

// mergeAfterActionRunner adapts the PromptRouter to merge.AfterActionRunner. It
// is derived from Prompts for the reason every other merge-driven prompt is: the
// action is admissible only against the session the merge lease was taken over.
type mergeAfterActionRunner struct{ prompts PromptRouter }

func (r mergeAfterActionRunner) Run(ctx context.Context, act merge.AfterAction) error {
	return r.prompts.RunMergeAfterAction(ctx, act)
}

var _ merge.AfterActionRunner = mergeAfterActionRunner{}

// MergeBeforeActionSource resolves the before_ws_merge action a workspace was
// CREATED with, keyed by that workspace's worktree path.
//
// It is the sibling of postmerge.PostprocessingSource and is satisfied by the
// SAME WorkspaceCreation bridge, which is the whole point: `before_ws_merge` and
// `postprocessing_prompt` are two fields of one create Request, so one merge run
// must read both out of one set of creation records. Resolving one from the
// creation store and the other from the geometry record gave the daemon two
// spellings of one creation-time fact, and a workspace created through a path
// that filled in only one of them merged with the other silently skipped.
type MergeBeforeActionSource interface {
	BeforeWSMergePrompt(worktreePath string) (string, error)
}

// mergeBeforeActions adapts the workspace-creation records to
// merge.BeforeActionSource.
type mergeBeforeActions struct{ creation MergeBeforeActionSource }

func (s mergeBeforeActions) BeforeAction(ws string) (string, error) {
	if s.creation == nil {
		// A merge cannot be answered at all without the creation records, and
		// reporting "no action" would run a merge that silently skipped the action
		// the user asked for at creation.
		return "", fmt.Errorf("server: no workspace-creation source is wired, so the before-merge action for workspace %q cannot be read", ws)
	}
	prompt, err := s.creation.BeforeWSMergePrompt(ws)
	if err != nil {
		return "", fmt.Errorf("server: read the before-merge action for workspace %q: %w", ws, err)
	}
	return prompt, nil
}

var _ merge.BeforeActionSource = mergeBeforeActions{}

// mergeSessionBringUp adapts the workspace lifecycle to merge.SessionBringUp: a
// merge brings its workspace's session up through THE SAME path a user's
// open_workspace command does, so a merge can never establish a session in a
// shape an ordinary open would not produce.
type mergeSessionBringUp struct{ lifecycle WorkspaceLifecycle }

// It is OpenDriveable rather than Open because the merge's very next act is a
// SEND — the lease's interrupt — and Open returns while the shim is still
// handshaking. Against a hibernated workspace that lost the race outright: the
// interrupt was refused with "no live shim connection" tens of milliseconds
// before the link came up, failing a merge whose session was in fact returning.
func (b mergeSessionBringUp) EnsureLive(ctx context.Context, ws string) error {
	if b.lifecycle == nil {
		// A merge drives this workspace's session; without the bring-up path
		// there is no way to establish one, and proceeding to the lease would
		// discover that one phase later with the lease already taken.
		return fmt.Errorf("server: no workspace lifecycle is wired, so the session for workspace %q cannot be brought up for its merge", ws)
	}
	return b.lifecycle.OpenForMerge(ctx, ws)
}

var _ merge.SessionBringUp = mergeSessionBringUp{}

// mergeSessionDeaths adapts the registry's deletion fact to merge.SessionDeaths.
type mergeSessionDeaths struct{ deaths SessionDeaths }

func (d mergeSessionDeaths) DeletedSession(ws string) (string, bool, error) {
	if d.deaths == nil {
		// Never the benign answer. A merge decides whether to bring a session up
		// on this, and "no source" reported as "not deleted" resurrects exactly
		// the session the user destroyed.
		return "", false, fmt.Errorf("server: no session-deaths source is wired, so the deletion state of workspace %q cannot be read for its merge", ws)
	}
	return d.deaths.DeletedSession(ws)
}

var _ merge.SessionDeaths = mergeSessionDeaths{}

var _ merge.ConflictResolver = mergeConflictResolver{}

// mergeTestFailureResolver adapts the PromptRouter to merge.TestFailureResolver,
// the port merge.Coordinator drives a broken test suite through. It is derived
// from Prompts for exactly the reason mergeConflictResolver is: the fix prompt
// is admissible only against the session the merge lease was taken over.
type mergeTestFailureResolver struct{ prompts PromptRouter }

func (r mergeTestFailureResolver) Resolve(ctx context.Context, res merge.TestFailureResolution) error {
	return r.prompts.ResolveMergeTestFailure(ctx, res)
}

var _ merge.TestFailureResolver = mergeTestFailureResolver{}

// The command handler's merge surface IS merge.Coordinator. There is no
// adapter between them any more: the handler enqueues, the coordinator owns
// the queue and the lease, and merge.Driver is reached only from inside the
// coordinator's drain. The adapter that used to sit here called merge.Driver
// straight from the command, which put every concurrent merge_workspace
// command on the same target worktree at once.
var _ MergeRunner = (*merge.QueueCoordinator)(nil)

// WireAgentShim builds the SSM, merge.Driver, and frontend Server wired
// together, and starts the SSM-subscribe -> PushWorkspaceState loop. The
// caller mounts cfg's listeners on the returned Server and calls Close on
// shutdown. A missing required dependency or a failed SSM open is a hard error
// (the caller decides whether to disable the capability or abort).
func WireAgentShim(cfg AgentShimConfig) (*AgentShim, error) {
	logf := cfg.Logf
	if logf == nil {
		logf = func(string, ...any) {}
	}
	switch {
	case cfg.LogVerbosef == nil:
		return nil, fmt.Errorf("server: WireAgentShim needs a verbose logger")
	case cfg.SSM == nil:
		return nil, fmt.Errorf("server: WireAgentShim needs an SSM")
	case cfg.Progress == nil:
		return nil, fmt.Errorf("server: WireAgentShim needs a progress resolver")
	case cfg.SessionCommands == nil:
		return nil, fmt.Errorf("server: WireAgentShim needs a SessionCommands binding")
	case cfg.WorkspaceCreation == nil:
		return nil, fmt.Errorf("server: WireAgentShim needs a WorkspaceCreation bridge")
	case cfg.Turns == nil:
		return nil, fmt.Errorf("server: WireAgentShim needs a TurnStateSource (without it the interrupt confirm gate cannot tell a live turn from working subagents)")
	case cfg.Resumes == nil:
		return nil, fmt.Errorf("server: WireAgentShim needs a ConversationResumeResolver (without it the daemon cannot tell a workspace's existing conversation from a new one, and every create would have to start fresh)")
	case cfg.MergeLease == nil:
		return nil, fmt.Errorf("server: WireAgentShim needs a merge.Lease (without it a cherry-pick would run into a session the user is still prompting)")
	case cfg.MergeQueue == nil:
		return nil, fmt.Errorf("server: WireAgentShim needs a merge.Queue (without it the merge queue is not durable and a self-merge's daemon bounce loses it)")
	case cfg.SessionDeaths == nil:
		return nil, fmt.Errorf("server: WireAgentShim needs a SessionDeaths source (without it a merge cannot tell a hibernated session from one the user deleted, and would resurrect the second while rehydrating the first)")
	case cfg.Prompts == nil:
		return nil, fmt.Errorf("server: WireAgentShim needs a PromptRouter (it is both the frontend's submit path and merge.Coordinator's conflict-resolution path)")
	}
	mgr := cfg.SSM

	// The per-commit test gate. It resolves the TARGET repository's own test
	// entrypoint, so a target that declares none skips the gate loudly rather
	// than failing every merge into a repository that is not this one.
	suite, err := merge.NewRepoSuiteRunner(logf)
	if err != nil {
		return nil, fmt.Errorf("server: build merge suite runner: %w", err)
	}
	driver, err := merge.NewDriver(merge.Config{Logf: logf, Sink: mergeSink{mgr}, Suite: suite})
	if err != nil {
		return nil, fmt.Errorf("server: build merge driver: %w", err)
	}
	keyer, err := merge.NewGitRepoKeyer(logf)
	if err != nil {
		return nil, fmt.Errorf("server: build merge repo keyer: %w", err)
	}
	// Process-level merge aftermath is independent of the workspace's own
	// after-action, which still runs under the merge lease.
	postMerge, err := buildPostMergeHook(logf)
	if err != nil {
		return nil, err
	}
	afterActions, err := buildAfterActionSource(cfg, logf)
	if err != nil {
		return nil, err
	}
	// The before-action's source is a CHECKED derivation of the creation bridge,
	// never a silent downgrade to "this workspace has none": a daemon whose
	// merges quietly stop running the gate the user asked for at creation is the
	// exact failure the acceptance gate exists to catch.
	beforeActions, ok := cfg.WorkspaceCreation.(MergeBeforeActionSource)
	if !ok {
		return nil, fmt.Errorf("server: the WorkspaceCreation bridge (%T) cannot resolve a workspace's before_ws_merge action, so a workspace created with one would merge without ever running the gate it was created with", cfg.WorkspaceCreation)
	}
	coordinator, err := merge.NewCoordinator(merge.CoordinatorConfig{
		Logf:         logf,
		Sink:         mergeSink{mgr},
		Queue:        cfg.MergeQueue,
		Phases:       mergePhases{mgr},
		Keyer:        keyer,
		Picker:       driver,
		Lease:        cfg.MergeLease,
		Resolver:     mergeConflictResolver{prompts: cfg.Prompts},
		TestResolver: mergeTestFailureResolver{prompts: cfg.Prompts},
		PostMerge:    postMerge,
		// The SAME sink the transitions go through, so a phase word and the
		// progress beneath it cannot come from two different records.
		Status:   mergeSink{mgr},
		Sessions: mergeSessionBringUp{lifecycle: cfg.Lifecycle},
		// Asked BEFORE the bring-up: a deleted session must never be spawned
		// back to run a merge action.
		Deaths: mergeSessionDeaths{deaths: cfg.SessionDeaths},
		// The before-action is a creation-time fact, so it is resolved from THE
		// records the create commands wrote — the same ones the after-action's
		// prompt comes out of.
		BeforeActions:      mergeBeforeActions{creation: beforeActions},
		AfterActions:       afterActions,
		BeforeActionRunner: mergeBeforeActionRunner{prompts: cfg.Prompts},
		AfterActionRunner:  mergeAfterActionRunner{prompts: cfg.Prompts},
	})
	if err != nil {
		return nil, fmt.Errorf("server: build merge coordinator: %w", err)
	}
	// Boot-time reconstruction. A workspace that merges the daemon's own
	// repository bounces the daemon mid-queue, so the durable entries left by
	// the previous process are resumed here rather than forgotten.
	if err := coordinator.Drain(context.Background()); err != nil {
		coordinator.Close()
		return nil, fmt.Errorf("server: drain merge queue: %w", err)
	}

	handler, err := newCommandHandler(
		cfg.Prompts, coordinator,
		cfg.Lifecycle, cfg.Resyncer, cfg.SessionCommands, cfg.RequestShutdown,
		cfg.Queues, logf,
		CommandHandlerConfig{
			WorkspaceCreation: cfg.WorkspaceCreation,
			MergeGeometry:     cfg.MergeGeometry,
			// The SAME sink the coordinator and merge.Driver write through, so
			// the handler's merge_enqueuing lands on one merge axis with every
			// later phase rather than on a parallel record.
			MergeStates:  mergeSink{mgr},
			Restarts:     cfg.Restarts,
			Hibernations: cfg.Hibernations,
			Health:       HealthConfig{Router: cfg.Health, Daemon: cfg.DaemonHealth},
			// The gate reads each fact from the authority that owns it: the
			// controller observes the turn boundary, and the progress resolver
			// already carries the live-task count to the footer.
			Interrupt:        InterruptGateConfig{Turns: cfg.Turns, LiveTasks: cfg.Progress},
			EstablishTimeout: cfg.EstablishTimeout,
			Resumes:          cfg.Resumes,
			LogTargets:       cfg.LogTargets,
		},
	)
	if err != nil {
		return nil, err
	}
	// Validate the required typed subscriptions before starting any push loop. That
	// keeps WireAgentShim atomic: an invalid bridge cannot leave a live SSM or
	// progress subscription behind after construction reports an error.
	workspaceAvailable, cancelWorkspaceAvailable := cfg.WorkspaceCreation.SubscribeWorkspaceAvailable()
	if workspaceAvailable == nil || cancelWorkspaceAvailable == nil {
		return nil, fmt.Errorf("server: workspace creation bridge returned an invalid workspace-available subscription")
	}
	hostActions, cancelHostActions := cfg.WorkspaceCreation.SubscribeHostActions()
	if hostActions == nil || cancelHostActions == nil {
		cancelWorkspaceAvailable()
		return nil, fmt.Errorf("server: workspace creation bridge returned an invalid host-action subscription")
	}
	publicationReleases, cancelPublicationReleases := cfg.WorkspaceCreation.SubscribeSessionPublicationReleases()
	if publicationReleases == nil || cancelPublicationReleases == nil {
		cancelWorkspaceAvailable()
		cancelHostActions()
		return nil, fmt.Errorf("server: workspace creation bridge returned an invalid session-publication release subscription")
	}
	publicationAllowed := sessionPublicationGate(cfg.WorkspaceCreation, logf)
	handler.clientLogs = cfg.ClientLogs
	// An unwired recorder is stated rather than assumed. main always supplies
	// one; a focused harness legitimately does not, and the difference between
	// those two cases must not be something an operator has to infer from an
	// absent record.
	if cfg.CommandLatency == nil {
		logf("server: frontend command lifecycle latency telemetry is NOT wired; per-command ack and processing durations, and the command queue depth at receipt, will not be recorded")
	}

	// The daemon-side merge ingress. It shares the handler above so a dispatched
	// merge and a frontend merge record the same phases through the same sink.
	mergeDispatch, err := NewMergeDispatch(cfg.MergeGeometry, handler, logf)
	if err != nil {
		cancelWorkspaceAvailable()
		cancelHostActions()
		return nil, err
	}

	snapshots := &ssmSnapshotProvider{
		ssm: mgr, sessions: cfg.Sessions, inits: cfg.Inits,
		catalogs: cfg.Catalogs, queues: cfg.Queues, daemon: cfg.SessionCommands,
		progress: cfg.Progress, workspaceCreation: cfg.WorkspaceCreation, logf: logf,
	}
	srv := frontend.New(frontend.Config{
		Logf:                      logf,
		Warnf:                     cfg.Warnf,
		LogVerbosef:               cfg.LogVerbosef,
		State:                     snapshots,
		Handler:                   handler,
		SessionPublicationAllowed: publicationAllowed,
		CommandLatency:            cfg.CommandLatency,
		AckWarnThreshold:          cfg.AckWarnThreshold,
	})

	// THE DRAIN LEASE, constructed last because it needs the frontend server to
	// broadcast through and the fleet to bind to, and bound into the handler and
	// the snapshot provider immediately afterwards. Both bindings are set
	// together: a lease that commands could take but snapshots did not carry
	// would leave a client that connected mid-drain seeing nothing at all.
	var scheduler *ShutdownScheduler
	if cfg.ShutdownSchedules != nil {
		switch {
		case cfg.DrainHolds == nil:
			cancelWorkspaceAvailable()
			cancelHostActions()
			return nil, fmt.Errorf("server: a durable shutdown-schedule store was supplied with no DrainHolds source; the lease would have nothing to derive its holds from and would bounce the daemon over a live turn")
		case cfg.DrainEvidence == nil:
			cancelWorkspaceAvailable()
			cancelHostActions()
			return nil, fmt.Errorf("server: a durable shutdown-schedule store was supplied with no DrainEvidence source; a lease restored mid-drain would judge quiescence against a fleet nothing has wired yet and bounce the daemon over every surviving mid-turn shim")
		case cfg.Queues == nil:
			// PARKING WITHOUT EXITS IS UNSHIPPABLE. Taking the lease parks every
			// submitted prompt, and a parked prompt's only ways out short of the
			// bounce are the queue's force and cancel commands. A daemon wired to
			// park prompts it can then neither run nor drop would strand the user
			// with a chip and no verb, so it is refused at construction rather
			// than discovered by the first person who types under a drain.
			cancelWorkspaceAvailable()
			cancelHostActions()
			return nil, fmt.Errorf("server: a durable shutdown-schedule store was supplied with no Queues backend; the drain lease would park prompts that no force and no cancel could ever release")
		}
		scheduler, err = NewShutdownScheduler(ShutdownSchedulerConfig{
			Store:     cfg.ShutdownSchedules,
			Holds:     cfg.DrainHolds,
			Evidence:  cfg.DrainEvidence,
			LiveTasks: cfg.Progress,
			Broadcast: srv.PushShutdownSchedule,
			// THE SAME graceful teardown the ordinary shutdown command runs.
			// A parallel path would be a second definition of what an orderly
			// exit means, and the two would drift.
			Shutdown: cfg.RequestShutdown,
			Logf:     logf,
		})
		if err != nil {
			cancelWorkspaceAvailable()
			cancelHostActions()
			return nil, err
		}
		handler.schedules = scheduler
		snapshots.shutdownSchedule = scheduler
	} else {
		logf("server: scheduled shutdown UNCONFIGURED — no durable schedule store was supplied, so schedule_shutdown and cancel_scheduled_shutdown are loud unsupported-capability nacks")
	}

	// SSM state changes -> frontend WorkspaceState pushes, AND the progress
	// resolver's phase mirror (F1). Feeding the progress resolver from the same
	// subscription is the seam between the two resolvers: the footer repeats the
	// SSM's verdict rather than forming a second opinion from the same events,
	// so the footer's phase can never disagree with the sidebar's. The loop ends
	// when Close cancels the subscription (which closes the channel).
	prog := cfg.Progress
	states, cancel := mgr.Subscribe()
	go func() {
		for ws := range states {
			logf("server: SSM workspace state forward ws=%q session=%q generation=%q state=%s connectivity=%s status=%s faults=%d cause_kind=%q cause_seq=%d branch=frontend+progress",
				ws.GetWorkspace(),
				ws.GetSessionId(),
				ws.GetControllerGenerationId(),
				ws.GetState(),
				ws.GetConnectivity(),
				ws.GetStatus(),
				len(ws.GetActiveFaults()),
				ws.GetCauseKind(),
				ws.GetCauseSeq())
			srv.PushWorkspaceState(ws)
			if err := prog.ObserveWorkspaceState(ws); err != nil {
				logf("server: progress observe workspace state: %v", err)
			}
			// A drain hold's LIVE-TASK half moves on this subscription, and it
			// is told AFTER the progress resolver has absorbed the state — the
			// lease reads the count from that resolver, so telling it first
			// would have it re-derive the holds from the previous count and
			// then never hear about the new one.
			if scheduler != nil {
				scheduler.NoteDrainActivity()
			}
		}
	}()

	go func() {
		for release := range publicationReleases {
			logf("server: session publication RELEASED job_id=%q worktree=%q session=%q action=publish_authoritative_snapshot", release.JobID, release.WorktreePath, release.SessionID)
			if release.Open == nil || release.Completion == nil {
				logf("server: SESSION PUBLICATION INVARIANT VIOLATION job_id=%q worktree=%q session=%q reason=missing_release_completion", release.JobID, release.WorktreePath, release.SessionID)
				panic(fmt.Sprintf("server: session publication release job=%q worktree=%q session=%q lacks open or completion", release.JobID, release.WorktreePath, release.SessionID))
			}
			release.Completion <- srv.ReleaseSessionPublication(release.Open, snapshots.Snapshot)
		}
	}()

	// Progress changes -> frontend ProgressView pushes, on their own
	// subscription so the resolver's coalescing governs the footer's frame rate
	// independently of the SSM's transition cadence.
	views, cancelProgress := prog.Subscribe()
	go func() {
		for v := range views {
			srv.PushProgressView(v)
		}
	}()

	// Durable workspace work -> host-only frontend pushes. The bridge's store
	// snapshot is authoritative on reconnect; these subscriptions only reduce
	// latency for an already-connected Emacs. Separate typed channels make an
	// ambiguous availability/action publication unrepresentable.
	go func() {
		forwardWorkspaceAvailable(logf, srv, workspaceAvailable)
	}()
	go func() {
		forwardHostActions(logf, srv, hostActions)
	}()

	return &AgentShim{
		Server: srv, SSM: mgr, Progress: prog, Merge: driver, MergeCoordinator: coordinator, MergeDispatch: mergeDispatch,
		ShutdownScheduler: scheduler,
		cancelPush:        cancel, cancelProgress: cancelProgress,
		cancelWorkspaceAvailable: cancelWorkspaceAvailable, cancelHostActions: cancelHostActions, logf: logf,
		cancelSessionPublicationReleases: cancelPublicationReleases,
	}, nil
}

// Close stops the push loop and closes the frontend server (disconnecting
// clients). It does NOT close the SSM: main opened it and owns its lifecycle
// (the per-session controller shares the same instance), so main closes it exactly
// once. Idempotent-safe for a single call.
func (a *AgentShim) Close() error {
	if a.cancelPush != nil {
		a.cancelPush()
	}
	if a.cancelProgress != nil {
		a.cancelProgress()
	}
	if a.cancelWorkspaceAvailable != nil {
		a.cancelWorkspaceAvailable()
	}
	if a.cancelHostActions != nil {
		a.cancelHostActions()
	}
	if a.cancelSessionPublicationReleases != nil {
		a.cancelSessionPublicationReleases()
	}
	if a.MergeCoordinator != nil {
		// Stops the drains. A merge in flight keeps its durable queue entry, so
		// the next daemon's Drain resumes it rather than losing it.
		if err := a.MergeCoordinator.Close(); err != nil {
			a.logf("server: merge coordinator close: %v", err)
		}
	}
	if a.Server != nil {
		if err := a.Server.Close(); err != nil {
			a.logf("server: frontend close: %v", err)
		}
	}
	return nil
}
