// agentshimwire.go assembles the daemon's agent-shim frontend surface (design
// §9.1 ADD, §14.2): the session-state manager (SSM), the workspace-merge
// Engine whose transitions feed the SSM, and the frontend.Server that snapshots
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
	"claude-repld/internal/ssm"
	"claude-repld/internal/workspace/merge"
)

// AgentShimConfig injects everything WireAgentShim binds. SSM is opened by the
// caller (main) and injected so its lifecycle — and the per-session driver that
// also feeds it — is owned in one place; the three routers back the frontend
// command handler; MergeDirs resolves a workspace to the cherry-pick request
// the merge Engine runs; Sessions supplies SessionView metadata for snapshots.
type AgentShimConfig struct {
	// SSM is the session-state manager, opened and owned by the caller (main).
	// Required: the frontend snapshot and the merge-transition push loop both
	// read/write it. WireAgentShim does NOT close it — main does (the same SSM
	// is shared with the per-session driver, so one owner closes it once).
	SSM *ssm.Manager
	// Progress is the progress-footer resolver (F1), a sibling of the SSM. It is
	// created and owned by the caller for the same reason the SSM is: the
	// per-session driver folds events into it too. Required.
	Progress *progress.Manager
	// Prompts routes prompt/interrupt/permission to the session shim.
	Prompts PromptRouter
	// Turns reports whether a workspace has a turn in flight, for the interrupt
	// confirm gate (I1). Required, and it must be the same driver fleet as
	// Prompts: the gate decides whether there is a turn to stop, and reading a
	// different fleet's answer would gate one session on another's liveness.
	Turns TurnStateSource
	// Health routes correlated session health checks to the existing live shim
	// connection.  It must be the same driver fleet as Prompts. It is also what
	// createSession's establishment gate proves the new session on, so an
	// unwired Health makes every create a loud nack rather than an unprovable
	// ok (see createestablish.go).
	Health SessionHealthRouter
	// Restarts hard-restarts one workspace's session (the restartSession
	// command). Nil makes that command a loud failing ack.
	Restarts SessionRestarter
	// EstablishTimeout bounds one createSession establishment round. Zero takes
	// the package default; only a harness sets it.
	EstablishTimeout time.Duration
	// DaemonHealth supplies the one daemon-global readiness assertion shared by
	// the HTTP health route and frontend health command.
	DaemonHealth DaemonHealthChecker
	// MergeDirs resolves a workspace to its merge.Request (source/target
	// worktrees + branch). Required: the merge Engine cannot run without it.
	MergeDirs MergeDirResolver
	// Lifecycle closes/opens workspaces (the Emacs workspace-command channel).
	Lifecycle WorkspaceLifecycle
	// Sessions supplies SessionView metadata (model/slug/title) for snapshots.
	Sessions SessionMetaSource
	// Inits supplies the retained SystemInit of every live session as
	// SessionInitViews for the connect snapshot (S9). Nil-safe: a nil source
	// leaves snapshot.inits empty. Satisfied by *sessiondrv.Manager.
	Inits SessionInitSource
	// Catalogs supplies every live session's complete detached-task roster for
	// connect/resync snapshots. Satisfied by *sessiondrv.Manager.
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
	RequestShutdown func(stopShims bool)
	// WorkspaceCreation owns durable workspace-create jobs and retained host
	// actions. Required: it receives create/materialized/completion commands
	// and supplies/publishes the host-only work that Emacs renders. It is kept
	// behind the server-local bridge so this transport package never imports the
	// workspace creation implementation or store.
	WorkspaceCreation WorkspaceCreationBridge
	// ClientLogs persists canonical browser records to the webapp workspace
	// target. A missing writer makes that command fail loudly.
	ClientLogs ClientLogWriter
	// Logf is the daemon logger. Nil discards.
	Logf func(string, ...any)
}

// MergeDirResolver resolves a workspace name to the cherry-pick request the
// merge Engine runs. It owns the workspace -> (source dir, source branch,
// target dir) policy the daemon needs and the frontend command does not carry.
type MergeDirResolver interface {
	Resolve(workspace string) (merge.Request, error)
}

// AgentShim is the assembled frontend surface. Server is mounted by main.go
// (ServeUDS + ServeWS); Close tears down the push loop and the frontend server.
// The SSM is injected and owned by main (Close does NOT close it), because the
// same SSM instance also backs the per-session driver.
type AgentShim struct {
	Server   *frontend.Server
	SSM      *ssm.Manager
	Progress *progress.Manager
	Merge    *merge.Engine

	cancelPush               func()
	cancelProgress           func()
	cancelWorkspaceAvailable func()
	cancelHostActions        func()
	logf                     func(string, ...any)
}

// hostWorkPublisher is the narrow frontend surface the durable creation
// manager needs. *frontend.Server satisfies it; isolating the forwarding loop
// here keeps it testable without opening a real listener.
type hostWorkPublisher interface {
	PushWorkspaceAvailable(*frontendv1.WorkspaceAvailable)
	PushHostAction(*frontendv1.HostAction)
}

func forwardWorkspaceAvailable(logf func(string, ...any), publisher hostWorkPublisher, values <-chan *frontendv1.WorkspaceAvailable) {
	for available := range values {
		if available == nil {
			panic("server: workspace creation bridge published nil WorkspaceAvailable")
		}
		logf("server: host-work push workspace_available job_id=%s workspace=%s", available.GetJobId(), available.GetFinalName())
		publisher.PushWorkspaceAvailable(available)
	}
}

func forwardHostActions(logf func(string, ...any), publisher hostWorkPublisher, values <-chan *frontendv1.HostAction) {
	for action := range values {
		if action == nil {
			panic("server: workspace creation bridge published nil HostAction")
		}
		logf("server: host-work push host_action action_id=%s action_type=%T", action.GetActionId(), action.GetAction())
		publisher.PushHostAction(action)
	}
}

// mergeSink adapts the SSM to merge.StateSink: every merge-state transition the
// Engine emits is appended to the SSM's per-workspace log (§9.2, §4.6). A sink
// failure propagates so the Engine aborts loudly rather than losing state.
type mergeSink struct{ mgr *ssm.Manager }

func (s mergeSink) RecordMergeTransition(ws string, phase merge.Phase, cause string) error {
	return s.mgr.ApplyMergeTransition(ws, string(phase), cause)
}

// mergeRunner backs the frontend MergeRunner with the merge Engine plus the
// workspace->dirs resolver.
type mergeRunner struct {
	engine   *merge.Engine
	resolver MergeDirResolver
}

func (m mergeRunner) Merge(ctx context.Context, workspace string) error {
	req, err := m.resolver.Resolve(workspace)
	if err != nil {
		return fmt.Errorf("merge %q: resolve dirs: %w", workspace, err)
	}
	_, err = m.engine.Merge(ctx, req)
	return err
}

func (m mergeRunner) Resume(ctx context.Context, workspace string) error {
	req, err := m.resolver.Resolve(workspace)
	if err != nil {
		return fmt.Errorf("resume merge %q: resolve dirs: %w", workspace, err)
	}
	_, err = m.engine.Resume(ctx, req)
	return err
}

// WireAgentShim builds the SSM, merge Engine, and frontend Server wired
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
	case cfg.SSM == nil:
		return nil, fmt.Errorf("server: WireAgentShim needs an SSM")
	case cfg.Progress == nil:
		return nil, fmt.Errorf("server: WireAgentShim needs a progress resolver")
	case cfg.MergeDirs == nil:
		return nil, fmt.Errorf("server: WireAgentShim needs a MergeDirResolver")
	case cfg.SessionCommands == nil:
		return nil, fmt.Errorf("server: WireAgentShim needs a SessionCommands binding")
	case cfg.WorkspaceCreation == nil:
		return nil, fmt.Errorf("server: WireAgentShim needs a WorkspaceCreation bridge")
	case cfg.Turns == nil:
		return nil, fmt.Errorf("server: WireAgentShim needs a TurnStateSource (without it the interrupt confirm gate cannot tell a live turn from working subagents)")
	}
	mgr := cfg.SSM

	engine, err := merge.NewEngine(merge.Config{Logf: logf, Sink: mergeSink{mgr}})
	if err != nil {
		return nil, fmt.Errorf("server: build merge engine: %w", err)
	}

	handler, err := newCommandHandler(
		cfg.Prompts, mergeRunner{engine: engine, resolver: cfg.MergeDirs},
		cfg.Lifecycle, cfg.Resyncer, cfg.SessionCommands, cfg.RequestShutdown,
		cfg.Queues, logf,
		CommandHandlerConfig{
			WorkspaceCreation: cfg.WorkspaceCreation,
			Restarts:          cfg.Restarts,
			Health:            HealthConfig{Router: cfg.Health, Daemon: cfg.DaemonHealth},
			// The gate reads each fact from the authority that owns it: the
			// driver observes the turn boundary, and the progress resolver
			// already carries the live-task count to the footer.
			Interrupt:        InterruptGateConfig{Turns: cfg.Turns, LiveTasks: cfg.Progress},
			EstablishTimeout: cfg.EstablishTimeout,
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
	handler.clientLogs = cfg.ClientLogs

	srv := frontend.New(frontend.Config{
		Logf: logf,
		State: &ssmSnapshotProvider{
			ssm: mgr, sessions: cfg.Sessions, inits: cfg.Inits,
			catalogs: cfg.Catalogs, queues: cfg.Queues, daemon: cfg.SessionCommands,
			progress: cfg.Progress, workspaceCreation: cfg.WorkspaceCreation, logf: logf,
		},
		Handler: handler,
	})

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
			srv.PushWorkspaceState(ws)
			if err := prog.ObserveWorkspaceState(ws); err != nil {
				logf("server: progress observe workspace state: %v", err)
			}
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
		Server: srv, SSM: mgr, Progress: prog, Merge: engine,
		cancelPush: cancel, cancelProgress: cancelProgress,
		cancelWorkspaceAvailable: cancelWorkspaceAvailable, cancelHostActions: cancelHostActions, logf: logf,
	}, nil
}

// Close stops the push loop and closes the frontend server (disconnecting
// clients). It does NOT close the SSM: main opened it and owns its lifecycle
// (the per-session driver shares the same instance), so main closes it exactly
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
	if a.Server != nil {
		if err := a.Server.Close(); err != nil {
			a.logf("server: frontend close: %v", err)
		}
	}
	return nil
}
