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

	"claude-repld/internal/frontend"
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
	// Prompts routes prompt/interrupt/permission to the session shim.
	Prompts PromptRouter
	// MergeDirs resolves a workspace to its merge.Request (source/target
	// worktrees + branch). Required: the merge Engine cannot run without it.
	MergeDirs MergeDirResolver
	// Lifecycle closes/opens workspaces (the Emacs workspace-command channel).
	Lifecycle WorkspaceLifecycle
	// Sessions supplies SessionView metadata (model/slug/title) for snapshots.
	Sessions SessionMetaSource
	// Resyncer replays a workspace's retained conversation deltas on a frontend
	// resync (design §5.4). Nil-safe: a nil Resyncer makes Resync a documented
	// no-op (the server still re-sends the StateSnapshot independently).
	Resyncer Resyncer
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
	Server *frontend.Server
	SSM    *ssm.Manager
	Merge  *merge.Engine

	cancelPush func()
	logf       func(string, ...any)
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
	case cfg.MergeDirs == nil:
		return nil, fmt.Errorf("server: WireAgentShim needs a MergeDirResolver")
	}
	mgr := cfg.SSM

	engine, err := merge.NewEngine(merge.Config{Logf: logf, Sink: mergeSink{mgr}})
	if err != nil {
		return nil, fmt.Errorf("server: build merge engine: %w", err)
	}

	handler, err := newCommandHandler(cfg.Prompts, mergeRunner{engine: engine, resolver: cfg.MergeDirs}, cfg.Lifecycle, cfg.Resyncer, logf)
	if err != nil {
		return nil, err
	}

	srv := frontend.New(frontend.Config{
		Logf:    logf,
		State:   &ssmSnapshotProvider{ssm: mgr, sessions: cfg.Sessions},
		Handler: handler,
	})

	// SSM state changes -> frontend WorkspaceState pushes. The loop ends when
	// Close cancels the subscription (which closes the channel).
	states, cancel := mgr.Subscribe()
	go func() {
		for ws := range states {
			srv.PushWorkspaceState(ws)
		}
	}()

	return &AgentShim{Server: srv, SSM: mgr, Merge: engine, cancelPush: cancel, logf: logf}, nil
}

// Close stops the push loop and closes the frontend server (disconnecting
// clients). It does NOT close the SSM: main opened it and owns its lifecycle
// (the per-session driver shares the same instance), so main closes it exactly
// once. Idempotent-safe for a single call.
func (a *AgentShim) Close() error {
	if a.cancelPush != nil {
		a.cancelPush()
	}
	if a.Server != nil {
		if err := a.Server.Close(); err != nil {
			a.logf("server: frontend close: %v", err)
		}
	}
	return nil
}
