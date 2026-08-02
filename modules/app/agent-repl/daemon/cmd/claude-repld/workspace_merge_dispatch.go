package main

import (
	"context"
	"errors"
	"fmt"
	"sync/atomic"

	"claude-repld/internal/server"
	workspacecreate "claude-repld/internal/workspace/create"
)

// WorkspaceMergeDispatchBinding is the late-bound bridge from the
// workspace-command inbox to the daemon's merge dispatch.
//
// The binding exists for construction order alone: the inbox is assembled
// before WireAgentShim (so frontend commands never see an unbound creation
// capability), while *server.MergeDispatch needs the merge coordinator
// WireAgentShim builds. main calls SetTarget once, before the inbox is started.
// It is the same late-bind shape as WorkspaceCreateHostForwarder.
//
// An UNBOUND dispatch is a construction-order bug, never a normal runtime state,
// so it fails structurally: the command file keeps its claim and the next scan
// retries it rather than the merge being quarantined as if the emitter were at
// fault.
type WorkspaceMergeDispatchBinding struct {
	Logf   func(string, ...any)
	target atomic.Pointer[server.MergeDispatch]
}

var _ workspacecreate.MergeDispatcher = (*WorkspaceMergeDispatchBinding)(nil)

// SetTarget binds the dispatch the inbox routes merges through.
func (b *WorkspaceMergeDispatchBinding) SetTarget(d *server.MergeDispatch) { b.target.Store(d) }

// DispatchMerge implements workspacecreate.MergeDispatcher.
//
// It translates the merge subsystem's rejection sentinel into the ingress's,
// which is what lets the inbox quarantine an unresolvable merge while still
// retrying a structural failure. The two sentinels stay separate because the
// packages are: neither imports the other.
func (b *WorkspaceMergeDispatchBinding) DispatchMerge(ctx context.Context, cmd workspacecreate.MergeCommand) error {
	dispatch := b.target.Load()
	if dispatch == nil {
		if b.Logf != nil {
			b.Logf("claude-repld: merge dispatch used before SetTarget workspace=%q project_dir=%s — the daemon's merge surface is not wired yet", cmd.Workspace, cmd.ProjectDir)
		}
		return fmt.Errorf("claude-repld: merge dispatch binding not wired")
	}
	err := dispatch.DispatchMerge(ctx, cmd.Workspace, cmd.ProjectDir, cmd.ID)
	if errors.Is(err, server.ErrMergeWorkspaceUnrecorded) {
		return fmt.Errorf("%w: %v", workspacecreate.ErrUnknownMergeWorkspace, err)
	}
	return err
}
