package server

import (
	"context"
	"errors"
	"fmt"
	"sync/atomic"

	workspacecreate "claude-repld/internal/workspace/create"
)

// MergeDispatchBinding is the late-bound bridge from the workspace-command inbox
// to the daemon's merge dispatch.
//
// The binding exists for construction order alone: the inbox is assembled before
// WireAgentShim (so frontend commands never see an unbound creation capability),
// while *MergeDispatch needs the merge coordinator WireAgentShim builds. The
// daemon calls SetTarget once, before the inbox is started. It is the same
// late-bind shape as the workspace-create host forwarder.
//
// IT LIVES IN THIS PACKAGE ON PURPOSE. It is the ONLY place the merge
// subsystem's rejection sentinel is translated into the ingress's, and a second
// copy of that translation — in a test harness or anywhere else — would be a
// second place for "rejected" and "structurally broken" to be told apart
// differently. Every assembly of the daemon, production and end-to-end alike,
// routes the inbox's merges through this one type.
//
// An UNBOUND dispatch is a construction-order bug, never a normal runtime state,
// so it fails structurally: the command file keeps its claim and the next scan
// retries it rather than the merge being quarantined as if the emitter were at
// fault.
type MergeDispatchBinding struct {
	Logf   func(string, ...any)
	target atomic.Pointer[MergeDispatch]
}

var _ workspacecreate.MergeDispatcher = (*MergeDispatchBinding)(nil)

// SetTarget binds the dispatch the inbox routes merges through.
func (b *MergeDispatchBinding) SetTarget(d *MergeDispatch) { b.target.Store(d) }

// DispatchMerge implements workspacecreate.MergeDispatcher.
//
// It translates the merge subsystem's rejection sentinel into the ingress's,
// which is what lets the inbox quarantine an unresolvable merge while still
// retrying a structural failure. The two sentinels stay separate because the
// packages are: workspacecreate does not import this one.
func (b *MergeDispatchBinding) DispatchMerge(ctx context.Context, cmd workspacecreate.MergeCommand) error {
	dispatch := b.target.Load()
	if dispatch == nil {
		if b.Logf != nil {
			b.Logf("server: merge dispatch used before SetTarget workspace=%q project_dir=%s — the daemon's merge surface is not wired yet", cmd.Workspace, cmd.ProjectDir)
		}
		return fmt.Errorf("server: merge dispatch binding not wired")
	}
	err := dispatch.DispatchMerge(ctx, cmd.Workspace, cmd.ProjectDir, cmd.ID)
	if errors.Is(err, ErrMergeWorkspaceUnrecorded) {
		return fmt.Errorf("%w: %v", workspacecreate.ErrUnknownMergeWorkspace, err)
	}
	return err
}
