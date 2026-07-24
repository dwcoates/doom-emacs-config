package main

import (
	"context"
	"fmt"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/registry"
	"claude-repld/internal/workspace/merge"
)

// This file holds the daemon-side production backends for the frontend.v1
// command surface (server.WireAgentShim). The SSM-backed state snapshot, the
// merge-transition push loop, and the prompt/interrupt/permission routing (the
// per-session driver, wired in main) are live; the command backends below are
// the stitch-phase seams for work that lands with the COUPLED parallel tasks,
// and each fails LOUDLY rather than silently no-opping (no-fallbacks rule):
//
//   - merge needs the workspace -> (source/target worktree, branch) resolution
//     that lives in the Emacs worktree layout; the daemon has no daemon-side
//     source for it yet (§9.3 open question).
//   - close/open have no entry in the workspacecmd channel (create/switch/fold
//     /task-* only); the Emacs lifecycle verbs are not yet exposed to the
//     daemon.
//
// registrySessions (the SessionView metadata source) IS real: it reads the
// persistent registry, so snapshots carry live per-session model/workspace.

// registrySessions supplies SessionView metadata from the persistent registry.
type registrySessions struct{ reg *registry.Registry }

func (r registrySessions) SessionViews() []*frontendv1.SessionView {
	var out []*frontendv1.SessionView
	for _, rec := range r.reg.All() {
		if rec.Terminal || rec.CWD == "" {
			continue
		}
		// Populate the SessionView from the registry record: model +
		// permission mode, plus the claude_session_id and cwd proto fields
		// (design §14.2 step 3). Slug/title are not carried in the registry
		// record (they arrive from ai-title/slug events the SSM does not yet
		// retain), so they stay blank here rather than being faked.
		out = append(out, &frontendv1.SessionView{
			Workspace:       rec.CWD,
			SessionId:       rec.SessionID,
			Model:           rec.Model,
			PermissionMode:  rec.PermissionMode,
			ClaudeSessionId: rec.ClaudeSessionID,
			Cwd:             rec.CWD,
		})
	}
	return out
}

// pendingMergeDirs is the not-yet-resolved MergeDirResolver: the daemon has no
// daemon-side source for a workspace's source/target worktrees and branch yet.
type pendingMergeDirs struct{}

func (pendingMergeDirs) Resolve(workspace string) (merge.Request, error) {
	return merge.Request{}, fmt.Errorf("merge dir resolution not wired for workspace %q: the workspace->worktree/branch mapping is not yet exposed daemon-side (§9.3)", workspace)
}

// pendingLifecycle is the not-yet-exposed WorkspaceLifecycle: the workspacecmd
// channel has no close/open entry type.
type pendingLifecycle struct{}

func (pendingLifecycle) Close(_ context.Context, workspace string) error {
	return fmt.Errorf("close-workspace not exposed daemon-side for %q (no workspacecmd entry)", workspace)
}
func (pendingLifecycle) Open(_ context.Context, workspace string) error {
	return fmt.Errorf("open-workspace not exposed daemon-side for %q (no workspacecmd entry)", workspace)
}
