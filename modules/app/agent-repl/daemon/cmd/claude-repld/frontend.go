package main

import (
	"fmt"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/registry"
	"claude-repld/internal/server"
	"claude-repld/internal/session"
	"claude-repld/internal/sessiondrv"
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
//
// open IS live: server.WorkspaceOpener discovers the workspace's on-disk
// transcript, binds it as the resume target, and ensures the session eagerly so
// the workspace never renders blue with an empty feed. close remains unexposed
// (the workspacecmd channel carries no close entry) and fails loudly from
// WorkspaceOpener.Close.
//
// registrySessions (the SessionView metadata source) IS real: it reads the
// persistent registry, so snapshots carry live per-session model/workspace.

// registrySessions supplies SessionView metadata from the persistent registry
// plus the driver's live pending-permission counts. It reads the SAME registry
// GET /sessions did, and now carries the S7 parity fields (terminal,
// death_reason, pending_permissions) so Emacs can drop the HTTP poller: TERMINAL
// records are included too (the orphan/reattach sweep re-keys on them), whereas
// a workspace-less record (empty cwd) has no workspace to key by and is skipped.
type registrySessions struct {
	reg    *registry.Registry
	driver *sessiondrv.Manager
	// logf carries the death-reason classifier's loud default for a record
	// written by a build that predates the failure vocabulary. Nil is
	// tolerated (the classifier checks) so a unit harness need not supply one.
	logf dlog.Logf
}

func (r registrySessions) SessionViews() []*frontendv1.SessionView {
	var out []*frontendv1.SessionView
	for _, rec := range r.reg.All() {
		if rec.CWD == "" {
			continue
		}
		// Live pending-permission ids only for a non-terminal session with a
		// driver; a terminal/hibernated one has none. Slug/title stay blank
		// (they arrive from ai-title/slug events the SSM does not retain), never
		// faked. server.SessionViewFromRecord is the single shaping shared with
		// the create/delete pushes, so the snapshot and pushes cannot drift.
		var pending []string
		live := false
		if !rec.Terminal && r.driver != nil {
			pending = r.driver.PendingPermissions(rec.CWD)
			// The connect snapshot is precisely where this matters: it is the
			// first thing a frontend sees after a daemon restart, and it is
			// what a switch-ensure consults before deciding a workspace has
			// nothing to bootstrap.
			live = r.driver.Live(rec.CWD)
		}
		out = append(out, server.SessionViewFromRecord(r.logf, rec, pending, live))
	}
	return out
}

// pendingMergeDirs is the not-yet-resolved MergeDirResolver: the daemon has no
// daemon-side source for a workspace's source/target worktrees and branch yet.
type pendingMergeDirs struct{}

func (pendingMergeDirs) Resolve(workspace string) (merge.Request, error) {
	return merge.Request{}, fmt.Errorf("merge dir resolution not wired for workspace %q: the workspace->worktree/branch mapping is not yet exposed daemon-side (§9.3)", workspace)
}

// knownConfigDirs is every Claude config root the daemon knows: the account
// roster's dirs plus the daemon-wide default. Transcript discovery probes all
// of them so a conversation living under another account is REPORTED as a
// migration candidate rather than silently missed.
func knownConfigDirs(accounts []server.Account) func() []string {
	return func() []string {
		dirs := []string{session.DefaultClaudeConfigDir()}
		for _, a := range accounts {
			dirs = append(dirs, session.ClaudeConfigDir(a.ConfigDir))
		}
		return dirs
	}
}
