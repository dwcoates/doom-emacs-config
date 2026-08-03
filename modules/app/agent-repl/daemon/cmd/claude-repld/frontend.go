package main

import (
	"claude-repld/internal/server"
	"claude-repld/internal/session"
)

// This file holds the daemon-side production backends for the frontend.v1
// command surface (server.WireAgentShim). The SSM-backed state snapshot, the
// merge-transition push loop, and the prompt/interrupt/permission routing (the
// per-session controller, wired in main) are live; the command backends below are
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
// The SessionView metadata source (server.RegistrySessions) IS real: it reads
// the persistent registry, so snapshots carry live per-session model/workspace.
// It lives in internal/server so main and the boot harnesses share ONE shaping
// of it rather than each growing a registry walk of its own.

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
