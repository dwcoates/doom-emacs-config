// workspaceopen.go implements the NEVER-BLUE contract: a workspace whose
// conversation already exists on disk must never render as a fresh, empty,
// blue workspace.
//
// Two things used to break that. First, a registry record can carry no
// `claude_session_id` — it is only filled in when a live shim reports one, so a
// record created before the shim ever ran (or one whose daemon died before
// system:init) has no resume target even though the CLI wrote a whole
// transcript for that cwd. Second, nothing ever brought a session up on
// workspace open: bring-up waited for the first submit, so a workspace the user
// merely LOOKED at stayed unrendered until they typed.
//
// WorkspaceOpener closes both. Discovery maps a session's cwd back to its
// `<config-dir>/projects/<encoded-cwd>` directory, takes the newest
// `<uuid>.jsonl` there, and binds that uuid into the registry when the record
// lacks one. Open then ensures the session immediately, which spawns the shim
// with `--resume <uuid>`; an SDK resume spawn is local until a prompt is sent,
// so nothing is charged and nothing is said on the user's behalf. The shim's
// SessionStarted lands, the SSM resolves the workspace to idle, and the
// sidecar's cursor-less first read of that transcript backfills its history
// into the store (dedup absorbs whatever the stream plane already persisted).
//
// A transcript found under a DIFFERENT config dir than the session's own is
// NEVER adopted. Binding it would resume a conversation the session's CLI root
// cannot see, so it is loud-logged as a migration candidate and left alone.
package server

import (
	"context"
	"fmt"

	"claude-repld/internal/registry"
	"claude-repld/internal/session"
)

// WorkspaceEnsurer brings a workspace's session up without submitting a
// prompt. Satisfied by *sessiondrv.Manager.
type WorkspaceEnsurer interface {
	Ensure(workspace string) error
}

// WorkspaceOpener is the production WorkspaceLifecycle.
type WorkspaceOpener struct {
	// Reg is the persistent session registry (required).
	Reg *registry.Registry
	// Ensurer brings a workspace's session up (required for Open).
	Ensurer WorkspaceEnsurer
	// ConfigDirs lists every Claude config root the daemon knows about. The
	// session's own dir is probed first; the rest only ever produce migration
	// candidates.
	ConfigDirs func() []string
	// Logf is the loud logger (required).
	Logf func(string, ...any)
}

var _ WorkspaceLifecycle = (*WorkspaceOpener)(nil)

// Open binds a discovered transcript to the workspace's session when it has
// none, then ensures the session so the workspace renders its history instead
// of sitting blue and empty.
func (o *WorkspaceOpener) Open(_ context.Context, workspace string) error {
	if o.Ensurer == nil {
		return fmt.Errorf("server: open-workspace %q has no session ensurer wired", workspace)
	}
	o.BindWorkspace(workspace)
	return o.Ensurer.Ensure(workspace)
}

// Close is still not exposed daemon-side: the workspacecmd channel carries no
// close entry, and the Emacs lifecycle owns teardown. Failing loudly is the
// honest answer (no-fallbacks rule); it is NOT a silent no-op.
func (o *WorkspaceOpener) Close(_ context.Context, workspace string) error {
	return fmt.Errorf("close-workspace not exposed daemon-side for %q (no workspacecmd entry)", workspace)
}

// BindAll runs discovery over every registered, non-terminal workspace. Called
// at daemon boot so a restart re-binds resume targets before any frontend
// connects, rather than waiting for each workspace to be opened.
func (o *WorkspaceOpener) BindAll() {
	if o.Reg == nil {
		return
	}
	bound := 0
	for _, rec := range o.Reg.All() {
		if rec.Terminal || rec.CWD == "" || rec.ClaudeSessionID != "" {
			continue
		}
		if o.bindRecord(rec) {
			bound++
		}
	}
	if bound > 0 {
		o.Logf("server: boot transcript discovery bound %d session(s) to an on-disk conversation", bound)
	}
}

// BindWorkspace runs discovery for one workspace's newest non-terminal
// session. Reports whether a vendor session id was newly bound.
func (o *WorkspaceOpener) BindWorkspace(workspace string) bool {
	if o.Reg == nil || workspace == "" {
		return false
	}
	id, ok := (&SessionLocator{Reg: o.Reg}).Locate(workspace)
	if !ok {
		// No record at all: nothing to bind a transcript ONTO. Loud, because a
		// workspace with a transcript and no record is exactly the state that
		// renders blue, and the caller cannot see this from the Ensure error.
		o.Logf("server: open-workspace %q has no registry record; transcript discovery skipped", workspace)
		return false
	}
	rec, found := o.Reg.Get(id)
	if !found {
		o.Logf("server: open-workspace %q located session %s but its record vanished", workspace, id)
		return false
	}
	return o.bindRecord(rec)
}

// bindRecord probes for rec's transcript and binds the discovered vendor
// session id when the record lacks one. Reports whether it bound.
func (o *WorkspaceOpener) bindRecord(rec registry.Record) bool {
	ownDir := session.ClaudeConfigDir(rec.ConfigDir)
	var dirs []string
	if o.ConfigDirs != nil {
		dirs = o.ConfigDirs()
	}
	d, err := session.Discover(ownDir, dirs, rec.CWD)
	if err != nil {
		o.Logf("server: transcript discovery for %s (cwd %s) FAILED: %v", rec.SessionID, rec.CWD, err)
		return false
	}
	for _, m := range d.Migrations {
		// Never adopted: see the file header. Named loudly so a human can move
		// or re-account the conversation deliberately.
		o.Logf("server: MIGRATION CANDIDATE — session %s (cwd %s, config dir %s) has a transcript under a DIFFERENT config dir %s (%s); NOT adopted",
			rec.SessionID, rec.CWD, ownDir, m.ConfigDir, m.Path)
	}
	if rec.ClaudeSessionID != "" {
		return false // already bound; discovery was only for the migration report
	}
	if !d.Found {
		return false
	}
	if _, err := o.Reg.Update(rec.SessionID, func(r *registry.Record) { r.ClaudeSessionID = d.Adopted.SessionID }); err != nil {
		o.Logf("server: binding transcript %s to session %s FAILED: %v", d.Adopted.Path, rec.SessionID, err)
		return false
	}
	o.Logf("server: session %s (cwd %s) had no vendor session id; bound the newest on-disk transcript %s (%s)",
		rec.SessionID, rec.CWD, d.Adopted.SessionID, d.Adopted.Path)
	return true
}
