// registrysessions.go is the SessionView metadata source every daemon boot
// wires into the connect snapshot.
//
// It lives here rather than in cmd/claude-repld because it is not main's
// private business: the boot harnesses stand up the same daemon and need the
// same snapshot, and two registry walks shaped independently would drift into
// two different answers to "what sessions exist".
package server

import (
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/registry"
	"claude-repld/internal/sessioncontroller"
)

// RegistrySessions supplies SessionView metadata from the persistent registry
// plus the session controller's live pending-permission counts. It reads the
// SAME registry GET /sessions did, and carries the S7 parity fields (terminal,
// death_reason, pending_permissions) so Emacs can drop the HTTP poller:
// TERMINAL records are included too (the orphan/reattach sweep re-keys on
// them), whereas a workspace-less record (empty cwd) has no workspace to key by
// and is skipped.
type RegistrySessions struct {
	Reg           *registry.Registry
	Controller    *sessioncontroller.Manager
	ModelCatalogs *SessionModelCatalogs
	TokenUsage    SessionTokenUsageSource
	// Logf carries the death-reason classifier's loud default for a record
	// written by a build that predates the failure vocabulary. Nil is
	// tolerated (the classifier checks) so a unit harness need not supply one.
	Logf dlog.Logf
}

func (r RegistrySessions) SessionViews() []*frontendv1.SessionView {
	var out []*frontendv1.SessionView
	for _, rec := range r.Reg.All() {
		if rec.CWD == "" {
			continue
		}
		// Live pending-permission ids only for a non-terminal session with a
		// controller; a terminal/hibernated one has none. Slug/title stay blank
		// (they arrive from ai-title/slug events the SSM does not retain), never
		// faked. SessionViewFromRecord is the single shaping shared with the
		// create/delete pushes, so the snapshot and pushes cannot drift.
		var pending []string
		live := false
		if !rec.Terminal && r.Controller != nil {
			pending = r.Controller.PendingPermissions(rec.CWD)
			// The connect snapshot is precisely where this matters: it is the
			// first thing a frontend sees after a daemon restart, and it is
			// what a switch-ensure consults before deciding a workspace has
			// nothing to bootstrap.
			live = r.Controller.Live(rec.CWD)
		}
		var modelOptions []*frontendv1.ModelOption
		if r.ModelCatalogs != nil {
			modelOptions = r.ModelCatalogs.Get(rec.SessionID)
		}
		out = append(out, SessionViewFromRecordWithModelsAndUsage(r.Logf, r.Reg, rec, pending, live, modelOptions, sessionTokenUtilization(r.Logf, r.TokenUsage, rec.SessionID)))
	}
	return out
}
