package sidebar

import (
	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/wsm"
)

// oneShot resolves the row's one-shot mark (frontend.v1.RosterRowOneShot): nil
// for a workspace that was not created as a one-shot, the mark otherwise. The
// mark says only that the workspace IS a one-shot; which arms draw its glyph is
// the shared vocabulary's `one_shot_glyph_arms` (owner ruling, 2026-10-13), so
// the daemon resolves no outcome for it.
func oneShot(rec wsm.Workspace) *frontendv1.RosterRowOneShot {
	if !rec.OneShot {
		return nil
	}
	return &frontendv1.RosterRowOneShot{}
}
