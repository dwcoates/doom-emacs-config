// Presentation of the superseded-death card while its handover is still in
// flight.
//
// supersederesolve.go closes the window DURABLY, and closes it on the only
// honest edge: the successor reaching operational. That is right, and this file
// does not move it. But it leaves a real gap on the wire. `supersede` runs
// inside Create, ahead of the successor's bring-up; the successor reaches
// OPERATIONAL only once its shim handshakes. Every push and every connect
// snapshot taken between those two instants re-derives the predecessor's death
// from the registry with resolved_at_ms=0, so a frontend that connects — or a
// workspace whose roster is re-pushed — during that gap is handed an OPEN
// "a new Claude session was started for this workspace, so this session was
// stopped" card about a handover that is proceeding exactly as intended.
//
// The supersede is the ordinary mechanics of a workspace continuing its one
// conversation. There is nothing for the user to do about it, and nothing for
// them to know, UNLESS the succession does not happen. So the card is withheld
// from the wire for exactly as long as a successor is still claiming the
// workspace, and for no longer:
//
//   - claiming successor present -> nothing to report; the death item is not
//     shaped onto the view at all.
//   - successor reaches operational -> supersederesolve.go stamps the record
//     and re-pushes; the card was never open, and now never will be.
//   - successor dies or is stood down before it is operational -> its record
//     goes terminal, the claim is gone, and the predecessor's OPEN death is
//     presented again on the very next push, unchanged.
//   - no successor at all (an orphaned supersede) -> presented immediately.
//
// THE RECORD IS NOT TOUCHED. This is a presentation rule, not a resolution:
// nothing here writes death_resolved_at_ms, so the durable claim "this window
// is still open" stays true and the only writers of its close remain the
// successor's operational edge and the boot reconciliation. Suppressing on the
// wire while lying on the record would have made the two disagree forever.
package server

import (
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/errclass"
	"claude-repld/internal/registry"
)

// supersedeClaimed reports whether workspace still has a live claim from some
// session other than sessionID — a non-terminal record on the same cwd.
//
// A non-terminal record on the cwd IS the successor and cannot be anything
// else: supersedeCreateConflicts stands down EVERY non-terminal record on a cwd
// when a newer one takes it, so at most one such record survives, and it is by
// construction the session that displaced sessionID (or a later one that
// displaced that). No recency comparison is needed, and none is done — record
// ordering is not a fact the registry guarantees.
//
// A record with an empty cwd claims no workspace, and workspace is required:
// a workspace-less supersede (the degenerate transcript-only stand-down) has no
// cwd to find a successor on, so it is never suppressed.
func supersedeClaimed(reg *registry.Registry, workspace, sessionID string) bool {
	if reg == nil || workspace == "" {
		return false
	}
	for _, rec := range reg.All() {
		if rec.Terminal || rec.SessionID == sessionID || rec.CWD != workspace {
			continue
		}
		return true
	}
	return false
}

// deathForView shapes the death item a SessionView carries for rec, withholding
// an OPEN superseded death while reg shows a claiming successor for the same
// workspace.
//
// It is the SINGLE derivation point for `SessionView.death`, so the connect
// snapshot and the per-session pushes cannot present a record differently —
// which is the same reason SessionViewFromRecord* is a single shaping.
//
// Only the superseded window is withheld. A delete and a shim death are
// EVENT-shaped: nothing that happens later makes them untrue, so a successor
// says nothing about them and they pass through whatever reg holds.
//
// A NIL reg withholds NOTHING. No registry means no way to know whether a
// successor is claiming the workspace, and the safe direction for an unknown is
// to report the failure rather than hide it — an unshaped harness or a caller
// without registry context gets the recorded death verbatim, exactly as before.
func deathForView(logf dlog.Logf, reg *registry.Registry, rec registry.Record) *frontendv1.FailureCardView {
	if resolvableSupersede(rec) && supersedeClaimed(reg, rec.CWD, rec.SessionID) {
		if logf != nil {
			logf("session %s: superseded death WITHHELD from its SessionView (ws %s) — a successor still claims the workspace, so the handover is in flight rather than failed; it is presented again if that claim goes away, and resolved durably when the successor reaches operational",
				rec.SessionID, rec.CWD)
		}
		return nil
	}
	return errclass.Death(logf, rec.SessionID, rec.DeathReason, rec.DeathResolvedAtMs)
}
