// Resolution of a workspace's WINDOW-shaped death cards on the successor's
// operational edge. deathwindow.go names which deaths those are and owns the
// stamp; this file owns the EDGE.
//
// A supersede is window-shaped and was recorded as if it were event-shaped.
// supersede.go marks the displaced record terminal with death_reason
// "superseded"; SessionViewFromRecord* re-derives a SystemFailureItem from that
// string on EVERY push, and the item carried resolved_at_ms=0 because nothing
// could ever write anything else. So the card reopened on every snapshot of
// every boot, for every session ever displaced, until the record aged out of
// TerminalRetention — a boot that restored a fortnight of workspaces presented
// 22 open blue cards about handovers completed days earlier.
//
// A DELETE had exactly the same defect and a louder symptom. DeleteSession
// marks the record terminal with death_reason "delete session" and nothing ever
// wrote a resolution, so the workspace's roster carried an open
// "the session was deleted" card forever — re-announced to the user on every
// snapshot, long after a successor session had taken the workspace and was
// serving turns on it.
//
// The sentence each card makes stops describing anything the moment the new
// session is genuinely up, and NOT before: at supersede time the successor has
// not even been minted (supersede runs ahead of newSessionID), so resolving
// there would close the card on a promise. The resolving edge is therefore the
// successor reaching OPERATIONAL, delivered through
// SessionRegistrar.SessionOperational from the bring-up gate — the same
// live-successor-healthy shape the withheld degradation cards settle on.
//
// The window is closed DURABLY, on the record, because the item is derived
// rather than retained: an in-memory resolution would be forgotten by the next
// push, never mind the next boot.
package server

import (
	"time"
)

// now resolves the registrar's clock, defaulting to the wall clock so a
// registrar built without one still stamps a real instant rather than zero —
// zero is the value that MEANS unresolved, so defaulting to it would make the
// stamp a silent no-op.
func (r *RegistryRegistrar) now() int64 {
	if r.Now != nil {
		return r.Now()
	}
	return time.Now().UnixMilli()
}

// SessionOperational stamps every OPEN window-shaped death on workspace and
// re-pushes each one's SessionView, so a frontend holding the open card
// receives the settled one under the same item uuid.
//
// sessionID — the session that just reached operational — is excluded on
// identity rather than assumed absent: a record cannot both be the live
// operational session and a terminal window-shaped one, and skipping it by name
// keeps that true even if some future path marks a record terminal while its
// controller is still coming up.
func (r *RegistryRegistrar) SessionOperational(workspace, sessionID string) {
	if r.Reg == nil || workspace == "" {
		return
	}
	at := r.now()
	for _, rec := range r.Reg.All() {
		if rec.CWD != workspace || rec.SessionID == sessionID || !resolvableDeath(rec) {
			continue
		}
		stamped, found, err := stampDeathResolution(r.Reg, rec.SessionID, at)
		if err != nil {
			if r.Logf != nil {
				r.Logf("server: session %s: death resolution write FAILED (ws %s reason %q) — its death card stays open and will reopen on the next boot: %v",
					rec.SessionID, workspace, rec.DeathReason, err)
			}
			continue
		}
		if !found {
			if r.Logf != nil {
				r.Logf("server: session %s: death resolution found no record (pruned between read and write)", rec.SessionID)
			}
			continue
		}
		if !stamped {
			continue
		}
		if r.Logf != nil {
			r.Logf("server: session %s: death RESOLVED at %d reason=%q decision=live_successor_healthy — %s reached operational on ws %s",
				rec.SessionID, at, rec.DeathReason, sessionID, workspace)
		}
		r.repush(rec.SessionID)
	}
}
