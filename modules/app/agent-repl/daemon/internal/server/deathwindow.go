// The WINDOW-shaped session deaths and the one way their window is closed.
//
// A death card is derived from the registry record on every push
// (SessionViewFromRecord -> deathForView -> errclass.Death), so the ONLY place a
// resolution can live is the record itself: an in-memory settlement would be
// forgotten by the next push, never mind the next boot. This file holds the
// closed set of reasons that HAVE a closing edge, and the single write that
// stamps one.
//
// # Which deaths are window-shaped
//
// A supersede says "a new Claude session was started for this workspace, so
// this session was stopped". A delete says "the session was deleted". Both stop
// describing anything the moment the workspace has a live session again — the
// first because the handover it announced completed, the second because the
// user's own destruction succeeded and the workspace moved on.
//
// A shim death is NOT here. "The agent process exited" stays true no matter
// what comes up afterwards, and settling it because a successor wired would be
// the daemon closing a card it never disproved.
//
// # Why the stamp is one function
//
// Three edges close these windows — the successor reaching operational
// (deathresolve.go), boot reconciliation of records a previous daemon left
// standing (deathreconcile.go), and the delete that mints its own death already
// settled (server.go DeleteSession). Each owes the same read-modify-write under
// the registry's own Update, re-checking the record INSIDE the callback so a
// concurrent writer cannot be overstamped. Three copies of that would drift,
// so there is one.
package server

import (
	"claude-repld/internal/errclass"
	"claude-repld/internal/registry"
)

// windowResolvableDeathReasons is the closed set of persisted death reasons a
// workspace's live successor disproves. See the file header for why a shim
// death is deliberately absent.
var windowResolvableDeathReasons = map[string]struct{}{
	supersedeReason:             {},
	errclass.DeathReasonDeleted: {},
}

// resolvableDeath reports whether rec is an OPEN window-shaped death — the only
// shape any resolver may stamp. Terminal-with-a-reason is not enough: a shim
// death is terminal too, and nothing later makes it untrue.
func resolvableDeath(rec registry.Record) bool {
	if !rec.Terminal || rec.DeathResolvedAtMs != 0 {
		return false
	}
	_, window := windowResolvableDeathReasons[rec.DeathReason]
	return window
}

// resolvableSupersede reports whether rec is an OPEN SUPERSEDED death. It is
// narrower than resolvableDeath on purpose: the presentation withhold rule
// (supersedepresent.go) suppresses a handover in flight and nothing else, so it
// asks about the supersede specifically rather than about window-shaped deaths
// in general.
func resolvableSupersede(rec registry.Record) bool {
	return rec.Terminal && rec.DeathReason == supersedeReason && rec.DeathResolvedAtMs == 0
}

// stampDeathResolution closes sessionID's open death window at instant at.
//
// stamped is false — with no error — when the record is no longer resolvable by
// the time the callback runs: it was pruned, or another edge settled it first.
// Two recovery events can legitimately race for one record, and the loser must
// not re-stamp a later instant onto a settlement that already happened.
//
// found reports whether the registry still holds the record at all, so a caller
// can tell "someone else closed it" from "it is gone" rather than conflating
// the two into one silent skip.
func stampDeathResolution(reg *registry.Registry, sessionID string, at int64) (stamped, found bool, err error) {
	if reg == nil {
		return false, false, nil
	}
	found, err = reg.Update(sessionID, func(cur *registry.Record) {
		if !resolvableDeath(*cur) {
			return
		}
		cur.DeathResolvedAtMs = at
		stamped = true
	})
	return stamped, found, err
}
