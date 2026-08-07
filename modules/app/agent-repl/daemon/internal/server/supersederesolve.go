// Resolution of the superseded-session death card.
//
// A supersede is WINDOW-shaped and was recorded as if it were EVENT-shaped.
// supersede.go marks the displaced record terminal with death_reason
// "superseded"; SessionViewFromRecord* re-derives a SystemFailureItem from that
// string on EVERY push, and the item carried resolved_at_ms=0 because nothing
// could ever write anything else. So the card reopened on every snapshot of
// every boot, for every session ever displaced, until the record aged out of
// TerminalRetention — a boot that restored a fortnight of workspaces presented
// 22 open blue cards about handovers completed days earlier.
//
// The sentence the card makes is "a new Claude session was started for this
// workspace, so this session was stopped". That sentence stops describing
// anything the moment the new session is genuinely up, and NOT before: at
// supersede time the successor has not even been minted (supersede runs ahead
// of newSessionID), so resolving there would close the card on a promise. The
// resolving edge is therefore the successor reaching OPERATIONAL, delivered
// through SessionRegistrar.SessionOperational from the bring-up gate.
//
// The window is closed DURABLY, on the record, because the item is derived
// rather than retained: an in-memory resolution would be forgotten by the next
// push, never mind the next boot.
package server

import (
	"time"

	"claude-repld/internal/registry"
)

// resolvableSupersede reports whether rec is an OPEN superseded death — the
// only shape either resolver may stamp. Terminal-with-a-reason is not enough:
// a deleted session is terminal too, and its death is permanent.
func resolvableSupersede(rec registry.Record) bool {
	return rec.Terminal && rec.DeathReason == supersedeReason && rec.DeathResolvedAtMs == 0
}

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

// SessionOperational stamps every OPEN superseded death on workspace and
// re-pushes each one's SessionView, so a frontend holding the open card
// receives the settled one under the same item uuid.
//
// sessionID — the session that just reached operational — is excluded on
// identity rather than assumed absent: a record cannot both be the live
// operational session and a terminal superseded one, and skipping it by name
// keeps that true even if some future path marks a record terminal while its
// controller is still coming up.
func (r *RegistryRegistrar) SessionOperational(workspace, sessionID string) {
	if r.Reg == nil || workspace == "" {
		return
	}
	at := r.now()
	for _, rec := range r.Reg.All() {
		if rec.CWD != workspace || rec.SessionID == sessionID || !resolvableSupersede(rec) {
			continue
		}
		stamped := false
		found, err := r.Reg.Update(rec.SessionID, func(cur *registry.Record) {
			if !resolvableSupersede(*cur) {
				return
			}
			cur.DeathResolvedAtMs = at
			stamped = true
		})
		if err != nil {
			if r.Logf != nil {
				r.Logf("server: session %s: supersede resolution write FAILED (ws %s) — its death card stays open and will reopen on the next boot: %v",
					rec.SessionID, workspace, err)
			}
			continue
		}
		if !found {
			if r.Logf != nil {
				r.Logf("server: session %s: supersede resolution found no record (pruned between read and write)", rec.SessionID)
			}
			continue
		}
		if !stamped {
			continue
		}
		if r.Logf != nil {
			r.Logf("server: session %s: supersede RESOLVED at %d — %s reached operational on ws %s",
				rec.SessionID, at, sessionID, workspace)
		}
		r.repush(rec.SessionID)
	}
}
