// Boot reconciliation for WINDOW-shaped death cards left standing by a previous
// daemon. See deathwindow.go for which deaths those are, and deathresolve.go for
// why the live resolution rides the successor's operational edge.
package server

import (
	"time"

	"claude-repld/internal/registry"
)

// ReconcileOpenDeaths stamps every OPEN window-shaped death left standing in
// reg, and reports how many it closed. It is the boot half of the fix: records
// written before the resolution existed carry no stamp and no successor edge is
// ever coming for them, so without this the accumulated history stays open
// forever — which is exactly how a workspace whose session was deleted weeks ago
// went on re-announcing "the session was deleted" at every snapshot.
//
// EVERY open one, unconditionally, because both window-shaped deaths are claims
// about something INSIDE ONE DAEMON LIFETIME. A supersede's two parties — the
// session that was stood down and the session that took the workspace — are
// gone by the time a new daemon reads the registry, so the claim can no longer
// be about anything live. A delete is the user's own completed action, and it
// was over before the previous daemon exited. A workspace whose successor
// genuinely failed to come up is blue for that reason and says so through its
// own bring-up failure; borrowing these cards to say it a second time is the
// noise this removes.
//
// It runs at boot ONLY, so it can never pre-resolve a supersede or a delete the
// running daemon just performed: within a lifetime, resolution comes solely
// from the successor reaching operational, or from the delete stamping its own
// death as it mints it.
//
// HISTORY IS KEPT. The record, its terminal flag and its death reason are
// untouched; only the resolution instant is added, so every dead session stays
// queryable exactly as before and merely stops presenting as open.
func ReconcileOpenDeaths(reg *registry.Registry, now func() int64, logf func(string, ...any)) int {
	if reg == nil {
		return 0
	}
	at := time.Now().UnixMilli()
	if now != nil {
		at = now()
	}
	resolved := 0
	for _, rec := range reg.All() {
		if !resolvableDeath(rec) {
			continue
		}
		stamped, found, err := stampDeathResolution(reg, rec.SessionID, at)
		if err != nil {
			if logf != nil {
				logf("server: session %s: boot death reconciliation write FAILED (reason %q) — its death card stays open: %v", rec.SessionID, rec.DeathReason, err)
			}
			continue
		}
		if !found {
			if logf != nil {
				logf("server: session %s: boot death reconciliation found no record", rec.SessionID)
			}
			continue
		}
		if stamped {
			resolved++
		}
	}
	if logf != nil {
		logf("server: boot death reconciliation resolved %d stale window-shaped death card(s) at %d", resolved, at)
	}
	return resolved
}
