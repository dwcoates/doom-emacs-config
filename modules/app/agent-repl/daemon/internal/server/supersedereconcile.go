// Boot reconciliation for superseded death cards left standing by a previous
// daemon. See supersederesolve.go for why the card is window-shaped and why the
// live resolution rides the successor's operational edge.
package server

import (
	"time"

	"claude-repld/internal/registry"
)

// ReconcileSupersededDeaths stamps every OPEN superseded death left standing in
// reg, and reports how many it closed. It is the boot half of the fix: records
// written before the resolution existed carry no stamp and no successor edge is
// ever coming for them, so without this the accumulated history stays open
// forever.
//
// EVERY open one, unconditionally, because a supersede is a claim about a
// handover INSIDE ONE DAEMON LIFETIME. Both parties to it — the session that
// was stood down and the session that took the workspace — are gone by the time
// a new daemon reads the registry, so the claim can no longer be about anything
// live. A workspace whose successor genuinely failed to come up is blue for
// that reason and says so through its own bring-up failure; borrowing the
// supersede card to say it a second time is the noise this removes.
//
// It runs at boot ONLY, so it can never pre-resolve a supersede the running
// daemon just performed: within a lifetime, resolution comes solely from the
// successor reaching operational.
//
// HISTORY IS KEPT. The record, its terminal flag and its death reason are
// untouched; only the resolution instant is added, so every superseded session
// stays queryable exactly as before and merely stops presenting as open.
func ReconcileSupersededDeaths(reg *registry.Registry, now func() int64, logf func(string, ...any)) int {
	if reg == nil {
		return 0
	}
	at := time.Now().UnixMilli()
	if now != nil {
		at = now()
	}
	resolved := 0
	for _, rec := range reg.All() {
		if !resolvableSupersede(rec) {
			continue
		}
		stamped := false
		found, err := reg.Update(rec.SessionID, func(cur *registry.Record) {
			if !resolvableSupersede(*cur) {
				return
			}
			cur.DeathResolvedAtMs = at
			stamped = true
		})
		if err != nil {
			if logf != nil {
				logf("server: session %s: boot supersede reconciliation write FAILED — its death card stays open: %v", rec.SessionID, err)
			}
			continue
		}
		if !found {
			if logf != nil {
				logf("server: session %s: boot supersede reconciliation found no record", rec.SessionID)
			}
			continue
		}
		if stamped {
			resolved++
		}
	}
	if logf != nil {
		logf("server: boot supersede reconciliation resolved %d stale superseded death card(s) at %d", resolved, at)
	}
	return resolved
}
