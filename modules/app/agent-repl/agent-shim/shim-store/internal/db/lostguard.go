package db

import (
	"context"
	"database/sql"
)

// lostguard.go — a LOST terminal never lands on a run whose ending is on
// record.
//
// LOST IS "WE STOPPED SEEING IT". It is the sidecar's statement about a run it
// was watching when the run's file went quiet or vanished, and it is never true
// of a run that already ended on evidence: the run's own terminator, the
// vendor's notification, a person's stop, or an earlier LOST. On 2026-09-30 a
// restarted sidecar re-tracked a finished run and its LOST terminal superseded
// the real one, rewriting `detached_work.ended_at_ms` to the LOST instant
// (docs/protobuf-design/run-settlements.md). The sidecar now asks
// GetRunSettlements before tracking, so a LOST reaching an ended row here is
// an invariant violation: the batch is refused whole, the record stands, and
// the server records the refusal at ERROR under SiteLostOverSettled.

// lostTerminalRun reports the run a FILE-PLANE entry concludes LOST, or false
// when the entry is not a LOST terminal. The two LOST terminals a producer
// mints are a shell run's interrupted-by-lost bash frame and a backgrounded
// subagent's failed-by-lost spawn activity, the activity id of which IS the
// run by the cross-plane minting rule.
func lostTerminalRun(r routed) (string, bool) {
	if r.plane != planeFile {
		return "", false
	}
	if r.kind == kindBash {
		interrupted := r.bashRow.GetFrame().GetSuccess().GetInterrupted()
		if interrupted.GetLost() == nil {
			return "", false
		}
		return r.bashRow.GetRun().GetValue(), true
	}
	activity := r.pageLine.GetAgentItem().GetAgentFrame().GetUpdate().GetActivity()
	if activity.GetSubagent().GetFailure().GetLost() == nil {
		return "", false
	}
	return activity.GetActivityId().GetValue(), true
}

// refuseLostOverSettled refuses a file-plane LOST terminal for a run whose
// detached_work row has already ended. One indexed lookup on
// detached_work(origin_unit); every other entry passes untouched.
func (d *DB) refuseLostOverSettled(ctx context.Context, tx *sql.Tx, r routed) error {
	run, lost := lostTerminalRun(r)
	if !lost || run == "" {
		return nil
	}
	var endedAtMs int64
	switch err := tx.QueryRowContext(ctx, lostOverSettledSQL, run).Scan(&endedAtMs); {
	case isNoRows(err):
		return nil
	case err != nil:
		return storagef(err, "reading whether run %q has already ended", run)
	}
	return invalidSitef(SiteLostOverSettled, entryField(r.index, ""),
		"entries[%d] concludes run %q LOST, but the record holds it as ended at %d; LOST is never true of a run whose ending is on record, so the recorded terminal stands (write_id=%q)",
		r.index, run, endedAtMs, r.writeID)
}

// lostOverSettledSQL finds an ended row of the run. At package scope so the
// suite EXPLAINs the production text itself.
const lostOverSettledSQL = `SELECT ended_at_ms FROM detached_work
  WHERE origin_unit = ? AND ended_at_ms IS NOT NULL LIMIT 1`
