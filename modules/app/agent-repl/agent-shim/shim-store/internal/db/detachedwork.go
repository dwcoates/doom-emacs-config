package db

import (
	"context"
	"database/sql"
	"errors"

	storev1 "agentrepl/proto/store/v1"
	"agentrepl/shim-store/internal/logging"
)

// detachedwork.go — WHAT KIND of detached work one unit left as, and whether
// the record holds it as ENDED (store.v1 GetDetachedWork).
//
// WHY IT EXISTS. A task-stream `task_notification` never states its task's
// kind, and the shim learns one only from the start it observed. A shim that
// restarted, or a keep-alive rewind whose new vendor query re-reports an old
// backgrounded shell as stopped, met a notification it could not type and
// recorded an ERROR for every one (2026-10-02 onward, workspace ship-gns). The
// run's `detached_work` row holds the kind and the end, so that is what answers.
//
// LOCATED BY THE ORIGIN UNIT, the spawning call's activity id, exactly as
// GetRunSettlements locates a run: it is the identity every task message of the
// run carries, and the column is indexed. The table holds one row per origin by
// construction (resolveDetachedRowKey); two is a broken invariant, refused as
// a storage failure rather than chosen between.
//
// THE KIND IS SERVED AS RECORDED. The `detached` marker (an announcement that
// continued a unit and stated no kind the store classifies) is the `unstated`
// arm, never a guess; a kind the store never writes is a corrupt record.

// detachedWorkByUnitSQL binds the origin unit. At package scope so the suite
// EXPLAINs the production text itself.
const detachedWorkByUnitSQL = `
	  SELECT work_id, kind, ended_at_ms FROM detached_work
	  WHERE origin_unit = ?
	  ORDER BY work_id ASC`

// errAmbiguousDetachedUnit is the cause of a lookup that found two rows located
// by one origin unit — a break in the table's own invariant.
var errAmbiguousDetachedUnit = errors.New("one origin unit locates more than one detached work row")

// errUnknownDetachedKind is the cause of a row whose recorded kind is none the
// store writes — a corrupt record.
var errUnknownDetachedKind = errors.New("a detached work row records a kind this store never writes")

// recordedKind is the wire form of one recorded kind, or false for a kind the
// store never writes.
func recordedKind(kind string) (*storev1.GetDetachedWorkKind, bool) {
	switch kind {
	case detachedKindSubagent:
		return &storev1.GetDetachedWorkKind{Kind: &storev1.GetDetachedWorkKind_Subagent{Subagent: &storev1.GetDetachedWorkKindSubagent{}}}, true
	case detachedKindBash:
		return &storev1.GetDetachedWorkKind{Kind: &storev1.GetDetachedWorkKind_Bash{Bash: &storev1.GetDetachedWorkKindBash{}}}, true
	case detachedKindWorkflow:
		return &storev1.GetDetachedWorkKind{Kind: &storev1.GetDetachedWorkKind_Workflow{Workflow: &storev1.GetDetachedWorkKindWorkflow{}}}, true
	case detachedKindMonitor:
		return &storev1.GetDetachedWorkKind{Kind: &storev1.GetDetachedWorkKind_Monitor{Monitor: &storev1.GetDetachedWorkKindMonitor{}}}, true
	case detachedKindDetached:
		return &storev1.GetDetachedWorkKind{Kind: &storev1.GetDetachedWorkKind_Unstated{Unstated: &storev1.GetDetachedWorkKindUnstated{}}}, true
	default:
		return nil, false
	}
}

// detachedWorkRow is one row the lookup read.
type detachedWorkRow struct {
	workID string
	kind   string
	ended  sql.NullInt64
}

// DetachedWorkByUnit answers what the record holds of the detached work that
// left `unit` (the spawning call's activity id): its kind and whether it ended.
//
// THREE ANSWERS. One row: the success, recorded at info. No row: `found` false
// with no error — an ordinary answer, recorded at info. Two or more, or a kind
// the store never writes: a storage failure recorded at ERROR. An empty unit is
// refused before any statement runs.
func (d *DB) DetachedWorkByUnit(ctx context.Context, unit string) (*storev1.GetDetachedWorkSuccess, bool, error) {
	base := logging.Fields{Operation: "store.db.detached-work-by-unit", Table: "detached_work", ActivityID: unit}
	if unit == "" {
		return nil, false, d.refuse(base, invalidFieldf("unit", "unit is empty — the lookup names the unit the work detached from"))
	}
	started := d.mono()
	rows, err := d.read.QueryContext(ctx, detachedWorkByUnitSQL, unit)
	if err != nil {
		return nil, false, d.refuse(base, storagef(err, "reading the detached work of unit %s", unit))
	}
	defer rows.Close() //nolint:errcheck // the deferred close of a read
	var found []detachedWorkRow
	for rows.Next() {
		var row detachedWorkRow
		if err := rows.Scan(&row.workID, &row.kind, &row.ended); err != nil {
			return nil, false, d.refuse(base, storagef(err, "reading a detached work row of unit %s", unit))
		}
		found = append(found, row)
	}
	if err := rows.Err(); err != nil {
		return nil, false, d.refuse(base, storagef(err, "reading the detached work of unit %s", unit))
	}
	d.observeQuery(StatementDetachedWorkByUnit, "detached_work", base, started, int64(len(found)))
	d.traceStatement(ctx, StatementDetachedWorkByUnit, "detached_work", base, int64(len(found)))
	switch len(found) {
	case 0:
		d.log.Log(base, "no detached work on record left unit %s", unit)
		return nil, false, nil
	case 1:
	default:
		return nil, false, d.refuse(base, storagef(errAmbiguousDetachedUnit,
			"unit %s locates %d detached work rows", unit, len(found)))
	}
	row := found[0]
	kind, ok := recordedKind(row.kind)
	if !ok {
		return nil, false, d.refuse(base, storagef(errUnknownDetachedKind,
			"detached work %s of unit %s records kind %q", row.workID, unit, row.kind))
	}
	success := &storev1.GetDetachedWorkSuccess{Kind: kind}
	if row.ended.Valid {
		success.State = &storev1.GetDetachedWorkSuccess_Ended{Ended: &storev1.GetDetachedWorkEnded{EndedAtMs: row.ended.Int64}}
	} else {
		success.State = &storev1.GetDetachedWorkSuccess_Live{Live: &storev1.GetDetachedWorkLive{}}
	}
	d.log.Log(base, "unit %s left detached work %s kind=%s ended=%t", unit, row.workID, row.kind, row.ended.Valid)
	return success, true, nil
}
