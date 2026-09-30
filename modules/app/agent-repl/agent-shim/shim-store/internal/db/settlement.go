package db

import (
	"context"

	"agentrepl/shim-store/internal/logging"
)

// settlement.go — which detached runs the record already holds as ENDED, and
// the lookup the sidecar asks it through before its LOST policy tracks a run.
//
// WHY IT EXISTS. The sidecar used to know that a run settled only in its own
// memory. A restarted sidecar resumes each file at its committed cursor, past
// the terminator it already converted, so it re-tracked a finished run from its
// quiet file and later concluded it LOST — and that LOST terminal superseded
// the real one (2026-09-30, docs/protobuf-design/run-settlements.md). The
// settle is durable in exactly one place, the run's `detached_work` row, so
// that is what answers.
//
// A RUN IS SETTLED ONLY WHEN EVERY ROW ITS ORIGIN UNIT LOCATES HAS ENDED. The
// table holds one row per run by construction (resolveDetachedRowKey); should
// two ever share an origin, one still live means the record does not hold the
// run as ended, and the answer leaves it out rather than guessing it settled.
// A run with no row is left out too: absence is "not settled", never an error.

// runSettlementsSQL answers each asked run whose rows have all ended, with the
// latest end instant. The `%s` is expanded to the asked ids (expandInList). At
// package scope so the suite EXPLAINs the production text itself.
const runSettlementsSQL = `
	  SELECT origin_unit, MAX(ended_at_ms) FROM detached_work
	  WHERE origin_unit IN (%s)
	  GROUP BY origin_unit
	  HAVING COUNT(*) = COUNT(ended_at_ms)
	  ORDER BY origin_unit ASC`

// SettledRun is one asked run the record holds as ended.
type SettledRun struct {
	RunID     string
	EndedAtMs int64
}

// RunSettlements answers which of the asked runs the record holds as ended,
// by the spawning call's activity id (the row's origin unit). A run absent from
// the answer is not settled. No ids, or an empty id, is refused before any
// statement runs.
func (d *DB) RunSettlements(ctx context.Context, runIDs []string) ([]SettledRun, error) {
	base := logging.Fields{Operation: "store.db.run-settlements", Table: "detached_work"}
	args, err := idListArgs(runIDs, "run_ids",
		"run_ids is empty — the lookup names the runs it answers", "a run id is empty — every asked id names a run")
	if err != nil {
		return nil, d.refuse(base, err)
	}
	started := d.mono()
	rows, err := d.read.QueryContext(ctx, expandInList(runSettlementsSQL, len(args)), args...)
	if err != nil {
		return nil, d.refuse(base, storagef(err, "reading the settlements of %d run(s)", len(args)))
	}
	defer rows.Close() //nolint:errcheck // the deferred close of a read
	var out []SettledRun
	for rows.Next() {
		var settled SettledRun
		if err := rows.Scan(&settled.RunID, &settled.EndedAtMs); err != nil {
			return nil, d.refuse(base, storagef(err, "reading a run settlement"))
		}
		out = append(out, settled)
	}
	if err := rows.Err(); err != nil {
		return nil, d.refuse(base, storagef(err, "reading the settlements of %d run(s)", len(args)))
	}
	d.observeQuery(StatementRunSettlements, "detached_work", base, started, int64(len(out)))
	d.traceStatement(ctx, StatementRunSettlements, "detached_work", base, int64(len(out)))
	d.log.LogVerbose(base, "answered %d settled run(s) of %d asked", len(out), len(args))
	return out, nil
}
