package db

import (
	"context"

	"agentrepl/shim-store/internal/logging"
)

// liveshells.go — every detached SHELL run the record holds no terminal for
// whose announcement was written before an instant, and the lookup the sidecar
// asks it through once per process with the machine's boot instant.
//
// WHY IT EXISTS. A process cannot outlive the kernel that ran it, so a shell
// announced before the boot is ended whatever the record says. Its terminal is
// the sidecar's, read off its spool, but a reboot empties /tmp, where spools
// live, and the sidecar resumes every transcript past the launches it read
// before the reboot. Nothing would ever write such a run's terminal, and the
// shim, for which a shell is live until the sidecar writes its end, would
// re-announce it forever (2026-10-23: three shells held a workspace's merge
// for the rest of the day). The record is the one place that still names it.
//
// THE STAMP IS THE STORE'S. `announced_at_ms` is written from the store's own
// clock when the announcement is written, and a re-announcement never moves
// it (the upsert in lifecycle.go leaves it alone), so "announced before the
// boot" is a comparison of two instants on this host's own clock.
//
// A RUN WITH NO ORIGIN UNIT OR NO OWNER IS NOT ANSWERED: a terminal is keyed by
// the first and written into the second's book, so such a row cannot be
// concluded by anyone, and liveWorkGaps already states it.

// liveShellRunsAnnouncedBeforeSQL binds (the bash kind, the instant). At
// package scope so the suite EXPLAINs the production text itself.
const liveShellRunsAnnouncedBeforeSQL = `
	  SELECT work_id, origin_unit, owner_agent, announced_at_ms FROM detached_work
	  WHERE ended_at_ms IS NULL AND kind = ? AND announced_at_ms < ?
	    AND origin_unit IS NOT NULL AND owner_agent IS NOT NULL
	  ORDER BY announced_at_ms ASC, work_id ASC`

// LiveShellRun is one live shell run the record holds.
type LiveShellRun struct {
	// WorkID is the run's handle (the row's key).
	WorkID string
	// Run is the spawning call's activity id, the row's origin unit.
	Run string
	// Owner is the agent whose book holds the run's start.
	Owner string
	// AnnouncedAtMs is when the store wrote the run's announcement.
	AnnouncedAtMs int64
}

// LiveShellRunsAnnouncedBefore answers every live shell run whose announcement
// was written before beforeMs, oldest first. A non-positive instant is refused
// before any statement runs.
func (d *DB) LiveShellRunsAnnouncedBefore(ctx context.Context, beforeMs int64) ([]LiveShellRun, error) {
	base := logging.Fields{Operation: "store.db.live-shell-runs", Table: "detached_work"}
	if beforeMs <= 0 {
		return nil, d.refuse(base, invalidFieldf("announced_before_ms", "announced_before_ms: %d is not an instant; the lookup names the instant its runs were announced before", beforeMs))
	}
	started := d.mono()
	rows, err := d.read.QueryContext(ctx, liveShellRunsAnnouncedBeforeSQL, detachedKindBash, beforeMs)
	if err != nil {
		return nil, d.refuse(base, storagef(err, "reading the live shell runs announced before %d", beforeMs))
	}
	defer rows.Close() //nolint:errcheck // the deferred close of a read
	var out []LiveShellRun
	for rows.Next() {
		var run LiveShellRun
		if err := rows.Scan(&run.WorkID, &run.Run, &run.Owner, &run.AnnouncedAtMs); err != nil {
			return nil, d.refuse(base, storagef(err, "reading a live shell run"))
		}
		out = append(out, run)
	}
	if err := rows.Err(); err != nil {
		return nil, d.refuse(base, storagef(err, "reading the live shell runs announced before %d", beforeMs))
	}
	d.observeQuery(StatementLiveShellRuns, "detached_work", base, started, int64(len(out)))
	d.traceStatement(ctx, StatementLiveShellRuns, "detached_work", base, int64(len(out)))
	d.log.LogVerbose(base, "answered %d live shell run(s) announced before %d", len(out), beforeMs)
	return out, nil
}
