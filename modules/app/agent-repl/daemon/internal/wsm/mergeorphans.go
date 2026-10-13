package wsm

import (
	"context"
	"database/sql"
	"time"

	"agent-repld/internal/dlog"
)

// SweepOrphanedMergeState removes what a merge left behind once nothing of it
// remains in its queue: a progress record whose workspace has no queue entry,
// and every ledger round still open whose lease no queue entry and no progress
// record names, closed as abandoned at `at`. It answers how many of each it
// swept.
//
// WHY. A merge suspended by a restart, a pause or a handover and then taken
// off its queue (an eviction, a dequeue, a workspace closed) could leave both
// behind: the next boot found a progress record with no entry to resume, and
// 29 ledger rounds of 22 leases stayed open for days, over-counting every
// reading of a merge's durations (2026-10-12). The rules touch only state no
// queue entry names, so a merge that is still queued, admitted or suspended
// keeps everything it owns.
func (s *store) SweepOrphanedMergeState(ctx context.Context, at time.Time) (progress, rounds int, err error) {
	const op = "daemon.wsm.sweep_orphaned_merge_state"
	fields := dlog.Context{}
	err = s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		res, err := tx.ExecContext(ctx, `DELETE FROM merge_progress WHERE workspace_id NOT IN (SELECT workspace_id FROM merge_queue)`)
		if err != nil {
			return err
		}
		n, err := res.RowsAffected()
		if err != nil {
			return err
		}
		progress = int(n)
		res, err = tx.ExecContext(ctx, `UPDATE merge_tab_intervals SET ended_at = ?, outcome = 'abandoned'
			WHERE ended_at IS NULL
			  AND lease_id NOT IN (SELECT ledger_id FROM merge_queue WHERE ledger_id IS NOT NULL AND ledger_id != '')
			  AND lease_id NOT IN (SELECT lease_id FROM merge_progress)`, at.UnixNano())
		if err != nil {
			return err
		}
		n, err = res.RowsAffected()
		if err != nil {
			return err
		}
		rounds = int(n)
		return nil
	})
	return progress, rounds, err
}
