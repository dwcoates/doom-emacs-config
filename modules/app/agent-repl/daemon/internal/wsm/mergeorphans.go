package wsm

import (
	"context"
	"database/sql"
	"time"

	"agent-repld/internal/dlog"
)

// OrphanedMergeState is what one sweep of the merge state no queue entry
// names removed.
type OrphanedMergeState struct {
	// ProgressRecords is how many progress records were dropped.
	ProgressRecords int
	// RoundsClosed is how many open ledger rounds were closed as abandoned.
	RoundsClosed int
	// ReleasedLeases are the merge leases released, each as it stood.
	ReleasedLeases []Lease
}

// SweepOrphanedMergeState removes what a merge left behind once nothing of it
// remains in its queue: a merge lease whose workspace has no queue entry, a
// progress record whose workspace has no queue entry, and every ledger round
// still open whose lease no queue entry and no progress record names, closed
// as abandoned at `at`. It answers what it swept.
//
// WHY. A merge suspended by a restart, a pause or a handover and then taken
// off its queue (an eviction, a dequeue, a workspace closed) could leave all
// three behind: the next boot found a progress record with no entry to
// resume, and 29 ledger rounds of 22 leases stayed open for days,
// over-counting every reading of a merge's durations (2026-10-12). A merge
// lease standing over no queue entry refuses every prompt held under the
// merge (bindMergeHold: a merge lease stands with exactly one queue entry),
// so the workspace could take no prompt at all (2026-10-14). The rules touch
// only state no queue entry names, so a merge that is still queued, admitted
// or suspended keeps everything it owns; a merge's own teardown releases its
// lease before it removes its entry, so no merge in flight ever stands as a
// lease with no entry.
func (s *store) SweepOrphanedMergeState(ctx context.Context, at time.Time) (OrphanedMergeState, error) {
	const op = "daemon.wsm.sweep_orphaned_merge_state"
	var out OrphanedMergeState
	fields := dlog.Context{}
	err := s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		out = OrphanedMergeState{}
		rows, err := tx.QueryContext(ctx, `SELECT `+leaseColumns+` FROM leases
			WHERE holder = ? AND workspace_id NOT IN (SELECT workspace_id FROM merge_queue) ORDER BY acquired_at, id`, int(HolderMerge))
		if err != nil {
			return err
		}
		for rows.Next() {
			l, err := scanLease(rows)
			if err != nil {
				_ = rows.Close()
				return err
			}
			out.ReleasedLeases = append(out.ReleasedLeases, l)
		}
		if err := rows.Close(); err != nil {
			return err
		}
		if err := rows.Err(); err != nil {
			return err
		}
		for _, l := range out.ReleasedLeases {
			if _, err := tx.ExecContext(ctx, `DELETE FROM leases WHERE id = ?`, l.ID); err != nil {
				return err
			}
		}
		res, err := tx.ExecContext(ctx, `DELETE FROM merge_progress WHERE workspace_id NOT IN (SELECT workspace_id FROM merge_queue)`)
		if err != nil {
			return err
		}
		n, err := res.RowsAffected()
		if err != nil {
			return err
		}
		out.ProgressRecords = int(n)
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
		out.RoundsClosed = int(n)
		return nil
	})
	if err != nil {
		return OrphanedMergeState{}, err
	}
	for _, l := range out.ReleasedLeases {
		s.disown(l.ID)
	}
	return out, nil
}
