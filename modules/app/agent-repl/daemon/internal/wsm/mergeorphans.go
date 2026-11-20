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
	// ClosedRounds are the live ledger rounds settled as abandoned, each as
	// it stood before the sweep settled it.
	ClosedRounds []AbandonedRound
	// ReleasedLeases are the merge leases released, each as it stood.
	ReleasedLeases []Lease
}

// AbandonedRound is one live ledger round the sweep settled as abandoned.
type AbandonedRound struct {
	// Lease is the ledger the round belongs to.
	Lease LeaseID
	// Workspace is the workspace whose merge ran under that ledger.
	Workspace WorkspaceID
	// Round and Kind name the round within its ledger.
	Round int
	Kind  string
	// HeldSince is when the merge was suspended at the round, nil when it
	// was not held. The sweep clears the hold as it settles the round: a
	// settled round is never held (HoldMergeRound).
	HeldSince *time.Time
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
//
// A ROUND IT SETTLES IS NO LONGER HELD: the sweep settles through settleLiveRounds,
// which clears the hold in the same write (HoldMergeRound). A suspended merge's
// abandoned round once kept its hold, and every read of its ledger refused it
// as settled and held (2026-11-18, eight rounds, a merge failed on each read).
func (s *store) SweepOrphanedMergeState(ctx context.Context, at time.Time) (OrphanedMergeState, error) {
	const op = "daemon.wsm.sweep_orphaned_merge_state"
	var out OrphanedMergeState
	fields := dlog.Context{}
	err := s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		out = OrphanedMergeState{}
		released, err := listRows(ctx, tx, `SELECT `+leaseColumns+` FROM leases
			WHERE holder = ? AND workspace_id NOT IN (SELECT workspace_id FROM merge_queue) ORDER BY acquired_at, id`, scanLease, int(HolderMerge))
		if err != nil {
			return err
		}
		out.ReleasedLeases = released
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
		// THE ROUNDS ARE READ BEFORE THEY ARE SETTLED, holds and all, so the
		// sweep's record names each one (2026-11-17: sidebar-roster-streams'
		// held fixes round was settled by no record at all).
		out.ClosedRounds, err = listRows(ctx, tx, `SELECT i.lease_id, l.workspace_id, i.round, i.kind, i.held_since
			FROM merge_tab_intervals AS i JOIN merge_ledger AS l ON l.lease_id = i.lease_id
			WHERE i.ended_at IS NULL AND `+orphanedRound("i.lease_id")+` ORDER BY i.lease_id, i.round, i.kind`, scanAbandonedRound)
		if err != nil {
			return err
		}
		_, err = settleLiveRounds(ctx, tx, "abandoned", sqlExpr{`?`, []any{at.UnixNano()}}, sqlExpr{orphanedRound("lease_id"), nil})
		return err
	})
	if err != nil {
		return OrphanedMergeState{}, err
	}
	for _, l := range out.ReleasedLeases {
		s.disown(l.ID)
	}
	return out, nil
}

// orphanedRound is the condition that no queue entry and no progress record
// names the ledger LEASE, the round's lease column as the query spells it.
func orphanedRound(lease string) string {
	return lease + ` NOT IN (SELECT ledger_id FROM merge_queue WHERE ledger_id IS NOT NULL AND ledger_id != '')
			  AND ` + lease + ` NOT IN (SELECT lease_id FROM merge_progress)`
}

// scanAbandonedRound reads one row of the sweep's round query.
func scanAbandonedRound(rows interface{ Scan(...any) error }) (AbandonedRound, error) {
	var (
		r    AbandonedRound
		held sql.NullInt64
	)
	if err := rows.Scan(&r.Lease, &r.Workspace, &r.Round, &r.Kind, &held); err != nil {
		return AbandonedRound{}, err
	}
	r.HeldSince = optTime(held)
	return r, nil
}
