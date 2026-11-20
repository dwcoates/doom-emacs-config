package wsm

import (
	"context"
	"database/sql"
	"errors"
	"fmt"
	"time"

	"agent-repld/internal/dlog"
)

// OpenMergeLedger opens a workspace's ledger for one merge lease. Reopening the
// same lease's ledger is idempotent; the ledger's intervals are append-only.
func (s *store) OpenMergeLedger(ctx context.Context, id WorkspaceID, lease LeaseID) error {
	fields := dlog.Context{"workspace": string(id), "lease": string(lease)}
	return s.write(ctx, "daemon.wsm.open_merge_ledger", fields, func(ctx context.Context, tx *sql.Tx) error {
		var openedAt int64
		err := tx.QueryRowContext(ctx, `SELECT opened_at FROM merge_ledger WHERE lease_id = ?`, lease).Scan(&openedAt)
		if err == nil {
			return nil
		}
		if !errors.Is(err, sql.ErrNoRows) {
			return err
		}
		_, err = tx.ExecContext(ctx, `INSERT INTO merge_ledger (lease_id, workspace_id, opened_at) VALUES (?, ?, ?)`,
			lease, id, nanos(nowUTC()))
		return err
	})
}

// RecordTabInterval appends one round's tab interval to a lease's ledger. The
// same (round, kind) is UPDATED rather than duplicated, which is how a running
// interval later records its end and outcome.
//
// A LEDGER HOLDS AT MOST ONE LIVE ROUND, the merge's current step (owner
// ruling, 2026-11-16). A live round written while another stands live means
// the merge moved on from that other step without settling it: a restart, a
// handover, a pause or a step aside cut it. That round is settled `restarted`
// at the new round's start IN THE SAME WRITE, so no reader ever sees two live
// rounds (2026-11-16: linux-wifi-port's ledger held six rebasing rounds that
// never ended beside a live tests round, and the roster drew both live).
func (s *store) RecordTabInterval(ctx context.Context, lease LeaseID, interval TabInterval) error {
	const op = "daemon.wsm.record_tab_interval"
	fields := dlog.Context{"lease": string(lease), "round": interval.Round, "kind": interval.Kind}
	if interval.Round < 1 {
		err := fmt.Errorf("wsm: merge tab rounds number from one, got %d", interval.Round)
		s.log.Error(op, "refused a merge tab interval with no round", withError(fields, err))
		return err
	}
	if interval.Kind == "" {
		err := errors.New("wsm: empty merge tab kind")
		s.log.Error(op, "refused a merge tab interval with no kind", withError(fields, err))
		return err
	}
	return s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		var openedAt int64
		err := tx.QueryRowContext(ctx, `SELECT opened_at FROM merge_ledger WHERE lease_id = ?`, lease).Scan(&openedAt)
		if errors.Is(err, sql.ErrNoRows) {
			return fmt.Errorf("wsm: merge ledger for lease %s: %w", lease, ErrNotFound)
		}
		if err != nil {
			return err
		}
		if interval.EndedAt == nil {
			cut, err := settleLiveRounds(ctx, tx, TabOutcomeRestarted,
				sqlExpr{`MAX(started_at, ?)`, []any{nanos(interval.StartedAt)}},
				sqlExpr{`lease_id = ? AND NOT (round = ? AND kind = ?)`, []any{lease, interval.Round, interval.Kind}})
			if err != nil {
				return err
			}
			if cut > 0 {
				s.log.Info(op, "a round opened while another stood live; the one left live is settled as cut in the same write", dlog.Merge(fields, dlog.Context{"cut": cut}))
			}
		}
		_, err = tx.ExecContext(ctx,
			`INSERT INTO merge_tab_intervals (lease_id, round, kind, started_at, ended_at, outcome) VALUES (?, ?, ?, ?, ?, ?)
			 ON CONFLICT(lease_id, round, kind) DO UPDATE SET
			   started_at = excluded.started_at, ended_at = excluded.ended_at, outcome = excluded.outcome,
			   held_since = CASE WHEN excluded.ended_at IS NULL THEN merge_tab_intervals.held_since END`,
			lease, interval.Round, interval.Kind, nanos(interval.StartedAt), nullNanos(interval.EndedAt), interval.Outcome)
		return err
	})
}

// sqlExpr is one SQL expression and the arguments its placeholders take.
type sqlExpr struct {
	text string
	args []any
}

// settleLiveRounds settles every live round of merge_tab_intervals WHERE
// picks, ending it at ENDED with OUTCOME, and answers how many it settled. It
// is the ONE write that settles a round: a settled round is never held, so it
// clears the round's hold in the same statement, and no settling write can
// leave a round settled and held (2026-11-17: the orphan sweep settled a held
// round by its own UPDATE, kept the hold, and MergeLedger refused the row).
func settleLiveRounds(ctx context.Context, tx *sql.Tx, outcome string, ended, where sqlExpr) (int64, error) {
	args := append(append(append([]any{}, ended.args...), outcome), where.args...)
	res, err := tx.ExecContext(ctx,
		`UPDATE merge_tab_intervals SET ended_at = `+ended.text+`, outcome = ?, held_since = NULL
		 WHERE ended_at IS NULL AND (`+where.text+`)`, args...)
	if err != nil {
		return 0, err
	}
	return res.RowsAffected()
}

// HoldMergeRound marks the live round of LEASE's ledger HELD since AT, or
// clears its hold when AT is nil, and answers whether a live round was there
// to mark. It is the ONE writer of a round's hold (merge's suspend and
// resume): a held round is one whose merge is suspended at it, which every
// view of the merge draws from this one column. A settled round is never
// held: settling a round clears its hold in the same write
// (RecordTabInterval).
func (s *store) HoldMergeRound(ctx context.Context, lease LeaseID, at *time.Time) (bool, error) {
	const op = "daemon.wsm.hold_merge_round"
	fields := dlog.Context{"lease": string(lease), "held": at != nil}
	if lease == "" {
		err := errors.New("wsm: a merge round's hold names its ledger")
		s.log.Error(op, "refused a merge round hold that names no ledger", withError(fields, err))
		return false, err
	}
	held := false
	err := s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		res, err := tx.ExecContext(ctx, `UPDATE merge_tab_intervals SET held_since = ? WHERE lease_id = ? AND ended_at IS NULL`, nullNanos(at), lease)
		if err != nil {
			return err
		}
		held, err = touchedRow(res)
		return err
	})
	return held, err
}

// HeldMergeRounds answers every ledger whose live round is held, and since
// when (HoldMergeRound): the one read the merge views take their holds from.
func (s *store) HeldMergeRounds(ctx context.Context) (map[LeaseID]time.Time, error) {
	const op = "daemon.wsm.held_merge_rounds"
	out := map[LeaseID]time.Time{}
	err := s.read(ctx, op, nil, func(ctx context.Context) error {
		rows, err := s.q(ctx).QueryContext(ctx, `SELECT lease_id, held_since FROM merge_tab_intervals WHERE held_since IS NOT NULL AND ended_at IS NULL`)
		if err != nil {
			return err
		}
		defer rows.Close()
		for rows.Next() {
			var lease LeaseID
			var at int64
			if err := rows.Scan(&lease, &at); err != nil {
				return err
			}
			out[lease] = fromNanos(at)
		}
		return rows.Err()
	})
	return out, err
}

// mergeRoundHoldDDL is the layout-157 addition: when the live round of a
// merge's ledger was suspended, NULL while it runs (HoldMergeRound).
const mergeRoundHoldDDL = `
ALTER TABLE merge_tab_intervals ADD COLUMN held_since INTEGER;
`

// unheldSettledRoundsDDL is the layout-170 repair: every settled round the
// orphan sweep left held (SweepOrphanedMergeState) has its hold cleared, so
// no stored round is both settled and held. It rewrites rows, so it is an
// additive step: a contract step only removes.
const unheldSettledRoundsDDL = `
UPDATE merge_tab_intervals SET held_since = NULL WHERE held_since IS NOT NULL AND ended_at IS NOT NULL;
`

// settledRoundsUnheldDDL is the layout-174 addition: A SETTLED ROUND IS
// NEVER HELD is the table's own rule rather than each writer's care. Two
// triggers abort any insert or update that would leave a round both settled
// and held, so a writer that settles a round without clearing its hold fails
// loudly at its write rather than storing a row every ledger read refuses
// (2026-11-17: the orphan sweep settled sidebar-roster-streams' held fixes
// round and left its hold, and its next merge aborted on the decode). It
// follows layout 170, which cleared every such row already stored. IT IS
// ADDITIVE: no column changes, and the previous build's writers already
// clear a round's hold as they settle it, so none of them meets the triggers.
const settledRoundsUnheldDDL = `
CREATE TRIGGER merge_tab_intervals_settled_unheld_insert BEFORE INSERT ON merge_tab_intervals
WHEN NEW.ended_at IS NOT NULL AND NEW.held_since IS NOT NULL
BEGIN SELECT RAISE(ABORT, 'merge_tab_intervals: a settled round is never held'); END;
CREATE TRIGGER merge_tab_intervals_settled_unheld_update BEFORE UPDATE OF ended_at, held_since ON merge_tab_intervals
WHEN NEW.ended_at IS NOT NULL AND NEW.held_since IS NOT NULL
BEGIN SELECT RAISE(ABORT, 'merge_tab_intervals: a settled round is never held'); END;
`

// TabOutcomeRestarted is the outcome of a round a later step cut before it
// settled (RecordTabInterval).
const TabOutcomeRestarted = "restarted"

// settledLiveRoundsDDL is the layout-155 addition: every round left live in
// a ledger before RecordTabInterval kept one live round per ledger is settled
// `restarted` at the next round's start, and every live round of a ledger no
// queued merge draws any more is settled at its own start, so the store holds
// at most one live round per ledger, the current one.
// It names no hold because it runs before layout 157 adds the column, so no
// round it settles can be held.
const settledLiveRoundsDDL = `
UPDATE merge_tab_intervals SET outcome = 'restarted',
  ended_at = COALESCE((SELECT MIN(later.started_at) FROM merge_tab_intervals AS later
                       WHERE later.lease_id = merge_tab_intervals.lease_id AND later.started_at > merge_tab_intervals.started_at),
                      merge_tab_intervals.started_at)
WHERE ended_at IS NULL
  AND (EXISTS (SELECT 1 FROM merge_tab_intervals AS later
               WHERE later.lease_id = merge_tab_intervals.lease_id AND later.started_at > merge_tab_intervals.started_at)
       OR NOT EXISTS (SELECT 1 FROM merge_queue WHERE merge_queue.ledger_id = merge_tab_intervals.lease_id));
`

// MergeLedger loads a workspace's ledger entries, all-or-nothing: one entry per
// merge lease, each carrying its rounds in order.
func (s *store) MergeLedger(ctx context.Context, id WorkspaceID) ([]MergeLedgerEntry, error) {
	var out []MergeLedgerEntry
	err := s.read(ctx, "daemon.wsm.merge_ledger", dlog.Context{"workspace": string(id)}, func(ctx context.Context) error {
		rows, err := s.q(ctx).QueryContext(ctx, `SELECT lease_id, opened_at FROM merge_ledger WHERE workspace_id = ? ORDER BY opened_at, lease_id`, id)
		if err != nil {
			return err
		}
		defer rows.Close()
		var loaded []MergeLedgerEntry
		for rows.Next() {
			var (
				entry  MergeLedgerEntry
				opened int64
			)
			if err := rows.Scan(&entry.Lease, &opened); err != nil {
				return err
			}
			entry.Workspace = id
			entry.OpenedAt = fromNanos(opened)
			loaded = append(loaded, entry)
		}
		if err := rows.Err(); err != nil {
			return err
		}
		for i := range loaded {
			intervals, err := s.tabIntervals(ctx, loaded[i].Lease)
			if err != nil {
				return err
			}
			loaded[i].Intervals = intervals
		}
		out = loaded
		return nil
	})
	if err != nil {
		return nil, err
	}
	return out, nil
}

// MergeLedgerWorkspace answers the workspace whose ledger holds LEASE: the
// workspace that ran the merge. A lease no ledger holds is ErrNotFound.
func (s *store) MergeLedgerWorkspace(ctx context.Context, lease LeaseID) (WorkspaceID, error) {
	var ws WorkspaceID
	err := s.read(ctx, "daemon.wsm.merge_ledger_workspace", dlog.Context{"lease": string(lease)}, func(ctx context.Context) error {
		err := s.q(ctx).QueryRowContext(ctx, `SELECT workspace_id FROM merge_ledger WHERE lease_id = ?`, lease).Scan(&ws)
		if errors.Is(err, sql.ErrNoRows) {
			return fmt.Errorf("wsm: merge ledger for lease %s: %w", lease, ErrNotFound)
		}
		return err
	})
	if err != nil {
		return "", err
	}
	return ws, nil
}

// tabIntervals loads one lease's rounds in order, all-or-nothing.
func (s *store) tabIntervals(ctx context.Context, lease LeaseID) ([]TabInterval, error) {
	rows, err := s.q(ctx).QueryContext(ctx,
		`SELECT round, kind, started_at, ended_at, outcome, held_since FROM merge_tab_intervals WHERE lease_id = ? ORDER BY round, started_at, kind`, lease)
	if err != nil {
		return nil, err
	}
	defer rows.Close()
	var loaded []TabInterval
	for rows.Next() {
		var (
			interval TabInterval
			started  int64
			ended    sql.NullInt64
			held     sql.NullInt64
		)
		if err := rows.Scan(&interval.Round, &interval.Kind, &started, &ended, &interval.Outcome, &held); err != nil {
			return nil, err
		}
		if interval.Round < 1 {
			return nil, &DecodeError{Table: "merge_tab_intervals", Row: string(lease), Field: "round", Err: fmt.Errorf("merge tab rounds number from one, got %d", interval.Round)}
		}
		interval.StartedAt = fromNanos(started)
		interval.EndedAt = optTime(ended)
		interval.HeldSince = optTime(held)
		if interval.HeldSince != nil && interval.EndedAt != nil {
			return nil, &DecodeError{Table: "merge_tab_intervals", Row: string(lease), Field: "held_since", Err: fmt.Errorf("round %d of %s is settled and held", interval.Round, interval.Kind)}
		}
		loaded = append(loaded, interval)
	}
	if err := rows.Err(); err != nil {
		return nil, err
	}
	return loaded, nil
}
