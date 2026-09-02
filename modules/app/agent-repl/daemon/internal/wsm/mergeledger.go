package wsm

import (
	"context"
	"database/sql"
	"errors"
	"fmt"

	"claude-repld/internal/dlog"
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
		_, err = tx.ExecContext(ctx,
			`INSERT INTO merge_tab_intervals (lease_id, round, kind, started_at, ended_at, outcome) VALUES (?, ?, ?, ?, ?, ?)
			 ON CONFLICT(lease_id, round, kind) DO UPDATE SET
			   started_at = excluded.started_at, ended_at = excluded.ended_at, outcome = excluded.outcome`,
			lease, interval.Round, interval.Kind, nanos(interval.StartedAt), nullNanos(interval.EndedAt), interval.Outcome)
		return err
	})
}

// MergeLedger loads a workspace's ledger entries, all-or-nothing: one entry per
// merge lease, each carrying its rounds in order.
func (s *store) MergeLedger(ctx context.Context, id WorkspaceID) ([]MergeLedgerEntry, error) {
	var out []MergeLedgerEntry
	err := s.read(ctx, "daemon.wsm.merge_ledger", dlog.Context{"workspace": string(id)}, func(ctx context.Context) error {
		rows, err := s.db().QueryContext(ctx, `SELECT lease_id, opened_at FROM merge_ledger WHERE workspace_id = ? ORDER BY opened_at, lease_id`, id)
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

// tabIntervals loads one lease's rounds in order, all-or-nothing.
func (s *store) tabIntervals(ctx context.Context, lease LeaseID) ([]TabInterval, error) {
	rows, err := s.db().QueryContext(ctx,
		`SELECT round, kind, started_at, ended_at, outcome FROM merge_tab_intervals WHERE lease_id = ? ORDER BY round, started_at, kind`, lease)
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
		)
		if err := rows.Scan(&interval.Round, &interval.Kind, &started, &ended, &interval.Outcome); err != nil {
			return nil, err
		}
		if interval.Round < 1 {
			return nil, &DecodeError{Table: "merge_tab_intervals", Row: string(lease), Field: "round", Err: fmt.Errorf("merge tab rounds number from one, got %d", interval.Round)}
		}
		interval.StartedAt = fromNanos(started)
		interval.EndedAt = optTime(ended)
		loaded = append(loaded, interval)
	}
	if err := rows.Err(); err != nil {
		return nil, err
	}
	return loaded, nil
}
