package wsm

import (
	"context"
	"database/sql"
	"errors"
	"fmt"

	"claude-repld/internal/dlog"
)

// PutDrainSchedule puts a drain schedule in force, replacing any current one. At
// most one schedule exists, which the single-row primary key makes structural.
func (s *store) PutDrainSchedule(ctx context.Context, schedule DrainSchedule) error {
	const op = "daemon.wsm.put_drain_schedule"
	fields := dlog.Context{"reason": schedule.Reason, "deadline": schedule.Deadline}
	if schedule.Reason == "" {
		err := errors.New("wsm: empty drain reason")
		s.log.Error(op, "refused a drain schedule with no reason", withError(fields, err))
		return err
	}
	if schedule.Deadline.IsZero() {
		err := errors.New("wsm: drain schedule with no deadline")
		s.log.Error(op, "refused a drain schedule with no deadline", withError(fields, err))
		return err
	}
	return s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		_, err := tx.ExecContext(ctx,
			`INSERT INTO drain_schedule (id, reason, deadline, set_at) VALUES (1, ?, ?, ?)
			 ON CONFLICT(id) DO UPDATE SET reason = excluded.reason, deadline = excluded.deadline, set_at = excluded.set_at`,
			schedule.Reason, nanos(schedule.Deadline), nanos(schedule.SetAt))
		return err
	})
}

// ClearDrainSchedule cancels the schedule in force. Clearing when none is in
// force is not an error: the post-state the caller asked for is the one that
// holds.
func (s *store) ClearDrainSchedule(ctx context.Context) error {
	return s.write(ctx, "daemon.wsm.clear_drain_schedule", dlog.Context{}, func(ctx context.Context, tx *sql.Tx) error {
		_, err := tx.ExecContext(ctx, `DELETE FROM drain_schedule WHERE id = 1`)
		return err
	})
}

// DrainSchedule loads the schedule in force, nil when none is.
func (s *store) DrainSchedule(ctx context.Context) (*DrainSchedule, error) {
	var out *DrainSchedule
	err := s.read(ctx, "daemon.wsm.drain_schedule", dlog.Context{}, func(ctx context.Context) error {
		var (
			schedule DrainSchedule
			deadline int64
			setAt    int64
		)
		err := s.db.QueryRowContext(ctx, `SELECT reason, deadline, set_at FROM drain_schedule WHERE id = 1`).
			Scan(&schedule.Reason, &deadline, &setAt)
		if errors.Is(err, sql.ErrNoRows) {
			out = nil
			return nil
		}
		if err != nil {
			return err
		}
		if schedule.Reason == "" {
			return &DecodeError{Table: "drain_schedule", Row: "1", Field: "reason", Err: fmt.Errorf("a schedule in force names its reason")}
		}
		schedule.Deadline = fromNanos(deadline)
		schedule.SetAt = fromNanos(setAt)
		out = &schedule
		return nil
	})
	return out, err
}
