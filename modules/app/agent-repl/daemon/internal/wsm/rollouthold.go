package wsm

import (
	"context"
	"database/sql"
	"errors"
	"time"

	"agent-repld/internal/dlog"
)

// RolloutHold is the daemon-wide hold on merge-queue rollouts: while Held, a
// merge that lands in the daemon's own checkout does not tell the deploy.
type RolloutHold struct {
	// Held reports whether the rollouts are held.
	Held bool
	// Since is when the hold began; zero when not held.
	Since time.Time
}

// SetMergeRolloutHold holds the rollouts from at (held true) or ends the hold
// (held false). Holding an already-held rollout keeps the ORIGINAL start, so
// Since always answers how long the build has been kept; the orchestrator
// refuses that no-op before it gets here.
func (s *store) SetMergeRolloutHold(ctx context.Context, held bool, at time.Time) error {
	const op = "daemon.wsm.set_merge_rollout_hold"
	return s.write(ctx, op, dlog.Context{"held": held}, func(ctx context.Context, tx *sql.Tx) error {
		if !held {
			_, err := tx.ExecContext(ctx, `DELETE FROM merge_rollout_hold WHERE id = 1`)
			return err
		}
		_, err := tx.ExecContext(ctx,
			`INSERT INTO merge_rollout_hold (id, held_at) VALUES (1, ?)
			 ON CONFLICT(id) DO NOTHING`, nanos(at))
		return err
	})
}

// MergeRolloutHold reports the rollout hold. An absent row is NOT HELD, the
// state every daemon before layout 37 ran in, and is answered without an
// error.
func (s *store) MergeRolloutHold(ctx context.Context) (RolloutHold, error) {
	var hold RolloutHold
	err := s.read(ctx, "daemon.wsm.merge_rollout_hold", dlog.Context{}, func(ctx context.Context) error {
		var since int64
		err := s.db().QueryRowContext(ctx, `SELECT held_at FROM merge_rollout_hold WHERE id = 1`).Scan(&since)
		if errors.Is(err, sql.ErrNoRows) {
			hold = RolloutHold{}
			return nil
		}
		if err != nil {
			return err
		}
		hold = RolloutHold{Held: true, Since: fromNanos(since)}
		return nil
	})
	return hold, err
}
