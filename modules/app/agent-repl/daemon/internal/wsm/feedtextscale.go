package wsm

import (
	"context"
	"database/sql"
	"errors"
	"fmt"
	"math"

	"claude-repld/internal/dlog"
)

// DefaultFeedTextScale is the feed text zoom multiplier when no preference has
// ever been set. An ABSENT feed_text_scale row means exactly this — "nobody has
// zoomed yet", a legitimate value rather than a missing one — so FeedTextScale
// answers it without an error. The daemon's clamp range is defined in the
// server package (which owns the step and the bounds); this is only the
// persistence default.
const DefaultFeedTextScale = 1.0

// PutFeedTextScale persists the feed text zoom, replacing any current value. At
// most one preference exists, which the single-row primary key makes
// structural. A non-finite or non-positive scale is a bug in the caller (the
// daemon clamps to a positive range before writing), so it is refused loudly
// rather than persisted.
func (s *store) PutFeedTextScale(ctx context.Context, scale float64) error {
	const op = "daemon.wsm.put_feed_text_scale"
	fields := dlog.Context{"scale": scale}
	if math.IsNaN(scale) || math.IsInf(scale, 0) || scale <= 0 {
		err := fmt.Errorf("wsm: invalid feed text scale %v", scale)
		s.log.Error(op, "refused a non-finite or non-positive feed text scale", withError(fields, err))
		return err
	}
	return s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		_, err := tx.ExecContext(ctx,
			`INSERT INTO feed_text_scale (id, scale) VALUES (1, ?)
			 ON CONFLICT(id) DO UPDATE SET scale = excluded.scale`,
			scale)
		return err
	})
}

// FeedTextScale loads the persisted feed text zoom, or DefaultFeedTextScale
// when none is set. An absent row is the "never zoomed" case and is NOT an
// error; a stored non-finite or non-positive value, on the other hand, is a
// corrupt record and is surfaced as a DecodeError.
func (s *store) FeedTextScale(ctx context.Context) (float64, error) {
	scale := DefaultFeedTextScale
	err := s.read(ctx, "daemon.wsm.feed_text_scale", dlog.Context{}, func(ctx context.Context) error {
		var stored float64
		err := s.db().QueryRowContext(ctx, `SELECT scale FROM feed_text_scale WHERE id = 1`).Scan(&stored)
		if errors.Is(err, sql.ErrNoRows) {
			scale = DefaultFeedTextScale
			return nil
		}
		if err != nil {
			return err
		}
		if math.IsNaN(stored) || math.IsInf(stored, 0) || stored <= 0 {
			return &DecodeError{Table: "feed_text_scale", Row: "1", Field: "scale", Err: fmt.Errorf("a stored scale is finite and positive")}
		}
		scale = stored
		return nil
	})
	return scale, err
}
