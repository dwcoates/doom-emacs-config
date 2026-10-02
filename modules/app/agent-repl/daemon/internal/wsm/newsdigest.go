package wsm

import (
	"context"
	"database/sql"
	"errors"
	"time"

	"claude-repld/internal/dlog"
)

// newsDigestDDL is the layout-19 addition: the daily news digest's durable
// state (internal/newsdigest), kept apart from the rest of the schema for the
// reason portedPromptsDDL is: a fresh file gets it as part of schemaDDL, a
// layout-18 file from the 18 -> 19 migration, so one text keeps the two shapes
// from drifting.
//
// news_digest is a SINGLETON (the CHECK (id = 1) makes "one digest state"
// structural): when the last run ended (the cadence is measured from it), the
// end of the last run whose reading was recorded (the next digest's
// baseline), the id of the newest digest ever minted, and that digest's
// encoded frontend.v1.NewsDigestOverlay while it stands (NULL once dismissed).
//
// news_digest_sources holds each watched source's snapshot as of the last
// recorded reading: what the next run diffs against. Its meaning is the
// newsdigest package's (seen entry ids for a feed, extracted text for a page).
const newsDigestDDL = `
CREATE TABLE news_digest (
  id               INTEGER PRIMARY KEY CHECK (id = 1),
  last_run_end     INTEGER NOT NULL,
  baseline         INTEGER,
  latest_digest_id TEXT,
  standing         BLOB
);

CREATE TABLE news_digest_sources (
  source_key TEXT PRIMARY KEY,
  snapshot   TEXT NOT NULL
);
`

// NewsDigestState is the news digest's whole durable state.
type NewsDigestState struct {
	// LastRunEnd is when the last run ended; zero when no run ever has.
	LastRunEnd time.Time
	// Baseline is the end of the last run whose reading was recorded; zero
	// when none was.
	Baseline time.Time
	// LatestID is the newest digest ever minted; empty when none was.
	LatestID string
	// Standing is the encoded overlay of the digest that stands; nil when
	// none stands.
	Standing []byte
	// Snapshots are the sources' recorded snapshots, by source key.
	Snapshots map[string]string
}

// NewsDigestRun is one finished run, recorded whole.
type NewsDigestRun struct {
	// EndedAt is when the run ended: the cadence's new origin. Required.
	EndedAt time.Time
	// Recorded is whether the run's reading is kept: its EndedAt becomes the
	// baseline and Snapshots replace the named sources'. A run whose model
	// failed, or that read no source, keeps nothing but its end, so the next
	// run reads the same material again.
	Recorded bool
	// Snapshots are the sources read this run, by source key. Only a Recorded
	// run may carry them.
	Snapshots map[string]string
	// Digest is the digest the run made, which now stands; nil when it made
	// none (any digest already standing is left standing).
	Digest *NewsDigestMinted
}

// NewsDigestMinted is a digest a run made.
type NewsDigestMinted struct {
	// ID is the digest's opaque identity. Required.
	ID string
	// Overlay is the encoded frontend.v1.NewsDigestOverlay. Required.
	Overlay []byte
}

// NewsDigestState loads the news digest's durable state. A file on which no
// run was ever recorded answers the zero state, which is the "never run"
// fact rather than a missing one.
func (s *store) NewsDigestState(ctx context.Context) (NewsDigestState, error) {
	var out NewsDigestState
	err := s.read(ctx, "daemon.wsm.news_digest_state", dlog.Context{}, func(ctx context.Context) error {
		var (
			lastRunEnd int64
			baseline   sql.NullInt64
			latest     sql.NullString
			standing   []byte
		)
		loaded := NewsDigestState{Snapshots: map[string]string{}}
		err := s.db().QueryRowContext(ctx,
			`SELECT last_run_end, baseline, latest_digest_id, standing FROM news_digest WHERE id = 1`).
			Scan(&lastRunEnd, &baseline, &latest, &standing)
		switch {
		case errors.Is(err, sql.ErrNoRows):
		case err != nil:
			return err
		default:
			loaded.LastRunEnd = fromNanos(lastRunEnd)
			if baseline.Valid {
				loaded.Baseline = fromNanos(baseline.Int64)
			}
			loaded.LatestID = latest.String
			if standing != nil && !latest.Valid {
				return &DecodeError{Table: "news_digest", Row: "1", Field: "standing",
					Err: errors.New("a standing digest carries the id it was minted with")}
			}
			loaded.Standing = standing
		}
		rows, err := s.db().QueryContext(ctx, `SELECT source_key, snapshot FROM news_digest_sources ORDER BY source_key`)
		if err != nil {
			return err
		}
		defer rows.Close()
		for rows.Next() {
			var key, snapshot string
			if err := rows.Scan(&key, &snapshot); err != nil {
				return err
			}
			loaded.Snapshots[key] = snapshot
		}
		if err := rows.Err(); err != nil {
			return err
		}
		out = loaded
		return nil
	})
	return out, err
}

// RecordNewsDigestRun records one finished run in one transaction: its end,
// and when it is Recorded its baseline and snapshots, and its digest when it
// made one. A malformed run is a bug in the caller and is refused loudly.
func (s *store) RecordNewsDigestRun(ctx context.Context, run NewsDigestRun) error {
	const op = "daemon.wsm.record_news_digest_run"
	fields := dlog.Context{"ended_at": run.EndedAt.UTC().Format(time.RFC3339Nano), "recorded": run.Recorded,
		"sources": len(run.Snapshots), "digest": run.Digest != nil}
	if err := validateNewsDigestRun(run); err != nil {
		s.log.Error(op, "refused a malformed news digest run", withError(fields, err))
		return err
	}
	return s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		if _, err := tx.ExecContext(ctx,
			`INSERT INTO news_digest (id, last_run_end) VALUES (1, ?)
			 ON CONFLICT(id) DO UPDATE SET last_run_end = excluded.last_run_end`, nanos(run.EndedAt)); err != nil {
			return err
		}
		if run.Recorded {
			if _, err := tx.ExecContext(ctx, `UPDATE news_digest SET baseline = ? WHERE id = 1`, nanos(run.EndedAt)); err != nil {
				return err
			}
		}
		for key, snapshot := range run.Snapshots {
			if _, err := tx.ExecContext(ctx,
				`INSERT INTO news_digest_sources (source_key, snapshot) VALUES (?, ?)
				 ON CONFLICT(source_key) DO UPDATE SET snapshot = excluded.snapshot`, key, snapshot); err != nil {
				return err
			}
		}
		if run.Digest != nil {
			if _, err := tx.ExecContext(ctx,
				`UPDATE news_digest SET latest_digest_id = ?, standing = ? WHERE id = 1`,
				run.Digest.ID, run.Digest.Overlay); err != nil {
				return err
			}
		}
		return nil
	})
}

// validateNewsDigestRun refuses a run the store cannot record faithfully.
func validateNewsDigestRun(run NewsDigestRun) error {
	switch {
	case run.EndedAt.IsZero():
		return errors.New("wsm: a news digest run states when it ended")
	case !run.Recorded && len(run.Snapshots) > 0:
		return errors.New("wsm: only a recorded news digest run carries snapshots")
	case !run.Recorded && run.Digest != nil:
		return errors.New("wsm: only a recorded news digest run makes a digest")
	case run.Digest != nil && run.Digest.ID == "":
		return errors.New("wsm: a news digest carries its id")
	case run.Digest != nil && len(run.Digest.Overlay) == 0:
		return errors.New("wsm: a news digest carries its overlay")
	}
	for key := range run.Snapshots {
		if key == "" {
			return errors.New("wsm: a news digest snapshot names its source")
		}
	}
	return nil
}

// DismissNewsDigest takes the standing digest down when id names the newest
// digest minted, answering true; dismissing it again is still true. An id
// naming any other digest, or none, answers false and changes nothing.
func (s *store) DismissNewsDigest(ctx context.Context, id string) (bool, error) {
	const op = "daemon.wsm.dismiss_news_digest"
	fields := dlog.Context{"digest": id}
	if id == "" {
		err := errors.New("wsm: a dismiss names the digest it dismisses")
		s.log.Error(op, "refused a dismiss naming no digest", withError(fields, err))
		return false, err
	}
	var matched bool
	err := s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		res, err := tx.ExecContext(ctx,
			`UPDATE news_digest SET standing = NULL WHERE id = 1 AND latest_digest_id = ?`, id)
		if err != nil {
			return err
		}
		n, err := res.RowsAffected()
		if err != nil {
			return err
		}
		matched = n == 1
		return nil
	})
	return matched, err
}
