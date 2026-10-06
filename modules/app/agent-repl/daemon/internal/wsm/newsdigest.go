package wsm

import (
	"context"
	"database/sql"
	"errors"
	"fmt"
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

// newsDigestRedisplayDDL is the layout-20 addition (docs/protobuf-design/
// startup-and-fault-domains.md, Addendum): the newest digest's overlay and the
// instant it was minted, KEPT after a dismiss so a full Emacs restart can stand
// the day's digest again, and the last Emacs process identity a WatchDaemon
// carried, durable so a daemon restart under a live Emacs reads its reconnect
// as the same Emacs. Like the other column-add steps it is ALTERs, applied
// after newsDigestDDL on a fresh file and as its own step on a migrated one.
const newsDigestRedisplayDDL = `
ALTER TABLE news_digest ADD COLUMN latest_overlay BLOB;
ALTER TABLE news_digest ADD COLUMN latest_made_at INTEGER;

CREATE TABLE editor_instance (
  id      INTEGER PRIMARY KEY CHECK (id = 1),
  value   TEXT NOT NULL,
  seen_at INTEGER NOT NULL
);
`

// newsDigestHistoryDDL is the layout-21 addition (docs/protobuf-design/
// news-digest.md, Addendum "Since last week"): every item of every digest a
// run made, kept with its run's end and its regression-risk mark, so each run
// can recompute the week's risks. news_digest_items holds the encoded
// frontend.v1.NewsDigestItem at its position in the run, and risk is the
// one-line reason it could regress agent-repl (NULL when unmarked). Rows are
// pruned by the run that keeps newer ones. history_since is the start of the
// span the FIRST kept run covered: the record's own start, so the weekly
// section never claims a week it did not see. Applied after
// newsDigestRedisplayDDL on a fresh file and as its own step on a migrated one.
const newsDigestHistoryDDL = `
CREATE TABLE news_digest_items (
  run_end  INTEGER NOT NULL,
  position INTEGER NOT NULL,
  item     BLOB NOT NULL,
  risk     TEXT,
  PRIMARY KEY (run_end, position)
);

ALTER TABLE news_digest ADD COLUMN history_since INTEGER;
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
	// LatestOverlay is the encoded overlay of the newest digest minted, kept
	// whether or not it stands; nil when none was minted since layout 20.
	LatestOverlay []byte
	// LatestMadeAt is when the newest digest was minted (its run's end); zero
	// when unknown.
	LatestMadeAt time.Time
	// HistorySince is the start of the span the item history covers: what the
	// first run that kept items covered. Zero when no run has kept items.
	HistorySince time.Time
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
	// History is the digest's items, kept for the weekly section, and the
	// pruning of older ones. Only a run that made a digest carries it; nil
	// keeps nothing and prunes nothing.
	History *NewsDigestHistory
}

// NewsDigestHistory is what one digest-making run keeps of its items.
type NewsDigestHistory struct {
	// CoversFrom is the start of the span the run's digest covers. The first
	// run that keeps items makes it the history's start. Required.
	CoversFrom time.Time
	// Items are the digest's items, in its order. Never empty: a digest
	// always has an item.
	Items []NewsDigestKeptItem
	// KeepSince prunes every kept item of a run that ended before it.
	// Required, and never after the run's end.
	KeepSince time.Time
}

// NewsDigestKeptItem is one item of a digest, as kept.
type NewsDigestKeptItem struct {
	// Item is the encoded frontend.v1.NewsDigestItem. Required.
	Item []byte
	// Risk is the one-line reason the item could regress agent-repl; empty
	// when the model did not mark it.
	Risk string
}

// NewsDigestRisk is one kept item that was marked as a regression risk.
type NewsDigestRisk struct {
	// RunEnd is when the run that kept it ended.
	RunEnd time.Time
	// Item is the encoded frontend.v1.NewsDigestItem.
	Item []byte
	// Reason is why it could regress agent-repl. Never empty.
	Reason string
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
			overlay    []byte
			madeAt     sql.NullInt64
			since      sql.NullInt64
		)
		loaded := NewsDigestState{Snapshots: map[string]string{}}
		err := s.db().QueryRowContext(ctx,
			`SELECT last_run_end, baseline, latest_digest_id, standing, latest_overlay, latest_made_at, history_since FROM news_digest WHERE id = 1`).
			Scan(&lastRunEnd, &baseline, &latest, &standing, &overlay, &madeAt, &since)
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
			loaded.LatestOverlay = overlay
			if madeAt.Valid {
				loaded.LatestMadeAt = fromNanos(madeAt.Int64)
			}
			if since.Valid {
				loaded.HistorySince = fromNanos(since.Int64)
			}
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
		"sources": len(run.Snapshots), "digest": run.Digest != nil, "kept_items": keptItems(run.History)}
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
				`UPDATE news_digest SET latest_digest_id = ?, standing = ?, latest_overlay = ?, latest_made_at = ? WHERE id = 1`,
				run.Digest.ID, run.Digest.Overlay, run.Digest.Overlay, nanos(run.EndedAt)); err != nil {
				return err
			}
		}
		if run.History != nil {
			return keepNewsDigestHistory(ctx, tx, run.EndedAt, *run.History)
		}
		return nil
	})
}

// keepNewsDigestHistory keeps one run's items, starts the history's span when
// it is the first run to keep any, and prunes the items of runs that ended
// before KeepSince. tx is the run's recording transaction.
func keepNewsDigestHistory(ctx context.Context, tx *sql.Tx, ended time.Time, h NewsDigestHistory) error {
	for i, it := range h.Items {
		var risk sql.NullString
		if it.Risk != "" {
			risk = sql.NullString{String: it.Risk, Valid: true}
		}
		if _, err := tx.ExecContext(ctx,
			`INSERT INTO news_digest_items (run_end, position, item, risk) VALUES (?, ?, ?, ?)`,
			nanos(ended), i, it.Item, risk); err != nil {
			return err
		}
	}
	if _, err := tx.ExecContext(ctx,
		`UPDATE news_digest SET history_since = COALESCE(history_since, ?) WHERE id = 1`, nanos(h.CoversFrom)); err != nil {
		return err
	}
	_, err := tx.ExecContext(ctx, `DELETE FROM news_digest_items WHERE run_end < ?`, nanos(h.KeepSince))
	return err
}

// keptItems counts the items a run keeps, for its record.
func keptItems(h *NewsDigestHistory) int {
	if h == nil {
		return 0
	}
	return len(h.Items)
}

// NewsDigestRisksSince loads every kept item marked as a regression risk
// whose run ended at or after since, oldest run first and in each run's order.
func (s *store) NewsDigestRisksSince(ctx context.Context, since time.Time) ([]NewsDigestRisk, error) {
	var out []NewsDigestRisk
	fields := dlog.Context{"since": since.UTC().Format(time.RFC3339Nano)}
	err := s.read(ctx, "daemon.wsm.news_digest_risks_since", fields, func(ctx context.Context) error {
		rows, err := s.db().QueryContext(ctx,
			`SELECT run_end, position, item, risk FROM news_digest_items
			 WHERE run_end >= ? AND risk IS NOT NULL ORDER BY run_end, position`, nanos(since))
		if err != nil {
			return err
		}
		defer rows.Close()
		var loaded []NewsDigestRisk
		for rows.Next() {
			var (
				runEnd, position int64
				item             []byte
				risk             string
			)
			if err := rows.Scan(&runEnd, &position, &item, &risk); err != nil {
				return err
			}
			if risk == "" {
				return &DecodeError{Table: "news_digest_items", Row: fmt.Sprintf("%d/%d", runEnd, position), Field: "risk",
					Err: errors.New("a marked item carries its reason")}
			}
			loaded = append(loaded, NewsDigestRisk{RunEnd: fromNanos(runEnd), Item: item, Reason: risk})
		}
		if err := rows.Err(); err != nil {
			return err
		}
		out = loaded
		return nil
	})
	return out, err
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
	case run.History != nil && run.Digest == nil:
		return errors.New("wsm: only a run that made a digest keeps its items")
	}
	if h := run.History; h != nil {
		switch {
		case h.CoversFrom.IsZero() || h.CoversFrom.After(run.EndedAt):
			return errors.New("wsm: kept news digest items state the span they cover, ending by the run's end")
		case h.KeepSince.IsZero() || h.KeepSince.After(run.EndedAt):
			return errors.New("wsm: kept news digest items state what is pruned, never past the run's end")
		case len(h.Items) == 0:
			return errors.New("wsm: a run that keeps a digest's items keeps at least one")
		}
		for _, it := range h.Items {
			if len(it.Item) == 0 {
				return errors.New("wsm: a kept news digest item carries its encoding")
			}
		}
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

// RestandNewsDigest stands the newest digest minted again from the overlay
// kept beside it, answering true when id names that digest and it was down. An
// id naming any other digest, a digest already standing, or one minted before
// layout 20 kept its overlay answers false and changes nothing.
func (s *store) RestandNewsDigest(ctx context.Context, id string) (bool, error) {
	const op = "daemon.wsm.restand_news_digest"
	fields := dlog.Context{"digest": id}
	if id == "" {
		err := errors.New("wsm: a restand names the digest it stands again")
		s.log.Error(op, "refused a restand naming no digest", withError(fields, err))
		return false, err
	}
	var matched bool
	err := s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		res, err := tx.ExecContext(ctx,
			`UPDATE news_digest SET standing = latest_overlay
			 WHERE id = 1 AND latest_digest_id = ? AND standing IS NULL AND latest_overlay IS NOT NULL`, id)
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

// NoteEditorInstance records the Emacs process identity a WatchDaemon carried
// and answers whether it is NEW: different from the last one recorded, or the
// first ever. The compare and the write are one transaction, so two streams of
// one new Emacs racing each other see it new exactly once.
func (s *store) NoteEditorInstance(ctx context.Context, instance string, at time.Time) (bool, error) {
	const op = "daemon.wsm.note_editor_instance"
	fields := dlog.Context{"instance": instance}
	if instance == "" {
		err := errors.New("wsm: an editor instance is never empty")
		s.log.Error(op, "refused an empty editor instance", withError(fields, err))
		return false, err
	}
	var isNew bool
	err := s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		var last string
		err := tx.QueryRowContext(ctx, `SELECT value FROM editor_instance WHERE id = 1`).Scan(&last)
		switch {
		case errors.Is(err, sql.ErrNoRows):
		case err != nil:
			return err
		}
		isNew = last != instance
		_, err = tx.ExecContext(ctx,
			`INSERT INTO editor_instance (id, value, seen_at) VALUES (1, ?, ?)
			 ON CONFLICT(id) DO UPDATE SET value = excluded.value, seen_at = excluded.seen_at`, instance, nanos(at))
		return err
	})
	return isNew, err
}
