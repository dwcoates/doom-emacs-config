package wsm

import (
	"context"
	"database/sql"
	"errors"
	"time"

	"agent-repld/internal/dlog"
)

// THE PRODUCTION PAUSE (agentrepl.v1.UpdateProductionPause). A repository's
// production checks can be paused: while they are, no span is owed, so its
// production mediator is told none. Pausing drops every span owed and not yet
// decided, and a span that ends during the pause is skipped, never owed, so
// no span that ends during a pause is ever reviewed. Resuming starts the
// checks again at the first landing after it: the span that landing closes
// began during the pause and is skipped too, so the first span owed runs from
// that landing to the next. A skipped span still moves where the next span
// starts (skipped_through), so no later span reaches back into the pause.
//
// NO UNDECIDED CHECK EXISTS WHILE A REPOSITORY IS PAUSED. The pause deletes
// them in the write that takes it, and OweProductionCheck inserts none while
// it stands, so a mark, a resumption or a decision on a check during a pause
// finds nothing to touch: the pause is structural, not a flag each reader
// must remember to test.

// productionPauseDDL is the layout-218 addition: each repository's pause of
// its production checks. IT IS ADDITIVE: a new table the previous build never
// names.
//
//   - paused_at: when the standing pause began; NULL while the checks run.
//   - resumed_at: when the checks last resumed; NULL until a first resume.
//   - skipped_through: the end of the last span skipped for the pause, where
//     the next span starts at the latest.
const productionPauseDDL = `
CREATE TABLE production_pauses (
  repo_key        TEXT PRIMARY KEY CHECK (repo_key <> ''),
  paused_at       INTEGER,
  resumed_at      INTEGER,
  skipped_through INTEGER
);
`

// ProductionPause is one repository's production-check standing.
type ProductionPause struct {
	// Paused reports whether the checks are paused; Since is when the pause
	// began, zero when not paused.
	Paused bool
	Since  time.Time
}

// ProductionOwing is what OweProductionCheck did with the span a landing
// closed.
type ProductionOwing string

// The owings.
const (
	// ProductionSpanOwed is a span owed to the mediator.
	ProductionSpanOwed ProductionOwing = "owed"
	// ProductionSpanEmpty is no span: the landing began where the last span
	// ended (overlapping landings), or no earlier deploy is known to start
	// from.
	ProductionSpanEmpty ProductionOwing = "empty"
	// ProductionSpanPaused is a span that ended while the checks were
	// paused, skipped.
	ProductionSpanPaused ProductionOwing = "paused"
	// ProductionSpanBeganInPause is the span the first landing after a resume
	// closed, which began during the pause, skipped.
	ProductionSpanBeganInPause ProductionOwing = "began_in_pause"
)

// ErrProductionPaused refuses a pause of checks already paused.
var ErrProductionPaused = errors.New("wsm: the repository's production checks are already paused")

// ErrProductionNotPaused refuses a resume of checks that are not paused.
var ErrProductionNotPaused = errors.New("wsm: the repository's production checks are not paused")

// PauseProduction pauses REPO's production checks from AT, dropping every
// check owed and not yet decided, in one write. It answers the checks it
// dropped, oldest first: their spans are never reviewed. Checks already
// paused are refused (ErrProductionPaused) and nothing changes.
func (s *store) PauseProduction(ctx context.Context, repo RepoKey, at time.Time) ([]ProductionCheck, error) {
	const op = "daemon.wsm.pause_production"
	fields := dlog.Context{"repo": string(repo), "at": nanos(at)}
	key, err := s.normalizedRepoKey(op, repo, fields)
	if err != nil {
		return nil, err
	}
	var dropped []ProductionCheck
	err = s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		var pausedAt sql.NullInt64
		err := tx.QueryRowContext(ctx, `SELECT paused_at FROM production_pauses WHERE repo_key = ?`, string(key)).Scan(&pausedAt)
		if err != nil && !errors.Is(err, sql.ErrNoRows) {
			return err
		}
		if pausedAt.Valid {
			return ErrProductionPaused
		}
		dropped, err = listRows(ctx, tx,
			`SELECT `+productionCheckColumns+` FROM production_checks WHERE repo_key = ? AND decided_at IS NULL ORDER BY span_until`, scanProductionCheck, string(key))
		if err != nil {
			return err
		}
		if _, err := tx.ExecContext(ctx, `DELETE FROM production_checks WHERE repo_key = ? AND decided_at IS NULL`, string(key)); err != nil {
			return err
		}
		var through sql.NullInt64
		if len(dropped) > 0 {
			through = sql.NullInt64{Int64: nanos(dropped[len(dropped)-1].Until), Valid: true}
		}
		_, err = tx.ExecContext(ctx,
			`INSERT INTO production_pauses (repo_key, paused_at, skipped_through) VALUES (?, ?, ?)
			 ON CONFLICT(repo_key) DO UPDATE SET paused_at = excluded.paused_at,
			   skipped_through = MAX(COALESCE(production_pauses.skipped_through, 0), COALESCE(excluded.skipped_through, 0))`,
			string(key), nanos(at), through)
		return err
	})
	if err != nil {
		return nil, err
	}
	return dropped, nil
}

// ResumeProduction resumes REPO's paused production checks at AT; the next
// landing after it starts them again. Checks that are not paused are refused
// (ErrProductionNotPaused).
func (s *store) ResumeProduction(ctx context.Context, repo RepoKey, at time.Time) error {
	const op = "daemon.wsm.resume_production"
	fields := dlog.Context{"repo": string(repo), "at": nanos(at)}
	key, err := s.normalizedRepoKey(op, repo, fields)
	if err != nil {
		return err
	}
	return s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		res, err := tx.ExecContext(ctx,
			`UPDATE production_pauses SET paused_at = NULL, resumed_at = ? WHERE repo_key = ? AND paused_at IS NOT NULL`, nanos(at), string(key))
		if err != nil {
			return err
		}
		touched, err := touchedRow(res)
		if err != nil {
			return err
		}
		if !touched {
			return ErrProductionNotPaused
		}
		return nil
	})
}

// ProductionPauseOf answers REPO's production-check standing. A repository
// with no row, or whose row stands resumed, is not paused.
func (s *store) ProductionPauseOf(ctx context.Context, repo RepoKey) (ProductionPause, error) {
	const op = "daemon.wsm.production_pause_of"
	fields := dlog.Context{"repo": string(repo)}
	key, err := s.normalizedRepoKey(op, repo, fields)
	if err != nil {
		return ProductionPause{}, err
	}
	var out ProductionPause
	err = s.read(ctx, op, fields, func(ctx context.Context) error {
		var pausedAt sql.NullInt64
		err := s.q(ctx).QueryRowContext(ctx, `SELECT paused_at FROM production_pauses WHERE repo_key = ?`, string(key)).Scan(&pausedAt)
		if errors.Is(err, sql.ErrNoRows) {
			return nil
		}
		if err != nil {
			return err
		}
		if pausedAt.Valid {
			out = ProductionPause{Paused: true, Since: fromNanos(pausedAt.Int64)}
		}
		return nil
	})
	return out, err
}

// productionPauseIn reads KEY's pause row inside TX: when the standing pause
// began and when the checks last resumed (each invalid when not), and where
// the last skipped span ended.
func productionPauseIn(ctx context.Context, tx *sql.Tx, key RepoKey) (pausedAt, resumedAt, skippedThrough sql.NullInt64, err error) {
	err = tx.QueryRowContext(ctx, `SELECT paused_at, resumed_at, skipped_through FROM production_pauses WHERE repo_key = ?`, string(key)).
		Scan(&pausedAt, &resumedAt, &skippedThrough)
	if errors.Is(err, sql.ErrNoRows) {
		err = nil
	}
	return pausedAt, resumedAt, skippedThrough, err
}

// skipProductionSpanIn records inside TX that KEY's span ending UNTIL was
// skipped for a pause, so the next span starts there.
func skipProductionSpanIn(ctx context.Context, tx *sql.Tx, key RepoKey, until time.Time) error {
	_, err := tx.ExecContext(ctx,
		`UPDATE production_pauses SET skipped_through = MAX(COALESCE(skipped_through, 0), ?) WHERE repo_key = ?`, nanos(until), string(key))
	return err
}
