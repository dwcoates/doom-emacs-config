package wsm

import (
	"context"
	"database/sql"
	"errors"
	"fmt"
	"math"

	"agent-repld/internal/dlog"
)

// revealGapsDDL is the layout-27 addition (docs/protobuf-design/
// streaming-reveal-window.md): the rolling windows of gaps between consecutive
// streamed FRAGMENTS, per model and per block kind.
//
// RETIRED AT LAYOUT 30, and kept only for the build before it. Those gaps were
// mostly the daemon's own folding pace inside one clump of fragments (2ms) and
// say nothing about when the next clump comes, so averaged with the gaps
// between bursts they would drag the reveal window back to the per-fragment
// figure that paced nothing. Nothing in this build reads or writes the table;
// the build before layout 30 still does while it hands over, which is why the
// table is not dropped here. Dropping it is a breaking step for a later deploy
// (MigrationKind: "A BREAKING CHANGE IS SHIPPED AS TWO DEPLOYS").
const revealGapsDDL = `
CREATE TABLE reveal_gaps (
  model    TEXT NOT NULL,
  kind     TEXT NOT NULL,
  position INTEGER NOT NULL,
  gap_ms   INTEGER NOT NULL,
  PRIMARY KEY (model, kind, position)
);
`

// revealBurstGapsDDL is the layout-30 addition (docs/protobuf-design/
// streaming-reveal-window.md): the rolling windows of gaps from one streamed
// BURST's start to the next's, per model and per block kind, that the daemon
// averaged into the reveal window of the build before layout 31.
// A new table rather than a reset of reveal_gaps, so the build handing over
// can never write a per-fragment gap where this build reads burst gaps. Kept
// apart from the rest of the schema for the reason forkLineageDDL is: a
// fresh file gets it as part of schemaDDL, a layout-29 file from the 29 -> 30
// migration.
//
// RETIRED AT LAYOUT 31, and kept only for the build before it. A gap window
// predicts the time to the next burst afresh for every burst, so one long
// pause owned the figure; the pace is now learned from whole blocks
// (reveal_block_speeds). Nothing in this build reads or writes the table, and
// dropping it is a breaking step for a later deploy (MigrationKind: "A
// BREAKING CHANGE IS SHIPPED AS TWO DEPLOYS").
const revealBurstGapsDDL = `
CREATE TABLE reveal_burst_gaps (
  model    TEXT NOT NULL,
  kind     TEXT NOT NULL,
  position INTEGER NOT NULL,
  gap_ms   INTEGER NOT NULL,
  PRIMARY KEY (model, kind, position)
);
`

// revealBlockSpeedsDDL is the layout-31 addition (docs/protobuf-design/
// learned-rate-reveal.md): the rolling windows of settled blocks' arrival
// speeds, in chars/sec, per model and per block kind, whose low percentiles
// the daemon serves as frontend.v1.FeedResponseRevealPace (internal/revealpace).
// A new table rather than a reuse of reveal_burst_gaps, so the build handing
// over can never write a gap where this build reads a speed. Kept apart from
// the rest of the schema for the reason forkLineageDDL is: a fresh file gets
// it as part of schemaDDL, a layout-30 file from the 30 -> 31 migration.
//
// A window is its rows in position order, oldest first. It belongs to no
// workspace: a model streams at the same speed whichever workspace runs it.
const revealBlockSpeedsDDL = `
CREATE TABLE reveal_block_speeds (
  model            TEXT NOT NULL,
  kind             TEXT NOT NULL,
  position         INTEGER NOT NULL,
  chars_per_second REAL NOT NULL,
  PRIMARY KEY (model, kind, position)
);
`

// RevealKind is which kind of streamed block a speed window measures. Prose
// and thinking are kept apart because they stream at different speeds.
type RevealKind string

// The two block kinds, spelled as a reveal_block_speeds row stores them.
const (
	RevealKindProse    RevealKind = "prose"
	RevealKindThinking RevealKind = "thinking"
)

// valid reports whether k is one of the two kinds.
func (k RevealKind) valid() bool {
	return k == RevealKindProse || k == RevealKindThinking
}

// RevealSpeedWindow is one model's window of block speeds for one block kind.
type RevealSpeedWindow struct {
	// Model is the model id the blocks streamed under.
	Model string
	// Kind is the block kind the speeds were measured in.
	Kind RevealKind
	// CharsPerSecond are the blocks' arrival speeds, oldest first.
	CharsPerSecond []float64
}

// PutRevealSpeedWindow replaces the stored window of w's model and kind with
// w, all or nothing. An empty model, an unknown kind, an empty window, or a
// speed that is not a positive finite number is a bug in the caller (the
// pacer only ever holds measured speeds), so it is refused loudly rather than
// persisted.
func (s *store) PutRevealSpeedWindow(ctx context.Context, w RevealSpeedWindow) error {
	const op = "daemon.wsm.put_reveal_speed_window"
	fields := dlog.Context{"model": w.Model, "kind": string(w.Kind), "speeds": len(w.CharsPerSecond)}
	if err := validRevealSpeedWindow(w); err != nil {
		s.log.Error(op, "refused an invalid reveal speed window", withError(fields, err))
		return err
	}
	return s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		if _, err := tx.ExecContext(ctx,
			`DELETE FROM reveal_block_speeds WHERE model = ? AND kind = ?`, w.Model, string(w.Kind)); err != nil {
			return err
		}
		for position, speed := range w.CharsPerSecond {
			if _, err := tx.ExecContext(ctx,
				`INSERT INTO reveal_block_speeds (model, kind, position, chars_per_second) VALUES (?, ?, ?, ?)`,
				w.Model, string(w.Kind), position, speed); err != nil {
				return err
			}
		}
		return nil
	})
}

// ValidRevealSpeed reports whether v is a block speed: a positive finite
// chars/sec. The one test every holder of a speed applies (the pacer on
// Observe, this store on write and on read).
func ValidRevealSpeed(v float64) bool {
	return v > 0 && !math.IsInf(v, 0)
}

// validRevealSpeedWindow answers why w cannot be stored, or nil.
func validRevealSpeedWindow(w RevealSpeedWindow) error {
	switch {
	case w.Model == "":
		return errors.New("wsm: a reveal speed window needs its model")
	case !w.Kind.valid():
		return fmt.Errorf("wsm: unknown reveal kind %q", w.Kind)
	case len(w.CharsPerSecond) == 0:
		return errors.New("wsm: a reveal speed window holds at least one speed")
	}
	for _, speed := range w.CharsPerSecond {
		if !ValidRevealSpeed(speed) {
			return fmt.Errorf("wsm: a reveal speed is a positive finite chars/sec, got %v", speed)
		}
	}
	return nil
}

// RevealSpeedWindows loads every stored window, ordered by model then kind. A
// row with an unknown kind, a speed that is not a positive finite number, or a
// window whose positions are not 0..n-1 is a corrupt record and is surfaced as
// a DecodeError.
func (s *store) RevealSpeedWindows(ctx context.Context) ([]RevealSpeedWindow, error) {
	var out []RevealSpeedWindow
	err := s.read(ctx, "daemon.wsm.reveal_speed_windows", dlog.Context{}, func(ctx context.Context) error {
		rows, err := s.q(ctx).QueryContext(ctx,
			`SELECT model, kind, position, chars_per_second FROM reveal_block_speeds ORDER BY model, kind, position`)
		if err != nil {
			return err
		}
		defer rows.Close()
		var loaded []RevealSpeedWindow
		for rows.Next() {
			var (
				model, kind string
				position    int
				speed       float64
			)
			if err := rows.Scan(&model, &kind, &position, &speed); err != nil {
				return err
			}
			key := fmt.Sprintf("%s/%s/%d", model, kind, position)
			if !RevealKind(kind).valid() {
				return &DecodeError{Table: "reveal_block_speeds", Row: key, Field: "kind", Err: fmt.Errorf("a stored kind is prose or thinking")}
			}
			if !ValidRevealSpeed(speed) {
				return &DecodeError{Table: "reveal_block_speeds", Row: key, Field: "chars_per_second", Err: fmt.Errorf("a stored speed is a positive finite chars/sec")}
			}
			last := len(loaded) - 1
			if last < 0 || loaded[last].Model != model || loaded[last].Kind != RevealKind(kind) {
				loaded = append(loaded, RevealSpeedWindow{Model: model, Kind: RevealKind(kind)})
				last++
			}
			if position != len(loaded[last].CharsPerSecond) {
				return &DecodeError{Table: "reveal_block_speeds", Row: key, Field: "position", Err: fmt.Errorf("a window's positions run 0..n-1")}
			}
			loaded[last].CharsPerSecond = append(loaded[last].CharsPerSecond, speed)
		}
		if err := rows.Err(); err != nil {
			return err
		}
		out = loaded
		return nil
	})
	return out, err
}
