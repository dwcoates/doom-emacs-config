package wsm

import (
	"context"
	"database/sql"
	"errors"
	"fmt"

	"claude-repld/internal/dlog"
)

// revealGapsDDL is the layout-27 addition (docs/protobuf-design/
// streaming-reveal-window.md): the rolling windows of gaps between consecutive
// streamed fragments, per model and per block kind, that the daemon averages
// into frontend.v1.FeedResponseRevealWindow. Kept apart from the rest of the
// schema for the reason portedPromptsDDL is: a fresh file gets it as part of
// schemaDDL, a layout-26 file from the 26 -> 27 migration.
//
// A window is its rows in position order, oldest first. It belongs to no
// workspace: a model streams at the same cadence whichever workspace runs it.
const revealGapsDDL = `
CREATE TABLE reveal_gaps (
  model    TEXT NOT NULL,
  kind     TEXT NOT NULL,
  position INTEGER NOT NULL,
  gap_ms   INTEGER NOT NULL,
  PRIMARY KEY (model, kind, position)
);
`

// RevealKind is which kind of streamed block a gap window measures. Prose and
// thinking are kept apart because nothing says they stream at one cadence.
type RevealKind string

// The two block kinds, spelled as the reveal_gaps row stores them.
const (
	RevealKindProse    RevealKind = "prose"
	RevealKindThinking RevealKind = "thinking"
)

// valid reports whether k is one of the two kinds.
func (k RevealKind) valid() bool {
	return k == RevealKindProse || k == RevealKindThinking
}

// RevealGapWindow is one model's window of fragment gaps for one block kind.
type RevealGapWindow struct {
	// Model is the model id the gaps were measured under.
	Model string
	// Kind is the block kind the gaps were measured in.
	Kind RevealKind
	// GapsMs are the gaps in milliseconds, oldest first.
	GapsMs []int64
}

// PutRevealGapWindow replaces the stored window of w's model and kind with w,
// all or nothing. An empty model, an unknown kind, an empty window, or a
// negative gap is a bug in the caller (the pacer only ever holds measured
// gaps), so it is refused loudly rather than persisted.
func (s *store) PutRevealGapWindow(ctx context.Context, w RevealGapWindow) error {
	const op = "daemon.wsm.put_reveal_gap_window"
	fields := dlog.Context{"model": w.Model, "kind": string(w.Kind), "gaps": len(w.GapsMs)}
	if err := validRevealGapWindow(w); err != nil {
		s.log.Error(op, "refused an invalid reveal gap window", withError(fields, err))
		return err
	}
	return s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		if _, err := tx.ExecContext(ctx,
			`DELETE FROM reveal_gaps WHERE model = ? AND kind = ?`, w.Model, string(w.Kind)); err != nil {
			return err
		}
		for position, gap := range w.GapsMs {
			if _, err := tx.ExecContext(ctx,
				`INSERT INTO reveal_gaps (model, kind, position, gap_ms) VALUES (?, ?, ?, ?)`,
				w.Model, string(w.Kind), position, gap); err != nil {
				return err
			}
		}
		return nil
	})
}

// validRevealGapWindow answers why w cannot be stored, or nil.
func validRevealGapWindow(w RevealGapWindow) error {
	switch {
	case w.Model == "":
		return errors.New("wsm: a reveal gap window needs its model")
	case !w.Kind.valid():
		return fmt.Errorf("wsm: unknown reveal kind %q", w.Kind)
	case len(w.GapsMs) == 0:
		return errors.New("wsm: a reveal gap window holds at least one gap")
	}
	for _, gap := range w.GapsMs {
		if gap < 0 {
			return fmt.Errorf("wsm: a reveal gap is never negative, got %d", gap)
		}
	}
	return nil
}

// RevealGapWindows loads every stored window, ordered by model then kind. A
// row with an unknown kind, a negative gap, or a window whose positions are
// not 0..n-1 is a corrupt record and is surfaced as a DecodeError.
func (s *store) RevealGapWindows(ctx context.Context) ([]RevealGapWindow, error) {
	var out []RevealGapWindow
	err := s.read(ctx, "daemon.wsm.reveal_gap_windows", dlog.Context{}, func(ctx context.Context) error {
		rows, err := s.db().QueryContext(ctx,
			`SELECT model, kind, position, gap_ms FROM reveal_gaps ORDER BY model, kind, position`)
		if err != nil {
			return err
		}
		defer rows.Close()
		var loaded []RevealGapWindow
		for rows.Next() {
			var (
				model, kind string
				position    int
				gap         int64
			)
			if err := rows.Scan(&model, &kind, &position, &gap); err != nil {
				return err
			}
			key := fmt.Sprintf("%s/%s/%d", model, kind, position)
			if !RevealKind(kind).valid() {
				return &DecodeError{Table: "reveal_gaps", Row: key, Field: "kind", Err: fmt.Errorf("a stored kind is prose or thinking")}
			}
			if gap < 0 {
				return &DecodeError{Table: "reveal_gaps", Row: key, Field: "gap_ms", Err: fmt.Errorf("a stored gap is never negative")}
			}
			last := len(loaded) - 1
			if last < 0 || loaded[last].Model != model || loaded[last].Kind != RevealKind(kind) {
				loaded = append(loaded, RevealGapWindow{Model: model, Kind: RevealKind(kind)})
				last++
			}
			if position != len(loaded[last].GapsMs) {
				return &DecodeError{Table: "reveal_gaps", Row: key, Field: "position", Err: fmt.Errorf("a window's positions run 0..n-1")}
			}
			loaded[last].GapsMs = append(loaded[last].GapsMs, gap)
		}
		if err := rows.Err(); err != nil {
			return err
		}
		out = loaded
		return nil
	})
	return out, err
}
