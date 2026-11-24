package wsm

import (
	"context"
	"database/sql"
	"fmt"

	"agent-repld/internal/dlog"
)

// composerSelectionsDDL is the layout-204 addition: every candidate the reader
// took from a composer's completion, with the instant of the latest taking, so
// the composer offers the most recently taken first from a record the daemon
// owns and that outlives a restart of either side (internal/composerselections).
// IT IS ADDITIVE: one new table the previous build never names.
//
// One row per (kind, name). The record is the user's, not a workspace's, so no
// workspace is named.
const composerSelectionsDDL = `
CREATE TABLE composer_selections (
  kind           TEXT NOT NULL CHECK (kind IN ('command', 'workspace', 'channel')),
  name           TEXT NOT NULL CHECK (name <> ''),
  selected_at_ms INTEGER NOT NULL CHECK (selected_at_ms > 0),
  PRIMARY KEY (kind, name)
);
`

// ComposerSelectionKind is the completion a composer selection was taken from.
type ComposerSelectionKind string

// The three kinds, spelled as the table's CHECK spells them.
const (
	// ComposerSelectionCommand is a slash command, by name without the slash.
	ComposerSelectionCommand ComposerSelectionKind = "command"
	// ComposerSelectionWorkspace is a workspace, by name.
	ComposerSelectionWorkspace ComposerSelectionKind = "workspace"
	// ComposerSelectionChannel is a Slack channel, by name without the hash.
	ComposerSelectionChannel ComposerSelectionKind = "channel"
)

// Known reports whether K is one of the three kinds.
func (k ComposerSelectionKind) Known() bool {
	return k == ComposerSelectionCommand || k == ComposerSelectionWorkspace || k == ComposerSelectionChannel
}

// ComposerSelection is one candidate the reader took, and the instant (epoch
// ms, stamped by the daemon) it was last taken.
type ComposerSelection struct {
	Kind         ComposerSelectionKind
	Name         string
	SelectedAtMs int64
}

// PutComposerSelection records SEL as last taken at its instant, replacing the
// instant the row held. A selection with an unknown kind, an empty name or no
// instant is a bug in the caller, refused loudly before anything is written.
func (s *store) PutComposerSelection(ctx context.Context, sel ComposerSelection) error {
	const op = "daemon.wsm.put_composer_selection"
	fields := dlog.Context{"kind": string(sel.Kind), "name": sel.Name, "selected_at_ms": sel.SelectedAtMs}
	if !sel.Kind.Known() || sel.Name == "" || sel.SelectedAtMs <= 0 {
		err := fmt.Errorf("wsm: a composer selection has a known kind, a name and an instant (%+v)", sel)
		s.log.Error(op, "refused a malformed composer selection", withError(fields, err))
		return err
	}
	return s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		_, err := tx.ExecContext(ctx,
			`INSERT INTO composer_selections (kind, name, selected_at_ms) VALUES (?, ?, ?)
			 ON CONFLICT(kind, name) DO UPDATE SET selected_at_ms = excluded.selected_at_ms`,
			string(sel.Kind), sel.Name, sel.SelectedAtMs)
		return err
	})
}

// ComposerSelections loads every recorded selection, in no particular order. A
// stored kind the table's CHECK should have refused is a DecodeError.
func (s *store) ComposerSelections(ctx context.Context) ([]ComposerSelection, error) {
	var out []ComposerSelection
	err := s.read(ctx, "daemon.wsm.composer_selections", dlog.Context{}, func(ctx context.Context) error {
		out = nil
		rows, err := s.q(ctx).QueryContext(ctx,
			`SELECT kind, name, selected_at_ms FROM composer_selections`)
		if err != nil {
			return err
		}
		defer rows.Close()
		for rows.Next() {
			var sel ComposerSelection
			var kind string
			if err := rows.Scan(&kind, &sel.Name, &sel.SelectedAtMs); err != nil {
				return err
			}
			sel.Kind = ComposerSelectionKind(kind)
			if !sel.Kind.Known() {
				return &DecodeError{Table: "composer_selections", Row: kind + "/" + sel.Name, Field: "kind", Err: fmt.Errorf("unknown kind %q", kind)}
			}
			out = append(out, sel)
		}
		return rows.Err()
	})
	return out, err
}
