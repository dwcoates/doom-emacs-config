package wsm

import (
	"context"
	"database/sql"
	"errors"
	"fmt"
	"time"

	"agent-repld/internal/dlog"
)

// This file is the durable half of the COLD-GATE BACKLOG (owner request,
// 2026-10-26; coldbacklog.Sweeper): a shown workspace whose cold gate has
// stood for over a weekday is moved into the hidden section, drawn as the
// Backlog, exactly as a hide moves it. The row records WHICH GATE it was
// moved for, by the instant that gate's cache lapsed, so a workspace the user
// shows again stays shown until a new conversation lapses into a new gate,
// across every daemon restart that raises the same gate again.

// coldGateBacklogDDL is the layout-106 addition:
// workspaces.cold_gate_backlogged_at, the lapse instant, in nanoseconds, of
// the cold gate the daemon last moved the workspace into the backlog for;
// NULL when it never has.
//
// IT IS ADDITIVE: the previous build's writes name no such column, so a row it
// writes keeps NULL, which is "never backlogged". The fresh-file schema
// appends this same text (schema.go).
const coldGateBacklogDDL = `
ALTER TABLE workspaces ADD COLUMN cold_gate_backlogged_at INTEGER;
`

// BacklogColdGate moves a shown workspace into the hidden section because the
// cold gate whose cache lapsed at GATELAPSEDAT has stood too long, stamping the
// hide at AT and recording the gate, and answers whether it moved it.
//
// THE JUDGMENT AND THE WRITE ARE ONE TRANSACTION, so a workspace the user
// selected, hid, closed or already had backlogged for this same gate
// meanwhile is left as it is (answered false, nothing written), never moved
// out from under them. A workspace with open children is refused by the
// family rules as a hide is (HasOpenChildrenError, family.go), nothing
// written. An unknown workspace is refused (ErrNotFound).
func (s *store) BacklogColdGate(ctx context.Context, id WorkspaceID, gateLapsedAt, at time.Time) (bool, error) {
	fields := dlog.Context{"workspace": string(id), "gate_lapsed_at": gateLapsedAt, "hidden_at": at}
	moved := false
	err := s.write(ctx, "daemon.wsm.backlog_cold_gate", fields, func(ctx context.Context, tx *sql.Tx) error {
		ws, err := scanWorkspace(tx.QueryRowContext(ctx, `SELECT `+workspaceColumns+` FROM workspaces WHERE id = ?`, id))
		if errors.Is(err, sql.ErrNoRows) {
			return fmt.Errorf("wsm: workspace %s: %w", id, ErrNotFound)
		}
		if err != nil {
			return err
		}
		if !ws.BacklogsColdGate(gateLapsedAt) {
			return nil
		}
		if err := refuseOpenChildren(ctx, tx, id); err != nil {
			return err
		}
		moved = true
		return updateWorkspaceColumns(ctx, tx, id,
			append(hideColumns(true, at), workspaceColumn{name: "cold_gate_backlogged_at", value: nanos(gateLapsedAt)})...)
	})
	if err != nil {
		return false, err
	}
	return moved, nil
}

// BacklogsColdGate reports whether a cold gate whose cache lapsed at
// GATELAPSEDAT, once it has stood long enough, moves the workspace into the
// backlog: the workspace is listed in the Open section (ListedOpen), the user
// is not looking at it, and it was not already backlogged for that same gate.
// THE ONE STATEMENT of the judgment: the store makes it in the transaction
// that writes the move (BacklogColdGate), and coldbacklog.Sweeper reads it to
// skip the writes it would decline.
func (ws Workspace) BacklogsColdGate(gateLapsedAt time.Time) bool {
	spent := ws.BackloggedGateAt != nil && ws.BackloggedGateAt.Equal(gateLapsedAt)
	return ws.ListedOpen() && !ws.Current && !spent
}

// errNoBacklogInstant is a decode of a backlog instant that is not positive.
var errNoBacklogInstant = errors.New("a backlogged cold gate records the positive instant its cache lapsed")

// decodeColdGateBacklog rebuilds the backlogged gate's lapse instant.
func decodeColdGateBacklog(id WorkspaceID, backlogged sql.NullInt64) (*time.Time, error) {
	if !backlogged.Valid {
		return nil, nil
	}
	if backlogged.Int64 <= 0 {
		return nil, &DecodeError{Table: "workspaces", Row: string(id), Field: "cold_gate_backlogged_at", Err: errNoBacklogInstant}
	}
	at := fromNanos(backlogged.Int64)
	return &at, nil
}
