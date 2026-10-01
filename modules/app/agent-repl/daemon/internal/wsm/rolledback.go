package wsm

import (
	"context"
	"database/sql"
	"errors"
	"fmt"
	"time"

	"claude-repld/internal/dlog"
)

// rolledBackTurnsDDL is the layout-17 addition, kept apart from the rest of the
// schema for the reason recordedRowsDDL is: a fresh file gets it as part of
// schemaDDL, a layout-16 file from the 16 -> 17 migration. A row says a turn
// was rolled back (agentrepl.v1.RollBack): the feed never draws it again.
const rolledBackTurnsDDL = `
CREATE TABLE rolled_back_turns (
  workspace_id   TEXT NOT NULL REFERENCES workspaces(id) ON DELETE CASCADE,
  turn_id        TEXT NOT NULL,
  rolled_back_at INTEGER NOT NULL,
  PRIMARY KEY (workspace_id, turn_id)
);
`

// RecordRolledBackTurns records that a workspace's turns were rolled back, all
// or nothing. Recording a turn twice is not an error: it is as rolled back as
// asked.
func (s *store) RecordRolledBackTurns(ctx context.Context, id WorkspaceID, turns []TurnID) error {
	const op = "daemon.wsm.record_rolled_back_turns"
	fields := dlog.Context{"workspace": string(id), "turns": len(turns)}
	if len(turns) == 0 {
		err := errors.New("wsm: a rollback names at least one turn")
		s.log.Error(op, "refused a rollback that named no turn", withError(fields, err))
		return err
	}
	for _, turn := range turns {
		if turn == "" {
			err := errors.New("wsm: a rolled-back turn needs its id")
			s.log.Error(op, "refused a rollback naming an empty turn", withError(fields, err))
			return err
		}
	}
	at := nanos(nowUTC())
	return s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		for _, turn := range turns {
			if _, err := tx.ExecContext(ctx,
				`INSERT INTO rolled_back_turns (workspace_id, turn_id, rolled_back_at) VALUES (?, ?, ?)
				 ON CONFLICT(workspace_id, turn_id) DO NOTHING`, id, turn, at); err != nil {
				return err
			}
		}
		return nil
	})
}

// RolledBackTurns loads every turn of a workspace that was rolled back.
func (s *store) RolledBackTurns(ctx context.Context, id WorkspaceID) ([]TurnID, error) {
	var out []TurnID
	err := s.read(ctx, "daemon.wsm.rolled_back_turns", dlog.Context{"workspace": string(id)}, func(ctx context.Context) error {
		rows, err := s.db().QueryContext(ctx,
			`SELECT turn_id FROM rolled_back_turns WHERE workspace_id = ? ORDER BY rolled_back_at, turn_id`, id)
		if err != nil {
			return err
		}
		defer rows.Close()
		var loaded []TurnID
		for rows.Next() {
			var turn string
			if err := rows.Scan(&turn); err != nil {
				return err
			}
			loaded = append(loaded, TurnID(turn))
		}
		if err := rows.Err(); err != nil {
			return err
		}
		out = loaded
		return nil
	})
	return out, err
}

// TurnStartedAt answers when a workspace's turn was opened; ErrNotFound when
// the workspace recorded no such turn (a turn the vendor started on its own).
func (s *store) TurnStartedAt(ctx context.Context, id WorkspaceID, turn TurnID) (time.Time, error) {
	var at time.Time
	err := s.read(ctx, "daemon.wsm.turn_started_at", dlog.Context{"workspace": string(id), "turn": string(turn)}, func(ctx context.Context) error {
		var started int64
		err := s.db().QueryRowContext(ctx, `SELECT started_at FROM turns WHERE id = ? AND workspace_id = ?`, turn, id).Scan(&started)
		if errors.Is(err, sql.ErrNoRows) {
			return fmt.Errorf("wsm: turn %s of workspace %s: %w", turn, id, ErrNotFound)
		}
		if err != nil {
			return err
		}
		at = fromNanos(started)
		return nil
	})
	return at, err
}
