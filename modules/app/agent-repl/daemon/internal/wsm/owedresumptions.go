package wsm

import (
	"context"
	"database/sql"
	"time"

	"agent-repld/internal/dlog"
)

// This file is the durable half of RESUMING A TURN THE DAEMON'S OWN STAND-DOWN
// INTERRUPTED (PROMPT_ORIGIN_RESUME_AFTER_RESTART): the record a stand-down
// leaves for every turn it cut, and the one claim that pays it back.
//
// THE ROW IS THE DEBT, AND DELETING IT IS THE PAYMENT. A stand-down owes the
// workspace a resumption for each turn it cut; the next session that serves
// the workspace claims every row it owes in ONE transaction and is handed the
// count, so a second claim (a second boot, a second session-up edge) finds
// nothing and resumes nothing. A daemon that died between the record and the
// claim leaves the row, which is exactly what the next boot pays.

// owedResumptionsDDL adds owed_resumptions: one row per turn a stand-down this
// daemon ordered cut. It references no turn row on purpose: the cut turn
// closes through the prompt queue's door like any other, and the debt is about
// the workspace's conversation, not the turn's record.
//
// The fresh-file schema appends this same text (schema.go), so a migrated file
// and a created one cannot differ.
const owedResumptionsDDL = `
CREATE TABLE owed_resumptions (
  turn_id        TEXT PRIMARY KEY,
  workspace_id   TEXT NOT NULL,
  interrupted_at INTEGER NOT NULL
);
CREATE INDEX owed_resumptions_by_workspace ON owed_resumptions (workspace_id);
`

// OweResumption records that a stand-down this daemon ordered cut turn on ws.
// Recording the same turn twice keeps the first instant.
func (s *store) OweResumption(ctx context.Context, ws WorkspaceID, turn TurnID, at time.Time) error {
	const op = "daemon.wsm.owe_resumption"
	fields := dlog.Context{"workspace": string(ws), "turn": string(turn), "interrupted_at": at}
	return s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		_, err := tx.ExecContext(ctx,
			`INSERT INTO owed_resumptions (turn_id, workspace_id, interrupted_at) VALUES (?, ?, ?)
			 ON CONFLICT(turn_id) DO NOTHING`, turn, ws, nanos(at))
		return err
	})
}

// ClaimResumptions takes every resumption ws is owed, in one transaction, and
// answers how many it took. Zero means nothing is owed, or another claim took
// it first: the caller resumes nothing.
func (s *store) ClaimResumptions(ctx context.Context, ws WorkspaceID) (int, error) {
	const op = "daemon.wsm.claim_resumptions"
	var claimed int
	err := s.write(ctx, op, dlog.Context{"workspace": string(ws)}, func(ctx context.Context, tx *sql.Tx) error {
		res, err := tx.ExecContext(ctx, `DELETE FROM owed_resumptions WHERE workspace_id = ?`, ws)
		if err != nil {
			return err
		}
		n, err := res.RowsAffected()
		if err != nil {
			return err
		}
		claimed = int(n)
		return nil
	})
	if err != nil {
		return 0, err
	}
	return claimed, nil
}

// OwedTurns answers the turns ws is owed a resumption for, without claiming
// them: what the boot reads to tell an ordered cut from a death.
func (s *store) OwedTurns(ctx context.Context, ws WorkspaceID) ([]TurnID, error) {
	const op = "daemon.wsm.owed_turns"
	var out []TurnID
	err := s.read(ctx, op, dlog.Context{"workspace": string(ws)}, func(ctx context.Context) error {
		rows, err := s.q(ctx).QueryContext(ctx, `SELECT turn_id FROM owed_resumptions WHERE workspace_id = ? ORDER BY interrupted_at, turn_id`, ws)
		if err != nil {
			return err
		}
		defer rows.Close()
		for rows.Next() {
			var turn TurnID
			if err := rows.Scan(&turn); err != nil {
				return err
			}
			out = append(out, turn)
		}
		return rows.Err()
	})
	if err != nil {
		return nil, err
	}
	return out, nil
}
