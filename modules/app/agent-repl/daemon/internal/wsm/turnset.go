package wsm

import (
	"context"
	"database/sql"
	"errors"
	"fmt"

	"agent-repld/internal/dlog"
)

// turnSet is a durable per-workspace SET OF TURNS the feed treats specially:
// one table keyed (workspace_id, turn_id) with the instant each turn joined.
// Every such set records and loads through the two methods below, so their
// refusals, their idempotence and their order cannot drift between tables.
type turnSet struct {
	// table is the set's table.
	table string
	// atColumn is the table's column holding when a turn joined the set.
	atColumn string
	// recordOp and loadOp are the operations the record and the load log as.
	recordOp, loadOp string
	// what names the act a recording performs ("a rollback"), for refusals.
	what string
}

// record adds TURNS to the workspace's set, all or nothing. Recording a turn
// twice is not an error: it is in the set as asked. A recording that names no
// turn, or an empty one, is refused and logged at ERROR.
func (s *store) recordTurnSet(ctx context.Context, set turnSet, id WorkspaceID, turns []TurnID) error {
	fields := dlog.Context{"workspace": string(id), "turns": len(turns)}
	if len(turns) == 0 {
		err := fmt.Errorf("wsm: %s names at least one turn", set.what)
		s.log.Error(set.recordOp, fmt.Sprintf("refused %s that named no turn", set.what), withError(fields, err))
		return err
	}
	for _, turn := range turns {
		if turn == "" {
			err := errors.New("wsm: a recorded turn needs its id")
			s.log.Error(set.recordOp, fmt.Sprintf("refused %s naming an empty turn", set.what), withError(fields, err))
			return err
		}
	}
	at := nanos(nowUTC())
	insert := fmt.Sprintf(
		`INSERT INTO %s (workspace_id, turn_id, %s) VALUES (?, ?, ?) ON CONFLICT(workspace_id, turn_id) DO NOTHING`,
		set.table, set.atColumn)
	return s.write(ctx, set.recordOp, fields, func(ctx context.Context, tx *sql.Tx) error {
		for _, turn := range turns {
			if _, err := tx.ExecContext(ctx, insert, id, turn, at); err != nil {
				return err
			}
		}
		return nil
	})
}

// loadTurnSet loads every turn in the workspace's set, in the order they
// joined it.
func (s *store) loadTurnSet(ctx context.Context, set turnSet, id WorkspaceID) ([]TurnID, error) {
	var out []TurnID
	query := fmt.Sprintf(`SELECT turn_id FROM %s WHERE workspace_id = ? ORDER BY %s, turn_id`, set.table, set.atColumn)
	err := s.read(ctx, set.loadOp, dlog.Context{"workspace": string(id)}, func(ctx context.Context) error {
		rows, err := s.q(ctx).QueryContext(ctx, query, id)
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
