package wsm

import (
	"context"
	"database/sql"
	"errors"
	"fmt"
	"time"

	"claude-repld/internal/dlog"
)

// turnColumns is the one select list every turn read shares.
const turnColumns = `id, workspace_id, text, origin, address, displaced, started_at, closed_at, close_kind`

// scanTurn decodes one turn all-or-nothing. A turn's close is a whole: a close
// kind without an instant (or the reverse) is corruption, and an undeclared
// close kind fails the read rather than reading as "completed".
func scanTurn(row interface{ Scan(...any) error }) (Turn, error) {
	var (
		t       Turn
		address sql.NullString
		started int64
		closed  sql.NullInt64
		kind    sql.NullInt64
	)
	if err := row.Scan(&t.ID, &t.Workspace, &t.Text, &t.Origin, &address, &t.Displaced, &started, &closed, &kind); err != nil {
		return Turn{}, err
	}
	id := string(t.ID)
	if address.Valid {
		addr, err := decodeAddress("turns", id, address.String)
		if err != nil {
			return Turn{}, err
		}
		t.Address = addr
	}
	t.StartedAt = fromNanos(started)
	switch {
	case !closed.Valid && !kind.Valid:
	case closed.Valid && kind.Valid:
		how := TurnClose(kind.Int64)
		if !how.valid() {
			return Turn{}, &DecodeError{Table: "turns", Row: id, Field: "close_kind", Err: fmt.Errorf("unknown turn close %d", kind.Int64)}
		}
		at := fromNanos(closed.Int64)
		t.ClosedAt = &at
		t.Close = &how
	default:
		return Turn{}, &DecodeError{Table: "turns", Row: id, Field: "close", Err: errors.New("a turn close is stored whole or not at all")}
	}
	return t, nil
}

// PutTurn records a turn's durable origin and address.
func (s *store) PutTurn(ctx context.Context, t Turn) error {
	const op = "daemon.wsm.put_turn"
	fields := dlog.Context{"workspace": string(t.Workspace), "turn": string(t.ID), "origin": t.Origin, "displaced": t.Displaced}
	address, err := encodeAddress(t.Address)
	if err != nil {
		s.log.Error(op, "refused an unencodable turn address", withError(fields, err))
		return err
	}
	if t.Close != nil && !t.Close.valid() {
		err := fmt.Errorf("wsm: undeclared turn close %d", int(*t.Close))
		s.log.Error(op, "refused an undeclared turn close", withError(fields, err))
		return err
	}
	if (t.Close == nil) != (t.ClosedAt == nil) {
		err := errors.New("wsm: a turn close is written whole or not at all")
		s.log.Error(op, "refused a half-written turn close", withError(fields, err))
		return err
	}
	return s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		var kind any
		if t.Close != nil {
			kind = int(*t.Close)
		}
		_, err := tx.ExecContext(ctx,
			`INSERT INTO turns (id, workspace_id, text, origin, address, displaced, started_at, closed_at, close_kind)
			 VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
			 ON CONFLICT(id) DO UPDATE SET
			   workspace_id = excluded.workspace_id, text = excluded.text, origin = excluded.origin,
			   address = excluded.address, displaced = excluded.displaced, started_at = excluded.started_at,
			   closed_at = excluded.closed_at, close_kind = excluded.close_kind`,
			t.ID, t.Workspace, t.Text, t.Origin, address, t.Displaced, nanos(t.StartedAt), nullNanos(t.ClosedAt), kind)
		return err
	})
}

// CloseTurn stamps a turn's close.
func (s *store) CloseTurn(ctx context.Context, turn TurnID, at time.Time, how TurnClose) error {
	const op = "daemon.wsm.close_turn"
	fields := dlog.Context{"turn": string(turn), "close_kind": int(how), "at": at}
	if !how.valid() {
		err := fmt.Errorf("wsm: undeclared turn close %d", int(how))
		s.log.Error(op, "refused an undeclared turn close", withError(fields, err))
		return err
	}
	return s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		res, err := tx.ExecContext(ctx, `UPDATE turns SET closed_at = ?, close_kind = ? WHERE id = ?`, nanos(at), int(how), turn)
		if err != nil {
			return err
		}
		return requireOneRow(res, fmt.Sprintf("wsm: turn %s", turn))
	})
}

// OpenTurns loads a workspace's turns that have no terminal, all-or-nothing.
func (s *store) OpenTurns(ctx context.Context, id WorkspaceID) ([]Turn, error) {
	var out []Turn
	err := s.read(ctx, "daemon.wsm.open_turns", dlog.Context{"workspace": string(id)}, func(ctx context.Context) error {
		loaded, err := openTurns(ctx, s.db(), id)
		if err != nil {
			return err
		}
		out = loaded
		return nil
	})
	if err != nil {
		return nil, err
	}
	return out, nil
}

// querier is what openTurns needs: the handle or a transaction, so the orphan
// close reads the same rows through the same decoder inside its transaction.
type querier interface {
	QueryContext(ctx context.Context, query string, args ...any) (*sql.Rows, error)
}

// openTurns loads the turns with no terminal, all-or-nothing.
func openTurns(ctx context.Context, q querier, id WorkspaceID) ([]Turn, error) {
	rows, err := q.QueryContext(ctx, `SELECT `+turnColumns+` FROM turns WHERE workspace_id = ? AND closed_at IS NULL ORDER BY started_at, id`, id)
	if err != nil {
		return nil, err
	}
	defer rows.Close()
	var loaded []Turn
	for rows.Next() {
		t, err := scanTurn(rows)
		if err != nil {
			return nil, err
		}
		loaded = append(loaded, t)
	}
	if err := rows.Err(); err != nil {
		return nil, err
	}
	return loaded, nil
}

// ClaimIdempotencyKey binds a client's key to a turn. When the key was already
// claimed for that workspace it returns the EXISTING turn and mints nothing, so
// a retried submission can never open a second turn.
//
// The claim lives in its own table rather than on the turn row because a key is
// claimed at submission, before the turn row exists.
func (s *store) ClaimIdempotencyKey(ctx context.Context, id WorkspaceID, key string, turn TurnID) (*TurnID, error) {
	const op = "daemon.wsm.claim_idempotency_key"
	fields := dlog.Context{"workspace": string(id), "turn": string(turn), "idempotency_key": key}
	if key == "" {
		err := errors.New("wsm: empty idempotency key")
		s.log.Error(op, "refused an empty idempotency key", withError(fields, err))
		return nil, err
	}
	var existing *TurnID
	err := s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		var claimed TurnID
		err := tx.QueryRowContext(ctx, `SELECT turn_id FROM idempotency_keys WHERE workspace_id = ? AND idempotency_key = ?`, id, key).Scan(&claimed)
		if err == nil {
			existing = &claimed
			return nil
		}
		if !errors.Is(err, sql.ErrNoRows) {
			return err
		}
		_, err = tx.ExecContext(ctx, `INSERT INTO idempotency_keys (workspace_id, idempotency_key, turn_id, claimed_at) VALUES (?, ?, ?, ?)`,
			id, key, turn, nanos(time.Now().UTC()))
		return err
	})
	if err != nil {
		return nil, err
	}
	return existing, nil
}

// CloseOrphans closes every turn without a terminal in ONE transaction and
// reports what it closed. Held prompts on the workspace are left exactly as
// they are — holds survive a shutdown — and the session's engagement is stamped
// in the same transaction, so a crash mid-teardown can never leave half the
// bookkeeping done.
func (s *store) CloseOrphans(ctx context.Context, id WorkspaceID, at time.Time) (OrphanReport, error) {
	var report OrphanReport
	err := s.write(ctx, "daemon.wsm.close_orphans", dlog.Context{"workspace": string(id), "at": at}, func(ctx context.Context, tx *sql.Tx) error {
		// Reading through the same all-or-nothing decoder is what makes a
		// corrupt turn row abort the whole teardown rather than close a subset.
		open, err := openTurns(ctx, tx, id)
		if err != nil {
			return err
		}
		closed := make([]TurnID, 0, len(open))
		for _, t := range open {
			if _, err := tx.ExecContext(ctx, `UPDATE turns SET closed_at = ?, close_kind = ? WHERE id = ?`,
				nanos(at), int(CloseOrphaned), t.ID); err != nil {
				return err
			}
			closed = append(closed, t.ID)
		}
		if _, err := tx.ExecContext(ctx, `UPDATE sessions SET last_engagement_at = ? WHERE workspace_id = ?`, nanos(at), id); err != nil {
			return err
		}
		report = OrphanReport{Turns: closed, At: at.UTC()}
		return nil
	})
	if err != nil {
		return OrphanReport{}, err
	}
	return report, nil
}

// AllDisplacedTurns loads every turn still marked displaced, all-or-nothing.
//
// THE READ IS NOT RESTRICTED TO OPEN TURNS. Capturing a displaced turn ENDS
// it, and the boot before this one closes whatever the crash left open, so by
// the time a recovery reads them the records it must put back are closed. The
// mark, not the close, is what says a turn is still owed to its user.
func (s *store) AllDisplacedTurns(ctx context.Context) ([]Turn, error) {
	var out []Turn
	err := s.read(ctx, "daemon.wsm.all_displaced_turns", nil, func(ctx context.Context) error {
		rows, err := s.db().QueryContext(ctx, `SELECT `+turnColumns+` FROM turns WHERE displaced = 1 ORDER BY started_at, id`)
		if err != nil {
			return err
		}
		defer rows.Close()
		var loaded []Turn
		for rows.Next() {
			t, err := scanTurn(rows)
			if err != nil {
				return err
			}
			loaded = append(loaded, t)
		}
		if err := rows.Err(); err != nil {
			return err
		}
		out = loaded
		return nil
	})
	if err != nil {
		return nil, err
	}
	return out, nil
}

// RetireDisplacedTurn clears the displaced mark and closes the turn if it is
// still open, in ONE statement.
//
// EXACTLY-ONCE LIVES HERE. Both owners of a displaced record — the merge's own
// release and the boot recovery — retire it through this call, so a record can
// be claimed once and only once: the mark going down is the claim, and it goes
// down durably. An already-closed turn keeps the close it has; the retirement
// is about the mark.
func (s *store) RetireDisplacedTurn(ctx context.Context, turn TurnID, at time.Time) error {
	const op = "daemon.wsm.retire_displaced_turn"
	fields := dlog.Context{"turn": string(turn), "at": at}
	return s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		res, err := tx.ExecContext(ctx,
			`UPDATE turns SET displaced = 0,
			   closed_at = COALESCE(closed_at, ?),
			   close_kind = COALESCE(close_kind, ?)
			 WHERE id = ?`, nanos(at), int(CloseCompleted), turn)
		if err != nil {
			return err
		}
		return requireOneRow(res, fmt.Sprintf("wsm: turn %s", turn))
	})
}
