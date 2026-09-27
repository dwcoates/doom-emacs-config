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

// bumpLastActivity advances a workspace's last-activity stamp to at, in the
// caller's transaction, so a turn write and the activity it represents commit
// together and a turn can never be recorded without the stamp moving.
//
// IT ONLY EVER MOVES FORWARD. `MAX(existing, at)` means an out-of-order write —
// a fleet rollout re-PUTTING an in-flight turn to mark it displaced, which
// carries the turn's ORIGINAL (older) start — cannot pull the stamp back to a
// past instant and mis-report a handover as fresh activity. A NULL existing
// stamp (the workspace has never taken a turn) reads as 0 through COALESCE, so
// the first real turn always wins.
//
// A write that matched no workspace is NOT an error here: the turn write the
// caller already guarded is the id's existence check, and a turn whose
// workspace vanished under the same transaction is that guard's to catch.
func bumpLastActivity(ctx context.Context, tx *sql.Tx, id WorkspaceID, at time.Time) error {
	_, err := tx.ExecContext(ctx,
		`UPDATE workspaces SET last_activity_at = MAX(COALESCE(last_activity_at, 0), ?) WHERE id = ?`,
		nanos(at), id)
	return err
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
	// A TURN CLOSES ONLY THROUGH THE PROMPT QUEUE'S DOOR, which draws the
	// turn's ending in the feed with the close. A record carrying a close is
	// refused, and the upsert below never touches the close columns, so a
	// re-put of a record read while it was open cannot reopen a turn that
	// closed in between either.
	if t.Close != nil || t.ClosedAt != nil {
		err := errors.New("wsm: PutTurn never writes a close; a turn closes through the prompt queue's door")
		s.log.Error(op, "refused a turn record carrying a close", withError(fields, err))
		return err
	}
	return s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		_, err := tx.ExecContext(ctx,
			`INSERT INTO turns (id, workspace_id, text, origin, address, displaced, started_at, closed_at, close_kind)
			 VALUES (?, ?, ?, ?, ?, ?, ?, NULL, NULL)
			 ON CONFLICT(id) DO UPDATE SET
			   workspace_id = excluded.workspace_id, text = excluded.text, origin = excluded.origin,
			   address = excluded.address, displaced = excluded.displaced, started_at = excluded.started_at`,
			t.ID, t.Workspace, t.Text, t.Origin, address, t.Displaced, nanos(t.StartedAt))
		if err != nil {
			return err
		}
		// A recorded turn IS activity: stamp the workspace's last-activity edge
		// in the same transaction so it advances with the turn, never at
		// compose or select time.
		return bumpLastActivity(ctx, tx, t.Workspace, t.StartedAt)
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
		var ws WorkspaceID
		if err := tx.QueryRowContext(ctx, `SELECT workspace_id FROM turns WHERE id = ?`, turn).Scan(&ws); err != nil {
			if errors.Is(err, sql.ErrNoRows) {
				return fmt.Errorf("wsm: turn %s: %w", turn, ErrNotFound)
			}
			return err
		}
		res, err := tx.ExecContext(ctx, `UPDATE turns SET closed_at = ?, close_kind = ? WHERE id = ?`, nanos(at), int(how), turn)
		if err != nil {
			return err
		}
		if err := requireOneRow(res, fmt.Sprintf("wsm: turn %s", turn)); err != nil {
			return err
		}
		// A turn closing IS activity — a response settled — so the workspace's
		// last-activity edge advances to the close instant in the same
		// transaction.
		return bumpLastActivity(ctx, tx, ws, at)
	})
}

// TurnCloses answers the recorded close of each named turn of one workspace
// that has one, all-or-nothing. An open turn, and a turn this workspace never
// recorded, is simply absent.
func (s *store) TurnCloses(ctx context.Context, id WorkspaceID, turns []TurnID) (map[TurnID]RecordedClose, error) {
	out := make(map[TurnID]RecordedClose, len(turns))
	if len(turns) == 0 {
		return out, nil
	}
	err := s.read(ctx, "daemon.wsm.turn_closes", dlog.Context{"workspace": string(id), "turns": len(turns)}, func(ctx context.Context) error {
		for _, turn := range turns {
			t, err := scanTurn(s.db().QueryRowContext(ctx, `SELECT `+turnColumns+` FROM turns WHERE id = ? AND workspace_id = ?`, turn, id))
			if errors.Is(err, sql.ErrNoRows) {
				continue
			}
			if err != nil {
				return err
			}
			if t.Close != nil {
				out[turn] = RecordedClose{How: *t.Close, At: *t.ClosedAt}
			}
		}
		return nil
	})
	if err != nil {
		return nil, err
	}
	return out, nil
}

// RecordedTurns answers which of the named turns this workspace RECORDED,
// open or closed, all-or-nothing. A turn another workspace recorded, and a
// turn no workspace ever recorded, is simply absent.
//
// It is the durable statement of "this workspace opened this turn": every turn
// the daemon delivers is recorded against its workspace before it is
// delivered, so a turn stamp the answer names is the workspace's own work, and
// one it omits is a turn the workspace never ran (a fork's inherited past).
func (s *store) RecordedTurns(ctx context.Context, id WorkspaceID, turns []TurnID) (map[TurnID]bool, error) {
	out := make(map[TurnID]bool, len(turns))
	if len(turns) == 0 {
		return out, nil
	}
	err := s.read(ctx, "daemon.wsm.recorded_turns", dlog.Context{"workspace": string(id), "turns": len(turns)}, func(ctx context.Context) error {
		for _, turn := range turns {
			var one int
			err := s.db().QueryRowContext(ctx, `SELECT 1 FROM turns WHERE id = ? AND workspace_id = ?`, turn, id).Scan(&one)
			if errors.Is(err, sql.ErrNoRows) {
				continue
			}
			if err != nil {
				return err
			}
			out[turn] = true
		}
		return nil
	})
	if err != nil {
		return nil, err
	}
	return out, nil
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

// HasTurns reports whether a workspace has EVER recorded a turn.
//
// IT IS DELIBERATELY KEYED ON THE WORKSPACE, because that is how the turns
// table is keyed: no turn row names the vendor conversation it belonged to. So
// the answer is "this workspace has been engaged", not "this conversation
// has", and a workspace that rotated conversations answers TRUE for a freshly
// minted id that never took a turn. That direction is the safe one — its
// caller reads a true as "cannot prove this conversation was never engaged"
// and stays loud — and a caller must never read a true as proof of engagement
// of one particular conversation.
func (s *store) HasTurns(ctx context.Context, id WorkspaceID) (bool, error) {
	var has bool
	err := s.read(ctx, "daemon.wsm.has_turns", dlog.Context{"workspace": string(id)}, func(ctx context.Context) error {
		var one int
		err := s.db().QueryRowContext(ctx, `SELECT 1 FROM turns WHERE workspace_id = ? LIMIT 1`, id).Scan(&one)
		if errors.Is(err, sql.ErrNoRows) {
			has = false
			return nil
		}
		if err != nil {
			return err
		}
		has = true
		return nil
	})
	if err != nil {
		return false, err
	}
	return has, nil
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

// ClaimIdempotencyKey binds a client's key to a turn, in ONE transaction that
// both reads what stands on the key and decides it.
//
// A DUPLICATE IS ONLY EVER A SUBMISSION THE QUEUE ACCEPTED. A key claimed for
// a submission that never reached acceptance -- its queue call hung, errored,
// was cancelled, or its process died between the claim and the acceptance --
// is not a turn anybody delivered, and refusing its retry is how a prompt was
// lost (2026-09-27: three prompts claimed, never delivered, re-driven under the
// same keys after a respawn, refused, and dropped by the client).
//
// So the claim answers one of three standings:
//
//   - no row: the key is bound to the offered turn, unaccepted (ClaimMinted);
//   - an ACCEPTED row: the accepted turn, and nothing is bound (ClaimAccepted);
//   - an UNACCEPTED row: the retry is re-driven under the turn the claim was
//     ALREADY bound to (ClaimRedriven); the offered turn is never used.
//
// THE RETRY KEEPS THE ORIGINAL TURN ID. The one window the stamp cannot close
// is a process death after the shim accepted the turn and before the stamp
// committed; the shim answers a repeated start of a turn id it already
// accepted as a no-op, so re-driving under the SAME id is what makes that
// window deliver once. A fresh id would be a second turn the shim cannot
// recognize.
//
// DURABLE EVIDENCE THE QUEUE TOOK IT MAKES AN UNSTAMPED CLAIM ACCEPTED, and
// the stamp is written here, in the same transaction as the read, so the
// evidence and the claim cannot disagree afterwards:
//
//   - a row in held_prompts under the claimed turn: the hold is the queue's
//     acceptance record, never deleted (only tombstoned);
//   - the claimed turn's row closed by a vendor terminal (completed, failed,
//     killed): only a turn the shim accepted ever ends through one.
//
// A turn row closed as ORPHANED or AGENT-DIED is no such evidence -- a boot,
// an adoption or a dying shim writes those closes for a turn nobody saw end,
// accepted or not -- so it is REOPENED here (its close cleared) before the
// retry re-drives it: the turn is about to run again under the same id, and a
// row reading closed would hide it from every open-turn read. An open turn row
// is re-driven as it stands; the queue answers a submission whose turn is
// already in flight without delivering it again.
//
// The claim lives in its own table rather than on the turn row because a key is
// claimed at submission, before the turn row exists.
func (s *store) ClaimIdempotencyKey(ctx context.Context, id WorkspaceID, key string, turn TurnID) (IdempotencyClaim, error) {
	const op = "daemon.wsm.claim_idempotency_key"
	fields := dlog.Context{"workspace": string(id), "turn": string(turn), "idempotency_key": key}
	if key == "" {
		err := errors.New("wsm: empty idempotency key")
		s.log.Error(op, "refused an empty idempotency key", withError(fields, err))
		return IdempotencyClaim{}, err
	}
	var claim IdempotencyClaim
	err := s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		var (
			claimed  TurnID
			accepted sql.NullInt64
		)
		err := tx.QueryRowContext(ctx,
			`SELECT turn_id, accepted_at FROM idempotency_keys WHERE workspace_id = ? AND idempotency_key = ?`,
			id, key).Scan(&claimed, &accepted)
		if errors.Is(err, sql.ErrNoRows) {
			_, err = tx.ExecContext(ctx,
				`INSERT INTO idempotency_keys (workspace_id, idempotency_key, turn_id, claimed_at, accepted_at) VALUES (?, ?, ?, ?, NULL)`,
				id, key, turn, nanos(time.Now().UTC()))
			claim = IdempotencyClaim{Standing: ClaimMinted, Turn: turn}
			return err
		}
		if err != nil {
			return err
		}
		if accepted.Valid {
			claim = IdempotencyClaim{Standing: ClaimAccepted, Turn: claimed}
			return nil
		}
		evidence, err := acceptanceEvidence(ctx, tx, id, claimed)
		if err != nil {
			return err
		}
		if evidence != "" {
			if _, err := tx.ExecContext(ctx,
				`UPDATE idempotency_keys SET accepted_at = ? WHERE workspace_id = ? AND idempotency_key = ?`,
				nanos(time.Now().UTC()), id, key); err != nil {
				return err
			}
			claim = IdempotencyClaim{Standing: ClaimAccepted, Turn: claimed, Evidence: evidence}
			return nil
		}
		reopened, err := reopenAmbiguousClose(ctx, tx, id, claimed)
		if err != nil {
			return err
		}
		if _, err := tx.ExecContext(ctx,
			`UPDATE idempotency_keys SET claimed_at = ? WHERE workspace_id = ? AND idempotency_key = ?`,
			nanos(time.Now().UTC()), id, key); err != nil {
			return err
		}
		claim = IdempotencyClaim{Standing: ClaimRedriven, Turn: claimed, Reopened: reopened}
		return nil
	})
	if err != nil {
		return IdempotencyClaim{}, err
	}
	s.log.Debug(op, "claimed the idempotency key", dlog.Context{
		"workspace": string(id), "idempotency_key": key, "offered_turn": string(turn),
		"standing": claim.Standing.String(), "turn": string(claim.Turn),
		"evidence": claim.Evidence, "reopened": claim.Reopened,
	})
	return claim, nil
}

// acceptanceEvidence names the durable record proving the queue accepted the
// submission bound to turn, read in the claim's transaction; empty when there
// is none. See ClaimIdempotencyKey for what counts.
func acceptanceEvidence(ctx context.Context, tx *sql.Tx, id WorkspaceID, turn TurnID) (string, error) {
	var held int
	if err := tx.QueryRowContext(ctx,
		`SELECT count(*) FROM held_prompts WHERE turn_id = ? AND workspace_id = ?`, turn, id).Scan(&held); err != nil {
		return "", err
	}
	if held > 0 {
		return EvidenceHeld, nil
	}
	var ended int
	if err := tx.QueryRowContext(ctx,
		`SELECT count(*) FROM turns WHERE id = ? AND workspace_id = ? AND close_kind IN (?, ?, ?)`,
		turn, id, int(CloseCompleted), int(CloseFailed), int(CloseKilled)).Scan(&ended); err != nil {
		return "", err
	}
	if ended > 0 {
		return EvidenceTerminal, nil
	}
	return "", nil
}

// reopenAmbiguousClose clears an orphaned or agent-died close on turn, in the
// claim's transaction, and reports whether it did. A turn row with no close,
// one closed by a vendor terminal (acceptanceEvidence answered those first),
// and a turn with no row at all are left exactly as they are.
func reopenAmbiguousClose(ctx context.Context, tx *sql.Tx, id WorkspaceID, turn TurnID) (bool, error) {
	res, err := tx.ExecContext(ctx,
		`UPDATE turns SET closed_at = NULL, close_kind = NULL WHERE id = ? AND workspace_id = ? AND close_kind IN (?, ?)`,
		turn, id, int(CloseOrphaned), int(CloseAgentDied))
	if err != nil {
		return false, err
	}
	n, err := res.RowsAffected()
	if err != nil {
		return false, err
	}
	return n == 1, nil
}

// AcceptIdempotencyKey stamps the claim on key accepted: the queue took the
// submission bound to it under turn. A key not bound to that turn -- never
// claimed, rebound by a later retry, or already accepted -- is refused, because
// stamping it would mark a DIFFERENT submission accepted than the one the
// queue took.
func (s *store) AcceptIdempotencyKey(ctx context.Context, id WorkspaceID, key string, turn TurnID) error {
	const op = "daemon.wsm.accept_idempotency_key"
	fields := dlog.Context{"workspace": string(id), "turn": string(turn), "idempotency_key": key}
	if key == "" {
		err := errors.New("wsm: empty idempotency key")
		s.log.Error(op, "refused to accept an empty idempotency key", withError(fields, err))
		return err
	}
	err := s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		res, err := tx.ExecContext(ctx,
			`UPDATE idempotency_keys SET accepted_at = ?
			 WHERE workspace_id = ? AND idempotency_key = ? AND turn_id = ? AND accepted_at IS NULL`,
			nanos(time.Now().UTC()), id, key, turn)
		if err != nil {
			return err
		}
		n, err := res.RowsAffected()
		if err != nil {
			return err
		}
		if n != 1 {
			return fmt.Errorf("wsm: no unaccepted claim binds key %q to turn %q on %q", key, turn, id)
		}
		return nil
	})
	if err != nil {
		return err
	}
	s.log.Debug(op, "the idempotency claim is accepted", fields)
	return nil
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

// ClaimDisplacedTurn takes a displaced turn exclusively: it clears the mark
// and closes the turn if it is still open, in ONE transaction, and reports
// whether this caller is the one that took it and whether it closed the turn.
//
// EXACTLY-ONCE LIVES HERE, AND IT IS THE DATABASE THAT ARBITRATES. A displaced
// record has two possible owners — the merge's own lease release and the boot
// recovery's sweep — and `WHERE displaced = 1` is what lets only one of them
// win, whatever the schedule: the loser is told false and puts nothing back. A
// turn that does not exist, or was never marked, answers false the same way,
// which is the same instruction: this caller does not own it.
//
// An already-closed turn keeps the close it has; the claim is about the mark.
// A turn still open is closed as CloseOrphaned: its capture's kill never
// produced a terminal anyone saw. The caller is the prompt queue's door, which
// draws that ending in the feed.
func (s *store) ClaimDisplacedTurn(ctx context.Context, turn TurnID, at time.Time) (DisplacedClaim, error) {
	const op = "daemon.wsm.claim_displaced_turn"
	fields := dlog.Context{"turn": string(turn), "at": at}
	var claim DisplacedClaim
	err := s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		var closed sql.NullInt64
		err := tx.QueryRowContext(ctx, `SELECT closed_at FROM turns WHERE id = ? AND displaced = 1`, turn).Scan(&closed)
		if errors.Is(err, sql.ErrNoRows) {
			return nil
		}
		if err != nil {
			return err
		}
		res, err := tx.ExecContext(ctx,
			`UPDATE turns SET displaced = 0,
			   closed_at = COALESCE(closed_at, ?),
			   close_kind = COALESCE(close_kind, ?)
			 WHERE id = ? AND displaced = 1`, nanos(at), int(CloseOrphaned), turn)
		if err != nil {
			return err
		}
		if err := requireOneRow(res, fmt.Sprintf("wsm: displaced turn %s", turn)); err != nil {
			return err
		}
		claim = DisplacedClaim{Claimed: true, Closed: !closed.Valid}
		return nil
	})
	if err != nil {
		return DisplacedClaim{}, err
	}
	return claim, nil
}
