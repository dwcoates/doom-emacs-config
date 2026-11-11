package wsm

import (
	"context"
	"database/sql"
	"errors"
	"fmt"
	"time"

	"agent-repld/internal/dlog"
)

// This file is the durable record of a PROMPT MOVING FROM ONE TURN TO ANOTHER.
// A prompt is minted a turn when it is submitted, and the daemon moves it to a
// different turn on several paths: it is held again under a new turn (a join
// the vendor gave back, a try the vendor blocked), folded into the turn
// already running, resubmitted as a new turn after the turn it was folded
// into failed, resumed after a stand-down cut its turn, or put back after a
// merge displaced its turn. Each move is one row: the turn the prompt left,
// how it left, and the turn it went to.
//
// WHAT READS IT: a workspace gather follows its prompt from the turn it was
// minted to the turn that answers it (SucceedingTurn). Every writer records
// its row BEFORE the turn it names is delivered, or, for a fold, on the same
// ordered stream the joined turn's answer arrives on, so the row always stands
// by the time the answer can be seen.

// turnSuccessionsDDL is the layout-122 addition, with the gather tables
// (gathersDDL). ONE ROW PER MOVE, keyed by the turn the prompt left and how:
// a turn is left at most once each way, and the workspace's row cascades the
// rows away when it is forgotten. The fresh-file schema appends this same text
// (schema.go), so a migrated file and a created one cannot differ.
const turnSuccessionsDDL = `
CREATE TABLE turn_successions (
  turn_id      TEXT NOT NULL,
  kind         TEXT NOT NULL CHECK (kind IN ('folded', 'reheld', 'resubmitted', 'resumed')),
  successor_id TEXT NOT NULL,
  workspace_id TEXT NOT NULL REFERENCES workspaces(id) ON DELETE CASCADE,
  at           INTEGER NOT NULL,
  PRIMARY KEY (turn_id, kind)
);
`

// TurnSuccessionKind is how a prompt left its turn for another. ITS SPELLINGS
// ARE DURABLE.
type TurnSuccessionKind string

// The succession kinds.
const (
	// SuccessionFolded is a prompt the vendor folded into the turn already
	// running (conversation.v1.AgentPrompt.folded_into): that turn answers it.
	SuccessionFolded TurnSuccessionKind = "folded"
	// SuccessionReheld is a prompt held again under a new turn: a join the
	// vendor gave back before taking it in, or a try the vendor blocked.
	SuccessionReheld TurnSuccessionKind = "reheld"
	// SuccessionResubmitted is a prompt submitted again as a new turn: one
	// folded into a turn that then failed, or one a merge displaced and put
	// back.
	SuccessionResubmitted TurnSuccessionKind = "resubmitted"
	// SuccessionResumed is a turn a stand-down cut, whose work the resumption
	// turn continues.
	SuccessionResumed TurnSuccessionKind = "resumed"
)

// valid reports whether the kind is declared.
func (k TurnSuccessionKind) valid() bool {
	switch k {
	case SuccessionFolded, SuccessionReheld, SuccessionResubmitted, SuccessionResumed:
		return true
	}
	return false
}

// TurnSuccession is one move of a prompt from Turn to Successor.
type TurnSuccession struct {
	Workspace WorkspaceID
	Turn      TurnID
	Kind      TurnSuccessionKind
	Successor TurnID
	At        time.Time
}

// errSuccessionCycle is a chain of successions that returns to a turn it left:
// every successor is a freshly minted turn, so it is an invariant violation.
var errSuccessionCycle = errors.New("wsm: a turn succession chain returns to a turn it left")

// RecordTurnSuccession records one move. Recording the same move twice keeps
// the first; a second successor for the same turn and kind is refused, since a
// turn is left at most once each way.
func (s *store) RecordTurnSuccession(ctx context.Context, m TurnSuccession) error {
	const op = "daemon.wsm.record_turn_succession"
	fields := successionFields(m)
	if err := validateSuccession(m); err != nil {
		s.log.Error(op, "refused an inconsistent turn succession", withError(fields, err))
		return err
	}
	return s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		return insertSuccession(ctx, tx, m)
	})
}

// validateSuccession refuses a move that names no turn, an undeclared kind, or
// a turn succeeding itself.
func validateSuccession(m TurnSuccession) error {
	switch {
	case m.Workspace == "" || m.Turn == "" || m.Successor == "":
		return errors.New("wsm: a turn succession names its workspace and both turns")
	case !m.Kind.valid():
		return fmt.Errorf("wsm: undeclared turn succession kind %q", m.Kind)
	case m.Turn == m.Successor:
		return errors.New("wsm: a turn does not succeed itself")
	}
	return nil
}

// insertSuccession writes M inside TX, refusing a different successor for a
// move already recorded.
func insertSuccession(ctx context.Context, tx *sql.Tx, m TurnSuccession) error {
	_, err := tx.ExecContext(ctx,
		`INSERT INTO turn_successions (turn_id, kind, successor_id, workspace_id, at) VALUES (?, ?, ?, ?, ?)
		 ON CONFLICT(turn_id, kind) DO NOTHING`, m.Turn, string(m.Kind), m.Successor, m.Workspace, nanos(m.At))
	if err != nil {
		return err
	}
	var successor TurnID
	if err := tx.QueryRowContext(ctx, `SELECT successor_id FROM turn_successions WHERE turn_id = ? AND kind = ?`, m.Turn, string(m.Kind)).Scan(&successor); err != nil {
		return err
	}
	if successor != m.Successor {
		return fmt.Errorf("wsm: turn %s already %s into %s, not %s", m.Turn, m.Kind, successor, m.Successor)
	}
	return nil
}

// SucceedingTurn answers the turn that now carries the prompt TURN was minted
// for: TURN itself when it never moved, else the end of its chain of moves.
//
// A MOVE THAT MADE A NEW TURN OUTRANKS A FOLD. A prompt folded into a turn
// that then failed is resubmitted from its own turn, so that turn holds both a
// fold and a resubmission, and the resubmission is where the prompt went.
// Every other kind leaves a turn that ran no fold, so at most one of them
// stands for a turn.
func (s *store) SucceedingTurn(ctx context.Context, turn TurnID) (TurnID, error) {
	const op = "daemon.wsm.succeeding_turn"
	fields := dlog.Context{"turn": string(turn)}
	current := turn
	err := s.read(ctx, op, fields, func(ctx context.Context) error {
		seen := map[TurnID]bool{turn: true}
		for {
			next, moved, err := s.nextTurn(ctx, current)
			if err != nil || !moved {
				return err
			}
			if seen[next] {
				return fmt.Errorf("%w: %s", errSuccessionCycle, next)
			}
			seen[next] = true
			current = next
		}
	})
	if err != nil {
		if errors.Is(err, errSuccessionCycle) {
			s.log.Error(op, "a turn succession chain returns to a turn it left", withError(dlog.Merge(fields, dlog.Context{"invariant_violation": "every successor is a freshly minted turn"}), err))
		}
		return "", err
	}
	return current, nil
}

// nextTurn answers where TURN's prompt went, if it moved.
func (s *store) nextTurn(ctx context.Context, turn TurnID) (TurnID, bool, error) {
	rows, err := s.q(ctx).QueryContext(ctx, `SELECT kind, successor_id FROM turn_successions WHERE turn_id = ?`, turn)
	if err != nil {
		return "", false, err
	}
	defer rows.Close()
	var folded, made TurnID
	for rows.Next() {
		var kind string
		var successor TurnID
		if err := rows.Scan(&kind, &successor); err != nil {
			return "", false, err
		}
		if TurnSuccessionKind(kind) == SuccessionFolded {
			folded = successor
		} else {
			made = successor
		}
	}
	if err := rows.Err(); err != nil {
		return "", false, err
	}
	switch {
	case made != "":
		return made, true, nil
	case folded != "":
		return folded, true, nil
	}
	return "", false, nil
}

func successionFields(m TurnSuccession) dlog.Context {
	return dlog.Context{"workspace": string(m.Workspace), "turn": string(m.Turn), "kind": string(m.Kind), "successor": string(m.Successor)}
}
