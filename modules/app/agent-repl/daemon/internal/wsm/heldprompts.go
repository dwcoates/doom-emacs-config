package wsm

import (
	"context"
	"database/sql"
	"errors"
	"fmt"

	"claude-repld/internal/dlog"
)

// heldPromptColumns is the one select list every held-prompt read shares.
const heldPromptColumns = `turn_id, workspace_id, text, origin, target, hold_kind,
	classification_interject, classification_reason, classification_failed, classification_at,
	tombstone_kind, tombstone_at, queued_at`

// scanHeldPrompt decodes one held prompt all-or-nothing. This is the row the
// all-or-nothing rule was written for: a corrupt hold must never restore as a
// partial set that silently loses what a user typed, so an undeclared hold kind,
// a half-written classification, a half-written tombstone or an unparseable
// target all fail the WHOLE read.
func scanHeldPrompt(row interface{ Scan(...any) error }) (HeldPrompt, error) {
	var (
		h         HeldPrompt
		target    sql.NullString
		holdKind  sql.NullInt64
		interject sql.NullBool
		reason    sql.NullString
		failed    sql.NullBool
		classAt   sql.NullInt64
		tombKind  sql.NullString
		tombAt    sql.NullInt64
		queued    int64
	)
	if err := row.Scan(&h.Turn, &h.Workspace, &h.Text, &h.Origin, &target, &holdKind,
		&interject, &reason, &failed, &classAt, &tombKind, &tombAt, &queued); err != nil {
		return HeldPrompt{}, err
	}
	id := string(h.Turn)
	if target.Valid {
		ref, err := decodeRef("held_prompts", id, target.String)
		if err != nil {
			return HeldPrompt{}, err
		}
		h.Target = ref
	}
	if holdKind.Valid {
		kind := HoldKind(holdKind.Int64)
		if !kind.valid() {
			return HeldPrompt{}, &DecodeError{Table: "held_prompts", Row: id, Field: "hold_kind", Err: fmt.Errorf("unknown hold kind %d", holdKind.Int64)}
		}
		h.Hold = &kind
	}
	switch {
	case !interject.Valid && !reason.Valid && !failed.Valid && !classAt.Valid:
	case interject.Valid && reason.Valid && failed.Valid && classAt.Valid:
		h.Classification = &Classification{Interject: interject.Bool, Reason: reason.String, Failed: failed.Bool, At: fromNanos(classAt.Int64)}
	default:
		return HeldPrompt{}, &DecodeError{Table: "held_prompts", Row: id, Field: "classification", Err: errors.New("a classification is stored whole or not at all")}
	}
	switch {
	case !tombKind.Valid && !tombAt.Valid:
	case tombKind.Valid && tombAt.Valid:
		h.Tombstone = &Tombstone{Kind: tombKind.String, At: fromNanos(tombAt.Int64)}
	default:
		return HeldPrompt{}, &DecodeError{Table: "held_prompts", Row: id, Field: "tombstone", Err: errors.New("a tombstone is stored whole or not at all")}
	}
	h.QueuedAt = fromNanos(queued)
	return h, nil
}

// PutHeldPrompt records a parked submission. WSM is the ONE durable hold store.
// A turn already TOMBSTONED refuses a new record: a retired hold never
// resurrects, so a late writer cannot bring a delivered or dropped prompt back.
func (s *store) PutHeldPrompt(ctx context.Context, h HeldPrompt) error {
	const op = "daemon.wsm.put_held_prompt"
	fields := dlog.Context{"workspace": string(h.Workspace), "turn": string(h.Turn), "origin": h.Origin}
	target, err := encodeRef(h.Target)
	if err != nil {
		s.log.Error(op, "refused an unencodable held-prompt target", withError(fields, err))
		return err
	}
	if h.Hold != nil && !h.Hold.valid() {
		err := fmt.Errorf("wsm: undeclared hold kind %d", int(*h.Hold))
		s.log.Error(op, "refused an undeclared hold kind", withError(fields, err))
		return err
	}
	return s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		if err := refuseTombstoned(ctx, tx, h.Turn); err != nil {
			return err
		}
		var holdKind any
		if h.Hold != nil {
			holdKind = int(*h.Hold)
		}
		var interject, reason, failed, classAt any
		if h.Classification != nil {
			interject, reason, failed, classAt = h.Classification.Interject, h.Classification.Reason, h.Classification.Failed, nanos(h.Classification.At)
		}
		var tombKind, tombAt any
		if h.Tombstone != nil {
			tombKind, tombAt = h.Tombstone.Kind, nanos(h.Tombstone.At)
		}
		_, err := tx.ExecContext(ctx,
			`INSERT INTO held_prompts (turn_id, workspace_id, text, origin, target, hold_kind,
			   classification_interject, classification_reason, classification_failed, classification_at,
			   tombstone_kind, tombstone_at, queued_at)
			 VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
			 ON CONFLICT(turn_id) DO UPDATE SET
			   workspace_id = excluded.workspace_id, text = excluded.text, origin = excluded.origin,
			   target = excluded.target, hold_kind = excluded.hold_kind,
			   classification_interject = excluded.classification_interject,
			   classification_reason = excluded.classification_reason,
			   classification_failed = excluded.classification_failed,
			   classification_at = excluded.classification_at,
			   tombstone_kind = excluded.tombstone_kind, tombstone_at = excluded.tombstone_at,
			   queued_at = excluded.queued_at`,
			h.Turn, h.Workspace, h.Text, h.Origin, target, holdKind,
			interject, reason, failed, classAt, tombKind, tombAt, nanos(h.QueuedAt))
		return err
	})
}

// refuseTombstoned refuses any write to a retired hold. It is the one place
// resurrection is checked, so no update path can forget it.
func refuseTombstoned(ctx context.Context, tx *sql.Tx, turn TurnID) error {
	var kind sql.NullString
	err := tx.QueryRowContext(ctx, `SELECT tombstone_kind FROM held_prompts WHERE turn_id = ?`, turn).Scan(&kind)
	if errors.Is(err, sql.ErrNoRows) {
		return nil
	}
	if err != nil {
		return err
	}
	if kind.Valid {
		return fmt.Errorf("wsm: held prompt %s retired as %q: %w", turn, kind.String, ErrTombstoned)
	}
	return nil
}

// requireStandingHold refuses an update to a hold that does not exist or has
// been retired.
func requireStandingHold(ctx context.Context, tx *sql.Tx, turn TurnID) error {
	var kind sql.NullString
	err := tx.QueryRowContext(ctx, `SELECT tombstone_kind FROM held_prompts WHERE turn_id = ?`, turn).Scan(&kind)
	if errors.Is(err, sql.ErrNoRows) {
		return fmt.Errorf("wsm: held prompt %s: %w", turn, ErrNotFound)
	}
	if err != nil {
		return err
	}
	if kind.Valid {
		return fmt.Errorf("wsm: held prompt %s retired as %q: %w", turn, kind.String, ErrTombstoned)
	}
	return nil
}

// UpdateHeldPromptClassification records the classifier's verdict.
func (s *store) UpdateHeldPromptClassification(ctx context.Context, turn TurnID, c Classification) error {
	fields := dlog.Context{"turn": string(turn), "interject": c.Interject, "failed": c.Failed}
	return s.write(ctx, "daemon.wsm.update_held_prompt_classification", fields, func(ctx context.Context, tx *sql.Tx) error {
		if err := requireStandingHold(ctx, tx, turn); err != nil {
			return err
		}
		_, err := tx.ExecContext(ctx,
			`UPDATE held_prompts SET classification_interject = ?, classification_reason = ?, classification_failed = ?, classification_at = ?
			 WHERE turn_id = ?`, c.Interject, c.Reason, c.Failed, nanos(c.At), turn)
		return err
	})
}

// UpdateHeldPromptHold changes or clears why a prompt is held. Clearing it is
// what makes the prompt deliverable.
func (s *store) UpdateHeldPromptHold(ctx context.Context, turn TurnID, h *HoldKind) error {
	const op = "daemon.wsm.update_held_prompt_hold"
	fields := dlog.Context{"turn": string(turn)}
	var kind any
	if h != nil {
		if !h.valid() {
			err := fmt.Errorf("wsm: undeclared hold kind %d", int(*h))
			s.log.Error(op, "refused an undeclared hold kind", withError(fields, err))
			return err
		}
		kind = int(*h)
		fields["hold_kind"] = int(*h)
	}
	return s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		if err := requireStandingHold(ctx, tx, turn); err != nil {
			return err
		}
		_, err := tx.ExecContext(ctx, `UPDATE held_prompts SET hold_kind = ? WHERE turn_id = ?`, kind, turn)
		return err
	})
}

// TombstoneHeldPrompt retires a held prompt with its reason. A prompt already
// retired is refused, so its recorded reason is final.
func (s *store) TombstoneHeldPrompt(ctx context.Context, turn TurnID, why Tombstone) error {
	fields := dlog.Context{"turn": string(turn), "tombstone_kind": why.Kind, "tombstone_at": why.At}
	return s.write(ctx, "daemon.wsm.tombstone_held_prompt", fields, func(ctx context.Context, tx *sql.Tx) error {
		if err := requireStandingHold(ctx, tx, turn); err != nil {
			return err
		}
		_, err := tx.ExecContext(ctx, `UPDATE held_prompts SET tombstone_kind = ?, tombstone_at = ? WHERE turn_id = ?`,
			why.Kind, nanos(why.At), turn)
		return err
	})
}

// HeldPrompts loads one workspace's STANDING holds, all-or-nothing. Retired
// holds are not standing and are not returned.
func (s *store) HeldPrompts(ctx context.Context, id WorkspaceID) ([]HeldPrompt, error) {
	return s.loadHeldPrompts(ctx, "daemon.wsm.held_prompts", dlog.Context{"workspace": string(id)},
		`SELECT `+heldPromptColumns+` FROM held_prompts WHERE workspace_id = ? AND tombstone_kind IS NULL ORDER BY queued_at, turn_id`, id)
}

// AllHeldPrompts loads every standing hold for the boot restore, all-or-nothing:
// a corrupt row fails the read and NOTHING is loaded.
func (s *store) AllHeldPrompts(ctx context.Context) ([]HeldPrompt, error) {
	return s.loadHeldPrompts(ctx, "daemon.wsm.all_held_prompts", dlog.Context{},
		`SELECT `+heldPromptColumns+` FROM held_prompts WHERE tombstone_kind IS NULL ORDER BY queued_at, turn_id`)
}

// loadHeldPrompts is the one hold-loading path, so both readers obey the same
// all-or-nothing rule: the accumulated slice is published only after every row
// decoded.
func (s *store) loadHeldPrompts(ctx context.Context, op string, fields dlog.Context, query string, args ...any) ([]HeldPrompt, error) {
	var out []HeldPrompt
	err := s.read(ctx, op, fields, func(ctx context.Context) error {
		rows, err := s.db.QueryContext(ctx, query, args...)
		if err != nil {
			return err
		}
		defer rows.Close()
		var loaded []HeldPrompt
		for rows.Next() {
			h, err := scanHeldPrompt(rows)
			if err != nil {
				return err
			}
			loaded = append(loaded, h)
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
