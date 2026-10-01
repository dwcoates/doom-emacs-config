package wsm

import (
	"context"
	"database/sql"
	"errors"
	"fmt"

	conversationv1 "agentrepl/proto/conversation/v1"

	"google.golang.org/protobuf/proto"

	"claude-repld/internal/dlog"
)

// heldPromptColumns is the one select list every held-prompt read shares, so a
// column added to the row can never be decoded by only some of them.
const heldPromptColumns = `turn_id, workspace_id, said, origin, target, hold_kind, hold_schedule_id,
	classification_arm, classification_reason, classification_command, classification_at,
	accepted, tombstone_kind, tombstone_at, queued_at, delivery, act_kind, act_value, coalesced`

// scanHeldPrompt decodes one held prompt all-or-nothing. This is the row the
// all-or-nothing rule was written for: a corrupt hold must never restore as a
// partial set that silently loses what a user typed. An unparseable `said`
// blob, an undeclared hold kind or classification arm, a half-written
// classification or tombstone, and an unparseable target all fail the WHOLE
// read.
func scanHeldPrompt(row interface{ Scan(...any) error }) (HeldPrompt, error) {
	var (
		h        HeldPrompt
		said     []byte
		target   sql.NullString
		holdKind sql.NullInt64
		schedule sql.NullString
		arm      sql.NullInt64
		reason   sql.NullString
		command  sql.NullInt64
		classAt  sql.NullInt64
		tombKind sql.NullString
		tombAt   sql.NullInt64
		queued   int64
		delivery int64
		actKind  sql.NullString
		actValue sql.NullString
	)
	if err := row.Scan(&h.Turn, &h.Workspace, &said, &h.Origin, &target, &holdKind, &schedule,
		&arm, &reason, &command, &classAt, &h.Accepted, &tombKind, &tombAt, &queued, &delivery,
		&actKind, &actValue, &h.Coalesced); err != nil {
		return HeldPrompt{}, err
	}
	id := string(h.Turn)
	// A delivery this build does not know is never read as the ordinary one: a
	// deferred prompt misread would be classified and could interject.
	h.Delivery = Delivery(delivery)
	if !h.Delivery.valid() {
		return HeldPrompt{}, &DecodeError{Table: "held_prompts", Row: id, Field: "delivery", Err: fmt.Errorf("unknown delivery %d", delivery)}
	}

	// The submission is the one fact a hold exists to preserve; an unparseable
	// blob is never read as an empty prompt.
	var decoded conversationv1.UserSaid
	if err := proto.Unmarshal(said, &decoded); err != nil {
		return HeldPrompt{}, &DecodeError{Table: "held_prompts", Row: id, Field: "said", Err: err}
	}
	h.Said = &decoded

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
		if schedule.Valid {
			h.ScheduleID = schedule.String
		}
		// A shutdown hold waits on a specific schedule; without it the daemon
		// could not tell which drain to resume from.
		if kind == HoldShutdown && h.ScheduleID == "" {
			return HeldPrompt{}, &DecodeError{Table: "held_prompts", Row: id, Field: "hold_schedule_id", Err: errors.New("a shutdown hold names the drain schedule it waits on")}
		}
	} else if schedule.Valid && schedule.String != "" {
		return HeldPrompt{}, &DecodeError{Table: "held_prompts", Row: id, Field: "hold_schedule_id", Err: errors.New("a schedule id without a hold kind")}
	}

	switch {
	case !arm.Valid && !reason.Valid && !command.Valid && !classAt.Valid:
	case arm.Valid && reason.Valid && command.Valid && classAt.Valid:
		verdict := ClassificationArm(arm.Int64)
		if !verdict.valid() {
			return HeldPrompt{}, &DecodeError{Table: "held_prompts", Row: id, Field: "classification_arm", Err: fmt.Errorf("unknown classification arm %d", arm.Int64)}
		}
		cmd := conversationv1.SessionCommand(command.Int64)
		if _, ok := conversationv1.SessionCommand_name[int32(command.Int64)]; !ok {
			return HeldPrompt{}, &DecodeError{Table: "held_prompts", Row: id, Field: "classification_command", Err: fmt.Errorf("unknown session command %d", command.Int64)}
		}
		h.Classification = &Classification{Arm: verdict, Reason: reason.String, Command: cmd, At: fromNanos(classAt.Int64)}
	default:
		return HeldPrompt{}, &DecodeError{Table: "held_prompts", Row: id, Field: "classification", Err: errors.New("a classification is stored whole or not at all")}
	}
	// Accepting is legal only on a hold_for_turn_end verdict, so an accepted row
	// with any other verdict is corruption rather than a state to render.
	if h.Accepted && (h.Classification == nil || h.Classification.Arm != ArmHoldForTurnEnd) {
		return HeldPrompt{}, &DecodeError{Table: "held_prompts", Row: id, Field: "accepted", Err: ErrAcceptNotOffered}
	}

	switch {
	case !tombKind.Valid && !tombAt.Valid:
	case tombKind.Valid && tombAt.Valid:
		h.Tombstone = &Tombstone{Kind: tombKind.String, At: fromNanos(tombAt.Int64)}
	default:
		return HeldPrompt{}, &DecodeError{Table: "held_prompts", Row: id, Field: "tombstone", Err: errors.New("a tombstone is stored whole or not at all")}
	}
	switch {
	case !actKind.Valid && !actValue.Valid:
	case actKind.Valid && actValue.Valid:
		act := &HeldAct{Kind: actKind.String, Value: actValue.String}
		if err := validateAct(act); err != nil {
			return HeldPrompt{}, &DecodeError{Table: "held_prompts", Row: id, Field: "act", Err: err}
		}
		h.Act = act
	default:
		return HeldPrompt{}, &DecodeError{Table: "held_prompts", Row: id, Field: "act", Err: errors.New("an act is stored whole or not at all")}
	}
	h.QueuedAt = fromNanos(queued)
	return h, nil
}

// validateAct refuses an act of a kind no build declares, or one that sets
// nothing.
func validateAct(act *HeldAct) error {
	switch act.Kind {
	case ActModel, ActPermissionMode:
	default:
		return fmt.Errorf("wsm: undeclared act kind %q", act.Kind)
	}
	if act.Value == "" {
		return fmt.Errorf("wsm: a %s act names the value it sets", act.Kind)
	}
	return nil
}

// PutHeldPrompt records a parked submission. WSM is the ONE durable hold store.
// A turn already TOMBSTONED refuses a new record: a retired hold never
// resurrects, so a late writer cannot bring a delivered or dropped prompt back.
func (s *store) PutHeldPrompt(ctx context.Context, h HeldPrompt) error {
	const op = "daemon.wsm.put_held_prompt"
	fields := dlog.Context{"workspace": string(h.Workspace), "turn": string(h.Turn), "origin": h.Origin, "delivery": h.Delivery.String()}
	if h.Said == nil {
		err := errors.New("wsm: a held prompt carries what the user said")
		s.log.Error(op, "refused a held prompt with no submission", withError(fields, err))
		return err
	}
	said, err := proto.Marshal(h.Said)
	if err != nil {
		wrapped := fmt.Errorf("wsm: encode held prompt submission: %w", err)
		s.log.Error(op, "refused an unencodable held-prompt submission", withError(fields, wrapped))
		return wrapped
	}
	target, err := encodeRef(h.Target)
	if err != nil {
		s.log.Error(op, "refused an unencodable held-prompt target", withError(fields, err))
		return err
	}
	if err := validateHold(h); err != nil {
		s.log.Error(op, "refused an inconsistent hold", withError(fields, err))
		return err
	}
	if h.Hold != nil {
		fields["hold_kind"] = h.Hold.String()
	}
	if h.Classification != nil {
		fields["classification_arm"] = h.Classification.Arm.String()
	}
	return s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		if err := refuseTombstoned(ctx, tx, h.Turn); err != nil {
			return err
		}
		if h.Hold != nil && *h.Hold == HoldMerge {
			if err := bindMergeHold(ctx, tx, h.Workspace); err != nil {
				return err
			}
		}
		var holdKind, schedule any
		if h.Hold != nil {
			holdKind = int(*h.Hold)
			schedule = h.ScheduleID
		}
		var arm, reason, command, classAt any
		if h.Classification != nil {
			arm, reason = int(h.Classification.Arm), h.Classification.Reason
			command, classAt = int32(h.Classification.Command), nanos(h.Classification.At)
		}
		var tombKind, tombAt any
		if h.Tombstone != nil {
			tombKind, tombAt = h.Tombstone.Kind, nanos(h.Tombstone.At)
		}
		var actKind, actValue any
		if h.Act != nil {
			actKind, actValue = h.Act.Kind, h.Act.Value
		}
		_, err := tx.ExecContext(ctx,
			`INSERT INTO held_prompts (turn_id, workspace_id, said, origin, target, hold_kind, hold_schedule_id,
			   classification_arm, classification_reason, classification_command, classification_at,
			   accepted, tombstone_kind, tombstone_at, queued_at, delivery, act_kind, act_value, coalesced)
			 VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
			 ON CONFLICT(turn_id) DO UPDATE SET
			   workspace_id = excluded.workspace_id, said = excluded.said, origin = excluded.origin,
			   target = excluded.target, hold_kind = excluded.hold_kind,
			   hold_schedule_id = excluded.hold_schedule_id,
			   classification_arm = excluded.classification_arm,
			   classification_reason = excluded.classification_reason,
			   classification_command = excluded.classification_command,
			   classification_at = excluded.classification_at,
			   accepted = excluded.accepted,
			   tombstone_kind = excluded.tombstone_kind, tombstone_at = excluded.tombstone_at,
			   queued_at = excluded.queued_at, delivery = excluded.delivery,
			   act_kind = excluded.act_kind, act_value = excluded.act_value, coalesced = excluded.coalesced`,
			h.Turn, h.Workspace, said, h.Origin, target, holdKind, schedule,
			arm, reason, command, classAt, h.Accepted, tombKind, tombAt, nanos(h.QueuedAt), int(h.Delivery),
			actKind, actValue, h.Coalesced)
		return err
	})
}

// validateHold refuses the combinations the row must never carry, so the
// decoder's rules and the writer's rules are the same rules.
func validateHold(h HeldPrompt) error {
	if h.Hold != nil {
		if !h.Hold.valid() {
			return fmt.Errorf("wsm: undeclared hold kind %d", int(*h.Hold))
		}
		if *h.Hold == HoldShutdown && h.ScheduleID == "" {
			return errors.New("wsm: a shutdown hold names the drain schedule it waits on")
		}
	} else if h.ScheduleID != "" {
		return errors.New("wsm: a schedule id without a hold kind")
	}
	if !h.Delivery.valid() {
		return fmt.Errorf("wsm: undeclared delivery %d", int(h.Delivery))
	}
	if h.Act != nil {
		if err := validateAct(h.Act); err != nil {
			return err
		}
	}
	if h.Classification != nil && !h.Classification.Arm.valid() {
		return fmt.Errorf("wsm: undeclared classification arm %d", int(h.Classification.Arm))
	}
	if h.Accepted && (h.Classification == nil || h.Classification.Arm != ArmHoldForTurnEnd) {
		return ErrAcceptNotOffered
	}
	return nil
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

// UpdateHeldPromptClassification records the classifier's verdict. A verdict
// that is not hold_for_turn_end clears any standing acceptance, because the
// offer the user accepted no longer exists.
func (s *store) UpdateHeldPromptClassification(ctx context.Context, turn TurnID, c Classification) error {
	const op = "daemon.wsm.update_held_prompt_classification"
	fields := dlog.Context{"turn": string(turn), "classification_arm": c.Arm.String()}
	if !c.Arm.valid() {
		err := fmt.Errorf("wsm: undeclared classification arm %d", int(c.Arm))
		s.log.Error(op, "refused an undeclared classification arm", withError(fields, err))
		return err
	}
	return s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		if err := requireStandingHold(ctx, tx, turn); err != nil {
			return err
		}
		_, err := tx.ExecContext(ctx,
			`UPDATE held_prompts SET classification_arm = ?, classification_reason = ?, classification_command = ?, classification_at = ?,
			   accepted = CASE WHEN ? THEN accepted ELSE 0 END
			 WHERE turn_id = ?`,
			int(c.Arm), c.Reason, int32(c.Command), nanos(c.At), c.Arm == ArmHoldForTurnEnd, turn)
		return err
	})
}

// SetHeldPromptAccepted records the user's acceptance of the tray's offer to let
// the prompt wait for the turn's end. It is LEGAL ONLY on a hold_for_turn_end
// verdict, which is checked here rather than trusted from the caller.
func (s *store) SetHeldPromptAccepted(ctx context.Context, turn TurnID) error {
	fields := dlog.Context{"turn": string(turn)}
	return s.write(ctx, "daemon.wsm.set_held_prompt_accepted", fields, func(ctx context.Context, tx *sql.Tx) error {
		if err := requireStandingHold(ctx, tx, turn); err != nil {
			return err
		}
		var arm sql.NullInt64
		if err := tx.QueryRowContext(ctx, `SELECT classification_arm FROM held_prompts WHERE turn_id = ?`, turn).Scan(&arm); err != nil {
			return err
		}
		if !arm.Valid || ClassificationArm(arm.Int64) != ArmHoldForTurnEnd {
			return fmt.Errorf("wsm: held prompt %s: %w", turn, ErrAcceptNotOffered)
		}
		_, err := tx.ExecContext(ctx, `UPDATE held_prompts SET accepted = 1 WHERE turn_id = ?`, turn)
		return err
	})
}

// UpdateHeldPromptHold changes or clears the daemon-side condition holding a
// prompt. Clearing it is part of what makes the prompt deliverable.
//
// scheduleID is the drain schedule a HoldShutdown waits on and is required for
// that arm; it is empty for every other kind.
func (s *store) UpdateHeldPromptHold(ctx context.Context, turn TurnID, h *HoldKind, scheduleID string) error {
	const op = "daemon.wsm.update_held_prompt_hold"
	fields := dlog.Context{"turn": string(turn)}
	var kind, schedule any
	if h != nil {
		if !h.valid() {
			err := fmt.Errorf("wsm: undeclared hold kind %d", int(*h))
			s.log.Error(op, "refused an undeclared hold kind", withError(fields, err))
			return err
		}
		if *h == HoldShutdown && scheduleID == "" {
			err := errors.New("wsm: a shutdown hold names the drain schedule it waits on")
			s.log.Error(op, "refused a shutdown hold with no schedule", withError(fields, err))
			return err
		}
		kind = int(*h)
		schedule = scheduleID
		fields["hold_kind"] = h.String()
		fields["hold_schedule_id"] = scheduleID
	} else if scheduleID != "" {
		err := errors.New("wsm: a schedule id without a hold kind")
		s.log.Error(op, "refused a schedule id with no hold kind", withError(fields, err))
		return err
	}
	return s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		if err := requireStandingHold(ctx, tx, turn); err != nil {
			return err
		}
		if h != nil && *h == HoldMerge {
			var ws WorkspaceID
			if err := tx.QueryRowContext(ctx, `SELECT workspace_id FROM held_prompts WHERE turn_id = ?`, turn).Scan(&ws); err != nil {
				return err
			}
			if err := bindMergeHold(ctx, tx, ws); err != nil {
				return err
			}
		}
		_, err := tx.ExecContext(ctx, `UPDATE held_prompts SET hold_kind = ?, hold_schedule_id = ? WHERE turn_id = ?`, kind, schedule, turn)
		return err
	})
}

// TombstoneHeldPrompt retires a held prompt with its reason. A prompt already
// retired is refused, so its recorded reason is final.
func (s *store) TombstoneHeldPrompt(ctx context.Context, turn TurnID, why Tombstone) error {
	return s.TombstoneHeldPrompts(ctx, []TurnID{turn}, why)
}

// TombstoneHeldPrompts retires held prompts with one reason, all or nothing: a
// prompt already retired or unknown refuses the whole batch, so a caller that
// drops several (a rollback) never leaves some dropped and some standing.
func (s *store) TombstoneHeldPrompts(ctx context.Context, turns []TurnID, why Tombstone) error {
	fields := dlog.Context{"turns": turnStrings(turns), "tombstone_kind": why.Kind, "tombstone_at": why.At}
	return s.write(ctx, "daemon.wsm.tombstone_held_prompt", fields, func(ctx context.Context, tx *sql.Tx) error {
		if why.Kind == "" {
			return errors.New("wsm: a tombstone names its reason")
		}
		if len(turns) == 0 {
			return errors.New("wsm: a tombstone names at least one held prompt")
		}
		for _, turn := range turns {
			if err := requireStandingHold(ctx, tx, turn); err != nil {
				return err
			}
			if _, err := tx.ExecContext(ctx, `UPDATE held_prompts SET tombstone_kind = ?, tombstone_at = ? WHERE turn_id = ?`,
				why.Kind, nanos(why.At), turn); err != nil {
				return err
			}
		}
		return nil
	})
}

func turnStrings(turns []TurnID) []string {
	out := make([]string, len(turns))
	for i, turn := range turns {
		out[i] = string(turn)
	}
	return out
}

// ReplaceHeldPromptSaid replaces a standing hold's content and discards its
// verdict in one transaction: the classification columns go NULL and the
// acceptance goes false, because both were about the content being replaced.
// queued_at is untouched, so the hold keeps its place in the queue. A retired
// or unknown hold is refused.
func (s *store) ReplaceHeldPromptSaid(ctx context.Context, turn TurnID, said *conversationv1.UserSaid) error {
	const op = "daemon.wsm.replace_held_prompt_said"
	fields := dlog.Context{"turn": string(turn)}
	if said == nil {
		err := errors.New("wsm: a held prompt carries what the user said")
		s.log.Error(op, "refused a replacement with no submission", withError(fields, err))
		return err
	}
	blob, err := proto.Marshal(said)
	if err != nil {
		wrapped := fmt.Errorf("wsm: encode held prompt submission: %w", err)
		s.log.Error(op, "refused an unencodable replacement submission", withError(fields, wrapped))
		return wrapped
	}
	return s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		if err := requireStandingHold(ctx, tx, turn); err != nil {
			return err
		}
		_, err := tx.ExecContext(ctx,
			`UPDATE held_prompts SET said = ?, classification_arm = NULL, classification_reason = NULL,
			   classification_command = NULL, classification_at = NULL, accepted = 0
			 WHERE turn_id = ?`, blob, turn)
		return err
	})
}

// CoalesceHeldPrompts folds one standing hold into another in ONE transaction:
// INTO takes the merged content and is marked coalesced, and FROM is retired
// with its tombstone. Either both land or neither does, so no reader ever sees
// the merged words beside a FROM still standing, or FROM gone with nothing
// merged. INTO keeps its queued_at, so it keeps its place in the queue.
//
// It refuses, writing nothing, when either hold is unknown or retired, when the
// two are one hold, or when they stand in different workspaces.
func (s *store) CoalesceHeldPrompts(ctx context.Context, c Coalescence) error {
	const op = "daemon.wsm.coalesce_held_prompts"
	fields := dlog.Context{
		"into_turn": string(c.Into), "from_turn": string(c.From),
		"tombstone_kind": c.Retired.Kind, "discard_verdict": c.DiscardVerdict,
	}
	if err := validateCoalescence(c); err != nil {
		s.log.Error(op, "refused an inconsistent coalescence", withError(fields, err))
		return err
	}
	blob, err := proto.Marshal(c.Said)
	if err != nil {
		wrapped := fmt.Errorf("wsm: encode the coalesced submission: %w", err)
		s.log.Error(op, "refused an unencodable coalesced submission", withError(fields, wrapped))
		return wrapped
	}
	return s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		if err := requireStandingHold(ctx, tx, c.Into); err != nil {
			return err
		}
		if err := requireStandingHold(ctx, tx, c.From); err != nil {
			return err
		}
		if err := requireSameWorkspace(ctx, tx, c.Into, c.From); err != nil {
			return err
		}
		update := `UPDATE held_prompts SET said = ?, coalesced = 1 WHERE turn_id = ?`
		if c.DiscardVerdict {
			update = `UPDATE held_prompts SET said = ?, coalesced = 1, classification_arm = NULL,
			   classification_reason = NULL, classification_command = NULL, classification_at = NULL, accepted = 0
			 WHERE turn_id = ?`
		}
		if _, err := tx.ExecContext(ctx, update, blob, c.Into); err != nil {
			return err
		}
		_, err := tx.ExecContext(ctx, `UPDATE held_prompts SET tombstone_kind = ?, tombstone_at = ? WHERE turn_id = ?`,
			c.Retired.Kind, nanos(c.Retired.At), c.From)
		return err
	})
}

// validateCoalescence refuses the coalescences no row may record.
func validateCoalescence(c Coalescence) error {
	switch {
	case c.Said == nil:
		return errors.New("wsm: a coalescence carries the merged submission")
	case c.Into == "" || c.From == "":
		return errors.New("wsm: a coalescence names both holds")
	case c.Into == c.From:
		return fmt.Errorf("wsm: held prompt %s cannot be coalesced into itself", c.Into)
	case c.Retired.Kind == "":
		return errors.New("wsm: a coalescence names the retired hold's tombstone")
	}
	return nil
}

// requireSameWorkspace refuses a coalescence across two workspaces' queues.
func requireSameWorkspace(ctx context.Context, tx *sql.Tx, into, from TurnID) error {
	var intoWS, fromWS string
	if err := tx.QueryRowContext(ctx, `SELECT workspace_id FROM held_prompts WHERE turn_id = ?`, into).Scan(&intoWS); err != nil {
		return err
	}
	if err := tx.QueryRowContext(ctx, `SELECT workspace_id FROM held_prompts WHERE turn_id = ?`, from).Scan(&fromWS); err != nil {
		return err
	}
	if intoWS != fromWS {
		return fmt.Errorf("wsm: held prompt %s stands in %s, not in %s with %s", from, fromWS, intoWS, into)
	}
	return nil
}

// HeldPromptByTurn loads one hold by its turn, INCLUDING a retired one, so a
// caller can tell a turn nothing was ever held under from a hold that was
// delivered or dropped. The bool reports whether any row exists.
func (s *store) HeldPromptByTurn(ctx context.Context, turn TurnID) (HeldPrompt, bool, error) {
	loaded, err := s.loadHeldPrompts(ctx, "daemon.wsm.held_prompt_by_turn", dlog.Context{"turn": string(turn)},
		`SELECT `+heldPromptColumns+` FROM held_prompts WHERE turn_id = ?`, turn)
	if err != nil {
		return HeldPrompt{}, false, err
	}
	if len(loaded) == 0 {
		return HeldPrompt{}, false, nil
	}
	return loaded[0], true, nil
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
		rows, err := s.db().QueryContext(ctx, query, args...)
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
