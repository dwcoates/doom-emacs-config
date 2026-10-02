package holds

import (
	"sort"
	"strings"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"google.golang.org/protobuf/proto"
	"google.golang.org/protobuf/types/descriptorpb"

	"claude-repld/internal/dlog"
	"claude-repld/internal/holdfold"
	"claude-repld/internal/ids"
	"claude-repld/internal/wsm"
)

// orderedHolds is the tray's display order: OLDEST FIRST, because the tray
// shows a delivery queue and the queue drains in the order it filled. The turn
// id breaks a tie so two prompts queued in the same millisecond still draw in a
// stable order rather than swapping between pushes.
//
// It copies before sorting: the slice belongs to the prompt queue, and a
// resolver never reorders its caller's memory.
func orderedHolds(held []wsm.HeldPrompt) []wsm.HeldPrompt {
	out := make([]wsm.HeldPrompt, len(held))
	copy(out, held)
	sort.SliceStable(out, func(i, j int) bool {
		if !out[i].QueuedAt.Equal(out[j].QueuedAt) {
			return out[i].QueuedAt.Before(out[j].QueuedAt)
		}
		return out[i].Turn < out[j].Turn
	})
	return out
}

// heldPrompt converts one durable hold into the tray's entry, or nil when the
// record is not a standing hold at all. EDITING says the hold is the one an
// EditHeldPrompt claim stands on.
//
// EVERY BRANCH LOGS. The two facts the contract requires — a classification arm
// and, on the uninterruptible arm, the command that made the turn
// uninterruptible — are recorded LOUDLY when the record cannot supply them, and
// the entry is still emitted: a prompt the daemon is really holding must be
// visible even when its explanation is defective, and dropping it would hide
// pending work.
func heldPrompt(h wsm.HeldPrompt, editing bool, log dlog.Logger) *frontendv1.HeldPrompt {
	if h.Tombstone != nil {
		log.Debug("daemon.holds.convert", "a retired hold was skipped: the tray draws standing holds only",
			dlog.Context{"turn_id": string(h.Turn), "tombstone": h.Tombstone.Kind})
		return nil
	}
	if h.Said == nil {
		log.Error("daemon.holds.convert", "a standing hold carried no said and was emitted without one",
			dlog.Context{
				"turn_id":             string(h.Turn),
				"invariant_violation": "HeldPrompt.Said is nil",
				"remediation":         "persist the whole UserSaid at submission",
			})
	}
	out := &frontendv1.HeldPrompt{
		Turn:     &conversationv1.TurnId{Value: string(h.Turn)},
		Said:     h.Said,
		QueuedAt: &frontendv1.HeldPromptQueuedAt{AtMs: h.QueuedAt.UnixMilli()},
	}
	setClassification(out, h, log)
	setHold(out, h, log)
	if editing {
		log.Debug("daemon.holds.editing", "the hold is being edited", dlog.Context{"turn_id": string(h.Turn)})
		out.Editing = &frontendv1.HeldPromptEditing{}
	}
	if h.Coalesced {
		log.Debug("daemon.holds.coalesced", "later prompts were folded into the hold", dlog.Context{"turn_id": string(h.Turn)})
		out.Coalesced = &frontendv1.HeldPromptCoalesced{}
	}
	if h.Act != nil {
		out.Act = heldSessionAct(h, log)
	}
	out.Badges = heldBadges(out, log)
	return out
}

// foldAbove is the entry's "fold above" button, or nil when it is not offered:
// there is no entry ahead of it, or holdfold.Foldable refuses the pair. It is
// the same reading FoldHeldPrompt refuses by, so the button stands exactly
// when the fold would be taken.
func foldAbove(h wsm.HeldPrompt, ahead *wsm.HeldPrompt, editing ids.TurnID, log dlog.Logger) *frontendv1.HeldPromptFoldAbove {
	ctx := dlog.Context{"turn_id": string(h.Turn)}
	if ahead == nil {
		log.Debug("daemon.holds.fold_above", "the first entry offers no fold: nothing stands ahead of it", ctx)
		return nil
	}
	ctx["above_turn"] = string(ahead.Turn)
	if err := holdfold.Foldable(h, *ahead, editing); err != nil {
		ctx["reason"] = err.Error()
		log.Debug("daemon.holds.fold_above", "the entry offers no fold into the entry ahead", ctx)
		return nil
	}
	log.Debug("daemon.holds.fold_above", "the entry offers a fold into the entry ahead", ctx)
	return &frontendv1.HeldPromptFoldAbove{Above: &conversationv1.TurnId{Value: string(ahead.Turn)}}
}

// heldSessionAct projects a held model or permission-mode change. The store
// refuses any other kind, so one reaching here is a defect, stated loudly, and
// the entry is still drawn as the prompt its `said` shows.
func heldSessionAct(h wsm.HeldPrompt, log dlog.Logger) *frontendv1.HeldSessionAct {
	ctx := dlog.Context{"turn_id": string(h.Turn), "act": h.Act.Kind, "value": h.Act.Value}
	switch h.Act.Kind {
	case wsm.ActModel:
		log.Debug("daemon.holds.act", "the hold is a model change", ctx)
		return &frontendv1.HeldSessionAct{Act: &frontendv1.HeldSessionAct_Model{
			Model: &frontendv1.HeldSessionActModel{Model: h.Act.Value}}}
	case wsm.ActPermissionMode:
		log.Debug("daemon.holds.act", "the hold is a permission-mode change", ctx)
		return &frontendv1.HeldSessionAct{Act: &frontendv1.HeldSessionAct_PermissionMode{
			PermissionMode: &frontendv1.HeldSessionActPermissionMode{Mode: h.Act.Value}}}
	default:
		ctx["invariant_violation"] = "unknown wsm.HeldAct kind"
		ctx["remediation"] = "add the act to the tray's projection"
		log.Error("daemon.holds.act", "a hold carried an unknown act and was drawn as its said alone", ctx)
		return nil
	}
}

// setClassification projects the durable verdict onto the tray's oneof. A
// record with NO verdict yet is the `classifying` arm — the judge is still
// running — which is why the arm is never left unset. EXCEPT one a daemon
// condition holds: the classifier never runs on such an entry, so its arm is
// `daemon_held` rather than a decision that is not being made.
func setClassification(out *frontendv1.HeldPrompt, h wsm.HeldPrompt, log dlog.Logger) {
	ctx := dlog.Context{"turn_id": string(h.Turn)}
	classifying := &frontendv1.HeldPrompt_Classifying{Classifying: &frontendv1.HeldPromptClassifying{}}
	if h.Classification == nil && h.Hold != nil {
		ctx["hold"] = h.Hold.String()
		log.Debug("daemon.holds.classification", "an unjudged hold a daemon condition holds drew the daemon_held arm", ctx)
		out.Classification = &frontendv1.HeldPrompt_DaemonHeld{DaemonHeld: &frontendv1.HeldPromptDaemonHeld{}}
		return
	}
	if h.Classification == nil {
		log.Debug("daemon.holds.classification", "an unjudged hold drew the classifying arm", ctx)
		out.Classification = classifying
		return
	}
	switch h.Classification.Arm {
	case wsm.ArmClassifying:
		ctx["arm"] = "classifying"
		log.Debug("daemon.holds.classification", "the hold is awaiting its verdict", ctx)
		out.Classification = classifying
	case wsm.ArmInterject:
		ctx["arm"] = "interject"
		log.Debug("daemon.holds.classification", "the hold interjects", ctx)
		out.Classification = &frontendv1.HeldPrompt_Interject{
			Interject: &frontendv1.HeldPromptInterject{Rationale: h.Classification.Reason}}
	case wsm.ArmAfterToolCall:
		ctx["arm"] = "after_tool_call"
		log.Debug("daemon.holds.classification", "the hold joins the running turn after its current tool call", ctx)
		out.Classification = &frontendv1.HeldPrompt_AfterToolCall{
			AfterToolCall: &frontendv1.HeldPromptAfterToolCall{Rationale: h.Classification.Reason}}
	case wsm.ArmHoldForTurnEnd:
		ctx["arm"] = "hold_for_turn_end"
		ctx["accepted"] = h.Accepted
		log.Debug("daemon.holds.classification", "the hold waits for the turn's end", ctx)
		out.Classification = &frontendv1.HeldPrompt_HoldForTurnEnd{
			HoldForTurnEnd: &frontendv1.HeldPromptHoldForTurnEnd{
				Rationale: h.Classification.Reason,
				Accepted:  &frontendv1.HeldPromptAccepted{Accepted: h.Accepted},
			}}
	case wsm.ArmUninterruptibleTurn:
		ctx["arm"] = "uninterruptible_turn"
		ctx["command"] = h.Classification.Command.String()
		if h.Classification.Command == conversationv1.SessionCommand_SESSION_COMMAND_UNSPECIFIED {
			ctx["invariant_violation"] = "HeldPromptUninterruptibleTurn.command is UNSPECIFIED"
			ctx["remediation"] = "record the recognized session command with the verdict"
			log.Error("daemon.holds.classification",
				"an uninterruptible-turn verdict named no command and was emitted without one", ctx)
		} else {
			log.Debug("daemon.holds.classification", "the hold waits behind a context cut", ctx)
		}
		out.Classification = &frontendv1.HeldPrompt_UninterruptibleTurn{
			UninterruptibleTurn: &frontendv1.HeldPromptUninterruptibleTurn{Command: h.Classification.Command}}
	case wsm.ArmClassificationError:
		ctx["arm"] = "classification_error"
		log.Debug("daemon.holds.classification", "the hold has no verdict because the classifier failed", ctx)
		out.Classification = &frontendv1.HeldPrompt_ClassificationError{
			ClassificationError: &frontendv1.HeldPromptClassificationError{Detail: h.Classification.Reason}}
	default:
		ctx["arm"] = int(h.Classification.Arm)
		ctx["invariant_violation"] = "unknown wsm.ClassificationArm"
		ctx["remediation"] = "add the arm to the tray's projection"
		log.Error("daemon.holds.classification",
			"a hold carried an unknown verdict and drew the classifying arm", ctx)
		out.Classification = classifying
	}
}

// setHold projects the daemon-side condition onto the tray's oneof. NO ARM is
// the ordinary case — a turn is running and the classifier decides delivery —
// so a nil condition leaves the oneof unset rather than inventing a reason.
func setHold(out *frontendv1.HeldPrompt, h wsm.HeldPrompt, log dlog.Logger) {
	ctx := dlog.Context{"turn_id": string(h.Turn)}
	if h.Hold == nil {
		log.Debug("daemon.holds.hold", "the hold is held by the running turn alone", ctx)
		return
	}
	switch *h.Hold {
	case wsm.HoldShutdown:
		ctx["hold"] = "shutdown"
		ctx["schedule_id"] = h.ScheduleID
		if h.ScheduleID == "" {
			ctx["invariant_violation"] = "HeldPromptShutdownHold.schedule_id is empty"
			ctx["remediation"] = "record the drain schedule the hold waits on"
			log.Error("daemon.holds.hold", "a shutdown hold named no schedule and was emitted without one", ctx)
		} else {
			log.Debug("daemon.holds.hold", "the hold waits on the shutdown drain", ctx)
		}
		out.Hold = &frontendv1.HeldPrompt_Shutdown{
			Shutdown: &frontendv1.HeldPromptShutdownHold{ScheduleId: h.ScheduleID}}
	case wsm.HoldReconnect:
		ctx["hold"] = "reconnect"
		log.Debug("daemon.holds.hold", "the hold waits for the session to reconnect", ctx)
		out.Hold = &frontendv1.HeldPrompt_Reconnect{
			Reconnect: &frontendv1.HeldPromptReconnectHold{}}
	case wsm.HoldBuildRefresh:
		ctx["hold"] = "build_refresh"
		log.Debug("daemon.holds.hold", "the hold waits for the build refresh", ctx)
		out.Hold = &frontendv1.HeldPrompt_BuildRefresh{
			BuildRefresh: &frontendv1.HeldPromptBuildRefreshHold{}}
	case wsm.HoldMerge:
		ctx["hold"] = "merge"
		log.Debug("daemon.holds.hold", "the hold waits for the merge to end", ctx)
		out.Hold = &frontendv1.HeldPrompt_Merge{Merge: &frontendv1.HeldPromptMergeHold{}}
	default:
		ctx["hold"] = int(*h.Hold)
		ctx["invariant_violation"] = "unknown wsm.HoldKind"
		ctx["remediation"] = "add the kind to the tray's projection"
		log.Error("daemon.holds.hold", "a hold carried an unknown kind and drew no hold arm", ctx)
	}
}

// commandLabelMax is how many characters of a context cut's command the short
// label keeps, so a badge stays one to three words however long a literal grows.
const commandLabelMax = 24

// heldBadges composes the card's status badges from the projected entry: THE
// ONE PLACE a held prompt's status words are decided. One badge per standing
// fact, in the order daemon_hold.proto fixes — the verdict, the edit, the
// verdict's confirmation, the hold — each a short label and, where the full
// sentence says more than the label, that sentence as the detail.
//
// A classification arm it cannot name is recorded LOUDLY and contributes no
// badge: the projection above never leaves one, and a frontend rejects a badge
// list that disagrees with the arms, so the defect is refused where it is drawn
// rather than papered over with invented words.
func heldBadges(p *frontendv1.HeldPrompt, log dlog.Logger) []*frontendv1.HeldPromptBadge {
	ctx := dlog.Context{"turn_id": p.GetTurn().GetValue()}
	var out, confirmed []*frontendv1.HeldPromptBadge
	switch arm := p.GetClassification().(type) {
	case *frontendv1.HeldPrompt_Classifying:
		out = append(out, badge("classifying", "queued — classifying"))
	case *frontendv1.HeldPrompt_Interject:
		out = append(out, badge("interrupting", "interjects"))
	case *frontendv1.HeldPrompt_AfterToolCall:
		out = append(out, badge("after this tool call", "joins the running turn after its current tool call"))
	case *frontendv1.HeldPrompt_HoldForTurnEnd:
		// A refused interrupt is returned to this arm, so it reads the same.
		out = append(out, badge("after this turn", "after this turn"))
		if arm.HoldForTurnEnd.GetAccepted().GetAccepted() {
			confirmed = append(confirmed, badge("confirmed", "confirmed"))
		}
	case *frontendv1.HeldPrompt_UninterruptibleTurn:
		literal, ok := commandLiteral(arm.UninterruptibleTurn.GetCommand())
		if ok {
			out = append(out, badge("after "+truncateLabel(literal, commandLabelMax), "waits for "+literal+" to finish"))
		} else {
			ctx["command"] = arm.UninterruptibleTurn.GetCommand().String()
			ctx["invariant_violation"] = "HeldPromptUninterruptibleTurn.command has no literal"
			ctx["remediation"] = "record a recognized session command with the verdict"
			log.Error("daemon.holds.badges", "an uninterruptible-turn badge could not name its command", ctx)
			out = append(out, badge("after a context cut", "waits for a context cut to finish"))
		}
	case *frontendv1.HeldPrompt_ClassificationError:
		out = append(out, badge("unclassified", "unclassified"))
	case *frontendv1.HeldPrompt_DaemonHeld:
		// No badge: the hold arm's badge below is what holds the entry.
	default:
		ctx["invariant_violation"] = "HeldPrompt.classification has no badge"
		ctx["remediation"] = "add the arm to heldBadges"
		log.Error("daemon.holds.badges", "a held prompt's verdict has no badge and was composed without one", ctx)
	}
	if p.GetEditing() != nil {
		out = append(out, badge("editing", "editing"))
	}
	if p.GetCoalesced() != nil {
		out = append(out, badge("coalesced", "later prompts were folded into this one"))
	}
	out = append(out, confirmed...)
	switch arm := p.GetHold().(type) {
	case nil:
	case *frontendv1.HeldPrompt_Shutdown:
		sentence := "held for the scheduled restart"
		if id := arm.Shutdown.GetScheduleId(); id != "" {
			sentence += " (" + id + ")"
		}
		out = append(out, badge("restart hold", sentence))
	case *frontendv1.HeldPrompt_BuildRefresh:
		out = append(out, badge("build refresh", "held for the build refresh"))
	case *frontendv1.HeldPrompt_Reconnect:
		out = append(out, badge("after reconnect", "held until the session reconnects"))
	case *frontendv1.HeldPrompt_Merge:
		out = append(out, badge("after the merge", "held until the merge ends; the workspace stays open for it"))
	default:
		ctx["invariant_violation"] = "HeldPrompt.hold has no badge"
		ctx["remediation"] = "add the arm to heldBadges"
		log.Error("daemon.holds.badges", "a held prompt's hold has no badge and was composed without one", ctx)
	}
	labels := make([]string, len(out))
	for i, b := range out {
		labels[i] = b.GetLabel()
	}
	ctx["labels"] = strings.Join(labels, ",")
	log.Debug("daemon.holds.badges", "the held prompt's badges were composed", ctx)
	return out
}

// badge is one badge: LABEL always, SENTENCE as the detail only when it says
// more than the label does.
func badge(label, sentence string) *frontendv1.HeldPromptBadge {
	b := &frontendv1.HeldPromptBadge{Label: label}
	if sentence != label {
		b.Detail = &sentence
	}
	return b
}

// truncateLabel keeps at most MAX characters of S, the last of them an
// ellipsis when anything was cut.
func truncateLabel(s string, max int) string {
	runes := []rune(s)
	if len(runes) <= max {
		return s
	}
	return string(runes[:max-1]) + "…"
}

// commandLiteral is COMMAND as the user types it, read off the enum value's
// own session_command_spec option so the badge and the recognizer spell it
// from one definition. False when the value carries no literal (UNSPECIFIED
// deliberately carries none).
func commandLiteral(command conversationv1.SessionCommand) (string, bool) {
	value := command.Descriptor().Values().ByNumber(command.Number())
	if value == nil {
		return "", false
	}
	options, ok := value.Options().(*descriptorpb.EnumValueOptions)
	if !ok {
		return "", false
	}
	spec, ok := proto.GetExtension(options, conversationv1.E_SessionCommandSpec).(*conversationv1.SessionCommandSpec)
	if !ok || spec.GetLiteral() == "" {
		return "", false
	}
	return spec.GetLiteral(), true
}
