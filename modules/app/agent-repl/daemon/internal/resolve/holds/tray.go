package holds

import (
	"sort"

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
	out.Badge, out.Notes = heldStatus(out, log)
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

// heldStatus composes the card's ONE badge and the notes for every other
// standing fact: THE ONE PLACE a held prompt's status words are decided, and
// the one place its facts are ranked. The strongest standing fact claims the
// badge, in the order daemon_hold.proto fixes — the edit, the hold, the
// verdict — and the rest are notes, strongest first, then the verdict's
// confirmation, then the coalescence.
//
// A fact it cannot name is recorded LOUDLY and contributes nothing: an entry
// left with no badge is rejected by its frontend, so the defect is refused
// where it is drawn rather than papered over with invented words.
func heldStatus(p *frontendv1.HeldPrompt, log dlog.Logger) (*frontendv1.HeldPromptBadge, []*frontendv1.HeldPromptStatusNote) {
	ctx := dlog.Context{"turn_id": p.GetTurn().GetValue()}
	var ranked []*frontendv1.HeldPromptBadge
	if p.GetEditing() != nil {
		ranked = append(ranked, badge("editing", "editing", func(b *frontendv1.HeldPromptBadge) {
			b.StandsFor = &frontendv1.HeldPromptBadge_Editing{Editing: &frontendv1.HeldPromptBadgeEditing{}}
		}))
	}
	if hold := holdBadge(p, ctx, log); hold != nil {
		ranked = append(ranked, hold)
	}
	if verdict := verdictBadge(p, ctx, log); verdict != nil {
		ranked = append(ranked, verdict)
	}
	var notes []*frontendv1.HeldPromptStatusNote
	for _, lost := range ranked[min(1, len(ranked)):] {
		notes = append(notes, note(lost.GetDetail(), lost.GetLabel()))
	}
	if p.GetHoldForTurnEnd().GetAccepted().GetAccepted() {
		notes = append(notes, note("", "confirmed"))
	}
	if p.GetCoalesced() != nil {
		notes = append(notes, note("", "later prompts were folded into this one"))
	}
	if len(ranked) == 0 {
		ctx["invariant_violation"] = "a held prompt has no standing fact to badge"
		ctx["remediation"] = "add the fact to heldStatus"
		log.Error("daemon.holds.badge", "a held prompt was composed with no badge", ctx)
		return nil, notes
	}
	ctx["label"] = ranked[0].GetLabel()
	ctx["notes"] = len(notes)
	log.Debug("daemon.holds.badge", "the held prompt's badge and notes were composed", ctx)
	return ranked[0], notes
}

// verdictBadge is the classification arm's badge, nil for `daemon_held` (its
// hold arm's badge is what holds the entry) and for an arm it cannot name.
func verdictBadge(p *frontendv1.HeldPrompt, ctx dlog.Context, log dlog.Logger) *frontendv1.HeldPromptBadge {
	switch arm := p.GetClassification().(type) {
	case *frontendv1.HeldPrompt_Classifying:
		return badge("classifying", "queued — classifying", func(b *frontendv1.HeldPromptBadge) {
			b.StandsFor = &frontendv1.HeldPromptBadge_Classifying{Classifying: &frontendv1.HeldPromptBadgeClassifying{}}
		})
	case *frontendv1.HeldPrompt_Interject:
		return badge("interrupting", "interjects", func(b *frontendv1.HeldPromptBadge) {
			b.StandsFor = &frontendv1.HeldPromptBadge_Interject{Interject: &frontendv1.HeldPromptBadgeInterject{}}
		})
	case *frontendv1.HeldPrompt_AfterToolCall:
		return badge("after this tool call", "joins the running turn after its current tool call", func(b *frontendv1.HeldPromptBadge) {
			b.StandsFor = &frontendv1.HeldPromptBadge_AfterToolCall{AfterToolCall: &frontendv1.HeldPromptBadgeAfterToolCall{}}
		})
	case *frontendv1.HeldPrompt_HoldForTurnEnd:
		// A refused interrupt is returned to this arm, so it reads the same.
		return badge("after this turn", "after this turn", func(b *frontendv1.HeldPromptBadge) {
			b.StandsFor = &frontendv1.HeldPromptBadge_HoldForTurnEnd{HoldForTurnEnd: &frontendv1.HeldPromptBadgeHoldForTurnEnd{}}
		})
	case *frontendv1.HeldPrompt_UninterruptibleTurn:
		stands := func(b *frontendv1.HeldPromptBadge) {
			b.StandsFor = &frontendv1.HeldPromptBadge_UninterruptibleTurn{UninterruptibleTurn: &frontendv1.HeldPromptBadgeUninterruptibleTurn{}}
		}
		literal, ok := commandLiteral(arm.UninterruptibleTurn.GetCommand())
		if ok {
			return badge("after "+truncateLabel(literal, commandLabelMax), "waits for "+literal+" to finish", stands)
		}
		ctx["command"] = arm.UninterruptibleTurn.GetCommand().String()
		ctx["invariant_violation"] = "HeldPromptUninterruptibleTurn.command has no literal"
		ctx["remediation"] = "record a recognized session command with the verdict"
		log.Error("daemon.holds.badge", "an uninterruptible-turn badge could not name its command", ctx)
		return badge("after a context cut", "waits for a context cut to finish", stands)
	case *frontendv1.HeldPrompt_ClassificationError:
		return badge("unclassified", "unclassified", func(b *frontendv1.HeldPromptBadge) {
			b.StandsFor = &frontendv1.HeldPromptBadge_ClassificationError{ClassificationError: &frontendv1.HeldPromptBadgeClassificationError{}}
		})
	case *frontendv1.HeldPrompt_DaemonHeld:
		return nil
	default:
		ctx["invariant_violation"] = "HeldPrompt.classification has no badge"
		ctx["remediation"] = "add the arm to verdictBadge"
		log.Error("daemon.holds.badge", "a held prompt's verdict has no badge and was composed without one", ctx)
		return nil
	}
}

// holdBadge is the hold arm's badge, nil when no hold is set or for an arm it
// cannot name.
func holdBadge(p *frontendv1.HeldPrompt, ctx dlog.Context, log dlog.Logger) *frontendv1.HeldPromptBadge {
	switch arm := p.GetHold().(type) {
	case nil:
		return nil
	case *frontendv1.HeldPrompt_Shutdown:
		sentence := "held for the scheduled restart"
		if id := arm.Shutdown.GetScheduleId(); id != "" {
			sentence += " (" + id + ")"
		}
		return badge("restart hold", sentence, func(b *frontendv1.HeldPromptBadge) {
			b.StandsFor = &frontendv1.HeldPromptBadge_Shutdown{Shutdown: &frontendv1.HeldPromptBadgeShutdown{}}
		})
	case *frontendv1.HeldPrompt_BuildRefresh:
		return badge("build refresh", "held for the build refresh", func(b *frontendv1.HeldPromptBadge) {
			b.StandsFor = &frontendv1.HeldPromptBadge_BuildRefresh{BuildRefresh: &frontendv1.HeldPromptBadgeBuildRefresh{}}
		})
	case *frontendv1.HeldPrompt_Reconnect:
		return badge("after reconnect", "held until the session reconnects", func(b *frontendv1.HeldPromptBadge) {
			b.StandsFor = &frontendv1.HeldPromptBadge_Reconnect{Reconnect: &frontendv1.HeldPromptBadgeReconnect{}}
		})
	case *frontendv1.HeldPrompt_Merge:
		return badge("after the merge", "held until the merge ends; the workspace stays open for it", func(b *frontendv1.HeldPromptBadge) {
			b.StandsFor = &frontendv1.HeldPromptBadge_Merge{Merge: &frontendv1.HeldPromptBadgeMerge{}}
		})
	default:
		ctx["invariant_violation"] = "HeldPrompt.hold has no badge"
		ctx["remediation"] = "add the arm to holdBadge"
		log.Error("daemon.holds.badge", "a held prompt's hold has no badge and was composed without one", ctx)
		return nil
	}
}

// badge is one badge: LABEL always, SENTENCE as the detail only when it says
// more than the label does, and the fact it stands for, which SET installs.
func badge(label, sentence string, set func(*frontendv1.HeldPromptBadge)) *frontendv1.HeldPromptBadge {
	b := &frontendv1.HeldPromptBadge{Label: label}
	if sentence != label {
		b.Detail = &sentence
	}
	set(b)
	return b
}

// note says a fact that lost the badge: its full sentence, else its label.
func note(detail, label string) *frontendv1.HeldPromptStatusNote {
	if detail == "" {
		detail = label
	}
	return &frontendv1.HeldPromptStatusNote{Sentence: detail}
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
