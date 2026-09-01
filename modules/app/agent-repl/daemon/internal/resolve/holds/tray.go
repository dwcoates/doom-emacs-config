package holds

import (
	"sort"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
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
// record is not a standing hold at all.
//
// EVERY BRANCH LOGS. The two facts the contract requires — a classification arm
// and, on the uninterruptible arm, the command that made the turn
// uninterruptible — are recorded LOUDLY when the record cannot supply them, and
// the entry is still emitted: a prompt the daemon is really holding must be
// visible even when its explanation is defective, and dropping it would hide
// pending work.
func heldPrompt(h wsm.HeldPrompt, log dlog.Logger) *frontendv1.HeldPrompt {
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
	return out
}

// setClassification projects the durable verdict onto the tray's oneof. A
// record with NO verdict yet is the `classifying` arm — the judge is still
// running — which is why the arm is never left unset.
func setClassification(out *frontendv1.HeldPrompt, h wsm.HeldPrompt, log dlog.Logger) {
	ctx := dlog.Context{"turn_id": string(h.Turn)}
	classifying := &frontendv1.HeldPrompt_Classifying{Classifying: &frontendv1.HeldPromptClassifying{}}
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
//
// There is no keep-alive arm to project: the keep-alive window is the shim's
// and the daemon never holds a prompt for it, so wsm.HoldKind has no such
// value and frontend.v1's keep_alive arm is dead on this side.
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
	case wsm.HoldSessionStarting:
		ctx["hold"] = "session_starting"
		log.Debug("daemon.holds.hold", "the hold waits for the session to come up", ctx)
		out.Hold = &frontendv1.HeldPrompt_SessionStarting{
			SessionStarting: &frontendv1.HeldPromptSessionStartingHold{}}
	case wsm.HoldBuildRefresh:
		ctx["hold"] = "build_refresh"
		log.Debug("daemon.holds.hold", "the hold waits for the build refresh", ctx)
		out.Hold = &frontendv1.HeldPrompt_BuildRefresh{
			BuildRefresh: &frontendv1.HeldPromptBuildRefreshHold{}}
	default:
		ctx["hold"] = int(*h.Hold)
		ctx["invariant_violation"] = "unknown wsm.HoldKind"
		ctx["remediation"] = "add the kind to the tray's projection"
		log.Error("daemon.holds.hold", "a hold carried an unknown kind and drew no hold arm", ctx)
	}
}
