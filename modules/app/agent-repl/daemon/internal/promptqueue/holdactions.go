package promptqueue

import (
	"context"
	"fmt"

	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/wsm"
)

// The tombstone kinds a retired hold carries.
const (
	tombstoneDelivered = "delivered"
	tombstoneDropped   = "dropped"
)

// Release delivers a held prompt NOW, overriding the hold and interrupting the
// running turn when that is what delivery takes.
//
// It REFUSES a hold nothing could deliver through: an uninterruptible turn has
// no interrupt to send, and a session that is still coming up has nothing to
// send to.
func (q *queue) Release(ctx context.Context, ws ids.WorkspaceID, turn ids.TurnID) error {
	log, err := q.logger(ctx, ws)
	if err != nil {
		return err
	}
	log = log.With(dlog.Context{"turn": string(turn)})

	// A RELEASE IS A DELIVERY DECISION, so it is taken under the delivery
	// lock: that is what lets a standing edit withhold it structurally.
	drain := &q.state(ws).drain
	drain.Lock()
	defer drain.Unlock()

	held, err := q.standingHold(ctx, ws, turn)
	if err != nil {
		log.Warn(opRelease, "there is no such standing hold to release", dlog.Context{"cause": err.Error()})
		return err
	}
	if q.withheldByEdit(ws, held) {
		log.Info(opRelease, "the prompt is being edited or is queued after one that is; the release is refused", nil)
		return ErrReleaseRefused
	}
	if held.Classification != nil && held.Classification.Arm == wsm.ArmUninterruptibleTurn {
		log.Warn(opRelease, "the running turn is uninterruptible; the release is refused", nil)
		return ErrReleaseRefused
	}
	if held.Hold != nil && *held.Hold == wsm.HoldSessionStarting {
		log.Warn(opRelease, "the session is still coming up; the release is refused", nil)
		return ErrReleaseRefused
	}
	// THE HOLD STAMP IS NOT THE ONLY EVIDENCE THE SESSION IS STILL COMING UP.
	// releaseRevivalHolds un-stamps every revival-pending hold as soon as the
	// bring-up reports a client, and only then delivers them; a release that
	// lands inside that window finds no stamp but still has nothing live to
	// send to. Answering "there is no session" there would be a lie about a
	// workspace that is mid-revival, so the bring-up that stamped the hold is
	// consulted directly and the domain-correct refusal stands.
	if q.isReviving(ws) {
		log.Warn(opRelease, "the session's bring-up is still running; the release is refused", nil)
		return ErrReleaseRefused
	}
	// A DRAINING WORKSPACE IS BETWEEN SHIMS: the bounce's own finish delivers
	// every held prompt to the new one, so a release here has nothing to
	// deliver through.
	if q.isDraining(ws) {
		log.Warn(opRelease, "the workspace is draining for a bounce; the release is refused and the prompt goes to the new shim", nil)
		return ErrReleaseRefused
	}

	watcher, ok := q.deps.Watcher(ws)
	if !ok {
		log.Warn(opRelease, "the workspace has no session watcher", nil)
		return ErrNoSession
	}
	// A RECORDED CONTEXT CUT IS RUNNING whether or not the watcher has learned
	// of it yet, and a force-through would deliver into it or interject it.
	if cut, ok := q.runningCut(ws); ok {
		keptBehindSessionAct(log, opRelease, cut, turn,
			"a session act is running; the release is refused and the prompt waits for the act to end")
		return ErrReleaseRefused
	}
	if running := watcher.TurnInFlight(); running != nil {
		// Delivery takes an interrupt: the release becomes the semantic head
		// and the real turn end delivers it, exactly as an interjection does.
		// A running session act refuses the interjection, and so the release.
		if !q.interject(ctx, submissionOf(held), *running, log) {
			return ErrReleaseRefused
		}
		return nil
	}
	return q.deliverHeld(ctx, ws, held, log)
}

// Drop discards a held prompt, DURABLY FIRST: the tombstone is written before
// anything else, and a failed drop refuses the action rather than leaving a
// prompt the user believes is gone.
func (q *queue) Drop(ctx context.Context, ws ids.WorkspaceID, turn ids.TurnID) error {
	log, err := q.logger(ctx, ws)
	if err != nil {
		return err
	}
	log = log.With(dlog.Context{"turn": string(turn)})

	// Under the delivery lock, because a drop can retire the edit that
	// withholds the prompts behind it.
	drain := &q.state(ws).drain
	drain.Lock()
	defer drain.Unlock()

	if _, err := q.standingHold(ctx, ws, turn); err != nil {
		log.Warn(opDrop, "there is no such standing hold to drop", dlog.Context{"cause": err.Error()})
		return err
	}
	if err := q.deps.DB.TombstoneHeldPrompt(ctx, turn, wsm.Tombstone{Kind: tombstoneDropped, At: q.deps.Now()}); err != nil {
		log.Error(opDrop, "the drop was refused: the hold could not be retired", dlog.Context{"cause": err.Error()})
		return fmt.Errorf("drop hold %q on %q: %w", turn, ws, err)
	}
	q.clearHeadIf(ws, turn)
	log.Info(opDrop, "the held prompt was dropped", nil)
	if err := q.pushTray(ctx, ws, log); err != nil {
		return err
	}
	q.retireEditIf(ctx, ws, turn, tombstoneDropped, log)
	return nil
}

// Accept confirms a hold_for_turn_end verdict. It is VIEW STATE ONLY: delivery
// is unchanged and the tray re-pushes.
func (q *queue) Accept(ctx context.Context, ws ids.WorkspaceID, turn ids.TurnID) error {
	log, err := q.logger(ctx, ws)
	if err != nil {
		return err
	}
	log = log.With(dlog.Context{"turn": string(turn)})

	held, err := q.standingHold(ctx, ws, turn)
	if err != nil {
		log.Warn(opAccept, "there is no such standing hold to accept", dlog.Context{"cause": err.Error()})
		return err
	}
	if held.Classification == nil || held.Classification.Arm != wsm.ArmHoldForTurnEnd {
		log.Warn(opAccept, "accept is legal only on a hold_for_turn_end verdict", dlog.Context{
			"arm": classificationName(held),
		})
		return ErrAcceptNotApplicable
	}
	if err := q.deps.DB.SetHeldPromptAccepted(ctx, turn); err != nil {
		log.Error(opAccept, "the acceptance could not be recorded", dlog.Context{"cause": err.Error()})
		return fmt.Errorf("accept hold %q on %q: %w", turn, ws, err)
	}
	log.Info(opAccept, "the user accepted the wait for the turn's end", nil)
	return q.pushTray(ctx, ws, log)
}

// deliverHeld sends a standing hold to the shim and retires it as delivered.
// Every caller holds the delivery lock and has already filtered what a
// standing edit withholds; the check here is the backstop that keeps a
// caller's defect from sending an edited prompt anyway.
func (q *queue) deliverHeld(ctx context.Context, ws ids.WorkspaceID, held wsm.HeldPrompt, log dlog.Logger) error {
	if q.withheldByEdit(ws, held) {
		log.Error(opDeliver, "a hold a standing edit withholds reached delivery; it was not sent", dlog.Context{
			"invariant_violation": "delivery of a held prompt at or after a standing edit",
			"remediation":         "filter withheldByEdit under the delivery lock before delivering",
		})
		return errDeliveryBehindEdit
	}
	sender, ok := q.deps.Client(ws)
	if !ok {
		// A HIBERNATED SESSION IS IDLE, NOT DEAD. The hold that a hibernation
		// parked is released by the sweep's own lease release, and the release
		// is exactly the moment the prompt must go — so the same revival a
		// fresh submission gets is taken here. Without it the prompt that
		// should have woken the workspace is dropped at its one delivery
		// point and waits forever.
		revived, err := q.revive(ctx, ws, log)
		if err != nil {
			return err
		}
		if !revived {
			log.Warn(opDeliver, "the workspace has no session to deliver the hold to", nil)
			return ErrNoSession
		}
		if sender, ok = q.deps.Client(ws); !ok {
			log.Error(opDeliver, "the revived workspace still has no session to deliver the hold to", nil)
			return ErrNoSession
		}
	}
	watcher, ok := q.deps.Watcher(ws)
	if !ok {
		log.Warn(opDeliver, "the workspace has no session watcher", nil)
		return ErrNoSession
	}

	sub := submissionOf(held)
	var derr error
	if sub.Target != nil {
		_, derr = q.deliverToAgent(ctx, sub, sender, log)
	} else {
		_, derr = q.deliver(ctx, sub, sender, watcher, log)
	}
	if derr != nil {
		return derr
	}

	if err := q.deps.DB.TombstoneHeldPrompt(ctx, held.Turn, wsm.Tombstone{Kind: tombstoneDelivered, At: q.deps.Now()}); err != nil {
		log.Error(opDeliver, "the hold was delivered but not retired", dlog.Context{"cause": err.Error()})
		return fmt.Errorf("retire delivered hold %q on %q: %w", held.Turn, ws, err)
	}
	q.clearHeadIf(ws, held.Turn)
	return q.pushTray(ctx, ws, log)
}

// clearHeadIf drops the semantic head when it names this turn, so a retired
// prompt cannot be delivered a second time by the next turn end.
func (q *queue) clearHeadIf(ws ids.WorkspaceID, turn ids.TurnID) {
	q.mu.Lock()
	defer q.mu.Unlock()
	if state, ok := q.states[ws]; ok && state.head != nil && *state.head == turn {
		state.head = nil
	}
}

// submissionOf reconstructs the submission a standing hold recorded.
func submissionOf(held wsm.HeldPrompt) Submission {
	return Submission{
		WS:       held.Workspace,
		Turn:     held.Turn,
		Said:     held.Said,
		Origin:   conversationv1.PromptOrigin(conversationv1.PromptOrigin_value[held.Origin]),
		Target:   held.Target,
		Delivery: held.Delivery,
	}
}

// classificationName renders a hold's verdict for a log record, naming its
// absence rather than a zero arm.
func classificationName(held wsm.HeldPrompt) string {
	if held.Classification == nil {
		return "none"
	}
	return armName(held.Classification.Arm)
}
