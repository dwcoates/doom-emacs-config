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
	// tombstoneRolledBack retires a held prompt a rollback dropped: it was
	// queued after the prompt the conversation was rolled back to.
	tombstoneRolledBack = "rolled_back"
	// tombstoneCoalesced retires a prompt folded into the queued prompt ahead
	// of it.
	tombstoneCoalesced = "coalesced"
	// tombstoneWithdrawn retires the accepted turn a stop withdrew while its
	// session was still coming up (WithdrawRevivalTurn).
	tombstoneWithdrawn = "withdrawn"
)

// Release delivers a held prompt NOW, overriding the hold and interrupting the
// running turn when that is what delivery takes.
//
// It REFUSES a hold nothing could deliver through: an uninterruptible turn has
// no interrupt to send, a session that is still coming up has nothing to send
// to, and a session a merge drives is the merge's.
func (q *queue) Release(ctx context.Context, ws ids.WorkspaceID, turn ids.TurnID) error {
	log, err := q.logger(ctx, ws)
	if err != nil {
		return err
	}
	log = log.With(dlog.Context{"turn": string(turn)})

	// THE INTERRUPT A RELEASE TAKES IS SENT OFF THE DELIVERY LOCK, once the
	// decision below has registered it: the running turn's end takes the lock
	// to pop what the interrupt made the head.
	var interrupt func()
	defer func() {
		if interrupt != nil {
			interrupt()
		}
	}()

	// A RELEASE IS A DELIVERY DECISION, so it is taken under the delivery
	// lock: that is what lets a standing edit withhold it structurally.
	d := q.lockDelivery(ws)
	defer d.unlock()

	held, err := q.standingHold(ctx, ws, turn)
	if err != nil {
		log.Warn(opRelease, "there is no such standing hold to release", dlog.Context{"cause": err.Error()})
		return err
	}
	if err := q.unclaimed(ws, held, log, opRelease); err != nil {
		return err
	}
	// A SHIM CALL IN FLIGHT IS A DELIVERY UNDER WAY: forcing another prompt
	// through beside it would race it for the session, so the release is
	// refused and the user may send it again once the call has settled.
	if call, ok := q.standingCall(ws); ok {
		log.Info(opRelease, "a delivery to the shim is in flight; the release is refused", dlog.Context{
			"call": call.what, "call_turn": string(call.turn),
		})
		return ErrReleaseRefused
	}
	if q.withheldByEdit(ws, held) {
		log.Info(opRelease, "the prompt is being edited or is queued after one that is; the release is refused", nil)
		return ErrReleaseRefused
	}
	if held.Classification != nil && held.Classification.Arm == wsm.ArmUninterruptibleTurn {
		log.Warn(opRelease, "the running turn is uninterruptible; the release is refused", nil)
		return ErrReleaseRefused
	}
	if held.Hold != nil && *held.Hold == wsm.HoldReconnect {
		log.Warn(opRelease, "the session is not up or its vendor does not serve it; the release is refused until it reconnects", nil)
		return ErrReleaseRefused
	}
	// A MERGE DRIVES THE SESSION: a prompt forced into it would run inside the
	// merge's own turns. The merge's end delivers it.
	if held.Hold != nil && *held.Hold == wsm.HoldMerge {
		log.Warn(opRelease, "a merge drives the session; the release is refused", nil)
		return ErrReleaseRefused
	}
	// THE HOLD STAMP IS NOT THE ONLY EVIDENCE THE SESSION IS STILL COMING UP.
	// releaseReconnectHolds un-stamps every reconnect hold as soon as the
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
		sub := submissionOf(held)
		if !q.registerInterjection(ctx, sub, *running, log) {
			return ErrReleaseRefused
		}
		interrupt = func() { q.sendInterrupt(ctx, sub, *running, log) }
		return nil
	}
	_, err = q.deliverHeld(ctx, d, held, log)
	return err
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
	d := q.lockDelivery(ws)
	defer d.unlock()

	held, err := q.standingHold(ctx, ws, turn)
	if err != nil {
		log.Warn(opDrop, "there is no such standing hold to drop", dlog.Context{"cause": err.Error()})
		return err
	}
	return q.dropLocked(ctx, d, held, tombstoneDropped, opDrop, log)
}

// dropLocked retires one standing hold under the delivery lock the caller holds
// (d), DURABLY FIRST, with WHY as its tombstone kind and LOG carrying the
// hold's turn: a hold a call in flight
// claims is refused, the tombstone is written before anything else, and only
// then is the head cleared, the tray re-pushed and an edit it withheld retired.
func (q *queue) dropLocked(ctx context.Context, d *delivery, held wsm.HeldPrompt, why, op string, log dlog.Logger) error {
	ws, turn := d.ws, held.Turn
	if err := q.unclaimed(ws, held, log, op); err != nil {
		return err
	}
	if err := q.deps.DB.TombstoneHeldPrompt(ctx, turn, wsm.Tombstone{Kind: why, At: q.deps.Now()}); err != nil {
		log.Error(op, "the drop was refused: the hold could not be retired", dlog.Context{"cause": err.Error()})
		return fmt.Errorf("drop hold %q on %q: %w", turn, ws, err)
	}
	q.clearHeadIf(ws, turn)
	log.Info(op, "the held prompt was dropped", nil)
	if err := q.pushTray(ctx, ws, log); err != nil {
		return err
	}
	q.retireEditIf(ctx, d, turn, why, log)
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
// Every caller holds the delivery lock (d) and has already filtered what a
// standing edit withholds; the check here is the backstop that keeps a
// caller's defect from sending an edited prompt anyway. Every call it makes
// -- the revival, the delivery itself -- is made with the lock released and
// claims the hold while it stands (call.go).
func (q *queue) deliverHeld(ctx context.Context, d *delivery, held wsm.HeldPrompt, log dlog.Logger) (bool, error) {
	ws := d.ws
	if q.withheldByEdit(ws, held) {
		log.Error(opDeliver, "a hold a standing edit withholds reached delivery; it was not sent", dlog.Context{
			"invariant_violation": "delivery of a held prompt at or after a standing edit",
			"remediation":         "filter withheldByEdit under the delivery lock before delivering",
		})
		return false, errDeliveryBehindEdit
	}
	sender, ok := q.deps.Client(ws)
	if !ok {
		// A HIBERNATED SESSION IS IDLE, NOT DEAD. The hold that a hibernation
		// parked is released by the sweep's own lease release, and the release
		// is exactly the moment the prompt must go — so the same revival a
		// fresh submission gets is taken here. Without it the prompt that
		// should have woken the workspace is dropped at its one delivery
		// point and waits forever.
		var revived bool
		var err error
		d.outside(shimCall{what: "revival", turn: held.Turn, holds: []ids.TurnID{held.Turn}}, log, func() {
			revived, err = q.revive(ctx, ws, log)
		})
		if err != nil {
			return false, err
		}
		if !revived {
			log.Warn(opDeliver, "the workspace has no session to deliver the hold to", nil)
			return false, ErrNoSession
		}
		if sender, ok = q.deps.Client(ws); !ok {
			log.Error(opDeliver, "the revived workspace still has no session to deliver the hold to", nil)
			return false, ErrNoSession
		}
	}
	watcher, ok := q.deps.Watcher(ws)
	if !ok {
		log.Warn(opDeliver, "the workspace has no session watcher", nil)
		return false, ErrNoSession
	}

	sub := submissionOf(held)
	sub.interjected = held.Classification != nil && held.Classification.Arm == wsm.ArmInterject
	sub.fromHold = true
	var derr error
	var disposition Disposition
	if sub.Target != nil {
		disposition, derr = q.deliverToAgent(ctx, d, sub, sender, log)
	} else {
		disposition, derr = q.deliver(ctx, d, sub, sender, watcher, log)
	}
	if derr != nil {
		return false, derr
	}
	if !disposition.Delivered {
		// THE SHIM HELD NO SESSION: the hold was stamped to wait for the
		// session to reconnect, and stays standing.
		return false, nil
	}

	return true, q.retireDelivered(ctx, ws, held.Turn, log)
}

// retireDelivered retires a hold the session took: tombstoned as delivered,
// dropped as the semantic head, and the tray re-pushed. Shared by a started
// hold and a joining one.
func (q *queue) retireDelivered(ctx context.Context, ws ids.WorkspaceID, turn ids.TurnID, log dlog.Logger) error {
	if err := q.deps.DB.TombstoneHeldPrompt(ctx, turn, wsm.Tombstone{Kind: tombstoneDelivered, At: q.deps.Now()}); err != nil {
		log.Error(opDeliver, "the hold was delivered but not retired", dlog.Context{"cause": err.Error()})
		return fmt.Errorf("retire delivered hold %q on %q: %w", turn, ws, err)
	}
	q.clearHeadIf(ws, turn)
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
		Act:      held.Act,
	}
}

// classificationName renders a hold's verdict for a log record, naming its
// absence rather than a zero arm.
func classificationName(held wsm.HeldPrompt) string {
	if held.Classification == nil {
		return "none"
	}
	return held.Classification.Arm.String()
}
