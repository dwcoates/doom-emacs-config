package promptqueue

import (
	"context"
	"errors"
	"fmt"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
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
	// tombstoneSuperseded retires a daemon-submitted prompt a newer one
	// carrying everything it said replaced before it was told (Supersede).
	tombstoneSuperseded = "superseded"
)

// Release delivers a held prompt NOW, overriding the hold: the prompt goes to
// the shim at once, exactly as a prompt the user had just typed with nothing
// to hold it would.
//
// A PROMPT RELEASED WHILE A TURN RUNS JOINS THAT TURN (owner request,
// 2026-10-26). It is sent to the shim on the spot, as a flush sends each held
// prompt (join.go): its row is drawn in the feed as accepted, its card leaves
// the tray, and the vendor takes it in at the running turn's next tool call,
// or runs it as the next turn once that one ends. A release used to interrupt
// the running turn and deliver only at its end: the card stood unchanged until
// then, a second click met the refusal of the delivery under way, and a
// stale-shim refresh that the turn end let in held the prompt for the build
// as well. Only a held session act still interrupts: an act runs as a turn of
// its own and joins nothing.
//
// It REFUSES a hold nothing could deliver through: a context cut is never
// interrupted or joined, a session that is still coming up has nothing to
// send to, and a session a merge drives is the merge's.
func (q *queue) Release(ctx context.Context, ws ids.WorkspaceID, turn ids.TurnID) error {
	log, err := q.logger(ctx, ws)
	if err != nil {
		return err
	}
	log = log.With(dlog.Context{"turn": string(turn)})

	// THE INTERRUPT A RELEASED ACT TAKES IS SENT OFF THE DELIVERY LOCK, once
	// the decision below has registered it: the running turn's end takes the
	// lock to pop what the interrupt made the head.
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
	// A SHIM CALL THAT OPENS A TURN IS THE TURN THE RELEASE JOINS: the join
	// is deferred to the call's settle (joinLocked), never refused. Any other
	// call in flight (a revival, a session act, a rollback) opens nothing to
	// join, and forcing the prompt through beside it would race it for the
	// session, so the release is refused and may be sent again once it settles.
	if call, ok := q.standingCall(ws); ok && !call.opensTurn {
		log.Info(opRelease, "a shim call that opens no turn is in flight; the release is refused", dlog.Context{
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
	// A PROMPT NEVER ANSWERS THE COLD GATE, a forced one included: sending it
	// would re-read the cold conversation without the user's answer, which is
	// the one spend the gate exists to ask about (judgement call 11).
	if held.Hold != nil && *held.Hold == wsm.HoldColdGate {
		log.Info(opRelease, "the session is parked at its cold gate; the release is refused until the gate is answered", nil)
		return ErrReleaseRefused
	}
	// A PROMPT KEPT AT AN EARLIER ANSWER is sent by its release, but not into
	// a session parked at a gate standing again: the gate's own answer decides
	// it there, as it decides every prompt held at it.
	if kind, why, waiting := q.sessionWait(ws); waiting && kind == wsm.HoldColdGate {
		log.Info(opRelease, "the session is parked at its cold gate; the release is refused until the gate is answered", dlog.Context{"why": why})
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

	if _, ok := q.deps.Watcher(ws); !ok {
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
	// A PROMPT ADDRESSED TO AN AGENT IS NOT THE SESSION'S TURN: it goes now
	// whatever the session is running.
	if held.Target != nil {
		_, err = q.deliverHeld(ctx, d, held, log)
		return err
	}
	// A verdict still being judged about this prompt is superseded: the
	// prompt is on its way to the shim, and a verdict settling after it must
	// not interject or re-hold it.
	q.supersede(ws, turn)
	running, ok := q.flushTarget(ws)
	if !ok {
		_, err = q.deliverHeld(ctx, d, held, log)
		return err
	}
	if held.Act != nil {
		return q.releaseActByInterrupt(ctx, held, running, &interrupt, log)
	}
	return q.releaseByJoin(ctx, d, held, running, log)
}

// releaseByJoin sends the released prompt HELD to join RUNNING (joinLocked).
// An interjection under way goes first: the prompt that interjects is already
// the next to go, and any other prompt joined now would run ahead of it.
// The caller holds the delivery lock (d).
func (q *queue) releaseByJoin(ctx context.Context, d *delivery, held wsm.HeldPrompt, running ids.TurnID, log dlog.Logger) error {
	q.mu.Lock()
	interrupting, head := d.state.interrupting, d.state.head
	q.mu.Unlock()
	if interrupting {
		if head != nil && *head == held.Turn {
			log.Info(opRelease, "the prompt is already interjecting; the interrupted turn's end delivers it", dlog.Context{"running_turn": string(running)})
			return nil
		}
		log.Info(opRelease, "an interjection is interrupting the running turn and goes first; the release is refused", dlog.Context{
			"running_turn": string(running), "interjecting_turn": string(*head),
		})
		return ErrReleaseRefused
	}
	switch outcome := q.joinLocked(ctx, d, submissionOf(held), running, joinReleased, log); outcome {
	case joinSent, joinDeferred:
		return nil
	case joinWaiting:
		// joinLocked recorded why on the prompt's verdict, which the card says.
		log.Info(opRelease, "the released prompt could not join the running turn; the release is refused", dlog.Context{"running_turn": string(running)})
		return ErrReleaseRefused
	case joinGone:
		log.Error(opRelease, "a hold read standing under the delivery lock was gone when it was sent to join", dlog.Context{
			"invariant_violation": "a standing hold changes only under the delivery lock",
		})
		return ErrAlreadyDelivered
	default:
		panic(fmt.Sprintf("promptqueue: unknown join outcome %d", outcome))
	}
}

// releaseActByInterrupt releases a held session act while RUNNING runs: an act
// runs as a turn of its own and joins nothing, so it becomes the semantic head
// and an interrupt ends the running turn, whose real end delivers it, exactly
// as an interjection does. The interrupt is set into INTERRUPT, which the
// caller sends once the delivery lock is released. A running session act
// refuses the interjection, and so the release.
func (q *queue) releaseActByInterrupt(ctx context.Context, held wsm.HeldPrompt, running ids.TurnID, interrupt *func(), log dlog.Logger) error {
	sub := submissionOf(held)
	if !q.registerInterjection(ctx, sub, running, log) {
		return ErrReleaseRefused
	}
	*interrupt = func() { q.sendInterrupt(ctx, sub, running, log) }
	return nil
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

// Supersede retires the held prompt TURN because a newer prompt the daemon is
// about to submit will say everything it said (a merge queue notice the
// notifier folds into the next one). It answers whether TURN was retired.
//
// A PROMPT NO LONGER HELD IS ANSWERED false, NEVER AN ERROR: delivered,
// being delivered, or never held (an idle agent was handed it at once), it
// has been or is being told, so the caller's newer prompt must not repeat
// it. The queue's delivery lock arbitrates the race between this retirement
// and a delivery, so exactly one of them takes the prompt.
func (q *queue) Supersede(ctx context.Context, ws ids.WorkspaceID, turn ids.TurnID) (bool, error) {
	log, err := q.logger(ctx, ws)
	if err != nil {
		return false, err
	}
	log = log.With(dlog.Context{"turn": string(turn)})

	d := q.lockDelivery(ws)
	defer d.unlock()

	held, err := q.standingHold(ctx, ws, turn)
	switch {
	case errors.Is(err, ErrAlreadyDelivered), errors.Is(err, ErrNoSuchHold):
		log.Debug(opSupersede, "the prompt is no longer held; it was told, so nothing is superseded", dlog.Context{"cause": err.Error()})
		return false, nil
	case err != nil:
		log.Error(opSupersede, "could not read the holds to supersede a prompt; it stands", dlog.Context{"cause": err.Error()})
		return false, err
	}
	if q.claimedByCall(ws, turn) {
		log.Debug(opSupersede, "the prompt is being delivered to the shim, so nothing is superseded", nil)
		return false, nil
	}
	if err := q.dropLocked(ctx, d, held, tombstoneSuperseded, opSupersede, log); err != nil {
		return false, err
	}
	return true, nil
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
			return false, q.undelivered(ctx, ws, held.Turn, wsm.HoldReconnect, ErrNoSession, log)
		}
		if sender, ok = q.deps.Client(ws); !ok {
			log.Error(opDeliver, "the revived workspace still has no session to deliver the hold to", nil)
			return false, q.undelivered(ctx, ws, held.Turn, wsm.HoldReconnect, ErrNoSession, log)
		}
	}
	// A SESSION PARKED AT ITS COLD GATE IS HANDED NOTHING, and the entry is
	// not an error: a revival above (or before this delivery) parked it, and
	// the gate's answer is the session-up edge that delivers it. The entry goes
	// back on the cold-gate hold, never left standing with no hold
	// (2026-10-10).
	if kind, why, waiting := q.sessionWait(ws); waiting && kind == wsm.HoldColdGate {
		log.Info(opDeliver, "the session is parked at its cold gate; the entry is held until the gate is answered", dlog.Context{"why": why})
		if err := q.reholdForSession(ctx, ws, held.Turn, kind, log); err != nil {
			return false, err
		}
		return false, nil
	}
	watcher, ok := q.deps.Watcher(ws)
	if !ok {
		log.Warn(opDeliver, "the workspace has no session watcher", nil)
		return false, q.undelivered(ctx, ws, held.Turn, wsm.HoldReconnect, ErrNoSession, log)
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

// undelivered answers CAUSE for an entry no session could take, after putting
// it back on the hold that waits for the session (KIND), so the session coming
// up delivers it. The failure is still the caller's to record: this keeps the
// entry from being stranded, never the error from being said.
func (q *queue) undelivered(ctx context.Context, ws ids.WorkspaceID, turn ids.TurnID, kind wsm.HoldKind, cause error, log dlog.Logger) error {
	if err := q.reholdForSession(ctx, ws, turn, kind, log); err != nil {
		return errors.Join(cause, err)
	}
	log.Info(opDeliver, "the undelivered entry is back on its hold; the session coming up delivers it", dlog.Context{"hold": kind.String()})
	return cause
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
		SentBy:   held.SentBy,
		Target:   held.Target,
		Delivery: held.Delivery,
		Act:      held.Act,
		Ask:      held.Ask,
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
