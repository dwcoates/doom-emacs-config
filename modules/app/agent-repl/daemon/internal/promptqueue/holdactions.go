package promptqueue

import (
	"context"
	"errors"
	"fmt"
	"strings"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/promptorigin"
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

// Release is the user's "Send now" on a held prompt: SEND NOW MEANS SEND NOW
// (owner ruling, 2026-11-10). The prompt goes to the shim at once, exactly as
// a prompt the user had just typed with nothing to hold it would, and what
// keeps it from going at once never answers a bare refusal:
//
//   - A PROMPT RELEASED WHILE A TURN RUNS JOINS THAT TURN (owner request,
//     2026-10-26): sent to the shim on the spot (join.go), its row drawn as
//     accepted, taken in at the running turn's next tool call or run as the
//     next turn once that one ends.
//   - A TURN IT CANNOT JOIN IS INTERRUPTED (releaseByInterrupt): one the
//     vendor started on its own, or one an ask blocks. The prompt becomes the
//     semantic head and the turn's real end delivers it, exactly as an
//     interjection and a released session act are.
//   - A BLOCKER THAT CLEARS ON ITS OWN ACCEPTS THE PRESS AND WAITS: the
//     prompt is marked sent now, durably (wsm.SentNow), its card says what
//     it waits for, and it goes the moment that clears, ahead of every other
//     held prompt but an interjection under way (sentnow.go).
//   - ONLY A SEND THAT WOULD LOSE THE USER'S WORK OR ANSWER WHAT ONLY THE
//     USER CAN IS REFUSED, with its typed cause (ReleaseRefusedError): an
//     edit under way, a cold gate standing, a session that refused the
//     prompt into its turn.
func (q *queue) Release(ctx context.Context, ws ids.WorkspaceID, turn ids.TurnID) error {
	log, err := q.logger(ctx, ws)
	if err != nil {
		return err
	}
	log = log.With(dlog.Context{"turn": string(turn)})

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
	return q.settleRelease(ctx, d, held, q.decideRelease(ctx, d, held, log), false, log)
}

// releaseVerdict is what a release decided about its prompt.
type releaseVerdict int

const (
	// releaseSent: the prompt went (delivered, or sent to join the running
	// turn), so no mark stands on it.
	releaseSent releaseVerdict = iota
	// releaseWaits: something keeps the prompt that clears on its own; the
	// press is accepted and the prompt goes the moment it clears.
	releaseWaits
	// releaseRefused: sending would lose the user's work or answer what only
	// the user can; the press is refused with its cause.
	releaseRefused
	// releaseFailed: the decision itself failed (err).
	releaseFailed
)

// releaseDecision is one release decision: the verdict and its evidence.
type releaseDecision struct {
	verdict releaseVerdict
	// waitsFor completes "it goes once …" for a releaseWaits.
	waitsFor string
	// refusal is a releaseRefused's cause.
	refusal ReleaseRefusal
	// err is a releaseFailed's failure.
	err error
}

func sent() releaseDecision { return releaseDecision{verdict: releaseSent} }
func waits(waitsFor string) releaseDecision {
	return releaseDecision{verdict: releaseWaits, waitsFor: waitsFor}
}
func refused(cause ReleaseRefusal) releaseDecision {
	return releaseDecision{verdict: releaseRefused, refusal: cause}
}
func failed(err error) releaseDecision { return releaseDecision{verdict: releaseFailed, err: err} }
func sentOrFailed(err error) releaseDecision {
	if err != nil {
		return failed(err)
	}
	return sent()
}

// The sentences a waiting release's card completes "it goes once …" with,
// one per blocker that clears on its own.
const (
	waitsForInterruptedTurn = "the running turn it interrupted stops"
	waitsForInterjection    = "the prompt interrupting the running turn has gone ahead of it"
	waitsForReconnect       = "the session reconnects"
	waitsForMerge           = "the merge driving the session ends"
	waitsForBringUp         = "the session finishes coming up"
	waitsForBounce          = "the bounce brings up the new session"
	waitsForSessionTake     = "the session can take it"
)

// waitsForCall is the sentence for a shim call in flight that opens no turn.
func waitsForCall(call shimCall) string {
	return "the session finishes the " + strings.ReplaceAll(call.what, "_", " ") + " under way"
}

// waitsForCut is the sentence for a context cut running.
func waitsForCut(command conversationv1.SessionCommand) string {
	switch command {
	case conversationv1.SessionCommand_SESSION_COMMAND_COMPACT:
		return "the /compact running now ends"
	case conversationv1.SessionCommand_SESSION_COMMAND_CLEAR:
		return "the /clear running now ends"
	default:
		return "the session act running now ends"
	}
}

// decideRelease decides what a release of HELD does now, and does it: sends
// it, interrupts the running turn for it, or answers what keeps it. The caller
// holds the delivery lock (d) and has refused a hold a shim call claims. The
// decision is the same whether the user just pressed "Send now" or a waiting
// press is replayed (sentnow.go), so a press waits on exactly what a fresh
// one would.
func (q *queue) decideRelease(ctx context.Context, d *delivery, held wsm.HeldPrompt, log dlog.Logger) releaseDecision {
	ws := d.ws
	q.mu.Lock()
	interrupting, head := d.state.interrupting, d.state.head
	q.mu.Unlock()
	// A SECOND PRESS WHILE ITS INTERRUPT IS UNDER WAY IS ALREADY GOING: the
	// interrupted turn's end delivers it.
	if interrupting && head != nil && *head == held.Turn {
		return waits(waitsForInterruptedTurn)
	}
	// A SHIM CALL THAT OPENS A TURN IS THE TURN THE RELEASE JOINS: the join
	// is deferred to the call's settle (joinLocked). Any other call in flight
	// (a revival, a session act, a rollback) opens nothing to join, and
	// forcing the prompt through beside it would race it for the session, so
	// the release waits for it to settle.
	if call, ok := q.standingCall(ws); ok && !call.opensTurn {
		return waits(waitsForCall(call))
	}
	// AN EDIT IS THE USER'S WORK UNDER WAY: sending the prompt it stands on
	// would discard it, and sending one behind it would carry that prompt
	// past an edit that keeps everything after it in order.
	if q.withheldByEdit(ws, held) {
		if q.editing(ws) == held.Turn {
			return refused(RefusedBeingEdited)
		}
		return refused(RefusedBehindEdit)
	}
	if held.Classification != nil && held.Classification.Arm == wsm.ArmUninterruptibleTurn {
		return waits(waitsForCut(held.Classification.Command))
	}
	if held.Hold != nil && *held.Hold == wsm.HoldReconnect {
		return waits(waitsForReconnect)
	}
	// A PROMPT NEVER ANSWERS THE COLD GATE, a forced one included: sending it
	// would re-read the cold conversation without the user's answer, which is
	// the one spend the gate exists to ask about (judgement call 11).
	if held.Hold != nil && *held.Hold == wsm.HoldColdGate {
		return refused(RefusedColdGate)
	}
	// A PROMPT KEPT AT AN EARLIER ANSWER is sent by its release, but not into
	// a session parked at a gate standing again: the gate's own answer decides
	// it there, as it decides every prompt held at it.
	if kind, _, waiting := q.sessionWait(ws); waiting && kind == wsm.HoldColdGate {
		return refused(RefusedColdGate)
	}
	// A MERGE DRIVES THE SESSION: a prompt forced into it would run inside the
	// merge's own turns. The merge's end delivers it, first.
	if held.Hold != nil && *held.Hold == wsm.HoldMerge {
		return waits(waitsForMerge)
	}
	// THE HOLD STAMP IS NOT THE ONLY EVIDENCE THE SESSION IS STILL COMING UP.
	// releaseReconnectHolds un-stamps every reconnect hold as soon as the
	// bring-up reports a client, and only then delivers them; a release that
	// lands inside that window finds no stamp but still has nothing live to
	// send to, so the bring-up that stamped the hold is consulted directly.
	if q.isReviving(ws) {
		return waits(waitsForBringUp)
	}
	// A DRAINING WORKSPACE IS BETWEEN SHIMS: the bounce's own finish delivers
	// every held prompt to the new one, this one first.
	if q.isDraining(ws) {
		return waits(waitsForBounce)
	}
	if _, ok := q.deps.Watcher(ws); !ok {
		log.Warn(opRelease, "the workspace has no session watcher", nil)
		return failed(ErrNoSession)
	}
	// A RECORDED CONTEXT CUT IS RUNNING whether or not the watcher has learned
	// of it yet, and a force-through would deliver into it or interject it.
	if cut, ok := q.runningCut(ws); ok {
		keptBehindSessionAct(log, opRelease, cut, held.Turn,
			"a session act is running; the release waits for the act to end")
		return waits(waitsForCut(cut.command))
	}
	// A PROMPT ADDRESSED TO AN AGENT IS NOT THE SESSION'S TURN: it goes now
	// whatever the session is running.
	if held.Target != nil {
		return q.releaseByDelivery(ctx, d, held, log)
	}
	// A verdict still being judged about this prompt is superseded: the
	// prompt is on its way to the shim, and a verdict settling after it must
	// not interject or re-hold it.
	q.supersede(ws, held.Turn)
	running, ok := q.flushTarget(ws)
	if !ok {
		return q.releaseByDelivery(ctx, d, held, log)
	}
	if held.Act != nil {
		return q.releaseByInterrupt(ctx, d, held, running, log)
	}
	return q.releaseByJoin(ctx, d, held, running, log)
}

// releaseByDelivery delivers HELD into a session running no turn of its own:
// one the shim held no session for is held again for the session to
// reconnect, and waits for it.
func (q *queue) releaseByDelivery(ctx context.Context, d *delivery, held wsm.HeldPrompt, log dlog.Logger) releaseDecision {
	delivered, err := q.deliverHeld(ctx, d, held, log)
	if err != nil || delivered {
		return sentOrFailed(err)
	}
	return waits(waitsForReconnect)
}

// releaseByJoin sends the released prompt HELD to join RUNNING (joinLocked).
// An interjection under way goes first: the prompt that interjects is already
// the next to go, so the release waits for it. A turn the prompt cannot join
// is interrupted for it instead (releaseByInterrupt). The caller holds the
// delivery lock (d).
func (q *queue) releaseByJoin(ctx context.Context, d *delivery, held wsm.HeldPrompt, running ids.TurnID, log dlog.Logger) releaseDecision {
	q.mu.Lock()
	interrupting := d.state.interrupting
	q.mu.Unlock()
	if interrupting {
		return waits(waitsForInterjection)
	}
	switch outcome := q.joinLocked(ctx, d, submissionOf(held), running, joinReleased, log); outcome {
	case joinSent:
		return sent()
	case joinDeferred:
		// The call's settle sends it to join (redriveJoins); a join that
		// cannot go then leaves the mark to be replayed.
		return waits(waitsForSessionTake)
	case joinUnjoinable, joinAskOpen:
		return q.releaseByInterrupt(ctx, d, held, running, log)
	case joinWaiting:
		// joinLocked recorded why on the prompt's verdict.
		return waits(waitsForSessionTake)
	case joinRefused:
		return refused(RefusedSessionRefused)
	case joinGone:
		log.Error(opRelease, "a hold read standing under the delivery lock was gone when it was sent to join", dlog.Context{
			"invariant_violation": "a standing hold changes only under the delivery lock",
		})
		return failed(ErrAlreadyDelivered)
	default:
		panic(fmt.Sprintf("promptqueue: unknown join outcome %d", outcome))
	}
}

// releaseByInterrupt releases HELD by interrupting RUNNING: the prompt becomes
// the semantic head and the running turn's real end delivers it, the one path
// an interjection takes (registerInterjection, sendInterrupt). It serves a
// held session act, which runs as a turn of its own and joins nothing, and a
// prompt whose running turn cannot be joined. The interrupt is sent once the
// delivery lock is released (delivery.afterUnlock): the running turn's end
// takes the lock to pop what the interrupt made the head. A running session
// act refuses the interjection, and the release then waits for the act's end.
func (q *queue) releaseByInterrupt(ctx context.Context, d *delivery, held wsm.HeldPrompt, running ids.TurnID, log dlog.Logger) releaseDecision {
	sub := submissionOf(held)
	if !q.registerInterjection(ctx, sub, running, log) {
		cut, _ := q.runningCut(d.ws)
		return waits(waitsForCut(cut.command))
	}
	d.afterUnlock(func() { q.sendInterrupt(ctx, sub, running, log) })
	log.Info(opRelease, "the running turn cannot take the released prompt; it is interrupted, and its end delivers the prompt", dlog.Context{
		"running_turn": string(running), "session_act": held.Act != nil,
	})
	return waits(waitsForInterruptedTurn)
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
// holdAtStandingGate puts TURN back on the cold-gate hold when a cold gate
// stands on WS, answering whether one did.
func (q *queue) holdAtStandingGate(ctx context.Context, ws ids.WorkspaceID, turn ids.TurnID, log dlog.Logger) (bool, error) {
	kind, why, waiting := q.sessionWait(ws)
	if !waiting || kind != wsm.HoldColdGate {
		return false, nil
	}
	log.Info(opDeliver, "the session is parked at its cold gate; the entry is held until the gate is answered", dlog.Context{"why": why})
	if err := q.reholdForSession(ctx, ws, turn, kind, log); err != nil {
		return true, err
	}
	return true, nil
}

func (q *queue) deliverHeld(ctx context.Context, d *delivery, held wsm.HeldPrompt, log dlog.Logger) (bool, error) {
	ws := d.ws
	if q.withheldByEdit(ws, held) {
		log.Error(opDeliver, "a hold a standing edit withholds reached delivery; it was not sent", dlog.Context{
			"invariant_violation": "delivery of a held prompt at or after a standing edit",
			"remediation":         "filter withheldByEdit under the delivery lock before delivering",
		})
		return false, errDeliveryBehindEdit
	}
	// A SESSION PARKED AT ITS COLD GATE IS HANDED NOTHING, and the entry is
	// not an error: the gate's answer is the session-up edge that delivers it,
	// and the gate runs no shim until then (owner ruling, 2026-10-27), so it is
	// read before any client is looked for. The entry goes back on the
	// cold-gate hold, never left standing with no hold (2026-10-10).
	if parked, err := q.holdAtStandingGate(ctx, ws, held.Turn, log); parked || err != nil {
		return false, err
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
			revived, err = q.revive(ctx, ws, revivalLoad, log)
		})
		if err != nil {
			return false, err
		}
		if !revived {
			log.Warn(opDeliver, "the workspace has no session to deliver the hold to", nil)
			return false, q.undelivered(ctx, ws, held.Turn, wsm.HoldReconnect, ErrNoSession, log)
		}
		// THE REVIVAL MAY HAVE PARKED AT THE GATE, which leaves no shim.
		if parked, err := q.holdAtStandingGate(ctx, ws, held.Turn, log); parked || err != nil {
			return false, err
		}
		if sender, ok = q.deps.Client(ws); !ok {
			log.Error(opDeliver, "the revived workspace still has no session to deliver the hold to", nil)
			return false, q.undelivered(ctx, ws, held.Turn, wsm.HoldReconnect, ErrNoSession, log)
		}
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
		Origin:   promptorigin.Named(held.Origin),
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
