package promptqueue

import (
	"context"
	"fmt"
	"slices"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/wsm"
)

// NO QUEUE LOCK IS EVER HELD ACROSS A CALL TO THE SHIM.
//
// The delivery lock (wsState.drain) serializes the decisions that deliver: a
// submission, a turn end, a lease change, a release, a bounce. Those decisions
// used to make their shim call -- StartTurn above all -- with the lock held, so
// the lock stood for as long as the shim took to answer. A shim that could not
// reach its store answered only when the store came back, and the lock stood
// for the whole outage: every other decision on the workspace waited behind
// it, and the stall watchdog recorded the wedge (TestDegradedDuringRealStoreOutage,
// 2026-10-02).
//
// So a decision now CLAIMS the call under the lock, RELEASES the lock, makes
// the call, RE-TAKES the lock and SETTLES the outcome (delivery.outside). The
// claim (wsState.call) is what keeps the queue's guarantees while the lock is
// free:
//
//   - ONE CALL AT A TIME per workspace. Every decision that would deliver, or
//     would start a bounce, finds the claim standing and DEFERS: it records
//     that the call's settle owes a re-decision (wsState.redrive) and returns.
//     A submission is parked as a hold rather than delivered beside the call.
//     Deliveries therefore leave in the order they were decided, as before.
//   - EXACTLY ONCE. The holds a call claims (shimCall.holds) are refused to
//     every step that would act on them while it stands -- a release, a drop,
//     an edit, a fold, a coalescence -- and no pop runs while it stands, so no
//     claimed hold can be delivered twice or retired under the call.
//   - NOTHING DEFERRED IS LOST. The delivery lock is released only through
//     delivery.unlock, which first runs every re-decision a call's settle
//     owes (redrive), so whichever entry point made the call is the one that
//     re-decides once it has settled its own outcome.

// shimCall is a call to the shim a decision made with the delivery lock
// released.
type shimCall struct {
	// what names the call for the record: "start_turn", "context_cut",
	// "session_act", "agent_prompt", "join", "revival", "rollback".
	what string
	// turn is the turn the call delivers, empty when it delivers none.
	turn ids.TurnID
	// opensTurn reports that the call opens turn as the session's turn: while
	// it stands, that turn is the one a new prompt waits behind.
	opensTurn bool
	// holds are the standing holds the call delivers or retires; nothing else
	// may act on them while it stands.
	holds []ids.TurnID
}

// delivery is a workspace's delivery lock, HELD. It is taken only by
// lockDelivery and released only by unlock, so every release runs the
// re-decisions the shim calls made under it deferred. A function that needs
// the lock held takes a *delivery, which is the proof that it is.
type delivery struct {
	q     *queue
	ws    ids.WorkspaceID
	state *wsState
}

// lockDelivery takes ws's delivery lock.
func (q *queue) lockDelivery(ws ids.WorkspaceID) *delivery {
	state := q.state(ws)
	state.drain.Lock()
	return &delivery{q: q, ws: ws, state: state}
}

// unlock runs every re-decision a settled call owes, then releases the lock.
// A re-decision can make a call of its own, which can owe another; each is
// run until none is owed.
func (d *delivery) unlock() {
	for d.q.takeRedrive(d.state) {
		d.q.redrive(d)
	}
	d.state.drain.Unlock()
}

// outside makes CALL with the delivery lock released. The caller holds the
// lock (d), has taken every decision the call rests on, and settles the
// outcome once outside returns, with the lock held again.
//
// A SECOND CALL WHILE ONE STANDS IS A DEFECT: every decision that could make
// one defers while a call stands, so reaching here with one standing means a
// caller skipped that check. It fails hard rather than letting two calls race
// for the same holds.
func (d *delivery) outside(call shimCall, log dlog.Logger, fn func()) {
	q := d.q
	q.mu.Lock()
	if standing := d.state.call; standing != nil {
		q.mu.Unlock()
		log.Error(opCall, "a second shim call was made while one stands", dlog.Context{
			"call": call.what, "turn": string(call.turn), "standing_call": standing.what, "standing_turn": string(standing.turn),
			"invariant_violation": "one shim call at a time per workspace",
			"remediation":         "defer the decision to the standing call (deferToCall) before calling the shim",
		})
		panic(fmt.Sprintf("promptqueue: a %s call on %s while a %s call stands", call.what, d.ws, standing.what))
	}
	d.state.call = &call
	q.mu.Unlock()
	log.Debug(opCall, "the delivery lock is released for the shim call", dlog.Context{
		"call": call.what, "turn": string(call.turn), "holds": len(call.holds),
	})
	d.state.drain.Unlock()
	defer func() {
		d.state.drain.Lock()
		q.mu.Lock()
		d.state.call = nil
		q.mu.Unlock()
		log.Debug(opCall, "the shim call returned; its outcome is settled under the delivery lock", dlog.Context{
			"call": call.what, "turn": string(call.turn),
		})
	}()
	fn()
}

// standingCall answers the shim call standing on ws, if one does.
func (q *queue) standingCall(ws ids.WorkspaceID) (shimCall, bool) {
	q.mu.Lock()
	defer q.mu.Unlock()
	state, ok := q.states[ws]
	if !ok || state.call == nil {
		return shimCall{}, false
	}
	return *state.call, true
}

// claimedByCall reports whether the shim call standing on ws claims the hold
// TURN.
func (q *queue) claimedByCall(ws ids.WorkspaceID, turn ids.TurnID) bool {
	call, ok := q.standingCall(ws)
	return ok && slices.Contains(call.holds, turn)
}

// deferToCall reports whether a shim call stands on the workspace and, when
// one does, records that its settle owes a re-decision of WHAT. The caller
// holds the delivery lock and returns without deciding.
func (q *queue) deferToCall(d *delivery, log dlog.Logger, what string) bool {
	q.mu.Lock()
	call := d.state.call
	if call != nil {
		d.state.redrive = true
	}
	q.mu.Unlock()
	if call == nil {
		return false
	}
	log.Info(opCall, "a shim call is in flight on the workspace; the decision is taken again once it settles", dlog.Context{
		"deferred": what, "call": call.what, "call_turn": string(call.turn),
	})
	return true
}

// takeRedrive answers and clears whether a re-decision is owed and may run
// now. WHILE A CALL STANDS NONE MAY: the re-decision is owed to the call's
// settle, and an entry point that ran beside the call (and deferred to it)
// releases the lock with the call still standing. The entry point that made
// the call runs it, at its own unlock, once the call has settled.
func (q *queue) takeRedrive(state *wsState) bool {
	q.mu.Lock()
	defer q.mu.Unlock()
	if state.call != nil {
		return false
	}
	owed := state.redrive
	state.redrive = false
	return owed
}

// takeParked answers and clears the submissions parked behind a call.
func (q *queue) takeParked(state *wsState) []Submission {
	q.mu.Lock()
	defer q.mu.Unlock()
	parked := state.parked
	state.parked = nil
	return parked
}

// holdBehindCall parks a submission that arrived while CALL stands. A call
// opening a turn is a running turn to the prompt, which is held and judged
// behind it as it would be behind any; any other call parks the prompt as a
// plain hold, which the call's settle delivers or judges (redrive). The
// caller holds the delivery lock.
func (q *queue) holdBehindCall(ctx context.Context, d *delivery, sub Submission, call shimCall, log dlog.Logger) (Disposition, error) {
	if call.turn == sub.Turn {
		q.logRepeatedStart(log, "submit", sub.Turn)
		return Disposition{Delivered: true}, nil
	}
	q.deferToCall(d, log, "submission")
	running := ids.TurnID("")
	if call.opensTurn && sub.Target == nil {
		running = call.turn
	}
	disposition, err := q.hold(ctx, sub, running, nil, log)
	if err != nil {
		return Disposition{}, err
	}
	if running == "" {
		q.mu.Lock()
		d.state.parked = append(d.state.parked, sub)
		q.mu.Unlock()
	}
	return disposition, nil
}

// redrive re-takes the decisions deferred while a shim call stood, in the
// order a turn end takes them: a registered bounce first, then the prompts
// parked behind the call, then the next held prompt. The caller holds the
// delivery lock.
func (q *queue) redrive(d *delivery) {
	ctx := context.Background()
	parked := q.takeParked(d.state)
	log, err := q.logger(ctx, d.ws)
	if err != nil {
		// q.logger recorded the failure. The parked prompts are durable holds
		// and stay held; the next turn end or lease change delivers them.
		return
	}
	log.Debug(opCall, "re-deciding what was deferred while a shim call stood", dlog.Context{"parked": len(parked)})
	if q.decideBounceLocked(d, log) {
		log.Info(opCall, "a bounce deferred behind the shim call was taken; what is held waits for it", nil)
		return
	}
	// A PROMPT ADDRESSED TO AN AGENT IS NOT THE SESSION'S TURN: it goes now
	// whatever the session is running, as it would have gone at submission.
	for _, sub := range parked {
		if sub.Target == nil {
			continue
		}
		held, err := q.standingHold(ctx, d.ws, sub.Turn)
		if err != nil {
			log.Info(opCall, "a prompt parked behind the shim call no longer stands; nothing to deliver", dlog.Context{
				"turn": string(sub.Turn), "cause": err.Error(),
			})
			continue
		}
		if err := q.deliverHeld(ctx, d, held, log); err != nil {
			log.Error(opCall, "a prompt parked behind the shim call was not delivered to its agent; it stays held", dlog.Context{
				"turn": string(sub.Turn), "cause": err.Error(),
			})
		}
	}
	if running, ok := q.runningTurn(d.ws); ok {
		// A TURN RUNS NOW (the call opened it, or the vendor did): the parked
		// prompts are held behind it, so they are judged against it exactly as
		// a prompt submitted behind it is.
		for _, sub := range parked {
			if sub.Target != nil {
				continue
			}
			held, err := q.standingHold(ctx, d.ws, sub.Turn)
			if err != nil || held.Classification != nil {
				continue
			}
			q.classifyHeld(ctx, sub, running, log)
		}
		log.Debug(opCall, "a turn runs; what is held waits for its end", dlog.Context{"turn_in_flight": string(running)})
		return
	}
	if _, err := q.popAndDeliver(ctx, d, log); err != nil {
		log.Error(opCall, "the hold deferred behind the shim call was not delivered", dlog.Context{"cause": err.Error()})
	}
}

// runningTurn answers the session's running turn: the watcher's turn in
// flight, else a context cut the queue recorded before the watcher learned of
// it.
func (q *queue) runningTurn(ws ids.WorkspaceID) (ids.TurnID, bool) {
	if watcher, ok := q.deps.Watcher(ws); ok {
		if running := watcher.TurnInFlight(); running != nil {
			return *running, true
		}
	}
	if cut, ok := q.runningCut(ws); ok {
		return cut.turn, true
	}
	return "", false
}

// errClaimedByCall refuses a step on a hold a shim call in flight is
// delivering or retiring. To the user the hold has left the queue: it is on
// its way to the shim, so the refusal is the one a delivered hold earns.
var errClaimedByCall = fmt.Errorf("the held prompt is being delivered to the shim: %w", ErrAlreadyDelivered)

// unclaimed refuses, with errClaimedByCall, a hold a call in flight claims.
func (q *queue) unclaimed(ws ids.WorkspaceID, held wsm.HeldPrompt, log dlog.Logger, op string) error {
	if !q.claimedByCall(ws, held.Turn) {
		return nil
	}
	log.Info(op, "the step is refused: the held prompt is being delivered to the shim", dlog.Context{"turn": string(held.Turn)})
	return errClaimedByCall
}
