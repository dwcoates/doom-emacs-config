package promptqueue

import (
	"agentrepl/logging/logsite"
	"context"
	"fmt"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/promptorigin"
	"agent-repld/internal/wsm"
)

// A PROMPT DELIVERED ON ARRIVAL IS NEVER HELD (owner ruling, 2026-11-21).
//
// A daemon notice to an agent and a prompt another workspace's agent sent
// (promptorigin.DeliveredOnArrival) go the moment they arrive:
//
//   - into an idle session, as its turn;
//   - into a running turn, as a join after the turn's current tool call
//     (join.go): nothing is interrupted, and the vendor folds it in or runs
//     it next.
//
// It never enters held_prompts, so nothing the held section, a release, a
// fold, an edit or a close's check reads can ever see it (the store refuses
// such a hold: wsm.ErrHeldOnArrival).
//
// WHERE IT GENUINELY CANNOT GO YET, IT WAITS UNDER A NAMED WAIT
// (wsm.ArrivalWait): its reason (wsm.ArrivalWaitReason) names the one event
// that ends it, and that event drives it again (driveArrivalsLocked), which
// delivers it or records the wait the decision then names. The wait is
// durable, so it outlives a daemon restart, and the successor's boot drives
// every one (DriveArrivalWaits).

// opArrival is the operation every record of the arrival path is filed under.
const opArrival = "daemon.promptqueue.arrival"

// deliveredOnArrival reports whether SUB takes the arrival path: a prompt of
// an origin delivered on arrival, to the session's turn (a bubble-addressed
// prompt goes to its agent whatever the turn, and a session act is not a
// prompt).
func deliveredOnArrival(sub Submission) bool {
	return promptorigin.DeliveredOnArrival(sub.Origin) && sub.Target == nil && sub.Act == nil
}

// arriveLocked delivers SUB, a prompt delivered on arrival, or records the
// named wait that keeps it. The caller holds the delivery lock (d).
func (q *queue) arriveLocked(ctx context.Context, d *delivery, sub Submission, log dlog.Logger) (Disposition, error) {
	lease, leased, err := q.deps.DB.Lease(ctx, sub.WS)
	if err != nil {
		log.Error(opArrival, "could not read the occupancy lease", dlog.Context{"cause": err.Error()})
		return Disposition{}, fmt.Errorf("read the lease for %q: %w", sub.WS, err)
	}
	if leased && !exemptFromLease(lease, sub.Origin) {
		switch lease.Policy {
		case wsm.PolicyRefuse, wsm.PolicyParked:
			return q.refuseMerging(ctx, lease, sub, log)
		case wsm.PolicyHold:
			kind, _, err := q.holdForLease(ctx, lease, log)
			if err != nil {
				return Disposition{}, err
			}
			reason, why := arrivalWaitOfLeaseHold(kind)
			return q.awaitArrival(ctx, sub, reason, why, log)
		default:
			log.Error(opArrival, "the lease carries a policy the queue does not know", dlog.Context{"lease": string(lease.ID), "holder": holderName(lease.Holder)})
			return Disposition{}, fmt.Errorf("lease policy %d on %q is unknown", lease.Policy, sub.WS)
		}
	}

	// A COLD GATE refuses what nobody is present to resend only when the
	// prompt first arrives, so its sender learns it did not go (an agent's
	// command prints the refusal). A prompt already waiting has no sender
	// listening, so it waits out the gate whatever its origin.
	if q.deps.ColdGate != nil {
		if detail, gated := q.deps.ColdGate(sub.WS); gated {
			if !sub.fromWait && !waitsOutColdGate(sub.Origin) {
				return Disposition{}, q.refuseAtColdGate(ctx, sub.WS, opArrival, "the prompt", log)
			}
			return q.awaitArrival(ctx, sub, wsm.WaitColdGate, "the session is parked at its cold gate: "+detail, log)
		}
	}
	if q.isDraining(sub.WS) {
		return q.awaitArrival(ctx, sub, wsm.WaitBounce, "the workspace is draining for a bounce", log)
	}
	// ONE SHIM CALL AT A TIME (call.go): the call's settle drives the wait
	// (redrive), and a call that opened a turn is then the turn it joins.
	if call, ok := q.standingCall(sub.WS); ok {
		if call.turn == sub.Turn {
			q.logRepeatedStart(log, "arrival", sub.Turn)
			return Disposition{Delivered: true}, nil
		}
		q.deferToCall(d, log, "arrival")
		return q.awaitArrival(ctx, sub, wsm.WaitShimCall, "a shim call ("+call.what+") is in flight on the workspace", log)
	}
	sender, ok := q.deps.Client(sub.WS)
	if !ok {
		// A HIBERNATED SESSION IS IDLE, NOT DEAD: the prompt revives it, as
		// any submission does, and the revived session's coming up delivers it.
		if q.deps.Revive == nil {
			log.Log(refusalLevel(ctx, dlog.LevelWarn), opArrival, logsite.Msg("the workspace has no session to deliver to"), nil)
			return Disposition{}, noSession(causeNoRevival)
		}
		disposition, err := q.awaitArrival(ctx, sub, wsm.WaitSession, "the workspace has no shim; its session is revived", log)
		if err != nil {
			return Disposition{}, err
		}
		q.reviveInBackground(context.WithoutCancel(ctx), sub.WS, revivalLoad, log)
		return disposition, nil
	}
	if kind, why, waiting := q.sessionWait(sub.WS); waiting {
		reason := wsm.WaitSession
		if kind == wsm.HoldColdGate {
			reason = wsm.WaitColdGate
		}
		return q.awaitArrival(ctx, sub, reason, why, log)
	}
	if block, blocked := q.vendorBlocked(sub.WS); blocked {
		return q.awaitArrival(ctx, sub, wsm.WaitVendor, "the vendor does not serve the session: "+block, log)
	}
	watcher, ok := q.deps.Watcher(sub.WS)
	if !ok {
		log.Log(refusalLevel(ctx, dlog.LevelWarn), opArrival, logsite.Msg("the workspace has no session watcher"), nil)
		return Disposition{}, noSession(causeNoWatcher)
	}
	running := watcher.TurnInFlight()
	if cut, ok := q.runningCut(sub.WS); ok {
		return q.awaitArrival(ctx, sub, wsm.WaitTurnEnd, "a context cut ("+cut.command.String()+") runs, which takes no join", log)
	}
	if running != nil {
		if *running == sub.Turn {
			q.logRepeatedStart(log, "arrival", sub.Turn)
			return Disposition{Delivered: true}, nil
		}
		// AN AGENT THAT ASKED FOR A TURN OF ITS OWN (the deferred delivery)
		// gets one: its prompt waits for the running turn's end, by name,
		// rather than joining it. The daemon's own notices ride the deferred
		// delivery by convention and join regardless.
		if sub.SentBy != nil && sub.Delivery == wsm.DeliveryDeferred {
			return q.awaitArrival(ctx, sub, wsm.WaitTurnEnd, "the sending agent asked for a turn of its own after the running one", log)
		}
		return q.joinArrivalLocked(ctx, d, sub, *running, sender, watcher, log)
	}
	disposition, err := q.deliver(ctx, d, sub, sender, watcher, log)
	if err == nil && disposition.Delivered {
		q.endArrivalWait(ctx, sub, "it started the session's turn", log)
	}
	return disposition, err
}

// arrivalWaitOfLeaseHold names the wait a prompt delivered on arrival takes
// under a holding lease, from the hold kind the lease projects
// (holdForLease), so the holder is read in one place for both paths.
func arrivalWaitOfLeaseHold(kind wsm.HoldKind) (wsm.ArrivalWaitReason, string) {
	switch kind {
	case wsm.HoldShutdown:
		return wsm.WaitShutdown, "the daemon is draining to exit"
	case wsm.HoldBuildRefresh:
		return wsm.WaitBounce, "the workspace's shim is being restarted"
	case wsm.HoldReconnect:
		return wsm.WaitSession, "the workspace is hibernating"
	case wsm.HoldMerge:
		return wsm.WaitMerge, "a merge drives the workspace's session"
	}
	panic(fmt.Sprintf("promptqueue: a lease projected hold kind %s, which names no arrival wait", kind))
}

// joinArrivalLocked sends SUB to join RUNNING after its current tool call. A
// running turn that takes no join keeps it waiting for that turn's end. The
// caller holds the delivery lock (d).
func (q *queue) joinArrivalLocked(ctx context.Context, d *delivery, sub Submission, running ids.TurnID, sender Sender, watcher Watcher, log dlog.Logger) (Disposition, error) {
	log = log.With(dlog.Context{"running_turn": string(running)})
	if !watcher.OnTurnJoining(sub.WS, sub.Turn) {
		return q.awaitArrival(ctx, sub, wsm.WaitTurnEnd, "the running turn is one the agent started on its own, which is never joined", log)
	}
	switch outcome, err := q.sendJoinLocked(ctx, d, sub, running, sender, watcher, nil, "arrived", log); outcome {
	case joinSendSent:
		q.endArrivalWait(ctx, sub, "it joined the running turn", log)
		return Disposition{Delivered: true}, nil
	case joinSendRecorded:
		q.closeRecorded(ctx, sub.WS, sub.Turn, err, opArrival,
			logsite.Msg("the vendor already recorded the prompt sent to join the running turn, so its delivery was lost from the daemon's records; nothing joins"), log)
		q.endArrivalWait(ctx, sub, "the vendor already recorded it", log)
		return Disposition{Delivered: true}, nil
	case joinSendAskOpen:
		return q.awaitArrival(ctx, sub, wsm.WaitTurnEnd, askOpenReason, log)
	case joinSendTurnUnrecorded:
		log.Error(opArrival, "could not record the turn before sending the prompt to join the running turn; it waits for it to end", dlog.Context{"cause": err.Error()})
		return q.awaitArrival(ctx, sub, wsm.WaitTurnEnd, "the prompt's turn could not be recorded", log)
	default:
		log.Error(opArrival, "the shim refused the prompt sent to join the running turn; it waits for it to end", dlog.Context{"cause": err.Error()})
		return q.awaitArrival(ctx, sub, wsm.WaitTurnEnd, "the session would not take the prompt into the running turn", log)
	}
}

// awaitArrival records SUB's named wait, REASON, in words WHY, and answers it.
// A prompt already waiting is recorded again under its new reason, keeping its
// place.
func (q *queue) awaitArrival(ctx context.Context, sub Submission, reason wsm.ArrivalWaitReason, why string, log dlog.Logger) (Disposition, error) {
	fields := dlog.Context{"turn": string(sub.Turn), "reason": reason.String(), "why": why, "already_waiting": sub.fromWait}
	if err := q.deps.DB.PutArrivalWait(ctx, wsm.ArrivalWait{
		Workspace: sub.WS, Turn: sub.Turn, Said: sub.Said, Origin: sub.Origin.String(), SentBy: sub.SentBy,
		QueuedAt: q.deps.Now(), Reason: reason, Why: why,
	}); err != nil {
		log.Error(opArrival, "could not record the wait of a prompt delivered on arrival", dlog.WithCause(fields, err))
		return Disposition{}, fmt.Errorf("record the arrival wait of %q on %q: %w", sub.Turn, sub.WS, err)
	}
	log.Info(opArrival, "the prompt delivered on arrival cannot go yet; it waits for the event its wait names", fields)
	return Disposition{Waiting: &reason}, nil
}

// endArrivalWait deletes SUB's wait once it was delivered, WHY saying how. A
// prompt that never waited has none.
func (q *queue) endArrivalWait(ctx context.Context, sub Submission, why string, log dlog.Logger) {
	fields := dlog.Context{"turn": string(sub.Turn), "how": why}
	if !sub.fromWait {
		log.Info(opArrival, "the prompt was delivered on arrival", fields)
		return
	}
	stood, err := q.deps.DB.DeleteArrivalWait(ctx, sub.Turn)
	switch {
	case err != nil:
		log.Error(opArrival, "the waiting prompt was delivered but its wait could not be ended; its next drive finds it already delivered", dlog.WithCause(fields, err))
	case !stood:
		log.Error(opArrival, "the waiting prompt was delivered with no wait standing", dlog.Merge(fields, dlog.Context{
			"invariant_violation": "a prompt driven from its wait is delivered once, under the delivery lock",
		}))
	default:
		log.Info(opArrival, "the waiting prompt was delivered and its wait ended", fields)
	}
}

// arrivalEdge is an event that ends some arrival waits.
type arrivalEdge int

const (
	// arrivalSessionUp: the session came up and the vendor serves it (the
	// session-up release, which the vendor serving again also takes). Every
	// wait is decided again: a session that serves is the edge each of them
	// ultimately waits for.
	arrivalSessionUp arrivalEdge = iota
	// arrivalTurnEnded: the running turn ended.
	arrivalTurnEnded
	// arrivalLeaseChanged: the workspace's occupancy lease changed.
	arrivalLeaseChanged
	// arrivalGateAnswered: the user answered the cold gate.
	arrivalGateAnswered
	// arrivalBounceEnded: a bounce's replacement took over the dispatch: a
	// session that serves, so every wait is decided again.
	arrivalBounceEnded
	// arrivalCallSettled: the standing shim call settled.
	arrivalCallSettled
	// arrivalBoot: the daemon booted; it drives every stored wait.
	arrivalBoot
)

func (e arrivalEdge) String() string {
	switch e {
	case arrivalSessionUp:
		return "session_up"
	case arrivalTurnEnded:
		return "turn_ended"
	case arrivalLeaseChanged:
		return "lease_changed"
	case arrivalGateAnswered:
		return "gate_answered"
	case arrivalBounceEnded:
		return "bounce_ended"
	case arrivalCallSettled:
		return "call_settled"
	case arrivalBoot:
		return "boot"
	}
	panic(fmt.Sprintf("promptqueue: undeclared arrival edge %d", int(e)))
}

// ends reports whether the edge is an event a wait of REASON waits on.
func (e arrivalEdge) ends(reason wsm.ArrivalWaitReason) bool {
	switch e {
	case arrivalSessionUp, arrivalBounceEnded, arrivalBoot:
		return true
	case arrivalTurnEnded:
		return reason == wsm.WaitTurnEnd
	case arrivalLeaseChanged:
		return reason == wsm.WaitMerge || reason == wsm.WaitBounce || reason == wsm.WaitSession || reason == wsm.WaitShutdown
	case arrivalGateAnswered:
		return reason == wsm.WaitColdGate
	case arrivalCallSettled:
		return reason == wsm.WaitShimCall
	}
	panic(fmt.Sprintf("promptqueue: undeclared arrival edge %d", int(e)))
}

// driveArrivalsLocked delivers, oldest first, every arrival wait of the
// workspace that EDGE ends, or records the wait each decision then names. The
// caller holds the delivery lock (d).
//
// UNTIL THE SESSION'S FACTS ARE IN, NOTHING IS DRIVEN: an adopted shim's
// running turn reads as none until then, and a wait driven on that nil would
// start a turn beside it (factsPending). The facts coming in drives them.
func (q *queue) driveArrivalsLocked(ctx context.Context, d *delivery, edge arrivalEdge, log dlog.Logger) {
	waits, err := q.deps.DB.ArrivalWaits(ctx, d.ws)
	if err != nil {
		log.Error(opArrival, "could not read the arrival waits the event ends; they wait for the next one", dlog.Context{"edge": edge.String(), "cause": err.Error()})
		return
	}
	var ended []wsm.ArrivalWait
	for _, w := range waits {
		if edge.ends(w.Reason) {
			ended = append(ended, w)
		}
	}
	if len(ended) == 0 {
		return
	}
	if q.factsPending(d.ws, "the arrival drive", log, func(ctx context.Context, d *delivery, log dlog.Logger) {
		q.driveArrivalsLocked(ctx, d, edge, log)
	}) {
		return
	}
	for _, w := range ended {
		fields := dlog.Context{"turn": string(w.Turn), "reason": w.Reason.String(), "edge": edge.String()}
		log.Info(opArrival, "the event a waiting prompt waits on came; it is delivered or waits again", fields)
		sub := Submission{
			// A DRIVEN WAIT HAS HAD ITS WAIT: whatever it waited on has
			// ended, so it is delivered now, joining whatever turn runs.
			WS: w.Workspace, Turn: w.Turn, Said: w.Said, Origin: promptorigin.Named(w.Origin), SentBy: w.SentBy,
			Delivery: wsm.DeliveryOrdinary, fromWait: true,
		}
		if _, err := q.arriveLocked(ctx, d, sub, log); err != nil {
			// THE WAIT STANDS: nothing about it was ended, so the next event
			// it waits on drives it again.
			log.Error(opArrival, "the waiting prompt could not be delivered; its wait stands for the next event", dlog.WithCause(fields, err))
		}
	}
}

// driveArrivals is driveArrivalsLocked under the workspace's delivery lock.
func (q *queue) driveArrivals(ctx context.Context, ws ids.WorkspaceID, edge arrivalEdge, log dlog.Logger) {
	d := q.lockDelivery(ws)
	defer d.unlock()
	q.driveArrivalsLocked(ctx, d, edge, log)
}

// DriveArrivalWaits implements Queue: the boot's drive of every stored wait,
// the event a wait on the daemon's exit waits on (wsm.WaitShutdown) and the
// one that revives a workspace that has no shim for its waiting prompts.
func (q *queue) DriveArrivalWaits(ctx context.Context) error {
	global := q.deps.Log.Global()
	all, err := q.deps.DB.AllArrivalWaits(ctx)
	if err != nil {
		global.Error(opArrival, "the boot could not read the arrival waits; none is driven", dlog.Context{"cause": err.Error()})
		return fmt.Errorf("drive the arrival waits: %w", err)
	}
	seen := map[ids.WorkspaceID]bool{}
	var order []ids.WorkspaceID
	for _, w := range all {
		if !seen[w.Workspace] {
			seen[w.Workspace] = true
			order = append(order, w.Workspace)
		}
	}
	global.Info(opArrival, "the boot drives every arrival wait", dlog.Context{"waits": len(all), "workspaces": len(order)})
	for _, ws := range order {
		log, err := q.logger(ctx, ws)
		if err != nil {
			continue
		}
		q.driveArrivals(ctx, ws, arrivalBoot, log)
	}
	return nil
}

// supersedeArrival ends TURN's wait when one stands, for a notice the next one
// supersedes: true when it was ended, which is the notice never having gone.
func (q *queue) supersedeArrival(ctx context.Context, turn ids.TurnID, log dlog.Logger) (bool, error) {
	stood, err := q.deps.DB.DeleteArrivalWait(ctx, turn)
	if err != nil {
		log.Error(opSupersede, "could not end the wait of a notice to supersede it; it stands", dlog.Context{"cause": err.Error()})
		return false, err
	}
	if stood {
		log.Info(opSupersede, "the waiting notice was superseded; the next one says it", nil)
	}
	return stood, nil
}
