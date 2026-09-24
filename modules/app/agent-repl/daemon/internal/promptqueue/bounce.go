package promptqueue

import (
	"context"
	"errors"
	"fmt"

	"claude-repld/internal/bounce"
	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/sessionwatcher"
	"claude-repld/internal/wsm"
)

// opBounce is the bounce registry's operation. Every decision the registry
// takes — registered, bounced now, forced, drained, finished — is recorded
// under it with the workspace and the reason.
const opBounce = "daemon.promptqueue.bounce"

// THE BOUNCE REGISTRY (owner design, 2026-09-23).
//
// A rollout that must replace what serves a workspace asks HERE, because this
// queue is what dispatches the workspace's turns: "the workspace is free" and
// "bounce it now" are decided under the SAME per-workspace delivery lock that
// every dispatch takes (wsState.drain), so a queued prompt cannot start a turn
// between the two. A decided bounce moves the workspace to DRAINING, where
// nothing is dispatched — a submission is held, a turn end pops nothing, a
// release is refused — until the bounce has finished and the queue delivers
// what it held to whatever now serves the workspace.
//
// A workspace with work in flight is REGISTERED instead, and the registry is
// checked on the daemon's own freeness edges: a turn's end (OnTurnEnded, which
// checks it BEFORE popping the next prompt), the last detached item's end
// (OnFree), and the shim's DEPARTURE (OnDeparted) -- a shim that dies with
// work recorded in flight produces neither of the first two, and its death is
// itself the end of all that work. Nothing polls and no goroutine waits per
// registered workspace.

// pendingBounce is one workspace's standing bounce.
type pendingBounce struct {
	req bounce.Request
	// dones are every requester's completion callbacks: a request that joins a
	// pending bounce is told how that bounce ended.
	dones []func(error)
	// draining reports that the bounce was decided and is running (or, with
	// KeepDraining, has run): nothing is dispatched to the workspace.
	draining bool
	// waitsOn is the watcher whose work the registered bounce waits on, so a
	// departure edge is matched to the shim it names: a late edge from a shim
	// already replaced must not be taken as the end of the new one's work.
	waitsOn Watcher
}

// ErrBounceMalformed refuses a request missing its reason or its action.
var ErrBounceMalformed = errors.New("promptqueue: a bounce request needs a reason and an action")

// RequestBounce implements Queue.
func (q *queue) RequestBounce(ctx context.Context, ws ids.WorkspaceID, req bounce.Request) (bounce.Decision, error) {
	if req.Reason == "" || req.Run == nil {
		q.deps.Log.Global().Error(opBounce, "refused a malformed bounce request", dlog.Context{
			"workspace": string(ws), "reason": req.Reason, "has_action": req.Run != nil,
		})
		return bounce.Decision{}, fmt.Errorf("bounce %q: %w", ws, ErrBounceMalformed)
	}
	log, err := q.logger(ctx, ws)
	if err != nil {
		return bounce.Decision{}, err
	}
	log = log.With(dlog.Context{"reason": req.Reason, "force": req.Force})

	state := q.state(ws)
	state.drain.Lock()
	defer state.drain.Unlock()

	turn, detached, free := q.inFlight(ws)
	decision := bounce.Decision{TurnInFlight: turn, DetachedWork: detached}
	current, _ := q.deps.Watcher(ws)

	q.mu.Lock()
	existing := state.bounce
	switch {
	case existing != nil && existing.draining:
		if req.Done != nil {
			existing.dones = append(existing.dones, req.Done)
		}
		q.mu.Unlock()
		decision.AlreadyPending, decision.Now = true, true
		log.Info(opBounce, "a bounce is already running for the workspace; the request joins it", nil)
		return decision, nil
	case existing != nil:
		// THE NEWEST REQUEST'S ACTION WINS and a force upgrades the pending
		// bounce: a later deploy's action is the one that knows the build to
		// bounce onto.
		force := existing.req.Force || req.Force
		existing.req = req
		existing.req.Force = force
		existing.waitsOn = current
		if req.Done != nil {
			existing.dones = append(existing.dones, req.Done)
		}
		decision.AlreadyPending = true
	default:
		existing = &pendingBounce{req: req, waitsOn: current}
		if req.Done != nil {
			existing.dones = []func(error){req.Done}
		}
		state.bounce = existing
	}
	force := existing.req.Force
	q.mu.Unlock()

	if !free && !force {
		log.Info(opBounce, "the workspace has work in flight; registered the bounce for when it ends", dlog.Context{
			"turn_in_flight": turn, "detached_work": detached, "already_registered": decision.AlreadyPending,
		})
		return decision, nil
	}
	decision.Now = true
	decision.Forced = !free
	if decision.Forced {
		// A FORCED BOUNCE IS THE CALLER'S ORDER, not a fault: it is recorded
		// at INFO, naming the work it ends.
		log.Info(opBounce, "a FORCED bounce: bouncing now over the work in flight, which ends with it", dlog.Context{
			"turn_in_flight": turn, "detached_work": detached,
		})
	} else {
		log.Info(opBounce, "the workspace is free; bouncing it now", nil)
	}
	q.startBounceLocked(ws, state, log)
	return decision, nil
}

// OnFree implements Queue: the watcher's freeness edge, told off its lock the
// moment the last turn or detached item ends. It is where a bounce registered
// on a busy workspace is taken.
func (q *queue) OnFree(ws ids.WorkspaceID) {
	ctx := context.Background()
	log, err := q.logger(ctx, ws)
	if err != nil {
		return
	}
	state := q.state(ws)
	state.drain.Lock()
	defer state.drain.Unlock()
	if !q.checkRegistryLocked(ws, state, log) {
		log.Debug(opBounce, "the workspace fell free with no bounce to take", nil)
	}
}

// OnDeparted implements Queue: the watcher's departure edge. It is told
// INLINE from the watcher's Close, which can run under this very workspace's
// delivery lock (a held prompt's revival retires the dead session it
// replaces), so the decision runs on a goroutine of its own and Drain joins
// it.
func (q *queue) OnDeparted(ws ids.WorkspaceID, departed Watcher, departure sessionwatcher.Departure) {
	// THE ADD HAPPENS UNDER mu, AGAINST DRAIN'S FLAG, so it can never race
	// the join: a WaitGroup Add from zero concurrent with its Wait is a misuse,
	// and a decision started behind the join would read a closed state client.
	q.mu.Lock()
	if q.exiting {
		q.mu.Unlock()
		q.deps.Log.Global().Debug(opBounce, "the daemon is exiting; a shim departure at the exit decides nothing", dlog.Context{
			"workspace": string(ws), "ordered": departure.Ordered, "cause": string(departure.Cause),
		})
		return
	}
	q.departing.Add(1)
	q.mu.Unlock()
	go func() {
		defer q.departing.Done()
		q.decideDeparture(ws, departed, departure)
	}()
}

// decideDeparture is the ONE decision a departure owes a registered bounce,
// taken under the workspace's delivery lock like every other bounce decision:
// whichever of it and a concurrent OnFree gets the lock first decides, and
// the other finds the workspace draining or the bounce gone.
func (q *queue) decideDeparture(ws ids.WorkspaceID, departed Watcher, departure sessionwatcher.Departure) {
	ctx := context.Background()
	fields := dlog.Context{"ordered": departure.Ordered, "cause": string(departure.Cause)}
	record, recordErr := q.deps.DB.Workspace(ctx, ws)
	log := q.deps.Log.Global().With(dlog.Context{"workspace": string(ws)})
	if recordErr == nil {
		log = q.deps.Log.WorkspaceOrCentral(record.Dir).With(dlog.Context{"workspace": string(ws)})
	}

	state := q.state(ws)
	state.drain.Lock()
	q.mu.Lock()
	pending := state.bounce
	q.mu.Unlock()
	switch {
	case pending == nil:
		state.drain.Unlock()
		log.Debug(opBounce, "the shim departed with no bounce registered", fields)
		return
	case pending.draining:
		state.drain.Unlock()
		log.Debug(opBounce, "the shim departed inside the bounce that is replacing it", fields)
		return
	case pending.waitsOn != departed:
		// NOT THE SHIM THE BOUNCE WAITS ON. The workspace is served by
		// another shim now, so its OWN work is what the bounce waits on, and
		// the ordinary judgement decides.
		log.Debug(opBounce, "the departed shim is not the one the registered bounce waits on; judging the workspace's current shim", fields)
		q.checkRegistryLocked(ws, state, log)
		state.drain.Unlock()
		return
	}
	fields["reason"] = pending.req.Reason

	// A NEWER SHIM MAY ALREADY SERVE THE WORKSPACE: a held prompt's revival
	// can bring one up between the death and this decision. The departed
	// shim's work is still over, but the workspace's work is now the newer
	// shim's, and a bounce taken here would stand down a shim that may be
	// mid-turn.
	current, served := q.deps.Watcher(ws)
	replaced := false
	if served && current != departed {
		_, currentDeparted := current.Departed()
		replaced = !currentDeparted
	}

	if pending.req.ReplacesShim {
		var unregister error
		switch {
		case replaced:
			// Every spawn runs the INSTALLED build, so the replacement the
			// bounce existed to make has already been made.
			fields["why"] = "a newer shim, spawned from the installed build, already serves the workspace"
			unregister = bounce.ErrUnregistered
		case departure.Ordered:
			fields["why"] = "this daemon ended the session itself; the next bring-up spawns the installed build"
			unregister = bounce.ErrUnregistered
		case errors.Is(recordErr, wsm.ErrNotFound):
			fields["why"] = "the workspace is no longer registered"
			unregister = bounce.ErrUnregistered
		case recordErr != nil:
			// NOTHING CAN BE DECIDED ABOUT A WORKSPACE THAT CANNOT BE READ, and
			// a bounce left registered here would wait on a shim that no longer
			// exists: it is failed, loudly, so its requester hears it.
			log.Error(opBounce, "the shim departed under a registered bounce and its workspace could not be read; the bounce is failed rather than left waiting",
				merged(fields, dlog.Context{"cause": recordErr.Error()}))
			unregister = fmt.Errorf("bounce %q: read the workspace after its shim departed: %w", ws, recordErr)
		case record.Closed:
			fields["why"] = "the workspace is closed"
			unregister = bounce.ErrUnregistered
		}
		if unregister != nil {
			dones := q.unregisterLocked(state)
			state.drain.Unlock()
			if errors.Is(unregister, bounce.ErrUnregistered) {
				log.Info(opBounce, "the shim departed under a registered bounce with nothing left to replace; unregistered it", fields)
			}
			for _, done := range dones {
				done(unregister)
			}
			return
		}
		log.Info(opBounce, "the shim died with work recorded in flight; that work ended with it, so its registered bounce relaunches it now", fields)
	} else if replaced {
		log.Debug(opBounce, "the shim departed and a newer one already serves the workspace; judging the newer shim's work", fields)
		q.checkRegistryLocked(ws, state, log)
		state.drain.Unlock()
		return
	} else {
		log.Info(opBounce, "the shim departed; the work its registered bounce waited on ended with it, so the bounce is taken now", fields)
	}
	q.startBounceLocked(ws, state, log)
	state.drain.Unlock()
}

// unregisterLocked drops the workspace's registered bounce unrun and answers
// its requesters' callbacks, which the caller tells once the delivery lock is
// released. The caller holds the delivery lock.
func (q *queue) unregisterLocked(state *wsState) []func(error) {
	q.mu.Lock()
	defer q.mu.Unlock()
	dones := state.bounce.dones
	state.bounce = nil
	return dones
}

// checkRegistryLocked takes a registered bounce the moment the workspace is
// free, and reports whether the workspace is DRAINING when it returns — just
// decided, or already. The caller holds the workspace's delivery lock, which
// is what makes "free" and "bounce" one decision.
func (q *queue) checkRegistryLocked(ws ids.WorkspaceID, state *wsState, log dlog.Logger) bool {
	q.mu.Lock()
	pending := state.bounce
	q.mu.Unlock()
	if pending == nil {
		return false
	}
	if pending.draining {
		return true
	}
	turn, detached, free := q.inFlight(ws)
	if !free {
		log.Debug(opBounce, "a registered bounce is still waiting on the workspace's work", dlog.Context{
			"reason": pending.req.Reason, "turn_in_flight": turn, "detached_work": detached,
		})
		return false
	}
	log.Info(opBounce, "the workspace's work ended; taking its registered bounce now", dlog.Context{
		"reason": pending.req.Reason,
	})
	q.startBounceLocked(ws, state, log)
	return true
}

// startBounceLocked moves the workspace to DRAINING and runs the bounce on its
// own goroutine, joinable through Drain. The caller holds the delivery lock.
func (q *queue) startBounceLocked(ws ids.WorkspaceID, state *wsState, log dlog.Logger) {
	q.mu.Lock()
	pending := state.bounce
	pending.draining = true
	req := pending.req
	q.mu.Unlock()
	log.Info(opBounce, "the workspace is draining: nothing is dispatched until the bounce has finished", dlog.Context{
		"reason": req.Reason, "state": "draining", "before": false, "after": true,
	})
	q.bouncing.Add(1)
	go func() {
		defer q.bouncing.Done()
		err := req.Run(q.lifetime(), ws)
		q.finishBounce(ws, req, err, log)
	}()
}

// finishBounce ends a bounce: the workspace leaves draining (unless the bounce
// keeps it drained), the acts and prompts it held are delivered to what now
// serves it, and every requester is told.
func (q *queue) finishBounce(ws ids.WorkspaceID, req bounce.Request, runErr error, log dlog.Logger) {
	ctx := context.Background()
	state := q.state(ws)
	state.drain.Lock()

	q.mu.Lock()
	pending := state.bounce
	var dones []func(error)
	if pending != nil {
		dones = pending.dones
		pending.dones = nil
	}
	keep := runErr == nil && req.KeepDraining
	if !keep {
		state.bounce = nil
	}
	q.mu.Unlock()

	switch {
	case runErr != nil:
		// THE WORKSPACE IS SERVED AS IT WAS. A bounce that failed replaced
		// nothing it could not put back, so dispatch resumes on whatever serves
		// the workspace, and the failure is the rollout's loud record too.
		log.Error(opBounce, "the bounce failed; dispatch resumes on what serves the workspace", dlog.Context{
			"reason": req.Reason, "cause": runErr.Error(), "state": "draining", "before": true, "after": false,
		})
	case keep:
		log.Info(opBounce, "the bounce finished and the workspace stays drained; its intake is its new owner's", dlog.Context{
			"reason": req.Reason,
		})
	default:
		log.Info(opBounce, "the bounce finished; dispatch resumes on the new shim", dlog.Context{
			"reason": req.Reason, "state": "draining", "before": true, "after": false,
		})
	}

	if !keep {
		q.resumeDispatchLocked(ctx, ws, log)
	}
	state.drain.Unlock()

	for _, done := range dones {
		done(runErr)
	}
}

// resumeDispatchLocked delivers what the drain held: the queued acts first, then
// the next prompt, exactly as a turn end would. The caller holds the delivery
// lock.
func (q *queue) resumeDispatchLocked(ctx context.Context, ws ids.WorkspaceID, log dlog.Logger) {
	if !q.releaseDrainHoldsLocked(ctx, ws, log) {
		return
	}
	if watcher, ok := q.deps.Watcher(ws); ok && watcher.TurnInFlight() != nil {
		log.Debug(opBounce, "a turn is already in flight on the new shim; the held prompts wait for its end", nil)
		return
	}
	q.drainActs(ctx, ws, log)
	delivered, err := q.popAndDeliver(ctx, ws, log)
	if err != nil {
		log.Error(opBounce, "a prompt held through the bounce was not delivered", dlog.Context{"cause": err.Error()})
		return
	}
	if delivered {
		log.Info(opBounce, "delivered a prompt held through the bounce to the new shim", nil)
	}
}

// releaseDrainHoldsLocked un-stamps the build-refresh holds the drain put on
// submissions that arrived while the bounce ran, so the resumed dispatch can
// deliver them. A holding lease that still stands keeps them: it is that
// lease's release that lets them go. It reports whether dispatch may resume.
func (q *queue) releaseDrainHoldsLocked(ctx context.Context, ws ids.WorkspaceID, log dlog.Logger) bool {
	lease, held, err := q.deps.DB.Lease(ctx, ws)
	if err != nil {
		log.Error(opBounce, "could not read the occupancy lease before resuming dispatch", dlog.Context{"cause": err.Error()})
		return false
	}
	if held && lease.Policy == wsm.PolicyHold {
		log.Debug(opBounce, "a holding lease still stands; its release delivers what the drain held",
			dlog.Context{"lease": string(lease.ID), "holder": holderName(lease.Holder)})
		return false
	}
	standing, err := q.deps.DB.HeldPrompts(ctx, ws)
	if err != nil {
		log.Error(opBounce, "could not read the holds the drain stamped", dlog.Context{"cause": err.Error()})
		return false
	}
	released := 0
	for _, h := range standing {
		if h.Tombstone != nil || h.Hold == nil || *h.Hold != wsm.HoldBuildRefresh {
			continue
		}
		if err := q.deps.DB.UpdateHeldPromptHold(ctx, h.Turn, nil, ""); err != nil {
			log.Error(opBounce, "could not release a hold the drain stamped",
				dlog.Context{"turn": string(h.Turn), "cause": err.Error()})
			continue
		}
		released++
	}
	if released > 0 {
		log.Info(opBounce, "released the prompts held through the bounce", dlog.Context{"released": released})
		if err := q.pushTray(ctx, ws, log); err != nil {
			return false
		}
	}
	return true
}

// isDraining reports whether a workspace's dispatch is suspended by a bounce.
func (q *queue) isDraining(ws ids.WorkspaceID) bool {
	q.mu.Lock()
	defer q.mu.Unlock()
	state, ok := q.states[ws]
	return ok && state.bounce != nil && state.bounce.draining
}

// inFlight answers what a bounce would have to wait on: the turn in flight
// and the live detached items. free is the conjunction. A prompt the shim is
// holding behind its own keep-alive is a StartTurn still in flight, and the
// bounce is decided under the same delivery lock that call is made under, so
// it can never be judged free past one.
//
// A DEPARTED WATCHER HAS NOTHING IN FLIGHT. The fleet keeps a dead shim's
// session until the next bring-up retires it, and the turn and live work its
// watcher last recorded ended with the shim: read as in flight, they would
// hold a bounce until a revival nobody may ever ask for.
func (q *queue) inFlight(ws ids.WorkspaceID) (turn bool, detached int, free bool) {
	if watcher, ok := q.deps.Watcher(ws); ok {
		if _, departed := watcher.Departed(); departed {
			return false, 0, true
		}
		if watcher.TurnInFlight() != nil {
			turn = true
		}
		live := watcher.LiveWork()
		detached = len(live.Agents) + len(live.Shells) + len(live.Monitors)
	}
	return turn, detached, !turn && detached == 0
}

// lifetime is the context a bounce runs on: the daemon's serving lifetime when
// one is wired, else a context bounded by the process alone.
func (q *queue) lifetime() context.Context {
	if q.deps.Lifetime != nil {
		return q.deps.Lifetime
	}
	return context.Background()
}
