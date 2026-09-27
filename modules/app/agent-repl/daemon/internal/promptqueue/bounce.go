package promptqueue

import (
	"context"
	"errors"
	"fmt"
	"io/fs"

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
//
// A BOUNCE IS UP TO TWO STAGES, AND A REQUEST OF ONE KIND NEVER REPLACES THE
// OTHER. A shim REPLACEMENT (a stale build, the restart verb, a log at its
// ceiling) swaps what serves the workspace here; a MOVE (a handover transfer,
// a layout restart's stand-down: `KeepDraining`) takes the workspace off this
// daemon. Two requests of the same kind coalesce and the newest wins, because
// a later deploy's action is the one that knows the build to bounce onto. Two
// requests of DIFFERENT kinds both run: the replacement first, then the move.
//
// Regression, 2026-09-27: a handover's transfer was registered behind a busy
// workspace and the restart verb then joined it; the newest action won, the
// restart ran on the outgoing daemon, the transfer's requester was told it had
// finished, and the daemon exited without ever pushing the transfer notice --
// the workspace's host stream died under Emacs with no end frame, and Emacs
// kept calling the dead daemon.
type pendingBounce struct {
	// replace is the shim-replacement stage, nil when none was asked. It runs
	// FIRST.
	replace *bounceStage
	// move is the stage that moves the workspace off this daemon, nil when
	// none was asked. It runs LAST, after any replacement, and a successful
	// `KeepDraining` move leaves the workspace drained.
	move *bounceStage
	// draining reports that the bounce was decided and is running (or, when
	// kept, has run): nothing is dispatched to the workspace.
	draining bool
	// kept reports that every stage has run and the last one's KeepDraining
	// holds the workspace drained for its new owner.
	kept bool
	// waitsOn is the watcher whose work the registered bounce waits on, so a
	// departure edge is matched to the shim it names: a late edge from a shim
	// already replaced must not be taken as the end of the new one's work.
	waitsOn Watcher
}

// bounceStage is one kind's request and every requester that joined it.
type bounceStage struct {
	req bounce.Request
	// dones are every requester's completion callbacks: a request that joins
	// a stage is told how that stage ended.
	dones []func(error)
	// started reports that the stage has been handed to the bounce's
	// goroutine; a request of its kind then joins it rather than replacing
	// its action.
	started bool
}

// isMove reports whether req moves the workspace off this daemon rather than
// replacing its shim.
func isMove(req bounce.Request) bool { return req.KeepDraining }

// slot answers the stage field req's kind lives in.
func (p *pendingBounce) slot(req bounce.Request) **bounceStage {
	if isMove(req) {
		return &p.move
	}
	return &p.replace
}

// stages answers the stages that stand, in the order they run.
func (p *pendingBounce) stages() []*bounceStage {
	var out []*bounceStage
	for _, s := range []*bounceStage{p.replace, p.move} {
		if s != nil {
			out = append(out, s)
		}
	}
	return out
}

// force reports whether any stage was asked to bounce over work in flight.
func (p *pendingBounce) force() bool {
	for _, s := range p.stages() {
		if s.req.Force {
			return true
		}
	}
	return false
}

// reason names every stage's reason, in run order, for the records.
func (p *pendingBounce) reason() string {
	var out string
	for _, s := range p.stages() {
		if out != "" {
			out += "+"
		}
		out += s.req.Reason
	}
	return out
}

// newStage answers a stage holding req and its requester.
func newStage(req bounce.Request) *bounceStage {
	stage := &bounceStage{req: req}
	if req.Done != nil {
		stage.dones = []func(error){req.Done}
	}
	return stage
}

// register adds a request to a bounce that has NOT started: a stage of its
// kind takes the newest action (a force upgrades it), a stage of the other
// kind is kept beside it. It reports whether a stage of the request's kind
// was already registered.
func (p *pendingBounce) register(req bounce.Request) bool {
	slot := p.slot(req)
	existing := *slot
	if existing == nil {
		*slot = newStage(req)
		return false
	}
	force := existing.req.Force || req.Force
	existing.req = req
	existing.req.Force = force
	if req.Done != nil {
		existing.dones = append(existing.dones, req.Done)
	}
	return true
}

// joinRunning adds a request to a bounce that is RUNNING. A MOVE the bounce
// has not got is queued behind the running stage, so a transfer asked while a
// replacement runs is still made; anything else joins a stage of its own kind
// (the newest action wins while that stage has not started), or the running
// stage when its kind has none. It reports whether a stage was queued.
func (p *pendingBounce) joinRunning(req bounce.Request) (queued bool) {
	slot := p.slot(req)
	switch {
	case *slot == nil && isMove(req):
		*slot = newStage(req)
		return true
	case *slot == nil:
		if running := p.running(); running != nil && req.Done != nil {
			running.dones = append(running.dones, req.Done)
		}
		return false
	case !(*slot).started:
		p.register(req)
		return false
	default:
		if req.Done != nil {
			(*slot).dones = append((*slot).dones, req.Done)
		}
		return false
	}
}

// running answers the stage the bounce's goroutine is running, nil when none.
func (p *pendingBounce) running() *bounceStage {
	var last *bounceStage
	for _, s := range p.stages() {
		if s.started {
			last = s
		}
	}
	return last
}

// next marks and answers the next stage to run, nil when every stage ran.
func (p *pendingBounce) next() *bounceStage {
	for _, s := range p.stages() {
		if !s.started {
			s.started = true
			return s
		}
	}
	return nil
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

	// A requester told at once is told AFTER the delivery lock is released
	// (registered before the unlock, so it runs after it), exactly as a
	// finished bounce's requesters are: its callback may call back in.
	var tellNow func()
	defer func() {
		if tellNow != nil {
			tellNow()
		}
	}()
	state := q.state(ws)
	state.drain.Lock()
	defer state.drain.Unlock()

	turn, detached, free := q.inFlight(ws)
	decision := bounce.Decision{TurnInFlight: turn, DetachedWork: detached}
	current, _ := q.deps.Watcher(ws)

	q.mu.Lock()
	existing := state.bounce
	switch {
	case existing != nil && existing.kept:
		// THE WORKSPACE IS NO LONGER THIS DAEMON'S: a move ran and keeps it
		// drained for its new owner, so nothing asked of it here can run.
		// Joining the finished bounce would leave the requester waiting on a
		// callback that was already made.
		q.mu.Unlock()
		decision.AlreadyPending = true
		log.Info(opBounce, "the workspace was moved off this daemon by the bounce that keeps it drained; the request is unregistered", nil)
		if req.Done != nil {
			tellNow = func() { req.Done(bounce.ErrUnregistered) }
		}
		return decision, nil
	case existing != nil && existing.draining:
		queued := existing.joinRunning(req)
		q.mu.Unlock()
		decision.AlreadyPending, decision.Now = true, true
		if queued {
			log.Info(opBounce, "a bounce is already running for the workspace; the request's move runs after it", nil)
		} else {
			log.Info(opBounce, "a bounce is already running for the workspace; the request joins it", nil)
		}
		return decision, nil
	case existing != nil:
		// THE NEWEST REQUEST OF A KIND WINS and a force upgrades the pending
		// bounce: a later deploy's action is the one that knows the build to
		// bounce onto. A request of the OTHER kind is kept beside it.
		existing.register(req)
		existing.waitsOn = current
		decision.AlreadyPending = true
	default:
		existing = &pendingBounce{waitsOn: current}
		existing.register(req)
		state.bounce = existing
	}
	force := existing.force()
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
		if !departure.Ordered {
			q.reviveAfterDeath(ws, state, record, recordErr, log)
		}
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
	fields["reason"] = pending.reason()

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

	if pending.replace != nil && pending.replace.req.ReplacesShim {
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
			dones, moveStands := q.unregisterReplacementLocked(state)
			if errors.Is(unregister, bounce.ErrUnregistered) {
				log.Info(opBounce, "the shim departed under a registered bounce with nothing left to replace; unregistered it", fields)
			}
			if moveStands {
				// THE MOVE STILL RUNS: the replacement was dropped, but the
				// workspace still has to leave this daemon, and the work it
				// waited on ended with the shim.
				log.Info(opBounce, "the shim departed under a registered bounce; its move is taken now without the replacement", fields)
				q.startBounceLocked(ws, state, log)
			}
			state.drain.Unlock()
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

// reviveAfterDeath brings back the session of a shim that DIED ON ITS OWN.
//
// AN OPEN WORKSPACE IS NEVER SESSION-LESS (owner ruling, 2026-09-13): the
// boot brings every open workspace's session up, and a live death is the same
// condition met while serving. Waiting for the user's next prompt left the
// workspace drawn dead on the roster and the footer until they typed. The
// revival is the one a prompt runs (reviveInBackground), so a prompt sent
// during it joins it and is delivered once the session is up, and the new
// shim resumes the same conversation.
//
// ONE UNATTENDED REVIVAL UNTIL A TURN ENDS. A shim that dies again before any
// turn has ended since the last one cannot hold a session; respawning it would
// loop. That second death is left down, loudly, and the next prompt revives it.
func (q *queue) reviveAfterDeath(ws ids.WorkspaceID, state *wsState, record wsm.Workspace, recordErr error, log dlog.Logger) {
	switch {
	case errors.Is(recordErr, wsm.ErrNotFound):
		log.Debug(opRevive, "the shim died with its workspace no longer registered; nothing is brought back", nil)
		return
	case recordErr != nil:
		log.Error(opRevive, "the shim died and its workspace could not be read; its session is not brought back", dlog.Context{"cause": recordErr.Error()})
		return
	case record.Closed:
		log.Debug(opRevive, "the shim of a closed workspace died; nothing is brought back", nil)
		return
	}
	// A DIRECTORY THAT IS GONE HAS NOTHING TO SERVE, which is also why the
	// boot closes such a workspace. A stat that does not say "not exist" is
	// never read as gone: the revival runs, and fails loudly if it must.
	if _, err := q.deps.Stat(record.Dir); errors.Is(err, fs.ErrNotExist) {
		log.Info(opRevive, "the shim died with its workspace directory gone; nothing is brought back", dlog.Context{"cause": err.Error()})
		return
	}
	q.mu.Lock()
	again := state.unattendedRevival
	state.unattendedRevival = true
	q.mu.Unlock()
	if again {
		log.Warn(opRevive, "the shim died again before any turn ended since the daemon last brought it back; it is left down until the next prompt", nil)
		return
	}
	log.Info(opRevive, "the shim died on its own; its session is brought back now", nil)
	q.reviveInBackground(q.lifetime(), ws, log)
}

// unregisterReplacementLocked drops the registered bounce's REPLACEMENT stage
// unrun and answers its requesters' callbacks, which the caller tells once
// the delivery lock is released. A move registered beside it stays, and
// moveStands says so; with none, the whole bounce is dropped. The caller
// holds the delivery lock.
func (q *queue) unregisterReplacementLocked(state *wsState) (dones []func(error), moveStands bool) {
	q.mu.Lock()
	defer q.mu.Unlock()
	dones = state.bounce.replace.dones
	state.bounce.replace = nil
	if state.bounce.move == nil {
		state.bounce = nil
		return dones, false
	}
	return dones, true
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
			"reason": pending.reason(), "turn_in_flight": turn, "detached_work": detached,
		})
		return false
	}
	log.Info(opBounce, "the workspace's work ended; taking its registered bounce now", dlog.Context{
		"reason": pending.reason(),
	})
	q.startBounceLocked(ws, state, log)
	return true
}

// startBounceLocked moves the workspace to DRAINING and runs the bounce's
// stages on its own goroutine, one after the other, joinable through Drain.
// The caller holds the delivery lock.
func (q *queue) startBounceLocked(ws ids.WorkspaceID, state *wsState, log dlog.Logger) {
	q.mu.Lock()
	pending := state.bounce
	pending.draining = true
	reason := pending.reason()
	first := pending.next()
	q.mu.Unlock()
	log.Info(opBounce, "the workspace is draining: nothing is dispatched until the bounce has finished", dlog.Context{
		"reason": reason, "state": "draining", "before": false, "after": true,
	})
	q.bouncing.Add(1)
	go func() {
		defer q.bouncing.Done()
		for stage := first; stage != nil; {
			err := stage.req.Run(q.lifetime(), ws)
			stage = q.finishStage(ws, stage, err, log)
		}
	}()
}

// finishStage ends one stage of a bounce and answers the next one to run, nil
// when the bounce is over. A stage with another behind it keeps the workspace
// draining; the last one ends the bounce: the workspace leaves draining
// (unless that stage keeps it drained), the acts and prompts it held are
// delivered to what now serves it, and every requester is told.
func (q *queue) finishStage(ws ids.WorkspaceID, stage *bounceStage, runErr error, log dlog.Logger) *bounceStage {
	ctx := context.Background()
	req := stage.req
	state := q.state(ws)
	state.drain.Lock()

	q.mu.Lock()
	dones := stage.dones
	stage.dones = nil
	pending := state.bounce
	var next *bounceStage
	if pending != nil {
		next = pending.next()
	}
	keep := runErr == nil && req.KeepDraining
	if next == nil {
		if keep && pending != nil {
			pending.kept = true
		} else {
			state.bounce = nil
		}
	}
	q.mu.Unlock()

	if next != nil {
		// THE WORKSPACE STAYS DRAINING for the stage behind this one, whether
		// this one finished or failed: a move asked of the workspace (a
		// handover's transfer) is owed whatever became of its replacement.
		if runErr != nil {
			log.Error(opBounce, "a stage of the bounce failed; the workspace stays draining for the stage after it", dlog.Context{
				"reason": req.Reason, "next": next.req.Reason, "cause": runErr.Error(),
			})
		} else {
			log.Info(opBounce, "a stage of the bounce finished; the workspace stays draining for the stage after it", dlog.Context{
				"reason": req.Reason, "next": next.req.Reason,
			})
		}
		state.drain.Unlock()
		for _, done := range dones {
			done(runErr)
		}
		return next
	}

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
	return nil
}

// EndKeptDrain implements Queue.
//
// A KEPT DRAIN IS THE HANDOVER'S: the workspace's intake became its new
// owner's the moment the transfer ran. When the new owner never took it and
// this daemon took the workspace back, the drain would otherwise stand for the
// rest of this process's life -- nothing dispatched, every prompt queued
// behind a bounce that finished long ago.
func (q *queue) EndKeptDrain(ws ids.WorkspaceID) {
	log := q.deps.Log.Global().With(dlog.Context{"workspace": string(ws)})
	state := q.state(ws)
	state.drain.Lock()
	defer state.drain.Unlock()
	q.mu.Lock()
	pending := state.bounce
	kept := pending != nil && pending.kept
	if kept {
		state.bounce = nil
	}
	q.mu.Unlock()
	if !kept {
		log.Debug(opBounce, "no kept drain stands on the workspace; nothing to end", nil)
		return
	}
	log.Info(opBounce, "ended the handover's kept drain; the workspace was taken back and dispatch resumes here", dlog.Context{
		"reason": pending.reason(), "state": "draining", "before": true, "after": false,
	})
	q.resumeDispatchLocked(context.Background(), ws, log)
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
