package drain

import (
	"context"
	"errors"
	"fmt"
	"time"

	shimv1 "agentrepl/proto/shim/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/shimlife"
	"agent-repld/internal/wsm"
)

// Sweep runs one pass of the idle sweep and reports what it parked.
//
// THE IDLE SWEEP IS ONE TRIGGER OF THE ONE PARK (owner ruling, 2026-11-09).
// It reaches the sessions no other stand-down does -- the workspace the user
// is looking at, one whose remote control is on -- and parks them as the
// keep-alive cycle parks the rest: the same gate, the same steps, the same
// `parked` terminal (wsm.TerminalParked), with its own detail naming it.
//
// A workspace is parked only when it is BOTH idle past the cutoff and FREE:
// a stand-down never interrupts, so a busy session is deferred to a later
// pass rather than stood down under a running turn.
func (c *controller) Sweep(ctx context.Context, now time.Time) ([]ids.WorkspaceID, error) {
	workspaces, err := c.deps.DB.ListWorkspaces(ctx)
	if err != nil {
		readFailed(c.log, opSweep, "could not list the workspaces to sweep",
			"the workspace list ended when the drain loop's context was cancelled", nil, err)
		return nil, fmt.Errorf("drain: sweep: %w", err)
	}
	queued, err := c.queuedSince(ctx)
	if err != nil {
		return nil, err
	}
	// EVERY SESSION IN ONE READ: a read per workspace queued each behind every
	// other reader on the store's one connection, every pass.
	sessions, err := wsm.SessionsByWorkspace(ctx, c.deps.DB)
	if err != nil {
		readFailed(c.log, opSweep, "could not read the workspaces' sessions",
			"the session read ended when the drain loop's context was cancelled", nil, err)
		return nil, fmt.Errorf("drain: sweep: %w", err)
	}
	var parked []ids.WorkspaceID
	for _, ws := range workspaces {
		fields := dlog.Context{"workspace": string(ws.ID), "cutoff": c.deps.IdleCutoff.String()}
		// EVERY RECORD BELOW IS WORKSPACE-BOUND and goes to that workspace's
		// own sink whenever the workspace can host one. A workspace whose
		// directory is a scratch path or has been deleted still gets swept:
		// resolution falls back to the central sink with the workspace named
		// on the record. Deferring the sweep on it instead cost an ERROR per
		// pass per workspace and left the session unparked forever.
		log := c.workspaceLog(ws)
		session, found := sessions[ws.ID]
		if !found {
			log.Debug(opSweep, "the workspace has no session to park", fields)
			continue
		}
		// A QUEUED MERGE KEEPS ITS SESSION WARM FOR AS LONG AS IT STANDS IN
		// LINE (owner rulings, 2026-10-13 and 2026-10-26): the merge's prompts
		// run on that session, and a long queue holds an entry for hours, so
		// no idle cutoff parks it while its entry stands.
		if enqueued, ok := queued[ws.ID]; ok {
			log.Debug(opSweep, "the workspace has a merge queued; its session is kept warm",
				dlog.Merge(fields, dlog.Context{"queued_at": enqueued.Format(time.RFC3339)}))
			continue
		}
		idle := now.Sub(session.LastEngagementAt)
		fields["idle"] = idle.String()
		// A SESSION ALREADY PARKED STAYS AS IT IS. There is one park, so a park
		// past the cutoff is no deeper than one inside it; the cache keeper owes
		// it no keep-alive past the cutoff, so its park carries none.
		if session.Terminal != nil {
			log.Debug(opSweep, "the workspace's session is already terminal",
				dlog.Merge(fields, dlog.Context{"terminal_kind": session.Terminal.Kind}))
			continue
		}
		if idle < c.deps.IdleCutoff {
			log.Debug(opSweep, "the session was engaged inside the cutoff", fields)
			continue
		}
		switch gate := c.gate(ws.ID); gate {
		case gateOpen:
		case gateBusy:
			log.Debug(opSweep, "the idle session is not free; deferring its park", fields)
			continue
		case gateNotServing:
			log.Debug(opSweep, "the workspace has no shim to address; skipping its park", fields)
			continue
		case gateStarting:
			log.Debug(opSweep, "a start of the workspace's session is under way; skipping its park", fields)
			continue
		case gateReviving:
			log.Debug(opSweep, "a prompt is reviving the session; deferring its park", fields)
			continue
		default:
			log.Error(opSweep, "the stand-down gate answered a value the sweep cannot read; skipping the park",
				dlog.Merge(fields, dlog.Context{"gate": gate.String(), "invariant_violation": "every gate value is handled"}))
			continue
		}
		if retry, backingOff := c.backingOff(ws.ID, now); backingOff {
			log.Debug(opSweep, "the workspace's last park failed; backing off before the next attempt",
				dlog.Merge(fields, dlog.Context{"failures": retry.failures, "next_attempt": retry.notBefore}))
			continue
		}
		switch c.park(ctx, log, ws.ID, fields, c.idleReason()) {
		case parkDone:
			c.clearRetry(ws.ID)
			parked = append(parked, ws.ID)
		case parkDeferred:
			c.clearRetry(ws.ID)
		case parkFailed:
			retry := c.recordFailure(ws.ID, now)
			log.Debug(opSweep, "backing off the failed park",
				dlog.Merge(fields, dlog.Context{"failures": retry.failures, "next_attempt": retry.notBefore}))
		}
	}
	c.log.Debug(opSweep, "one idle-sweep pass finished",
		dlog.Context{"scanned": len(workspaces), "parked": ids.Strings(parked)})
	return parked, nil
}

// standGate is whether a session may be stood down right now, and if not,
// why not. The idle sweep and a withheld workspace's stand-down ask the SAME
// question (gate), in the same order.
type standGate int

const (
	// gateOpen: nothing stands in the way.
	gateOpen standGate = iota
	// gateBusy: a turn is in flight or detached work is live. Teardown never
	// interrupts.
	gateBusy
	// gateNotServing: this daemon holds no shim it can address.
	gateNotServing
	// gateStarting: a start of the session is still under way.
	gateStarting
	// gateReviving: a prompt is reviving the session.
	gateReviving
)

// String names the gate for the records.
func (g standGate) String() string {
	switch g {
	case gateOpen:
		return "open"
	case gateBusy:
		return "busy"
	case gateNotServing:
		return "not_serving"
	case gateStarting:
		return "starting"
	case gateReviving:
		return "reviving"
	default:
		return "unknown"
	}
}

// gate answers whether ws's session may be stood down right now.
func (c *controller) gate(ws ids.WorkspaceID) standGate {
	if !c.deps.Freeness.Free(ws) {
		return gateBusy
	}
	// A DURABLE SESSION ROW IS NOT A SHIM. A stand-down is a directive to a
	// running shim, so a workspace whose session this daemon cannot
	// address has nothing to stand down: its bring-up is still in flight,
	// its close already is, or the row outlived the process that served
	// it. That is a STATE, and it is skipped here rather than sending
	// a directive whose only possible answer is a failure -- which is what
	// a registry row left behind by a removed worktree cost in the field:
	// the same ERROR every five minutes, forever, for a session no daemon
	// had ever held (realtest 7, 2026-09-12).
	if !c.deps.Stand.Serving(ws) {
		return gateNotServing
	}
	// A START STILL UNDER WAY HAS NOT RECORDED ITS ENGAGEMENT: the row
	// read before it carries the one from before it, so the idle measured is
	// the workspace's time closed, not the session's time idle.
	if c.deps.Stand.StartUnderWay(ws) {
		return gateStarting
	}
	// A SESSION A PROMPT IS REVIVING IS ENGAGED, whatever its record says.
	// The revival's bring-up serves the session and retires its terminal
	// before it records the session's facts, and delivers the prompt it
	// holds only after that, so across that window the record reads live
	// and idle since the engagement before the last park. Standing
	// it down there parked the shim the prompt was about to reach: the
	// hold then met `query_dead`, or no session at all, and was dropped.
	//
	// THE ORDER IS WHAT MAKES THIS TOTAL. Serving was read true above, so
	// any revival behind it had already raised its flag; a flag read false
	// here means that revival has delivered, and the turn it started is
	// the shim's own `turn_in_flight` refusal to the directive below.
	if c.deps.Reviving(ws) {
		return gateReviving
	}
	return gateOpen
}

// parkOutcome is what one park attempt came to.
type parkOutcome int

const (
	// parkDone: the session was stood down and recorded.
	parkDone parkOutcome = iota
	// parkDeferred: an ORDINARY deferral -- another lease holder, a
	// session that went away, a turn in flight. The next pass asks again at
	// the sweep's own cadence.
	parkDeferred
	// parkFailed: something that should have worked did not -- a failed
	// or unreadable directive, a warned refusal, a failed stand-down or
	// record. The workspace is BACKED OFF (see parkRetryDelay).
	parkFailed
)

// standDownReason is why a session is stood down: the records it is filed
// under, the detail its terminal carries, and the sentence its success says.
// Every trigger -- the idle sweep, a withheld workspace, the keep-alive cycle,
// a settings visit -- takes the SAME steps (park) and leaves the SAME
// park (wsm.TerminalParked); only these words differ, and the detail is where
// the trigger stays on the record as evidence. No view reads it.
type standDownReason struct {
	op     string
	detail string
	done   string
	// life is the reason the shim lifecycle series counts it under.
	life shimlife.StandDownReason
}

// idleReason is the idle sweep's.
func (c *controller) idleReason() standDownReason {
	return standDownReason{
		op:     opSweep,
		detail: fmt.Sprintf("idle past the %s cutoff; the transcript was left as it stood", c.deps.IdleCutoff),
		done:   "parked a session idle past the cutoff",
		life:   shimlife.StandDownHibernate,
	}
}

// park stands one session down: the Hibernate directive, then the ack,
// then a GRACEFUL KillSession, then the durable stand-down record. It reports
// what the attempt came to; every failure and every refusal DEFERS the
// workspace rather than failing the whole pass, because one wedged session
// must not stop the sweep reaching the rest.
func (c *controller) park(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID, fields dlog.Context, why standDownReason) parkOutcome {
	lease, err := c.deps.DB.AcquireLease(ctx, ws, wsm.HolderHibernate, wsm.PolicyHold)
	if err != nil {
		log.Debug(why.op, "another holder has the lease; deferring the park",
			dlog.WithCause(fields, err))
		return parkDeferred
	}
	defer func() {
		if err := c.deps.DB.ReleaseLease(context.WithoutCancel(ctx), lease.ID); err != nil {
			log.Warn(why.op, "could not release the stand-down lease",
				dlog.WithCause(dlog.Merge(fields, dlog.Context{"lease": string(lease.ID)}), err))
			return
		}
		// TELLING THE QUEUE IS WHAT DRAINS THE INTAKE. A prompt that arrived
		// inside the stand-down window is held against this lease, and the
		// release alone changes a row the queue is not watching — so without
		// this the prompt that should have REVIVED the session waits forever.
		if c.deps.LeaseChanged != nil {
			c.deps.LeaseChanged(ws)
		}
		// AND THE HOST VIEW IS STALE UNTIL SOMETHING RECOMPOSES IT. The
		// composer arm was `draining` for as long as this lease stood, the
		// last push every host client received was taken while it was held,
		// and no other flow republishes after a park. Emacs refuses a
		// submission while its gate reads draining, so the republish is what
		// makes the parked workspace revivable by a prompt.
		c.publishHost(ws)
	}()

	// A DELIVERY UNDER WAY IS ENGAGEMENT. The gate read the session free
	// before the lease was taken, and a submission may have decided in
	// between: it read no lease and stood its shim call. The lease is taken
	// now, and the queue reads it under the lock Delivering answers under, so
	// a delivery not seen here is held against the lease, and one seen here
	// defers the stand-down to the turn it starts.
	if c.deps.Delivering(ws) {
		log.Info(why.op, "a prompt is being delivered to the shim; deferring the park", fields)
		return parkDeferred
	}

	directive, cancelDirective := context.WithTimeout(ctx, c.deps.StandBound)
	answer, err := c.deps.Stand.Hibernate(directive, ws)
	cancelDirective()
	if err != nil {
		// THE RACE, NOT A FAULT. The pass selected a serving workspace, and
		// the session went away before the directive reached it. There is
		// nothing to stand down and nothing to fix, so it is recorded as the
		// state it is and deferred; the ERROR below still covers every
		// directive that failed against a shim that WAS there.
		if errors.Is(err, ErrNoLiveSession) {
			log.Debug(why.op, "the session went away before the hibernate directive; deferring the park",
				dlog.WithCause(fields, err))
			return parkDeferred
		}
		log.Error(why.op, "the hibernate directive failed; deferring the park",
			dlog.WithCause(fields, err))
		return parkFailed
	}
	if refusal := answer.GetError(); refusal != nil {
		// A REFUSAL IS AN ANSWER. turn_in_flight simply defers; the other
		// arm is worth a warning, and defers just the same.
		refused := dlog.Merge(fields, dlog.Context{"refusal": hibernateRefusal(refusal)})
		if refusal.GetTurnInFlight() != nil {
			log.Debug(why.op, "a turn is in flight; deferring the park", refused)
			return parkDeferred
		}
		log.Warn(why.op, "the shim refused to hibernate; deferring", refused)
		return parkFailed
	}
	// AND EVERY OTHER ARM IS A DEFERRAL, NOT A STAND-DOWN. The arms above are
	// the whole of what this daemon knows how to read; anything else is a shim
	// built against a contract this daemon has not been taught, and standing a
	// session down on an answer nobody here understood could kill a turn the
	// shim was refusing to let go of.
	if answer.GetSuccess() == nil {
		log.Error(why.op, "the shim answered the hibernate directive with an arm this daemon cannot read; deferring the park", fields)
		return parkFailed
	}

	standDown, cancelStandDown := context.WithTimeout(shimlife.WithStandDown(ctx, why.life), c.deps.StandBound)
	err = c.deps.Stand.KillSession(standDown, ws, false)
	cancelStandDown()
	if err != nil {
		log.Error(why.op, "the graceful stand-down failed after the hibernate ack; deferring",
			dlog.WithCause(fields, err))
		return parkFailed
	}

	if err := c.deps.DB.SetSessionTerminal(ctx, ws, wsm.SessionTerminal{
		Kind:   wsm.TerminalParked,
		Detail: why.detail,
		At:     c.deps.Clock.Now(),
	}); err != nil {
		log.Error(why.op, "could not record the park", dlog.WithCause(fields, err))
		return parkFailed
	}
	// The shim is gone, so the pid the intent manifest would name is gone with
	// it. A pid left behind would make the next reconciliation read a dead
	// session as preserved.
	if err := c.deps.DB.SetShimPID(ctx, ws, nil); err != nil {
		log.Error(why.op, "could not clear the parked session's shim pid", dlog.WithCause(fields, err))
		return parkFailed
	}
	// AND SO IS THE SPAWN THE REGISTRY RECORDS. That pid exists so a successor
	// daemon waits for a shim that is still starting instead of spawning a
	// second one; a parked workspace's shim is gone, and a pid left behind
	// would make the next boot wait out its whole adoption bound for it.
	if err := c.deps.DB.SetSpawnedShimPID(ctx, ws, nil); err != nil {
		log.Error(why.op, "could not clear the parked workspace's recorded spawn pid", dlog.WithCause(fields, err))
		return parkFailed
	}
	// AND SO IS THE FOOTER'S, AND THE CONNECTIVITY INDICATOR'S. Both are
	// in-memory accumulations fed by events, so unlike the roster they cannot
	// read the terminal back -- they are told here, after the record exists,
	// so no surface can report the park before the record that justifies it.
	//
	// A failure is not possible to report: these setters publish rather than
	// answer. What they change is that the footer's `disconnected` step stops
	// calling the deliberate stand-down a dead link, which is what reopens the
	// webapp's composer for the prompt that revives the session.
	if c.deps.SetParked != nil {
		c.deps.SetParked(ws, true)
	}
	// THE ROSTER'S ARM FOR THIS ROW IS A FUNCTION OF THE RECORD ABOVE. The
	// roster resolver reads the session terminal to know a park from a fault,
	// and it publishes on the events it is handed -- the last of which, the
	// shim link going dead, arrived during the KillSession above, before this
	// terminal existed. The row resolved on that event says `dead`, which
	// Emacs paints as "something on this machine broke" for a session the
	// daemon stood down on purpose. Nothing else republishes the roster after
	// a park, so this is the republish that makes the park READ as one.
	//
	// A failure here is recorded and nothing more: the park HAPPENED,
	// and reporting it as refused would leave the sweep retrying a session
	// that is already stood down. The stale view is the defect, and the
	// record is what names it.
	if c.deps.PublishRegistry != nil {
		if err := c.deps.PublishRegistry(ctx); err != nil {
			log.Error(why.op, "could not republish the roster after the park",
				dlog.WithCause(fields, err))
		}
	}
	log.Info(why.op, why.done, fields)
	return parkDone
}

// workspaceLog resolves one workspace's own durable sink, or the central sink
// with the workspace named on the record when the workspace cannot host one.
// The sweep runs over every registry row, so its logging is TOTAL: a
// workspace's unavailable directory can cost the record's placement, never the
// sweep of that workspace.
func (c *controller) workspaceLog(ws wsm.Workspace) dlog.Logger {
	return c.deps.Log.WorkspaceOrCentral(ws.Dir).With(dlog.Context{"workspace": string(ws.ID)})
}

// hibernateRefusal names the shim's refusal arm for a record. The ARM is the
// fact; the detail it carries is evidence.
func hibernateRefusal(err *shimv1.HibernateError) string {
	switch {
	case err.GetTurnInFlight() != nil:
		return "turn_in_flight"
	case err.GetNoSession() != nil:
		return "no_session"
	default:
		return "unspecified"
	}
}

// Run drives the idle sweep on its cadence and fires the standing schedule when
// its deadline passes.
//
// One wait per iteration, sized to whichever comes first — the next sweep or
// the standing deadline — so a schedule armed between two sweeps is honored at
// its own instant. Schedule and Cancel wake the loop so it re-sizes the wait
// at once.
func (c *controller) Run(ctx context.Context) error {
	c.log.Debug(opRun, "the drain loop is running",
		dlog.Context{"sweep_every": c.deps.SweepEvery.String()})
	for {
		schedule, err := c.deps.DB.DrainSchedule(ctx)
		if err != nil {
			readFailed(c.log, opRun, "could not read the standing drain schedule",
				"the schedule read ended when the drain loop's context was cancelled", nil, err)
			return fmt.Errorf("drain: run: %w", err)
		}
		now := c.deps.Clock.Now()
		if schedule != nil && !now.Before(schedule.Deadline) && c.claimFire() {
			return c.fire(ctx, *schedule)
		}
		wait := c.deps.SweepEvery
		if schedule != nil {
			if until := schedule.Deadline.Sub(now); until > 0 && until < wait {
				wait = until
			}
		}
		select {
		case <-ctx.Done():
			c.log.Debug(opRun, "the drain loop stopped with its context", nil)
			return ctx.Err()
		case <-c.rearm:
			c.log.Debug(opRun, "the standing schedule changed; re-sizing the wait", nil)
		case <-c.deps.Clock.After(wait):
			if _, err := c.Sweep(ctx, c.deps.Clock.Now()); err != nil {
				return err
			}
		}
	}
}

// claimFire reports whether this pass is the one that fires the schedule, so a
// deadline observed twice announces once.
func (c *controller) claimFire() bool {
	c.mu.Lock()
	defer c.mu.Unlock()
	if c.fired {
		return false
	}
	c.fired = true
	return true
}

// queuedSince answers when each workspace with a queued merge joined its
// queue.
func (c *controller) queuedSince(ctx context.Context) (map[ids.WorkspaceID]time.Time, error) {
	queues, err := c.deps.DB.AllMergeQueues(ctx)
	if err != nil {
		readFailed(c.log, opSweep, "could not read the merge queues to sweep",
			"the merge queues' read ended when the drain loop's context was cancelled", nil, err)
		return nil, fmt.Errorf("drain: sweep: read the merge queues: %w", err)
	}
	since := map[ids.WorkspaceID]time.Time{}
	for _, entries := range queues {
		for _, e := range entries {
			since[e.Workspace] = e.EnqueuedAt
		}
	}
	return since, nil
}
