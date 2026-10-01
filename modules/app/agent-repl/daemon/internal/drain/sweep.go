package drain

import (
	"context"
	"errors"
	"fmt"
	"time"

	shimv1 "agentrepl/proto/shim/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/wsm"
)

// TerminalHibernated is the session terminal the sweep records. The value has
// ONE author (wsm), because the boot's bring-up and the open and select verbs
// all read it back to tell a deliberate sleep from a death.
const TerminalHibernated = wsm.TerminalHibernated

// Sweep runs one pass of the idle sweep and reports what it hibernated.
//
// A workspace is hibernated only when it is BOTH idle past the cutoff and
// FREE: hibernation never interrupts, so a busy session is deferred to a later
// pass rather than stood down under a running turn.
func (c *controller) Sweep(ctx context.Context, now time.Time) ([]ids.WorkspaceID, error) {
	workspaces, err := c.deps.DB.ListWorkspaces(ctx)
	if err != nil {
		c.log.Error(opSweep, "could not list the workspaces to sweep", withCause(nil, err))
		return nil, fmt.Errorf("drain: sweep: %w", err)
	}
	var hibernated []ids.WorkspaceID
	for _, ws := range workspaces {
		fields := dlog.Context{"workspace": string(ws.ID), "cutoff": c.deps.IdleCutoff.String()}
		// EVERY RECORD BELOW IS WORKSPACE-BOUND and goes to that workspace's
		// own sink whenever the workspace can host one. A workspace whose
		// directory is a scratch path or has been deleted still gets swept:
		// resolution falls back to the central sink with the workspace named
		// on the record. Deferring the sweep on it instead cost an ERROR per
		// pass per workspace and left the session unhibernated forever.
		log := c.workspaceLog(ws)
		session, found, err := c.deps.DB.Session(ctx, ws.ID)
		if err != nil {
			log.Error(opSweep, "could not read a workspace's session", withCause(fields, err))
			return nil, fmt.Errorf("drain: sweep %q: %w", ws.ID, err)
		}
		if !found {
			log.Debug(opSweep, "the workspace has no session to hibernate", fields)
			continue
		}
		if session.Terminal != nil {
			log.Debug(opSweep, "the workspace's session is already terminal",
				merge(fields, dlog.Context{"terminal_kind": session.Terminal.Kind}))
			continue
		}
		idle := now.Sub(session.LastEngagementAt)
		fields["idle"] = idle.String()
		if idle < c.deps.IdleCutoff {
			log.Debug(opSweep, "the session was engaged inside the cutoff", fields)
			continue
		}
		if !c.deps.Freeness.Free(ws.ID) {
			log.Debug(opSweep, "the idle session is not free; deferring its hibernation", fields)
			continue
		}
		// A DURABLE SESSION ROW IS NOT A SHIM. Hibernation is a directive to a
		// running shim, so a workspace whose session this daemon cannot
		// address has nothing to stand down: its bring-up is still in flight,
		// its close already is, or the row outlived the process that served
		// it. That is a STATE, and the sweep skips it here rather than sending
		// a directive whose only possible answer is a failure -- which is what
		// a registry row left behind by a removed worktree cost in the field:
		// the same ERROR every five minutes, forever, for a session no daemon
		// had ever held (realtest 7, 2026-09-12).
		if !c.deps.Stand.Serving(ws.ID) {
			log.Debug(opSweep, "the workspace has no shim to address; skipping its hibernation", fields)
			continue
		}
		// A SESSION A PROMPT IS REVIVING IS ENGAGED, whatever its record says.
		// The revival's bring-up serves the session and retires its terminal
		// before it records the session's facts, and delivers the prompt it
		// holds only after that, so across that window the record reads live
		// and idle since the engagement before the last hibernation. Standing
		// it down there hibernated the shim the prompt was about to reach: the
		// hold then met `query_dead`, or no session at all, and was dropped.
		//
		// THE ORDER IS WHAT MAKES THIS TOTAL. Serving was read true above, so
		// any revival behind it had already raised its flag; a flag read false
		// here means that revival has delivered, and the turn it started is
		// the shim's own `turn_in_flight` refusal to the directive below.
		if c.deps.Reviving(ws.ID) {
			log.Debug(opSweep, "a prompt is reviving the session; deferring its hibernation", fields)
			continue
		}
		if retry, backingOff := c.backingOff(ws.ID, now); backingOff {
			log.Debug(opSweep, "the workspace's last hibernation failed; backing off before the next attempt",
				merge(fields, dlog.Context{"failures": retry.failures, "next_attempt": retry.notBefore}))
			continue
		}
		switch c.hibernate(ctx, log, ws.ID, fields) {
		case hibernateDone:
			c.clearRetry(ws.ID)
			hibernated = append(hibernated, ws.ID)
		case hibernateDeferred:
			c.clearRetry(ws.ID)
		case hibernateFailed:
			retry := c.recordFailure(ws.ID, now)
			log.Debug(opSweep, "backing off the failed hibernation",
				merge(fields, dlog.Context{"failures": retry.failures, "next_attempt": retry.notBefore}))
		}
	}
	c.log.Debug(opSweep, "one idle-sweep pass finished",
		dlog.Context{"scanned": len(workspaces), "hibernated": workspaceIDs(hibernated)})
	return hibernated, nil
}

// hibernateOutcome is what one hibernation attempt came to.
type hibernateOutcome int

const (
	// hibernateDone: the session was stood down and recorded.
	hibernateDone hibernateOutcome = iota
	// hibernateDeferred: an ORDINARY deferral -- another lease holder, a
	// session that went away, a turn in flight. The next pass asks again at
	// the sweep's own cadence.
	hibernateDeferred
	// hibernateFailed: something that should have worked did not -- a failed
	// or unreadable directive, a warned refusal, a failed stand-down or
	// record. The workspace is BACKED OFF (see hibernateRetryDelay).
	hibernateFailed
)

// hibernate stands one session down: the Hibernate directive, then the ack,
// then a GRACEFUL KillSession, then the durable stand-down record. It reports
// what the attempt came to; every failure and every refusal DEFERS the
// workspace rather than failing the whole pass, because one wedged session
// must not stop the sweep reaching the rest.
func (c *controller) hibernate(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID, fields dlog.Context) hibernateOutcome {
	lease, err := c.deps.DB.AcquireLease(ctx, ws, wsm.HolderHibernate, wsm.PolicyHold)
	if err != nil {
		log.Debug(opSweep, "another holder has the lease; deferring the hibernation",
			withCause(fields, err))
		return hibernateDeferred
	}
	defer func() {
		if err := c.deps.DB.ReleaseLease(context.WithoutCancel(ctx), lease.ID); err != nil {
			log.Warn(opSweep, "could not release the hibernation lease",
				withCause(merge(fields, dlog.Context{"lease": string(lease.ID)}), err))
			return
		}
		// TELLING THE QUEUE IS WHAT DRAINS THE INTAKE. A prompt that arrived
		// inside the hibernation window is held against this lease, and the
		// release alone changes a row the queue is not watching — so without
		// this the prompt that should have REVIVED the session waits forever.
		if c.deps.LeaseChanged != nil {
			c.deps.LeaseChanged(ws)
		}
		// AND THE HOST VIEW IS STALE UNTIL SOMETHING RECOMPOSES IT. The
		// composer arm was `draining` for as long as this lease stood, the
		// last push every host client received was taken while it was held,
		// and no other flow republishes after a hibernation. Emacs refuses a
		// submission while its gate reads draining, so the republish is what
		// makes the hibernated workspace revivable by a prompt.
		c.publishHost(ws)
	}()

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
			log.Debug(opSweep, "the session went away before the hibernate directive; deferring the hibernation",
				withCause(fields, err))
			return hibernateDeferred
		}
		log.Error(opSweep, "the hibernate directive failed; deferring the hibernation",
			withCause(fields, err))
		return hibernateFailed
	}
	if refusal := answer.GetError(); refusal != nil {
		// A REFUSAL IS AN ANSWER. turn_in_flight simply defers; the other
		// arm is worth a warning, and defers just the same.
		refused := merge(fields, dlog.Context{"refusal": hibernateRefusal(refusal)})
		if refusal.GetTurnInFlight() != nil {
			log.Debug(opSweep, "a turn is in flight; deferring the hibernation", refused)
			return hibernateDeferred
		}
		log.Warn(opSweep, "the shim refused to hibernate; deferring", refused)
		return hibernateFailed
	}
	// AND EVERY OTHER ARM IS A DEFERRAL, NOT A STAND-DOWN. The arms above are
	// the whole of what this daemon knows how to read; anything else is a shim
	// built against a contract this daemon has not been taught, and standing a
	// session down on an answer nobody here understood could kill a turn the
	// shim was refusing to let go of.
	if answer.GetSuccess() == nil {
		log.Error(opSweep, "the shim answered the hibernate directive with an arm this daemon cannot read; deferring the hibernation", fields)
		return hibernateFailed
	}

	standDown, cancelStandDown := context.WithTimeout(ctx, c.deps.StandBound)
	err = c.deps.Stand.KillSession(standDown, ws, false)
	cancelStandDown()
	if err != nil {
		log.Error(opSweep, "the graceful stand-down failed after the hibernate ack; deferring",
			withCause(fields, err))
		return hibernateFailed
	}

	if err := c.deps.DB.SetSessionTerminal(ctx, ws, wsm.SessionTerminal{
		Kind:   TerminalHibernated,
		Detail: fmt.Sprintf("idle past the %s cutoff; the transcript was left as it stood", c.deps.IdleCutoff),
		At:     c.deps.Clock.Now(),
	}); err != nil {
		log.Error(opSweep, "could not record the hibernation stand-down", withCause(fields, err))
		return hibernateFailed
	}
	// The shim is gone, so the pid the intent manifest would name is gone with
	// it. A pid left behind would make the next reconciliation read a dead
	// session as preserved.
	if err := c.deps.DB.SetShimPID(ctx, ws, nil); err != nil {
		log.Error(opSweep, "could not clear the hibernated session's shim pid", withCause(fields, err))
		return hibernateFailed
	}
	// AND SO IS THE SPAWN THE REGISTRY RECORDS. That pid exists so a successor
	// daemon waits for a shim that is still starting instead of spawning a
	// second one; a hibernated workspace's shim is gone, and a pid left behind
	// would make the next boot wait out its whole adoption bound for it.
	if err := c.deps.DB.SetSpawnedShimPID(ctx, ws, nil); err != nil {
		log.Error(opSweep, "could not clear the hibernated workspace's recorded spawn pid", withCause(fields, err))
		return hibernateFailed
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
	// a hibernation, so this is the republish that makes the park READ as one.
	//
	// A failure here is recorded and nothing more: the hibernation HAPPENED,
	// and reporting it as refused would leave the sweep retrying a session
	// that is already stood down. The stale view is the defect, and the
	// record is what names it.
	if c.deps.PublishRegistry != nil {
		if err := c.deps.PublishRegistry(ctx); err != nil {
			log.Error(opSweep, "could not republish the roster after the hibernation",
				withCause(fields, err))
		}
	}
	log.Info(opSweep, "hibernated an idle session", fields)
	return hibernateDone
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
			// The serving lifetime ending under the read is this daemon's own
			// exit withdrawing the loop, not a schedule that could not be read:
			// there is no drain left to fire and nothing to remediate. A read
			// that fails while serving is still an error.
			if cancelled(err) {
				c.log.Debug(opRun, "the schedule read ended when the drain loop's context was cancelled", withCause(nil, err))
			} else {
				c.log.Error(opRun, "could not read the standing drain schedule", withCause(nil, err))
			}
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

// cancelled reports whether err is a context ending -- this daemon's own exit
// cancelling the serving context under work in flight, rather than a failure
// of the work itself.
func cancelled(err error) bool {
	return errors.Is(err, context.Canceled) || errors.Is(err, context.DeadlineExceeded)
}
