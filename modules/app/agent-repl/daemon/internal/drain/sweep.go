package drain

import (
	"context"
	"fmt"
	"time"

	shimv1 "agentrepl/proto/shim/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/wsm"
)

// TerminalHibernated is the session terminal the sweep records. It is
// REHYDRATABLE — unlike "deleted", which refuses resurrection — because the
// next mount of the workspace revives the session from the compacted
// transcript the Hibernate directive left behind.
const TerminalHibernated = "hibernated"

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
		// own sink; a record about one workspace's session written globally is
		// the invariant violation the logging contract names.
		log, err := c.workspaceLog(ws)
		if err != nil {
			c.log.Error(opSweep, "could not resolve a workspace's log sink; deferring its sweep",
				withCause(fields, err))
			continue
		}
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
		if c.hibernate(ctx, log, ws.ID, fields) {
			hibernated = append(hibernated, ws.ID)
		}
	}
	c.log.Debug(opSweep, "one idle-sweep pass finished",
		dlog.Context{"scanned": len(workspaces), "hibernated": workspaceIDs(hibernated)})
	return hibernated, nil
}

// hibernate stands one session down: the Hibernate directive, then the ack,
// then a GRACEFUL KillSession, then the durable stand-down record. It reports
// whether the session was actually hibernated; every failure and every refusal
// DEFERS the workspace rather than failing the whole pass, because one wedged
// session must not stop the sweep reaching the rest.
func (c *controller) hibernate(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID, fields dlog.Context) bool {
	lease, err := c.deps.DB.AcquireLease(ctx, ws, wsm.HolderHibernate, wsm.PolicyHold)
	if err != nil {
		log.Debug(opSweep, "another holder has the lease; deferring the hibernation",
			withCause(fields, err))
		return false
	}
	defer func() {
		if err := c.deps.DB.ReleaseLease(ctx, lease.ID); err != nil {
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
		log.Error(opSweep, "the hibernate directive failed; deferring the hibernation",
			withCause(fields, err))
		return false
	}
	if refusal := answer.GetError(); refusal != nil {
		// A REFUSAL IS AN ANSWER. turn_in_flight simply defers; the other two
		// arms are worth a warning, and defer just the same.
		refused := merge(fields, dlog.Context{"refusal": hibernateRefusal(refusal)})
		if refusal.GetTurnInFlight() != nil {
			log.Debug(opSweep, "a turn is in flight; deferring the hibernation", refused)
		} else {
			log.Warn(opSweep, "the shim refused to hibernate; deferring", refused)
		}
		return false
	}

	standDown, cancelStandDown := context.WithTimeout(ctx, c.deps.StandBound)
	err = c.deps.Stand.KillSession(standDown, ws, false)
	cancelStandDown()
	if err != nil {
		log.Error(opSweep, "the graceful stand-down failed after the hibernate ack; deferring",
			withCause(fields, err))
		return false
	}

	if err := c.deps.DB.SetSessionTerminal(ctx, ws, wsm.SessionTerminal{
		Kind:   TerminalHibernated,
		Detail: fmt.Sprintf("idle past the %s cutoff; the transcript was compacted before the stand-down", c.deps.IdleCutoff),
		At:     c.deps.Clock.Now(),
	}); err != nil {
		log.Error(opSweep, "could not record the hibernation stand-down", withCause(fields, err))
		return false
	}
	// The shim is gone, so the pid the intent manifest would name is gone with
	// it. A pid left behind would make the next reconciliation read a dead
	// session as preserved.
	if err := c.deps.DB.SetShimPID(ctx, ws, nil); err != nil {
		log.Error(opSweep, "could not clear the hibernated session's shim pid", withCause(fields, err))
		return false
	}
	log.Info(opSweep, "hibernated an idle session", fields)
	return true
}

// workspaceLog resolves one workspace's own durable sink. A record about a
// workspace belongs there and nowhere else.
func (c *controller) workspaceLog(ws wsm.Workspace) (dlog.Logger, error) {
	log, err := c.deps.Log.Workspace(ws.Dir)
	if err != nil {
		return nil, err
	}
	return log.With(dlog.Context{"workspace": string(ws.ID)}), nil
}

// hibernateRefusal names the shim's refusal arm for a record. The ARM is the
// fact; the detail it carries is evidence.
func hibernateRefusal(err *shimv1.HibernateError) string {
	switch {
	case err.GetTurnInFlight() != nil:
		return "turn_in_flight"
	case err.GetCompactionFailed() != nil:
		return "compaction_failed"
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
			c.log.Error(opRun, "could not read the standing drain schedule", withCause(nil, err))
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
