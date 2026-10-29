package drain

import (
	"context"
	"errors"
	"time"

	"agent-repld/internal/bringup"
	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/shimlife"
	"agent-repld/internal/wsm"
)

// opCycle is the operation a keep-alive cycle's stand-down records under.
const opCycle = "daemon.drain.keepalive_cycle"

// AN IDLE SHIM IS NOT KEPT BETWEEN KEEP-ALIVES (owner ruling, 2026-10-26).
// The daemon drives every keep-alive (cachelapse.Keeper), and once a beat has
// gone out a session nobody is using is stood down until a minute before the
// next one, so the ~350MB its shim and vendor child hold is free while it
// would only have waited. It is stood down exactly as the idle sweep stands
// one down -- the same gate, the same hibernate directive and graceful
// KillSession -- with the keep-alive cycle's own terminal, which every view
// draws as the live idle session it stands in for (wsm.ParkKeepaliveCycle).
//
// THE SAME RULES AS EVERY STAND-DOWN, AND THREE OF ITS OWN. It never
// interrupts a turn or live detached work: a busy session stays PENDING and is
// asked again on the daemon's own edges (RetryWithheld), never by a poll. And
// it stands nothing down for the workspace the user is looking at, for a
// hidden or closed one (the cache keeper's lapse owns those), or once a real
// turn has run since the beat -- that turn is engagement, and the next beat
// decides again.

// THE SAME STAND-DOWN ENDS A SETTINGS VISIT (owner request, 2026-10-29). A
// model, permission-mode or effort pick on a workspace with no shim starts
// one (promptqueue's revivalSettings); the switch away stands it down again
// through this path (VisitDown), under the same rules, unless a real turn has
// run since the pick started it. It leaves the keep-alive cycle's park, so the
// workspace is drawn as the idle session it was.

// opVisit is the operation a settings visit's stand-down records under.
const opVisit = "daemon.drain.settings_visit"

// visitReason is a settings visit's stand-down reason.
var visitReason = standDownReason{
	op:     opVisit,
	detail: "stood down on the switch away from a settings pick; the transcript was left as it stood",
	done:   "stood down the shim a settings pick started",
	park:   wsm.ParkKeepaliveCycle,
	life:   shimlife.StandDownSettingsVisit,
}

// cycleState is one pending cycle stand-down's bookkeeping, under c.mu.
type cycleState struct {
	// beat is the instant a real turn since which keeps the session up: the
	// keep-alive this stand-down follows, or the settings pick's start.
	beat time.Time
	// reason is why it stands down: the keep-alive cycle, or a settings visit.
	reason standDownReason
	// running is an attempt in flight on the detached lifetime.
	running bool
	// again is an edge that arrived while an attempt ran.
	again bool
}

// cycleReason is a keep-alive cycle's stand-down reason.
var cycleReason = standDownReason{
	op:     opCycle,
	detail: "stood down between keep-alives with its cache warm; the transcript was left as it stood",
	done:   "stood an idle session down until its next keep-alive",
	park:   wsm.ParkKeepaliveCycle,
	life:   shimlife.StandDownKeepaliveCycle,
}

// CycleDown asks for the stand-down of WS's session after the keep-alive asked
// at BEAT. It returns at once; the attempt runs on the detached lifetime. A
// later beat's ask replaces the earlier one's instant.
func (c *controller) CycleDown(ws ids.WorkspaceID, beat time.Time) {
	c.askDown(ws, beat, cycleReason)
}

// VisitDown asks for the stand-down of WS's session, a settings pick having
// started it at SINCE, the user having switched away. It returns at once.
func (c *controller) VisitDown(ws ids.WorkspaceID, since time.Time) {
	c.askDown(ws, since, visitReason)
}

// askDown asks for WS's stand-down for REASON unless a real turn has run
// since BEAT. A later ask replaces the earlier one's instant and reason.
func (c *controller) askDown(ws ids.WorkspaceID, beat time.Time, reason standDownReason) {
	c.mu.Lock()
	if c.cycles == nil {
		c.cycles = map[ids.WorkspaceID]*cycleState{}
	}
	st := c.cycles[ws]
	if st == nil {
		st = &cycleState{}
		c.cycles[ws] = st
	}
	st.beat, st.reason = beat, reason
	c.mu.Unlock()
	c.log.Debug(reason.op, "a stand-down was asked for", dlog.Context{"workspace": string(ws), "since": beat})
	c.kickCycle(ws, st)
}

// retryCycle is a daemon edge for ws: a cycle stand-down pending on it is
// asked again. A workspace with none pending is no change.
func (c *controller) retryCycle(ws ids.WorkspaceID) {
	c.mu.Lock()
	st := c.cycles[ws]
	c.mu.Unlock()
	if st == nil {
		return
	}
	c.log.Debug(opCycle, "an edge arrived for a pending keep-alive cycle stand-down; asking again", dlog.Context{"workspace": string(ws)})
	c.kickCycle(ws, st)
}

// kickCycle runs an attempt for ws unless one is running, in which case that
// one runs once more when it ends.
func (c *controller) kickCycle(ws ids.WorkspaceID, st *cycleState) {
	c.mu.Lock()
	if st.running {
		st.again = true
		c.mu.Unlock()
		return
	}
	st.running = true
	c.mu.Unlock()
	c.deps.Detach(func(ctx context.Context) {
		for {
			c.mu.Lock()
			beat, reason := st.beat, st.reason
			c.mu.Unlock()
			outcome := c.cycleDown(ctx, ws, beat, reason)
			c.mu.Lock()
			if st.again && ctx.Err() == nil {
				st.again = false
				c.mu.Unlock()
				continue
			}
			st.running, st.again = false, false
			if outcome == withheldSettled && c.cycles[ws] == st {
				delete(c.cycles, ws)
			}
			c.mu.Unlock()
			return
		}
	})
}

// cycleDown is one attempt.
func (c *controller) cycleDown(ctx context.Context, ws ids.WorkspaceID, beat time.Time, reason standDownReason) withheldOutcome {
	op := reason.op
	fields := dlog.Context{"workspace": string(ws), "since": beat}
	if err := ctx.Err(); err != nil {
		c.log.Info(op, "the daemon is leaving; the stand-down is not taken", dlog.WithCause(fields, err))
		return withheldPending
	}
	record, err := c.deps.DB.Workspace(ctx, ws)
	if err != nil {
		if errors.Is(err, wsm.ErrNotFound) {
			c.log.Info(op, "the workspace is no longer registered; nothing is stood down", fields)
			return withheldSettled
		}
		c.log.Error(op, "could not read the workspace's record; its stand-down is dropped", dlog.WithCause(fields, err))
		return withheldSettled
	}
	log := c.workspaceLog(record)
	if record.Current {
		log.Info(op, "the user is looking at the workspace; its session is kept up", fields)
		return withheldSettled
	}
	if record.Closed {
		log.Debug(op, "the workspace is closed; its cache's lapse stands it down", fields)
		return withheldSettled
	}
	visible, err := bringup.Visible(ctx, c.deps.DB, ws)
	if err != nil {
		log.Error(op, "could not read whether the workspace is visible; its stand-down is dropped", dlog.WithCause(fields, err))
		return withheldSettled
	}
	if !visible {
		log.Debug(op, "the workspace is withheld; its cache's lapse stands it down", fields)
		return withheldSettled
	}
	session, found, err := c.deps.DB.Session(ctx, ws)
	if err != nil {
		log.Error(op, "could not read the workspace's session; its stand-down is dropped", dlog.WithCause(fields, err))
		return withheldSettled
	}
	if !found || session.Terminal != nil {
		log.Debug(op, "the workspace has no live session; nothing to stand down", fields)
		return withheldSettled
	}
	if session.LastEngagementAt.After(beat) {
		log.Info(op, "a real turn ran since; the session is kept up",
			dlog.Merge(fields, dlog.Context{"engaged_at": session.LastEngagementAt}))
		return withheldSettled
	}
	switch gate := c.gate(ws); gate {
	case gateOpen:
	case gateNotServing:
		log.Debug(op, "the workspace has no shim to address; nothing to stand down", fields)
		return withheldSettled
	case gateBusy, gateStarting, gateReviving:
		log.Info(op, "the session is busy; its stand-down waits for it to fall free",
			dlog.Merge(fields, dlog.Context{"gate": gate.String()}))
		return withheldPending
	default:
		log.Error(op, "the stand-down gate answered a value this stand-down cannot read; it is dropped",
			dlog.Merge(fields, dlog.Context{"gate": gate.String(), "invariant_violation": "every gate value is handled"}))
		return withheldSettled
	}
	switch c.hibernate(ctx, log, ws, fields, reason) {
	case hibernateDone:
		return withheldSettled
	case hibernateDeferred:
		log.Info(op, "the stand-down was deferred; it is taken when the session falls free", fields)
		return withheldPending
	default:
		// hibernate recorded the failure at its own level; the next beat asks
		// afresh.
		return withheldSettled
	}
}
