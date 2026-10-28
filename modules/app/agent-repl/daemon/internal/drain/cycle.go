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

// cycleState is one pending cycle stand-down's bookkeeping, under c.mu.
type cycleState struct {
	// beat is when the keep-alive this stand-down follows was asked.
	beat time.Time
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
	c.mu.Lock()
	if c.cycles == nil {
		c.cycles = map[ids.WorkspaceID]*cycleState{}
	}
	st := c.cycles[ws]
	if st == nil {
		st = &cycleState{}
		c.cycles[ws] = st
	}
	st.beat = beat
	c.mu.Unlock()
	c.log.Debug(opCycle, "a keep-alive cycle's stand-down was asked for", dlog.Context{"workspace": string(ws), "beat": beat})
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
			beat := st.beat
			c.mu.Unlock()
			outcome := c.cycleDown(ctx, ws, beat)
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
func (c *controller) cycleDown(ctx context.Context, ws ids.WorkspaceID, beat time.Time) withheldOutcome {
	fields := dlog.Context{"workspace": string(ws), "beat": beat}
	if err := ctx.Err(); err != nil {
		c.log.Info(opCycle, "the daemon is leaving; a keep-alive cycle's stand-down is not taken", dlog.WithCause(fields, err))
		return withheldPending
	}
	record, err := c.deps.DB.Workspace(ctx, ws)
	if err != nil {
		if errors.Is(err, wsm.ErrNotFound) {
			c.log.Info(opCycle, "the workspace is no longer registered; nothing is stood down", fields)
			return withheldSettled
		}
		c.log.Error(opCycle, "could not read the workspace's record; its keep-alive cycle's stand-down is dropped", dlog.WithCause(fields, err))
		return withheldSettled
	}
	log := c.workspaceLog(record)
	if record.Current {
		log.Info(opCycle, "the user is looking at the workspace; its session is kept up between keep-alives", fields)
		return withheldSettled
	}
	if record.Closed {
		log.Debug(opCycle, "the workspace is closed; its cache's lapse stands it down, not the keep-alive cycle", fields)
		return withheldSettled
	}
	visible, err := bringup.Visible(ctx, c.deps.DB, ws)
	if err != nil {
		log.Error(opCycle, "could not read whether the workspace is visible; its keep-alive cycle's stand-down is dropped", dlog.WithCause(fields, err))
		return withheldSettled
	}
	if !visible {
		log.Debug(opCycle, "the workspace is withheld; its cache's lapse stands it down, not the keep-alive cycle", fields)
		return withheldSettled
	}
	session, found, err := c.deps.DB.Session(ctx, ws)
	if err != nil {
		log.Error(opCycle, "could not read the workspace's session; its keep-alive cycle's stand-down is dropped", dlog.WithCause(fields, err))
		return withheldSettled
	}
	if !found || session.Terminal != nil {
		log.Debug(opCycle, "the workspace has no live session; nothing to stand down", fields)
		return withheldSettled
	}
	if session.LastEngagementAt.After(beat) {
		log.Info(opCycle, "a real turn ran since the keep-alive; the session is kept up, and the next beat decides again",
			dlog.Merge(fields, dlog.Context{"engaged_at": session.LastEngagementAt}))
		return withheldSettled
	}
	switch gate := c.gate(ws); gate {
	case gateOpen:
	case gateNotServing:
		log.Debug(opCycle, "the workspace has no shim to address; nothing to stand down", fields)
		return withheldSettled
	case gateBusy, gateStarting, gateReviving:
		log.Info(opCycle, "the session is busy; its keep-alive cycle's stand-down waits for it to fall free",
			dlog.Merge(fields, dlog.Context{"gate": gate.String()}))
		return withheldPending
	default:
		log.Error(opCycle, "the stand-down gate answered a value this stand-down cannot read; it is dropped",
			dlog.Merge(fields, dlog.Context{"gate": gate.String(), "invariant_violation": "every gate value is handled"}))
		return withheldSettled
	}
	switch c.hibernate(ctx, log, ws, fields, cycleReason) {
	case hibernateDone:
		return withheldSettled
	case hibernateDeferred:
		log.Info(opCycle, "the keep-alive cycle's stand-down was deferred; it is taken when the session falls free", fields)
		return withheldPending
	default:
		// hibernate recorded the failure at its own level; the next beat asks
		// afresh.
		return withheldSettled
	}
}
