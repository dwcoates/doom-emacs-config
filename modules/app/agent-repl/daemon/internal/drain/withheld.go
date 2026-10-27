package drain

import (
	"context"
	"errors"

	"agent-repld/internal/bringup"
	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// opWithheld is the operation a withheld workspace's stand-down records under.
const opWithheld = "daemon.drain.withheld"

// A WORKSPACE THE USER WITHHELD RUNS NOTHING (owner principle 1,
// docs/protobuf-design/open-tabs.md). Collapsing a repository's section, and a
// hidden workspace's cache lapsing (cachelapse.Keeper), stands its session down
// exactly as the idle sweep does — the same gate, the same hibernate directive, graceful
// KillSession, hibernated terminal and cleared pids (hibernate) — with its own
// reason on the records and the terminal.
//
// A FOLD SPARES WHAT THE RECORD SPARES (owner ruling, 2026-10-25): a one-shot,
// or a workspace with a merge pending (wsm.Workspace.FoldSpare), is never
// stood down for a collapse, whichever attempt reads it.
//
// IT NEVER INTERRUPTS A TURN. A workspace the gate holds back (a turn in
// flight, live detached work, a start or a revival under way, the shim's own
// turn_in_flight refusal) stays PENDING, and is asked again on the daemon's
// own edges, never by a poll or a timer: the free edge a turn's end raises
// (the session watcher's OnFree) and a session coming up (SessionUp). Every
// attempt first re-reads the workspace's visibility, so one the user showed
// again in the meantime is dropped, not stood down.

// withheldState is one pending stand-down's bookkeeping, under c.mu.
type withheldState struct {
	// running is an attempt in flight on the detached lifetime.
	running bool
	// again is an edge that arrived while an attempt ran: one more attempt
	// follows it, so no edge is lost.
	again bool
}

// withheldOutcome is what one attempt came to.
type withheldOutcome int

const (
	// withheldSettled: stood down, or nothing left to stand down (shown
	// again, no session, a failure already recorded). Forgotten.
	withheldSettled withheldOutcome = iota
	// withheldPending: held back; asked again on the next edge.
	withheldPending
)

// StandDownWithheld asks for the stand-down of a workspace whose repository
// section the user collapsed, or a hidden or closed one whose cache has
// lapsed. It
// returns at once; the attempt runs on the detached lifetime.
func (c *controller) StandDownWithheld(ws ids.WorkspaceID) {
	c.mu.Lock()
	if c.withheld == nil {
		c.withheld = map[ids.WorkspaceID]*withheldState{}
	}
	st := c.withheld[ws]
	if st == nil {
		st = &withheldState{}
		c.withheld[ws] = st
	}
	c.mu.Unlock()
	c.log.Debug(opWithheld, "a withheld workspace's stand-down was asked for", dlog.Context{"workspace": string(ws)})
	c.kick(ws, st)
}

// RetryWithheld is a daemon edge for ws (the free edge a turn's end raises, or
// a session coming up): a stand-down pending on ws is asked again. A
// workspace with none pending is no change.
func (c *controller) RetryWithheld(ws ids.WorkspaceID) {
	c.retryCycle(ws)
	c.mu.Lock()
	st := c.withheld[ws]
	c.mu.Unlock()
	if st == nil {
		return
	}
	c.log.Debug(opWithheld, "an edge arrived for a pending stand-down; asking again", dlog.Context{"workspace": string(ws)})
	c.kick(ws, st)
}

// kick runs an attempt for ws unless one is running, in which case that one
// runs once more when it ends.
func (c *controller) kick(ws ids.WorkspaceID, st *withheldState) {
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
			outcome := c.standDownWithheld(ctx, ws)
			c.mu.Lock()
			if st.again && ctx.Err() == nil {
				st.again = false
				c.mu.Unlock()
				continue
			}
			st.running, st.again = false, false
			if outcome == withheldSettled && c.withheld[ws] == st {
				delete(c.withheld, ws)
			}
			c.mu.Unlock()
			return
		}
	})
}

// withheldReason is a withheld workspace's stand-down reason.
var withheldReason = standDownReason{
	op:     opWithheld,
	detail: "hidden, collapsed or closed by the user; the transcript was left as it stood",
	done:   "stood a withheld workspace's session down",
	park:   wsm.ParkHibernated,
}

// standDownWithheld is one attempt.
func (c *controller) standDownWithheld(ctx context.Context, ws ids.WorkspaceID) withheldOutcome {
	fields := dlog.Context{"workspace": string(ws)}
	if err := ctx.Err(); err != nil {
		c.log.Info(opWithheld, "the daemon is leaving; a withheld workspace's stand-down is not taken", dlog.WithCause(fields, err))
		return withheldPending
	}
	record, err := c.deps.DB.Workspace(ctx, ws)
	if err != nil {
		if errors.Is(err, wsm.ErrNotFound) {
			c.log.Info(opWithheld, "a withheld workspace is no longer registered; nothing is stood down", fields)
			return withheldSettled
		}
		c.log.Error(opWithheld, "could not read a withheld workspace's record; its stand-down is dropped", dlog.WithCause(fields, err))
		return withheldSettled
	}
	log := c.workspaceLog(record)
	// A FOLD NEVER STANDS DOWN WHAT IT SPARES (owner ruling, 2026-10-25),
	// read afresh here too: a workspace a fold held back while it was busy may
	// have become a one-shot, or queued a merge, by the time it falls free.
	if spare, spared := record.FoldSpare(); spared {
		log.Info(opWithheld, "the folded workspace is spared; its session keeps running",
			dlog.Merge(fields, dlog.Context{"spare": string(spare)}))
		return withheldSettled
	}
	// A WORKSPACE WITH A MERGE QUEUED IS NEVER STOOD DOWN (owner ruling,
	// 2026-10-26), hidden or closed alike: its session carries the merge, and
	// the queue can hold it for hours (wsm.Workspace.Runs).
	queued, err := bringup.MergeQueued(ctx, c.deps.DB, record)
	if err != nil {
		log.Error(opWithheld, "could not read whether the workspace has a merge queued; its stand-down is dropped", dlog.WithCause(fields, err))
		return withheldSettled
	}
	if queued {
		log.Info(opWithheld, "the workspace has a merge queued; its session keeps running", fields)
		return withheldSettled
	}
	// SHOWN OR REOPENED MEANWHILE IS NOT STOOD DOWN: the visibility and the
	// close are read afresh on every attempt. A CLOSED workspace is withheld
	// whatever its visibility: its cache lapsed after its last keep-alive
	// round (cachelapse.Keeper).
	visible, err := bringup.Visible(ctx, c.deps.DB, ws)
	if err != nil {
		log.Error(opWithheld, "could not read whether the workspace is still withheld; its stand-down is dropped", dlog.WithCause(fields, err))
		return withheldSettled
	}
	if visible && !record.Closed {
		log.Info(opWithheld, "the workspace was shown or reopened before its session was stood down; it keeps running", fields)
		return withheldSettled
	}
	session, found, err := c.deps.DB.Session(ctx, ws)
	if err != nil {
		log.Error(opWithheld, "could not read a withheld workspace's session; its stand-down is dropped", dlog.WithCause(fields, err))
		return withheldSettled
	}
	if !found || session.Terminal != nil {
		log.Debug(opWithheld, "a withheld workspace has no live session; nothing to stand down", fields)
		return withheldSettled
	}
	switch gate := c.gate(ws); gate {
	case gateOpen:
	case gateNotServing:
		log.Debug(opWithheld, "a withheld workspace has no shim to address; nothing to stand down", fields)
		return withheldSettled
	case gateBusy, gateStarting, gateReviving:
		log.Info(opWithheld, "a withheld workspace's session is busy; its stand-down waits for the turn to end",
			dlog.Merge(fields, dlog.Context{"gate": gate.String()}))
		return withheldPending
	default:
		log.Error(opWithheld, "the stand-down gate answered a value this stand-down cannot read; it is dropped",
			dlog.Merge(fields, dlog.Context{"gate": gate.String(), "invariant_violation": "every gate value is handled"}))
		return withheldSettled
	}
	switch c.hibernate(ctx, log, ws, fields, withheldReason) {
	case hibernateDone:
		return withheldSettled
	case hibernateDeferred:
		log.Info(opWithheld, "a withheld workspace's stand-down was deferred; it is taken on the next turn end", fields)
		return withheldPending
	default:
		// hibernate recorded the failure at its own level; the idle sweep's
		// backoff is not this path's, and the next hide asks afresh.
		return withheldSettled
	}
}
