package workspace

import (
	"context"
	"fmt"
	"sync"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
)

// A HIBERNATED WORKSPACE IS REVIVED BY BEING LOOKED AT, not only by being
// prompted (owner ruling, 2026-09-13). The idle sweep stands a session down to
// reclaim its ~500MB, which is the right trade for a workspace nobody has
// touched for six hours — and the wrong one the instant the user opens or
// SWITCHES TO it, because from then on every surface they read is answering
// for a session that is not there.
//
// The revival is the path the prompt queue already takes (`Sessions.Start`,
// which is `promptqueue`'s Revive), so a look-driven revival and a
// prompt-driven one are the same bring-up: THE COLD GATE STILL STANDS, and a
// revived cold session refuses until it is answered exactly as it does today.

// parked reports whether this workspace's session was stood down by the idle
// sweep. The terminal is durable and is cleared by the bring-up's own
// PutSession, so it answers no again the moment a revival lands.
//
// A READ THAT FAILS IS NEVER READ AS "NOT PARKED": the caller propagates it,
// because reviving nothing on an unreadable record is how a workspace stays
// asleep with no one able to say why.
func (v *verbs) parked(ctx context.Context, ws ids.WorkspaceID) (bool, error) {
	session, exists, err := v.deps.DB.Session(ctx, ws)
	if err != nil {
		return false, fmt.Errorf("read the session record of %q: %w", ws, err)
	}
	return exists && session.Hibernated(), nil
}

// unpark lifts the park from the SESSION-SCOPED views the sweep installed it
// on. The topbar's park is what draws the hibernated strip and the footer's is
// what stops its `disconnected` step calling the stand-down a dead link, so
// both are lifted by the revival that made them untrue.
//
// The link states the revived shim publishes lift the park too, and that
// redundancy is deliberate: those arrive when the shim attaches, and the view
// must stop claiming a sleep the moment the revival is decided rather than
// whenever the process gets around to answering.
func (v *verbs) unpark(ws ids.WorkspaceID) {
	v.deps.Topbar.SetParked(ws, false)
	v.deps.Footer.SetParked(ws, false)
}

// AT MOST ONE REVIVAL IS IN FLIGHT PER WORKSPACE. A bring-up takes the best
// part of a second, and a user switching back and forth fires several selects
// at one parked workspace inside it; each used to read "parked" (the terminal
// clears only when the bring-up lands) and each called Sessions.Start, which
// logged the workspace revived four times over for one switch. So the
// decision AND the start are single-flight, keyed by workspace: the first
// caller leads, and every caller arriving while it runs JOINS it — it starts
// nothing of its own, waits for the leader's outcome, and answers that outcome
// as its own. A caller arriving after the flight ended starts a fresh one,
// which re-reads the park and finds a revived workspace awake.
type revivalFlights struct {
	mu       sync.Mutex
	inFlight map[ids.WorkspaceID]*revivalFlight
	// observeJoin, when set, is told every caller that JOINED a flight rather
	// than leading one, after it has joined. It is the test seam that lets a
	// test release the leader only once every joiner is provably waiting on
	// it; production leaves it nil.
	observeJoin func(ids.WorkspaceID)
}

// revivalFlight is one revival's outcome, published to its joiners when done
// closes. The fields are written by the leader before the close and read by
// joiners only after it.
type revivalFlight struct {
	done    chan struct{}
	revived bool
	err     error
}

// join returns the workspace's flight and whether the caller LEADS it: true
// for a caller that found none in flight (it must run the revival and call
// finish), false for one that found a flight already running.
func (r *revivalFlights) join(ws ids.WorkspaceID) (*revivalFlight, bool) {
	r.mu.Lock()
	if flight, ok := r.inFlight[ws]; ok {
		observe := r.observeJoin
		r.mu.Unlock()
		if observe != nil {
			observe(ws)
		}
		return flight, false
	}
	if r.inFlight == nil {
		r.inFlight = map[ids.WorkspaceID]*revivalFlight{}
	}
	flight := &revivalFlight{done: make(chan struct{})}
	r.inFlight[ws] = flight
	r.mu.Unlock()
	return flight, true
}

// finish publishes the leader's outcome and retires the flight, so the next
// caller leads a fresh one.
func (r *revivalFlights) finish(ws ids.WorkspaceID, flight *revivalFlight, revived bool, err error) {
	r.mu.Lock()
	delete(r.inFlight, ws)
	r.mu.Unlock()
	flight.revived, flight.err = revived, err
	close(flight.done)
}

// reviveIfParked brings a hibernated workspace's session back and lifts the
// park. It reports whether a revival was performed, so the caller's own record
// can say a look woke a workspace.
//
// SINGLE-FLIGHT PER WORKSPACE (see revivalFlights): a caller arriving while a
// revival of the same workspace is in flight joins it and answers the
// leader's outcome, a failure included, without starting anything itself.
//
// While the leader's Sessions.Start runs, the roster row carries the REVIVING
// marker; it is lowered the moment Start returns, whichever way it returned,
// so the row never says "coming back" about a session that already did or
// never will.
func (v *verbs) reviveIfParked(ctx context.Context, log dlog.Logger, operation string, ws ids.WorkspaceID) (bool, error) {
	flight, leads := v.revivals.join(ws)
	if !leads {
		log.Debug(operation, "a revival of this workspace is already in flight; joining it", nil)
		select {
		case <-flight.done:
			return flight.revived, flight.err
		case <-ctx.Done():
			log.Error(operation, "gave up waiting on the in-flight revival", dlog.Context{"cause": ctx.Err().Error()})
			return false, fmt.Errorf("revive %q: await the in-flight revival: %w", ws, ctx.Err())
		}
	}
	revived, err := v.revive(ctx, log, operation, ws)
	v.revivals.finish(ws, flight, revived, err)
	return revived, err
}

// revive is the leader's half of reviveIfParked: the park check and, for a
// parked workspace, the bring-up under the REVIVING marker.
func (v *verbs) revive(ctx context.Context, log dlog.Logger, operation string, ws ids.WorkspaceID) (bool, error) {
	asleep, err := v.parked(ctx, ws)
	if err != nil {
		log.Error(operation, "could not tell whether the workspace was hibernated", dlog.Context{"cause": err.Error()})
		return false, err
	}
	if !asleep {
		return false, nil
	}
	log.Info(operation, "reviving the hibernated workspace", nil)
	v.deps.Sidebar.SetReviving(ws, true)
	err = v.deps.Sessions.Start(ctx, ws)
	v.deps.Sidebar.SetReviving(ws, false)
	if err != nil {
		log.Error(operation, "the hibernated workspace's session did not come back up", dlog.Context{"cause": err.Error()})
		return false, fmt.Errorf("revive %q: start the session: %w", ws, err)
	}
	v.unpark(ws)
	log.Info(operation, "revived the hibernated workspace", nil)
	return true, nil
}
