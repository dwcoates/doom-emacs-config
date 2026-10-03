package workspace

import (
	"context"
	"errors"
	"fmt"
	"sync"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/shimclient"
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
	// observeFinish, when set, is told every flight that finished, after its
	// outcome is published. It is the test seam for a revival that outlives
	// the caller that started it; production leaves it nil.
	observeFinish func(ids.WorkspaceID)
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
	observe := r.observeFinish
	r.mu.Unlock()
	flight.revived, flight.err = revived, err
	close(flight.done)
	if observe != nil {
		observe(ws)
	}
}

// reviveIfSessionless brings the session of a workspace with none behind it
// up -- a hibernated one's, lifting its park, and any other open one's (see
// lead). It reports whether a revival was performed, so the caller's own
// record can say a look woke a workspace.
//
// SINGLE-FLIGHT PER WORKSPACE (see revivalFlights): a caller arriving while a
// revival of the same workspace is in flight joins it and answers the
// leader's outcome, a failure included, without starting anything itself.
//
// While the leader's Sessions.Start runs, the roster row carries the REVIVING
// marker; it is lowered the moment Start returns, whichever way it returned,
// so the row never says "coming back" about a session that already did or
// never will.
//
// THE REVIVAL IS NOT THE CALLER'S TO CANCEL. It runs as a detached start
// (Sessions.StartDetached, on the fleet's own lifetime, which the daemon's
// exit ends and joins), so a caller that leaves mid-bring-up -- a user
// switching workspaces quickly -- stops WAITING and nothing else: the
// bring-up lands whole. Bound to the rpc, it was torn in half at its session
// record's write ("begin transaction: context canceled") and reported at
// ERROR as a session that did not come back (2026-09-24T18:28:56, workspace
// 0100059cb65649bc). A caller that left is recorded at INFO and answered its
// own cancellation.
func (v *verbs) reviveIfSessionless(ctx context.Context, log dlog.Logger, operation string, ws ids.WorkspaceID) (bool, error) {
	flight, leads := v.revivals.join(ws)
	if leads {
		v.lead(ctx, log, operation, ws, flight)
	} else {
		log.Debug(operation, "a revival of this workspace is already in flight; joining it", nil)
	}
	select {
	case <-flight.done:
		return flight.revived, flight.err
	case <-ctx.Done():
	}
	// An outcome that is already published is answered: the caller did not
	// miss it.
	select {
	case <-flight.done:
		return flight.revived, flight.err
	default:
	}
	log.Info(operation, "the caller left before the revival finished; the revival goes on without it", dlog.Context{"cause": ctx.Err().Error()})
	return false, fmt.Errorf("revive %q: await the revival: %w", ws, ctx.Err())
}

// lead is the leader's half of reviveIfSessionless: the session check and,
// for a workspace with no session behind it, the detached bring-up. A parked
// workspace's bring-up runs under the REVIVING marker and lifts the park. It
// finishes the flight on every path. The reads are the flight's, not the
// caller's, so they are not cancelled with the caller either.
//
// A WORKSPACE LOOKED AT WITH NO SESSION IS STARTED, PARKED OR NOT (owner
// ruling, 2026-10-02: "it should be structurally required to have started up
// when the workspace's webpage was restarted or opened"). The feed draws
// history only through a live session's watch, so a selected workspace with
// no session showed an empty feed until a prompt revived it under a "starting
// up" hold. Only a hibernated one used to be revived by a look; a workspace
// left session-less any other way -- a bring-up that failed, a shim left down
// after dying twice, a row never opened -- stayed blank. A closed workspace is
// not started: closing it is how the user said it should have no session.
func (v *verbs) lead(ctx context.Context, log dlog.Logger, operation string, ws ids.WorkspaceID, flight *revivalFlight) {
	reads := context.WithoutCancel(ctx)
	asleep, err := v.parked(reads, ws)
	if err != nil {
		log.Error(operation, "could not tell whether the workspace was hibernated", dlog.Context{"cause": err.Error()})
		v.revivals.finish(ws, flight, false, err)
		return
	}
	if !asleep {
		v.startSessionless(reads, log, operation, ws, flight)
		return
	}
	log.Info(operation, "reviving the hibernated workspace", nil)
	v.deps.Sidebar.SetReviving(ws, true)
	v.deps.Sessions.StartDetached(ws, func(err error) {
		v.deps.Sidebar.SetReviving(ws, false)
		if err != nil {
			if why, ended := startEndedByDaemon(err); ended {
				log.Info(operation, "the hibernated workspace's revival "+why, dlog.Context{"cause": err.Error()})
			} else {
				log.Error(operation, "the hibernated workspace's session did not come back up", dlog.Context{"cause": err.Error()})
			}
			v.revivals.finish(ws, flight, false, fmt.Errorf("revive %q: start the session: %w", ws, err))
			return
		}
		v.unpark(ws)
		log.Info(operation, "revived the hibernated workspace", nil)
		v.revivals.finish(ws, flight, true, nil)
	})
}

// startSessionless is lead's branch for a workspace that is NOT parked: a
// live session or a closed row is left as it is, and anything else -- an open
// workspace with no session behind it -- has its session started detached,
// exactly as a parked one's revival is. It finishes the flight on every path.
func (v *verbs) startSessionless(ctx context.Context, log dlog.Logger, operation string, ws ids.WorkspaceID, flight *revivalFlight) {
	if v.deps.Sessions.Live(ws) {
		v.revivals.finish(ws, flight, false, nil)
		return
	}
	record, err := v.deps.DB.Workspace(ctx, ws)
	if err != nil {
		log.Error(operation, "could not read the workspace looked at with no session behind it", dlog.Context{"cause": err.Error()})
		v.revivals.finish(ws, flight, false, fmt.Errorf("revive %q: read the workspace: %w", ws, err))
		return
	}
	if record.Closed {
		log.Debug(operation, "a closed workspace was looked at; it is not started", nil)
		v.revivals.finish(ws, flight, false, nil)
		return
	}
	log.Info(operation, "starting the session of a workspace looked at with none behind it", nil)
	v.deps.Sessions.StartDetached(ws, func(err error) {
		if err != nil {
			if why, ended := startEndedByDaemon(err); ended {
				log.Info(operation, "the looked-at workspace's start "+why, dlog.Context{"cause": err.Error()})
			} else {
				log.Error(operation, "the looked-at workspace's session did not come up", dlog.Context{"cause": err.Error()})
			}
			v.revivals.finish(ws, flight, false, fmt.Errorf("revive %q: start the session: %w", ws, err))
			return
		}
		log.Info(operation, "started the session of the looked-at workspace", nil)
		v.revivals.finish(ws, flight, true, nil)
	})
}

// startEndedByDaemon reports whether a DETACHED start ended because this
// daemon is leaving rather than because the session failed to come up, and
// says which. A detached start runs on the fleet's own lifetime, so its
// context ending is the daemon's exit, and a standing-down refusal is the
// same departure reached from the other side.
func startEndedByDaemon(err error) (string, bool) {
	switch {
	case canceled(err):
		return "ended when its context was cancelled", true
	case errors.Is(err, shimclient.ErrStandingDown):
		return "stopped because this daemon is standing down", true
	case errors.Is(err, ErrHandedOver):
		return "finished after the workspace was handed to a successor, which adopts its shim", true
	default:
		return "", false
	}
}
