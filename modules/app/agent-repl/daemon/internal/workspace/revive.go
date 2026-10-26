package workspace

import (
	"context"
	"errors"
	"fmt"
	"sync"

	"agent-repld/internal/ctxend"
	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/shimclient"
	"agent-repld/internal/wsm"
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
	return exists && session.Parked(), nil
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
	v.deps.Topbar.SetParked(ws, wsm.ParkNone)
	v.deps.Footer.SetParked(ws, wsm.ParkNone)
}

// AT MOST ONE REVIVAL IS IN FLIGHT PER WORKSPACE. A bring-up takes the best
// part of a second, and a user switching back and forth fires several selects
// at one parked workspace inside it; each used to read "parked" (the terminal
// clears only when the bring-up lands) and each called Sessions.Start, which
// logged the workspace revived four times over for one switch. So the
// decision AND the start are single-flight, keyed by workspace: the first
// look leads, and every look arriving while it runs is carried by it and
// starts nothing of its own. A look arriving after the flight ended leads a
// fresh one, which re-reads the park and finds a revived workspace awake.
type revivalFlights struct {
	mu       sync.Mutex
	inFlight map[ids.WorkspaceID]bool
	// observeJoin, when set, is told every look that a flight in progress
	// carried rather than one it led. It is a test seam; production leaves it
	// nil.
	observeJoin func(ids.WorkspaceID)
	// observeFinish, when set, is told every flight that finished and how. It
	// is the test seam for a revival no caller waits on; production leaves it
	// nil.
	observeFinish func(ws ids.WorkspaceID, revived bool, err error)
}

// join answers whether the caller LEADS the workspace's revival: true for one
// that found none in flight (it must decide, start, and finish), false for one
// a flight in progress already carries.
func (r *revivalFlights) join(ws ids.WorkspaceID) bool {
	r.mu.Lock()
	if r.inFlight[ws] {
		observe := r.observeJoin
		r.mu.Unlock()
		if observe != nil {
			observe(ws)
		}
		return false
	}
	if r.inFlight == nil {
		r.inFlight = map[ids.WorkspaceID]bool{}
	}
	r.inFlight[ws] = true
	r.mu.Unlock()
	return true
}

// finish retires the workspace's flight, so the next look leads a fresh one.
func (r *revivalFlights) finish(ws ids.WorkspaceID, revived bool, err error) {
	r.mu.Lock()
	delete(r.inFlight, ws)
	observe := r.observeFinish
	r.mu.Unlock()
	if observe != nil {
		observe(ws, revived, err)
	}
}

// startRevival brings up the session of a workspace looked at with none
// behind it -- a hibernated one's, lifting its park, and any other open
// one's (see lead) -- WITHOUT THE CALLER WAITING FOR IT.
//
// A SESSION START IS NOT PART OF A SELECTION'S ANSWER (SelectWorkspace
// answers that the workspace is selected, nothing more), exactly as it is not
// part of a registration's (Fleet.StartDetached). Waiting on it made the
// answer as slow as the vendor: on 2026-10-25T14:53:08 Emacs selected
// `fold-spares-busy`, whose session had to be brought up, and timed the
// select out at 10s with the selection long recorded. The bring-up reports
// its own outcome -- the REVIVING marker, its records, a fault the start
// itself raises -- so nothing about it is lost by not answering it here.
//
// THE DECISION IS SYNCHRONOUS AND ONLY THE BRING-UP IS DETACHED. The reads
// that decide whether a start is owed are the caller's to see fail, and a
// daemon that is standing down refuses the start before anything is detached
// (shimclient.ErrStandingDown), so the caller can still tell its client to
// ask the daemon that serves next. A look carried by a flight in progress
// answers nil: that flight starts the session.
func (v *verbs) startRevival(ctx context.Context, log dlog.Logger, operation string, ws ids.WorkspaceID) error {
	if !v.revivals.join(ws) {
		log.Debug(operation, "a revival of this workspace is already in flight; it carries this look too", nil)
		return nil
	}
	return v.lead(context.WithoutCancel(ctx), log, operation, ws)
}

// lead is the leader's half of startRevival: the session check and, for a
// workspace with no session behind it, the detached bring-up. A parked
// workspace's bring-up runs under the REVIVING marker and lifts the park. It
// finishes the flight on every path, and answers only what failed before
// anything was detached.
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
func (v *verbs) lead(ctx context.Context, log dlog.Logger, operation string, ws ids.WorkspaceID) error {
	asleep, err := v.parked(ctx, ws)
	if err != nil {
		log.Error(operation, "could not tell whether the workspace was hibernated", dlog.Context{"cause": err.Error()})
		v.revivals.finish(ws, false, err)
		return err
	}
	if !asleep {
		return v.startSessionless(ctx, log, operation, ws)
	}
	if err := v.refuseStartStandingDown(ws); err != nil {
		return err
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
			v.revivals.finish(ws, false, fmt.Errorf("revive %q: start the session: %w", ws, err))
			return
		}
		v.unpark(ws)
		log.Info(operation, "revived the hibernated workspace", nil)
		v.revivals.finish(ws, true, nil)
	})
	return nil
}

// refuseStartStandingDown finishes the workspace's flight and answers
// shimclient.ErrStandingDown when this daemon is standing down: it starts no
// session for a workspace it is about to stop serving, and says so before
// anything is detached.
func (v *verbs) refuseStartStandingDown(ws ids.WorkspaceID) error {
	if !v.deps.Sessions.StandingDown() {
		return nil
	}
	v.revivals.finish(ws, false, shimclient.ErrStandingDown)
	return fmt.Errorf("revive %q: %w", ws, shimclient.ErrStandingDown)
}

// startSessionless is lead's branch for a workspace that is NOT parked: a
// live session or a closed row is left as it is, and anything else -- an open
// workspace with no session behind it -- has its session started detached,
// exactly as a parked one's revival is. It finishes the flight on every path.
func (v *verbs) startSessionless(ctx context.Context, log dlog.Logger, operation string, ws ids.WorkspaceID) error {
	if v.deps.Sessions.Live(ws) {
		v.revivals.finish(ws, false, nil)
		return nil
	}
	record, err := v.deps.DB.Workspace(ctx, ws)
	if err != nil {
		log.Error(operation, "could not read the workspace looked at with no session behind it", dlog.Context{"cause": err.Error()})
		err = fmt.Errorf("revive %q: read the workspace: %w", ws, err)
		v.revivals.finish(ws, false, err)
		return err
	}
	if record.Closed {
		log.Debug(operation, "a closed workspace was looked at; it is not started", nil)
		v.revivals.finish(ws, false, nil)
		return nil
	}
	if err := v.refuseStartStandingDown(ws); err != nil {
		return err
	}
	log.Info(operation, "starting the session of a workspace looked at with none behind it", nil)
	v.deps.Sessions.StartDetached(ws, func(err error) {
		if err != nil {
			if why, ended := startEndedByDaemon(err); ended {
				log.Info(operation, "the looked-at workspace's start "+why, dlog.Context{"cause": err.Error()})
			} else {
				log.Error(operation, "the looked-at workspace's session did not come up", dlog.Context{"cause": err.Error()})
			}
			v.revivals.finish(ws, false, fmt.Errorf("revive %q: start the session: %w", ws, err))
			return
		}
		log.Info(operation, "started the session of the looked-at workspace", nil)
		v.revivals.finish(ws, true, nil)
	})
	return nil
}

// startEndedByDaemon reports whether a DETACHED start ended for a reason that
// is no agent-repl failure of the session coming up, and says which: this
// daemon leaving (a detached start runs on the fleet's own lifetime, so its
// context ending is the daemon's exit, and a standing-down refusal is the same
// departure reached from the other side), its shim taken by a handover or a
// kill, or a VENDOR FAULT -- the vendor would not start, or this machine could
// not reach the network -- which the start itself recorded as that fault, on
// the footer and the roster (owner rule: vendor faults never gate agent-repl's
// own functions, and are not agent-repl's errors).
func startEndedByDaemon(err error) (string, bool) {
	var label *startLabel
	switch {
	case errors.As(err, &label) && label.network:
		return "met a network this machine cannot reach; its network fault stands", true
	case errors.As(err, &label) && label.vendor:
		return "met a vendor that would not start; its vendor fault stands", true
	case ctxend.Is(err):
		return "ended when its context was cancelled", true
	case errors.Is(err, shimclient.ErrStandingDown):
		return "stopped because this daemon is standing down", true
	case errors.Is(err, ErrShimTaken):
		return "ended when its shim was taken from it (handed to a successor, or stopped)", true
	default:
		return "", false
	}
}
