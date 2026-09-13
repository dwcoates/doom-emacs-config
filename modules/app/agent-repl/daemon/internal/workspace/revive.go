package workspace

import (
	"context"
	"fmt"

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

// reviveIfParked brings a hibernated workspace's session back and lifts the
// park. It reports whether a revival was performed, so the caller's own record
// can say a look woke a workspace.
func (v *verbs) reviveIfParked(ctx context.Context, log dlog.Logger, operation string, ws ids.WorkspaceID) (bool, error) {
	asleep, err := v.parked(ctx, ws)
	if err != nil {
		log.Error(operation, "could not tell whether the workspace was hibernated", dlog.Context{"cause": err.Error()})
		return false, err
	}
	if !asleep {
		return false, nil
	}
	if err := v.deps.Sessions.Start(ctx, ws); err != nil {
		log.Error(operation, "the hibernated workspace's session did not come back up", dlog.Context{"cause": err.Error()})
		return false, fmt.Errorf("revive %q: start the session: %w", ws, err)
	}
	v.unpark(ws)
	log.Info(operation, "revived the hibernated workspace", nil)
	return true, nil
}
