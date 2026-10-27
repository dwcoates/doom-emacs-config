package cachelapse

import (
	"context"
	"errors"
	"slices"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// A SESSION RESTORED ONLY TO BE VIEWED IS PARKED AGAIN WHEN THE USER LOOKS
// AWAY (owner ruling, 2026-10-27). A reader that needs a parked session's
// history restores it (RestoreParked), so flicking through workspaces -- or
// every page reloading after a rollout -- would bring each one's shim back
// until its next beat. Instead the Keeper remembers each restore a reader
// asked for, and once the user is not looking at the workspace it asks the
// keep-alive cycle to stand the session down again, timed from the instant
// the restore came up: a prompt since then is engagement, and the cycle keeps
// the session up (drain.Controller.CycleDown), as it keeps a busy one up until
// it falls free. One the user switched away from before its restore started
// is taken out of line instead.
//
// A READER NEVER RESTORES A WORKSPACE THE USER IS NOT LOOKING AT (owner
// ruling, 2026-10-27). Every page reloads after a rollout and asks for its
// newest rows; a parked workspace's page is left unloaded instead, and its
// ask is remembered, so selecting the workspace restores it and the waiting
// reader is served once the session is up.

// viewedUp takes a reader's restore coming up: the session is viewed from now,
// and stood down at once when the user is already not looking at it.
func (k *Keeper) viewedUp(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID) {
	if _, ok := k.viewed[ws]; !ok {
		return
	}
	k.viewed[ws] = k.deps.Clock.Now()
	record, err := k.deps.DB.Workspace(ctx, ws)
	if err != nil {
		delete(k.viewed, ws)
		if errors.Is(err, wsm.ErrNotFound) {
			log.Info(opRestore, "a workspace restored to be viewed is no longer registered; nothing is stood down", nil)
			return
		}
		if !leaving(ctx, log, opRestore, "reading the workspace restored to be viewed", err) {
			log.Error(opRestore, "could not read whether the user still looks at the workspace restored to be viewed; it stays up until its next keep-alive", dlog.Context{"cause": err.Error()})
		}
		return
	}
	k.releaseViewed(log, record)
}

// restoreDeferred restores RECORD's parked session once the user selects it,
// when a reader asked for its history while the user was not looking at it.
func (k *Keeper) restoreDeferred(ctx context.Context, log dlog.Logger, record wsm.Workspace) {
	if !record.Current || !k.deferred[record.ID] {
		return
	}
	log.Info(opRestore, "the user selected a workspace whose parked session a reader is waiting on; it is restored", nil)
	k.restoreAsked(ctx, record.ID)
}

// releaseViewed stands RECORD's session down again when a reader's restore
// brought it up and the user is not looking at it: out of line when its
// restore has not started, through the keep-alive cycle once it is up. One
// still restoring is decided when its restore ends (viewedUp).
func (k *Keeper) releaseViewed(log dlog.Logger, record wsm.Workspace) {
	ws := record.ID
	upAt, ok := k.viewed[ws]
	if !ok || record.Current || k.restoring[ws] {
		return
	}
	delete(k.viewed, ws)
	if upAt.IsZero() {
		k.queued = slices.DeleteFunc(k.queued, func(q queuedRestore) bool { return q.ws == ws })
		log.Info(opRestore, "the user looked away before the session restored to be viewed was started; it is not restored", nil)
		return
	}
	log.Info(opRestore, "the user looked away from a session restored only to be viewed; it is stood down until its next keep-alive unless a prompt engaged it",
		dlog.Context{"up_at": upAt})
	k.deps.CycleDown(ws, upAt)
}
