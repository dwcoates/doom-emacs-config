package main

import (
	"context"
	"fmt"

	"claude-repld/internal/workspace/geometry"
)

// deferredGeometryBackfill is the boot-time merge-geometry repair, moved OFF
// the serial boot path and behind a gate the merge command path awaits.
//
// THE REPAIR IS A GIT SUBPROCESS PER UNRECORDED WORKSPACE. With a fleet in the
// hundreds that is tens of seconds of boot, and every one of those seconds used
// to sit in front of the frontend UDS listener — which is what Emacs's startup
// restore probes with a connect snapshot. The snapshot owes this repair
// nothing: an unrecorded workspace's branch renders as unknown and is corrected
// by the next push.
//
// A MERGE DOES OWE IT. resolveMergeGeometry refuses an unrecorded workspace with
// an explanation, and a workspace that is merely not-yet-derived is not
// unrecorded — refusing it would turn a boot optimization into a wrong answer.
// So the merge command path reads THROUGH this type, which blocks until the
// repair has finished before it answers. That is the one retained ordering; it
// gates the merge commands rather than the listener.
type deferredGeometryBackfill struct {
	store *geometry.Store
	// done is closed exactly once, by Finish, when the backfill has completed.
	done chan struct{}
}

func newDeferredGeometryBackfill(store *geometry.Store) *deferredGeometryBackfill {
	if store == nil {
		panic("claude-repld: deferred geometry backfill requires a geometry store")
	}
	return &deferredGeometryBackfill{store: store, done: make(chan struct{})}
}

// Finish opens the gate. It is called once, after the backfill has run.
func (d *deferredGeometryBackfill) Finish() { close(d.done) }

// Lookup answers a merge command's geometry question, waiting for the backfill
// first. A cancelled context is reported as an error and NEVER as "no record":
// a not-found answer is what refuses a merge, and manufacturing one out of a
// cancellation would refuse a mergeable workspace for a reason that is not its
// own.
func (d *deferredGeometryBackfill) Lookup(ctx context.Context, workspace string) (geometry.Record, bool, error) {
	select {
	case <-d.done:
	case <-ctx.Done():
		return geometry.Record{}, false, fmt.Errorf("claude-repld: merge geometry for %s is unavailable: the boot-time geometry backfill had not finished when this request was cancelled: %w", workspace, ctx.Err())
	}
	return d.store.Lookup(ctx, workspace)
}
