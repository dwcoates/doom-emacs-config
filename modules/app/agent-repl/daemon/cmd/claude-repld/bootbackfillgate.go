package main

import (
	"time"

	"claude-repld/internal/dlog"
)

// backfillHoldGrace bounds how long deferred boot work yields to a
// still-absent host. A daemon booted with no Emacs attached must still repair
// its merge geometry — the repair is load-bearing for every merge — so the
// hold is a YIELD, never a dependency.
const backfillHoldGrace = 5 * time.Second

// backfillStartReason names why the hold ended. It is logged, so a boot that
// ran the backfill early says so rather than leaving the operator to infer it.
type backfillStartReason string

const (
	// backfillStartHostServed: the reconnecting host got its connect snapshot,
	// which is the event the whole recovery budget is spent on. This is the
	// ordinary reason on any bounce Emacs is waiting through.
	backfillStartHostServed backfillStartReason = "host-connect-snapshot-served"
	// backfillStartGrace: no host connected within the grace. The repair runs
	// anyway; refusing to would make merges depend on an Emacs being up.
	backfillStartGrace backfillStartReason = "hold-grace-expired"
	// backfillStartShutdown: the daemon is going down; the caller must not run.
	backfillStartShutdown backfillStartReason = "daemon-shutdown"
)

// backfillHold makes deferred boot work yield to the host's connect.
//
// THE ORDERING THIS ENFORCES IS THE POINT. geometry-backfill spawns a git/stat
// subprocess per unrecorded workspace; at fleet scale that is ~145 of them,
// landing microseconds after the listeners come up and delaying the host's
// accept by ~1.5s of a 3s per-workspace recovery budget. Deferring the work off
// the SERIAL boot path was never enough — it only moved the contention. This
// holds it until the host has actually been served.
//
// served and grace are injected channels rather than a wall clock so a test
// asserts the GATE, deterministically, instead of racing a sleep.
type backfillHold struct {
	served <-chan struct{}
	grace  <-chan time.Time
	stop   <-chan struct{}
	log    *dlog.Logger
}

// newBackfillHold builds the production hold: yield until the frontend serves a
// host connect snapshot, or until backfillHoldGrace elapses.
func newBackfillHold(served <-chan struct{}, stop <-chan struct{}, log *dlog.Logger) *backfillHold {
	if served == nil || log == nil {
		panic("claude-repld: the deferred-backfill hold requires a host-connect signal and a logger")
	}
	return &backfillHold{served: served, grace: time.After(backfillHoldGrace), stop: stop, log: log}
}

// Wait blocks until the held work may start, and reports why it may. A false
// second return means the daemon is shutting down and the work must NOT run.
func (h *backfillHold) Wait() (backfillStartReason, bool) {
	// A host that was already served before the hold was built must not wait
	// out the grace: select's arms are evaluated together, so this is decided
	// by which channel is ready, not by which was written first.
	select {
	case <-h.served:
		return h.release(backfillStartHostServed), true
	case <-h.grace:
		return h.release(backfillStartGrace), true
	case <-h.stop:
		return h.release(backfillStartShutdown), false
	}
}

func (h *backfillHold) release(reason backfillStartReason) backfillStartReason {
	h.log.With("operation", "boot.backfill.hold", "reason", string(reason)).
		Log("claude-repld: deferred boot backfill released: %s", reason)
	return reason
}
