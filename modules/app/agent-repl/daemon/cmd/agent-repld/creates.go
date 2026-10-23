package main

import (
	"context"
	"fmt"
	"time"

	"agent-repld/internal/creategate"
	"agent-repld/internal/dlog"
)

// awaitCreatesAtExit answers the exit's wait for the creates in flight: it
// closes the create gate, naming no successor, and waits until every create
// it admitted has finished, naming them every EVERY while it waits.
//
// EVERY EXIT WAITS, not only a rollout (2026-10-23): a drain, a signal and a
// state-root loss each cut a create off wherever it stood, leaving a worktree
// and a branch with no workspace. A handover or restart has closed the gate
// and waited already, so here it finds nothing in flight. Each step of a
// create is bounded, so the wait is too.
func awaitCreatesAtExit(gate *creategate.Gate, every time.Duration, after func(time.Duration) <-chan time.Time, log dlog.Logger) func() {
	const op = "daemon.cmd.serve"
	return func() {
		gate.Close("")
		inFlight := gate.InFlight()
		if inFlight == 0 {
			log.Info(op, "closed the create gate with no create in flight", nil)
			return
		}
		log.Info(op, "the exit waits for the creates in flight to finish", dlog.Context{"creates_in_flight": inFlight})
		// The exit has no lifetime left to end this wait; each create's own
		// steps are bounded, and that bounds it. A context that never ends
		// cannot end the wait, so an error here is a broken invariant.
		err := gate.AwaitIdle(context.Background(), every, after, func(warnings, inFlight int) {
			log.Warn(op, "the exit is still waiting for creates in flight to finish", dlog.Context{
				"creates_in_flight": inFlight,
				"warnings":          warnings,
				"cadence":           every.String(),
			})
		})
		if err != nil {
			panic(fmt.Sprintf("agent-repld: the exit's wait for creates ended with %v under a context that never ends", err))
		}
		log.Info(op, "every create in flight finished; the exit proceeds", dlog.Context{"creates_in_flight": inFlight})
	}
}
