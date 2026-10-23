package main

import (
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
		idle := gate.Idle()
		for warnings := 1; ; warnings++ {
			select {
			case <-idle:
				log.Info(op, "every create in flight finished; the exit proceeds", dlog.Context{"creates_in_flight": inFlight})
				return
			case <-after(every):
				log.Warn(op, "the exit is still waiting for creates in flight to finish", dlog.Context{
					"creates_in_flight": gate.InFlight(),
					"warnings":          warnings,
					"cadence":           every.String(),
				})
			}
		}
	}
}
