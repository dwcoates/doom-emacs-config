package harness

import (
	"testing"

	"agent-repld/integration/grantedclock"
)

// THE HARNESS'S BOUNDS ARE KEPT ON THE GRANTED CLOCK (grantedclock), never on
// wall time.
//
// A bound here is a claim that the daemon, or a process it started, failed to
// do something in time. Wall time is not their time when the host is short of
// CPU: a full run puts every chunk's test binary, daemon and shims on one
// machine, and a busy moment leaves a boot runnable with nowhere to run.
// MEASURED, merge gate 2026-10-20 (branch reply-quote-label-plural, a string
// change): four TestEveryScenarioGoldenProjectsToItsFeedGolden cases failed
// their daemon.addr wait at DefaultTimeout while the daemon's SIGQUIT dump
// showed it still in package init and inside sqlite, runnable; the same cases
// pass alone. Every wait the harness bounds -- the run budget, each wait off
// it, a stop's reap, a dump's exit, a leader's stall report, the watcher hold
// -- is a grantedclock.WithBound context, so it is charged only the time this
// binary's processes were not starved (on darwin, this process tree's runnable
// time; on linux, the container's cpu.pressure).
//
// A RUN THAT CANNOT READ THE CLOCK DOES NOT START: every bound above is kept on
// it, so it is refused once, here, rather than as every wait's failure.
func requireGrantedClock(t testing.TB) {
	t.Helper()
	requireGrantedClockFrom(t, grantedclock.Source)
}

// requireGrantedClockFrom is requireGrantedClock on SOURCE.
func requireGrantedClockFrom(t testing.TB, source func() (grantedclock.Stall, error)) {
	t.Helper()
	stall, err := source()
	if err == nil {
		_, err = stall()
	}
	if err != nil {
		t.Fatalf("harness: the CPU stall cannot be read, and every harness bound is kept on it: %v", err)
	}
}
