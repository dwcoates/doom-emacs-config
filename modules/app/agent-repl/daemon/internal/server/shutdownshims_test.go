package server

import (
	"errors"
	"fmt"
	"slices"
	"strings"
	"testing"
	"time"

	"claude-repld/internal/sessioncontroller"
)

// ShutdownAll owns the idle sweeper's lifetime. Once shutdown has begun, it
// cannot return while a sweep still has access to daemon-owned durable state.
// This pins the ordering that prevents a sweep from reading a closed token
// ledger during teardown.
func TestShutdownAllJoinsAnInFlightIdleSweep(t *testing.T) {
	// Arrange.
	ticks := make(chan time.Time)
	h := newHarnessWith(t, Config{IdleSweepTicks: ticks})
	sweepStarted := make(chan struct{})
	releaseSweep := make(chan struct{})
	h.srv.idleSweep = func() {
		close(sweepStarted)
		<-releaseSweep
	}

	// Act: make the only sweeper enter its state-owning work, then begin
	// shutdown while that work is still held.
	ticks <- time.Now()
	<-sweepStarted
	shutdownReturned := make(chan struct{})
	go func() {
		h.srv.ShutdownAll(false, sessioncontroller.StopCauseDaemonShutdown())
		close(shutdownReturned)
	}()
	<-h.srv.stopped

	// Assert: closing the stop latch cannot let teardown outrun the in-flight
	// owner. The default arm is deterministic because releaseSweep is still
	// unclosed, so the worker cannot reach sweeperDone.
	select {
	case <-shutdownReturned:
		t.Fatal("ShutdownAll returned while the idle sweeper still owned durable-state access")
	default:
	}
	close(releaseSweep)
	<-shutdownReturned
}

// SHIMS SURVIVE AN ORDERLY SHUTDOWN.
//
// The daemon used to SIGTERM every live shim on its way out, which threw away
// exactly what the transport inversion was built to preserve: a shim outlives
// its daemon, redials the one well-known socket forever, and is parked by the
// next daemon's listener before anything asks for it. These pin the new
// default and the one mode that still wants the old behavior.

// TestShutdownPreservesShimsByDefault — the default is PRESERVE, and it is the
// default precisely because an unqualified "stop this daemon" says nothing
// about the conversations behind it.
func TestShutdownPreservesShimsByDefault(t *testing.T) {
	// Arrange — a live session, so there is a shim to preserve or kill.
	h := newHarness(t)
	id := createSession(t, h, `{"cwd":"/w"}`)
	if err := h.controller.Ensure("/w"); err != nil {
		t.Fatalf("Ensure: %v", err)
	}
	// Act.
	h.srv.ShutdownAll(false, sessioncontroller.StopCauseDaemonShutdown())

	// Assert.
	if stopped := h.spawner.stoppedIDs(); len(stopped) != 0 {
		t.Fatalf("shutdown stopped %v (session %s); a preserved shim must survive to redial the next daemon", stopped, id)
	}
}

// TestShutdownStopShimsModeStopsThem — the escape hatch a bundle-changing
// deploy uses, where a survivor would keep running the previous build.
func TestShutdownStopShimsModeStopsThem(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	id := createSession(t, h, `{"cwd":"/w"}`)
	if err := h.controller.Ensure("/w"); err != nil {
		t.Fatalf("Ensure: %v", err)
	}
	markControllerOperational(t, h, "/w")

	// Act.
	h.srv.ShutdownAll(true, sessioncontroller.StopCauseDaemonShutdown())

	// Assert.
	if stopped := h.spawner.stoppedIDs(); !slices.Contains(stopped, id) {
		t.Fatalf("stop-shims mode stopped %v, want session %s among them", stopped, id)
	}
}

// THE DRAIN OVERLAPS ITS SHIM EXIT WAITS, and this is the test that says so.
//
// Every session's hibernation ends in a SIGTERM plus a wait on the shim's
// actual exit, so a serial drain costs the SUM of the fleet's exit waits: a
// ~90-session fleet overran the daemon's 30s stop grace and was SIGKILLed
// mid-drain, which is what skips lease release and merge reconstruction.
//
// The proof is structural rather than a stopwatch. Every stop blocks until ALL
// of them have arrived, so a drain that issues them one at a time can never
// reach the second one — the barrier is unsatisfiable under a serial drain and
// trivially satisfied under a concurrent one.
func TestShutdownDrainsItsShimStopsConcurrently(t *testing.T) {
	// Arrange — a live fleet, sized under the pool's own bound rather than from
	// it, so shrinking the bound fails this test instead of weakening it.
	if shutdownHibernateWorkers < 8 {
		t.Fatalf("shutdownHibernateWorkers = %d, too narrow to overlap a fleet's shim exit waits", shutdownHibernateWorkers)
	}
	h := newHarness(t)
	const fleet = 8
	for i := range fleet {
		ws := fmt.Sprintf("/w%d", i)
		createSession(t, h, fmt.Sprintf(`{"cwd":%q}`, ws))
		if err := h.controller.Ensure(ws); err != nil {
			t.Fatalf("Ensure(%s): %v", ws, err)
		}
		markControllerOperational(t, h, ws)
	}
	arrived := make(chan string, fleet)
	release := make(chan struct{})
	h.spawner.stopHook = func(sessionID string) {
		arrived <- sessionID
		<-release
	}

	// Act.
	drained := make(chan struct{})
	go func() {
		h.srv.ShutdownAll(true, sessioncontroller.StopCauseDaemonShutdown())
		close(drained)
	}()

	// Assert: all fleet stops are in flight at the same instant.
	for i := range fleet {
		select {
		case <-arrived:
		case <-time.After(30 * time.Second):
			t.Fatalf("only %d of %d shim stops were in flight together; the drain is issuing them serially, so its cost is the sum of the fleet's exit waits", i, fleet)
		}
	}
	close(release)
	<-drained
	if stopped := h.spawner.stoppedIDs(); len(stopped) != fleet {
		t.Fatalf("stopped %d shims, want all %d — a concurrent drain must still reach every session", len(stopped), fleet)
	}
}

// A parallel drain must surface every session's failure, not the last one to
// finish: the joined error is what tells a shutdown it lost sessions.
func TestShutdownDrainJoinsEverySessionsHibernateFailure(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	var ids []string
	for i := range 3 {
		ws := fmt.Sprintf("/w%d", i)
		ids = append(ids, createSession(t, h, fmt.Sprintf(`{"cwd":%q}`, ws)))
		if err := h.controller.Ensure(ws); err != nil {
			t.Fatalf("Ensure(%s): %v", ws, err)
		}
		markControllerOperational(t, h, ws)
	}
	h.spawner.stopErr = errors.New("shim refused to stop")

	// Act.
	err := h.srv.hibernateAllForShutdown(sessioncontroller.StopCauseDaemonShutdown())

	// Assert.
	if err == nil {
		t.Fatal("hibernateAllForShutdown reported success while every session's stop failed")
	}
	for _, id := range ids {
		if !strings.Contains(err.Error(), id) {
			t.Fatalf("joined drain error %v does not name session %s; a parallel drain must not lose a session's failure", err, id)
		}
	}
}

// The drain's join on the idle sweeper is only as short as the sweep it joins,
// and a sweep is a serial walk that hibernates whatever it reaches. Once
// teardown has begun the sweep abandons its remainder — the shutdown drain
// hibernates those sessions itself, concurrently.
func TestIdleSweepAbandonsItsRemainderOnceTeardownBegins(t *testing.T) {
	// Arrange — the same fixture TestIdleSweepPersistsAnIdleCutoffHibernation
	// proves this sweep DOES hibernate, with teardown already begun.
	h, id := legacySweptWorkspace(t, time.Minute)
	h.srv.stopOnce.Do(func() { close(h.srv.stopped) })

	// Act.
	h.srv.sweepIdle()

	// Assert.
	rec, ok := h.reg.Get(id)
	if !ok {
		t.Fatalf("session %s has no record after the sweep", id)
	}
	if rec.Hibernation.Cause != "" {
		t.Fatalf("the sweep hibernated session %s (cause %q) after teardown began; it must abandon its remainder so the shutdown drain is not held behind a serial walk",
			id, rec.Hibernation.Cause)
	}
}

// benchmarkDrainFleet is the production fleet size the 2026-08-08 SIGKILL was
// measured against, and benchmarkShimExit models the wait that dominates one
// session's hibernation: SIGTERM, then the kernel releasing the workspace's
// session lock when the shim actually dies. It is a MODEL of the wait, not a
// synchronization delay — the benchmark exists to price the drain's overlap of
// those waits against the daemon's 30s stop grace.
const (
	benchmarkDrainFleet = 90
	benchmarkShimExit   = 250 * time.Millisecond
)

// BenchmarkShutdownDrain is the standing guard on the daemon's stop grace: a
// serial drain of this fleet costs fleet*exit (~23s measured), which overran the
// grace and got the daemon SIGKILLed mid-drain, skipping lease release and merge
// reconstruction. Run it with -benchtime=1x.
func BenchmarkShutdownDrain(b *testing.B) {
	for b.Loop() {
		b.StopTimer()
		h := newHarness(b)
		for i := range benchmarkDrainFleet {
			ws := fmt.Sprintf("/w%d", i)
			createSession(b, h, fmt.Sprintf(`{"cwd":%q}`, ws))
			if err := h.controller.Ensure(ws); err != nil {
				b.Fatalf("Ensure(%s): %v", ws, err)
			}
			markControllerOperational(b, h, ws)
		}
		h.spawner.stopHook = func(string) { time.Sleep(benchmarkShimExit) }
		b.StartTimer()

		if err := h.srv.hibernateAllForShutdown(sessioncontroller.StopCauseDaemonShutdown()); err != nil {
			b.Fatalf("drain: %v", err)
		}
	}
}

// A shutdown NEVER marks a record terminal, in either mode: a stopped shim's
// session is merely unwired, not dead, and the next boot must still find it.
func TestShutdownLeavesRecordsNonTerminal(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	id := createSession(t, h, `{"cwd":"/w"}`)
	if err := h.controller.Ensure("/w"); err != nil {
		t.Fatalf("Ensure: %v", err)
	}
	markControllerOperational(t, h, "/w")

	// Act.
	h.srv.ShutdownAll(true, sessioncontroller.StopCauseDaemonShutdown())

	// Assert.
	rec, ok := h.reg.Get(id)
	if !ok {
		t.Fatalf("session %s vanished from the registry across a shutdown", id)
	}
	if rec.Terminal {
		t.Fatal("shutdown marked the record terminal; the session is merely unwired, not dead")
	}
}
