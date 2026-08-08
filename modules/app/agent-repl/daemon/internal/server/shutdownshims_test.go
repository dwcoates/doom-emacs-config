package server

import (
	"slices"
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

// The drain's join on the idle sweeper is only as short as the sweep it joins,
// and a sweep is a serial walk that hibernates whatever it reaches. Once
// teardown has begun the sweep abandons its remainder — the shutdown drain
// hibernates those sessions itself.
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
