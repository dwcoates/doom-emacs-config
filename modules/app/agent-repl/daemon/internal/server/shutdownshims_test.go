package server

import (
	"slices"
	"testing"
)

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
	h.srv.ShutdownAll(false)

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

	// Act.
	h.srv.ShutdownAll(true)

	// Assert.
	if stopped := h.spawner.stoppedIDs(); !slices.Contains(stopped, id) {
		t.Fatalf("stop-shims mode stopped %v, want session %s among them", stopped, id)
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

	// Act.
	h.srv.ShutdownAll(true)

	// Assert.
	rec, ok := h.reg.Get(id)
	if !ok {
		t.Fatalf("session %s vanished from the registry across a shutdown", id)
	}
	if rec.Terminal {
		t.Fatal("shutdown marked the record terminal; the session is merely unwired, not dead")
	}
}
