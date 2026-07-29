package server

import (
	"testing"
	"time"

	corev1 "agentrepl/proto/agentshim/core/v1"
)

// The idle sweeper's gate. `!turn_active` alone is satisfied the instant a turn
// ends, so it is the ELAPSED-QUIET gate below that makes the configured window
// mean anything.

// sweptWorkspace is a registered session on workspace "/w", plus a setter that
// puts the server's clock a given distance past the workspace's newest state
// row. Dating off the real row avoids reaching into the SSM's own clock.
func sweptWorkspace(t *testing.T, window time.Duration) (*harness, string, func(time.Duration)) {
	t.Helper()
	h := newHarnessWith(t, Config{IdleTimeout: window})
	id := createSession(t, h, `{"cwd":"/w"}`)
	at, dated, err := h.ssm.LastActivityMs("/w")
	if err != nil || !dated {
		t.Fatalf("LastActivityMs(/w) = (%d, %t, %v), want the workspace dated", at, dated, err)
	}
	return h, id, func(since time.Duration) {
		h.srv.now = func() time.Time { return time.UnixMilli(at + int64(since/time.Millisecond)) }
	}
}

func TestAWorkspaceQuietPastTheWindowIsSweepable(t *testing.T) {
	// Arrange — nothing has happened to the workspace for the full window.
	h, id, quietFor := sweptWorkspace(t, time.Hour)
	quietFor(time.Hour)

	// Act.
	got := h.srv.sweepable(id, "/w", h.srv.now().UnixMilli())

	// Assert.
	if !got {
		t.Fatal("sweepable = false, want a workspace quiet for the full window hibernated")
	}
}

func TestAWorkspaceQuietInsideTheWindowIsHeld(t *testing.T) {
	// Arrange — seven minutes of quiet, which is the case the old gate
	// hibernated on the very next tick.
	h, id, quietFor := sweptWorkspace(t, time.Hour)
	quietFor(7 * time.Minute)

	// Act.
	got := h.srv.sweepable(id, "/w", h.srv.now().UnixMilli())

	// Assert.
	if got {
		t.Fatal("sweepable = true, want a recently active workspace left alone")
	}
}

func TestAWorkspaceWithNoStateAtAllIsHeld(t *testing.T) {
	// Arrange — nothing has ever been recorded about this workspace, which is
	// the bring-up-in-flight case that used to fall straight through to
	// Hibernate.
	h, id, quietFor := sweptWorkspace(t, time.Hour)
	quietFor(24 * time.Hour)

	// Act.
	got := h.srv.sweepable(id, "/never-seen", h.srv.now().UnixMilli())

	// Assert.
	if got {
		t.Fatal("sweepable = true, want an unknown workspace held rather than reaped")
	}
}

func TestATurnActiveWorkspaceIsHeldHoweverOldItsLogIs(t *testing.T) {
	// Arrange — a live turn, with the clock long past the window. The elapsed
	// gate must never be able to override the turn gate.
	h, id, quietFor := sweptWorkspace(t, time.Hour)
	if err := h.ssm.Apply(&corev1.Event{
		SessionId: id, Seq: 1,
		Payload: &corev1.Event_TurnStarted{TurnStarted: &corev1.TurnStarted{PromptPreview: "go"}},
	}); err != nil {
		t.Fatalf("apply turn started: %v", err)
	}
	quietFor(24 * time.Hour)

	// Act.
	got := h.srv.sweepable(id, "/w", h.srv.now().UnixMilli())

	// Assert.
	if got {
		t.Fatal("sweepable = true, want a turn-active workspace never hibernated")
	}
}
