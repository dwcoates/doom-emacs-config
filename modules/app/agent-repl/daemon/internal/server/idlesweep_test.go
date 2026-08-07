package server

import (
	"testing"
	"time"

	corev1 "agentrepl/proto/agentshim/core/v1"

	"claude-repld/internal/registry"
)

// legacySweptWorkspace is a workspace the KEEP-ALIVE POLICY declines and the
// idle sweep's own threshold catches — the case that used to be torn down
// without a hibernation record.
//
// Two clocks have to agree for that path to be reachable, and the helper is
// where they are made to. The server's clock is moved one window past the
// workspace's newest state row, which is what makes it sweepable; the record's
// last-turn-end is put two windows into the REAL past, which is the clock the
// session controller's claim re-reads when it re-validates the elapsed. A
// last-turn-end that recent on the server's clock also keeps the keep-alive
// policy answering none, so the legacy branch is the one that runs.
func legacySweptWorkspace(t *testing.T, window time.Duration) (*harness, string) {
	t.Helper()
	h := newHarnessWith(t, Config{IdleTimeout: window})
	id := createSession(t, h, `{"cwd":"/w"}`)
	markControllerOperational(t, h, "/w")
	at, dated, err := h.ssm.LastActivityMs("/w")
	if err != nil || !dated {
		t.Fatalf("LastActivityMs(/w) = (%d, %t, %v), want the workspace dated", at, dated, err)
	}
	windowMs := int64(window / time.Millisecond)
	if _, err := h.reg.Update(id, func(r *registry.Record) {
		r.LastTurnEndMs = time.Now().UnixMilli() - 2*windowMs
	}); err != nil {
		t.Fatalf("seed last_turn_end_ms: %v", err)
	}
	h.srv.now = func() time.Time { return time.UnixMilli(at + windowMs) }
	return h, id
}

// THE SWEEP'S TEARDOWN IS THE ONE TRANSITION. It used to call Hibernate
// directly, stopping the shim while the record still said the session was
// awake — a stopped-but-awake durable state whose next prompt brought the
// session back up instead of meeting the revival gate.
func TestIdleSweepPersistsAnIdleCutoffHibernation(t *testing.T) {
	// Arrange.
	h, id := legacySweptWorkspace(t, time.Minute)

	// Act.
	h.srv.sweepIdle()

	// Assert.
	rec, ok := h.reg.Get(id)
	if !ok {
		t.Fatalf("session %s has no record after the sweep", id)
	}
	if rec.Hibernation.Cause != registry.HibernationCauseIdleCutoff {
		t.Fatalf("hibernation cause = %q, want %q; the sweep's teardown must be durable",
			rec.Hibernation.Cause, registry.HibernationCauseIdleCutoff)
	}
}

// The account carries the threshold that actually tripped — this daemon's own
// idle timeout — rather than the keep-alive policy's cutoff, which is not what
// this teardown was decided against.
func TestIdleSweepRecordsItsOwnCutoff(t *testing.T) {
	// Arrange.
	h, id := legacySweptWorkspace(t, time.Minute)

	// Act.
	h.srv.sweepIdle()

	// Assert.
	rec, _ := h.reg.Get(id)
	if rec.Hibernation.CutoffMs != int64(time.Minute/time.Millisecond) {
		t.Fatalf("hibernation cutoff_ms = %d, want the sweep's own one-minute timeout", rec.Hibernation.CutoffMs)
	}
}

// A LEGACY RECORD IS STAMPED FROM DURABLE STATE. Without a last-turn-end the
// policy's "every unknown answers none" rule leaves the session outside the
// keep-alive loop forever.
func TestIdleSweepStampsALegacyRecordsLastTurnEnd(t *testing.T) {
	// Arrange — a record whose last-turn-end was never written, as every
	// pre-branch record's is.
	h, id, quietFor := sweptWorkspace(t, time.Hour)
	atMs, _, err := h.ssm.LastActivityMs("/w")
	if err != nil {
		t.Fatalf("LastActivityMs: %v", err)
	}
	quietFor(time.Hour)

	// Act.
	h.srv.sweepIdle()

	// Assert.
	rec, _ := h.reg.Get(id)
	if rec.LastTurnEndMs != atMs {
		t.Fatalf("last_turn_end_ms = %d, want the workspace's dated activity %d; stamping now() would make a long-quiet session look permanently fresh",
			rec.LastTurnEndMs, atMs)
	}
}

// A record that ALREADY has a last-turn-end is left alone: the stamp is a
// backfill for records that predate the policy, not a per-sweep write that
// would keep resetting a session's measured idleness.
func TestIdleSweepDoesNotRestampARecordThatHasATurnEnd(t *testing.T) {
	// Arrange.
	h, id, quietFor := sweptWorkspace(t, time.Hour)
	const recorded = int64(1_234_567)
	if _, err := h.reg.Update(id, func(r *registry.Record) { r.LastTurnEndMs = recorded }); err != nil {
		t.Fatalf("seed last_turn_end_ms: %v", err)
	}
	quietFor(time.Hour)

	// Act.
	h.srv.sweepIdle()

	// Assert.
	rec, _ := h.reg.Get(id)
	if rec.LastTurnEndMs != recorded {
		t.Fatalf("last_turn_end_ms = %d, want the recorded %d left untouched", rec.LastTurnEndMs, recorded)
	}
}

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
	markControllerOperational(t, h, "/w")
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
	_, got := h.srv.sweepable(id, "/w", h.srv.now().UnixMilli())

	// Assert.
	if !got {
		state, found, stateErr := h.ssm.Current("/w")
		activity, dated, activityErr := h.ssm.LastActivityMs("/w")
		t.Fatalf("sweepable = false, want a workspace quiet for the full window hibernated; state=%+v found=%t state_err=%v activity=%d dated=%t activity_err=%v now=%d",
			state, found, stateErr, activity, dated, activityErr, h.srv.now().UnixMilli())
	}
}

func TestAWorkspaceQuietInsideTheWindowIsHeld(t *testing.T) {
	// Arrange — seven minutes of quiet, which is the case the old gate
	// hibernated on the very next tick.
	h, id, quietFor := sweptWorkspace(t, time.Hour)
	quietFor(7 * time.Minute)

	// Act.
	_, got := h.srv.sweepable(id, "/w", h.srv.now().UnixMilli())

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
	_, got := h.srv.sweepable(id, "/never-seen", h.srv.now().UnixMilli())

	// Assert.
	if got {
		t.Fatal("sweepable = true, want an unknown workspace held rather than reaped")
	}
}

func TestATurnActiveWorkspaceIsHeldHoweverOldItsLogIs(t *testing.T) {
	// Arrange — a live turn, with the clock long past the window. The elapsed
	// gate must never be able to override the turn gate.
	h, id, quietFor := sweptWorkspace(t, time.Hour)
	if _, err := h.ssm.ApplyTurnBoundary("/w", id, "", &corev1.Event{
		SessionId: id, Seq: 2,
		Plane:     corev1.Plane_PLANE_STREAM,
		RequestId: "turn-1",
		Payload: &corev1.Event_TurnStarted{TurnStarted: &corev1.TurnStarted{
			PromptPreview: "go",
			TurnId:        "turn-1",
		}},
	}); err != nil {
		t.Fatalf("apply turn started: %v", err)
	}
	quietFor(24 * time.Hour)

	// Act.
	_, got := h.srv.sweepable(id, "/w", h.srv.now().UnixMilli())

	// Assert.
	if got {
		t.Fatal("sweepable = true, want a turn-active workspace never hibernated")
	}
}
