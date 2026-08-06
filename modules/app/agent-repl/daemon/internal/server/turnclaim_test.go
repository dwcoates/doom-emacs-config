package server

import (
	"testing"

	corev1 "agentrepl/proto/agentshim/core/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// A DELETE MID-TURN LEAVES NOBODY TO END THE TURN. The shim is stopped, so the
// TurnEnded that would supersede the workspace's `thinking` row is never
// produced — and the row went on suppressing the readiness of every later
// session on the same workspace, which is how "readiness suppressed (turn in
// flight)" survived a delete-and-recreate.
func TestDeleteSessionReleasesTheWorkspacesTurnClaim(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	id := createSession(t, h, `{"cwd":"/w","model":"m"}`)
	turn := &corev1.Event{
		SessionId: id,
		Seq:       2,
		Plane:     corev1.Plane_PLANE_STREAM,
		RequestId: "turn-1",
		Payload:   &corev1.Event_TurnStarted{TurnStarted: &corev1.TurnStarted{TurnId: "turn-1"}},
	}
	markControllerOperational(t, h, "/w")
	if _, err := h.ssm.ApplyTurnBoundary("/w", id, "", turn); err != nil {
		t.Fatalf("turn started: %v", err)
	}
	cur, found, err := h.ssm.Current("/w")
	if err != nil || !found {
		t.Fatalf("Current: found=%v err=%v", found, err)
	}
	if cur.GetState() != frontendv1.RenderState_RENDER_STATE_THINKING {
		t.Fatalf("precondition state = %s, want THINKING", cur.GetState())
	}

	// Act.
	if err := h.srv.DeleteSession(id); err != nil {
		t.Fatalf("DeleteSession: %v", err)
	}

	// Assert.
	cur, found, err = h.ssm.Current("/w")
	if err != nil || !found {
		t.Fatalf("Current after delete: found=%v err=%v", found, err)
	}
	if cur.GetState() == frontendv1.RenderState_RENDER_STATE_THINKING {
		t.Fatal("workspace still THINKING after its session was deleted: the turn claim outlived its claimant")
	}
}
