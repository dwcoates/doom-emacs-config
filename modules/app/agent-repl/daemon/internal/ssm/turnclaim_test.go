package ssm

import (
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// ---------------------------------------------------------------------------
// A TURN CLAIM DIES WITH ITS SESSION.
//
// `thinking` is a promise that the session which wrote it will report the
// turn's end. The no-regress guard treats that promise as the stronger claim
// and drops readiness arriving over it — which is right for the session that
// made it and wrong for every other one, because a session that is gone can
// never keep the promise. These cover both halves plus the delete path, whose
// stopped shim is exactly the "nobody will ever end this turn" case.
// ---------------------------------------------------------------------------

func TestReadinessSuppressedWhenTheSameSessionReattaches(t *testing.T) {
	// Arrange — s1 owns a running turn.
	m, cl, _ := openTest(t, fakeResolver{"s1": "ws1"})
	if err := m.Apply(evTurnStarted("s1", 1)); err != nil {
		t.Fatalf("turn started: %v", err)
	}

	// Act — the SAME session re-announces readiness (a reattach).
	if err := m.Apply(evSessionStarted("s1", 2)); err != nil {
		t.Fatalf("session started: %v", err)
	}

	// Assert.
	if got := mustCurrent(t, m, "ws1").State; got != frontendv1.RenderState_RENDER_STATE_THINKING {
		t.Fatalf("state = %s, want THINKING: the session's own turn is still the stronger claim", renderName(got))
	}
	if !cl.contains("readiness suppressed (turn in flight)") {
		t.Fatalf("no suppression logged; lines=%v", cl.lines)
	}
}

func TestReadinessInvalidatesAPriorSessionsTurnClaim(t *testing.T) {
	// Arrange — s1 leaves a `thinking` row behind and never ends its turn.
	m, cl, _ := openTest(t, fakeResolver{"s1": "ws1", "s2": "ws1"})
	if err := m.Apply(evTurnStarted("s1", 1)); err != nil {
		t.Fatalf("turn started: %v", err)
	}

	// Act — a NEW session now drives the workspace and reports ready.
	if err := m.Apply(evSessionStarted("s2", 1)); err != nil {
		t.Fatalf("session started: %v", err)
	}

	// Assert.
	if got := mustCurrent(t, m, "ws1").State; got != frontendv1.RenderState_RENDER_STATE_READY {
		t.Fatalf("state = %s, want READY: s2's readiness supersedes a claim s1 can never end", renderName(got))
	}
	if !cl.contains("turn claim INVALIDATED") {
		t.Fatalf("no invalidation logged; lines=%v", cl.lines)
	}
}

func TestReadinessStillDefersToAnUnattributedTurnClaim(t *testing.T) {
	// Arrange — the `thinking` a permission close restores carries NO session
	// id. It describes the workspace's own turn, so there is no rival identity
	// in it and it must keep suppressing exactly as it always has.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	if err := m.Apply(evTurnStarted("s1", 1)); err != nil {
		t.Fatalf("turn started: %v", err)
	}
	if err := m.ApplyPermission("ws1", true, "asked"); err != nil {
		t.Fatalf("permission open: %v", err)
	}
	if err := m.ApplyPermission("ws1", false, "answered"); err != nil {
		t.Fatalf("permission close: %v", err)
	}

	// Act.
	if err := m.Apply(evSessionStarted("s1", 2)); err != nil {
		t.Fatalf("session started: %v", err)
	}

	// Assert.
	if got := mustCurrent(t, m, "ws1").State; got != frontendv1.RenderState_RENDER_STATE_THINKING {
		t.Fatalf("state = %s, want THINKING preserved", renderName(got))
	}
}

func TestInvalidateTurnClaimReleasesTheDeletedSessionsTurn(t *testing.T) {
	// Arrange.
	m, cl, _ := openTest(t, fakeResolver{"s1": "ws1"})
	if err := m.Apply(evTurnStarted("s1", 1)); err != nil {
		t.Fatalf("turn started: %v", err)
	}

	// Act.
	if err := m.InvalidateTurnClaim("ws1", "s1", "session_deleted"); err != nil {
		t.Fatalf("InvalidateTurnClaim: %v", err)
	}

	// Assert.
	if got := mustCurrent(t, m, "ws1").State; got != frontendv1.RenderState_RENDER_STATE_IDLE {
		t.Fatalf("state = %s, want IDLE", renderName(got))
	}
	if !cl.contains("turn claim INVALIDATED") {
		t.Fatalf("no invalidation logged; lines=%v", cl.lines)
	}
}

func TestInvalidateTurnClaimLeavesASettledAgentAxisAlone(t *testing.T) {
	// Arrange — the turn already ended, so there is nothing stuck to unstick
	// and `idle` would discard a more specific true fact.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	if err := m.Apply(evTurnStarted("s1", 1)); err != nil {
		t.Fatalf("turn started: %v", err)
	}
	if err := m.Apply(evTurnEnded("s1", 2, false)); err != nil {
		t.Fatalf("turn ended: %v", err)
	}

	// Act.
	if err := m.InvalidateTurnClaim("ws1", "s1", "session_deleted"); err != nil {
		t.Fatalf("InvalidateTurnClaim: %v", err)
	}

	// Assert.
	if got := mustCurrent(t, m, "ws1").State; got != frontendv1.RenderState_RENDER_STATE_DONE {
		t.Fatalf("state = %s, want DONE preserved", renderName(got))
	}
}

func TestInvalidateTurnClaimDeclinesAnotherSessionsClaim(t *testing.T) {
	// Arrange — s2 holds the running turn; deleting s1 is not s2's claim to
	// spend.
	m, cl, _ := openTest(t, fakeResolver{"s2": "ws1"})
	if err := m.Apply(evTurnStarted("s2", 1)); err != nil {
		t.Fatalf("turn started: %v", err)
	}

	// Act.
	if err := m.InvalidateTurnClaim("ws1", "s1", "session_deleted"); err != nil {
		t.Fatalf("InvalidateTurnClaim: %v", err)
	}

	// Assert.
	if got := mustCurrent(t, m, "ws1").State; got != frontendv1.RenderState_RENDER_STATE_THINKING {
		t.Fatalf("state = %s, want THINKING preserved", renderName(got))
	}
	if !cl.contains("DECLINED") {
		t.Fatalf("no decline logged; lines=%v", cl.lines)
	}
}

func TestInvalidateTurnClaimRejectsAnEmptySessionID(t *testing.T) {
	// Arrange.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})

	// Act.
	err := m.InvalidateTurnClaim("ws1", "", "session_deleted")

	// Assert.
	if err == nil {
		t.Fatal("InvalidateTurnClaim with no session id must be a loud error")
	}
}
