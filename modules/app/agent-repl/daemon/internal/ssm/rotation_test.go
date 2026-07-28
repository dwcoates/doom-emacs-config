package ssm

import (
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// ---------------------------------------------------------------------------
// ApplySessionRotated — the agent axis across a VENDOR SESSION UUID ROTATION.
//
// The vendor retires one transcript identity mid-stream and mints another (a
// `/clear` does exactly this). The turn that was running can never report its
// end under the retired identity, so the `thinking` row held for it has nothing
// arriving to supersede it — the workspace would sit red forever. This is the
// reconciliation that unsticks it, without inventing a conclusion the vendor
// never reported.
// ---------------------------------------------------------------------------

func TestRotationResolvesARunningTurnOutOfThinking(t *testing.T) {
	// Arrange — a turn in flight when the uuid rotated.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	if err := m.Apply(evTurnStarted("s1", 1)); err != nil {
		t.Fatalf("turn started: %v", err)
	}
	if got := mustCurrent(t, m, "ws1").State; got != frontendv1.RenderState_RENDER_STATE_THINKING {
		t.Fatalf("precondition state = %s, want THINKING", renderName(got))
	}

	// Act.
	if err := m.ApplySessionRotated("ws1", "uuid-old", "uuid-new"); err != nil {
		t.Fatalf("ApplySessionRotated: %v", err)
	}

	// Assert.
	if got := mustCurrent(t, m, "ws1").State; got != frontendv1.RenderState_RENDER_STATE_IDLE {
		t.Fatalf("state = %s, want IDLE: nothing is running under the retired identity", renderName(got))
	}
}

func TestRotationReportsIdleRatherThanDone(t *testing.T) {
	// Arrange — `done` would claim the turn COMPLETED, which no vendor message
	// ever reported. The distinction is the point of the row.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	if err := m.Apply(evTurnStarted("s1", 1)); err != nil {
		t.Fatalf("turn started: %v", err)
	}

	// Act.
	if err := m.ApplySessionRotated("ws1", "uuid-old", "uuid-new"); err != nil {
		t.Fatalf("ApplySessionRotated: %v", err)
	}

	// Assert.
	if got := mustCurrent(t, m, "ws1").CauseKind; got != causeSessionRotated+":uuid-new" {
		t.Fatalf("cause_kind = %q, want %q", got, causeSessionRotated+":uuid-new")
	}
}

func TestRotationLeavesASettledAgentAxisAlone(t *testing.T) {
	// Arrange — no turn in flight. There is nothing stuck to unstick, and
	// appending `idle` over `done` would discard a more specific true fact.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	if err := m.Apply(evTurnStarted("s1", 1)); err != nil {
		t.Fatalf("turn started: %v", err)
	}
	if err := m.Apply(evTurnEnded("s1", 2, false)); err != nil {
		t.Fatalf("turn ended: %v", err)
	}

	// Act.
	if err := m.ApplySessionRotated("ws1", "uuid-old", "uuid-new"); err != nil {
		t.Fatalf("ApplySessionRotated: %v", err)
	}

	// Assert.
	if got := mustCurrent(t, m, "ws1").State; got != frontendv1.RenderState_RENDER_STATE_DONE {
		t.Fatalf("state = %s, want DONE preserved", renderName(got))
	}
}

func TestRotationDropsAStandingInterruptMark(t *testing.T) {
	// Arrange — a user-commanded stop marked the running turn, and then the
	// uuid rotated. That turn's end belongs to the retired identity, so the
	// mark could only ever be spent by a LATER turn that received no stop.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1", "s2": "ws1"})
	if err := m.Apply(evTurnStarted("s1", 1)); err != nil {
		t.Fatalf("turn started: %v", err)
	}
	if err := m.MarkTurnInterrupted("ws1"); err != nil {
		t.Fatalf("mark: %v", err)
	}
	if err := m.ApplySessionRotated("ws1", "uuid-old", "uuid-new"); err != nil {
		t.Fatalf("ApplySessionRotated: %v", err)
	}

	// Act — a genuinely new turn under the new identity, run to completion.
	if err := m.Apply(evTurnStarted("s2", 1)); err != nil {
		t.Fatalf("new turn started: %v", err)
	}
	if err := m.Apply(evTurnEnded("s2", 2, false)); err != nil {
		t.Fatalf("new turn ended: %v", err)
	}

	// Assert.
	if got := mustCurrent(t, m, "ws1").State; got != frontendv1.RenderState_RENDER_STATE_DONE {
		t.Fatalf("state = %s, want DONE: a dropped mark must not paint a later turn `interrupted`", renderName(got))
	}
}

func TestRotationAnnouncesTheDroppedMarkLoudly(t *testing.T) {
	// Arrange — silently discarding a user's stop is the failure mode; the log
	// line is the only account of why the stop stopped mattering.
	m, cl, _ := openTest(t, fakeResolver{"s1": "ws1"})
	if err := m.Apply(evTurnStarted("s1", 1)); err != nil {
		t.Fatalf("turn started: %v", err)
	}
	if err := m.MarkTurnInterrupted("ws1"); err != nil {
		t.Fatalf("mark: %v", err)
	}

	// Act.
	if err := m.ApplySessionRotated("ws1", "uuid-old", "uuid-new"); err != nil {
		t.Fatalf("ApplySessionRotated: %v", err)
	}

	// Assert.
	if !cl.contains("interrupt mark DROPPED as stale") {
		t.Fatal("the dropped interrupt mark passed SILENTLY")
	}
}

func TestRotationRejectsAnEmptyRotatedSessionID(t *testing.T) {
	// Arrange — "rotated to nothing" is not a fact; it is a caller bug, and a
	// silently-accepted one would reconcile a turn away for no stated reason.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})

	// Act.
	err := m.ApplySessionRotated("ws1", "uuid-old", "")

	// Assert.
	if err == nil {
		t.Fatal("ApplySessionRotated accepted an empty rotated session id")
	}
}

func TestRotationRejectsAnEmptyWorkspace(t *testing.T) {
	// Arrange — every row is workspace-keyed; an empty one would bind state to
	// the empty-string workspace.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})

	// Act.
	err := m.ApplySessionRotated("", "uuid-old", "uuid-new")

	// Assert.
	if err == nil {
		t.Fatal("ApplySessionRotated accepted an empty workspace")
	}
}
