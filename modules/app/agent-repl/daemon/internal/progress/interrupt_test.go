package progress

import (
	"testing"

	corev1 "agentrepl/proto/agentshim/core/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// --- the interrupt window (I1) ---------------------------------------------

// The window opens the moment the ack lands, carrying the shim's verdict
// verbatim. All three outcomes open it: two of them move no workspace phase,
// so the window is the only place they are reported at all.
func TestInterruptWindowOpensCarryingTheAckOutcome(t *testing.T) {
	tests := []struct {
		name    string
		outcome corev1.InterruptOutcome
	}{
		{"a live turn was stopped", corev1.InterruptOutcome_INTERRUPT_OUTCOME_INTERRUPTED},
		{"the turn had already ended", corev1.InterruptOutcome_INTERRUPT_OUTCOME_ALREADY_COMPLETE},
		{"the stop could not be delivered", corev1.InterruptOutcome_INTERRUPT_OUTCOME_FAILED},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			// Act.
			h.m.NoteInterrupt(testWS, testSID, tc.outcome)
			// Assert.
			got := h.last().GetInterrupt()
			if !got.GetActive() || got.GetOutcome() != tc.outcome || got.GetSinceMs() != atMs {
				t.Fatalf("interrupt window = %+v, want active with outcome %s since %d", got, tc.outcome, atMs)
			}
		})
	}
}

// The next TURN START clears it — the one and only thing that does.
func TestInterruptWindowClearsOnTheNextTurnStart(t *testing.T) {
	// Arrange — a stop landed and the window is open.
	h := newHarness(t)
	h.m.NoteInterrupt(testWS, testSID, corev1.InterruptOutcome_INTERRUPT_OUTCOME_INTERRUPTED)
	h.drain()
	// Act.
	h.m.NoteTurnAccepted(testWS, testSID)
	// Assert.
	if got := h.last().GetInterrupt(); got.GetActive() {
		t.Fatalf("interrupt window = %+v, want cleared by the next turn start", got)
	}
}

// The window survives the turn's own END. The turn ending is the CONSEQUENCE
// of the stop, so clearing there would erase the report at the instant it
// became true.
func TestInterruptWindowSurvivesTheStoppedTurnsEnd(t *testing.T) {
	// Arrange — a turn is running and the user's stop lands.
	h := newHarness(t)
	h.openTurn()
	h.m.NoteInterrupt(testWS, testSID, corev1.InterruptOutcome_INTERRUPT_OUTCOME_INTERRUPTED)
	h.drain()
	// Act — the turn the stop ended reports its end.
	h.apply(&corev1.Event{
		SessionId:    testSID,
		ProducedAtMs: atMs,
		Payload:      &corev1.Event_TurnEnded{TurnEnded: &corev1.TurnEnded{StopReason: "aborted"}},
	})
	// Assert.
	if got := h.last().GetInterrupt(); !got.GetActive() {
		t.Fatalf("interrupt window = %+v, want still open after the stopped turn ended", got)
	}
}

// A second stop replaces the verdict rather than accumulating: the newest ack
// is the current answer.
func TestASecondInterruptReplacesTheOutcome(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.m.NoteInterrupt(testWS, testSID, corev1.InterruptOutcome_INTERRUPT_OUTCOME_INTERRUPTED)
	h.drain()
	// Act.
	h.m.NoteInterrupt(testWS, testSID, corev1.InterruptOutcome_INTERRUPT_OUTCOME_ALREADY_COMPLETE)
	// Assert.
	if got := h.last().GetInterrupt().GetOutcome(); got != corev1.InterruptOutcome_INTERRUPT_OUTCOME_ALREADY_COMPLETE {
		t.Fatalf("outcome = %s, want the newest ack's ALREADY_COMPLETE", got)
	}
}

// A workspace-less call is loud and changes nothing: the view is keyed by
// workspace, so there is nowhere honest to file it.
func TestNoteInterruptWithoutAWorkspaceIsIgnored(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	// Act.
	h.m.NoteInterrupt("", testSID, corev1.InterruptOutcome_INTERRUPT_OUTCOME_INTERRUPTED)
	// Assert.
	if got := h.drain(); len(got) != 0 {
		t.Fatalf("pushed %d view(s), want none", len(got))
	}
}

// --- the live-task read the confirm gate uses -------------------------------

// LiveTasks reports the count this resolver adopted off the SSM's state, so
// the interrupt confirm gate reads the same figure the footer renders rather
// than deriving a second one.
func TestLiveTasksReportsTheAdoptedCount(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	state := &frontendv1.WorkspaceState{Workspace: testWS, SessionId: testSID, LiveTaskCount: 3}
	if err := h.m.ObserveWorkspaceState(state); err != nil {
		t.Fatalf("ObserveWorkspaceState: %v", err)
	}
	// Act.
	got, ok := h.m.LiveTasks(testWS)
	// Assert.
	if !ok || got != 3 {
		t.Fatalf("LiveTasks = (%d, %v), want (3, true)", got, ok)
	}
}

// An unknown workspace is an explicit MISS, never a zero-valued answer: the
// gate must be able to tell "no live tasks" from "nobody has said".
func TestLiveTasksReportsAnUnknownWorkspaceAsAMiss(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	// Act.
	got, ok := h.m.LiveTasks("ws-never-seen")
	// Assert.
	if ok || got != 0 {
		t.Fatalf("LiveTasks = (%d, %v), want (0, false)", got, ok)
	}
}
