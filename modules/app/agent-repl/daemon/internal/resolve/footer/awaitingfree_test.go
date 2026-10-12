package footer

import (
	"testing"

	"agent-repld/internal/ids"
)

// awaiting is a merge that holds the workspace and waits for it to fall free.
func awaiting() MergeFacts {
	return MergeFacts{State: "merging", Step: StepAwaitingFree}
}

// TestAnAwaitingMergeDrawsTheAwaitingFreeSubstatus pins the arm: a merge that
// has not started is `merging · awaiting_free`, never a step of a run.
func TestAnAwaitingMergeDrawsTheAwaitingFreeSubstatus(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)

	// Act
	h.r.SetMerge(testWS, awaiting())

	// Assert
	if got := h.view(t).GetStrip().GetStatus().GetMerging().GetAwaitingFree(); got == nil {
		t.Fatalf("merging = %+v, want the awaiting_free substatus", h.view(t).GetStrip().GetStatus().GetMerging())
	}
}

// TestAnAwaitingMergeNamesWhatItWaitsOn covers each thing the requester's
// own wait can be on: the substatus is read off the footer's own state.
func TestAnAwaitingMergeNamesWhatItWaitsOn(t *testing.T) {
	tests := []struct {
		name      string
		arrange   func(h *harness)
		turn      bool
		subagents uint32
		shells    uint32
	}{
		{name: "a turn in flight", arrange: func(h *harness) { h.r.SetTurn(testWS, &TurnStarted{At: instant}) }, turn: true},
		{name: "two subagents", arrange: func(h *harness) {
			h.r.OnLiveWorkChanged(testWS, liveSet([]string{"a-1", "a-2"}, nil, nil))
		}, subagents: 2},
		{name: "a shell", arrange: func(h *harness) {
			h.r.OnLiveWorkChanged(testWS, liveSet(nil, []string{"s-1"}, nil))
		}, shells: 1},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			connected(h)
			tt.arrange(h)

			// Act
			h.r.SetMerge(testWS, awaiting())

			// Assert
			got := h.view(t).GetStrip().GetStatus().GetMerging().GetAwaitingFree()
			if got.GetTurnInFlight() != tt.turn || got.GetSubagents() != tt.subagents || got.GetShells() != tt.shells {
				t.Fatalf("awaiting_free = %+v, want turn=%v subagents=%d shells=%d", got, tt.turn, tt.subagents, tt.shells)
			}
		})
	}
}

// TestAnAwaitingMergeFollowsItsWorkspaceFallingQuiet pins the restatement: a
// subagent concluding redraws what the merge waits on.
func TestAnAwaitingMergeFollowsItsWorkspaceFallingQuiet(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.OnLiveWorkChanged(testWS, liveSet([]string{"a-1", "a-2"}, nil, nil))
	h.r.SetMerge(testWS, awaiting())

	// Act
	h.r.OnLiveWorkChanged(testWS, liveSet([]string{"a-2"}, nil, nil))

	// Assert
	if got := h.view(t).GetStrip().GetStatus().GetMerging().GetAwaitingFree().GetSubagents(); got != 1 {
		t.Fatalf("subagents = %d, want 1", got)
	}
}

// TestAMergeAwaitingAnotherWorkspaceIsRedrawnByItsChange pins the other
// workspace's case: the requester's footer draws what THAT workspace runs,
// and a change there republishes the requester.
func TestAMergeAwaitingAnotherWorkspaceIsRedrawnByItsChange(t *testing.T) {
	// Arrange
	const other = ids.WorkspaceID("ws-other")
	h := newHarness(t)
	connected(h)
	h.r.OnLiveWorkChanged(other, liveSet(nil, []string{"s-1"}, nil))
	facts := awaiting()
	facts.AwaitOther, facts.AwaitOtherName = other, "other-branch"
	h.r.SetMerge(testWS, facts)

	// Act
	h.r.OnLiveWorkChanged(other, liveSet(nil, []string{"s-1", "s-2"}, nil))

	// Assert
	got := h.view(t).GetStrip().GetStatus().GetMerging().GetAwaitingFree()
	if got.GetOtherWorkspace() != "other-branch" || got.GetShells() != 2 {
		t.Fatalf("awaiting_free = %+v, want other-branch with 2 shells", got)
	}
}
