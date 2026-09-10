package footer

import (
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
)

// replayedPage is a resumed session's opening catch-up page carrying one prior
// prompt — the only statement a relaunched daemon gets that a turn ever ran.
func replayedPage() *conversationv1.HistoryPage {
	return &conversationv1.HistoryPage{
		Entries: []*conversationv1.HistoryEntryAt{{
			At: &conversationv1.HistoryPointer{Value: "ptr-1"},
			Entry: &conversationv1.HistoryEntry{
				Entry: &conversationv1.HistoryEntry_UserPrompt{UserPrompt: &conversationv1.AgentPrompt{
					Id:    &conversationv1.TurnId{Value: string(testTurnID)},
					Agent: mainAgent,
				}},
			},
		}},
		Boundary: &conversationv1.HistoryPage_Floor{Floor: &conversationv1.HistoryFloor{}},
	}
}

// TestIdleIsDoneAfterAReplayedPageSaysATurnRan is the RELAUNCH's footer. The
// prior turn's open edge and terminal belonged to a daemon process that is
// gone, so without the page the strip would report a rehydrated conversation
// as one that has never run.
func TestIdleIsDoneAfterAReplayedPageSaysATurnRan(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)

	// Act
	h.r.OnHistoryPage(testWS, mainAgent, replayedPage())

	// Assert
	idle := h.view(t).GetStrip().GetStatus().GetIdle()
	if idle.GetDone() == nil {
		t.Fatalf("substatus = %+v, want done", idle.GetSubstatus())
	}
}

// TestIdleStaysReadyAfterAnEmptyReplayedPage is the FRESH conversation's
// footer: a floor page with no entries states that nothing has ever run, and
// reading it as a turn would tell every new workspace its conversation had
// already happened.
func TestIdleStaysReadyAfterAnEmptyReplayedPage(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)

	// Act
	h.r.OnHistoryPage(testWS, mainAgent, &conversationv1.HistoryPage{
		Boundary: &conversationv1.HistoryPage_Floor{Floor: &conversationv1.HistoryFloor{}},
	})

	// Assert
	idle := h.view(t).GetStrip().GetStatus().GetIdle()
	if idle.GetReady() == nil {
		t.Fatalf("substatus = %+v, want ready", idle.GetSubstatus())
	}
}
