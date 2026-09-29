package feed

import (
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/ids"
	"claude-repld/internal/resolve/ladder"
	"claude-repld/internal/wsm"
)

// THE LIVE TURN ENDING: what a turn's end said, filed for the desktop banner
// and taken once by the prompt queue's turn end.

func TestAConcludedTurnFilesItsFinalAnswer(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	h.concludeAnswer("turn-1", "unit-1", "the whole answer")

	// Assert
	ending, ok := h.resolver.TakeTurnEnding(testWorkspace, "turn-1")
	if !ok {
		t.Fatal("no ending was filed for a concluded turn")
	}
	if ending.Answer != "the whole answer" || ending.Failure != ladder.NoFailure || ending.Error != "" {
		t.Fatalf("ending = %+v, want the answer, no failure and no error", ending)
	}
}

func TestAFailedTurnFilesItsErroredLineAndClass(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.deliverPrompt("turn-1", "hello")

	// Act
	h.terminal("turn-1", nil, maxTurnsFailure())

	// Assert
	ending, ok := h.resolver.TakeTurnEnding(testWorkspace, "turn-1")
	if !ok {
		t.Fatal("no ending was filed for a failed turn")
	}
	want := h.terminalRow("turn-1").GetErrored().GetHeadline().GetText()
	if ending.Error != want || want == "" {
		t.Fatalf("error = %q, want the drawn headline %q", ending.Error, want)
	}
	if ending.Failure != ladder.TurnFailed {
		t.Fatalf("failure = %s, want turn_failed", ending.Failure)
	}
}

func TestAnExpectedStopFilesItsClass(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.deliverPrompt("turn-1", "hello")

	// Act
	h.terminal("turn-1", nil, stopHookFailure())

	// Assert
	ending, _ := h.resolver.TakeTurnEnding(testWorkspace, "turn-1")
	if ending.Failure != ladder.ExpectedStop {
		t.Fatalf("failure = %s, want expected_stop", ending.Failure)
	}
}

func TestATurnClosedWithNoTerminalFilesItsClosedEnding(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.deliverPrompt("turn-1", "hello")

	// Act
	h.closeTurn("turn-1", wsm.CloseAgentDied)

	// Assert
	ending, ok := h.resolver.TakeTurnEnding(testWorkspace, "turn-1")
	if !ok {
		t.Fatal("no ending was filed for a turn closed with no terminal")
	}
	if !contains(ending.Error, "the agent process died") || ending.Failure != ladder.NoFailure {
		t.Fatalf("ending = %+v, want the agent-death line and no failure class", ending)
	}
}

func TestAReplayedTerminalFilesNoEnding(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.deliverPrompt("turn-1", "hello")
	h.resolver.mu.Lock()
	h.resolver.state(testWorkspace).plane = planeHistory
	h.resolver.mu.Unlock()

	// Act
	h.terminal("turn-1", nil, maxTurnsFailure())

	// Assert
	if ending, ok := h.resolver.TakeTurnEnding(testWorkspace, "turn-1"); ok {
		t.Fatalf("a replayed terminal filed %+v, want nothing", ending)
	}
}

func TestTakingATurnEndingForgetsIt(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.concludeAnswer("turn-1", "unit-1", "answer")
	h.resolver.TakeTurnEnding(testWorkspace, "turn-1")

	// Act
	_, ok := h.resolver.TakeTurnEnding(testWorkspace, "turn-1")

	// Assert
	if ok {
		t.Fatal("a taken ending was answered a second time")
	}
}

func TestTakingAnEndingOfAnUnknownWorkspaceAnswersNone(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	_, ok := h.resolver.TakeTurnEnding(ids.WorkspaceID("nowhere"), "turn-1")

	// Assert
	if ok {
		t.Fatal("an unknown workspace answered an ending")
	}
}

func TestErroredLineComposesHeadlineAndVendorSentence(t *testing.T) {
	cases := []struct {
		name    string
		errored *frontendv1.FeedTurnEndedErrored
		want    string
	}{
		{name: "not errored", errored: nil, want: ""},
		{name: "headline only", errored: &frontendv1.FeedTurnEndedErrored{Headline: &frontendv1.FeedTurnErrorHeadline{Text: "rate limited"}}, want: "rate limited"},
		{name: "headline and message", errored: &frontendv1.FeedTurnEndedErrored{
			Headline: &frontendv1.FeedTurnErrorHeadline{Text: "rate limited"},
			Message:  &frontendv1.FeedTurnErrorMessage{Text: "slow down"},
		}, want: "rate limited: slow down"},
		{name: "message repeats the headline", errored: &frontendv1.FeedTurnEndedErrored{
			Headline: &frontendv1.FeedTurnErrorHeadline{Text: "same"},
			Message:  &frontendv1.FeedTurnErrorMessage{Text: "same"},
		}, want: "same"},
		{name: "message only", errored: &frontendv1.FeedTurnEndedErrored{Message: &frontendv1.FeedTurnErrorMessage{Text: "vendor words"}}, want: "vendor words"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := erroredLine(tc.errored)

			// Assert
			if got != tc.want {
				t.Fatalf("erroredLine = %q, want %q", got, tc.want)
			}
		})
	}
}

// A confirmed /clear's divider is its whole outcome, so it files no ending.
func TestAConfirmedClearFilesNoEnding(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.resolver.mu.Lock()
	h.resolver.state(testWorkspace).clearConfirmed[ids.TurnID("turn-1")] = true
	h.resolver.mu.Unlock()
	h.deliverPrompt("turn-1", "/clear")

	// Act
	h.terminal("turn-1", &conversationv1.AgentSuccess{
		Outcome: &conversationv1.AgentSuccess_Completed{Completed: &conversationv1.AgentCompleted{}},
	}, nil)

	// Assert
	if ending, ok := h.resolver.TakeTurnEnding(testWorkspace, "turn-1"); ok {
		t.Fatalf("a confirmed /clear filed %+v, want nothing", ending)
	}
}
