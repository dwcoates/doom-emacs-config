package feed

import (
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/ids"
)

// completedWith is the success terminal that names its answering response unit —
// the frame that draws the green final-answer border on that row.
func completedWith(unit string) *conversationv1.AgentSuccess {
	return &conversationv1.AgentSuccess{
		Outcome: &conversationv1.AgentSuccess_Completed{Completed: &conversationv1.AgentCompleted{
			Answer: &conversationv1.AgentActivityId{Value: unit},
		}},
	}
}

// concludeAnswer draws one settled response and concludes its turn on it, so
// the row becomes a selectable final response.
func (h *harness) concludeAnswer(turn, unit, markdown string) {
	h.t.Helper()
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		responseFrame(unit, &conversationv1.AgentResponseSuccess{
			Prose: &conversationv1.AgentResponseProse{Markdown: markdown},
		}, nil), nil, noAddress())
	id := ids.TurnID(turn)
	h.resolver.OnAgentTerminal(testWorkspace, mainAgent(), &id, completedWith(unit), nil, noAddress())
}

// TestFinalResponsesAreOrderedOldestFirst pins that concluded answers accrue in
// conclusion order, which is root-feed order, so the last element is the most
// recent.
func TestFinalResponsesAreOrderedOldestFirst(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	h.concludeAnswer("turn-1", "unit-1", "first answer")
	h.concludeAnswer("turn-2", "unit-2", "second answer")

	// Assert.
	got := h.resolver.FinalResponses(testWorkspace)
	if len(got) != 2 {
		t.Fatalf("final responses = %d, want 2", len(got))
	}
	first := got[0].GetValue()
	last := got[1].GetValue()
	if md, ok := h.resolver.ResponseMarkdown(testWorkspace, got[0]); !ok || md != "first answer" {
		t.Fatalf("markdown[%s] = (%q, %v), want (\"first answer\", true)", first, md, ok)
	}
	if md, ok := h.resolver.ResponseMarkdown(testWorkspace, got[1]); !ok || md != "second answer" {
		t.Fatalf("markdown[%s] = (%q, %v), want (\"second answer\", true)", last, md, ok)
	}
}

// TestFinalResponsesAreDedupedAcrossReplayedTerminals pins that a terminal
// replayed across planes appends its answer ONCE.
func TestFinalResponsesAreDedupedAcrossReplayedTerminals(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.concludeAnswer("turn-1", "unit-1", "the answer")

	// Act: the same terminal arrives again (the file plane after the stream).
	id := ids.TurnID("turn-1")
	h.resolver.OnAgentTerminal(testWorkspace, mainAgent(), &id, completedWith("unit-1"), nil, noAddress())

	// Assert.
	if got := h.resolver.FinalResponses(testWorkspace); len(got) != 1 {
		t.Fatalf("final responses = %d, want 1 after a replayed terminal", len(got))
	}
}

// TestFinalResponsesEmptyForAWorkspaceWithNoConclusions pins that a workspace
// with no concluded answers answers an empty set, never an error.
func TestFinalResponsesEmptyForAWorkspaceWithNoConclusions(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	got := h.resolver.FinalResponses(testWorkspace)

	// Assert.
	if len(got) != 0 {
		t.Fatalf("final responses = %d, want none", len(got))
	}
}

// TestResponseMarkdownMissesAnUnselectableFeedid pins that a feedid naming no
// final-response row answers not-selectable, which is the submit path's cue to
// refuse rather than deliver an empty prefix.
func TestResponseMarkdownMissesAnUnselectableFeedid(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.concludeAnswer("turn-1", "unit-1", "the answer")

	// Act.
	_, ok := h.resolver.ResponseMarkdown(testWorkspace, &frontendv1.FeedId{Value: "not-a-final"})

	// Assert.
	if ok {
		t.Fatalf("ResponseMarkdown reported selectable for a feedid that names no final response")
	}
}

// TestConcludingATurnStampsItsAnswerRowFinal pins the LIVE green: when a turn
// concludes on an answering response, that response ROW carries
// final_answer=true, so the client draws the green border from the row's data
// rather than from a live turn-ended event. The response is drawn before the
// terminal names it, so this proves the terminal re-stamps the already-drawn
// row.
func TestConcludingATurnStampsItsAnswerRowFinal(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.concludeAnswer("turn-1", "unit-1", "the answer")

	// Assert.
	rows := h.responseRows()
	if len(rows) != 1 {
		t.Fatalf("response rows = %d, want 1", len(rows))
	}
	if !rows[0].GetActivity().GetResponse().GetFinalAnswer() {
		t.Fatal("the concluded answer row was not stamped final_answer=true")
	}
}

// TestABackgroundedConclusionStampsNoAnswerRow pins that a turn concluding with
// NO answering response (backgrounded) leaves every response row unstamped —
// the flag names exactly the row the conclusion pointed at.
func TestABackgroundedConclusionStampsNoAnswerRow(t *testing.T) {
	// Arrange: an ordinary response, then a conclusion that names no answer.
	h := newHarness(t)
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		responseFrame("unit-1", &conversationv1.AgentResponseSuccess{
			Prose: &conversationv1.AgentResponseProse{Markdown: "some prose"},
		}, nil), nil, noAddress())
	id := ids.TurnID("turn-1")

	// Act: conclude with backgrounded (no answer unit).
	h.resolver.OnAgentTerminal(testWorkspace, mainAgent(), &id, &conversationv1.AgentSuccess{
		Outcome: &conversationv1.AgentSuccess_Backgrounded{Backgrounded: &conversationv1.AgentBackgrounded{}},
	}, nil, noAddress())

	// Assert.
	rows := h.responseRows()
	if len(rows) != 1 {
		t.Fatalf("response rows = %d, want 1", len(rows))
	}
	if rows[0].GetActivity().GetResponse().GetFinalAnswer() {
		t.Fatal("a response was stamped final_answer with no conclusion naming it")
	}
}
