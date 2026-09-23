package feed

import (
	"errors"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/internal/feedid"
)

// A FORK'S PORTED CONVERSATION. The child's feed is the parent's conversation
// up to the fork point followed by the child's own turns, in one order.

// rowTexts renders the root feed's rows as the text a reader would see, which
// is what every ordering assertion here is about.
func (h *harness) rowTexts() []string {
	h.t.Helper()
	var out []string
	for _, row := range h.rows(feedid.Feed{Root: true}) {
		switch {
		case row.GetUserPrompt() != nil:
			for _, block := range row.GetUserPrompt().GetSuccess().GetBody().GetBlocks() {
				out = append(out, block.GetText().GetText())
			}
		case row.GetActivity().GetResponse() != nil:
			out = append(out, row.GetActivity().GetResponse().GetSuccess().GetProse().GetMarkdown())
		}
	}
	return out
}

func TestAPortedPromptIsDrawnOnTheForksFeed(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.ported = []PortedPrompt{{Turn: "parent-turn", Text: "what is 2+2", Origin: conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT}}

	// Act.
	h.replay(historyPage(&conversationv1.HistoryFloor{}))

	// Assert.
	if got := h.rowTexts(); len(got) != 1 || got[0] != "what is 2+2" {
		t.Fatalf("rows = %v, want the parent's question drawn on the fork's feed", got)
	}
}

// TestAPortedPromptStandsAboveTheStoresOwnHistory is the defect's ordering
// half: the parent's ANSWER reaches the child through the store, and the
// question it answers must stand above it.
func TestAPortedPromptStandsAboveTheStoresOwnHistory(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.ported = []PortedPrompt{{Turn: "parent-turn", Text: "what is 2+2", Origin: conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT}}

	// Act.
	h.replay(historyPage(&conversationv1.HistoryFloor{},
		frameEntry(mainAgent(), &conversationv1.AgentUpdate{
			Update: &conversationv1.AgentUpdate_Activity{
				Activity: responseSuccessActivity("unit-1", "4"),
			},
		}),
	))

	// Assert.
	got := h.rowTexts()
	if len(got) != 2 || got[0] != "what is 2+2" || got[1] != "4" {
		t.Fatalf("rows = %v, want the parent's question above the parent's answer", got)
	}
}

// TestAPortedPromptStandsAboveARowDrawnBeforeThePageArrived is the other
// ordering half: the fork's own first prompt is mirrored the moment it is
// accepted, which is BEFORE the opening page arrives, and the ported
// conversation must still come first.
func TestAPortedPromptStandsAboveARowDrawnBeforeThePageArrived(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.ported = []PortedPrompt{{Turn: "parent-turn", Text: "what is 2+2", Origin: conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT}}
	h.deliverPrompt("fork-turn", "and what is 3+3")

	// Act.
	h.replay(historyPage(&conversationv1.HistoryFloor{}))

	// Assert.
	got := h.rowTexts()
	if len(got) != 2 || got[0] != "what is 2+2" || got[1] != "and what is 3+3" {
		t.Fatalf("rows = %v, want the ported conversation above the fork's own prompt", got)
	}
}

// TestThePortedConversationKeepsItsOwnOrder covers the ported rows against one
// another: they are drawn oldest first, as the record hands them over.
func TestThePortedConversationKeepsItsOwnOrder(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.ported = []PortedPrompt{
		{Turn: "parent-turn-1", Text: "first", Origin: conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT},
		{Turn: "parent-turn-2", Text: "second", Origin: conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT},
	}

	// Act.
	h.replay(historyPage(&conversationv1.HistoryFloor{}))

	// Assert.
	got := h.rowTexts()
	if len(got) != 2 || got[0] != "first" || got[1] != "second" {
		t.Fatalf("rows = %v, want the ported conversation in its own order", got)
	}
}

// TestThePortedConversationIsDrawnOnce covers a second page: the conversation
// is history, so a re-opened watch redraws nothing.
func TestThePortedConversationIsDrawnOnce(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.ported = []PortedPrompt{{Turn: "parent-turn", Text: "what is 2+2", Origin: conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT}}
	h.replay(historyPage(&conversationv1.HistoryFloor{}))

	// Act.
	h.ported = []PortedPrompt{{Turn: "parent-turn", Text: "REDRAWN", Origin: conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT}}
	h.replay(historyPage(&conversationv1.HistoryFloor{}))

	// Assert.
	if got := h.rowTexts(); len(got) != 1 || got[0] != "what is 2+2" {
		t.Fatalf("rows = %v, want the ported conversation drawn once", got)
	}
}

// TestAPortedPromptOpensNoTurn covers the one thing a ported prompt must NOT
// do: a question settled in another workspace is not the turn this session is
// running.
func TestAPortedPromptOpensNoTurn(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.ported = []PortedPrompt{{Turn: "parent-turn", Text: "what is 2+2", Origin: conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT}}

	// Act.
	h.replay(historyPage(&conversationv1.HistoryFloor{}))

	// Assert.
	h.resolver.mu.Lock()
	defer h.resolver.mu.Unlock()
	if turn := h.resolver.state(testWorkspace).turnInFlight; turn != nil {
		t.Fatalf("turn in flight = %q, want none opened by a ported prompt", *turn)
	}
}

// TestAnUnreadablePortedConversationStillDrawsThePage covers the read failure:
// it is recorded, and the history the store carries is served either way.
func TestAnUnreadablePortedConversationStillDrawsThePage(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.portedErr = errors.New("the state database is unreadable")

	// Act.
	h.replay(historyPage(&conversationv1.HistoryFloor{},
		frameEntry(mainAgent(), &conversationv1.AgentUpdate{
			Update: &conversationv1.AgentUpdate_Activity{
				Activity: responseSuccessActivity("unit-1", "4"),
			},
		}),
	))

	// Assert.
	if got := h.rowTexts(); len(got) != 1 || got[0] != "4" {
		t.Fatalf("rows = %v, want the store's own history drawn despite the failed read", got)
	}
}

// TestAnUnreadablePortedConversationIsRecorded covers the surfacing: a read
// that failed is never swallowed.
func TestAnUnreadablePortedConversationIsRecorded(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.portedErr = errors.New("the state database is unreadable")

	// Act.
	h.replay(historyPage(&conversationv1.HistoryFloor{}))

	// Assert.
	recorded := false
	for _, record := range h.log.Records() {
		if record.Operation == "daemon.feed.ported_prompts_unreadable" {
			recorded = true
		}
	}
	if !recorded {
		t.Fatalf("records = %+v, want the failed ported-conversation read recorded", h.log.Records())
	}
}

// A PORTED PROMPT'S TURN ENDED IN THE PARENT: no terminal for it will ever
// reach the fork, so it is drawn settled.
func TestAPortedPromptIsNotWorking(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.ported = []PortedPrompt{{Turn: "parent-turn", Text: "what is 2+2", Origin: conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT}}

	// Act.
	h.replay(historyPage(&conversationv1.HistoryFloor{}))

	// Assert.
	if h.promptWorking("parent-turn") {
		t.Fatal("a ported prompt is working")
	}
}
