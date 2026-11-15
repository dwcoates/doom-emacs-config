package feed

import (
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
)

// standingKeys spells AsksStanding's answer as kind:id@agent, in order.
func standingKeys(asks []*conversationv1.SessionOpenAsk) []string {
	var out []string
	for _, a := range asks {
		switch {
		case a.GetQuestion() != nil:
			out = append(out, "question:"+a.GetQuestion().GetId().GetValue()+"@"+a.GetAgent().GetValue())
		case a.GetPermission() != nil:
			out = append(out, "permission:"+a.GetPermission().GetId().GetValue()+"@"+a.GetAgent().GetValue())
		}
	}
	return out
}

func TestAsksStandingAnswersEveryOpenAskOldestFirst(t *testing.T) {
	// Arrange: a question, then a consent ask, then a settled question.
	h := newHarness(t)
	batch := twoQuestionBatch()
	h.pose("q-1", &conversationv1.AgentQuestionStart{Batch: batch})
	h.ask("p-1", "unit-1", &conversationv1.AgentPermissionStart{
		Prompt: &conversationv1.AgentPermissionPrompt{Title: "Claude wants to read foo.txt"},
	})
	h.pose("q-2", &conversationv1.AgentQuestionStart{Batch: batch})
	h.pose("q-2", &conversationv1.AgentQuestionFailure{})

	// Act.
	got := standingKeys(h.resolver.AsksStanding(testWorkspace))

	// Assert.
	want := []string{"question:q-1@" + mainAgent().GetValue(), "permission:p-1@" + mainAgent().GetValue()}
	if len(got) != len(want) || got[0] != want[0] || got[1] != want[1] {
		t.Fatalf("AsksStanding = %v, want %v", got, want)
	}
}

func TestAsksStandingRestatesTheBatchAQuestionWasServed(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	batch := twoQuestionBatch()
	h.pose("q-1", &conversationv1.AgentQuestionStart{Batch: batch})

	// Act.
	asks := h.resolver.AsksStanding(testWorkspace)

	// Assert.
	if len(asks) != 1 || len(asks[0].GetQuestion().GetStart().GetBatch().GetQuestions()) != len(batch.GetQuestions()) {
		t.Fatalf("AsksStanding = %v, want q-1 restating its served batch", asks)
	}
}

func TestAsksStandingAnswersNothingForAFeedWithNoOpenAsk(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	asks := h.resolver.AsksStanding(testWorkspace)

	// Assert.
	if len(asks) != 0 {
		t.Fatalf("AsksStanding = %v, want none", asks)
	}
}

func TestOpenAsksListsEveryOpenCardOldestFirstWithWhetherItCanBeAnswered(t *testing.T) {
	// Arrange: a consent ask, then a question whose batch a delivery claims.
	h := newHarness(t)
	h.ask("p-1", "unit-1", &conversationv1.AgentPermissionStart{
		Prompt: &conversationv1.AgentPermissionPrompt{Title: "Claude wants to read foo.txt"},
	})
	h.pose("q-1", &conversationv1.AgentQuestionStart{Batch: twoQuestionBatch()})
	h.resolver.state(testWorkspace).questionAsks["q-1"].delivering = true

	// Act
	open := openAsks(h.resolver.state(testWorkspace))

	// Assert
	if len(open) != 2 || open[0].id != "p-1" || !open[0].answerable || open[1].id != "q-1" || open[1].answerable {
		t.Fatalf("openAsks = %+v, want p-1 answerable then q-1 claimed", open)
	}
}

// TestTheAnswerRoutingAndTheReconciliationReadTheSameOpenAsks pins that both
// readers of the open asks share one rule: the oldest ask a typed answer goes
// to is the oldest ask the reconciliation is handed.
func TestTheAnswerRoutingAndTheReconciliationReadTheSameOpenAsks(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.pose("q-1", &conversationv1.AgentQuestionStart{Batch: twoQuestionBatch()})
	h.ask("p-1", "unit-1", &conversationv1.AgentPermissionStart{
		Prompt: &conversationv1.AgentPermissionPrompt{Title: "Claude wants to read foo.txt"},
	})

	// Act
	standing, found := h.resolver.StandingAsk(testWorkspace, nil)
	asks := h.resolver.AsksStanding(testWorkspace)

	// Assert
	if !found || len(asks) == 0 || asks[0].GetQuestion().GetId().GetValue() != standing.ID {
		t.Fatalf("StandingAsk = %+v, AsksStanding = %v, want the same oldest ask", standing, standingKeys(asks))
	}
}
