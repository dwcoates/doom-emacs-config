package feed

import (
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/feedid"
)

// THE HEADER OF A PROMPT BETWEEN TWO AGENTS OF ONE SESSION: the subagent by its
// requested name, or by its identifier when it has none; the main agent by the
// daemon's word for it; the arrow on the side the direction names.

// deliverToSubagent delivers one prompt from the main agent to CREATED.
func (h *harness) deliverToSubagent(turn string, created *conversationv1.AgentId) {
	h.t.Helper()
	h.resolver.OnPrompt(testWorkspace, mainAgent(), &conversationv1.AgentPrompt{
		Id:     &conversationv1.TurnId{Value: turn},
		Agent:  created,
		Origin: conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT,
		Said: &conversationv1.UserSaid{Content: &conversationv1.UserContent{
			Blocks: []*conversationv1.UserContentBlock{textBlock("also check the shim")},
		}},
	}, nil)
}

// promptHeaderOn answers the header of the one agent prompt on FEED.
func (h *harness) promptHeaderOn(feed feedid.Feed) string {
	h.t.Helper()
	for _, row := range h.rows(feed) {
		if prompt := row.GetAgentPrompt(); prompt != nil {
			return addressLine(prompt.GetAddress())
		}
	}
	h.t.Fatalf("no agent prompt on %+v", feed)
	return ""
}

func TestAPromptToANamedSubagentIsHeadedByItsNameThenARightArrow(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	created := &conversationv1.AgentId{Value: "agent-vetter"}
	h.spawnNamedSubagent("spawn-1", created, "vetter")

	// Act.
	h.deliverToSubagent("turn-2", created)

	// Assert.
	if got := h.promptHeaderOn(rootFeed()); got != "named vetter →" {
		t.Fatalf("header = %q, want the subagent's requested name, then the arrow", got)
	}
}

func TestAPromptToAnUnnamedSubagentIsHeadedByItsIdentifier(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	created := &conversationv1.AgentId{Value: "agent-explore"}
	h.spawnSubagent("spawn-1", created, "Explore", "map the daemon")

	// Act.
	h.deliverToSubagent("turn-2", created)

	// Assert.
	if got := h.promptHeaderOn(rootFeed()); got != "identified agent-explore →" {
		t.Fatalf("header = %q, want the identifier, never the type or the description", got)
	}
}

func TestThePromptASubagentReceivesIsHeadedByALeftArrowThenItsLead(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	created := &conversationv1.AgentId{Value: "agent-vetter"}
	h.spawnNamedSubagent("spawn-1", created, "vetter")

	// Act.
	h.deliverToSubagent("turn-2", created)

	// Assert.
	if got := h.promptHeaderOn(feedid.Feed{Agent: created}); got != "← main the main agent" {
		t.Fatalf("header = %q, want the arrow, then the lead", got)
	}
}

func TestASubagentsNameFromALaterFrameIsKept(t *testing.T) {
	// Arrange: the start states no name; a later frame restates the commission
	// with one.
	h := newHarness(t)
	created := &conversationv1.AgentId{Value: "agent-vetter"}
	h.spawnSubagent("spawn-1", created, "Explore", "")
	h.spawnNamedSubagent("spawn-1", created, "vetter")

	// Act.
	h.deliverToSubagent("turn-2", created)

	// Assert.
	if got := h.promptHeaderOn(rootFeed()); got != "named vetter →" {
		t.Fatalf("header = %q, want the name a later frame gave", got)
	}
}

func TestASubagentsNameIsNotForgottenByAFrameThatStatesNone(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	created := &conversationv1.AgentId{Value: "agent-vetter"}
	h.spawnNamedSubagent("spawn-1", created, "vetter")
	h.spawnSubagent("spawn-1", created, "Explore", "")

	// Act.
	h.deliverToSubagent("turn-2", created)

	// Assert.
	if got := h.promptHeaderOn(rootFeed()); got != "named vetter →" {
		t.Fatalf("header = %q, want the name kept", got)
	}
}

func TestAgentNamedAnswersTheMainAgentByTheDaemonsWord(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	h.resolver.mu.Lock()
	end := h.resolver.agentNamed(h.resolver.state(testWorkspace), mainAgent().GetValue())
	h.resolver.mu.Unlock()

	// Assert.
	if got := agentIdentity(end.identity) + " " + end.name; got != "main the main agent" {
		t.Fatalf("agent = %q, want the main agent", got)
	}
}

func TestAgentNamedAnswersAnAgentThisWorkspaceKnowsNothingOfByItsIdentifier(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	h.resolver.mu.Lock()
	end := h.resolver.agentNamed(h.resolver.state(testWorkspace), "agent-elsewhere")
	h.resolver.mu.Unlock()

	// Assert.
	if got := agentIdentity(end.identity) + " " + end.name; got != "identified agent-elsewhere" {
		t.Fatalf("agent = %q, want its identifier", got)
	}
}
