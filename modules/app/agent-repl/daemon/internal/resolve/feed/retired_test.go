package feed

import (
	"context"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/feedid"
)

// RETIRED ENTRIES. A page line the store retired is removed from every feed it
// was drawn on, through retire, so an open tail drops it live.

// openRootTail opens a following tail on the root feed and answers its rows.
func (h *harness) openRootTail() <-chan *frontendv1.FeedRow {
	h.t.Helper()
	_, token := h.openPage(rootFeed(), "reader-1")
	ctx, cancel := context.WithCancel(context.Background())
	h.t.Cleanup(cancel)
	tail, err := h.resolver.Tail(ctx, testWorkspace, rootFeed(), token)
	if err != nil {
		h.t.Fatalf("Tail: %v", err)
	}
	return tail.Rows(ctx)
}

// retiredPrompt is the prompt entry as the shim last served it.
func retiredPrompt(turn string, agent *conversationv1.AgentId) *conversationv1.AgentPrompt {
	return &conversationv1.AgentPrompt{Id: &conversationv1.TurnId{Value: turn}, Agent: agent}
}

// evidenceOf answers a turn's pending evidence messages.
func (h *harness) evidenceOf(turn string) []string {
	h.resolver.mu.Lock()
	defer h.resolver.mu.Unlock()
	var out []string
	for _, line := range h.resolver.state(testWorkspace).turnEvidence[turn] {
		out = append(out, line.apiMessage)
	}
	return out
}

func TestARetiredPromptPublishesTheRemovalOfItsRow(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	rows := h.openRootTail()
	h.deliverPrompt("turn-1", "<task-notification>")
	<-rows

	// Act.
	h.resolver.OnPromptRetired(testWorkspace, retiredPrompt("turn-1", mainAgent()), noAddress())

	// Assert.
	removal := <-rows
	if removal.GetRemoved() == nil || removal.GetId().GetValue() != h.promptRowID("turn-1") {
		t.Fatalf("streamed %v, want the removal of turn-1's prompt row", removal)
	}
}

func TestARetiredPromptLeavesTheFeedWithoutItsRow(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "<task-notification>")

	// Act.
	h.resolver.OnPromptRetired(testWorkspace, retiredPrompt("turn-1", mainAgent()), noAddress())

	// Assert.
	if got := h.rows(rootFeed()); len(got) != 0 {
		t.Fatalf("root rows = %v, want none after the retirement", got)
	}
}

func TestARetiredAgentPromptRemovesBothEnds(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "spawn an explorer")
	created := &conversationv1.AgentId{Value: "agent-explore"}
	h.spawnSubagent("spawn-1", created, "Explore", "map the daemon")
	h.resolver.OnPrompt(testWorkspace, mainAgent(), &conversationv1.AgentPrompt{
		Id:    &conversationv1.TurnId{Value: "turn-2"},
		Agent: created,
		Said:  &conversationv1.UserSaid{Content: &conversationv1.UserContent{Blocks: []*conversationv1.UserContentBlock{textBlock("go")}}},
	}, noAddress())

	// Act.
	h.resolver.OnPromptRetired(testWorkspace, retiredPrompt("turn-2", created), noAddress())

	// Assert.
	for _, feed := range []feedid.Feed{rootFeed(), {Agent: created}} {
		for _, row := range h.rows(feed) {
			if row.GetAgentPrompt() != nil && row.GetTurn().GetValue() == "turn-2" {
				t.Fatalf("feed %v still draws the retired agent prompt %v", feed, row.GetId())
			}
		}
	}
}

func TestARetiredPeerMessagePublishesTheRemovalOfItsRow(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	rows := h.openRootTail()
	h.peerMessage("p1", "Explore", "found it")
	<-rows

	// Act.
	h.resolver.OnPeerMessageRetired(testWorkspace, &conversationv1.PeerMessage{Agent: mainAgent(), Id: "p1"}, noAddress())

	// Assert.
	removal := <-rows
	if removal.GetRemoved() == nil || removal.GetId().GetValue() != h.peerRowID("p1") {
		t.Fatalf("streamed %v, want the removal of p1's peer row", removal)
	}
}

func TestARetiredEntryTheFeedNeverDrewIsRecordedAtDebug(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	h.resolver.OnPromptRetired(testWorkspace, retiredPrompt("turn-never", mainAgent()), noAddress())

	// Assert.
	if !h.hasRecord("debug", "daemon.feed.retired_entry_undrawn") {
		t.Fatalf("records = %+v, want the undrawn retirement at debug", h.records())
	}
}

func TestARetiredEntryTheFeedNeverDrewWarnsNothing(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	h.resolver.OnPromptRetired(testWorkspace, retiredPrompt("turn-never", mainAgent()), noAddress())

	// Assert.
	for _, record := range h.records() {
		if record.Level == "warn" || record.Level == "error" {
			t.Fatalf("record %+v, want nothing above debug for an undrawn retirement", record)
		}
	}
}

func TestARetiredApiErrorWithdrawsItsEvidenceLine(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "go")
	stamp := &conversationv1.TurnId{Value: "turn-1"}
	failed := &conversationv1.ApiRequestFailed{Message: "529 overloaded"}
	h.resolver.OnApiError(testWorkspace, mainAgent(), failed, stamp, noAddress())

	// Act.
	h.resolver.OnApiErrorRetired(testWorkspace, mainAgent(), failed, stamp, noAddress())

	// Assert.
	if got := h.evidenceOf("turn-1"); len(got) != 0 {
		t.Fatalf("turn-1 evidence = %v, want the retired line withdrawn", got)
	}
}

func TestAnUnstampedRetiredApiErrorWithdrawsNothing(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "go")
	failed := &conversationv1.ApiRequestFailed{Message: "529 overloaded"}
	h.resolver.OnApiError(testWorkspace, mainAgent(), failed, nil, noAddress())

	// Act.
	h.resolver.OnApiErrorRetired(testWorkspace, mainAgent(), failed, nil, noAddress())

	// Assert.
	if got := h.evidenceOf("turn-1"); len(got) != 1 {
		t.Fatalf("turn-1 evidence = %v, want the unattributable line kept", got)
	}
}
