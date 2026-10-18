package feed

import (
	"errors"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/ids"
)

// senderWorkspace is the workspace whose agent sends the prompts these tests
// draw.
const senderWorkspace = ids.WorkspaceID("ws-sender")

// agentSentEntry is promptEntry's prompt as another workspace's agent sent it.
func agentSentEntry(turn, text string) *conversationv1.HistoryEntry {
	entry := promptEntry(turn, text)
	entry.GetUserPrompt().Origin = conversationv1.PromptOrigin_PROMPT_ORIGIN_AGENT_SENT
	return entry
}

// deliverAgentSent sends TURN's agent-sent prompt through the sink, its sender
// handed over first, as the prompt queue records the turn before it delivers.
func (h *harness) deliverAgentSent(turn string, sender *ids.WorkspaceID) {
	h.t.Helper()
	h.resolver.SenderTurn(testWorkspace, ids.TurnID(turn), sender)
	prompt := agentSentEntry(turn, "please review the parser").GetUserPrompt()
	h.resolver.OnPrompt(testWorkspace, mainAgent(), prompt, nil)
}

func TestAnAgentSentPromptIsHeadedByTheWorkspaceThatSentIt(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.names = map[ids.WorkspaceID]string{senderWorkspace: "parser-fix"}
	sender := senderWorkspace

	// Act
	h.deliverAgentSent("t1", &sender)

	// Assert
	row := h.only(rootFeed())
	if got := addressLine(row.GetAgentPrompt().GetAddress()); got != "← workspace parser-fix" {
		t.Fatalf("address = %q, want the sending workspace", got)
	}
	if got := agentPromptTexts(row.GetAgentPrompt()); len(got) != 1 || got[0] != "please review the parser" {
		t.Fatalf("body = %q, want the prompt's text", got)
	}
}

func TestAnAgentSentPromptIsStampedWithItsTurn(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.names = map[ids.WorkspaceID]string{senderWorkspace: "parser-fix"}
	sender := senderWorkspace

	// Act
	h.deliverAgentSent("t1", &sender)

	// Assert
	row := h.only(rootFeed())
	if row.GetTurn().GetValue() != "t1" || !row.GetAgentPrompt().GetWorking() {
		t.Fatalf("row = %v, want it stamped with t1 and working", row)
	}
}

func TestASenderThatCannotBeNamedIsHeadedUnnamed(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.nameErr = errors.New("database is locked")
	sender := senderWorkspace

	// Act
	h.deliverAgentSent("t1", &sender)

	// Assert
	if got := addressLine(h.only(rootFeed()).GetAgentPrompt().GetAddress()); got != "← unnamed workspace "+unnamedWorkspaceSender {
		t.Fatalf("address = %q, want the unnamed stand-in", got)
	}
	if !h.hasRecordWith("error", "daemon.feed.peer_workspace_name_failed", "other_workspace", string(senderWorkspace)) {
		t.Fatalf("records = %+v, want the unreadable name at ERROR", h.records())
	}
}

func TestAnAgentSentPromptWithNoRecordedSenderIsHeadedUnnamedAndIsAnError(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	h.deliverAgentSent("t1", nil)

	// Assert
	if got := addressLine(h.only(rootFeed()).GetAgentPrompt().GetAddress()); got != "← unnamed workspace "+unnamedWorkspaceSender {
		t.Fatalf("address = %q, want the unnamed stand-in", got)
	}
	if !h.hasRecordWith("error", "daemon.feed.turn_sender_missing", "turn", "t1") {
		t.Fatalf("records = %+v, want the missing sender at ERROR", h.records())
	}
}

func TestPromptHeaderIsNoneForAPromptNoAgentSent(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	header := h.resolver.PromptHeader(testWorkspace, "t1", conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT)

	// Assert
	if header != nil {
		t.Fatalf("header = %v, want none", header)
	}
}

func TestAReplayedAgentSentPromptIsHeadedByItsRecordedSender(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.names = map[ids.WorkspaceID]string{senderWorkspace: "parser-fix"}
	h.senders = map[ids.TurnID]ids.WorkspaceID{"t1": senderWorkspace}

	// Act
	h.replay(historyPage(&conversationv1.HistoryFloor{}, agentSentEntry("t1", "please review the parser")))

	// Assert
	if got := addressLine(h.only(rootFeed()).GetAgentPrompt().GetAddress()); got != "← workspace parser-fix" {
		t.Fatalf("address = %q, want the recorded sender", got)
	}
}

func TestAnUnreadableRecordedSenderIsAnErrorAndThePageStillDraws(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.senders = map[ids.TurnID]ids.WorkspaceID{"t1": senderWorkspace}
	h.sendersErr = errors.New("database is locked")

	// Act
	h.replay(historyPage(&conversationv1.HistoryFloor{}, agentSentEntry("t1", "please review the parser")))

	// Assert
	if !h.hasRecordWith("error", "daemon.feed.turn_senders_unreadable", "cause", "database is locked") {
		t.Fatalf("the failed read was not recorded at ERROR: %+v", h.records())
	}
	if got := addressLine(h.only(rootFeed()).GetAgentPrompt().GetAddress()); got != "← unnamed workspace "+unnamedWorkspaceSender {
		t.Fatalf("address = %q, want the unnamed stand-in", got)
	}
}
