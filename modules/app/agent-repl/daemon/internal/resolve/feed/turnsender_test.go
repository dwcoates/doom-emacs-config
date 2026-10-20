package feed

import (
	"errors"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/feedid"
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

// sentRowID is the id SentPrompt draws KEY's row under in SENDER's root feed.
func sentRowID(sender ids.WorkspaceID, key string) string {
	return testEncode(feedid.Ref{WS: sender, Feed: rootFeed(), Row: feedid.RowKey{Kind: feedid.KindPrompt, ID: key, Sub: "sent"}}).GetValue()
}

func TestASentPromptIsDrawnInTheSendersFeedHeadedByTheRecipient(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	h.resolver.SentPrompt(testWorkspace, "parser-fix", "k-1", SaidText("please review the parser"), SentFate{})

	// Assert
	row := h.only(rootFeed())
	if row.GetId().GetValue() != sentRowID(testWorkspace, "k-1") || row.GetTurn() != nil {
		t.Fatalf("row = %v, want the sent row, stamped with no turn", row)
	}
	if got := addressLine(row.GetAgentPrompt().GetAddress()); got != "workspace parser-fix →" {
		t.Fatalf("address = %q, want the recipient workspace then a right arrow", got)
	}
	if got := agentPromptTexts(row.GetAgentPrompt()); len(got) != 1 || got[0] != "please review the parser" {
		t.Fatalf("body = %q, want the prompt's text", got)
	}
	if row.GetAgentPrompt().GetDelivery() != nil {
		t.Fatalf("delivery = %v, want none for a prompt delivered at once", row.GetAgentPrompt().GetDelivery())
	}
}

func TestASentPromptStatesHowItFared(t *testing.T) {
	why := "the session is parked at its cold gate"
	tests := []struct {
		name  string
		fate  SentFate
		check func(*frontendv1.FeedAgentPrompt) bool
	}{
		{name: "held behind the recipient's work", fate: SentFate{Queued: true}, check: func(p *frontendv1.FeedAgentPrompt) bool {
			return p.GetQueuedToLive() != nil
		}},
		{name: "refused", fate: SentFate{Refused: &why}, check: func(p *frontendv1.FeedAgentPrompt) bool {
			return p.GetRefused().GetReason().GetText() == why
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)

			// Act
			h.resolver.SentPrompt(testWorkspace, "parser-fix", "k-1", SaidText("hello"), tt.fate)

			// Assert
			if prompt := h.only(rootFeed()).GetAgentPrompt(); !tt.check(prompt) {
				t.Fatalf("prompt = %v, want the fate stated", prompt)
			}
		})
	}
}

func TestASentPromptIsRecordedDurably(t *testing.T) {
	// Arrange
	store := newFakeDurableRows()
	r, _ := daemonOver(t, store)

	// Act
	r.SentPrompt(testWorkspace, "parser-fix", "k-1", SaidText("hello"), SentFate{})

	// Assert
	if _, ok := store.recorded(testWorkspace, sentRowID(testWorkspace, "k-1")); !ok {
		t.Fatal("the sent row was not recorded durably")
	}
}
