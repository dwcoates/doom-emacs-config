package feed

import (
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/feedid"
)

// ①c The outgoing send. A SendMessage is an agent-addressed prompt, so the
// SENDER's feed draws it with the agent_prompt component and a composed
// address line naming the recipient.

// sendMessage pushes one send frame through the sink under `unit`.
func (h *harness) sendMessage(unit string, result any) {
	h.t.Helper()
	send := &conversationv1.AgentSendMessage{}
	switch r := result.(type) {
	case *conversationv1.AgentSendMessageStart:
		send.Result = &conversationv1.AgentSendMessage_Start{Start: r}
	case *conversationv1.AgentSendMessageSuccess:
		send.Result = &conversationv1.AgentSendMessage_Success{Success: r}
	case *conversationv1.AgentSendMessageFailure:
		send.Result = &conversationv1.AgentSendMessage_Failure{Failure: r}
	default:
		h.t.Fatalf("sendMessage: unhandled result %T", result)
	}
	h.send(&conversationv1.AgentActivity{
		ActivityId: &conversationv1.AgentActivityId{Value: unit},
		Item:       &conversationv1.AgentActivity_SendMessage{SendMessage: send},
	})
}

// sendRow returns the agent-prompt row a send drew on the sender's feed,
// failing when the row is any other kind.
func (h *harness) sendRow() *frontendv1.FeedAgentPrompt {
	h.t.Helper()
	row := h.only(rootFeed())
	prompt := row.GetAgentPrompt()
	if prompt == nil {
		h.t.Fatalf("row = %T, want an agent prompt", row.GetRow())
	}
	return prompt
}

// startTo is a send's start arm addressed to `to` with `summary` as its
// one-line preview; an empty summary means the caller supplied none.
func startTo(to, summary string) *conversationv1.AgentSendMessageStart {
	start := &conversationv1.AgentSendMessageStart{
		AddressedTo: to,
		Body:        &conversationv1.AgentSendMessageBody{Text: "the whole relayed message, never drawn"},
		StartedAt:   &conversationv1.AgentActivityStartedAt{AtMs: 1_000},
	}
	if summary != "" {
		start.Summary = &conversationv1.AgentSendMessageSummary{Text: summary}
	}
	return start
}

func TestASendDrawsAnAgentPromptOnTheSendersFeed(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.sendMessage("unit-1", startTo("ac8caa658f5487d6d", "Report even/odd status for each number"))

	// Assert.
	if got := h.sendRow().GetAddress().GetText(); got != "→ ac8caa658f5487d6d" {
		t.Fatalf("address = %q, want the addressed recipient", got)
	}
}

func TestASendDrawsTheCallersSummaryAsItsBody(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.sendMessage("unit-1", startTo("vetter", "Report even/odd status for each number"))

	// Assert.
	blocks := h.sendRow().GetBody().GetBlocks()
	if len(blocks) != 1 {
		t.Fatalf("blocks = %d, want 1", len(blocks))
	}
	if got := blocks[0].GetText().GetText(); got != "Report even/odd status for each number" {
		t.Fatalf("block = %q, want the summary", got)
	}
}

func TestASendWithNoSummaryDrawsNoBodyRatherThanTheMessage(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.sendMessage("unit-1", startTo("vetter", ""))

	// Assert.
	if blocks := h.sendRow().GetBody().GetBlocks(); len(blocks) != 0 {
		t.Fatalf("blocks = %d, want 0: the relayed body is never drawn", len(blocks))
	}
}

func TestASendAddressesAResolvedRecipientByItsBubbleLabel(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	recipient := &conversationv1.AgentId{Value: "agent-sub"}
	h.spawnSubagent("spawn-1", recipient, "Explore", "longhand counter")

	// Act.
	h.sendMessage("unit-1", startTo("agent-sub", "count out loud"))
	h.sendMessage("unit-1", &conversationv1.AgentSendMessageSuccess{
		RecipientAgentId: recipient,
		Delivery: &conversationv1.AgentSendMessageSuccess_QueuedToLive{
			QueuedToLive: &conversationv1.AgentSendMessageQueuedToLive{},
		},
	})

	// Assert.
	got := h.sendRowAt(rootFeed(), "unit-1").GetAddress().GetText()
	if got != "→ longhand counter" {
		t.Fatalf("address = %q, want the recipient's bubble label", got)
	}
}

func TestAQueuedThenResumedSendDrawsOneRowNotTwo(t *testing.T) {
	tests := []struct {
		name     string
		delivery any
	}{
		{
			name:     "the recipient was already running and the message queued",
			delivery: &conversationv1.AgentSendMessageQueuedToLive{},
		},
		{
			name:     "the recipient was dormant and was resumed to receive it",
			delivery: &conversationv1.AgentSendMessageResumedRecipient{},
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			success := &conversationv1.AgentSendMessageSuccess{
				RecipientAgentId: &conversationv1.AgentId{Value: "agent-sub"},
			}
			switch d := tc.delivery.(type) {
			case *conversationv1.AgentSendMessageQueuedToLive:
				success.Delivery = &conversationv1.AgentSendMessageSuccess_QueuedToLive{QueuedToLive: d}
			case *conversationv1.AgentSendMessageResumedRecipient:
				success.Delivery = &conversationv1.AgentSendMessageSuccess_ResumedRecipient{ResumedRecipient: d}
			}

			// Act.
			h.sendMessage("unit-1", startTo("vetter", "vet the diff"))
			h.sendMessage("unit-1", success)

			// Assert.
			if rows := h.rows(rootFeed()); len(rows) != 1 {
				t.Fatalf("rows = %d, want exactly 1: every frame upserts the one send", len(rows))
			}
		})
	}
}

func TestASendKeepsTheStartsSummaryWhenTheSuccessRestatesNone(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.sendMessage("unit-1", startTo("vetter", "vet the diff"))

	// Act.
	h.sendMessage("unit-1", &conversationv1.AgentSendMessageSuccess{
		RecipientAgentId: &conversationv1.AgentId{Value: "agent-sub"},
	})

	// Assert.
	blocks := h.sendRow().GetBody().GetBlocks()
	if len(blocks) != 1 || blocks[0].GetText().GetText() != "vet the diff" {
		t.Fatalf("blocks = %v, want the start's summary held across the success", blocks)
	}
}

func TestASendWhoseRecipientIsUnnameableSaysSo(t *testing.T) {
	// Arrange, Act. Nothing on the wire names the recipient: the caller wrote
	// no addressed string and no identity was resolved.
	h := newHarness(t)
	h.sendMessage("unit-1", startTo("", "vet the diff"))

	// Assert.
	if got := h.sendRow().GetAddress().GetText(); got != "→ an agent the send did not name" {
		t.Fatalf("address = %q, want the honest unnamed address", got)
	}
}

func TestASendWithNoAddressedStringFallsBackToTheResolvedIdentity(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.sendMessage("unit-1", startTo("", "vet the diff"))

	// Act. The identity resolves, but names no feed this workspace knows.
	h.sendMessage("unit-1", &conversationv1.AgentSendMessageSuccess{
		RecipientAgentId: &conversationv1.AgentId{Value: "agent-unknown"},
	})

	// Assert.
	if got := h.sendRow().GetAddress().GetText(); got != "→ agent-unknown" {
		t.Fatalf("address = %q, want the resolved identity", got)
	}
}

func TestAFailedSendStillDrawsTheAttempt(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.sendMessage("unit-1", startTo("vetter", "vet the diff"))

	// Act.
	h.sendMessage("unit-1", &conversationv1.AgentSendMessageFailure{})

	// Assert.
	if got := h.sendRow().GetAddress().GetText(); got != "→ vetter" {
		t.Fatalf("address = %q, want the attempted recipient", got)
	}
}

func TestASendWithNoResultArmDrawsNothing(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.send(&conversationv1.AgentActivity{
		ActivityId: &conversationv1.AgentActivityId{Value: "unit-1"},
		Item: &conversationv1.AgentActivity_SendMessage{
			SendMessage: &conversationv1.AgentSendMessage{},
		},
	})

	// Assert.
	if rows := h.rows(rootFeed()); len(rows) != 0 {
		t.Fatalf("rows = %d, want 0", len(rows))
	}
}

// sendRowAt finds a send's row by its identity, for the cases whose feed holds
// more than the send alone.
func (h *harness) sendRowAt(feed feedid.Feed, unit string) *frontendv1.FeedAgentPrompt {
	h.t.Helper()
	want := testEncode(feedid.Ref{
		WS: testWorkspace, Feed: feed,
		Row: feedid.RowKey{Kind: feedid.KindPrompt, ID: unit, Sub: "send"},
	}).GetValue()
	for _, row := range h.rows(feed) {
		if row.GetId().GetValue() != want {
			continue
		}
		prompt := row.GetAgentPrompt()
		if prompt == nil {
			h.t.Fatalf("row = %T, want an agent prompt", row.GetRow())
		}
		return prompt
	}
	h.t.Fatalf("no send row %q on the feed", want)
	return nil
}
