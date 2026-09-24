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

// sendMessage pushes one send frame through the sink under `unit`, as a row
// that predates the stands-alone contract writes it.
func (h *harness) sendMessage(unit string, result any) {
	h.t.Helper()
	h.send(sendActivity(h.t, unit, result))
}

// sendMessageBound pushes one send frame as a producer bound by the
// stands-alone contract writes it.
func (h *harness) sendMessageBound(unit string, result any) {
	h.t.Helper()
	h.send(bound(sendActivity(h.t, unit, result)))
}

// sendActivity wraps one send arm as an activity under `unit`.
func sendActivity(t *testing.T, unit string, result any) *conversationv1.AgentActivity {
	t.Helper()
	send := &conversationv1.AgentSendMessage{}
	switch r := result.(type) {
	case *conversationv1.AgentSendMessageStart:
		send.Result = &conversationv1.AgentSendMessage_Start{Start: r}
	case *conversationv1.AgentSendMessageSuccess:
		send.Result = &conversationv1.AgentSendMessage_Success{Success: r}
	case *conversationv1.AgentSendMessageFailure:
		send.Result = &conversationv1.AgentSendMessage_Failure{Failure: r}
	default:
		t.Fatalf("sendMessage: unhandled result %T", result)
	}
	return &conversationv1.AgentActivity{
		ActivityId: &conversationv1.AgentActivityId{Value: unit},
		Item:       &conversationv1.AgentActivity_SendMessage{SendMessage: send},
	}
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

func TestASendDrawsTheProducersDeliveryArmOnTheSendersRow(t *testing.T) {
	tests := []struct {
		name     string
		delivery func(*conversationv1.AgentSendMessageSuccess)
		want     string
	}{
		{
			name: "the recipient was already running and the message queued",
			delivery: func(s *conversationv1.AgentSendMessageSuccess) {
				s.Delivery = &conversationv1.AgentSendMessageSuccess_QueuedToLive{
					QueuedToLive: &conversationv1.AgentSendMessageQueuedToLive{},
				}
			},
			want: "queued_to_live",
		},
		{
			name: "the recipient was dormant and was resumed to receive it",
			delivery: func(s *conversationv1.AgentSendMessageSuccess) {
				s.Delivery = &conversationv1.AgentSendMessageSuccess_ResumedRecipient{
					ResumedRecipient: &conversationv1.AgentSendMessageResumedRecipient{},
				}
			},
			want: "resumed_recipient",
		},
		{
			name:     "the producer stated no arm, so the row states none either",
			delivery: nil,
			want:     "",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			success := &conversationv1.AgentSendMessageSuccess{
				RecipientAgentId: &conversationv1.AgentId{Value: "agent-sub"},
			}
			if tc.delivery != nil {
				tc.delivery(success)
			}

			// Act.
			h.sendMessage("unit-1", startTo("vetter", "vet the diff"))
			h.sendMessage("unit-1", success)

			// Assert.
			if got := deliveryWord(h.sendRow()); got != tc.want {
				t.Fatalf("delivery = %q, want %q", got, tc.want)
			}
		})
	}
}

func TestADeliveredPromptsRecipientCopyCarriesNoDeliveryArm(t *testing.T) {
	// Arrange: a subagent bubble whose feed the delivered prompt is drawn on.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "spawn an explorer")
	created := &conversationv1.AgentId{Value: "agent-explore"}
	h.spawnSubagent("spawn-1", created, "Explore", "map the daemon")

	// Act: the prompt delivered to that subagent.
	h.resolver.OnPrompt(testWorkspace, mainAgent(), &conversationv1.AgentPrompt{
		Id:     &conversationv1.TurnId{Value: "turn-2"},
		Agent:  created,
		Origin: conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT,
		Said: &conversationv1.UserSaid{Content: &conversationv1.UserContent{
			Blocks: []*conversationv1.UserContentBlock{textBlock("also check the shim")},
		}},
	}, noAddress())

	// Assert: the recipient's copy states no delivery — the arm is the
	// SENDER's row alone.
	var seen int
	for _, row := range h.rows(feedid.Feed{Agent: created}) {
		prompt := row.GetAgentPrompt()
		if prompt == nil {
			continue
		}
		seen++
		if got := deliveryWord(prompt); got != "" {
			t.Fatalf("recipient copy delivery = %q, want unset", got)
		}
	}
	if seen == 0 {
		t.Fatal("no agent-prompt row on the recipient's feed to check")
	}
}

// deliveryWord names the drawn delivery arm, empty when the row states none.
func deliveryWord(prompt *frontendv1.FeedAgentPrompt) string {
	switch prompt.GetDelivery().(type) {
	case *frontendv1.FeedAgentPrompt_QueuedToLive:
		return "queued_to_live"
	case *frontendv1.FeedAgentPrompt_ResumedRecipient:
		return "resumed_recipient"
	case *frontendv1.FeedAgentPrompt_Refused:
		return "refused"
	}
	return ""
}

// refusalOf is a failure arm answering with `text` as its only account, the
// shape the vendor's refusal prose arrives in.
func refusalOf(text string) *conversationv1.AgentSendMessageFailure {
	return &conversationv1.AgentSendMessageFailure{
		Error: &conversationv1.AgentToolFailure{
			Content: &conversationv1.ToolResultContent{
				Blocks: []*conversationv1.ToolResultContentBlock{{
					Block: &conversationv1.ToolResultContentBlock_Text{
						Text: &conversationv1.TextBlock{Text: text},
					},
				}},
			},
		},
	}
}

// A refusal must be distinguishable from an ABSENCE: an unset delivery is what
// a producer that stated nothing leaves behind, so the refused send has to
// state an arm of its own (feed.proto, landing 14).
func TestARefusedSendStatesTheRefusedArmRatherThanNoArmAtAll(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.sendMessage("unit-1", startTo("vetter", "vet the diff"))

	// Act.
	h.sendMessage("unit-1", refusalOf("The agent was stopped by the user."))

	// Assert.
	if got := deliveryWord(h.sendRow()); got != "refused" {
		t.Fatalf("delivery = %q, want %q", got, "refused")
	}
}

func TestARefusedSendCarriesTheProducersOwnWordsAsItsReason(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	const prose = "The agent was stopped by the user."
	h.sendMessage("unit-1", startTo("vetter", "vet the diff"))

	// Act.
	h.sendMessage("unit-1", refusalOf(prose))

	// Assert.
	if got := h.sendRow().GetRefused().GetReason().GetText(); got != prose {
		t.Fatalf("refusal reason = %q, want the producer's words %q", got, prose)
	}
}

// The ARM is the refusal; the reason is only its detail. A refusal the producer
// gave no account for is still a refusal, and must not fall back to the unset
// oneof that means "the producer stated nothing".
func TestARefusalWithNoAccountStillStatesTheArmAndNoReason(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.sendMessage("unit-1", startTo("vetter", "vet the diff"))

	// Act.
	h.sendMessage("unit-1", &conversationv1.AgentSendMessageFailure{})

	// Assert.
	prompt := h.sendRow()
	if got := deliveryWord(prompt); got != "refused" {
		t.Fatalf("delivery = %q, want %q", got, "refused")
	}
	if reason := prompt.GetRefused().GetReason(); reason != nil {
		t.Fatalf("refusal reason = %q, want unset — no account was given", reason.GetText())
	}
}

// A SEND'S UNIT IS DELIVERED TWICE. The shim's stream plane converts the SDK's
// events live and the sidecar's file plane replays the SAME units out of the
// vendor transcript under the SAME key, so the terminal frame is followed,
// ~160ms later, by the start frame again. The row must not walk back.
func TestAReplayedStartDoesNotUnstateARefusalTheSendAlreadyCarried(t *testing.T) {
	// Arrange: the send is refused, exactly as the stream plane converted it.
	h := newHarness(t)
	const prose = "The agent was stopped by the user."
	h.sendMessage("unit-1", startTo("vetter", "vet the diff"))
	h.sendMessage("unit-1", refusalOf(prose))

	// Act: the file plane replays the unit, starting with its start frame.
	h.sendMessage("unit-1", startTo("vetter", "vet the diff"))

	// Assert.
	prompt := h.sendRow()
	if got := deliveryWord(prompt); got != "refused" {
		t.Fatalf("delivery after the replayed start = %q, want %q — an unset delivery is "+
			"indistinguishable from a producer that stated nothing", got, "refused")
	}
	if got := prompt.GetRefused().GetReason().GetText(); got != prose {
		t.Fatalf("refusal reason after the replayed start = %q, want the producer's words %q", got, prose)
	}
}

// The same walk-back for the LANDING arms: a replayed start must not turn a
// send the recipient received into one that reads as still on its way.
func TestAReplayedStartDoesNotUnstateADeliveryTheSendAlreadyCarried(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.sendMessage("unit-1", startTo("vetter", "vet the diff"))
	h.sendMessage("unit-1", &conversationv1.AgentSendMessageSuccess{
		Delivery: &conversationv1.AgentSendMessageSuccess_QueuedToLive{
			QueuedToLive: &conversationv1.AgentSendMessageQueuedToLive{},
		},
	})

	// Act.
	h.sendMessage("unit-1", startTo("vetter", "vet the diff"))

	// Assert.
	if got := deliveryWord(h.sendRow()); got != "queued_to_live" {
		t.Fatalf("delivery after the replayed start = %q, want %q", got, "queued_to_live")
	}
}

// The resolved identity is the success arm's alone: the replayed start restates
// the ADDRESSED string and never the id, so an address line drawn from the
// frame walked back from the recipient's own bubble label to the raw string.
func TestAReplayedStartKeepsTheRecipientTheSuccessResolved(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.sendMessage("unit-1", startTo("", "vet the diff"))
	h.sendMessage("unit-1", &conversationv1.AgentSendMessageSuccess{
		RecipientAgentId: &conversationv1.AgentId{Value: "agent-unknown"},
	})

	// Act.
	h.sendMessage("unit-1", startTo("", "vet the diff"))

	// Assert.
	if got := h.sendRow().GetAddress().GetText(); got != "→ agent-unknown" {
		t.Fatalf("address after the replayed start = %q, want the resolved identity, not the unnamed fallback", got)
	}
}

// A SETTLED SEND STANDS ALONE. Its start and its settle upsert one unit, and
// the store keeps one row per unit, so a replay (a workspace open, a transcript
// select) serves the settle with no start beside it. Drawn from the start's
// fields alone, row prompt:toolu_01GiUF1L5VCoxJQZ8nURrUEv:send replayed with an
// EMPTY body; the settle now restates the address and the summary.

// replayedSends are a send's two settle arms, each restating what its start
// carried, with no start ever delivered.
func replayedSends(to, summary string) []struct {
	name   string
	settle any
} {
	var restated *conversationv1.AgentSendMessageSummary
	if summary != "" {
		restated = &conversationv1.AgentSendMessageSummary{Text: summary}
	}
	return []struct {
		name   string
		settle any
	}{
		{
			name: "a delivered send",
			settle: &conversationv1.AgentSendMessageSuccess{
				RecipientAgentId: &conversationv1.AgentId{Value: "agent-unknown"},
				AddressedTo:      to,
				Summary:          restated,
			},
		},
		{
			name:   "a refused send",
			settle: &conversationv1.AgentSendMessageFailure{AddressedTo: to, Summary: restated},
		},
	}
}

func TestAReplayedSettledSendDrawsItsSummary(t *testing.T) {
	for _, tc := range replayedSends("vetter", "Scroll fix landed; merge master in") {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)

			// Act: the settle alone, as a replay serves it.
			h.sendMessage("unit-1", tc.settle)

			// Assert.
			blocks := h.sendRow().GetBody().GetBlocks()
			if len(blocks) != 1 || blocks[0].GetText().GetText() != "Scroll fix landed; merge master in" {
				t.Fatalf("blocks = %v, want the restated summary", blocks)
			}
		})
	}
}

func TestAReplayedSettledSendDrawsItsAddress(t *testing.T) {
	for _, tc := range replayedSends("vetter", "Scroll fix landed; merge master in") {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)

			// Act.
			h.sendMessage("unit-1", tc.settle)

			// Assert.
			if got := h.sendRow().GetAddress().GetText(); got != "→ vetter" {
				t.Fatalf("address = %q, want the restated addressed string", got)
			}
		})
	}
}

func TestAReplayedSettledSendWithNoSummaryDrawsNoBody(t *testing.T) {
	for _, tc := range replayedSends("vetter", "") {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)

			// Act.
			h.sendMessage("unit-1", tc.settle)

			// Assert: the caller supplied none, so there is nothing to draw —
			// never the relayed message.
			if blocks := h.sendRow().GetBody().GetBlocks(); len(blocks) != 0 {
				t.Fatalf("blocks = %d, want 0", len(blocks))
			}
		})
	}
}

func TestAReplayedSettledSendRestatingNoAddressDrawsNothing(t *testing.T) {
	for _, tc := range replayedSends("", "") {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)

			// Act: a settle that restated nothing, with no start held.
			h.sendMessage("unit-1", tc.settle)

			// Assert: no row with an empty body is drawn.
			if rows := h.rows(rootFeed()); len(rows) != 0 {
				t.Fatalf("rows = %d, want 0: an unrestated settle must not draw an empty send", len(rows))
			}
		})
	}
}

func TestAReplayedSettledSendRestatingNoAddressIsRecordedAtError(t *testing.T) {
	for _, tc := range replayedSends("", "") {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)

			// Act: a producer bound by the contract that restated nothing.
			h.sendMessageBound("unit-1", tc.settle)

			// Assert.
			if !h.hasRecord("error", "daemon.feed.activity_undrawable") {
				t.Fatalf("records = %+v, want an ERROR daemon.feed.activity_undrawable", h.records())
			}
		})
	}
}

func TestASettledSendRestatingNoAddressIsRecordedAtErrorWithTheStartHeld(t *testing.T) {
	for _, tc := range replayedSends("", "") {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			h.sendMessage("unit-1", startTo("vetter", "vet the diff"))

			// Act: a producer bound by the contract that restated nothing.
			h.sendMessageBound("unit-1", tc.settle)

			// Assert.
			if !h.hasRecord("error", "daemon.feed.settle_not_restated") {
				t.Fatalf("records = %+v, want an ERROR daemon.feed.settle_not_restated", h.records())
			}
		})
	}
}

func TestAReplayedPreContractSendRestatingNoAddressIsRecordedAtInfo(t *testing.T) {
	for _, tc := range replayedSends("", "") {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)

			// Act: a row written before the contract, replayed alone.
			h.sendMessage("unit-1", tc.settle)

			// Assert.
			if !h.hasRecord("info", "daemon.feed.settle_predates_contract") || len(h.anyErrors()) != 0 {
				t.Fatalf("records = %+v, want an INFO daemon.feed.settle_predates_contract and no ERROR", h.records())
			}
		})
	}
}

func TestAReplayedPreContractSendRestatingNoAddressDrawsNothing(t *testing.T) {
	for _, tc := range replayedSends("", "") {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)

			// Act.
			h.sendMessage("unit-1", tc.settle)

			// Assert.
			if rows := h.rows(rootFeed()); len(rows) != 0 {
				t.Fatalf("rows = %d, want 0: a send with an empty body is never drawn", len(rows))
			}
		})
	}
}

func TestAPreContractSendRestatingNoAddressIsRecordedAtInfoWithTheStartHeld(t *testing.T) {
	for _, tc := range replayedSends("", "") {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			h.sendMessage("unit-1", startTo("vetter", "vet the diff"))

			// Act.
			h.sendMessage("unit-1", tc.settle)

			// Assert.
			if !h.hasRecord("info", "daemon.feed.settle_predates_contract") || len(h.anyErrors()) != 0 {
				t.Fatalf("records = %+v, want an INFO daemon.feed.settle_predates_contract and no ERROR", h.records())
			}
		})
	}
}

func TestASettledSendRestatingNoAddressIsDrawnFromTheStartHeld(t *testing.T) {
	for _, tc := range replayedSends("", "") {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			h.sendMessage("unit-1", startTo("vetter", "vet the diff"))

			// Act.
			h.sendMessage("unit-1", tc.settle)

			// Assert.
			blocks := h.sendRow().GetBody().GetBlocks()
			if len(blocks) != 1 || blocks[0].GetText().GetText() != "vet the diff" {
				t.Fatalf("blocks = %v, want the held start's summary", blocks)
			}
		})
	}
}
