package feed

import (
	"context"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/feedid"
	"claude-repld/internal/ids"
	"claude-repld/internal/sessionwatcher"
)

// THE RESET IS A BIND'S. A workspace pointed at a DIFFERENT vendor
// conversation must be left with a feed as empty as one never opened: not one
// row of the conversation it no longer runs, and nothing held about those rows
// that a later frame could bring back.

// subAgent is the agent a spawn creates, whose frames land on its own
// sub-feed.
func subAgent() *conversationv1.AgentId { return &conversationv1.AgentId{Value: "agent-sub"} }

// subFeed is that agent's sub-feed address.
func subFeed() feedid.Feed { return feedid.Feed{Agent: subAgent()} }

// reset empties the harness's workspace, as a bind does.
func (h *harness) reset() {
	h.t.Helper()
	h.resolver.ResetWorkspace(testWorkspace, "the workspace was bound to a different conversation")
	// THE NEW SESSION'S WATCHER NAMES ITS MAIN AGENT before its first frame,
	// exactly as the first one did; the reset forgot the old naming with
	// everything else of the old conversation.
	h.resolver.OnMainAgent(testWorkspace, mainAgent())
}

func TestAResetLeavesNoRowOfThePreviousConversationOnTheRootFeed(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "the old conversation")
	h.concludeAnswer("turn-2", "unit-2", "the old answer")
	if len(h.rows(rootFeed())) == 0 {
		t.Fatal("the arrangement drew no rows at all")
	}

	// Act.
	h.reset()

	// Assert.
	if got := h.rows(rootFeed()); len(got) != 0 {
		t.Fatalf("root feed rows = %v, want none", rowIDs(got))
	}
}

func TestAResetLeavesNoRowOfThePreviousConversationOnASubFeed(t *testing.T) {
	// Arrange: a spawn's bubble on the root feed and a row inside its
	// sub-feed, which is a feed the reset is never handed by name.
	h := newHarness(t)
	h.spawnSubagent("unit-1", subAgent(), "explore", "go and look")
	h.resolver.OnActivity(testWorkspace, subAgent(),
		responseSuccessActivity("unit-sub", "what the subagent found"), nil, nil)
	if len(h.rows(subFeed())) == 0 {
		t.Fatal("the arrangement drew no sub-feed rows at all")
	}

	// Act.
	h.reset()

	// Assert.
	if got := h.rows(subFeed()); len(got) != 0 {
		t.Fatalf("sub-feed rows = %v, want none", rowIDs(got))
	}
}

func TestAResetLeavesNoRowWITHHELDBelowAContextCutBound(t *testing.T) {
	// Arrange: a prompt, then a clear. The prompt is stored but WITHHELD by
	// the cut's delivery bound, so it is the row a reset that only walked what
	// is delivered would leave behind.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "before the clear")
	h.cut(&conversationv1.ContextCut{
		Cut: &conversationv1.ContextCut_Cleared{Cleared: &conversationv1.ContextCleared{}},
	})
	withheld := h.promptRowID("turn-1")
	if !h.holdsRow(rootFeed(), withheld) {
		t.Fatal("the arrangement did not store the withheld prompt row")
	}

	// Act.
	h.reset()

	// Assert.
	if h.holdsRow(rootFeed(), withheld) {
		t.Fatalf("row %q survived the reset", withheld)
	}
}

func TestAResetDropsTheDeliveryBoundItself(t *testing.T) {
	// Arrange: a clear, whose divider is the feed's delivery bound.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "before the clear")
	h.cut(&conversationv1.ContextCut{
		Cut: &conversationv1.ContextCut_Cleared{Cleared: &conversationv1.ContextCleared{}},
	})

	// Act: the reset, then one row of the NEW conversation.
	h.reset()
	h.deliverPrompt("turn-9", "the newly bound conversation")

	// Assert: the new row is DELIVERED, so no bound of the old conversation
	// survived to withhold it.
	page, _ := h.openPage(rootFeed(), "reader-1")
	if got := rowIDs(pageRows(t, page)); len(got) != 1 || got[0] != h.promptRowID("turn-9") {
		t.Fatalf("page rows = %v, want only the new conversation's prompt", got)
	}
}

func TestAResetDropsAStandingPermissionAsk(t *testing.T) {
	// Arrange: an ask the daemon served and is holding an agent against.
	h := newHarness(t)
	h.ask("ask-1", "unit-1", &conversationv1.AgentPermissionStart{
		Prompt:    &conversationv1.AgentPermissionPrompt{Title: "Claude wants to read foo.txt"},
		StartedAt: &conversationv1.AgentActivityStartedAt{AtMs: 1_000},
	})
	if _, _, ok := h.resolver.ServedPermission(testWorkspace, "ask-1"); !ok {
		t.Fatal("the arrangement served no permission ask")
	}

	// Act.
	h.reset()

	// Assert: an answer naming it now finds nothing served and is refused by
	// the ordinary path, rather than reaching a session that never asked.
	if _, _, ok := h.resolver.ServedPermission(testWorkspace, "ask-1"); ok {
		t.Fatal("the served permission ask survived the reset")
	}
}

func TestAResetDropsAStandingQuestionAsk(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.pose("ask-q", &conversationv1.AgentQuestionStart{
		Batch:     twoQuestionBatch(),
		StartedAt: &conversationv1.AgentActivityStartedAt{AtMs: 1_000},
	})
	if _, _, ok := h.resolver.ServedQuestion(testWorkspace, "ask-q"); !ok {
		t.Fatal("the arrangement served no question ask")
	}

	// Act.
	h.reset()

	// Assert.
	if _, _, ok := h.resolver.ServedQuestion(testWorkspace, "ask-q"); ok {
		t.Fatal("the served question ask survived the reset")
	}
}

func TestAResetDropsTheSelectableFinalResponses(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.concludeAnswer("turn-1", "unit-1", "the old answer")
	if len(h.resolver.FinalResponses(testWorkspace)) != 1 {
		t.Fatal("the arrangement recorded no selectable final response")
	}

	// Act.
	h.reset()

	// Assert: reply-to-a-past-response can name no answer of a conversation
	// this workspace no longer runs.
	if got := h.resolver.FinalResponses(testWorkspace); len(got) != 0 {
		t.Fatalf("final responses = %d, want none", len(got))
	}
}

func TestAResetDropsTheTurnAddresses(t *testing.T) {
	// Arrange: a turn addressed at a merge tab of the session that is being
	// swapped out.
	h := newHarness(t)
	lease := ids.LeaseID("lease-7")
	h.resolver.AddressTurn(testWorkspace, "turn-9", &sessionwatcher.OutputAddress{
		Feed: feedid.Feed{Merge: &lease},
	})

	// Act.
	h.reset()
	h.resolver.UpsertAtTurnAddress(testWorkspace, "turn-9",
		feedid.RowKey{Kind: feedid.KindPrompt, ID: "turn-9"},
		&frontendv1.FeedRow{
			Id: &frontendv1.FeedId{Value: "row|after-the-reset"},
			Row: &frontendv1.FeedRow_UserPrompt{
				UserPrompt: &frontendv1.FeedUserPrompt{},
			},
		})

	// Assert: the row lands on the ROOT feed, because the turn's address went
	// with the conversation it belonged to.
	if got := len(h.rows(rootFeed())); got != 1 {
		t.Fatalf("root feed rows = %d, want the one row the reset re-rooted", got)
	}
	if got := len(h.rows(feedid.Feed{Merge: &lease})); got != 0 {
		t.Fatalf("merge feed rows = %d, want none", got)
	}
}

func TestAResetRetractsTheStandingFinalAnswerFault(t *testing.T) {
	// Arrange: a turn that concluded naming no answer stands a fault the
	// footer draws.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "do the thing")
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		responseFrame("unit-1", &conversationv1.AgentResponseSuccess{
			Prose: &conversationv1.AgentResponseProse{Markdown: "the answer"},
		}, nil), nil, nil)
	h.concludeWithoutAnswer("turn-1")
	if len(h.faults.standing()) != 1 {
		t.Fatalf("standing faults = %+v, want one", h.faults.standing())
	}

	// Act.
	h.reset()

	// Assert: the footer stops drawing a line about a conversation that is
	// gone, and it is RETRACTED rather than merely forgotten.
	if got := h.faults.standing(); len(got) != 0 {
		t.Fatalf("standing faults = %+v, want none after the reset", got)
	}
}

func TestAResetStopsEveryArmedStallWindow(t *testing.T) {
	// Arrange: an open response fold, whose stall window is armed.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "do the thing")
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		responseFrame("unit-1", &conversationv1.AgentResponseUpdate{NewMarkdown: "half an ans"}, nil), nil, nil)
	if h.clock.live() == nil {
		t.Fatal("the arrangement armed no stall window")
	}

	// Act.
	h.reset()

	// Assert: no timer of the old conversation can raise a fault against the
	// new one.
	if armed := h.clock.live(); armed != nil {
		t.Fatal("a stall window of the old conversation survived the reset")
	}
}

func TestAResetPublishesARemovalForEveryRowOnAnOpenTail(t *testing.T) {
	// Arrange: a reader streaming the feed, with one row on it.
	h := newHarness(t)
	_, token := h.openPage(rootFeed(), "reader-1")
	h.deliverPrompt("turn-1", "the old conversation")
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	tail, err := h.resolver.Tail(ctx, testWorkspace, rootFeed(), token)
	if err != nil {
		t.Fatalf("Tail: %v", err)
	}
	rows := tail.Rows(ctx)
	if got := <-rows; got.GetId().GetValue() != h.promptRowID("turn-1") {
		t.Fatalf("streamed row = %q, want the prompt", got.GetId().GetValue())
	}

	// Act.
	h.reset()

	// Assert: the reader empties LIVE rather than keeping the old
	// conversation painted until it reloads.
	removal := <-rows
	if removal.GetId().GetValue() != h.promptRowID("turn-1") {
		t.Fatalf("removal named %q, want the prompt row", removal.GetId().GetValue())
	}
	if removal.GetRemoved() == nil {
		t.Fatalf("published %T, want a removal", removal.GetRow())
	}
}

func TestAResetKeepsAnOpenTailStreamingTheNewConversation(t *testing.T) {
	// Arrange: a reader streaming across the bind.
	h := newHarness(t)
	_, token := h.openPage(rootFeed(), "reader-1")
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	tail, err := h.resolver.Tail(ctx, testWorkspace, rootFeed(), token)
	if err != nil {
		t.Fatalf("Tail: %v", err)
	}
	rows := tail.Rows(ctx)

	// Act.
	h.reset()
	h.deliverPrompt("turn-9", "the newly bound conversation")

	// Assert: the connection is the reader's, not the conversation's.
	row := <-rows
	if row.GetId().GetValue() != h.promptRowID("turn-9") {
		t.Fatalf("streamed row = %q, want the new conversation's prompt", row.GetId().GetValue())
	}
}

func TestAResetKeepsAWatchTokenMintedBeforeIt(t *testing.T) {
	// Arrange: a token minted before the bind.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "the old conversation")
	_, token := h.openPage(rootFeed(), "reader-1")

	// Act.
	h.reset()

	// Assert: the sequence never rewound, so the pin still resolves.
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	if _, err := h.resolver.Tail(ctx, testWorkspace, rootFeed(), token); err != nil {
		t.Fatalf("Tail after the reset: %v", err)
	}
}

func TestAResetReParksAStandingWalkAtTheStartOfTheEmptiedFeed(t *testing.T) {
	// Arrange: a reader whose walk stands part-way back through the old
	// conversation.
	h := newHarness(t)
	for _, turn := range []string{"turn-1", "turn-2", "turn-3", "turn-4"} {
		h.deliverPrompt(turn, "the old conversation")
	}
	h.openPage(rootFeed(), "reader-1")

	// Act.
	h.reset()

	// Assert: a `next` answers the emptied feed rather than refusing a walk
	// the reader did begin.
	page, err := h.resolver.NextPage(context.Background(), testWorkspace, rootFeed(), "reader-1")
	if err != nil {
		t.Fatalf("NextPage after the reset: %v", err)
	}
	if got := rowIDs(pageRows(t, page)); len(got) != 0 {
		t.Fatalf("page rows = %v, want none", got)
	}
}

func TestAResetNeverReMintsASynthesizedIdentity(t *testing.T) {
	// Arrange: a non-durable command panel, whose identity is minted from the
	// per-workspace synthesized sequence.
	h := newHarness(t)
	before := h.resolver.UpsertCommandPanel(testWorkspace, &frontendv1.FeedCommandPanel{})

	// Act.
	h.reset()
	after := h.resolver.UpsertCommandPanel(testWorkspace, &frontendv1.FeedCommandPanel{})

	// Assert: a slow reader may still hold the first, so the second is never
	// the same identity.
	if before.GetValue() == after.GetValue() {
		t.Fatalf("both panels minted %q, want distinct identities", before.GetValue())
	}
}

func TestAResetRedrawsAForksPortedConversation(t *testing.T) {
	// Arrange: a fork whose ported parent conversation was drawn on its
	// opening history page.
	h := newHarness(t)
	h.ported = []PortedPrompt{{
		Turn: "parent-1", Text: "what the parent was asked",
		Origin: conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT,
	}}
	h.replay(&conversationv1.HistoryPage{})
	if len(h.rows(rootFeed())) != 1 {
		t.Fatalf("rows = %v, want the one ported prompt", rowIDs(h.rows(rootFeed())))
	}

	// Act: the reset takes the ported rows with everything else, and the next
	// opening page replays them.
	h.reset()
	h.replay(&conversationv1.HistoryPage{})

	// Assert: the ported conversation belongs to the WORKSPACE's origin, not
	// to the vendor conversation it happens to run, so it comes back.
	if got := len(h.rows(rootFeed())); got != 1 {
		t.Fatalf("rows after the reset = %d, want the ported prompt redrawn", got)
	}
}

func TestAResetOfAWorkspaceThatDrewNothingRecordsItAndChangesNothing(t *testing.T) {
	// Arrange: a workspace the resolver has never seen. (The harness's own
	// workspace is not one: its main agent was named at construction.)
	h := newHarness(t)
	unseen := ids.WorkspaceID("ws-unseen")

	// Act.
	h.resolver.ResetWorkspace(unseen, "the workspace was bound to a different conversation")

	// Assert: the reset is a fact worth a record, and it invented no state.
	if !h.hasRecord("info", opWorkspaceReset) {
		t.Fatalf("records = %+v, want an INFO %s", h.records(), opWorkspaceReset)
	}
	h.resolver.mu.Lock()
	_, held := h.resolver.workspaces[unseen]
	h.resolver.mu.Unlock()
	if held {
		t.Fatal("the reset minted state for a workspace that had drawn nothing")
	}
}

func TestAResetRecordsWhatItEmptied(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "the old conversation")

	// Act.
	h.reset()

	// Assert: the record names the rows, so a feed that went blank is
	// findable afterwards.
	for _, record := range h.records() {
		if record.Level == "info" && record.Operation == opWorkspaceReset && record.Context["rows"] == 1 {
			return
		}
	}
	t.Fatalf("records = %+v, want an INFO %s naming one row", h.records(), opWorkspaceReset)
}

// holdsRow reports whether a feed STORES this row, withheld rows included —
// which is what a reset has to be judged against, not what is delivered.
func (h *harness) holdsRow(feed feedid.Feed, id string) bool {
	h.t.Helper()
	for _, row := range h.rows(feed) {
		if row.GetId().GetValue() == id {
			return true
		}
	}
	return false
}
