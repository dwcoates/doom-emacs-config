package feed

import (
	"context"
	"fmt"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/feedid"
	"claude-repld/internal/ids"
	"claude-repld/internal/sessionwatcher"
)

// THE PROMPT ROWS. The author label is the origin's business, the drawn text
// has the host's sentinel spans stripped, and an image reference is resolved to
// a src by the DAEMON.

// promptWith sends one prompt with the given origin and blocks.
func (h *harness) promptWith(turn string, origin conversationv1.PromptOrigin, blocks ...*conversationv1.UserContentBlock) {
	h.t.Helper()
	h.resolver.OnPrompt(testWorkspace, mainAgent(), &conversationv1.AgentPrompt{
		Id:     &conversationv1.TurnId{Value: turn},
		Agent:  mainAgent(),
		Origin: origin,
		Said:   &conversationv1.UserSaid{Content: &conversationv1.UserContent{Blocks: blocks}},
	}, nil)
}

// textBlock is one typed block.
func textBlock(text string) *conversationv1.UserContentBlock {
	return &conversationv1.UserContentBlock{
		Block: &conversationv1.UserContentBlock_Text{Text: &conversationv1.TextBlock{Text: text}},
	}
}

func TestTheAuthorLabelComesFromTheOrigin(t *testing.T) {
	tests := []struct {
		name   string
		origin conversationv1.PromptOrigin
		want   string
	}{
		{
			name:   "a person typed it",
			origin: conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT,
			want:   "You",
		},
		{
			name:   "a merge conflict repair is the merge's, not the user's",
			origin: conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_CONFLICT_REPAIR,
			want:   "Merge",
		},
		{
			name:   "a displaced turn resumed by the merge is the merge's",
			origin: conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_DISPLACED_TURN_RESUME,
			want:   "Merge",
		},
		{
			name:   "a restart re-drive says so rather than posing as a fresh turn",
			origin: conversationv1.PromptOrigin_PROMPT_ORIGIN_RESUME_AFTER_RESTART,
			want:   "Resumed after restart",
		},
		{
			name:   "a workspace's initial brief is labelled as one",
			origin: conversationv1.PromptOrigin_PROMPT_ORIGIN_WORKSPACE_CREATED,
			want:   "Workspace brief",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)

			// Act.
			h.promptWith("turn-1", tc.origin, textBlock("do the thing"))

			// Assert.
			got := h.only(rootFeed()).GetUserPrompt().GetAuthor().GetLabel()
			if got != tc.want {
				t.Fatalf("author = %q, want %q", got, tc.want)
			}
		})
	}
}

func TestTheDrawnPromptHasItsSentinelSpansStripped(t *testing.T) {
	// Arrange: a resolver whose injected stripper removes the host's spans.
	h := newHarness(t)
	h.resolver.deps.StripSentinels = func(text string) string {
		return "fix the flaky test"
	}

	// Act.
	h.promptWith("turn-1", conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT_WITH_METAPROMPT,
		textBlock("<<META>>house rules<<END>>fix the flaky test"))

	// Assert: the DRAWN text alone is stripped; the client never sniffs a
	// sentinel, and the full text stays on the durable record elsewhere.
	blocks := h.only(rootFeed()).GetUserPrompt().GetSuccess().GetBody().GetBlocks()
	if got := blocks[0].GetText().GetText(); got != "fix the flaky test" {
		t.Fatalf("drawn text = %q, want the stripped text", got)
	}
}

func TestAnImageReferenceIsResolvedToADrawableSrc(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.promptWith("turn-1", conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT,
		&conversationv1.UserContentBlock{
			Block: &conversationv1.UserContentBlock_Image{Image: &conversationv1.ImageBlock{
				Location:  &conversationv1.ImageBlock_Path{Path: &conversationv1.ImageBlockPath{Path: "/tmp/shot.png"}},
				MediaType: "image/png",
			}},
		})

	// Assert: that resolution is the daemon's, never the client's.
	block := h.only(rootFeed()).GetUserPrompt().GetSuccess().GetBody().GetBlocks()[0].GetImage()
	if block.GetSrc() != "https://host/img" || block.GetAlt() != "screenshot.png" {
		t.Fatalf("image block = %+v, want the resolved src and alt", block)
	}
}

func TestAnUnresolvableImageIsDrawnAsUnsupportedAndWarned(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.resolver.deps.ResolveImage = func(*conversationv1.ImageBlock) (string, string, error) {
		return "", "", fmt.Errorf("the file is gone")
	}

	// Act.
	h.promptWith("turn-1", conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT,
		&conversationv1.UserContentBlock{
			Block: &conversationv1.UserContentBlock_Image{Image: &conversationv1.ImageBlock{MediaType: "image/png"}},
		})

	// Assert: the block is named rather than dropped, and the failure is loud.
	block := h.only(rootFeed()).GetUserPrompt().GetSuccess().GetBody().GetBlocks()[0]
	if block.GetUnsupported().GetKind() != "image" {
		t.Fatalf("block = %+v, want an unsupported image", block)
	}
	if !h.hasRecord("warn", "daemon.feed.image_unresolved") {
		t.Fatalf("records = %+v, want a WARN daemon.feed.image_unresolved", h.records())
	}
}

func TestAnUnmodeledBlockIsNamedRatherThanDropped(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.promptWith("turn-1", conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT,
		&conversationv1.UserContentBlock{
			Block: &conversationv1.UserContentBlock_Unsupported{
				Unsupported: &conversationv1.UnsupportedBlock{Kind: "vendor_widget"},
			},
		})

	// Assert.
	block := h.only(rootFeed()).GetUserPrompt().GetSuccess().GetBody().GetBlocks()[0]
	if block.GetUnsupported().GetKind() != "vendor_widget" {
		t.Fatalf("block = %+v, want the kind named", block)
	}
}

func TestABlockSequenceKeepsTheOrderThePersonComposed(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.promptWith("turn-1", conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT,
		textBlock("look at this"),
		&conversationv1.UserContentBlock{
			Block: &conversationv1.UserContentBlock_Image{Image: &conversationv1.ImageBlock{MediaType: "image/png"}},
		},
		textBlock("and fix it"))

	// Assert.
	blocks := h.only(rootFeed()).GetUserPrompt().GetSuccess().GetBody().GetBlocks()
	if len(blocks) != 3 || blocks[0].GetText() == nil || blocks[1].GetImage() == nil || blocks[2].GetText() == nil {
		t.Fatalf("blocks = %+v, want text, image, text in order", blocks)
	}
}

func TestThePromptRowIsStampedWithItsTurn(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.deliverPrompt("turn-42", "hello")

	// Assert: what a client matches its own SubmitPromptSuccess against.
	if got := h.only(rootFeed()).GetTurn().GetValue(); got != "turn-42" {
		t.Fatalf("turn stamp = %q, want turn-42", got)
	}
}

func TestAnAgentAddressedPromptIsDrawnAtBothEnds(t *testing.T) {
	// Arrange: a subagent whose bubble the resolver has seen created.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "spawn an explorer")
	created := &conversationv1.AgentId{Value: "agent-explore"}
	h.spawnSubagent("spawn-1", created, "Explore", "map the daemon")

	// Act: a prompt delivered to that subagent.
	h.resolver.OnPrompt(testWorkspace, mainAgent(), &conversationv1.AgentPrompt{
		Id:     &conversationv1.TurnId{Value: "turn-2"},
		Agent:  created,
		Origin: conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT,
		Said: &conversationv1.UserSaid{Content: &conversationv1.UserContent{
			Blocks: []*conversationv1.UserContentBlock{textBlock("also check the shim")},
		}},
	}, nil)

	// Assert: the outgoing send on the sender's feed…
	var outgoing, delivered string
	for _, row := range h.rows(rootFeed()) {
		if prompt := row.GetAgentPrompt(); prompt != nil {
			outgoing = prompt.GetAddress().GetText()
		}
	}
	for _, row := range h.rows(feedid.Feed{Agent: created}) {
		if prompt := row.GetAgentPrompt(); prompt != nil {
			delivered = prompt.GetAddress().GetText()
		}
	}
	if outgoing != "→ map the daemon" {
		t.Fatalf("sender address = %q, want the outgoing form", outgoing)
	}
	// …and the delivered prompt on the recipient's, differing ONLY in the line.
	if delivered != "from the main agent" {
		t.Fatalf("recipient address = %q, want the delivered form", delivered)
	}
}

func TestAnAgentAddressedPromptCarriesTheSameBodyAtBothEnds(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "spawn an explorer")
	created := &conversationv1.AgentId{Value: "agent-explore"}
	h.spawnSubagent("spawn-1", created, "Explore", "map the daemon")

	// Act.
	h.resolver.OnPrompt(testWorkspace, mainAgent(), &conversationv1.AgentPrompt{
		Id:     &conversationv1.TurnId{Value: "turn-2"},
		Agent:  created,
		Origin: conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT,
		Said: &conversationv1.UserSaid{Content: &conversationv1.UserContent{
			Blocks: []*conversationv1.UserContentBlock{textBlock("also check the shim")},
		}},
	}, nil)

	// Assert: ONE component, both ends.
	var senderBody, recipientBody string
	for _, row := range h.rows(rootFeed()) {
		if prompt := row.GetAgentPrompt(); prompt != nil {
			senderBody = prompt.GetBody().GetBlocks()[0].GetText().GetText()
		}
	}
	for _, row := range h.rows(feedid.Feed{Agent: created}) {
		if prompt := row.GetAgentPrompt(); prompt != nil {
			recipientBody = prompt.GetBody().GetBlocks()[0].GetText().GetText()
		}
	}
	if senderBody != "also check the shim" || recipientBody != senderBody {
		t.Fatalf("bodies = (%q, %q), want the same content at both ends", senderBody, recipientBody)
	}
}

func TestAPromptOfAnAddressedTurnStaysAUserPrompt(t *testing.T) {
	// Arrange: a merge's own turn is addressed, and the recipient is a
	// subagent the resolver knows.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "spawn an explorer")
	created := &conversationv1.AgentId{Value: "agent-explore"}
	h.spawnSubagent("spawn-1", created, "Explore", "map the daemon")
	lease := ids.LeaseID("lease-7")
	h.resolver.AddressTurn(testWorkspace, "turn-2", &sessionwatcher.OutputAddress{
		Feed: feedid.Feed{Merge: &lease},
	})

	// Act.
	h.resolver.OnPrompt(testWorkspace, mainAgent(), &conversationv1.AgentPrompt{
		Id:     &conversationv1.TurnId{Value: "turn-2"},
		Agent:  created,
		Origin: conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_CONFLICT_REPAIR,
		Said:   &conversationv1.UserSaid{Content: &conversationv1.UserContent{Blocks: []*conversationv1.UserContentBlock{textBlock("resolve it")}}},
	}, nil)

	// Assert: the address wins — the row lands on the merge feed as a labelled
	// user prompt rather than being split across two agent feeds.
	rows := h.rows(feedid.Feed{Merge: &lease})
	if len(rows) != 1 || rows[0].GetUserPrompt() == nil {
		t.Fatalf("merge feed rows = %+v, want one labelled user prompt", rows)
	}
	if got := rows[0].GetUserPrompt().GetAuthor().GetLabel(); got != "Merge" {
		t.Fatalf("author = %q, want Merge", got)
	}
}

// userPromptRows answers every user-prompt row on the root feed, for the tests
// whose subject is whether a bubble was drawn.
func (h *harness) userPromptRows() []*frontendv1.FeedRow {
	h.t.Helper()
	var out []*frontendv1.FeedRow
	for _, row := range h.rows(rootFeed()) {
		if row.GetUserPrompt() != nil {
			out = append(out, row)
		}
	}
	return out
}

// A /clear IS A DIRECTIVE, NOT A PROMPT. Its only visible outcome is the
// separation bar; the shim's own prompt frame draws no user-prompt bubble.
func TestAClearDirectiveDrawsNoPromptBubble(t *testing.T) {
	// Arrange: the daemon accepted a /clear and registered its turn.
	h := newHarness(t)
	h.resolver.OnClearReceived(testWorkspace, ids.TurnID("turn-2"))

	// Act: the shim's prompt frame for the /clear arrives.
	h.deliverPrompt("turn-2", "/clear")

	// Assert: no prompt bubble — only the bar the receipt drew.
	if got := h.userPromptRows(); len(got) != 0 {
		t.Fatalf("user-prompt rows = %d, want none for a /clear directive", len(got))
	}
}

// /compact IS ALSO A DIRECTIVE. It draws no optimistic bar and no prompt bubble;
// its divider follows the shim's compaction.
func TestACompactDirectiveDrawsNoPromptBubble(t *testing.T) {
	// Arrange: the daemon accepted a /compact and registered its turn.
	h := newHarness(t)
	h.resolver.OnCompactReceived(testWorkspace, ids.TurnID("turn-2"))

	// Act: the shim's prompt frame for the /compact arrives.
	h.deliverPrompt("turn-2", "/compact")

	// Assert: no prompt bubble.
	if got := h.userPromptRows(); len(got) != 0 {
		t.Fatalf("user-prompt rows = %d, want none for a /compact directive", len(got))
	}
}

// AN ORDINARY PROMPT STILL DRAWS ITS BUBBLE. The suppression is for directives
// alone, so a conversational prompt is unaffected.
func TestAnOrdinaryPromptStillDrawsItsBubble(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "hello there")

	// Assert.
	if got := h.userPromptRows(); len(got) != 1 {
		t.Fatalf("user-prompt rows = %d, want the ordinary prompt's bubble", len(got))
	}
}

// A RE-SENT PROMPT REPLACES ITS BUBBLE IN PLACE. The file plane writes an
// edited or re-sent version of a still-unanswered prompt onto the first
// version's row and turn (shim-sidecar convert/resend.go), so the feed receives
// the same turn's prompt again with new words: it must redraw the one bubble,
// never add a second.
func TestAPromptReServedOnItsTurnWithNewWordsRedrawsTheOneBubble(t *testing.T) {
	// Arrange: the first version is drawn.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "is that not the case?")

	// Act: the re-sent version arrives on the same turn.
	h.deliverPrompt("turn-1", "is that not the case? it does send some")

	// Assert.
	rows := h.userPromptRows()
	if len(rows) != 1 {
		t.Fatalf("user-prompt rows = %d, want the one bubble redrawn", len(rows))
	}
	blocks := rows[0].GetUserPrompt().GetSuccess().GetBody().GetBlocks()
	if len(blocks) != 1 || blocks[0].GetText().GetText() != "is that not the case? it does send some" {
		t.Fatalf("bubble holds %v, want the re-sent version's words", blocks)
	}
}

// THE DIRECTIVE'S PROMPT STAYS SUPPRESSED WHEN THE OTHER PLANE RE-DELIVERS IT
// LATE. The file plane's copy of the /clear prompt lands after the turn's
// terminal; a suppression that forgot the turn at the terminal let it draw a
// stale bubble below the bar. It must draw nothing on every delivery.
func TestAClearsPromptStaysSuppressedOnLateRedelivery(t *testing.T) {
	// Arrange: a /clear ran to completion — stream prompt suppressed, cut
	// confirmed, terminal taken.
	h := newHarness(t)
	h.resolver.OnClearReceived(testWorkspace, ids.TurnID("turn-2"))
	h.resolver.OnTurnOpened(testWorkspace, ids.TurnID("turn-2"))
	h.deliverPrompt("turn-2", "/clear")
	h.cutAt("entry-clear", clearedContextCut())
	h.terminal("turn-2", interruptedByUser(), nil)

	// Act: the file plane re-delivers the /clear prompt, now with no turn in
	// flight.
	h.deliverPrompt("turn-2", "/clear")

	// Assert: still no prompt bubble.
	if got := h.userPromptRows(); len(got) != 0 {
		t.Fatalf("user-prompt rows = %d, want none however many planes deliver the /clear", len(got))
	}
}

// A /clear FOLLOWED BY A NORMAL PROMPT LEAVES ONLY THE NORMAL PROMPT'S BUBBLE.
// The owner saw the /clear bubble render AFTER a later "hello" because the file
// plane's late /clear prompt leaked once the terminal had forgotten the turn.
func TestAClearThenANormalPromptDrawsOnlyTheNormalBubble(t *testing.T) {
	// Arrange: a /clear ran, then a normal prompt was delivered.
	h := newHarness(t)
	h.resolver.OnClearReceived(testWorkspace, ids.TurnID("turn-2"))
	h.resolver.OnTurnOpened(testWorkspace, ids.TurnID("turn-2"))
	h.deliverPrompt("turn-2", "/clear")
	h.cutAt("entry-clear", clearedContextCut())
	h.terminal("turn-2", interruptedByUser(), nil)
	h.deliverPrompt("turn-3", "hello")

	// Act: the file plane's late /clear prompt arrives after the normal one.
	h.deliverPrompt("turn-2", "/clear")

	// Assert: exactly one prompt bubble, the normal "hello".
	rows := h.userPromptRows()
	if len(rows) != 1 {
		t.Fatalf("user-prompt rows = %d, want only the normal prompt's bubble", len(rows))
	}
	if rows[0].GetId().GetValue() != h.promptRowID("turn-3") {
		t.Fatalf("prompt row = %q, want the normal prompt turn-3", rows[0].GetId().GetValue())
	}
}

// promptWorking answers the working flag on the turn's user-prompt row.
func (h *harness) promptWorking(turn string) bool {
	h.t.Helper()
	for _, row := range h.rows(rootFeed()) {
		if row.GetId().GetValue() == h.promptRowID(turn) {
			return row.GetUserPrompt().GetWorking()
		}
	}
	h.t.Fatalf("no user-prompt row for turn %q", turn)
	return false
}

// completed is a concluded terminal naming UNIT as the answer ("" names none).
func completed(unit string) *conversationv1.AgentSuccess {
	done := &conversationv1.AgentCompleted{}
	if unit != "" {
		done.Answer = &conversationv1.AgentActivityId{Value: unit}
	}
	return &conversationv1.AgentSuccess{
		Outcome: &conversationv1.AgentSuccess_Completed{Completed: done},
	}
}

// THE PROMPT WORKS FROM ITS DRAW UNTIL ITS TURN'S TERMINAL, and nothing else
// moves it: an interim response and a final answer drawn before the terminal
// names it leave it working; every way the turn can end settles it.
func TestAPromptRowWorksUntilItsTurnsTerminal(t *testing.T) {
	interim := func(h *harness) {
		h.resolver.OnActivity(testWorkspace, mainAgent(),
			responseFrame("unit-1", &conversationv1.AgentResponseUpdate{}, nil), nil, nil)
	}
	answer := func(h *harness) {
		h.resolver.OnActivity(testWorkspace, mainAgent(),
			responseSuccessActivity("unit-2", "the answer"), nil, nil)
	}
	cases := []struct {
		name        string
		act         func(h *harness)
		wantWorking bool
	}{
		{name: "the prompt alone", act: func(*harness) {}, wantWorking: true},
		{name: "an interim response", act: interim, wantWorking: true},
		{name: "a final answer drawn before the terminal", act: func(h *harness) {
			interim(h)
			answer(h)
		}, wantWorking: true},
		{name: "a concluded terminal", act: func(h *harness) {
			answer(h)
			h.terminal("turn-1", completed("unit-2"), nil)
		}, wantWorking: false},
		{name: "an errored terminal", act: func(h *harness) {
			h.terminal("turn-1", nil, &conversationv1.AgentFailure{
				Failure: &conversationv1.AgentFailure_ExecutionError{ExecutionError: &conversationv1.AgentExecutionError{}},
			})
		}, wantWorking: false},
		{name: "an interrupt", act: func(h *harness) {
			h.terminal("turn-1", interruptedByUser(), nil)
		}, wantWorking: false},
		{name: "a query death", act: func(h *harness) {
			h.queryDied(&conversationv1.SessionQueryDied{})
		}, wantWorking: false},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: the turn's prompt, working from its draw.
			h := newHarness(t)
			h.deliverPrompt("turn-1", "do the thing")
			if !h.promptWorking("turn-1") {
				t.Fatal("precondition: the prompt of an open turn is not working")
			}

			// Act.
			tc.act(h)

			// Assert.
			if got := h.promptWorking("turn-1"); got != tc.wantWorking {
				t.Fatalf("working = %v, want %v", got, tc.wantWorking)
			}
		})
	}
}

// THE SETTLE IS A PUBLICATION on the terminal's edge, not only a stored fact: a
// reader already following the feed is pushed the prompt row, no longer working.
func TestATurnsTerminalRepublishesItsPromptSettled(t *testing.T) {
	// Arrange: a reader follows the feed while the turn works.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "do the thing")
	if !h.promptWorking("turn-1") {
		t.Fatal("precondition: the prompt of an open turn is not working")
	}
	_, token := h.openPage(rootFeed(), "reader-1")
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	tail, err := h.resolver.Tail(ctx, testWorkspace, rootFeed(), token)
	if err != nil {
		t.Fatalf("Tail: %v", err)
	}
	rows := tail.Rows(ctx)

	// Act: the terminal, then a marker row that bounds the read.
	h.terminal("turn-1", interruptedByUser(), nil)
	h.deliverPrompt("turn-2", "marker")

	// Assert: the prompt row was pushed again, settled, before the marker.
	for row := range rows {
		switch row.GetId().GetValue() {
		case h.promptRowID("turn-1"):
			if row.GetUserPrompt().GetWorking() {
				t.Fatal("the republished prompt row is still working")
			}
			return
		case h.promptRowID("turn-2"):
			t.Fatal("the terminal did not republish the prompt row")
		}
	}
	t.Fatal("the tail closed before the marker row")
}

// A PROMPT OF A TURN THE RESOLVER HAS SEEN END ARRIVES SETTLED: a later replay
// of it never stands it back up, not even for one publication.
func TestAReplayedPromptOfAnEndedTurnArrivesSettled(t *testing.T) {
	// Arrange: the turn was watched to its end, and a reader follows the feed.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "do the thing")
	h.terminal("turn-1", completed(""), nil)
	_, token := h.openPage(rootFeed(), "reader-1")
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	tail, err := h.resolver.Tail(ctx, testWorkspace, rootFeed(), token)
	if err != nil {
		t.Fatalf("Tail: %v", err)
	}
	rows := tail.Rows(ctx)

	// Act: the opening page replays the prompt (with an edited text, so the
	// replay is a publication rather than churn), then a marker row behind it.
	h.replay(historyPage(&conversationv1.HistoryFloor{},
		promptEntry("turn-1", "do the thing, replayed"),
	))
	h.deliverPrompt("turn-2", "marker")

	// Assert: the replayed prompt was published settled.
	for row := range rows {
		switch row.GetId().GetValue() {
		case h.promptRowID("turn-1"):
			if row.GetUserPrompt().GetWorking() {
				t.Fatal("the replayed prompt of an ended turn was published working")
			}
		case h.promptRowID("turn-2"):
			return
		}
	}
	t.Fatal("the tail closed before the marker row")
}

// A FRESH RESOLVER REPLAYING A WHOLE TURN — prompt, then terminal — leaves the
// prompt settled, as a reload or reconnect draws it.
func TestAReplayedEndedTurnLeavesItsPromptSettled(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	h.replay(historyPage(&conversationv1.HistoryFloor{},
		frameEntry(mainAgent(), completed("")),
		promptEntry("turn-1", "do the thing"),
	))

	// Assert.
	if h.promptWorking("turn-1") {
		t.Fatal("a replayed ended turn's prompt is still working")
	}
}

// THE QUEUE'S ACCEPTED PROMPT IS STAMPED ON THE SAME PATH as the resolver's own draw: an
// accepted prompt of an open turn works.
func TestAnAcceptedPromptOfAnOpenTurnWorks(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	row := &frontendv1.FeedRow{
		Turn: &conversationv1.TurnId{Value: "turn-1"},
		Row: &frontendv1.FeedRow_UserPrompt{UserPrompt: &frontendv1.FeedUserPrompt{
			Author: &frontendv1.FeedUserPromptAuthor{Label: "You"},
		}},
	}

	// Act.
	h.resolver.UpsertAtTurnAddress(testWorkspace, "turn-1", feedid.RowKey{Kind: feedid.KindPrompt, ID: "turn-1"}, row)

	// Assert.
	if !h.promptWorking("turn-1") {
		t.Fatal("the accepted prompt of an open turn is not working")
	}
}

// A PROMPT NAMING NO TURN has no turn to wait on, so it is not working.
func TestAPromptNamingNoTurnIsNotWorking(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	row := &frontendv1.FeedRow{
		Row: &frontendv1.FeedRow_UserPrompt{UserPrompt: &frontendv1.FeedUserPrompt{
			Author: &frontendv1.FeedUserPromptAuthor{Label: "You"},
		}},
	}

	// Act.
	h.resolver.UpsertAtTurnAddress(testWorkspace, "unturned", feedid.RowKey{Kind: feedid.KindPrompt, ID: "unturned"}, row)

	// Assert.
	if h.only(rootFeed()).GetUserPrompt().GetWorking() {
		t.Fatal("a prompt naming no turn is working")
	}
}

// AN AGENT PROMPT FOLLOWS THE TURN IT IS STAMPED WITH: a spawn's commission,
// drawn on the subagent's feed during the main turn, settles at that turn's
// terminal like the user's own prompt.
func TestAnAgentPromptSettlesAtItsTurnsTerminal(t *testing.T) {
	cases := []struct {
		name        string
		end         bool
		wantWorking bool
	}{
		{name: "while the turn works", end: false, wantWorking: true},
		{name: "after the turn's terminal", end: true, wantWorking: false},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			h.deliverPrompt("turn-1", "spawn an explorer")
			created := &conversationv1.AgentId{Value: "agent-explore"}
			h.spawnSubagent("spawn-1", created, "Explore", "map the daemon")
			commission := func() *frontendv1.FeedAgentPrompt {
				for _, row := range h.rows(feedid.Feed{Agent: created}) {
					if prompt := row.GetAgentPrompt(); prompt != nil {
						return prompt
					}
				}
				t.Fatal("no commission row on the subagent's feed")
				return nil
			}
			if !commission().GetWorking() {
				t.Fatal("precondition: the commission of an open turn is not working")
			}

			// Act.
			if tc.end {
				h.terminal("turn-1", completed(""), nil)
			}

			// Assert.
			if got := commission().GetWorking(); got != tc.wantWorking {
				t.Fatalf("working = %v, want %v", got, tc.wantWorking)
			}
		})
	}
}

// A VENDOR-STARTED TURN'S PROMPT ROW IS AN EDGE, NOT WORDS. The shim writes it
// only to open a turn the vendor began on its own, so it draws no bubble.
func TestAVendorStartedTurnDrawsNoPromptBubble(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	h.promptWith("turn-v", conversationv1.PromptOrigin_PROMPT_ORIGIN_VENDOR_STARTED)

	// Assert.
	if got := h.userPromptRows(); len(got) != 0 {
		t.Fatalf("user-prompt rows = %d, want none for a vendor-started turn", len(got))
	}
}

// THE SUPPRESSED ROW STILL OPENS ITS TURN, so the turn's rows are stamped with
// it and its terminal draws the ending and marks the final answer.
func TestAVendorStartedPromptOpensItsTurn(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	h.promptWith("turn-v", conversationv1.PromptOrigin_PROMPT_ORIGIN_VENDOR_STARTED)

	// Assert.
	if turn := h.resolver.state(testWorkspace).turnInFlight; turn == nil || *turn != "turn-v" {
		t.Fatalf("turn in flight = %v, want the vendor-started turn", turn)
	}
}

// NO READER NAMES THE USER AS THE AUTHOR of a turn the vendor began.
func TestTheAuthorLabelOfAVendorStartedTurnIsTheVendor(t *testing.T) {
	// Arrange, Act.
	got := AuthorLabel(conversationv1.PromptOrigin_PROMPT_ORIGIN_VENDOR_STARTED)

	// Assert.
	if got != "Vendor" {
		t.Fatalf("author = %q, want Vendor", got)
	}
}

// deliverFoldedPrompt draws a prompt the vendor folded into INTO at a tool
// boundary.
func (h *harness) deliverFoldedPrompt(turn, into, text string) {
	h.t.Helper()
	h.resolver.OnPrompt(testWorkspace, mainAgent(), &conversationv1.AgentPrompt{
		Id:         &conversationv1.TurnId{Value: turn},
		Agent:      mainAgent(),
		Origin:     conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT,
		FoldedInto: &conversationv1.TurnId{Value: into},
		Said: &conversationv1.UserSaid{Content: &conversationv1.UserContent{
			Blocks: []*conversationv1.UserContentBlock{textBlock(text)},
		}},
	}, nil)
}

// promptRow is the user-prompt row keyed by TURN on the root feed.
func (h *harness) promptRow(turn string) *frontendv1.FeedRow {
	h.t.Helper()
	for _, row := range h.rows(rootFeed()) {
		if row.GetId().GetValue() == h.promptRowID(turn) {
			return row
		}
	}
	h.t.Fatalf("no user-prompt row for turn %q", turn)
	return nil
}

func TestAFoldedPromptIsStampedWithTheTurnItJoined(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.deliverPrompt("turn-1", "port the footer")

	// Act
	h.deliverFoldedPrompt("turn-2", "turn-1", "also cover the edge case")

	// Assert
	if got := h.promptRow("turn-2").GetTurn().GetValue(); got != "turn-1" {
		t.Fatalf("turn stamp = %q, want the joined turn-1", got)
	}
}

func TestAFoldedPromptLeavesTheRunningTurnStanding(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.deliverPrompt("turn-1", "port the footer")

	// Act
	h.deliverFoldedPrompt("turn-2", "turn-1", "also cover the edge case")

	// Assert
	h.resolver.mu.Lock()
	running := h.resolver.state(testWorkspace).turnInFlight
	h.resolver.mu.Unlock()
	if running == nil || *running != "turn-1" {
		t.Fatalf("turn in flight = %v, want turn-1 still running", running)
	}
}

func TestAFoldedPromptWorksWhileTheTurnItJoinedRuns(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.deliverPrompt("turn-1", "port the footer")

	// Act
	h.deliverFoldedPrompt("turn-2", "turn-1", "also cover the edge case")

	// Assert
	if !h.promptRow("turn-2").GetUserPrompt().GetWorking() {
		t.Fatal("a folded prompt must work while the turn it joined runs")
	}
}

func TestTheJoinedTurnsTerminalSettlesTheFoldedPrompt(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.deliverPrompt("turn-1", "port the footer")
	h.deliverFoldedPrompt("turn-2", "turn-1", "also cover the edge case")

	// Act
	h.terminal("turn-1", completed(""), nil)

	// Assert
	if h.promptRow("turn-2").GetUserPrompt().GetWorking() {
		t.Fatal("the joined turn's terminal must settle the folded prompt")
	}
}
