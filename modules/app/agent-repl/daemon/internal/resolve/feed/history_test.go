package feed

import (
	"context"
	"errors"
	"fmt"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/feedid"
	"claude-repld/internal/ids"
	"claude-repld/internal/sessionwatcher"
	"claude-repld/internal/wsm"
)

// HISTORY REPLAY goes through THE SAME per-family functions a live frame does,
// so a replayed row and a live one cannot differ.

// historyPage builds a page from entries stated NEWEST FIRST, as the producer
// serves them.
func historyPage(boundary any, newestFirst ...*conversationv1.HistoryEntry) *conversationv1.HistoryPage {
	page := &conversationv1.HistoryPage{}
	for i, entry := range newestFirst {
		page.Entries = append(page.Entries, &conversationv1.HistoryEntryAt{
			At:    &conversationv1.HistoryPointer{Value: string(rune('a' + i))},
			Entry: entry,
		})
	}
	switch b := boundary.(type) {
	case *conversationv1.HistoryFloor:
		page.Boundary = &conversationv1.HistoryPage_Floor{Floor: b}
	case *conversationv1.HistoryMore:
		page.Boundary = &conversationv1.HistoryPage_More{More: b}
	}
	return page
}

// promptEntry is a replayed prompt.
func promptEntry(turn, text string) *conversationv1.HistoryEntry {
	return &conversationv1.HistoryEntry{
		Entry: &conversationv1.HistoryEntry_UserPrompt{UserPrompt: &conversationv1.AgentPrompt{
			Id:     &conversationv1.TurnId{Value: turn},
			Agent:  mainAgent(),
			Origin: conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT,
			Said: &conversationv1.UserSaid{Content: &conversationv1.UserContent{
				Blocks: []*conversationv1.UserContentBlock{textBlock(text)},
			}},
		}},
	}
}

// frameEntry is a replayed agent frame.
func frameEntry(agent *conversationv1.AgentId, result any) *conversationv1.HistoryEntry {
	frame := &conversationv1.AgentFrame{AgentId: agent}
	switch r := result.(type) {
	case *conversationv1.AgentUpdate:
		frame.Result = &conversationv1.AgentFrame_Update{Update: r}
	case *conversationv1.AgentSuccess:
		frame.Result = &conversationv1.AgentFrame_Success{Success: r}
	case *conversationv1.AgentFailure:
		frame.Result = &conversationv1.AgentFrame_Failure{Failure: r}
	case *conversationv1.AgentDetachedWork:
		frame.Result = &conversationv1.AgentFrame_DetachedWork{DetachedWork: r}
	}
	return &conversationv1.HistoryEntry{
		Entry: &conversationv1.HistoryEntry_AgentFrame{AgentFrame: frame},
	}
}

// replay pushes one history page through the sink.
func (h *harness) replay(page *conversationv1.HistoryPage) {
	h.t.Helper()
	h.resolver.OnHistoryPage(testWorkspace, mainAgent(), page, noAddress())
}

// cutEntry is a replayed context cut.
func cutEntry(cut *conversationv1.ContextCut) *conversationv1.HistoryEntry {
	return frameEntry(mainAgent(), &conversationv1.AgentUpdate{
		Update: &conversationv1.AgentUpdate_ContextCut{ContextCut: cut},
	})
}

// multiCutPage is a replayed history carrying N compactions — turn-0, cut-1,
// turn-1, …, cut-N, turn-N, oldest first — served NEWEST FIRST as a page is,
// every entry at a pointer of its own ("p-<oldest-first index>").
type multiCutPage struct {
	page *conversationv1.HistoryPage
	cuts int
}

// newMultiCutPage builds the page for N cuts.
func newMultiCutPage(cuts int) multiCutPage {
	var oldestFirst []*conversationv1.HistoryEntryAt
	add := func(entry *conversationv1.HistoryEntry) {
		oldestFirst = append(oldestFirst, &conversationv1.HistoryEntryAt{
			At:    &conversationv1.HistoryPointer{Value: fmt.Sprintf("p-%d", len(oldestFirst))},
			Entry: entry,
		})
	}
	add(promptEntry("turn-0", "before every cut"))
	for i := 1; i <= cuts; i++ {
		add(cutEntry(compactedCut(fmt.Sprintf("summary %d", i))))
		add(promptEntry(fmt.Sprintf("turn-%d", i), fmt.Sprintf("after cut %d", i)))
	}
	page := &conversationv1.HistoryPage{Boundary: &conversationv1.HistoryPage_Floor{Floor: &conversationv1.HistoryFloor{}}}
	for i := len(oldestFirst) - 1; i >= 0; i-- {
		page.Entries = append(page.Entries, oldestFirst[i])
	}
	return multiCutPage{page: page, cuts: cuts}
}

// cutPointer is the pointer the Ith cut (1-based) sits at.
func (m multiCutPage) cutPointer(i int) string { return fmt.Sprintf("p-%d", 2*i-1) }

// newestCut is the pointer of the page's newest cut.
func (m multiCutPage) newestCut() string { return m.cutPointer(m.cuts) }

// newestTurn is the one turn after the newest cut.
func (m multiCutPage) newestTurn() string { return fmt.Sprintf("turn-%d", m.cuts) }

// aboveNewestCut is the id of every row the newest cut withholds: each older
// turn's prompt and each older cut's divider.
func (m multiCutPage) aboveNewestCut(h *harness) []string {
	var out []string
	for i := 0; i < m.cuts; i++ {
		out = append(out, h.promptRowID(fmt.Sprintf("turn-%d", i)))
	}
	for i := 1; i < m.cuts; i++ {
		out = append(out, h.separationRowID(m.cutPointer(i)))
	}
	return out
}

// THE PAGE'S NEWEST CUT IS ESTABLISHED BEFORE ANY OF ITS ROWS IS SERVED. A page
// is drawn oldest first, so published as drawn every row above its newest cut
// reached a following reader before that cut did, and the reader drew it and
// then truncated it at the cut. Each case is one history; the assertion is the
// exact push a reader following the feed receives.
func TestAReplayPushesOnlyWhatFollowsItsNewestCut(t *testing.T) {
	cases := []struct {
		name string
		cuts int
	}{
		{name: "one cut", cuts: 1},
		{name: "two cuts", cuts: 2},
		{name: "sixteen cuts", cuts: 16},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: a reader following the feed before the replay.
			h := newHarness(t)
			rows := h.follow(rootFeed(), "reader-1")
			page := newMultiCutPage(tc.cuts)

			// Act.
			h.replay(page.page)
			got := pushedBefore(t, rows, h.sendSentinel())

			// Assert: the newest divider first — so a reader truncates nothing —
			// and then the one turn after it.
			want := []string{h.separationRowID(page.newestCut()), h.promptRowID(page.newestTurn())}
			if !equalIDs(got, want) {
				t.Fatalf("pushed = %v, want %v", got, want)
			}
		})
	}
}

func TestAReplayWithManyCutsServesNothingAboveItsNewestCutOnTheFirstPage(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	page := newMultiCutPage(16)
	h.replay(page.page)

	// Act.
	served, _ := h.openPage(rootFeed(), "reader-1")

	// Assert.
	withheld := map[string]bool{}
	for _, id := range page.aboveNewestCut(h) {
		withheld[id] = true
	}
	for _, id := range rowIDs(pageRows(t, served)) {
		if withheld[id] {
			t.Fatalf("the first page served %q from above the newest cut", id)
		}
	}
}

func TestPagingBackThroughAReplayedHistoryWalksEveryPostCutRowAndEndsAtTheCut(t *testing.T) {
	// Arrange: one cut, then more turns than a page holds (the harness page
	// size is 3), so reaching the cut takes a walk.
	h := newHarness(t)
	oldestFirst := []*conversationv1.HistoryEntry{
		promptEntry("turn-old", "above the cut"),
		cutEntry(compactedCut("what survived")),
	}
	for i := 1; i <= 5; i++ {
		oldestFirst = append(oldestFirst, promptEntry(fmt.Sprintf("turn-%d", i), "after the cut"))
	}
	var newestFirst []*conversationv1.HistoryEntry
	for i := len(oldestFirst) - 1; i >= 0; i-- {
		newestFirst = append(newestFirst, oldestFirst[i])
	}
	page := historyPage(&conversationv1.HistoryFloor{}, newestFirst...)
	h.replay(page)
	cutPointer := page.Entries[len(newestFirst)-2].GetAt().GetValue()

	// Act: the first page, then every older page until the walk stands at the
	// start.
	first, _ := h.openPage(rootFeed(), "reader-1")
	served := rowIDs(pageRows(t, first))
	edge := first.GetResult().(*frontendv1.FeedPage_Success).Success.GetEdge()
	for walks := 0; ; walks++ {
		if _, atStart := edge.(*frontendv1.FeedPageSuccess_AtStart); atStart {
			break
		}
		if walks > 3 {
			t.Fatalf("the walk never reached the start; served %v", served)
		}
		older, err := h.resolver.NextPage(context.Background(), testWorkspace, rootFeed(), "reader-1")
		if err != nil {
			t.Fatalf("NextPage: %v", err)
		}
		served = append(rowIDs(pageRows(t, older)), served...)
		edge = older.GetResult().(*frontendv1.FeedPage_Success).Success.GetEdge()
	}

	// Assert: the divider and every turn after it, oldest first, and nothing
	// from above the cut.
	want := []string{h.separationRowID(cutPointer)}
	for i := 1; i <= 5; i++ {
		want = append(want, h.promptRowID(fmt.Sprintf("turn-%d", i)))
	}
	if !equalIDs(served, want) {
		t.Fatalf("walked rows = %v, want %v", served, want)
	}
}

func TestAReplayedCompactionThatFailedWithholdsNothingFromThePush(t *testing.T) {
	// Arrange: a reader following the feed.
	h := newHarness(t)
	rows := h.follow(rootFeed(), "reader-1")
	page := historyPage(&conversationv1.HistoryFloor{},
		promptEntry("turn-2", "after"),
		cutEntry(&conversationv1.ContextCut{Cut: &conversationv1.ContextCut_CompactionFailed{
			CompactionFailed: &conversationv1.ContextCompactionFailed{Error: "the summarizer refused"},
		}}),
		promptEntry("turn-1", "before"),
	)

	// Act.
	h.replay(page)
	got := pushedBefore(t, rows, h.sendSentinel())

	// Assert: the turn above the failed compaction reaches the reader.
	pushed := false
	for _, id := range got {
		if id == h.promptRowID("turn-1") {
			pushed = true
		}
	}
	if !pushed {
		t.Fatalf("pushed = %v, want the turn above a compaction that cut nothing", got)
	}
}

func TestAReplayedPostCutConversationIsPushedWhole(t *testing.T) {
	// Arrange: a reconnect's catch-up page carries only the conversation AFTER
	// a compaction whose cut is not on the page; a reader is following.
	h := newHarness(t)
	rows := h.follow(rootFeed(), "reader-1")

	// Act.
	h.replay(historyPage(&conversationv1.HistoryFloor{},
		promptEntry("turn-7", "after two"),
		promptEntry("turn-6", "after one"),
	))
	got := pushedBefore(t, rows, h.sendSentinel())

	// Assert: holding the page back until it is placed withholds nothing the
	// bound keeps.
	want := []string{h.promptRowID("turn-6"), h.promptRowID("turn-7")}
	if !equalIDs(got, want) {
		t.Fatalf("pushed = %v, want %v", got, want)
	}
}

func TestALiveCutAfterAReplayIsPushedAtOnce(t *testing.T) {
	// Arrange: a replayed conversation, a reader following, and then a live
	// compaction — the replay's hold must have ended with its page.
	h := newHarness(t)
	h.replay(historyPage(&conversationv1.HistoryFloor{}, promptEntry("turn-1", "replayed")))
	rows := h.follow(rootFeed(), "reader-1")

	// Act.
	h.cutAt("entry-live", compactedCut("what survived"))
	got := pushedBefore(t, rows, h.sendSentinel())

	// Assert.
	want := []string{h.separationRowID("entry-live")}
	if !equalIDs(got, want) {
		t.Fatalf("pushed = %v, want the live divider alone", got)
	}
}

func TestAReplayLandsRowsOldestFirstEvenThoughThePageIsNewestFirst(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.replay(historyPage(&conversationv1.HistoryFloor{},
		promptEntry("turn-3", "third"),
		promptEntry("turn-2", "second"),
		promptEntry("turn-1", "first"),
	))

	// Assert: the feed's order is oldest → newest.
	got := rowIDs(h.rows(rootFeed()))
	want := []string{h.promptRowID("turn-1"), h.promptRowID("turn-2"), h.promptRowID("turn-3")}
	for i := range want {
		if got[i] != want[i] {
			t.Fatalf("rows = %v, want %v", got, want)
		}
	}
}

func TestAReplayedPromptIsDrawnByTheSameFamilyAsALiveOne(t *testing.T) {
	// Arrange: the same prompt, once replayed and once live, in two resolvers.
	replayed := newHarness(t)
	replayed.replay(historyPage(&conversationv1.HistoryFloor{}, promptEntry("turn-1", "hello")))
	live := newHarness(t)
	live.deliverPrompt("turn-1", "hello")

	// Act, Assert: identical identity and identical drawing.
	replayedRow := replayed.only(rootFeed())
	liveRow := live.only(rootFeed())
	if replayedRow.GetId().GetValue() != liveRow.GetId().GetValue() {
		t.Fatalf("ids differ: replayed %q, live %q",
			replayedRow.GetId().GetValue(), liveRow.GetId().GetValue())
	}
	if replayedRow.GetUserPrompt().GetAuthor().GetLabel() != liveRow.GetUserPrompt().GetAuthor().GetLabel() {
		t.Fatal("a replayed prompt drew a different author label from a live one")
	}
}

func TestAReplayedSettledActivityDrawsItsCard(t *testing.T) {
	// Arrange, Act: no `start` is replayed — the settled frame carries the
	// start's facts.
	h := newHarness(t)
	h.replay(historyPage(&conversationv1.HistoryFloor{},
		frameEntry(mainAgent(), &conversationv1.AgentUpdate{
			Update: &conversationv1.AgentUpdate_Activity{
				Activity: activityOf("unit-1", &conversationv1.AgentRead{
					Result: &conversationv1.AgentRead_Success{Success: &conversationv1.AgentReadSuccess{
						Path: &conversationv1.ReadPath{Path: "row.go"},
						Extent: &conversationv1.AgentReadSuccess_Whole{
							Whole: &conversationv1.AgentReadWhole{Contents: "package feed"},
						},
					}},
				}),
			},
		}),
	))

	// Assert.
	card := h.card()
	if card.GetName().GetText() != "Read" || card.GetReturned() == nil {
		t.Fatalf("card = %+v, want a settled read", card)
	}
}

func TestAFrameIsAttributedByItsOwnAgentIdAndNotThePagesAgent(t *testing.T) {
	// Arrange: a page requested for the main agent that carries a subagent's
	// frame — frames are FLAT and their agent_id is the whole attribution.
	h := newHarness(t)
	created := &conversationv1.AgentId{Value: "agent-explore"}
	h.deliverPrompt("turn-1", "spawn one")
	h.spawnSubagent("spawn-1", created, "Explore", "map it")

	// Act.
	h.replay(historyPage(&conversationv1.HistoryFloor{},
		frameEntry(created, &conversationv1.AgentUpdate{
			Update: &conversationv1.AgentUpdate_Activity{
				Activity: responseSuccessActivity("unit-9", "what I found"),
			},
		}),
	))

	// Assert: on the subagent's sub-feed. The row is SOUGHT rather than taken
	// from the end, because this test's subject is ATTRIBUTION and a replayed
	// row's PLACE is the ordering rule's subject: a history-plane row stands
	// above the commission the arrangement drew live.
	rows := h.rows(feedid.Feed{Agent: created})
	var prose *frontendv1.FeedRow
	for _, row := range rows {
		if row.GetActivity().GetResponse() != nil {
			prose = row
		}
	}
	if prose == nil {
		t.Fatalf("sub-feed rows = %+v, want the subagent's prose", rows)
	}
}

func TestAReplayedTerminalDrawsTheTurnItStandsIn(t *testing.T) {
	// Arrange, Act: the prompt establishes the turn, then its terminal.
	h := newHarness(t)
	h.replay(historyPage(&conversationv1.HistoryFloor{},
		frameEntry(mainAgent(), &conversationv1.AgentSuccess{
			Outcome: &conversationv1.AgentSuccess_Completed{Completed: &conversationv1.AgentCompleted{}},
		}),
		promptEntry("turn-1", "hello"),
	))

	// Assert: history replays how the turn ended.
	if h.terminalRow("turn-1").GetConcluded() == nil {
		t.Fatalf("outcome = %T, want concluded", h.terminalRow("turn-1").GetOutcome())
	}
}

// THE PAGE'S HEAD CAN OPEN MID-TURN. Every turn open repaints the opening page,
// and that page's oldest terminal can end a turn whose prompt is older than the
// page; it must never be charged to the turn the queue has just opened.
func TestAReplayedTerminalBeforeEveryPromptDoesNotEndTheLiveTurn(t *testing.T) {
	cases := []struct {
		name string
		page *conversationv1.HistoryPage
	}{
		{
			name: "the live turn's prompt is on the page",
			page: historyPage(&conversationv1.HistoryMore{},
				promptEntry("turn-live", "now"),
				frameEntry(mainAgent(), &conversationv1.AgentSuccess{
					Outcome: &conversationv1.AgentSuccess_Completed{Completed: &conversationv1.AgentCompleted{}},
				}),
			),
		},
		{
			name: "the live turn's prompt is not on the page yet",
			page: historyPage(&conversationv1.HistoryMore{},
				frameEntry(mainAgent(), &conversationv1.AgentSuccess{
					Outcome: &conversationv1.AgentSuccess_Completed{Completed: &conversationv1.AgentCompleted{}},
				}),
			),
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: the queue opened the live turn.
			h := newHarness(t)
			h.resolver.OnTurnOpened(testWorkspace, ids.TurnID("turn-live"))

			// Act: the opening page replays with an orphan terminal at its head.
			h.replay(tc.page)

			// Assert: the live turn has not ended.
			if h.hasTerminalRow("turn-live") {
				t.Fatal("the page's orphan terminal was drawn as the live turn's terminal")
			}
		})
	}
}

func TestAReplayedContextCutDrawsItsDivider(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.replay(historyPage(&conversationv1.HistoryFloor{},
		frameEntry(mainAgent(), &conversationv1.AgentUpdate{
			Update: &conversationv1.AgentUpdate_ContextCut{
				ContextCut: &conversationv1.ContextCut{
					Cut: &conversationv1.ContextCut_Cleared{Cleared: &conversationv1.ContextCleared{}},
				},
			},
		}),
	))

	// Assert.
	if h.separationRow().GetSeparation().GetCleared() == nil {
		t.Fatal("a replayed clear drew no divider")
	}
}

func TestAReplayedApiErrorStaysEvidenceAndNeverBecomesATerminal(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.replay(historyPage(&conversationv1.HistoryFloor{},
		frameEntry(mainAgent(), &conversationv1.AgentUpdate{
			Update: &conversationv1.AgentUpdate_ApiError{
				ApiError: &conversationv1.ApiRequestFailed{Message: "connection reset"},
			},
		}),
		promptEntry("turn-1", "hello"),
	))

	// Assert: the prompt alone — replaying it as a terminal would invent a turn
	// ending.
	rows := h.rows(rootFeed())
	if len(rows) != 1 || rows[0].GetUserPrompt() == nil {
		t.Fatalf("rows = %+v, want the prompt alone", rowIDs(rows))
	}
}

func TestAReplayedContextBudgetWarningDrawsNoRow(t *testing.T) {
	// Arrange, Act: the vendor's own budget warning is a PAGE LINE whose home
	// is the footer's activity line.
	h := newHarness(t)
	h.replay(historyPage(&conversationv1.HistoryFloor{},
		frameEntry(mainAgent(), &conversationv1.AgentUpdate{
			Update: &conversationv1.AgentUpdate_ContextBudgetWarning{
				ContextBudgetWarning: &conversationv1.ContextBudgetWarning{
					Text: "the context window is filling",
				},
			},
		}),
	))

	// Assert: no row — and not the unset-arm warning either, because the arm
	// is handled and simply draws nothing.
	if rows := h.rows(rootFeed()); len(rows) != 0 {
		t.Fatalf("rows = %d, want none", len(rows))
	}
	if h.hasRecord("warn", "daemon.feed.history_update_unset") {
		t.Fatalf("records = %+v, want the arm handled rather than unrecognized", h.records())
	}
	if !h.hasRecord("debug", "daemon.feed.context_budget_warning_draws_nothing") {
		t.Fatalf("records = %+v, want the draws-nothing branch recorded", h.records())
	}
}

func TestAReplayedQuestionAndPermissionDrawTheirCards(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.replay(historyPage(&conversationv1.HistoryFloor{},
		frameEntry(mainAgent(), &conversationv1.AgentUpdate{
			Update: &conversationv1.AgentUpdate_Permission{
				Permission: &conversationv1.AgentPermission{
					Id:        &conversationv1.AgentPermissionId{Value: "ask-2"},
					GatedCall: &conversationv1.AgentActivityId{Value: "unit-1"},
					Result: &conversationv1.AgentPermission_Start{Start: &conversationv1.AgentPermissionStart{
						Prompt:    &conversationv1.AgentPermissionPrompt{Title: "Claude wants to read foo.txt"},
						StartedAt: &conversationv1.AgentActivityStartedAt{AtMs: 1_000},
					}},
				},
			},
		}),
		frameEntry(mainAgent(), &conversationv1.AgentUpdate{
			Update: &conversationv1.AgentUpdate_Question{
				Question: &conversationv1.AgentQuestion{
					Id: &conversationv1.AgentQuestionId{Value: "ask-1"},
					Result: &conversationv1.AgentQuestion_Start{Start: &conversationv1.AgentQuestionStart{
						Batch:     twoQuestionBatch(),
						StartedAt: &conversationv1.AgentActivityStartedAt{AtMs: 1_000},
					}},
				},
			},
		}),
	))

	// Assert.
	if h.questionCard().GetOpen() == nil {
		t.Fatal("a replayed question drew no open card")
	}
	if h.permissionCard().GetOpen() == nil {
		t.Fatal("a replayed permission drew no open card")
	}
}

func TestAReplayedDetachedWorkDrawsItsBubble(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	created := &conversationv1.AgentId{Value: "agent-remote"}
	h.replay(historyPage(&conversationv1.HistoryFloor{},
		frameEntry(mainAgent(), &conversationv1.AgentDetachedWork{
			Work:  &conversationv1.DetachedWorkId{Value: "work-1"},
			Owner: mainAgent(),
			Origin: &conversationv1.AgentDetachedWork_Created{Created: &conversationv1.DetachedWorkCreated{
				WorkCreated: &conversationv1.DetachableWork{
					Work: &conversationv1.DetachableWork_Subagent{Subagent: &conversationv1.AgentSubagent{
						Result: &conversationv1.AgentSubagent_Start{Start: &conversationv1.AgentSubagentStart{
							CreatedAgentId: created,
							Prompt:         &conversationv1.AgentSubagentPrompt{Text: "go"},
							StartedAt:      &conversationv1.AgentActivityStartedAt{AtMs: 1_000},
						}},
					}},
				},
			}},
		}),
	))

	// Assert.
	rows := h.rows(rootFeed())
	if len(rows) != 1 || rows[0].GetDetachedSubagent() == nil {
		t.Fatalf("rows = %+v, want one detached bubble", rows)
	}
}

func TestAFlooredReplayClearsAnyStandingTruncation(t *testing.T) {
	// Arrange: a page that stopped short.
	h := newHarness(t)
	h.replay(historyPage(&conversationv1.HistoryMore{
		LastEntry: &conversationv1.HistoryPointer{Value: "a"},
	}, promptEntry("turn-2", "second")))

	// Act: a later page reaches the oldest retained entry.
	h.replay(historyPage(&conversationv1.HistoryFloor{}, promptEntry("turn-1", "first")))

	// Assert: the hole is gone, so a walk may claim the start.
	h.resolver.mu.Lock()
	more := h.resolver.feed(h.resolver.state(testWorkspace), rootFeed()).historyMore
	h.resolver.mu.Unlock()
	if more != nil {
		t.Fatalf("historyMore = %+v, want cleared by the floor", more)
	}
}

func TestAReplayRecordsWhatItDelivered(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.replay(historyPage(&conversationv1.HistoryMore{
		LastEntry: &conversationv1.HistoryPointer{Value: "a"},
	}, promptEntry("turn-2", "second"), promptEntry("turn-1", "first")))

	// Assert.
	var entries any
	for _, record := range h.records() {
		if record.Operation == "daemon.feed.history_page" {
			entries = record.Context["entries"]
		}
	}
	if entries != 2 {
		t.Fatalf("logged entries = %v, want 2", entries)
	}
}

func TestAReplayedEntryWithNoArmIsWarned(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.replay(historyPage(&conversationv1.HistoryFloor{}, &conversationv1.HistoryEntry{}))

	// Assert.
	if !h.hasRecord("warn", "daemon.feed.history_entry_unset") {
		t.Fatalf("records = %+v, want a WARN daemon.feed.history_entry_unset", h.records())
	}
}

func TestAReplayedFrameWithNoArmIsWarned(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.replay(historyPage(&conversationv1.HistoryFloor{},
		&conversationv1.HistoryEntry{
			Entry: &conversationv1.HistoryEntry_AgentFrame{
				AgentFrame: &conversationv1.AgentFrame{AgentId: mainAgent()},
			},
		}))

	// Assert.
	if !h.hasRecord("warn", "daemon.feed.history_frame_unset") {
		t.Fatalf("records = %+v, want a WARN daemon.feed.history_frame_unset", h.records())
	}
}

func TestAReplayedUpdateWithNoArmIsWarned(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.replay(historyPage(&conversationv1.HistoryFloor{},
		frameEntry(mainAgent(), &conversationv1.AgentUpdate{})))

	// Assert.
	if !h.hasRecord("warn", "daemon.feed.history_update_unset") {
		t.Fatalf("records = %+v, want a WARN daemon.feed.history_update_unset", h.records())
	}
}

// TestAReplayedDetachedSubagentSettlesSucceededNeverFailed locks the resume
// guarantee behind BUG B: once the child watch (opened on resume by the session
// watcher for a spawn on the opening page) delivers the run's head-settle, the
// detached bubble takes the STORED outcome — a success settles Succeeded, never
// a failed default. The delivery mirrors the store: the announcement is one page
// line and the run's SubagentSuccess settle is another on the owner's book.
func TestAReplayedDetachedSubagentSettlesSucceededNeverFailed(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	created := &conversationv1.AgentId{Value: "agent-remote"}
	h.replay(historyPage(&conversationv1.HistoryFloor{},
		// NEWEST FIRST: the success settle is newer than the announcement.
		frameEntry(mainAgent(), &conversationv1.AgentUpdate{
			Update: &conversationv1.AgentUpdate_Activity{Activity: &conversationv1.AgentActivity{
				ActivityId: &conversationv1.AgentActivityId{Value: "work-1"},
				Item: &conversationv1.AgentActivity_Subagent{Subagent: &conversationv1.AgentSubagent{
					Result: &conversationv1.AgentSubagent_Success{Success: &conversationv1.AgentSubagentSuccess{
						CreatedAgentId: created,
						SettledAt:      &conversationv1.AgentActivitySettledAt{AtMs: 9_000},
					}},
				}},
			}},
		}),
		frameEntry(mainAgent(), &conversationv1.AgentDetachedWork{
			Work:  &conversationv1.DetachedWorkId{Value: "work-1"},
			Owner: mainAgent(),
			Origin: &conversationv1.AgentDetachedWork_Created{Created: &conversationv1.DetachedWorkCreated{
				WorkCreated: &conversationv1.DetachableWork{
					Work: &conversationv1.DetachableWork_Subagent{Subagent: &conversationv1.AgentSubagent{
						Result: &conversationv1.AgentSubagent_Start{Start: &conversationv1.AgentSubagentStart{
							CreatedAgentId: created,
							Prompt:         &conversationv1.AgentSubagentPrompt{Text: "go"},
							StartedAt:      &conversationv1.AgentActivityStartedAt{AtMs: 1_000},
						}},
					}},
				},
			}},
		}),
	))

	// Assert: the detached bubble is settled Succeeded, never a failed default.
	var bubble *frontendv1.FeedSubagent
	for _, row := range h.everyRow() {
		if detached := row.GetDetachedSubagent(); detached != nil {
			bubble = detached.GetSubagent()
		}
	}
	if bubble == nil {
		t.Fatalf("rows = %+v, want a detached subagent bubble", h.everyRow())
	}
	if bubble.GetSettled().GetSucceeded() == nil {
		t.Fatalf("outcome = %T, want Succeeded (never a failed default)", bubble.GetSettled().GetOutcome())
	}
}

// THE CRUX REGRESSION: A REPLAYED CONCLUDED TURN STAMPS ITS ANSWER ROW GREEN.
// This is the reload/reconnect/adopt case — a feed rebuilt from the store with
// NO live turn-ended event. The recurring bug was that the green depended on a
// live event and never appeared on a replayed feed. Because the flag is a data
// property re-stamped along the same terminal path replay walks, the replayed
// answer row must carry final_answer=true just as a live one does.
func TestAReplayedConcludedTurnStampsItsAnswerRowFinal(t *testing.T) {
	// Arrange, Act: a stored turn — prompt, its answering response, and the
	// concluded terminal naming that response — replayed newest-first.
	h := newHarness(t)
	h.replay(historyPage(&conversationv1.HistoryFloor{},
		frameEntry(mainAgent(), &conversationv1.AgentSuccess{
			Outcome: &conversationv1.AgentSuccess_Completed{Completed: &conversationv1.AgentCompleted{
				Answer: &conversationv1.AgentActivityId{Value: "unit-1"},
			}},
		}),
		frameEntry(mainAgent(), &conversationv1.AgentUpdate{
			Update: &conversationv1.AgentUpdate_Activity{
				Activity: responseSuccessActivity("unit-1", "the replayed answer"),
			},
		}),
		promptEntry("turn-1", "ask it"),
	))

	// Assert: the replayed answer row is green, with no live turn-ended event.
	var answer *frontendv1.FeedResponse
	for _, row := range h.rows(rootFeed()) {
		if resp := row.GetActivity().GetResponse(); resp != nil {
			answer = resp
		}
	}
	if answer == nil {
		t.Fatal("no response row was replayed")
	}
	if !answer.GetFinalAnswer() {
		t.Fatal("a replayed concluded turn's answer row was not stamped final_answer=true")
	}
}

// TestAnEmptyPageOfAnUnnamedWatchDrawsNothingAndReportsNothing: a fresh
// session's main watch opens on an empty floor before any row has named the
// main agent. There is nothing to place, so nothing is reported unplaceable.
func TestAnEmptyPageOfAnUnnamedWatchDrawsNothingAndReportsNothing(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	h.resolver.OnHistoryPage(testWorkspace, nil, historyPage(&conversationv1.HistoryFloor{}), noAddress())

	// Assert.
	if h.hasRecord("error", "daemon.feed.unplaceable_agent") {
		t.Fatalf("records = %+v, want no unplaceable report for an empty page", h.records())
	}
	if !h.hasRecord("debug", "daemon.feed.history_page_empty") {
		t.Fatalf("records = %+v, want the empty page recorded", h.records())
	}
}

// TestAPageOfAnUnnamedAgentWithRowsIsReportedUnplaceable: a page that DOES
// carry rows for an agent nothing named cannot be placed, and says so.
func TestAPageOfAnUnnamedAgentWithRowsIsReportedUnplaceable(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	h.resolver.OnHistoryPage(testWorkspace, &conversationv1.AgentId{Value: "agent-ghost"},
		historyPage(&conversationv1.HistoryFloor{}, frameEntry(&conversationv1.AgentId{Value: "agent-ghost"},
			&conversationv1.AgentUpdate{Update: &conversationv1.AgentUpdate_Activity{
				Activity: responseSuccessActivity("unit-1", "orphaned prose"),
			}})), noAddress())

	// Assert.
	if rows := h.everyRow(); len(rows) != 0 {
		t.Fatalf("rows = %+v, want nothing drawn", rows)
	}
	if !h.hasRecord("error", "daemon.feed.unplaceable_agent") {
		t.Fatalf("records = %+v, want the ERROR", h.records())
	}
}

// TestAReplayedQueryDeathDrawsQueryDied covers a turn whose query died, served
// from history: a replay carries no session push, only the stored terminal, so
// the terminal's own query_died arm is what makes the replay draw the same
// ending the live turn did.
func TestAReplayedQueryDeathDrawsQueryDied(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.replay(historyPage(&conversationv1.HistoryFloor{},
		frameEntry(mainAgent(), queryDiedTerminal(iteratorDeath())),
		promptEntry("turn-1", "!query-fail"),
	))

	// Assert.
	errored := h.terminalRow("turn-1").GetErrored()
	if errored.GetQueryDied().GetIteratorFailure() == nil {
		t.Fatalf("arm = %q (%v), want query_died.iterator_failure", erroredArmWord(errored), errored)
	}
}

// stampedPage stamps a page's entries with their turns, indexed NEWEST FIRST as
// historyPage lists them; an empty turn leaves the entry unstamped.
func stampedPage(page *conversationv1.HistoryPage, turns ...string) *conversationv1.HistoryPage {
	for i, turn := range turns {
		if turn != "" {
			page.Entries[i].Turn = &conversationv1.TurnId{Value: turn}
		}
	}
	return page
}

// A STAMPED TERMINAL ENDS THE TURN IT NAMES, whatever prompt it follows. Each
// case is one arrangement of prompts and terminals on a replayed page; the
// assertion is which turns got a terminal row.
func TestAStampedReplayedTerminalEndsTheTurnItNames(t *testing.T) {
	cases := []struct {
		name    string
		page    *conversationv1.HistoryPage
		ended   string
		unended string
	}{
		{
			name: "a terminal that arrives after the next turn's prompt ends its own turn",
			page: stampedPage(historyPage(&conversationv1.HistoryFloor{},
				frameEntry(mainAgent(), completed("")),
				promptEntry("turn-2", "second"),
				promptEntry("turn-1", "first"),
			), "turn-1", "turn-2", "turn-1"),
			ended: "turn-1", unended: "turn-2",
		},
		{
			name: "a terminal whose turn's prompt is missing is not charged to the last prompt",
			page: stampedPage(historyPage(&conversationv1.HistoryFloor{},
				frameEntry(mainAgent(), completed("")),
				promptEntry("turn-1", "first"),
			), "turn-lost", "turn-1"),
			ended: "turn-lost", unended: "turn-1",
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)

			// Act
			h.replay(tc.page)

			// Assert
			if !h.hasTerminalRow(tc.ended) {
				t.Fatalf("no terminal row for %q", tc.ended)
			}
			if h.hasTerminalRow(tc.unended) {
				t.Fatalf("the terminal was charged to %q", tc.unended)
			}
		})
	}
}

func TestAStampedEntryNamingATurnItsBookNeverOpenedIsAnError(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act: the prompt of turn-lost is missing from a page that reaches the floor.
	h.replay(stampedPage(historyPage(&conversationv1.HistoryFloor{},
		frameEntry(mainAgent(), completed("")),
		promptEntry("turn-1", "first"),
	), "turn-lost", "turn-1"))

	// Assert
	if !h.hasRecord("error", "daemon.feed.replayed_turn_unknown") {
		t.Fatalf("no error for the unknown turn; records: %+v", h.records())
	}
}

func TestAStampedTerminalOlderThanThePageIsNeitherDrawnNorAnError(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act: the page opens mid-conversation; its head ends a turn whose prompt
	// is below the page.
	h.replay(stampedPage(historyPage(&conversationv1.HistoryMore{},
		promptEntry("turn-2", "now"),
		frameEntry(mainAgent(), completed("")),
	), "turn-2", "turn-1"))

	// Assert
	if h.hasTerminalRow("turn-1") || h.hasTerminalRow("turn-2") {
		t.Fatal("the head terminal of an older turn was drawn")
	}
	if h.hasRecord("error", "daemon.feed.replayed_turn_unknown") {
		t.Fatal("a turn older than the page was reported as unknown")
	}
}

func TestAStampedSubagentTerminalEndsNoTurn(t *testing.T) {
	// Arrange
	h := newHarness(t)
	sub := &conversationv1.AgentId{Value: "agent-sub"}

	// Act: a subagent's frames carry the turn they were produced within.
	h.replay(stampedPage(historyPage(&conversationv1.HistoryFloor{},
		frameEntry(sub, completed("")),
		promptEntry("turn-1", "first"),
	), "turn-1", "turn-1"))

	// Assert
	if !h.promptWorking("turn-1") {
		t.Fatal("a subagent's stream ending settled the turn's prompt")
	}
}

func TestAnUnstampedReplayFallsBackToPositionAtInfo(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act: pre-contract data carries no stamps at all.
	h.replay(historyPage(&conversationv1.HistoryFloor{},
		frameEntry(mainAgent(), completed("")),
		promptEntry("turn-1", "first"),
	))

	// Assert
	if !h.hasTerminalRow("turn-1") {
		t.Fatal("the unstamped terminal was not charged to the page's last prompt")
	}
	if !h.hasRecord("info", "daemon.feed.replay_unstamped") {
		t.Fatalf("no info record for the positional fallback; records: %+v", h.records())
	}
}

func TestAFullyStampedReplayWritesNoPositionalRecord(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	h.replay(stampedPage(historyPage(&conversationv1.HistoryFloor{},
		frameEntry(mainAgent(), completed("")),
		promptEntry("turn-1", "first"),
	), "turn-1", "turn-1"))

	// Assert
	if h.hasRecord("info", "daemon.feed.replay_unstamped") {
		t.Fatal("a stamped replay reported a positional fallback")
	}
}

func TestAStampedPromptIsNeverJudgedAnUnknownTurn(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act: the prompt carries its own turn's stamp, as the producer writes it.
	h.replay(stampedPage(historyPage(&conversationv1.HistoryFloor{},
		frameEntry(mainAgent(), completed("")),
		promptEntry("turn-1", "first"),
	), "turn-1", "turn-1"))

	// Assert
	if h.hasRecord("error", "daemon.feed.replayed_turn_unknown") {
		t.Fatal("a stamped prompt was reported as naming an unknown turn")
	}
}

// A REPLAYED TURN IS DRAWN WHERE IT WAS DRAWN LIVE: at its recorded output
// address, through the same upsert, so a mirrored address's root copies are
// part of the replayed history.

// recordedTabAddress is a merge tab's output address as the prompt queue
// records it on a turn.
func recordedTabAddress(lease ids.LeaseID, mirror bool) *wsm.OutputAddress {
	parent := feedid.Ref{WS: testWorkspace, Feed: feedid.Feed{Merge: &lease},
		Row: feedid.RowKey{Kind: feedid.KindMergeTab, ID: "conflicts", Sub: "1"}}
	return &wsm.OutputAddress{Feed: feedid.Feed{Merge: &lease}, Parent: &parent, Mirror: mirror}
}

// repairReply is a replayed, unstamped main-agent terminal that ends the turn
// it answers.
func repairReply() *conversationv1.HistoryEntry {
	return frameEntry(mainAgent(), &conversationv1.AgentSuccess{
		Outcome: &conversationv1.AgentSuccess_Completed{Completed: &conversationv1.AgentCompleted{}},
	})
}

func TestAReplayedTurnRecordedAtAMirroredAddressIsDrawnInItsTab(t *testing.T) {
	// Arrange
	h := newHarness(t)
	lease := ids.LeaseID("lease-7")
	h.addresses = map[ids.TurnID]*wsm.OutputAddress{"turn-1": recordedTabAddress(lease, true)}

	// Act
	h.replay(historyPage(&conversationv1.HistoryFloor{}, promptEntry("turn-1", "resolve the conflict")))

	// Assert
	row := h.only(feedid.Feed{Merge: &lease})
	if row.GetParent().GetRow().GetValue() != testEncode(*recordedTabAddress(lease, true).Parent).GetValue() {
		t.Fatalf("tab row parent = %v, want the recorded tab", row.GetParent())
	}
}

func TestAReplayedTurnRecordedAtAMirroredAddressDrawsItsRootCopy(t *testing.T) {
	// Arrange
	h := newHarness(t)
	lease := ids.LeaseID("lease-7")
	h.addresses = map[ids.TurnID]*wsm.OutputAddress{"turn-1": recordedTabAddress(lease, true)}

	// Act
	h.replay(historyPage(&conversationv1.HistoryFloor{}, promptEntry("turn-1", "resolve the conflict")))

	// Assert
	row := h.only(rootFeed())
	if row.GetId().GetValue() != h.promptRowID("turn-1") || row.GetParent() != nil {
		t.Fatalf("root row = %v, want the prompt's top-level root copy %q", row, h.promptRowID("turn-1"))
	}
}

func TestAReplayedTurnRecordedAtAnUnmirroredAddressDrawsNothingOnTheRoot(t *testing.T) {
	// Arrange
	h := newHarness(t)
	lease := ids.LeaseID("lease-7")
	h.addresses = map[ids.TurnID]*wsm.OutputAddress{"turn-1": recordedTabAddress(lease, false)}

	// Act
	h.replay(historyPage(&conversationv1.HistoryFloor{}, promptEntry("turn-1", "the before-merge prompt")))

	// Assert
	if rows := h.rows(rootFeed()); len(rows) != 0 {
		t.Fatalf("root rows = %v, want none for a turn recorded at an unmirrored tab", rowIDs(rows))
	}
	h.only(feedid.Feed{Merge: &lease})
}

func TestAReplayedTurnsUnstampedTerminalFollowsItsTurnsRecordedAddress(t *testing.T) {
	// Arrange
	h := newHarness(t)
	lease := ids.LeaseID("lease-7")
	h.addresses = map[ids.TurnID]*wsm.OutputAddress{"turn-1": recordedTabAddress(lease, true)}

	// Act
	h.replay(historyPage(&conversationv1.HistoryFloor{},
		repairReply(),
		promptEntry("turn-1", "resolve the conflict"),
	))

	// Assert: the tab and the root carry the same rows, the root's as copies.
	tab, root := h.rows(feedid.Feed{Merge: &lease}), h.rows(rootFeed())
	if len(tab) < 2 || len(root) != len(tab) {
		t.Fatalf("tab rows = %v, root rows = %v, want the reply drawn in the tab and mirrored", rowIDs(tab), rowIDs(root))
	}
}

func TestAReplayedRootTurnIsDrawnOnTheRootWhileAnAddressStands(t *testing.T) {
	// Arrange: the turn ran with no address; a merge's address stands now.
	h := newHarness(t)
	lease := ids.LeaseID("lease-7")
	mirroredMergeAddress(h, lease)
	h.addresses = map[ids.TurnID]*wsm.OutputAddress{"turn-1": nil}

	// Act
	h.replay(historyPage(&conversationv1.HistoryFloor{}, promptEntry("turn-1", "an ordinary turn")))

	// Assert
	if rows := h.rows(feedid.Feed{Merge: &lease}); len(rows) != 0 {
		t.Fatalf("tab rows = %v, want none for a turn recorded at the root", rowIDs(rows))
	}
	h.only(rootFeed())
}

func TestAReplayedUnrecordedTurnIsDrawnAtTheStandingAddress(t *testing.T) {
	// Arrange
	h := newHarness(t)
	lease := ids.LeaseID("lease-7")
	h.resolver.SetOutputAddress(testWorkspace, &sessionwatcher.OutputAddress{Feed: feedid.Feed{Merge: &lease}})

	// Act
	h.replay(historyPage(&conversationv1.HistoryFloor{}, promptEntry("turn-1", "never recorded")))

	// Assert
	h.only(feedid.Feed{Merge: &lease})
}

func TestAReplayRestoresTheStandingAddressAfterDrawingARecordedTurn(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.addresses = map[ids.TurnID]*wsm.OutputAddress{"turn-1": recordedTabAddress("lease-7", true)}

	// Act
	h.replay(historyPage(&conversationv1.HistoryFloor{}, promptEntry("turn-1", "resolve the conflict")))

	// Assert
	if addr := h.resolver.OutputAddress(testWorkspace); addr != nil {
		t.Fatalf("standing address after the replay = %+v, want none, as before it", addr)
	}
}

func TestAnUnreadableRecordedAddressIsAnErrorAndThePageStillDraws(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.addresses = map[ids.TurnID]*wsm.OutputAddress{"turn-1": recordedTabAddress("lease-7", true)}
	h.addressesErr = errors.New("database is locked")

	// Act
	h.replay(historyPage(&conversationv1.HistoryFloor{}, promptEntry("turn-1", "resolve the conflict")))

	// Assert
	if !h.hasRecord("error", "daemon.feed.turn_addresses_unreadable") {
		t.Fatalf("the failed read was not recorded at ERROR: %+v", h.records())
	}
	h.only(rootFeed())
}

// THE INVARIANT: a reader joining late sees the root feed a live reader saw.
func TestAReplayedMirroredTurnDrawsTheRootFeedTheLiveDrawDrew(t *testing.T) {
	// Arrange: one resolver draws the repair turn live under its mirrored
	// address; another replays it from history with the address recorded.
	lease := ids.LeaseID("lease-7")
	live := newHarness(t)
	mirroredMergeAddress(live, lease)
	live.deliverPrompt("turn-1", "resolve the conflict")
	replayed := newHarness(t)
	replayed.addresses = map[ids.TurnID]*wsm.OutputAddress{"turn-1": recordedTabAddress(lease, true)}

	// Act
	replayed.replay(historyPage(&conversationv1.HistoryFloor{}, promptEntry("turn-1", "resolve the conflict")))

	// Assert
	want, got := rowIDs(live.rows(rootFeed())), rowIDs(replayed.rows(rootFeed()))
	if fmt.Sprint(got) != fmt.Sprint(want) {
		t.Fatalf("replayed root rows = %v, want the live draw's %v", got, want)
	}
}

func TestOutputAddressAnswersACopyOfTheStandingAddress(t *testing.T) {
	// Arrange
	h := newHarness(t)
	lease := ids.LeaseID("lease-7")
	mirroredMergeAddress(h, lease)

	// Act
	got := h.resolver.OutputAddress(testWorkspace)
	got.Mirror = false

	// Assert
	if again := h.resolver.OutputAddress(testWorkspace); again == nil || !again.Mirror {
		t.Fatalf("standing address = %+v, want the caller's copy not to alter it", again)
	}
}

func TestOutputAddressOfAWorkspaceTheFeedNeverSawIsNone(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	got := h.resolver.OutputAddress("never-seen")

	// Assert
	if got != nil {
		t.Fatalf("OutputAddress = %+v, want none", got)
	}
}
