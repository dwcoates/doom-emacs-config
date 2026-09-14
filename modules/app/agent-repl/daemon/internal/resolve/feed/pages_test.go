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
)

// The walk is PER READER and EPHEMERAL. The harness page size is 3, so five
// rows is two pages and an edge.

func TestOpenPageServesTheNewestPageOldestFirstWithinIt(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	for i := 1; i <= 5; i++ {
		h.deliverPrompt(fmt.Sprintf("turn-%d", i), "prompt")
	}

	// Act.
	page, _ := h.openPage(rootFeed(), "reader-1")

	// Assert: the NEWEST three, oldest → newest within the page.
	got := rowIDs(pageRows(t, page))
	want := []string{h.promptRowID("turn-3"), h.promptRowID("turn-4"), h.promptRowID("turn-5")}
	for i := range want {
		if got[i] != want[i] {
			t.Fatalf("page rows = %v, want %v", got, want)
		}
	}
}

func TestOpenPageSaysHasMoreWhenOlderRowsExist(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	for i := 1; i <= 5; i++ {
		h.deliverPrompt(fmt.Sprintf("turn-%d", i), "prompt")
	}

	// Act.
	page, _ := h.openPage(rootFeed(), "reader-1")

	// Assert.
	success := page.GetResult().(*frontendv1.FeedPage_Success).Success
	if _, ok := success.GetEdge().(*frontendv1.FeedPageSuccess_HasMore); !ok {
		t.Fatalf("edge = %T, want has_more", success.GetEdge())
	}
}

func TestOpenPageSaysAtStartWhenThePageReachesTheBeginning(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "only")

	// Act.
	page, _ := h.openPage(rootFeed(), "reader-1")

	// Assert.
	success := page.GetResult().(*frontendv1.FeedPage_Success).Success
	if _, ok := success.GetEdge().(*frontendv1.FeedPageSuccess_AtStart); !ok {
		t.Fatalf("edge = %T, want at_start", success.GetEdge())
	}
}

func TestAnEmptyFeedIsAPageAndNotAnError(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	page, _ := h.openPage(rootFeed(), "reader-1")

	// Assert: a domain outcome is a SUCCESS answer.
	if len(pageRows(t, page)) != 0 {
		t.Fatal("an empty feed served rows")
	}
}

func TestNextPageWalksOlderFromWhereTheWalkStands(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	for i := 1; i <= 5; i++ {
		h.deliverPrompt(fmt.Sprintf("turn-%d", i), "prompt")
	}
	h.openPage(rootFeed(), "reader-1")

	// Act.
	page, err := h.resolver.NextPage(context.Background(), testWorkspace, rootFeed(), "reader-1")
	if err != nil {
		t.Fatalf("NextPage: %v", err)
	}

	// Assert: the two rows before the first page, and now at the start.
	got := rowIDs(pageRows(t, page))
	want := []string{h.promptRowID("turn-1"), h.promptRowID("turn-2")}
	if len(got) != len(want) {
		t.Fatalf("page rows = %v, want %v", got, want)
	}
	for i := range want {
		if got[i] != want[i] {
			t.Fatalf("page rows = %v, want %v", got, want)
		}
	}
}

func TestNextPageWithNoWalkStandingIsARefusal(t *testing.T) {
	// Arrange: a reader that never opened.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "prompt")

	// Act.
	_, err := h.resolver.NextPage(context.Background(), testWorkspace, rootFeed(), "reader-1")

	// Assert: a refusal, NOT an empty page.
	if !errors.Is(err, ErrNoWalk) {
		t.Fatalf("NextPage err = %v, want ErrNoWalk", err)
	}
	if !h.hasRecord("warn", "daemon.feed.next_without_walk") {
		t.Fatalf("records = %+v, want a WARN daemon.feed.next_without_walk", h.records())
	}
}

func TestWalksArePerReader(t *testing.T) {
	// Arrange: two readers on the same feed, one of which has paged back.
	h := newHarness(t)
	for i := 1; i <= 5; i++ {
		h.deliverPrompt(fmt.Sprintf("turn-%d", i), "prompt")
	}
	h.openPage(rootFeed(), "reader-1")
	h.openPage(rootFeed(), "reader-2")
	if _, err := h.resolver.NextPage(context.Background(), testWorkspace, rootFeed(), "reader-1"); err != nil {
		t.Fatalf("NextPage: %v", err)
	}

	// Act: the second reader's next is still the second page, not the third.
	page, err := h.resolver.NextPage(context.Background(), testWorkspace, rootFeed(), "reader-2")
	if err != nil {
		t.Fatalf("NextPage: %v", err)
	}

	// Assert.
	got := rowIDs(pageRows(t, page))
	if len(got) != 2 || got[0] != h.promptRowID("turn-1") {
		t.Fatalf("reader-2 page = %v, want its own second page", got)
	}
}

func TestOpeningAgainDropsTheReadersWalk(t *testing.T) {
	// Arrange: a reader that has paged back once.
	h := newHarness(t)
	for i := 1; i <= 5; i++ {
		h.deliverPrompt(fmt.Sprintf("turn-%d", i), "prompt")
	}
	h.openPage(rootFeed(), "reader-1")
	if _, err := h.resolver.NextPage(context.Background(), testWorkspace, rootFeed(), "reader-1"); err != nil {
		t.Fatalf("NextPage: %v", err)
	}

	// Act: a fresh open lands at the tail again.
	page, _ := h.openPage(rootFeed(), "reader-1")

	// Assert.
	got := rowIDs(pageRows(t, page))
	if got[0] != h.promptRowID("turn-3") {
		t.Fatalf("re-opened page = %v, want the newest page", got)
	}
}

func TestCloseReaderDropsTheWalk(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "prompt")
	h.openPage(rootFeed(), "reader-1")

	// Act.
	h.resolver.CloseReader(testWorkspace, "reader-1")
	_, err := h.resolver.NextPage(context.Background(), testWorkspace, rootFeed(), "reader-1")

	// Assert.
	if !errors.Is(err, ErrNoWalk) {
		t.Fatalf("NextPage err = %v, want ErrNoWalk once the reader closed", err)
	}
}

func TestNonDurableRowsNeverAppearInAPage(t *testing.T) {
	// Arrange: one durable row and two non-durable ones.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "prompt")
	h.resolver.UpsertCommandPanel(testWorkspace, &frontendv1.FeedCommandPanel{})
	h.resolver.UpsertCommandRefused(testWorkspace, "/agents", "/agents is not supported here", true)

	// Act.
	page, _ := h.openPage(rootFeed(), "reader-1")

	// Assert: resolver memory only — the page carries the prompt alone.
	got := pageRows(t, page)
	if len(got) != 1 || got[0].GetUserPrompt() == nil {
		t.Fatalf("page rows = %v, want the durable prompt alone", rowIDs(got))
	}
}

func TestNonDurableRowsStillReachTheTail(t *testing.T) {
	// Arrange.
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
	h.resolver.UpsertCommandRefused(testWorkspace, "/help", "/help is not supported here", true)

	// Assert: pushed live even though it is never paged.
	row := <-rows
	if row.GetCommandRefused().GetCommand().GetText() != "/help" {
		t.Fatalf("streamed row = %+v, want the refusal card", row)
	}
}

func TestCommandRefusedCarriesTheAddSupportOfferOnlyWhenOffered(t *testing.T) {
	tests := []struct {
		name       string
		addSupport bool
	}{
		{name: "offered", addSupport: true},
		{name: "not offered", addSupport: false},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)

			// Act.
			id := h.resolver.UpsertCommandRefused(testWorkspace, "/agents", "not supported", tc.addSupport)

			// Assert.
			h.resolver.mu.Lock()
			row := h.resolver.feed(h.resolver.state(testWorkspace), rootFeed()).rows[id.GetValue()]
			h.resolver.mu.Unlock()
			offered := row.GetCommandRefused().GetAddSupport() != nil
			if offered != tc.addSupport {
				t.Fatalf("add_support present = %v, want %v", offered, tc.addSupport)
			}
		})
	}
}

func TestSynthesizedIdentitiesAreDistinctPerRow(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	first := h.resolver.UpsertCommandRefused(testWorkspace, "/agents", "not supported", true)
	second := h.resolver.UpsertCommandRefused(testWorkspace, "/help", "not supported", true)

	// Assert: the per-workspace sequence advances, so a re-push upserts and a
	// new card does not collide.
	if first.GetValue() == second.GetValue() {
		t.Fatalf("both cards minted %q, want distinct identities", first.GetValue())
	}
}

func TestBreadcrumbsAreEmptyOnTheRootFeed(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	page, _ := h.openPage(rootFeed(), "reader-1")

	// Assert.
	success := page.GetResult().(*frontendv1.FeedPage_Success).Success
	if got := len(success.GetBreadcrumbs().GetCrumbs()); got != 0 {
		t.Fatalf("crumbs = %d, want 0 on the feed's own top", got)
	}
}

func TestBreadcrumbsRunOutermostFirst(t *testing.T) {
	// Arrange: a bubble inside a bubble.
	h := newHarness(t)
	outer := &conversationv1.AgentId{Value: "agent-outer"}
	inner := &conversationv1.AgentId{Value: "agent-inner"}
	outerHead := &frontendv1.FeedId{Value: "row|outer"}
	innerHead := &frontendv1.FeedId{Value: "row|inner"}
	h.resolver.mu.Lock()
	s := h.resolver.state(testWorkspace)
	f := h.resolver.feed(s, rootFeed())
	f.rows[outerHead.GetValue()] = &frontendv1.FeedRow{Id: outerHead}
	h.resolver.mintSubFeed(s, outerHead, feedid.Feed{Agent: outer}, "Explore the daemon")
	outerFeed := h.resolver.feed(s, feedid.Feed{Agent: outer})
	outerFeed.rows[innerHead.GetValue()] = &frontendv1.FeedRow{Id: innerHead}
	h.resolver.mintSubFeed(s, innerHead, feedid.Feed{Agent: inner}, "Read the protos")
	h.resolver.mu.Unlock()

	// Act.
	page, _ := h.openPage(feedid.Feed{Agent: inner}, "reader-1")

	// Assert.
	crumbs := page.GetResult().(*frontendv1.FeedPage_Success).Success.GetBreadcrumbs().GetCrumbs()
	if len(crumbs) != 2 {
		t.Fatalf("crumbs = %d, want 2", len(crumbs))
	}
	if crumbs[0].GetLabel() != "Explore the daemon" || crumbs[1].GetLabel() != "Read the protos" {
		t.Fatalf("crumbs = [%q, %q], want outermost first", crumbs[0].GetLabel(), crumbs[1].GetLabel())
	}
}

func TestBreadcrumbLabelIsTheMergesBranchLine(t *testing.T) {
	// Arrange: the orchestrator records its own head's label.
	h := newHarness(t)
	lease := ids.LeaseID("lease-7")
	head := &frontendv1.FeedId{Value: "row|merge-head"}
	h.resolver.MintSubFeedHead(testWorkspace, head, feedid.Feed{Merge: &lease}, "DWC/fix-flaky → master")

	// Act.
	page, _ := h.openPage(feedid.Feed{Merge: &lease}, "reader-1")

	// Assert.
	crumbs := page.GetResult().(*frontendv1.FeedPage_Success).Success.GetBreadcrumbs().GetCrumbs()
	if len(crumbs) != 1 || crumbs[0].GetLabel() != "DWC/fix-flaky → master" {
		t.Fatalf("crumbs = %+v, want the merge's branch line", crumbs)
	}
}

func TestAWalkThatRunsOutWhileHistoryRemainsAnswersTruncated(t *testing.T) {
	// Arrange: a replay that did not reach the oldest retained entry.
	h := newHarness(t)
	h.resolver.OnHistoryPage(testWorkspace, mainAgent(), &conversationv1.HistoryPage{
		Entries: []*conversationv1.HistoryEntryAt{{
			At: &conversationv1.HistoryPointer{Value: "p1"},
			Entry: &conversationv1.HistoryEntry{
				Entry: &conversationv1.HistoryEntry_UserPrompt{UserPrompt: &conversationv1.AgentPrompt{
					Id:     &conversationv1.TurnId{Value: "turn-1"},
					Agent:  mainAgent(),
					Origin: conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT,
					Said:   &conversationv1.UserSaid{Content: &conversationv1.UserContent{}},
				}},
			},
		}},
		Boundary: &conversationv1.HistoryPage_More{More: &conversationv1.HistoryMore{
			LastEntry: &conversationv1.HistoryPointer{Value: "p1"},
		}},
	}, noAddress())
	h.openPage(rootFeed(), "reader-1")

	// Act: the walk is already at the oldest replayed row.
	page, err := h.resolver.NextPage(context.Background(), testWorkspace, rootFeed(), "reader-1")
	if err != nil {
		t.Fatalf("NextPage: %v", err)
	}

	// Assert: the hole is stated rather than drawn as a beginning.
	errPage, ok := page.GetResult().(*frontendv1.FeedPage_Error)
	if !ok {
		t.Fatalf("page = %T, want the truncated error", page.GetResult())
	}
	if errPage.Error.GetHistoryReplayTruncated() == nil {
		t.Fatalf("page error kind = %T, want history_replay_truncated", errPage.Error.GetKind())
	}
	if !h.hasRecord("warn", "daemon.feed.history_replay_truncated") {
		t.Fatalf("records = %+v, want a WARN daemon.feed.history_replay_truncated", h.records())
	}
}

func TestAWalkThatReachesAFlooredReplayClaimsTheStart(t *testing.T) {
	// Arrange: a replay that DID reach the oldest retained entry.
	h := newHarness(t)
	h.resolver.OnHistoryPage(testWorkspace, mainAgent(), &conversationv1.HistoryPage{
		Boundary: &conversationv1.HistoryPage_Floor{Floor: &conversationv1.HistoryFloor{}},
	}, noAddress())
	h.deliverPrompt("turn-1", "prompt")
	h.openPage(rootFeed(), "reader-1")

	// Act.
	page, err := h.resolver.NextPage(context.Background(), testWorkspace, rootFeed(), "reader-1")
	if err != nil {
		t.Fatalf("NextPage: %v", err)
	}

	// Assert.
	success, ok := page.GetResult().(*frontendv1.FeedPage_Success)
	if !ok {
		t.Fatalf("page = %T, want a success", page.GetResult())
	}
	if _, atStart := success.Success.GetEdge().(*frontendv1.FeedPageSuccess_AtStart); !atStart {
		t.Fatalf("edge = %T, want at_start", success.Success.GetEdge())
	}
}

// TestOpenPageAfterAReplayCarriesTheReplayedRows is THE RELAUNCH's page: a
// daemon that resumed a conversation it never watched has only the opening
// history page, and what a client that opens the feed then walks must be that
// conversation — not the empty page a feed with no live frames would serve.
func TestOpenPageAfterAReplayCarriesTheReplayedRows(t *testing.T) {
	// Arrange: a page replayed as a resumed session's opening catch-up.
	h := newHarness(t)
	h.replay(historyPage(&conversationv1.HistoryFloor{},
		promptEntry("turn-2", "second"),
		promptEntry("turn-1", "first"),
	))

	// Act.
	page, _ := h.openPage(rootFeed(), "reader-1")

	// Assert: both prior prompts, oldest first.
	got := rowIDs(pageRows(t, page))
	want := []string{h.promptRowID("turn-1"), h.promptRowID("turn-2")}
	if len(got) != len(want) {
		t.Fatalf("page rows = %v, want the replayed conversation %v", got, want)
	}
	for i := range want {
		if got[i] != want[i] {
			t.Fatalf("page rows = %v, want %v", got, want)
		}
	}
}

// ---- THE FEED BEGINS AT THE NEWEST SEPARATION ----
//
// A compaction or a clear is the session saying that what came before it is no
// longer the conversation. The rows above the newest such divider are not
// served on a first page and not reachable by walking back; the divider itself
// is, and for a compaction it CARRIES the surviving summary.
//
// It is a DELIVERY bound: nothing is retired and no identity is reminted, so
// the cases below assert what a READER is served, never what the feed holds.

// separationRowID is the identity the divider a cut at AT drew.
func (h *harness) separationRowID(at string) string {
	return testEncode(feedid.Ref{
		WS: testWorkspace, Feed: rootFeed(),
		Row: feedid.RowKey{Kind: feedid.KindSeparation, ID: "context_cut:" + at},
	}).GetValue()
}

// clearDividerRowID is the identity a /clear's divider takes: keyed on the turn
// it belongs to, so the optimistic bar and the shim's confirming cut are one row.
func (h *harness) clearDividerRowID(turn string) string {
	return testEncode(feedid.Ref{
		WS: testWorkspace, Feed: rootFeed(),
		Row: feedid.RowKey{Kind: feedid.KindSeparation, ID: "context_cut:clear:" + turn},
	}).GetValue()
}

// clearedCut is the cut a `/clear` produces.
func clearedCut() *conversationv1.ContextCut {
	return &conversationv1.ContextCut{
		Cut: &conversationv1.ContextCut_Cleared{Cleared: &conversationv1.ContextCleared{}},
	}
}

func TestTheFirstPageStartsAtTheNewestSeparation(t *testing.T) {
	// Arrange: a conversation, a compaction, and two turns after it.
	h := newHarness(t)
	for i := 1; i <= 5; i++ {
		h.deliverPrompt(fmt.Sprintf("turn-%d", i), "prompt")
	}
	h.cutAt("entry-cut", compactedCut("what survived"))
	h.deliverPrompt("turn-6", "prompt")
	h.deliverPrompt("turn-7", "prompt")

	// Act.
	page, _ := h.openPage(rootFeed(), "reader-1")

	// Assert: the divider first, then the turns after it, and at_start.
	got := rowIDs(pageRows(t, page))
	want := []string{h.separationRowID("entry-cut"), h.promptRowID("turn-6"), h.promptRowID("turn-7")}
	if len(got) != len(want) {
		t.Fatalf("page rows = %v, want %v", got, want)
	}
	for i := range want {
		if got[i] != want[i] {
			t.Fatalf("page rows = %v, want %v", got, want)
		}
	}
	success := page.GetResult().(*frontendv1.FeedPage_Success).Success
	if _, ok := success.GetEdge().(*frontendv1.FeedPageSuccess_AtStart); !ok {
		t.Fatalf("edge = %T, want at_start: the feed begins at the divider", success.GetEdge())
	}
}

func TestLoadOlderAtTheSeparationAnswersNothingOlder(t *testing.T) {
	// Arrange: five rows above the cut, four below it, so the newest page is
	// short of the bound and one walk reaches it.
	h := newHarness(t)
	for i := 1; i <= 5; i++ {
		h.deliverPrompt(fmt.Sprintf("turn-%d", i), "prompt")
	}
	h.cutAt("entry-cut", compactedCut("what survived"))
	for i := 6; i <= 9; i++ {
		h.deliverPrompt(fmt.Sprintf("turn-%d", i), "prompt")
	}
	h.openPage(rootFeed(), "reader-1")

	// Act: the walk back, then one more ask at the bound.
	if _, err := h.resolver.NextPage(context.Background(), testWorkspace, rootFeed(), "reader-1"); err != nil {
		t.Fatalf("NextPage: %v", err)
	}
	page, err := h.resolver.NextPage(context.Background(), testWorkspace, rootFeed(), "reader-1")
	if err != nil {
		t.Fatalf("NextPage: %v", err)
	}

	// Assert: at_start, and nothing from above the cut was ever served.
	success := page.GetResult().(*frontendv1.FeedPage_Success).Success
	if _, ok := success.GetEdge().(*frontendv1.FeedPageSuccess_AtStart); !ok {
		t.Fatalf("edge = %T, want at_start", success.GetEdge())
	}
	for _, id := range rowIDs(success.GetRows()) {
		if id == h.promptRowID("turn-5") {
			t.Fatalf("a row from above the cut was served: %q", id)
		}
	}
}

func TestTwoCompactionsDeliverOnlyTheNewestDividerAndItsSummary(t *testing.T) {
	// Arrange: a compaction, a turn, and a second compaction over it.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "prompt")
	h.cutAt("entry-first", compactedCut("the first account"))
	h.deliverPrompt("turn-2", "prompt")
	h.cutAt("entry-second", compactedCut("the second account"))

	// Act.
	page, _ := h.openPage(rootFeed(), "reader-1")

	// Assert: the newest divider alone, carrying the newest summary.
	rows := pageRows(t, page)
	got := rowIDs(rows)
	if len(got) != 1 || got[0] != h.separationRowID("entry-second") {
		t.Fatalf("page rows = %v, want only the newest divider", got)
	}
	summary := rows[0].GetSeparation().GetCompacted().GetSummary().GetMarkdown()
	if summary != "the second account" {
		t.Fatalf("summary = %q, want the newest compaction's", summary)
	}
}

func TestAClearDeliversTheDividerAndTheTurnsAfterIt(t *testing.T) {
	// Arrange: a clear discards what came before and leaves no summary. turn-2 is
	// the /clear directive itself (its prompt draws no bubble); its cut is keyed
	// on that turn.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "prompt")
	h.deliverPrompt("turn-2", "/clear")
	h.cutAt("entry-clear", clearedCut())
	h.deliverPrompt("turn-3", "prompt")

	// Act.
	page, _ := h.openPage(rootFeed(), "reader-1")

	// Assert: the divider, then the later turn, and nothing else. The clear
	// arrived while turn-2 (the directive) was in flight, so its divider is keyed
	// on that turn.
	got := rowIDs(pageRows(t, page))
	want := []string{h.clearDividerRowID("turn-2"), h.promptRowID("turn-3")}
	if len(got) != len(want) {
		t.Fatalf("page rows = %v, want %v", got, want)
	}
	for i := range want {
		if got[i] != want[i] {
			t.Fatalf("page rows = %v, want %v", got, want)
		}
	}
}

func TestASeparationAfterThePageWasServedMovesTheBoundForTheWalk(t *testing.T) {
	// Arrange: a reader standing on a page served BEFORE the cut.
	h := newHarness(t)
	for i := 1; i <= 5; i++ {
		h.deliverPrompt(fmt.Sprintf("turn-%d", i), "prompt")
	}
	h.openPage(rootFeed(), "reader-1")

	// Act: the cut lands, then the reader walks back.
	h.cutAt("entry-cut", compactedCut("what survived"))
	page, err := h.resolver.NextPage(context.Background(), testWorkspace, rootFeed(), "reader-1")
	if err != nil {
		t.Fatalf("NextPage: %v", err)
	}

	// Assert: the walk answers the divider alone, at_start — the rows it was
	// walking toward are behind the bound now.
	got := rowIDs(pageRows(t, page))
	if len(got) != 1 || got[0] != h.separationRowID("entry-cut") {
		t.Fatalf("page rows = %v, want only the divider", got)
	}
	success := page.GetResult().(*frontendv1.FeedPage_Success).Success
	if _, ok := success.GetEdge().(*frontendv1.FeedPageSuccess_AtStart); !ok {
		t.Fatalf("edge = %T, want at_start", success.GetEdge())
	}
}

func TestTheDeliveryBoundMoveIsRecordedAtInfo(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "prompt")

	// Act.
	h.cutAt("entry-cut", compactedCut("what survived"))

	// Assert.
	if !h.hasRecord("info", "daemon.feed.delivery_bound_moved") {
		t.Fatal("the bound moving was not recorded at INFO")
	}
}

func TestACompactionThatFailedDoesNotBoundTheFeed(t *testing.T) {
	// Arrange: nothing was cut, which is the whole of what the divider says.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "prompt")
	h.cutAt("entry-failed", &conversationv1.ContextCut{
		Cut: &conversationv1.ContextCut_CompactionFailed{
			CompactionFailed: &conversationv1.ContextCompactionFailed{Error: "the summarizer refused"},
		},
	})

	// Act.
	page, _ := h.openPage(rootFeed(), "reader-1")

	// Assert: the turn above it is still served.
	got := rowIDs(pageRows(t, page))
	if len(got) == 0 || got[0] != h.promptRowID("turn-1") {
		t.Fatalf("page rows = %v, want the turn above the failed compaction", got)
	}
}
