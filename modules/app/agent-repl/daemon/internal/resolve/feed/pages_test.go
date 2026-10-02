package feed

import (
	"context"
	"errors"
	"fmt"
	"strings"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/feedid"
	"claude-repld/internal/ids"
)

// The walk is PER READER and EPHEMERAL. The harness page size is 3, so five
// rows is two pages and an edge.

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
	h.mainBook(2, promptsBook(6))
	h.openPage(rootFeed(), "reader-1")
	h.openPage(rootFeed(), "reader-2")
	h.nextPage("reader-1")

	// Act: the second reader's next is still the second page, not the third.
	page := h.nextPage("reader-2")

	// Assert.
	if got, want := strings.Join(rowIDs(pageRows(t, page)), ","), h.promptRowIDs("turn-2", "turn-3"); got != want {
		t.Fatalf("reader-2 page = %v, want its own second page %v", got, want)
	}
}

func TestOpeningAgainDropsTheReadersWalk(t *testing.T) {
	// Arrange: a reader that has paged back once.
	h := newHarness(t)
	h.mainBook(3, promptsBook(5))
	h.openPage(rootFeed(), "reader-1")
	h.nextPage("reader-1")

	// Act: a fresh open lands at the tail again.
	page, _ := h.openPage(rootFeed(), "reader-1")

	// Assert.
	if got, want := strings.Join(rowIDs(pageRows(t, page)), ","), h.promptRowIDs("turn-2", "turn-3", "turn-4"); got != want {
		t.Fatalf("re-opened page = %v, want the newest page %v", got, want)
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
	h.resolver.mintSubFeed(s, outerHead, rootFeed(), feedid.Feed{Agent: outer}, "Explore the daemon")
	outerFeed := h.resolver.feed(s, feedid.Feed{Agent: outer})
	outerFeed.rows[innerHead.GetValue()] = &frontendv1.FeedRow{Id: innerHead}
	h.resolver.mintSubFeed(s, innerHead, feedid.Feed{Agent: outer}, feedid.Feed{Agent: inner}, "Read the protos")
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

func TestBreadcrumbsClimbThroughAHeadMintedBeforeItsRowIsPlaced(t *testing.T) {
	// Arrange: a subagent of a subagent whose head is minted BEFORE its row is
	// upserted on the outer sub-feed, which is the order composeSubagent takes.
	h := newHarness(t)
	outer := &conversationv1.AgentId{Value: "agent-outer"}
	inner := &conversationv1.AgentId{Value: "agent-inner"}
	outerHead := &frontendv1.FeedId{Value: "row|outer"}
	innerHead := &frontendv1.FeedId{Value: "row|inner"}
	h.resolver.mu.Lock()
	s := h.resolver.state(testWorkspace)
	h.resolver.mintSubFeed(s, outerHead, rootFeed(), feedid.Feed{Agent: outer}, "Explore the daemon")
	h.resolver.mintSubFeed(s, innerHead, feedid.Feed{Agent: outer}, feedid.Feed{Agent: inner}, "Read the protos")
	h.resolver.mu.Unlock()

	// Act.
	page, _ := h.openPage(feedid.Feed{Agent: inner}, "reader-1")

	// Assert: the chain reaches the root through the outer bubble.
	crumbs := page.GetResult().(*frontendv1.FeedPage_Success).Success.GetBreadcrumbs().GetCrumbs()
	if len(crumbs) != 2 || crumbs[0].GetTarget().GetValue() != "row|outer" || crumbs[1].GetTarget().GetValue() != "row|inner" {
		t.Fatalf("crumbs = %+v, want [outer, inner]: the inner head's parent is the feed it is drawn on", crumbs)
	}
}

func TestBreadcrumbLabelIsTheMergesBranchLine(t *testing.T) {
	// Arrange: the orchestrator records its own head's label.
	h := newHarness(t)
	lease := ids.LeaseID("lease-7")
	head := &frontendv1.FeedId{Value: "row|merge-head"}
	h.resolver.MintSubFeedHead(testWorkspace, head, feedid.Feed{Root: true}, feedid.Feed{Merge: &lease}, "DWC/fix-flaky → master")

	// Act.
	page, _ := h.openPage(feedid.Feed{Merge: &lease}, "reader-1")

	// Assert.
	crumbs := page.GetResult().(*frontendv1.FeedPage_Success).Success.GetBreadcrumbs().GetCrumbs()
	if len(crumbs) != 1 || crumbs[0].GetLabel() != "DWC/fix-flaky → master" {
		t.Fatalf("crumbs = %+v, want the merge's branch line", crumbs)
	}
}

func TestAWalkThatReachesAFlooredReplayClaimsTheStart(t *testing.T) {
	// Arrange: a replay that DID reach the oldest retained entry.
	h := newHarness(t)
	h.resolver.OnHistoryPage(testWorkspace, mainAgent(), &conversationv1.HistoryPage{
		Boundary: &conversationv1.HistoryPage_Floor{Floor: &conversationv1.HistoryFloor{}},
	})
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

// ---- THE DELIVERY BOUND GOVERNS THE PUSH, NOT JUST THE PAGE ----
//
// A separation that cut context withholds every row above it from a first
// page. The same must hold for the LIVE push: a row first drawn after the
// divider yet sorting above it — a history/file-plane row the sidecar forwards
// late — must be kept off the wire, or a client that appends by arrival lands
// it below the divider where nothing retracts it. That was the "/clear dumped
// all previous history" bug.

func TestALateHistoryRowAboveAClearDividerIsKeptOffThePush(t *testing.T) {
	// Arrange: a reader following the live feed, then a /clear divider.
	h := newHarness(t)
	_, token := h.openPage(rootFeed(), "reader-1")
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	tail, err := h.resolver.Tail(ctx, testWorkspace, rootFeed(), token)
	if err != nil {
		t.Fatalf("Tail: %v", err)
	}
	rows := tail.Rows(ctx)
	h.cutPlaced("entry-1", clearedCut(), 200)

	// Act: a history page replays a prompt placed before the divider AFTER
	// the divider arrived, so it sorts above the divider; then a live prompt
	// lands below it.
	h.replay(placedPage(&conversationv1.HistoryFloor{}, pagedEntry{atMs: 100, entry: promptEntry("turn-old", "from above the cut")}))
	h.deliverPromptAt("turn-new", "below the cut", 300)

	// Assert: the tail's first two pushes are the divider and the live prompt,
	// in that order — the withheld history row never reached the wire. Were it
	// pushed, it would arrive between them (deliveries are FIFO per reader).
	got := map[string]bool{}
	got[(<-rows).GetId().GetValue()] = true
	got[(<-rows).GetId().GetValue()] = true
	if !got[h.promptRowID("turn-new")] {
		t.Fatalf("the live prompt below the cut was not among the first two pushes: %v", got)
	}
	if got[h.promptRowID("turn-old")] {
		t.Fatalf("a history row above the /clear divider was pushed to the reader")
	}
}

// TestADetachedShellsSpoolIsNotPushedToARootTailThatNeverOpenedTheBubble pins
// the lazy streaming the fold rests on: the shell's spool BODY lands on the
// shell's own sub-feed, so a reader following only the ROOT feed — one that
// never opened the bubble — receives the HEAD but never a byte of the spool.
// Mirrors TestALateHistoryRowAboveAClearDividerIsKeptOffThePush: a row that
// must not reach the wire is proven absent by a sentinel that must.
func TestADetachedShellsSpoolIsNotPushedToARootTailThatNeverOpenedTheBubble(t *testing.T) {
	// Arrange: a reader following the ROOT feed's tail. It never opens the
	// shell's sub-feed.
	h := newHarness(t)
	_, token := h.openPage(rootFeed(), "reader-1")
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	tail, err := h.resolver.Tail(ctx, testWorkspace, rootFeed(), token)
	if err != nil {
		t.Fatalf("Tail: %v", err)
	}
	rows := tail.Rows(ctx)

	// Act: a born-detached shell with output — HEAD on the root feed, spool
	// BODY on the shell's own sub-feed — then a sentinel root prompt.
	h.bash("work-1", &conversationv1.AgentBashStart{
		Command:   &conversationv1.AgentBashCommand{Line: "npm run dev"},
		StartedAt: &conversationv1.AgentActivityStartedAt{AtMs: 1_000},
	})
	h.bash("work-1", tailOf("compiling\n"))
	h.deliverPrompt("turn-new", "sentinel")

	// Assert: the spool BODY row id never reaches the root tail; the sentinel
	// (guaranteed a root push) does — and drives the read so the assertion never
	// blocks.
	bodyID := testEncode(feedid.Ref{
		WS:   testWorkspace,
		Feed: shellSubFeed("work-1"),
		Row:  feedid.RowKey{Kind: feedid.KindDetachedShell, ID: "work-1"},
	}).GetValue()
	sentinel := h.promptRowID("turn-new")
	for {
		id := (<-rows).GetId().GetValue()
		if id == bodyID {
			t.Fatalf("the spool body row was pushed to a root tail that never opened the bubble")
		}
		if id == sentinel {
			break
		}
	}
}

func TestAWithheldRowStaysStoredForPaging(t *testing.T) {
	// Arrange: a /clear divider, then a history row replayed above it.
	h := newHarness(t)
	h.cutPlaced("entry-1", clearedCut(), 200)

	// Act.
	h.replay(placedPage(&conversationv1.HistoryFloor{}, pagedEntry{atMs: 100, entry: promptEntry("turn-old", "from above the cut")}))

	// Assert: withholding from the push is recorded, and the row is still in
	// the feed's order (stored, so a walk back to it still orders it).
	if !h.hasRecord("info", "daemon.feed.push_withheld") {
		t.Fatal("withholding a row above the bound was not recorded at INFO")
	}
	stored := false
	for _, id := range rowIDs(h.rows(rootFeed())) {
		if id == h.promptRowID("turn-old") {
			stored = true
		}
	}
	if !stored {
		t.Fatal("the withheld row was dropped from the feed order; it must stay stored for paging")
	}
}

// ---- A DELIVERY-BOUND MOVE MUST NOT BLANK A REPLAYED CONVERSATION ----
//
// A context cut is not carried in the agent's page; it arrives on the LIVE plane
// while the conversation around it was drawn on the HISTORY plane. By plane order
// every history row reads as older than the live cut, so a reconnect that
// replayed the compaction's POST-cut conversation and THEN took the cut live had
// its whole feed withheld — blank but for the divider, though the conversation
// was on screen a moment before. The rows' conversation places tell the
// post-cut rows from a late-forwarded pre-cut row, whichever arrived first.

func TestALiveCutAfterAReplayKeepsThePostCutConversation(t *testing.T) {
	// Arrange: a reconnect replays post-compaction turns in the history plane.
	h := newHarness(t)
	h.replay(placedPage(&conversationv1.HistoryFloor{},
		pagedEntry{atMs: 400, entry: promptEntry("turn-7", "after two")},
		pagedEntry{atMs: 300, entry: promptEntry("turn-6", "after one")},
	))

	// Act: the compaction cut, placed before them, arrives LIVE after the
	// replay.
	h.cutPlaced("entry-cut", compactedCut("what survived"), 200)
	page, _ := h.openPage(rootFeed(), "reader-1")

	// Assert: the replayed conversation survives the bound move — the feed is not
	// blank, and the post-cut turns are intact.
	got := rowIDs(pageRows(t, page))
	if len(got) == 0 {
		t.Fatal("the feed went blank across the delivery-bound move")
	}
	want := map[string]bool{h.promptRowID("turn-6"): true, h.promptRowID("turn-7"): true}
	seen := 0
	for _, id := range got {
		if want[id] {
			seen++
		}
	}
	if seen != len(want) {
		t.Fatalf("page rows = %v, want the replayed post-cut turns intact", got)
	}
}

func TestALiveCutAfterAReplayIsNotRecordedAsWithholding(t *testing.T) {
	// Arrange: post-cut turns replayed in the history plane, then the live cut.
	h := newHarness(t)
	h.replay(placedPage(&conversationv1.HistoryFloor{},
		pagedEntry{atMs: 300, entry: promptEntry("turn-6", "after one")},
	))
	h.cutPlaced("entry-cut", compactedCut("what survived"), 200)

	// Act.
	h.openPage(rootFeed(), "reader-1")

	// Assert: nothing was withheld, so no bound-at-separation record was made for
	// a conversation the reader can still see.
	if h.hasRecord("info", "daemon.feed.bound_at_separation") {
		t.Fatal("a replayed post-cut conversation was recorded as withheld by the bound")
	}
}

func TestBoundHides(t *testing.T) {
	// Arrange: keys in the resolver's own vocabulary (order.go) — class, then
	// the entry's place.
	own := func(atMs int64) string {
		return entryBase('2', &conversationv1.ConversationPlace{AtMs: atMs}) + "00000000"
	}
	inherited := func(atMs int64) string {
		return entryBase('1', &conversationv1.ConversationPlace{AtMs: atMs}) + "00000000"
	}
	ported := "0.00000001"
	tests := []struct {
		name  string
		row   rowRank
		bound rowRank
		want  bool
	}{
		{
			name: "a row placed before the cut is pre-cut and hidden",
			row:  rowRank{plane: planeLive, key: own(3)}, bound: rowRank{plane: planeLive, key: own(5)}, want: true,
		},
		{
			name: "a row placed after the cut is post-cut and kept",
			row:  rowRank{plane: planeLive, key: own(7)}, bound: rowRank{plane: planeLive, key: own(5)}, want: false,
		},
		{
			name: "a history row placed before a live cut is a late pre-cut row, hidden",
			row:  rowRank{plane: planeHistory, key: own(3)}, bound: rowRank{plane: planeLive, key: own(5)}, want: true,
		},
		{
			name: "a history row placed after a live cut is replayed post-cut content, kept",
			row:  rowRank{plane: planeHistory, key: own(7)}, bound: rowRank{plane: planeLive, key: own(5)}, want: false,
		},
		{
			name: "a row following the cut is after it, kept",
			row:  rowRank{plane: planeLive, key: own(5) + ".00000001"}, bound: rowRank{plane: planeLive, key: own(5)}, want: false,
		},
		{
			name: "a fork's inherited row precedes the fork's own cut whatever its place, hidden",
			row:  rowRank{plane: planeInherited, key: inherited(9)}, bound: rowRank{plane: planeLive, key: own(5)}, want: true,
		},
		{
			name: "a fork's ported row precedes the fork's own cut, hidden",
			row:  rowRank{plane: planePorted, key: ported}, bound: rowRank{plane: planeHistory, key: own(5)}, want: true,
		},
		{
			name: "a ported row is never hidden by an inherited cut",
			row:  rowRank{plane: planePorted, key: ported}, bound: rowRank{plane: planeInherited, key: inherited(5)}, want: false,
		},
		{
			name: "the fork's own row is never hidden by an inherited cut",
			row:  rowRank{plane: planeLive, key: own(1)}, bound: rowRank{plane: planeInherited, key: inherited(9)}, want: false,
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			got := boundHides(tc.row, tc.bound)

			// Assert.
			if got != tc.want {
				t.Fatalf("boundHides(%+v, %+v) = %v, want %v", tc.row, tc.bound, got, tc.want)
			}
		})
	}
}
