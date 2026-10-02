package feed

import (
	"context"
	"errors"
	"fmt"
	"strings"
	"sync"
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/feedid"
	"claude-repld/internal/ids"
)

// ---- the fake store ----

// fakeHistory is a store behind a fake shim: each agent's book, NEWEST FIRST,
// served in pages of the store's own size. The page size is the FAKE STORE's
// fact, the one thing this package must never state for itself.
type fakeHistory struct {
	mu sync.Mutex
	// books is each agent's history, newest first; "" is the main agent's.
	books map[string][]*conversationv1.HistoryEntryAt
	// pageSize is the store's page size.
	pageSize int
	// reads are the reads made, in order.
	reads []historyRead
	// failFrom fails every read from this 1-based read number on; 0 never.
	failFrom int
	// noSource answers every read ErrNoHistorySource.
	noSource bool
}

// historyRead is one read the resolver made.
type historyRead struct {
	target string
	after  string
}

func newFakeHistory(pageSize int) *fakeHistory {
	return &fakeHistory{books: map[string][]*conversationv1.HistoryEntryAt{}, pageSize: pageSize}
}

// ReadHistory serves one page of a book.
func (f *fakeHistory) ReadHistory(_ context.Context, _ ids.WorkspaceID, target *conversationv1.AgentId, after *conversationv1.HistoryPointer) (*conversationv1.HistoryPage, error) {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.reads = append(f.reads, historyRead{target: target.GetValue(), after: after.GetValue()})
	if f.noSource {
		return nil, ErrNoHistorySource
	}
	if f.failFrom > 0 && len(f.reads) >= f.failFrom {
		return nil, errors.New("store unreachable")
	}
	book := f.books[target.GetValue()]
	start := 0
	if after != nil {
		start = -1
		for i, at := range book {
			if at.GetAt().GetValue() == after.GetValue() {
				start = i + 1
			}
		}
		if start < 0 {
			return nil, fmt.Errorf("stale pointer %q", after.GetValue())
		}
	}
	end := min(start+f.pageSize, len(book))
	page := &conversationv1.HistoryPage{Entries: book[start:end]}
	if end == len(book) {
		page.Boundary = &conversationv1.HistoryPage_Floor{Floor: &conversationv1.HistoryFloor{}}
	} else {
		page.Boundary = &conversationv1.HistoryPage_More{More: &conversationv1.HistoryMore{LastEntry: book[end-1].GetAt()}}
	}
	return page, nil
}

// readCount answers how many reads were made.
func (f *fakeHistory) readCount() int {
	f.mu.Lock()
	defer f.mu.Unlock()
	return len(f.reads)
}

// lastRead answers the newest read.
func (f *fakeHistory) lastRead() historyRead {
	f.mu.Lock()
	defer f.mu.Unlock()
	return f.reads[len(f.reads)-1]
}

// withHistory wires the fake store in as the resolver's history source.
func (h *harness) withHistory(f *fakeHistory) *fakeHistory {
	h.resolver.deps.History = f
	return f
}

// bookEntry is one entry of a scripted book, oldest-first index I: pointer
// p-I at conversation place (I+1)*100.
type bookEntry struct {
	turn  string
	entry *conversationv1.HistoryEntry
}

// promptsBook is a main-agent book of N prompts, turn-0 oldest.
func promptsBook(n int) []bookEntry {
	out := make([]bookEntry, 0, n)
	for i := range n {
		turn := fmt.Sprintf("turn-%d", i)
		out = append(out, bookEntry{entry: promptEntry(turn, "prompt "+turn)})
	}
	return out
}

// newestFirst renders a scripted book as the store serves it.
func newestFirst(oldestFirst []bookEntry) []*conversationv1.HistoryEntryAt {
	out := make([]*conversationv1.HistoryEntryAt, 0, len(oldestFirst))
	for i := len(oldestFirst) - 1; i >= 0; i-- {
		at := &conversationv1.HistoryEntryAt{
			At:    &conversationv1.HistoryPointer{Value: fmt.Sprintf("p-%d", i)},
			Entry: oldestFirst[i].entry,
			Place: &conversationv1.HistoryEntryAt_RecordedPlace{RecordedPlace: placeAt(int64(i+1) * 100)},
		}
		if oldestFirst[i].turn != "" {
			at.Turn = &conversationv1.TurnId{Value: oldestFirst[i].turn}
		}
		out = append(out, at)
	}
	return out
}

// mainBook scripts the main agent's book on a fake store of PAGE entries.
func (h *harness) mainBook(page int, oldestFirst []bookEntry) *fakeHistory {
	f := h.withHistory(newFakeHistory(page))
	f.books[""] = newestFirst(oldestFirst)
	return f
}

// nextPage asks for a reader's next page, failing the test on an error.
func (h *harness) nextPage(reader ReaderID) *frontendv1.FeedPage {
	h.t.Helper()
	page, err := h.resolver.NextPage(context.Background(), testWorkspace, rootFeed(), reader)
	if err != nil {
		h.t.Fatalf("NextPage: %v", err)
	}
	return page
}

// promptRowIDs is the prompt row of each named turn.
func (h *harness) promptRowIDs(turns ...string) string {
	out := make([]string, 0, len(turns))
	for _, turn := range turns {
		out = append(out, h.promptRowID(turn))
	}
	return strings.Join(out, ",")
}

// edgeOf names a page's edge arm.
func edgeOf(t *testing.T, page *frontendv1.FeedPage) string {
	t.Helper()
	switch page.GetSuccess().GetEdge().(type) {
	case *frontendv1.FeedPageSuccess_HasMore:
		return "has_more"
	case *frontendv1.FeedPageSuccess_AtStart:
		return "at_start"
	}
	t.Fatalf("page %v carries no edge", page)
	return ""
}

// ---- opening ----

func TestOpenFeedReadsOneStorePageWhenNothingIsHeld(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	store := h.mainBook(3, promptsBook(5))

	// Act.
	page, _ := h.openPage(rootFeed(), "reader-1")

	// Assert.
	if got := store.readCount(); got != 1 {
		t.Fatalf("reads = %d, want exactly one", got)
	}
	if got, want := strings.Join(rowIDs(pageRows(t, page)), ","), h.promptRowIDs("turn-2", "turn-3", "turn-4"); got != want {
		t.Fatalf("page rows = %v, want %v", got, want)
	}
}

func TestOpenFeedReadsTheNewestPage(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	store := h.mainBook(3, promptsBook(5))

	// Act.
	h.openPage(rootFeed(), "reader-1")

	// Assert.
	if got := store.lastRead(); got.after != "" || got.target != "" {
		t.Fatalf("read = %+v, want the main agent's newest page", got)
	}
}

func TestOpenFeedSaysHasMoreWhenTheStoreHasOlderPages(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.mainBook(3, promptsBook(5))

	// Act.
	page, _ := h.openPage(rootFeed(), "reader-1")

	// Assert.
	if got := edgeOf(t, page); got != "has_more" {
		t.Fatalf("edge = %s, want has_more", got)
	}
}

func TestOpenFeedReadsNothingWhenTheNewestPageIsHeld(t *testing.T) {
	// Arrange: an earlier reader loaded the newest page.
	h := newHarness(t)
	store := h.mainBook(3, promptsBook(5))
	h.openPage(rootFeed(), "reader-1")

	// Act.
	page, _ := h.openPage(rootFeed(), "reader-2")

	// Assert.
	if got := store.readCount(); got != 1 {
		t.Fatalf("reads = %d, want the held page served from memory", got)
	}
	if got, want := strings.Join(rowIDs(pageRows(t, page)), ","), h.promptRowIDs("turn-2", "turn-3", "turn-4"); got != want {
		t.Fatalf("page rows = %v, want %v", got, want)
	}
}

func TestOpenFeedReadsTheNewestPageAgainOnceAnEntryMovedIt(t *testing.T) {
	// Arrange: the newest page was loaded, then the conversation went on.
	h := newHarness(t)
	store := h.mainBook(3, promptsBook(5))
	h.openPage(rootFeed(), "reader-1")
	h.deliverPromptAt("turn-5", "after the load", 600)

	// Act.
	h.openPage(rootFeed(), "reader-2")

	// Assert.
	if got := store.readCount(); got != 2 {
		t.Fatalf("reads = %d, want the moved newest page read again", got)
	}
}

func TestANewestPageReadAgainKeepsTheOlderPagesLoaded(t *testing.T) {
	// Arrange: a walk loaded two pages, then the conversation moved.
	h := newHarness(t)
	store := h.mainBook(2, promptsBook(6))
	h.openPage(rootFeed(), "reader-1")
	h.nextPage("reader-1")
	h.deliverPromptAt("turn-6", "after the walk", 700)
	h.openPage(rootFeed(), "reader-2")

	// Act: reader-2 walks past what reader-1 loaded.
	h.nextPage("reader-2")
	h.nextPage("reader-2")
	h.nextPage("reader-2")

	// Assert: the read below the walk continues after the oldest loaded page.
	if got := store.lastRead().after; got != "p-2" {
		t.Fatalf("last read after %q, want p-2", got)
	}
}

func TestOpenFeedWithNoSessionServesWhatIsHeld(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	store := h.withHistory(newFakeHistory(3))
	store.noSource = true
	h.deliverPromptAt("turn-1", "live", 100)

	// Act.
	page, _, err := h.resolver.OpenPage(context.Background(), testWorkspace, rootFeed(), "reader-1")

	// Assert.
	if err != nil {
		t.Fatalf("OpenPage: %v, want the held rows served", err)
	}
	if got := rowIDs(pageRows(t, page)); len(got) != 1 || got[0] != h.promptRowID("turn-1") {
		t.Fatalf("page rows = %v, want the live prompt", got)
	}
}

func TestOpenFeedWhoseReadFailsIsHistoryUnavailable(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	store := h.mainBook(3, promptsBook(5))
	store.failFrom = 1

	// Act.
	_, _, err := h.resolver.OpenPage(context.Background(), testWorkspace, rootFeed(), "reader-1")

	// Assert.
	if !errors.Is(err, ErrHistoryUnavailable) {
		t.Fatalf("OpenPage err = %v, want ErrHistoryUnavailable", err)
	}
}

func TestAMergeTabsOpeningReadsTheRootsNewestPage(t *testing.T) {
	// Arrange: a merge tab's rows are the main agent's addressed turns.
	h := newHarness(t)
	store := h.mainBook(3, promptsBook(5))
	lease := ids.LeaseID("lease-1")

	// Act.
	h.openPage(feedid.Feed{Merge: &lease}, "reader-1")

	// Assert.
	if got := store.readCount(); got != 1 || store.lastRead().target != "" || store.lastRead().after != "" {
		t.Fatalf("reads = %d (last %+v), want the root's newest page once", got, store.lastRead())
	}
}

func TestAMergeTabReadsNothingWhenTheRootsNewestPageIsHeld(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	store := h.mainBook(3, promptsBook(5))
	h.openPage(rootFeed(), "reader-root")
	lease := ids.LeaseID("lease-1")

	// Act.
	h.openPage(feedid.Feed{Merge: &lease}, "reader-1")

	// Assert.
	if got := store.readCount(); got != 1 {
		t.Fatalf("reads = %d, want none beyond the root's", got)
	}
}

func TestASubFeedReadsItsOwnAgentsBook(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	store := h.withHistory(newFakeHistory(3))
	sub := &conversationv1.AgentId{Value: "agent-sub"}

	// Act.
	h.openPage(feedid.Feed{Agent: sub}, "reader-1")

	// Assert.
	if got := store.lastRead().target; got != "agent-sub" {
		t.Fatalf("read target = %q, want agent-sub", got)
	}
}

// ---- the walk ----

func TestNextFetchesThePageItDoesNotHold(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	store := h.mainBook(3, promptsBook(5))
	h.openPage(rootFeed(), "reader-1")

	// Act.
	page := h.nextPage("reader-1")

	// Assert.
	if got := store.lastRead().after; got != "p-2" {
		t.Fatalf("read after %q, want p-2 (the oldest loaded page's more pointer)", got)
	}
	if got, want := strings.Join(rowIDs(pageRows(t, page)), ","), h.promptRowIDs("turn-0", "turn-1"); got != want {
		t.Fatalf("page rows = %v, want %v", got, want)
	}
}

func TestNextServesAHeldPageFromMemory(t *testing.T) {
	// Arrange: an earlier reader walked one page back.
	h := newHarness(t)
	store := h.mainBook(3, promptsBook(5))
	h.openPage(rootFeed(), "reader-1")
	h.nextPage("reader-1")
	h.openPage(rootFeed(), "reader-2")

	// Act.
	page := h.nextPage("reader-2")

	// Assert.
	if got := store.readCount(); got != 2 {
		t.Fatalf("reads = %d, want the held page served from memory", got)
	}
	if got, want := strings.Join(rowIDs(pageRows(t, page)), ","), h.promptRowIDs("turn-0", "turn-1"); got != want {
		t.Fatalf("page rows = %v, want %v", got, want)
	}
}

func TestAWalkReachesTheConversationsStart(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.mainBook(3, promptsBook(8))
	h.openPage(rootFeed(), "reader-1")
	h.nextPage("reader-1")

	// Act.
	page := h.nextPage("reader-1")

	// Assert.
	if got := edgeOf(t, page); got != "at_start" {
		t.Fatalf("edge = %s, want at_start", got)
	}
	if got, want := strings.Join(rowIDs(pageRows(t, page)), ","), h.promptRowIDs("turn-0", "turn-1"); got != want {
		t.Fatalf("page rows = %v, want %v", got, want)
	}
}

func TestAWalkNeverAnswersHistoryReplayTruncated(t *testing.T) {
	// Arrange: the walk's every page is read from a store that has more.
	h := newHarness(t)
	h.mainBook(2, promptsBook(6))
	h.openPage(rootFeed(), "reader-1")

	// Act, Assert: every page of the walk to the start is a success.
	for range 3 {
		page, err := h.resolver.NextPage(context.Background(), testWorkspace, rootFeed(), "reader-1")
		if err != nil {
			t.Fatalf("NextPage: %v", err)
		}
		if page.GetError() != nil {
			t.Fatalf("page = %v, want a success", page.GetError())
		}
	}
}

func TestNextWhoseReadFailsIsHistoryUnavailable(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	store := h.mainBook(3, promptsBook(5))
	h.openPage(rootFeed(), "reader-1")
	store.failFrom = 2

	// Act.
	_, err := h.resolver.NextPage(context.Background(), testWorkspace, rootFeed(), "reader-1")

	// Assert.
	if !errors.Is(err, ErrHistoryUnavailable) {
		t.Fatalf("NextPage err = %v, want ErrHistoryUnavailable", err)
	}
}

func TestAPageWithNoBoundaryIsHistoryUnavailable(t *testing.T) {
	// Arrange: a store page stating neither floor nor more.
	h := newHarness(t)
	h.resolver.deps.History = historyFunc(func(context.Context, ids.WorkspaceID, *conversationv1.AgentId, *conversationv1.HistoryPointer) (*conversationv1.HistoryPage, error) {
		return &conversationv1.HistoryPage{Entries: newestFirst(promptsBook(1))}, nil
	})

	// Act.
	_, _, err := h.resolver.OpenPage(context.Background(), testWorkspace, rootFeed(), "reader-1")

	// Assert.
	if !errors.Is(err, ErrHistoryUnavailable) {
		t.Fatalf("OpenPage err = %v, want ErrHistoryUnavailable", err)
	}
}

// historyFunc adapts a function to a HistorySource.
type historyFunc func(context.Context, ids.WorkspaceID, *conversationv1.AgentId, *conversationv1.HistoryPointer) (*conversationv1.HistoryPage, error)

func (f historyFunc) ReadHistory(ctx context.Context, ws ids.WorkspaceID, target *conversationv1.AgentId, after *conversationv1.HistoryPointer) (*conversationv1.HistoryPage, error) {
	return f(ctx, ws, target, after)
}

// ---- a row split across pages ----

// splitTurnBook is turn-1's prompt on one page and its answer on the next
// newer one, at a store page of two: [p-3 ans-2, p-2 prompt-2] [p-1 ans-1,
// p-0 prompt-1].
func splitTurnBook() []bookEntry {
	return []bookEntry{
		{entry: promptEntry("turn-1", "first")},
		{turn: "turn-1", entry: frameEntry(mainAgent(), &conversationv1.AgentUpdate{Update: &conversationv1.AgentUpdate_Activity{Activity: responseSuccessActivity("ans-1", "first answer")}})},
		{entry: promptEntry("turn-2", "second")},
		{turn: "turn-2", entry: frameEntry(mainAgent(), &conversationv1.AgentUpdate{Update: &conversationv1.AgentUpdate_Activity{Activity: responseSuccessActivity("ans-2", "second answer")}})},
	}
}

func TestAnAnswerWhosePromptIsOnAnUnloadedPageIsWithheld(t *testing.T) {
	// Arrange: the newest page opens with turn-1's answer, whose prompt is
	// older (store page of three: ans-2, prompt-2, ans-1).
	h := newHarness(t)
	h.mainBook(3, splitTurnBook())

	// Act.
	page, _ := h.openPage(rootFeed(), "reader-1")

	// Assert.
	for _, id := range rowIDs(pageRows(t, page)) {
		if id == h.responseRowID("ans-1") {
			t.Fatalf("page rows = %v: turn-1's answer was drawn before its prompt's page loaded", rowIDs(pageRows(t, page)))
		}
	}
}

func TestAWithheldAnswerIsDrawnWholeOnceItsPromptsPageLoads(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.mainBook(3, splitTurnBook())
	h.openPage(rootFeed(), "reader-1")

	// Act.
	page := h.nextPage("reader-1")

	// Assert: the older page carries the prompt AND the answer withheld above.
	if got, want := strings.Join(rowIDs(pageRows(t, page)), ","), h.promptRowID("turn-1")+","+h.responseRowID("ans-1"); got != want {
		t.Fatalf("page rows = %v, want %v", got, want)
	}
}

func TestAWithheldEntryAtTheConversationsStartIsDrawn(t *testing.T) {
	// Arrange: a book whose only entry names a turn no prompt opened, on a
	// page that is the conversation's start.
	h := newHarness(t)
	h.mainBook(3, []bookEntry{{turn: "turn-x", entry: frameEntry(mainAgent(), &conversationv1.AgentUpdate{Update: &conversationv1.AgentUpdate_Activity{Activity: responseSuccessActivity("ans-x", "orphan")}})}})

	// Act.
	page, _ := h.openPage(rootFeed(), "reader-1")

	// Assert: nothing older can complete it, so it is drawn.
	if got := rowIDs(pageRows(t, page)); len(got) != 1 || got[0] != h.responseRowID("ans-x") {
		t.Fatalf("page rows = %v, want the orphan answer", got)
	}
}

// ---- what reaches a tail ----

func TestAnOlderPagesRowsAreNotPushed(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.mainBook(3, promptsBook(5))
	rows := h.follow(rootFeed(), "reader-1")

	// Act.
	h.nextPage("reader-1")
	sentinel := h.sendSentinel()

	// Assert.
	if got := pushedBefore(t, rows, sentinel); len(got) != 0 {
		t.Fatalf("pushed %v before the sentinel, want the older page delivered by the page alone", got)
	}
}

func TestAWithheldRowCompletedAboveTheWalkIsPushed(t *testing.T) {
	// Arrange: a late answer of turn-1 is written after turn-2's prompt, so
	// the newest page (late-1, prompt-2) withholds it, and once turn-1's page
	// loads it is drawn ABOVE the oldest row the reader already holds.
	h := newHarness(t)
	h.mainBook(2, []bookEntry{
		{entry: promptEntry("turn-1", "first")},
		{entry: promptEntry("turn-2", "second")},
		{turn: "turn-1", entry: frameEntry(mainAgent(), &conversationv1.AgentUpdate{Update: &conversationv1.AgentUpdate_Activity{Activity: responseSuccessActivity("late-1", "a late answer of turn-1")}})},
	})
	rows := h.follow(rootFeed(), "reader-1")

	// Act.
	h.nextPage("reader-1")
	sentinel := h.sendSentinel()

	// Assert.
	got := pushedBefore(t, rows, sentinel)
	if len(got) != 1 || got[0] != h.responseRowID("late-1") {
		t.Fatalf("pushed %v, want the completed answer", got)
	}
}

// ---- where the daemon's own rows sit ----

func TestADaemonRowInAnEmptyFeedSortsAtTheMomentItWasMade(t *testing.T) {
	// Arrange: a row the daemon makes before any history is loaded, at a
	// moment after every entry of the book.
	h := newHarness(t)
	h.mainBook(3, promptsBook(2))
	h.nowMs = 1_000
	h.resolver.UpsertSynthesized(testWorkspace, rootFeed(), &frontendv1.FeedRow{
		Id:  &frontendv1.FeedId{Value: "row|cold-gate"},
		Row: &frontendv1.FeedRow_Activity{Activity: &frontendv1.FeedTurnActivity{}},
	})

	// Act.
	page, _ := h.openPage(rootFeed(), "reader-1")

	// Assert: it is on the newest page, below the history it postdates.
	got := rowIDs(pageRows(t, page))
	if want := h.promptRowIDs("turn-0", "turn-1") + ",row|cold-gate"; strings.Join(got, ",") != want {
		t.Fatalf("page rows = %v, want %v", got, want)
	}
}

// ---- a reset ----

func TestAResetFeedReadsTheNewestPageAgain(t *testing.T) {
	// Arrange: the newest page was loaded, then a bind reset the feed.
	h := newHarness(t)
	store := h.mainBook(3, promptsBook(5))
	h.openPage(rootFeed(), "reader-1")
	h.resolver.ResetWorkspace(testWorkspace, "a transcript was selected")
	h.resolver.OnMainAgent(testWorkspace, mainAgent())

	// Act.
	h.openPage(rootFeed(), "reader-1")

	// Assert.
	if got := store.readCount(); got != 2 {
		t.Fatalf("reads = %d, want the new conversation's newest page read", got)
	}
}

func TestAPageReadAcrossAResetIsNotDrawn(t *testing.T) {
	// Arrange: the bind resets the feed while the page is being read.
	h := newHarness(t)
	store := newFakeHistory(3)
	store.books[""] = newestFirst(promptsBook(2))
	h.resolver.deps.History = historyFunc(func(ctx context.Context, ws ids.WorkspaceID, target *conversationv1.AgentId, after *conversationv1.HistoryPointer) (*conversationv1.HistoryPage, error) {
		h.resolver.ResetWorkspace(testWorkspace, "a transcript was selected")
		h.resolver.deps.History = store
		return store.ReadHistory(ctx, ws, target, after)
	})

	// Act.
	h.openPage(rootFeed(), "reader-1")

	// Assert.
	if !h.hasRecord("info", "daemon.feed.history_load_discarded") {
		t.Fatalf("records = %+v, want the stale page discarded", h.records())
	}
}

// ---- which feed is whose book ----

func TestBookTarget(t *testing.T) {
	lease := ids.LeaseID("lease-1")
	sub := &conversationv1.AgentId{Value: "agent-sub"}
	tests := []struct {
		name   string
		feed   feedid.Feed
		target string
		isBook bool
	}{
		{name: "the root is the main agent's book", feed: rootFeed(), target: "", isBook: true},
		{name: "a sub-feed is its agent's book", feed: feedid.Feed{Agent: sub}, target: "agent-sub", isBook: true},
		{name: "a merge tab is no book", feed: feedid.Feed{Merge: &lease}, isBook: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			target, isBook := bookTarget(tt.feed)

			// Assert.
			if isBook != tt.isBook || target.GetValue() != tt.target {
				t.Fatalf("bookTarget = %q, %v; want %q, %v", target.GetValue(), isBook, tt.target, tt.isBook)
			}
		})
	}
}

func TestAnUnstampedEntryAtAPagesHeadIsDrawn(t *testing.T) {
	// Arrange: the newest page opens with an unstamped answer, its prompt
	// older (store page of one).
	h := newHarness(t)
	h.mainBook(1, []bookEntry{
		{entry: promptEntry("turn-1", "first")},
		{entry: frameEntry(mainAgent(), &conversationv1.AgentUpdate{Update: &conversationv1.AgentUpdate_Activity{Activity: responseSuccessActivity("ans-1", "unstamped")}})},
	})

	// Act.
	page, _ := h.openPage(rootFeed(), "reader-1")

	// Assert: nothing names its turn, so it is drawn by position.
	if got := rowIDs(pageRows(t, page)); len(got) != 1 || got[0] != h.responseRowID("ans-1") {
		t.Fatalf("page rows = %v, want the unstamped answer", got)
	}
}

// ---- a reader that opened before a session was up ----

func TestAWatchOpeningLoadsTheNewestPageForAReaderThatHadNoSource(t *testing.T) {
	// Arrange: the feed was opened before a session was up; then one is.
	h := newHarness(t)
	store := h.mainBook(3, promptsBook(2))
	store.noSource = true
	rows := h.follow(rootFeed(), "reader-1")
	store.mu.Lock()
	store.noSource = false
	store.mu.Unlock()

	// Act: the session's main watch opens (tail_only: an empty page).
	h.resolver.OnHistoryPage(testWorkspace, mainAgent(), &conversationv1.HistoryPage{})

	// Assert: the newest page reaches the reader's tail.
	for _, want := range []string{h.promptRowID("turn-0"), h.promptRowID("turn-1")} {
		select {
		case row := <-rows:
			if row.GetId().GetValue() != want {
				t.Fatalf("pushed %q, want %q", row.GetId().GetValue(), want)
			}
		case <-time.After(tailWait):
			t.Fatalf("the newest page never reached the waiting reader's tail")
		}
	}
}

func TestAWatchOpeningLoadsNothingWithoutAWaitingReader(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	store := h.mainBook(3, promptsBook(2))

	// Act.
	h.resolver.OnHistoryPage(testWorkspace, mainAgent(), &conversationv1.HistoryPage{})
	h.openPage(rootFeed(), "reader-1")

	// Assert: only the reader's own open read.
	if got := store.readCount(); got != 1 {
		t.Fatalf("reads = %d, want the reader's open alone", got)
	}
}

func TestTheOldestPageAtTheStartCarriesRowsKeyedAboveTheHistory(t *testing.T) {
	// Arrange: a fork's ported question was drawn when its watch opened,
	// above the history the reader's open then loads.
	h := newHarness(t)
	h.mainBook(3, promptsBook(2))
	h.ported = []PortedPrompt{{Turn: "parent-1", Text: "the parent asked", Origin: conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT}}
	h.resolver.OnHistoryPage(testWorkspace, mainAgent(), &conversationv1.HistoryPage{})

	// Act.
	page, _ := h.openPage(rootFeed(), "reader-1")

	// Assert: the conversation's start is one page, from the very top.
	if got := len(pageRows(t, page)); got != 3 {
		t.Fatalf("page rows = %v, want the ported question and both prompts", rowIDs(pageRows(t, page)))
	}
}

// ---- the live gap a re-read newest page leaves ----

func TestAWalkIntoTheLiveGapReadsItsStorePages(t *testing.T) {
	// Arrange: the newest page was loaded at the start of the book, then the
	// conversation ran on live past a whole store page.
	h := newHarness(t)
	store := h.mainBook(2, promptsBook(1))
	h.openPage(rootFeed(), "reader-1")
	book := promptsBook(6)
	for i := 1; i < 6; i++ {
		h.deliverPromptAt(fmt.Sprintf("turn-%d", i), "prompt turn-"+fmt.Sprint(i), int64(i+1)*100)
	}
	store.mu.Lock()
	store.books[""] = newestFirst(book)
	store.mu.Unlock()
	h.openPage(rootFeed(), "reader-2")

	// Act.
	page := h.nextPage("reader-2")

	// Assert: the next page is the store's page below the newest, read.
	if got, want := strings.Join(rowIDs(pageRows(t, page)), ","), h.promptRowIDs("turn-2", "turn-3"); got != want {
		t.Fatalf("next page = %v, want the store page %v", got, want)
	}
	if got := store.lastRead().after; got != "p-4" {
		t.Fatalf("last read after %q, want p-4", got)
	}
}

// ---- a history source that is a shim, whatever its session is doing ----

// awaitPushed fails the test unless the tail pushes WANT, in order.
func awaitPushed(t *testing.T, rows <-chan *frontendv1.FeedRow, want ...string) {
	t.Helper()
	for _, id := range want {
		select {
		case row := <-rows:
			if row.GetId().GetValue() != id {
				t.Fatalf("pushed %q, want %q", row.GetId().GetValue(), id)
			}
		case <-time.After(tailWait):
			t.Fatalf("%q never reached the waiting reader's tail", id)
		}
	}
}

func TestSourceUpLoadsTheNewestPageForAReaderThatHadNoSource(t *testing.T) {
	// Arrange: the feed was opened before any shim was up; then one is.
	h := newHarness(t)
	store := h.mainBook(3, promptsBook(2))
	store.noSource = true
	rows := h.follow(rootFeed(), "reader-1")
	store.mu.Lock()
	store.noSource = false
	store.mu.Unlock()

	// Act.
	h.resolver.SourceUp(testWorkspace)

	// Assert.
	awaitPushed(t, rows, h.promptRowID("turn-0"), h.promptRowID("turn-1"))
}

func TestSourceUpLoadsNothingWithoutAWaitingReader(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	store := h.mainBook(3, promptsBook(2))
	h.openPage(rootFeed(), "reader-1")

	// Act.
	h.resolver.SourceUp(testWorkspace)
	h.openPage(rootFeed(), "reader-1")

	// Assert: only the first open read.
	if got := store.readCount(); got != 1 {
		t.Fatalf("reads = %d, want the first open's alone", got)
	}
}

func TestKeepNewestPageLoadsTheRootsNewestPage(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	store := h.mainBook(3, promptsBook(2))

	// Act.
	if err := h.resolver.KeepNewestPage(context.Background(), testWorkspace); err != nil {
		t.Fatalf("KeepNewestPage: %v", err)
	}

	// Assert: a reader that opens with no source is served the kept page.
	store.mu.Lock()
	store.noSource = true
	store.mu.Unlock()
	page, _ := h.openPage(rootFeed(), "reader-1")
	if got := strings.Join(rowIDs(pageRows(t, page)), ","); got != h.promptRowIDs("turn-0", "turn-1") {
		t.Fatalf("page rows = %v, want the kept newest page", got)
	}
}

func TestKeepNewestPageReadsNothingWhenTheNewestPageIsHeld(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	store := h.mainBook(3, promptsBook(2))
	h.openPage(rootFeed(), "reader-1")

	// Act.
	if err := h.resolver.KeepNewestPage(context.Background(), testWorkspace); err != nil {
		t.Fatalf("KeepNewestPage: %v", err)
	}

	// Assert.
	if got := store.readCount(); got != 1 {
		t.Fatalf("reads = %d, want the open's alone", got)
	}
}

func TestKeepNewestPageWhoseReadFailsIsHistoryUnavailable(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	store := h.mainBook(3, promptsBook(2))
	store.failFrom = 1

	// Act.
	err := h.resolver.KeepNewestPage(context.Background(), testWorkspace)

	// Assert.
	if !errors.Is(err, ErrHistoryUnavailable) {
		t.Fatalf("err = %v, want ErrHistoryUnavailable", err)
	}
}

func TestAPushedNewestLoadWithNoSourceIsLoadedByTheNextSource(t *testing.T) {
	// Arrange: the source went between the ask and the read.
	h := newHarness(t)
	store := h.mainBook(3, promptsBook(2))
	store.noSource = true
	if err := h.resolver.KeepNewestPage(context.Background(), testWorkspace); err != nil {
		t.Fatalf("KeepNewestPage: %v", err)
	}
	rows := h.follow(rootFeed(), "reader-1")
	store.mu.Lock()
	store.noSource = false
	store.mu.Unlock()

	// Act.
	h.resolver.SourceUp(testWorkspace)

	// Assert.
	awaitPushed(t, rows, h.promptRowID("turn-0"), h.promptRowID("turn-1"))
}
