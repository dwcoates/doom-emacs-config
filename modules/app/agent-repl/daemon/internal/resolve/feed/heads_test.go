package feed

import (
	"context"
	"errors"
	"strings"
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/feedid"
	"agent-repld/internal/ids"
)

// headsBook is longTurnBook(5) on a store page of two, the store serving turn
// heads: [ans-5 ans-4 | head prompt-1] [ans-3 ans-2 | head prompt-1]
// [ans-1 prompt-1] [prompt-0].
func (h *harness) headsBook() *fakeHistory {
	store := h.mainBook(2, longTurnBook(5))
	store.heads = true
	return store
}

// earlierStepsRowID is TURN's earlier-steps row on the root feed.
func (h *harness) earlierStepsRowID(turn string) string {
	return testEncode(feedid.Ref{
		WS: testWorkspace, Feed: rootFeed(),
		Row: feedid.RowKey{Kind: feedid.KindPrompt, ID: turn, Sub: earlierStepsSub},
	}).GetValue()
}

// pushedRowsBefore drains a tail up to the sentinel row, answering every
// publication whole (removals included).
func pushedRowsBefore(t *testing.T, rows <-chan *frontendv1.FeedRow, sentinel string) []*frontendv1.FeedRow {
	t.Helper()
	var got []*frontendv1.FeedRow
	for {
		select {
		case row := <-rows:
			if id := row.GetId().GetValue(); id == sentinel {
				return got
			}
			got = append(got, row)
		case <-time.After(tailWait):
			t.Fatalf("the sentinel %q never reached the tail; pushed so far: %v", sentinel, rowIDs(got))
		}
	}
}

func TestAnOpeningWithTurnHeadsReadsOneStorePage(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	store := h.headsBook()

	// Act.
	h.openPage(rootFeed(), "reader-1")

	// Assert.
	if got := store.readCount(); got != 1 {
		t.Fatalf("reads = %d, want exactly one", got)
	}
}

func TestAnOpeningWithoutTurnHeadsWalksOlderPagesAsBefore(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	store := h.mainBook(2, longTurnBook(5))

	// Act.
	h.openPage(rootFeed(), "reader-1")

	// Assert: [ans-5 ans-4] and [ans-3 ans-2] are withheld; [ans-1 prompt-1]
	// draws the turn.
	if got := store.readCount(); got != 3 {
		t.Fatalf("reads = %d, want the old walk's three", got)
	}
}

func TestAnOpeningFromOneReadIsRecordedAtInfo(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.headsBook()

	// Act.
	h.openPage(rootFeed(), "reader-1")

	// Assert.
	if !h.hasRecordWith(dlog.LevelInfo, "daemon.feed.opening_reads", "reads", 1) {
		t.Fatal("no INFO record says the opening was served from one read")
	}
}

func TestAHeadsPageDrawsThePromptTheGapRowAndItsSteps(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.headsBook()

	// Act.
	page, _ := h.openPage(rootFeed(), "reader-1")

	// Assert.
	want := strings.Join([]string{
		h.promptRowID("turn-1"), h.earlierStepsRowID("turn-1"),
		h.responseRowID("ans-4"), h.responseRowID("ans-5"),
	}, ",")
	if got := strings.Join(rowIDs(pageRows(t, page)), ","); got != want {
		t.Fatalf("page rows = %v, want %v", got, want)
	}
}

func TestTheGapRowNamesItsTurn(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.headsBook()

	// Act.
	h.openPage(rootFeed(), "reader-1")

	// Assert.
	row, ok := h.findRow(rootFeed(), h.earlierStepsRowID("turn-1"))
	if !ok || row.GetEarlierSteps() == nil || row.GetTurn().GetValue() != "turn-1" {
		t.Fatalf("gap row = %v, want an earlier_steps row of turn-1", row)
	}
}

func TestADrawnGapRowIsRecordedAtInfo(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.headsBook()

	// Act.
	h.openPage(rootFeed(), "reader-1")

	// Assert.
	if !h.hasRecordWith(dlog.LevelInfo, "daemon.feed.earlier_steps_drawn", "turn", "turn-1") {
		t.Fatal("no INFO record says the gap row was drawn")
	}
}

func TestTheGapRowIsRemovedWhenTheWalkLoadsTheHeadsOwnPage(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.headsBook()
	rows := h.follow(rootFeed(), "reader-1")

	// Act.
	h.nextPage("reader-1")
	pushed := pushedRowsBefore(t, rows, h.sendSentinel())

	// Assert.
	removed := false
	for _, row := range pushed {
		removed = removed || (row.GetRemoved() != nil && row.GetId().GetValue() == h.earlierStepsRowID("turn-1"))
	}
	if !removed {
		t.Fatalf("pushed %v, want the gap row's removal", rowIDs(pushed))
	}
	if _, stands := h.findRow(rootFeed(), h.earlierStepsRowID("turn-1")); stands {
		t.Fatal("the gap row's removal was pushed but the feed still holds it")
	}
}

func TestTheGapRowStandsWhileTheWalkHasNotReachedTheHead(t *testing.T) {
	// Arrange: a store page of three leaves the head on the third page:
	// [ans-5 ans-4 ans-3] [ans-2 ans-1 prompt-1].
	h := newHarness(t)
	store := h.mainBook(3, longTurnBook(5))
	store.heads = true
	h.openPage(rootFeed(), "reader-1")

	// Act: re-open, which serves from memory and reads nothing older.
	h.openPage(rootFeed(), "reader-2")

	// Assert.
	if _, stands := h.findRow(rootFeed(), h.earlierStepsRowID("turn-1")); !stands {
		t.Fatal("the gap row was removed though no load met its head")
	}
}

func TestAHeadMetAgainOnAnOlderPageIsAbsorbed(t *testing.T) {
	// Arrange: the second page [ans-3 ans-2] carries prompt-1 as its head
	// again.
	h := newHarness(t)
	h.headsBook()
	h.openPage(rootFeed(), "reader-1")

	// Act.
	h.nextPage("reader-1")

	// Assert.
	if got := len(h.recordsAt(dlog.LevelInfo, "daemon.feed.earlier_steps_drawn")); got != 1 {
		t.Fatalf("gap rows drawn = %d, want the one the opening drew", got)
	}
	if !h.hasRecordWith(dlog.LevelDebug, "daemon.feed.turn_head_absorbed", "turn", "turn-1") {
		t.Fatal("no record says the repeated head was absorbed")
	}
}

func TestAHeadMetAgainLeavesOnePromptRow(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.headsBook()
	h.openPage(rootFeed(), "reader-1")

	// Act.
	h.nextPage("reader-1")

	// Assert.
	n := 0
	for _, row := range h.rows(rootFeed()) {
		if row.GetUserPrompt() != nil && row.GetTurn().GetValue() == "turn-1" {
			n++
		}
	}
	if n != 1 {
		t.Fatalf("turn-1 prompt rows = %d, want one", n)
	}
}

func TestAHeadThatIsNotAPromptIsAnErrorAndDrawsNothing(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	bogus := newestFirst([]bookEntry{answerEntry("turn-1", "stray")})[0]
	steps := newestFirst(longTurnBook(1))[:1]
	h.resolver.deps.History = historyFunc(func(context.Context, ids.WorkspaceID, *conversationv1.AgentId, *conversationv1.HistoryPointer) (*conversationv1.HistoryPage, error) {
		return &conversationv1.HistoryPage{
			Entries:   steps,
			TurnHeads: []*conversationv1.HistoryEntryAt{bogus},
			Boundary:  &conversationv1.HistoryPage_Floor{Floor: &conversationv1.HistoryFloor{}},
		}, nil
	})

	// Act.
	h.openPage(rootFeed(), "reader-1")

	// Assert.
	if !h.hasRecord(dlog.LevelError, "daemon.feed.turn_head_not_prompt") {
		t.Fatal("no ERROR record names the head that is not a prompt")
	}
	if _, drawn := h.findRow(rootFeed(), h.responseRowID("stray")); drawn {
		t.Fatal("a head that is not a prompt was drawn")
	}
}

func TestACatchUpPagesTurnHeadsAreNotDrawn(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	book := newestFirst(longTurnBook(1))
	page := &conversationv1.HistoryPage{
		Entries:   book[:1],
		TurnHeads: []*conversationv1.HistoryEntryAt{book[1]},
		Boundary:  &conversationv1.HistoryPage_More{More: &conversationv1.HistoryMore{LastEntry: book[0].GetAt()}},
	}

	// Act.
	h.replay(page)

	// Assert.
	if _, drawn := h.findRow(rootFeed(), h.promptRowID("turn-1")); drawn {
		t.Fatal("a watch's catch-up page drew a turn head")
	}
}

func TestAGapRowOfAMirroredTurnIsNotMirrored(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	lease := ids.LeaseID("lease-7")
	mirroredTurn(h, "turn-1", pickMirror(lease))
	h.headsBook()

	// Act.
	h.openPage(rootFeed(), "reader-1")

	// Assert: the turn's own rows stand in the mirror, its fold does not.
	mirrored := rowsOf(h.resolver, requesterWorkspace, feedid.Feed{Merge: &lease})
	if len(mirrored) == 0 {
		t.Fatal("the mirror holds no row of the turn; the case proves nothing")
	}
	for _, row := range mirrored {
		if row.GetEarlierSteps() != nil {
			t.Fatalf("the mirror holds the earlier-steps row %v", row.GetId())
		}
	}
}

func TestReadNewestHoldsThePageForTheNextOpening(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	store := h.headsBook()
	if err := h.resolver.ReadNewest(context.Background(), testWorkspace); err != nil {
		t.Fatalf("ReadNewest: %v", err)
	}

	// Act.
	h.openPage(rootFeed(), "reader-1")

	// Assert.
	if got := store.readCount(); got != 1 {
		t.Fatalf("reads = %d, want only ReadNewest's own", got)
	}
}

func TestReadNewestReadsNothingWhenThePageIsHeld(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	store := h.headsBook()
	h.openPage(rootFeed(), "reader-1")

	// Act.
	err := h.resolver.ReadNewest(context.Background(), testWorkspace)

	// Assert.
	if err != nil || store.readCount() != 1 {
		t.Fatalf("err = %v, reads = %d; want nil and only the opening's read", err, store.readCount())
	}
}

func TestReadNewestWithNoSourceIsErrNoHistorySource(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.headsBook().noSource = true

	// Act.
	err := h.resolver.ReadNewest(context.Background(), testWorkspace)

	// Assert.
	if !errors.Is(err, ErrNoHistorySource) {
		t.Fatalf("err = %v, want ErrNoHistorySource", err)
	}
}

func TestReadNewestWhoseReadFailsIsHistoryUnavailable(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.headsBook().failFrom = 1

	// Act.
	err := h.resolver.ReadNewest(context.Background(), testWorkspace)

	// Assert.
	if !errors.Is(err, ErrHistoryUnavailable) {
		t.Fatalf("err = %v, want ErrHistoryUnavailable", err)
	}
}
