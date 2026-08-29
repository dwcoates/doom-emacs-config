// knownthrough_test.go — SUBJECT 4: known_through, the caller's own high-water
// mark.
//
// The store tracks NOTHING about what it previously served. UNSET means
// repaint; SET means catch-up. When the gap since the mark is wider than
// page_size, the page holds the newest page_size items and `more` points INTO
// the gap, which the caller walks older until it meets its own mark.
package integration

import (
	"context"
	"fmt"
	"testing"

	storev1 "agentrepl/proto/store/v1"
)

// writeNumberedLines writes n page lines labelled L1..Ln to one book and
// returns nothing: every assertion below reads them back through the service.
func writeNumberedLines(ctx context.Context, t *testing.T, shim *producer, book string, n int) {
	t.Helper()
	entries := make([]*storev1.StoreEntry, 0, n)
	for i := 1; i <= n; i++ {
		label := fmt.Sprintf("L%d", i)
		entries = append(entries, shim.agentEntry(
			fmt.Sprintf("w-%s-%s", book, label),
			fmt.Sprintf("u-%s-%s", book, label),
			frameLine(agentID(book), responseFrame(book, "act-"+label, label)),
		))
	}
	shim.write(ctx, t, entries...)
}

// TestKnownThroughUnsetRepaintsNewestFirst is the repaint arm.
func TestKnownThroughUnsetRepaintsNewestFirst(t *testing.T) {
	// Arrange.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	cli := store.client()
	writeNumberedLines(ctx, t, streamProducer(cli), "main", 3)

	// Act.
	opened := openSession(ctx, t, cli, "main", 10, nil)

	// Assert.
	assertTexts(t, "a full repaint", pageTexts(opened.GetPage()), []string{"L3", "L2", "L1"})
	assertPageFloor(t, opened.GetPage())
	store.assertNoErrorRecords()
}

// TestKnownThroughSetServesOnlyNewerItems is the catch-up arm: never the mark
// itself, never anything older.
func TestKnownThroughSetServesOnlyNewerItems(t *testing.T) {
	// Arrange.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	cli := store.client()
	shim := streamProducer(cli)
	writeNumberedLines(ctx, t, shim, "main", 4)
	repaint := openSession(ctx, t, cli, "main", 10, nil)
	assertTexts(t, "the repaint", pageTexts(repaint.GetPage()), []string{"L4", "L3", "L2", "L1"})
	markOfL2 := &storev1.StoreItemPointer{Value: pagePointers(repaint.GetPage())[2]}

	// Act.
	caught := openSession(ctx, t, cli, "main", 10, markOfL2)

	// Assert.
	assertTexts(t, "the catch-up page", pageTexts(caught.GetPage()), []string{"L4", "L3"})
	assertPageFloor(t, caught.GetPage())
	store.assertNoErrorRecords()
}

// TestGapWiderThanPageSizeIsWalkedUntilTheMarkIsMet: the boundary points INTO
// the gap and ReadAgentPage walks it down to the caller's own mark.
func TestGapWiderThanPageSizeIsWalkedUntilTheMarkIsMet(t *testing.T) {
	// Arrange.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	cli := store.client()
	shim := streamProducer(cli)
	writeNumberedLines(ctx, t, shim, "main", 7)
	repaint := openSession(ctx, t, cli, "main", 10, nil)
	// The caller holds through L2; L3..L7 is a five-wide gap.
	markOfL2 := &storev1.StoreItemPointer{Value: pagePointers(repaint.GetPage())[5]}

	// Act.
	caught := openSession(ctx, t, cli, "main", 2, markOfL2)

	// Assert: the newest two, and a boundary pointing into the gap.
	assertTexts(t, "the bounded catch-up page", pageTexts(caught.GetPage()), []string{"L7", "L6"})
	cursor := assertPageMore(t, caught.GetPage())

	next := readPage(ctx, t, cli, "main", 2, cursor)
	assertTexts(t, "the first continuation", readTexts(next), []string{"L5", "L4"})
	cursor = assertReadMore(t, next)

	// The walk ends when the caller MEETS its own mark: the store knows nothing
	// about it, so L2 is served and the caller stops on recognizing it.
	last := readPage(ctx, t, cli, "main", 2, cursor)
	assertTexts(t, "the walk down to the mark", readTexts(last), []string{"L3", "L2"})
	store.assertNoErrorRecords()
}

// TestStaleKnownThroughPointerIsRefused: a pointer that names no position in
// THAT book is a typed failure, never a silently repainted page.
func TestStaleKnownThroughPointerIsRefused(t *testing.T) {
	// Arrange.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	cli := store.client()
	shim := streamProducer(cli)
	writeNumberedLines(ctx, t, shim, "main", 2)
	writeNumberedLines(ctx, t, shim, "other", 2)
	otherBook := openSession(ctx, t, cli, "other", 10, nil)
	pointerInAnotherBook := &storev1.StoreItemPointer{Value: pagePointers(otherBook.GetPage())[0]}

	// Act.
	detail := openSessionExpectingFailure(ctx, t, cli, &storev1.OpenAgentSessionRequest{
		Agent:        agentID("main"),
		PageSize:     10,
		KnownThrough: pointerInAnotherBook,
	})

	// Assert.
	if detail == "" {
		t.Errorf("the stale-pointer refusal carried no detail")
	}
}
