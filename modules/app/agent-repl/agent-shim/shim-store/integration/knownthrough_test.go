// knownthrough_test.go — SUBJECT 4: known_through, the caller's own high-water
// mark.
//
// The store tracks NOTHING about what it previously served. UNSET means
// repaint; SET means catch-up. When the gap since the mark is wider than the
// store's page (db.PageSize), the page holds the newest page of items and
// `more` points INTO the gap, which the caller walks older until it meets its
// own mark.
package integration

import (
	"context"
	"fmt"
	"testing"

	storev1 "agentrepl/proto/store/v1"
	"agentrepl/proto/store/v1/storev1connect"
	"agentrepl/shim-store/internal/db"
	"agentrepl/shim-store/internal/testclose"
)

// descendingLabels is the labels writeNumberedLines gave L<newest> down to
// L<oldest>, in the order a page serves them.
func descendingLabels(newest, oldest int) []string {
	out := make([]string, 0, newest-oldest+1)
	for i := newest; i >= oldest; i-- {
		out = append(out, fmt.Sprintf("L%d", i))
	}
	return out
}

// walkBook reads one whole book, newest first — the repaint and then every
// continuation to the floor — and answers every line's pointer.
func walkBook(ctx context.Context, t *testing.T, cli storev1connect.ShimStoreClient, book string) []string {
	t.Helper()
	opened := openSession(ctx, t, cli, book, nil)
	pointers := pagePointers(opened.GetPage())
	if opened.GetPage().GetFloor() != nil {
		return pointers
	}
	cursor := assertPageMore(t, opened.GetPage())
	for {
		next := readPage(ctx, t, cli, book, cursor)
		pointers = append(pointers, readPointers(next)...)
		if next.GetFloor() != nil {
			return pointers
		}
		cursor = assertReadMore(t, next)
	}
}

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
	opened := openSession(ctx, t, cli, "main", nil)

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
	repaint := openSession(ctx, t, cli, "main", nil)
	assertTexts(t, "the repaint", pageTexts(repaint.GetPage()), []string{"L4", "L3", "L2", "L1"})
	markOfL2 := &storev1.StoreItemPointer{Value: pagePointers(repaint.GetPage())[2]}

	// Act.
	caught := openSession(ctx, t, cli, "main", markOfL2)

	// Assert.
	assertTexts(t, "the catch-up page", pageTexts(caught.GetPage()), []string{"L4", "L3"})
	assertPageFloor(t, caught.GetPage())
	store.assertNoErrorRecords()
}

// TestGapWiderThanPageSizeIsWalkedUntilTheMarkIsMet: the boundary points INTO
// the gap and ReadAgentPage walks it down to the caller's own mark, one store
// page at a time.
func TestGapWiderThanPageSizeIsWalkedUntilTheMarkIsMet(t *testing.T) {
	// Arrange: the caller holds through L2; L3..L(2P+2) is a gap two pages wide.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	cli := store.client()
	shim := streamProducer(cli)
	newest := 2*db.PageSize + 2
	writeNumberedLines(ctx, t, shim, "main", newest)
	pointers := walkBook(ctx, t, cli, "main")
	markOfL2 := &storev1.StoreItemPointer{Value: pointers[newest-2]}

	// Act.
	caught := openSession(ctx, t, cli, "main", markOfL2)

	// Assert: the newest page, and a boundary pointing into the gap.
	assertTexts(t, "the bounded catch-up page", pageTexts(caught.GetPage()), descendingLabels(newest, newest-db.PageSize+1))
	cursor := assertPageMore(t, caught.GetPage())

	next := readPage(ctx, t, cli, "main", cursor)
	assertTexts(t, "the first continuation", readTexts(next), descendingLabels(newest-db.PageSize, 3))
	cursor = assertReadMore(t, next)

	// The walk ends when the caller MEETS its own mark: the store knows nothing
	// about it, so L2 is served and the caller stops on recognizing it.
	last := readPage(ctx, t, cli, "main", cursor)
	assertTexts(t, "the walk down to the mark", readTexts(last), []string{"L2", "L1"})
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
	otherBook := openSession(ctx, t, cli, "other", nil)
	pointerInAnotherBook := &storev1.StoreItemPointer{Value: pagePointers(otherBook.GetPage())[0]}

	// Act.
	failure := openSessionExpectingFailure(ctx, t, cli, &storev1.OpenAgentSessionRequest{
		Agent:   agentID("main"),
		Opening: &storev1.OpenAgentSessionRequest_KnownThrough{KnownThrough: pointerInAnotherBook},
	})

	// Assert: a well-formed pointer naming no row of THIS book is stale, not
	// malformed — the caller's recovery is a repaint, not a bug fix.
	assertOpenStalePointer(t, failure)
}

// TestACatchUpGapExactlyThePageSizeReachesTheFloor is the off-by-one.
//
// `more` vs `floor` is decided by asking for ONE MORE ROW than the page holds,
// so the boundary is observed rather than counted. The case that separates a
// correct implementation from a `>=` is a gap of EXACTLY the page size: the
// page is full, and there is nothing below it. Answering `more` there sends
// the caller walking for a page that does not exist; answering `floor` is the
// truth.
func TestACatchUpGapExactlyThePageSizeReachesTheFloor(t *testing.T) {
	// Arrange: the caller holding L2, and the gap L3..L(P+2) exactly one page.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	cli := store.client()
	shim := streamProducer(cli)
	newest := db.PageSize + 2
	writeNumberedLines(ctx, t, shim, "main", newest)
	pointers := walkBook(ctx, t, cli, "main")
	knownThrough := &storev1.StoreItemPointer{Value: pointers[newest-2]} // L2, newest-first

	// Act
	page := openSession(ctx, t, cli, "main", knownThrough)

	// Assert
	assertTexts(t, "the catch-up page", pageTexts(page.GetPage()), descendingLabels(newest, 3))
	assertPageFloor(t, page.GetPage())
	store.assertNoErrorRecords()
}

// TestACatchUpGapOneAboveThePageSizeReportsMore is the control for the subject
// above: one more row in the gap, and the boundary must flip.
func TestACatchUpGapOneAboveThePageSizeReportsMore(t *testing.T) {
	// Arrange: the caller holding L2, and the gap L3..L(P+3) one row over a page.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	cli := store.client()
	shim := streamProducer(cli)
	newest := db.PageSize + 3
	writeNumberedLines(ctx, t, shim, "main", newest)
	pointers := walkBook(ctx, t, cli, "main")
	knownThrough := &storev1.StoreItemPointer{Value: pointers[newest-2]} // L2

	// Act
	page := openSession(ctx, t, cli, "main", knownThrough)

	// Assert
	assertTexts(t, "the catch-up page", pageTexts(page.GetPage()), descendingLabels(newest, 4))
	if got := assertPageMore(t, page.GetPage()).GetValue(); got != pointers[newest-4] {
		t.Fatalf("more.last_item = %q, want L4's pointer %q", got, pointers[newest-4])
	}
}

// TestTailOnlyOpensOnAnEmptyPageAtTheFloor: tail_only replays no history, and
// an empty page has no oldest line for `more` to name, so it reports the floor
// — nothing to walk FROM it. History is reached by a repaint.
func TestTailOnlyOpensOnAnEmptyPageAtTheFloor(t *testing.T) {
	// Arrange.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	cli := store.client()
	writeNumberedLines(ctx, t, streamProducer(cli), "main", db.PageSize+1)

	// Act.
	opened := openTailOnly(ctx, t, cli, "main")

	// Assert.
	assertTexts(t, "the tail-only page", pageTexts(opened.GetPage()), nil)
	assertPageFloor(t, opened.GetPage())
	store.assertNoErrorRecords()
}

// TestTailOnlyWatchDeliversOnlyLinesWrittenAfterTheOpen: the tail begins after
// the newest line as of the open, so the stream carries the later line and
// none of the history before it.
func TestTailOnlyWatchDeliversOnlyLinesWrittenAfterTheOpen(t *testing.T) {
	// Arrange.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	cli := store.client()
	shim := streamProducer(cli)
	writeNumberedLines(ctx, t, shim, "main", 3)
	opened := openTailOnly(ctx, t, cli, "main")
	stream := watchStream(ctx, t, cli, opened.GetWatch())
	defer testclose.OrFail(t, stream)

	// Act.
	shim.write(ctx, t, shim.agentEntry("w-main-later", "u-main-later",
		frameLine(agentID("main"), responseFrame("main", "act-later", "later"))))

	// Assert.
	assertTexts(t, "the tail", receivedTexts(receiveLines(t, stream, 1)), []string{"later"})
	store.assertNoErrorRecords()
}

// TestTailOnlyOpenNamesTheBooksNewestItem: the anchor a tail-only caller
// stands on without reading a page.
func TestTailOnlyOpenNamesTheBooksNewestItem(t *testing.T) {
	// Arrange.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	cli := store.client()
	writeNumberedLines(ctx, t, streamProducer(cli), "main", 3)
	head := pagePointers(openSession(ctx, t, cli, "main", nil).GetPage())[0]

	// Act.
	opened := openTailOnly(ctx, t, cli, "main")

	// Assert.
	if got := opened.GetNewest().GetValue(); got != head {
		t.Fatalf("newest = %q, want the book's head %q", got, head)
	}
	store.assertNoErrorRecords()
}
