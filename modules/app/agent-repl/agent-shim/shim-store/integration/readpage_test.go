// readpage_test.go — SUBJECT 5: ReadAgentPage, the next-only walk.
//
// There is no first-page arm: the first page is always OpenAgentSession's
// answer and this verb only ever walks OLDER, from a pointer the store itself
// served. Every page is the store's own size (db.PageSize): no request field
// carries a budget.
package integration

import (
	"testing"

	storev1 "agentrepl/proto/store/v1"
	"agentrepl/shim-store/internal/db"
)

// TestReadAgentPageWalksOlderNewestFirst is the ordinary continuation.
func TestReadAgentPageWalksOlderNewestFirst(t *testing.T) {
	// Arrange.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	cli := store.client()
	newest := 2*db.PageSize + 1
	writeNumberedLines(ctx, t, streamProducer(cli), "main", newest)
	opened := openSession(ctx, t, cli, "main", nil)
	assertTexts(t, "the opening page", pageTexts(opened.GetPage()), descendingLabels(newest, newest-db.PageSize+1))

	// Act.
	next := readPage(ctx, t, cli, "main", assertPageMore(t, opened.GetPage()))

	// Assert.
	assertTexts(t, "the continuation", readTexts(next), descendingLabels(newest-db.PageSize, 2))
	store.assertNoErrorRecords()
}

// TestEveryPageOfOneWalkIsTheStorePage: no caller picks a budget, so every
// full page of a walk is the store's page and only the last one runs short.
func TestEveryPageOfOneWalkIsTheStorePage(t *testing.T) {
	// Arrange.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	cli := store.client()
	writeNumberedLines(ctx, t, streamProducer(cli), "main", 2*db.PageSize+1)
	opened := openSession(ctx, t, cli, "main", nil)

	// Act.
	next := readPage(ctx, t, cli, "main", assertPageMore(t, opened.GetPage()))
	last := readPage(ctx, t, cli, "main", assertReadMore(t, next))

	// Assert.
	got := []int{len(opened.GetPage().GetLines()), len(next.GetLines()), len(last.GetLines())}
	if want := []int{db.PageSize, db.PageSize, 1}; got[0] != want[0] || got[1] != want[1] || got[2] != want[2] {
		t.Fatalf("page sizes = %v, want %v", got, want)
	}
	store.assertNoErrorRecords()
}

// TestFloorArmMarksTheOldestRetainedLine: the walk's end is an ARM, never an
// empty page the caller has to infer from.
func TestFloorArmMarksTheOldestRetainedLine(t *testing.T) {
	// Arrange.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	cli := store.client()
	writeNumberedLines(ctx, t, streamProducer(cli), "main", db.PageSize+1)
	opened := openSession(ctx, t, cli, "main", nil)

	// Act.
	last := readPage(ctx, t, cli, "main", assertPageMore(t, opened.GetPage()))

	// Assert.
	assertTexts(t, "the final page", readTexts(last), []string{"L1"})
	assertReadFloor(t, last)
	store.assertNoErrorRecords()
}

// TestKnownButUnwrittenBookReadsEmptyAtFloor: a book its agent has written
// nothing to is empty and complete, not a refusal.
func TestKnownButUnwrittenBookReadsEmptyAtFloor(t *testing.T) {
	// Arrange.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	cli := store.client()
	shim := streamProducer(cli)
	writeNumberedLines(ctx, t, shim, "main", db.PageSize+1)
	registerEmptyBook(ctx, t, shim, "main", "unwritten", "unwritten")
	opened := openSession(ctx, t, cli, "main", nil)
	pointerInMain := assertPageMore(t, opened.GetPage())

	// Act: the same store, a known agent with no rows of its own, read from its
	// own open.
	empty := openSession(ctx, t, cli, "unwritten", nil)

	// Assert.
	assertTexts(t, "a known but unwritten book's page", pageTexts(empty.GetPage()), nil)
	assertPageFloor(t, empty.GetPage())

	// And a pointer from ANOTHER book is stale here, never silently accepted.
	assertReadStalePointer(t, readPageExpectingFailure(ctx, t, cli, &storev1.ReadAgentPageRequest{
		Book:     agentID("unwritten"),
		Position: &storev1.ReadAgentPageRequest_After{After: pointerInMain},
	}))
}

// TestContinuationLinesCarryTheSamePointersTheOpeningPageWouldHave: a
// continuation page's pointers are REAL positions, not placeholders — the same
// values OpenAgentSession serves for the same rows.
func TestContinuationLinesCarryTheSamePointersTheOpeningPageWouldHave(t *testing.T) {
	// Arrange: one page of lines read by an open while the book still fits in
	// it, then two more lines, so a later open must walk to the oldest two.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	cli := store.client()
	shim := streamProducer(cli)
	writeNumberedLines(ctx, t, shim, "main", db.PageSize)
	wide := openSession(ctx, t, cli, "main", nil)
	wantPointers := pagePointers(wide.GetPage())
	shim.write(ctx, t,
		shim.agentEntry("w-main-extra-1", "u-main-extra-1", frameLine(agentID("main"), responseFrame("main", "act-extra-1", "extra-1"))),
		shim.agentEntry("w-main-extra-2", "u-main-extra-2", frameLine(agentID("main"), responseFrame("main", "act-extra-2", "extra-2"))),
	)

	// Act.
	narrow := openSession(ctx, t, cli, "main", nil)
	next := readPage(ctx, t, cli, "main", assertPageMore(t, narrow.GetPage()))

	// Assert.
	assertTexts(t, "the continuation's pointers", readPointers(next), wantPointers[db.PageSize-2:])
	store.assertNoErrorRecords()
}

// TestContinuationMoreArmEchoesTheLastLinesOwnPointer: the boundary and the
// lines cannot disagree, because the boundary IS one of the lines.
func TestContinuationMoreArmEchoesTheLastLinesOwnPointer(t *testing.T) {
	// Arrange.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	cli := store.client()
	writeNumberedLines(ctx, t, streamProducer(cli), "main", 2*db.PageSize+1)
	opened := openSession(ctx, t, cli, "main", nil)

	// Act.
	next := readPage(ctx, t, cli, "main", assertPageMore(t, opened.GetPage()))

	// Assert.
	pointers := readPointers(next)
	if got := assertReadMore(t, next).GetValue(); got != pointers[len(pointers)-1] {
		t.Fatalf("more.last_item = %q, want the last line's own pointer %q", got, pointers[len(pointers)-1])
	}
}

// TestAContinuationPointerReopensTheSessionAtThatMark: the pointers a
// continuation served are accepted as known_through, which is the whole reason
// they must be real.
func TestAContinuationPointerReopensTheSessionAtThatMark(t *testing.T) {
	// Arrange.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	cli := store.client()
	newest := db.PageSize + 2
	writeNumberedLines(ctx, t, streamProducer(cli), "main", newest)
	opened := openSession(ctx, t, cli, "main", nil)
	next := readPage(ctx, t, cli, "main", assertPageMore(t, opened.GetPage()))
	mark := &storev1.StoreItemPointer{Value: readPointers(next)[0]}

	// Act.
	reopened := openSession(ctx, t, cli, "main", mark)

	// Assert: only the rows NEWER than the walked-to mark. The walk reached L2,
	// so L3 and above are what the caller does not hold.
	assertTexts(t, "the page after a continuation mark", pageTexts(reopened.GetPage()), descendingLabels(newest, 3))
	store.assertNoErrorRecords()
}
