// readpage_test.go — SUBJECT 5: ReadAgentPage, the next-only walk.
//
// There is no first-page arm: the first page is always OpenAgentSession's
// answer and this verb only ever walks OLDER, from a pointer the store itself
// served. page_size rides each request, so a caller may change its budget in
// the middle of one walk.
package integration

import (
	"testing"

	storev1 "agentrepl/proto/store/v1"
)

// TestReadAgentPageWalksOlderNewestFirst is the ordinary continuation.
func TestReadAgentPageWalksOlderNewestFirst(t *testing.T) {
	// Arrange.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	cli := store.client()
	writeNumberedLines(ctx, t, streamProducer(cli), "main", 5)
	opened := openSession(ctx, t, cli, "main", 2, nil)
	assertTexts(t, "the opening page", pageTexts(opened.GetPage()), []string{"L5", "L4"})

	// Act.
	next := readPage(ctx, t, cli, "main", 2, assertPageMore(t, opened.GetPage()))

	// Assert.
	assertTexts(t, "the continuation", readTexts(next), []string{"L3", "L2"})
	store.assertNoErrorRecords()
}

// TestPageSizeMayVaryAcrossOneWalk: the budget is the caller's, per call.
func TestPageSizeMayVaryAcrossOneWalk(t *testing.T) {
	// Arrange.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	cli := store.client()
	writeNumberedLines(ctx, t, streamProducer(cli), "main", 6)
	opened := openSession(ctx, t, cli, "main", 1, nil)
	assertTexts(t, "the opening page", pageTexts(opened.GetPage()), []string{"L6"})

	// Act.
	wide := readPage(ctx, t, cli, "main", 3, assertPageMore(t, opened.GetPage()))
	narrow := readPage(ctx, t, cli, "main", 2, assertReadMore(t, wide))

	// Assert.
	assertTexts(t, "the wide continuation", readTexts(wide), []string{"L5", "L4", "L3"})
	assertTexts(t, "the narrow continuation", readTexts(narrow), []string{"L2", "L1"})
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
	writeNumberedLines(ctx, t, streamProducer(cli), "main", 3)
	opened := openSession(ctx, t, cli, "main", 2, nil)

	// Act.
	last := readPage(ctx, t, cli, "main", 2, assertPageMore(t, opened.GetPage()))

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
	writeNumberedLines(ctx, t, shim, "main", 2)
	registerEmptyBook(ctx, t, shim, "main", "unwritten", "unwritten")
	opened := openSession(ctx, t, cli, "main", 1, nil)
	pointerInMain := assertPageMore(t, opened.GetPage())

	// Act: the same store, a known agent with no rows of its own, read from its
	// own open.
	empty := openSession(ctx, t, cli, "unwritten", 5, nil)

	// Assert.
	assertTexts(t, "a known but unwritten book's page", pageTexts(empty.GetPage()), nil)
	assertPageFloor(t, empty.GetPage())

	// And a pointer from ANOTHER book is stale here, never silently accepted.
	assertReadStalePointer(t, readPageExpectingFailure(ctx, t, cli, &storev1.ReadAgentPageRequest{
		Book:     agentID("unwritten"),
		PageSize: 5,
		Position: &storev1.ReadAgentPageRequest_After{After: pointerInMain},
	}))
}

// TestContinuationLinesCarryTheSamePointersTheOpeningPageWouldHave: a
// continuation page's pointers are REAL positions, not placeholders — the same
// values OpenAgentSession serves for the same rows.
func TestContinuationLinesCarryTheSamePointersTheOpeningPageWouldHave(t *testing.T) {
	// Arrange: one book read two ways — a wide open that sees every row, and a
	// narrow open plus a continuation that walks to them.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	cli := store.client()
	writeNumberedLines(ctx, t, streamProducer(cli), "main", 4)
	wide := openSession(ctx, t, cli, "main", 4, nil)
	wantPointers := pagePointers(wide.GetPage())

	// Act.
	narrow := openSession(ctx, t, cli, "main", 2, nil)
	next := readPage(ctx, t, cli, "main", 2, assertPageMore(t, narrow.GetPage()))

	// Assert.
	assertTexts(t, "the continuation's pointers", readPointers(next), wantPointers[2:])
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
	writeNumberedLines(ctx, t, streamProducer(cli), "main", 5)
	opened := openSession(ctx, t, cli, "main", 1, nil)

	// Act.
	next := readPage(ctx, t, cli, "main", 2, assertPageMore(t, opened.GetPage()))

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
	writeNumberedLines(ctx, t, streamProducer(cli), "main", 4)
	opened := openSession(ctx, t, cli, "main", 2, nil)
	next := readPage(ctx, t, cli, "main", 1, assertPageMore(t, opened.GetPage()))
	mark := &storev1.StoreItemPointer{Value: readPointers(next)[0]}

	// Act.
	reopened := openSession(ctx, t, cli, "main", 10, mark)

	// Assert: only the rows NEWER than the walked-to mark. The walk reached L2,
	// so L3 and L4 are what the caller does not hold.
	assertTexts(t, "the page after a continuation mark", pageTexts(reopened.GetPage()), []string{"L4", "L3"})
	store.assertNoErrorRecords()
}
