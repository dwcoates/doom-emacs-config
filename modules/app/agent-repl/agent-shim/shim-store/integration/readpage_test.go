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

// TestUnknownBookReadsEmptyAtFloor: a book nothing was ever written to is
// empty and complete, not a refusal.
func TestUnknownBookReadsEmptyAtFloor(t *testing.T) {
	// Arrange.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	cli := store.client()
	writeNumberedLines(ctx, t, streamProducer(cli), "main", 2)
	opened := openSession(ctx, t, cli, "main", 1, nil)
	pointerInMain := assertPageMore(t, opened.GetPage())

	// Act: the same store, a book with no rows, read from its own open.
	empty := openSession(ctx, t, cli, "unwritten", 5, nil)

	// Assert.
	assertTexts(t, "an unknown book's page", pageTexts(empty.GetPage()), nil)
	assertPageFloor(t, empty.GetPage())

	// And a pointer from ANOTHER book is stale here, never silently accepted.
	detail := readPageExpectingFailure(ctx, t, cli, &storev1.ReadAgentPageRequest{
		Book:     agentID("unwritten"),
		PageSize: 5,
		After:    pointerInMain,
	})
	if detail == "" {
		t.Errorf("the stale-pointer refusal carried no detail")
	}
}
