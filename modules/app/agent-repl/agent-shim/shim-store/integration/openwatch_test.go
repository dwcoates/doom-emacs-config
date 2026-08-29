// openwatch_test.go — SUBJECT 3: the open/watch bifurcation.
//
// The open answers a page AND the address of the tail that follows it; the
// watch is a PURE tail pinned exactly after the page's newest item, so nothing
// is missed and nothing is doubled at the handoff. The token is the whole
// mechanism: it is minted at one open, consumed by one watch, and a caller
// cannot watch an agent it did not open.
package integration

import (
	"testing"

	storev1 "agentrepl/proto/store/v1"
)

// TestOpenAnswersAPageAndAToken is the shape of the open itself.
func TestOpenAnswersAPageAndAToken(t *testing.T) {
	// Arrange.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	cli := store.client()
	shim := streamProducer(cli)
	shim.write(ctx, t,
		shim.agentEntry("w-open-1", "u-open-1", frameLine(agentID("main"), responseFrame("main", "act-1", "only"))),
	)

	// Act.
	opened := openSession(ctx, t, cli, "main", 10, nil)

	// Assert.
	assertTexts(t, "the opening page", pageTexts(opened.GetPage()), []string{"only"})
	if opened.GetWatch().GetValue() == "" {
		t.Errorf("the open answered no watch token; a caller cannot watch an agent it did not open")
	}
	store.assertNoErrorRecords()
}

// TestEmptyBookIsALegalOpen: an agent with no rows is an empty book, not an
// unknown agent — the page is empty, the boundary is floor, the token is real.
func TestEmptyBookIsALegalOpen(t *testing.T) {
	// Arrange.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	cli := store.client()

	// Act.
	opened := openSession(ctx, t, cli, "never-written-to", 10, nil)

	// Assert.
	assertTexts(t, "an empty book's page", pageTexts(opened.GetPage()), nil)
	assertPageFloor(t, opened.GetPage())
	stream := watchStream(ctx, t, cli, opened.GetWatch())
	defer stream.Close()
	shim := streamProducer(cli)
	shim.write(ctx, t,
		shim.agentEntry("w-empty-1", "u-empty-1", frameLine(agentID("main"), responseFrame("never-written-to", "act-1", "first ever"))),
	)
	assertTexts(t, "the empty book's tail", receivedTexts(receiveLines(t, stream, 1)), []string{"first ever"})
	store.assertNoErrorRecords()
}

// TestWatchIsPinnedExactlyAfterThePage is the handoff invariant: a line
// written BEFORE the open is in the page and never on the stream.
func TestWatchIsPinnedExactlyAfterThePage(t *testing.T) {
	// Arrange.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	cli := store.client()
	shim := streamProducer(cli)
	shim.write(ctx, t,
		shim.agentEntry("w-pin-a", "u-pin-a", frameLine(agentID("main"), responseFrame("main", "act-a", "A"))),
	)

	// Act.
	opened := openSession(ctx, t, cli, "main", 10, nil)
	stream := watchStream(ctx, t, cli, opened.GetWatch())
	defer stream.Close()
	shim.write(ctx, t,
		shim.agentEntry("w-pin-b", "u-pin-b", frameLine(agentID("main"), responseFrame("main", "act-b", "B"))),
	)

	// Assert.
	assertTexts(t, "the page", pageTexts(opened.GetPage()), []string{"A"})
	assertTexts(t, "the tail", receivedTexts(receiveLines(t, stream, 1)), []string{"B"})
	store.assertNoErrorRecords()
}

// TestWriteRacingBetweenOpenAndWatchIsDeliveredExactlyOnce is the same
// invariant at its hard edge: a line written after the page was read but
// before the watch was dialed belongs to the stream, exactly once.
func TestWriteRacingBetweenOpenAndWatchIsDeliveredExactlyOnce(t *testing.T) {
	// Arrange.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	cli := store.client()
	shim := streamProducer(cli)
	shim.write(ctx, t,
		shim.agentEntry("w-race-a", "u-race-a", frameLine(agentID("main"), responseFrame("main", "act-a", "A"))),
	)
	opened := openSession(ctx, t, cli, "main", 10, nil)

	// Act: the racing write lands while no watcher is attached at all.
	shim.write(ctx, t,
		shim.agentEntry("w-race-b", "u-race-b", frameLine(agentID("main"), responseFrame("main", "act-b", "B"))),
	)
	stream := watchStream(ctx, t, cli, opened.GetWatch())
	defer stream.Close()
	shim.write(ctx, t,
		shim.agentEntry("w-race-c", "u-race-c", frameLine(agentID("main"), responseFrame("main", "act-c", "C"))),
	)

	// Assert: B is replayed from the pin, C arrives live, neither is doubled.
	assertTexts(t, "the tail across the handoff", receivedTexts(receiveLines(t, stream, 2)), []string{"B", "C"})
	store.assertNoErrorRecords()
}

// TestUpsertOfAnOldLineStreamsAtItsOriginalPointer: order is by FIRST insert,
// so a settling unit keeps the position it has always had.
func TestUpsertOfAnOldLineStreamsAtItsOriginalPointer(t *testing.T) {
	// Arrange.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	cli := store.client()
	shim := streamProducer(cli)
	shim.write(ctx, t,
		shim.agentEntry("w-up-a", "u-unit-a", frameLine(agentID("main"), responseFrame("main", "act-a", "A"))),
		shim.agentEntry("w-up-b", "u-unit-b", frameLine(agentID("main"), responseFrame("main", "act-b", "B"))),
	)
	opened := openSession(ctx, t, cli, "main", 10, nil)
	pointers := pagePointers(opened.GetPage())
	assertTexts(t, "the page before the upsert", pageTexts(opened.GetPage()), []string{"B", "A"})
	pointerOfA := pointers[1]

	stream := watchStream(ctx, t, cli, opened.GetWatch())
	defer stream.Close()

	// Act: unit A settles — same upsert_key, a new write.
	shim.write(ctx, t,
		shim.agentEntry("w-up-a2", "u-unit-a", frameLine(agentID("main"), responseFrame("main", "act-a", "A settled"))),
	)

	// Assert.
	got := receiveLines(t, stream, 1)
	assertTexts(t, "the upsert on the tail", receivedTexts(got), []string{"A settled"})
	if got[0].pointer != pointerOfA {
		t.Errorf("the upsert streamed at pointer %q, want its original %q", got[0].pointer, pointerOfA)
	}
	store.assertNoErrorRecords()
}

// TestWatchTokenIsSingleUse: the second watch on one token is refused.
func TestWatchTokenIsSingleUse(t *testing.T) {
	// Arrange.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	cli := store.client()
	opened := openSession(ctx, t, cli, "main", 10, nil)
	first := watchStream(ctx, t, cli, opened.GetWatch())
	defer first.Close()

	// Act.
	second := watchStream(ctx, t, cli, opened.GetWatch())
	defer second.Close()

	// Assert.
	assertWatchRefused(t, second)
}

// TestUnknownWatchTokenIsRefused: a token the store never minted buys nothing.
func TestUnknownWatchTokenIsRefused(t *testing.T) {
	// Arrange.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	cli := store.client()
	mark := store.logMark()

	// Act.
	stream := watchStream(ctx, t, cli, &storev1.AgentSessionToken{Value: "0123456789abcdef0123456789abcdef"})
	defer stream.Close()

	// Assert.
	assertWatchRefused(t, stream)
	refusals := recordsWithContextKey(store.logRecordsAfter(mark), "watch_token_hash")
	if len(refusals) == 0 {
		t.Errorf("a refused watch logged no record carrying watch_token_hash")
	}
}

// TestDroppedWatcherRecoversByReopeningWithKnownThrough: the recovery story is
// a re-open, and it hands back exactly the gap.
func TestDroppedWatcherRecoversByReopeningWithKnownThrough(t *testing.T) {
	// Arrange.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	cli := store.client()
	shim := streamProducer(cli)
	shim.write(ctx, t,
		shim.agentEntry("w-drop-a", "u-drop-a", frameLine(agentID("main"), responseFrame("main", "act-a", "A"))),
	)
	opened := openSession(ctx, t, cli, "main", 10, nil)
	stream := watchStream(ctx, t, cli, opened.GetWatch())
	shim.write(ctx, t,
		shim.agentEntry("w-drop-b", "u-drop-b", frameLine(agentID("main"), responseFrame("main", "act-b", "B"))),
	)
	got := receiveLines(t, stream, 1)
	assertTexts(t, "what the watcher saw before dropping", receivedTexts(got), []string{"B"})
	highWater := &storev1.StoreItemPointer{Value: got[0].pointer}

	// Act: the watcher drops, misses two writes, and re-opens at its own mark.
	if err := stream.Close(); err != nil {
		t.Fatalf("closing the watch stream: %v", err)
	}
	shim.write(ctx, t,
		shim.agentEntry("w-drop-c", "u-drop-c", frameLine(agentID("main"), responseFrame("main", "act-c", "C"))),
		shim.agentEntry("w-drop-d", "u-drop-d", frameLine(agentID("main"), responseFrame("main", "act-d", "D"))),
	)
	recovered := openSession(ctx, t, cli, "main", 10, highWater)

	// Assert: exactly the gap, nothing it already held.
	assertTexts(t, "the recovery page", pageTexts(recovered.GetPage()), []string{"D", "C"})
	store.assertNoErrorRecords()
}
