// scoping_test.go — SUBJECT 7: never-served exclusion and logical-session
// scoping.
//
// Three facts the store must hold across the whole read surface: an unserved
// row is first-class as NEVER-SERVED (it has no book at all, so nothing can
// return it), a keep-alive is not stored at all (the arm is retired), and the
// store scopes by OUR main-agent id — the vendor's session id is a mutable
// attribute, so its rotation must never split an agent's book.
package integration

import (
	"agentrepl/shim-store/internal/db"
	"agentrepl/shim-store/internal/testclose"
	"fmt"
	"testing"

	storev1 "agentrepl/proto/store/v1"
)

// TestAnUnservedRowBetweenTwoRealRowsNeverAppears: the unserved row sits in the
// same position space as the lines around it and is still invisible to every
// read.
func TestAnUnservedRowBetweenTwoRealRowsNeverAppears(t *testing.T) {
	// Arrange.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	cli := store.client()
	shim := streamProducer(cli)

	// Act.
	shim.write(ctx, t,
		shim.agentEntry("w-ka-before", "u-ka-before", frameLine(agentID("main"), responseFrame("main", "act-1", "before"))),
		shim.agentEntry("w-ka", "u-ka-turn", vendorSpecificLine("vendor_only_thing")),
		shim.agentEntry("w-ka-after", "u-ka-after", frameLine(agentID("main"), responseFrame("main", "act-2", "after"))),
	)

	// Assert: the page is the two real rows, adjacent.
	opened := openSession(ctx, t, cli, "main", nil)
	assertTexts(t, "a book straddling an unserved row", pageTexts(opened.GetPage()), []string{"after", "before"})
	assertPageFloor(t, opened.GetPage())
	store.assertNoErrorRecords()
}

// TestAStorePageOfRealRowsAroundAnUnservedRowIsComplete: an unserved row takes
// no room in a page, so a book of exactly one page of real rows reaches the
// floor rather than reporting a short page with more below it.
func TestAStorePageOfRealRowsAroundAnUnservedRowIsComplete(t *testing.T) {
	// Arrange.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	cli := store.client()
	shim := streamProducer(cli)
	shim.write(ctx, t,
		shim.agentEntry("w-ka-before", "u-ka-before", frameLine(agentID("main"), responseFrame("main", "act-1", "before"))),
		shim.agentEntry("w-ka", "u-ka-turn", vendorSpecificLine("vendor_only_thing")),
	)
	writeNumberedLines(ctx, t, shim, "main", db.PageSize-1)

	// Act.
	sized := openSession(ctx, t, cli, "main", nil)

	// Assert.
	assertTexts(t, "a page of exactly the real rows", pageTexts(sized.GetPage()), append(descendingLabels(db.PageSize-1, 1), "before"))
	assertPageFloor(t, sized.GetPage())
	store.assertNoErrorRecords()
}

// TestIdentityRotationDoesNotSplitTheBook: the pages before and after a
// vendor-session rotation are ONE continuous walk under the same AgentId.
func TestIdentityRotationDoesNotSplitTheBook(t *testing.T) {
	// Arrange.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	cli := store.client()
	shim := streamProducer(cli)
	shim.write(ctx, t,
		shim.agentEntry("w-rot-1", "u-rot-1", frameLine(agentID("main"), responseFrame("main", "act-1", "L1"))),
		shim.agentEntry("w-rot-2", "u-rot-2", frameLine(agentID("main"), responseFrame("main", "act-2", "L2"))),
	)

	// Act: the vendor rotates the session id under us, then work continues.
	shim.write(ctx, t,
		shim.sessionEntry("w-rot-su", "u-rot-session", identityRotated("vendor-first", "vendor-second")),
	)
	after := make([]*storev1.StoreEntry, 0, db.PageSize)
	var afterTexts []string
	for i := db.PageSize; i >= 1; i-- {
		label := fmt.Sprintf("R%d", i)
		afterTexts = append(afterTexts, label)
		after = append([]*storev1.StoreEntry{shim.agentEntry("w-rot-"+label, "u-rot-"+label,
			frameLine(agentID("main"), responseFrame("main", "act-"+label, label)))}, after...)
	}
	shim.write(ctx, t, after...)

	// Assert: one book, walked straight through the rotation.
	opened := openSession(ctx, t, cli, "main", nil)
	assertTexts(t, "the page after the rotation", pageTexts(opened.GetPage()), afterTexts)

	across := readPage(ctx, t, cli, "main", assertPageMore(t, opened.GetPage()))
	assertTexts(t, "the page across the rotation", readTexts(across), []string{"L2", "L1"})
	assertReadFloor(t, across)
	store.assertNoErrorRecords()
}

// TestRotationDoesNotSplitTheWatchedTail is the same invariant on the stream:
// a watcher pinned before the rotation sees the lines after it, unbroken.
func TestRotationDoesNotSplitTheWatchedTail(t *testing.T) {
	// Arrange.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	cli := store.client()
	shim := streamProducer(cli)
	shim.write(ctx, t,
		shim.agentEntry("w-rotw-1", "u-rotw-1", frameLine(agentID("main"), responseFrame("main", "act-1", "L1"))),
	)
	opened := openSession(ctx, t, cli, "main", nil)
	stream := watchStream(ctx, t, cli, opened.GetWatch())
	defer testclose.OrFail(t, stream)

	// Act.
	shim.write(ctx, t,
		shim.sessionEntry("w-rotw-su", "u-rotw-session", identityRotated("vendor-first", "vendor-second")),
		shim.agentEntry("w-rotw-2", "u-rotw-2", frameLine(agentID("main"), responseFrame("main", "act-2", "L2"))),
	)

	// Assert: the session fact is not a line, and L2 arrives on the same tail.
	assertTexts(t, "the tail across the rotation", receivedTexts(receiveLines(t, stream, 1)), []string{"L2"})
	store.assertNoErrorRecords()
}

// TestAnUnservedRowItselfSurvivesARestart proves the ROW, not the ledger.
//
// Absorption alone became a weak proof once write_ids got their own ledger
// table: a replay is absorbed because the LEDGER remembers the write, which
// would still hold if the entry row itself had been lost. What cannot happen
// unless the row survived is an identity refusal — the store can only object
// that this upsert_key would change KIND if it still holds a row under that key.
// An unserved row becoming a page line is a KIND change (vendor_specific →
// page_line), and a kind change stays a batch-fatal refusal even though a mere
// book move is now a per-entry skip: nothing legitimate ever re-ingests a row as
// a different kind.
func TestAnUnservedRowItselfSurvivesARestart(t *testing.T) {
	// Arrange.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	shim := streamProducer(store.client())
	shim.write(ctx, t, shim.agentEntry("w-ka-row", "u-ka-row", vendorSpecificLine("vendor_only_thing")))

	// Act: after a restart, claim the same key for a PAGE LINE.
	store.restart()
	after, cancelAfter := callContext(t)
	defer cancelAfter()
	revived := streamProducer(store.client())
	failure := revived.writeExpectingFailure(after, t, nil,
		revived.agentEntry("w-ka-row-2", "u-ka-row",
			frameLine(agentID("main"), responseFrame("main", "act-1", "would overwrite the unserved row"))))

	// Assert: the refusal can only exist because the row is still there. It is
	// the KIND half of the identity check that fires — the stored row is
	// vendor_specific and this write claims the key for a page line, which is
	// checked before the book move (never-served NULL → the agent) that also
	// holds here.
	assertWriteInvalidRequest(t, failure, "entries[0].agent_update")
	// An unserved row is never an agent's first sight, so the refused page line
	// left the store with no agent row for "main" at all.
	openUnknownAgent(after, t, store.client(), "main")
}

// TestAnUnservedRowIsHeldDurablyEvenThoughItIsNeverServed: never-served is not
// dropped — the write is still absorbed after a restart, so the store kept it.
func TestAnUnservedRowIsHeldDurablyEvenThoughItIsNeverServed(t *testing.T) {
	// Arrange.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	shim := streamProducer(store.client())
	unserved := shim.agentEntry("w-ka-durable", "u-ka-durable", vendorSpecificLine("vendor_only_thing"))
	shim.write(ctx, t, unserved)

	// Act: the same write_id replayed after a restart must be ABSORBED, which
	// is only possible if the row was actually kept.
	store.restart()
	after, cancelAfter := callContext(t)
	defer cancelAfter()
	replayed := streamProducer(store.client())
	resp, err := replayed.attempt(after, &storev1.EntryBatch{Entries: []*storev1.StoreEntry{unserved}})
	if err != nil {
		t.Fatalf("replaying an unserved row after restart: %v", err)
	}

	// Assert.
	if resp.GetSuccess() == nil {
		t.Fatalf("replaying a durable unserved row was refused: %s", resp.GetFailure().GetDetail())
	}
	openUnknownAgent(after, t, store.client(), "main")
	store.assertNoErrorRecords()
}

// TestAKeepAliveIsRefusedAsRetired: nothing of a keep-alive is stored on either
// plane, so the arm is refused on its own field and recorded at its own site.
func TestAKeepAliveIsRefusedAsRetired(t *testing.T) {
	// Arrange.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	shim := streamProducer(store.client())
	mark := store.logMark()

	// Act.
	failure := shim.writeExpectingFailure(ctx, t, nil, shim.agentEntry("w-ka-retired", "u-ka-retired",
		keepaliveLine(agentID("main"), promptFact("turn-ka-retired", "main", "warm"))))

	// Assert.
	assertWriteInvalidRequest(t, failure, "entries[0].agent_update.unserved_item.keepalive")
	rec := assertExactlyOneNormalRecord(t, recordsAtOperation(store.logRecordsAfter(mark), "store.rpc.write-batch"), "the refused keep-alive")
	assertRefusalKeys(t, rec, "keepalive_retired", "invalid_request")
}
