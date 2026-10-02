// order_test.go — SUBJECT 6: order is by FIRST insert, never last write.
//
// This is what makes a StoreItemPointer stable and a walk safe: a unit that
// settles while a caller is paging cannot teleport across a continuation and
// be seen twice, or jump over the boundary and be missed.
package integration

import (
	"agentrepl/shim-store/internal/db"
	"testing"
)

// TestUpsertMidWalkDoesNotTeleportAcrossAContinuation.
func TestUpsertMidWalkDoesNotTeleportAcrossAContinuation(t *testing.T) {
	// Arrange: two units, and a full store page of newer ones above them.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	cli := store.client()
	shim := streamProducer(cli)
	shim.write(ctx, t,
		shim.agentEntry("w-ord-a", "u-ord-a", frameLine(agentID("main"), responseFrame("main", "act-a", "A"))),
		shim.agentEntry("w-ord-b", "u-ord-b", frameLine(agentID("main"), responseFrame("main", "act-b", "B"))),
	)
	writeNumberedLines(ctx, t, shim, "main", db.PageSize)
	opened := openSession(ctx, t, cli, "main", nil)
	assertTexts(t, "the opening page", pageTexts(opened.GetPage()), descendingLabels(db.PageSize, 1))
	cursor := assertPageMore(t, opened.GetPage())

	// Act: B settles between the two pages — a new write of an OLD unit.
	shim.write(ctx, t,
		shim.agentEntry("w-ord-b2", "u-ord-b", frameLine(agentID("main"), responseFrame("main", "act-b", "B settled"))),
	)
	next := readPage(ctx, t, cli, "main", cursor)

	// Assert: B is still between A and L1, carrying its settled content.
	assertTexts(t, "the continuation after a mid-walk upsert", readTexts(next), []string{"B settled", "A"})
	store.assertNoErrorRecords()
}
