// unowned_test.go — a page line written `owner_unknown` over the wire: the
// shim's task stream names no owner for a backgrounded subagent's own spawn,
// and the store files the line in the book that already holds its unit.
package integration

import (
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	storev1 "agentrepl/proto/store/v1"
)

// unownedLine is FRAME as an owner-unknown page line: no book, no attribution.
func unownedLine(f *conversationv1.AgentFrame) *storev1.StoreAgentUpdate {
	f.AgentId = nil
	return &storev1.StoreAgentUpdate{AgentInfo: &storev1.StoreAgentUpdate_ServeableFrame{ServeableFrame: &storev1.StorePageLine{
		Book:      &storev1.StorePageLine_OwnerUnknown{OwnerUnknown: &storev1.StorePageLineOwnerUnknown{}},
		AgentItem: frameItem(f),
	}}}
}

// THE PRODUCTION CASE (footer-activity-updates, 2026-09-30): the nested spawn
// toolu_01CieP7uiZSR86ZztFV134Gv lives in its spawner's book
// toolu_016fJ1MXgpBhD13bnUrwNzPE, and the shim's later write of the same unit
// names no owner.
func TestAnUnownedWriteLandsInTheBookThatHoldsItsUnit(t *testing.T) {
	// Arrange.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	cli := store.client()
	const spawner, unit = "toolu_016fJ1MXgpBhD13bnUrwNzPE", "toolu_01CieP7uiZSR86ZztFV134Gv"
	sidecar := fileProducer(cli)
	sidecar.write(ctx, t, sidecar.agentEntry("w-spawn", "activity:"+unit, frameLine(agentID(spawner), responseFrame(spawner, unit, "spawned"))))
	shim := streamProducer(cli)

	// Act.
	shim.write(ctx, t, shim.agentEntry("w-beat", "activity:"+unit, unownedLine(responseFrame("", unit, "progressed"))))

	// Assert.
	page := openSession(ctx, t, cli, spawner, 10, nil)
	assertTexts(t, "the spawner's book after the unowned write", pageTexts(page.GetPage()), []string{"progressed"})
	store.assertNoErrorRecords()
}

func TestAnUnownedWriteOfANewUnitIsAnsweredUnplaced(t *testing.T) {
	// Arrange.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	shim := streamProducer(store.client())

	// Act.
	resp, err := shim.attempt(ctx, &storev1.EntryBatch{Entries: []*storev1.StoreEntry{
		shim.agentEntry("w-beat", "activity:toolu_new", unownedLine(responseFrame("", "toolu_new", "progressed"))),
	}})

	// Assert.
	if err != nil {
		t.Fatalf("WriteBatch transport error: %v", err)
	}
	unplaced := resp.GetSuccess().GetUnplaced()
	if len(unplaced) != 1 || unplaced[0].GetUpsertKey() != "activity:toolu_new" {
		t.Fatalf("response = %v, want the entry answered unplaced", resp)
	}
}
