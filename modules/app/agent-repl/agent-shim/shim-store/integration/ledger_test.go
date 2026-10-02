// ledger_test.go — SUBJECT: replaying a SUPERSEDED write.
//
// A producer's retry buffer is not ordered against the store's history: it can
// re-send w1 long after w2 settled the same row. Absorption is therefore a
// question about the WRITE — "has this write ever been applied?" — and the
// store answers it from its write ledger. Asking the row instead answered "is
// this the write that currently owns the row?", which read a superseded replay
// as never-seen, overwrote the settled content with the stale content, and
// re-delivered the regression to every live watcher.
package integration

import (
	"agentrepl/shim-store/internal/testclose"
	"context"
	"testing"

	storev1 "agentrepl/proto/store/v1"
)

// supersededPair writes "A" and then settles the SAME upsert_key with
// "A settled", and hands back the original write for the replay.
func supersededPair(ctx context.Context, t *testing.T, shim *producer) *storev1.StoreEntry {
	t.Helper()
	first := shim.agentEntry("w-super-1", "u-super", frameLine(agentID("main"), responseFrame("main", "act-super", "A")))
	settled := shim.agentEntry("w-super-2", "u-super", frameLine(agentID("main"), responseFrame("main", "act-super", "A settled")))
	shim.write(ctx, t, first)
	shim.write(ctx, t, settled)
	return first
}

func TestReplayingASupersededWriteIsAcknowledgedAsSuccess(t *testing.T) {
	// Arrange
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	shim := streamProducer(store.client())
	replayed := supersededPair(ctx, t, shim)

	// Act + Assert: absorption is not a lesser success, so the producer retires
	// the batch exactly as it would a first landing.
	shim.write(ctx, t, replayed)
	store.assertNoErrorRecords()
}

func TestReplayingASupersededWriteLeavesTheSettledLineInThePage(t *testing.T) {
	// Arrange
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	cli := store.client()
	shim := streamProducer(cli)
	replayed := supersededPair(ctx, t, shim)

	// Act
	shim.write(ctx, t, replayed)

	// Assert
	page := openSession(ctx, t, cli, "main", nil)
	assertTexts(t, "the book after a superseded replay", pageTexts(page.GetPage()), []string{"A settled"})
}

func TestReplayingASupersededWriteDeliversNothingToAWatcher(t *testing.T) {
	// Arrange: the watcher's NEXT frame is the assertion. A regressed row would
	// have been published first, so the very next frame naming the later write
	// proves nothing was published in between — with no sleep and no polling.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	cli := store.client()
	shim := streamProducer(cli)
	replayed := supersededPair(ctx, t, shim)
	opened := openSession(ctx, t, cli, "main", nil)
	stream := watchStream(ctx, t, cli, opened.GetWatch())
	defer testclose.OrFail(t, stream)

	// Act
	shim.write(ctx, t, replayed)
	shim.write(ctx, t, shim.agentEntry("w-super-3", "u-super-next",
		frameLine(agentID("main"), responseFrame("main", "act-next", "next"))))

	// Assert
	assertTexts(t, "the tail after a superseded replay", receivedTexts(receiveLines(t, stream, 1)), []string{"next"})
}
