package integration

import (
	"testing"

	"google.golang.org/protobuf/proto"
)

// SUBJECT 6 — reading WriteBatchResponse.
//
// SUCCESS means durable. FAILURE means NOTHING was committed, so the sidecar
// does not advance: it needs no retry buffer and no spill, because its sources
// are durable files it re-reads from the last committed cursor. A store error
// suspends ALL production until a full recover-cursors-then-rescan succeeds.

// TestARefusedBatchIsReplayedIdentically asserts the re-sent records carry the
// SAME write_ids, so the store can absorb them once it recovers.
func TestARefusedBatchIsReplayedIdentically(t *testing.T) {
	t.Parallel()
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	captured := loadCapturedSession(t)
	fake.FailWrites(2, "the store refused this batch")

	// The file is complete before the sidecar starts, so the replayed batch is
	// compared against the refused one rather than against a longer file.
	g := newGrowingFile(t, tree.sessionPath(captured.Slug, captured.Session))
	for _, line := range captured.Lines {
		g.AppendLine(line)
	}

	// Act.
	startSidecar(t, defaultSidecarOptions(t, fake.Socket, tree))
	fake.awaitBatches(ctx, t, 3)

	// Assert.
	batches := fake.Batches()
	firstIDs := writeIDList(batches[0].GetBatch().GetEntries())
	var replay []string
	for _, b := range batches[1:] {
		replay = writeIDList(b.GetBatch().GetEntries())
		if len(replay) > 0 {
			break
		}
	}
	if len(firstIDs) == 0 || len(replay) == 0 {
		t.Fatalf("expected two non-empty batches to compare; got %d and %d entries", len(firstIDs), len(replay))
	}
	for i := range firstIDs {
		if i >= len(replay) {
			break
		}
		if firstIDs[i] != replay[i] {
			t.Fatalf("the replayed batch minted write_id %q where the refused one minted %q; a re-read must be byte-identical",
				replay[i], firstIDs[i])
		}
	}
	if !proto.Equal(batches[0].GetBatch(), batches[1].GetBatch()) {
		t.Errorf("the replayed batch differs from the refused one; a failure commits nothing, so the same batch is re-offered whole")
	}
}
