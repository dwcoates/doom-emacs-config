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

// TestAFailedBatchDoesNotAdvanceTheCursor asserts the store's refusal is
// honored: the same position is re-offered rather than moved past.
func TestAFailedBatchDoesNotAdvanceTheCursor(t *testing.T) {
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	captured := loadCapturedSession(t)
	fake.FailWrites(3, "the store refused this batch")

	// Act.
	startSidecar(t, defaultSidecarOptions(t, fake.Socket, tree))
	g := newGrowingFile(t, tree.sessionPath(captured.Slug, captured.Session))
	for _, line := range captured.Lines {
		g.AppendLine(line)
	}
	fake.awaitBatches(ctx, t, 4)

	// Assert: the first three batches all state the SAME cursor, because none of
	// them was ever committed.
	batches := batchesCarryingCursorFor(fake.Batches(), g.Path())
	if len(batches) < 2 {
		t.Fatalf("the sidecar offered %d cursor-bearing batches, wanted at least 2 to compare", len(batches))
	}
	first := batches[0].GetBatch().GetCursorAdvance().GetOffset()
	second := batches[1].GetBatch().GetCursorAdvance().GetOffset()
	if second > first {
		t.Errorf("the cursor advanced from %d to %d across a refused batch; a failure commits nothing", first, second)
	}
}

// TestARefusedBatchIsReplayedIdentically asserts the re-sent records carry the
// SAME write_ids, so the store can absorb them once it recovers.
func TestARefusedBatchIsReplayedIdentically(t *testing.T) {
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	captured := loadCapturedSession(t)
	fake.FailWrites(2, "the store refused this batch")

	// Act.
	startSidecar(t, defaultSidecarOptions(t, fake.Socket, tree))
	g := newGrowingFile(t, tree.sessionPath(captured.Slug, captured.Session))
	for _, line := range captured.Lines {
		g.AppendLine(line)
	}
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

// TestAStoreOutageSuspendsProductionOfEveryFile asserts a refusal suspends ALL
// production, not just the file that was refused.
func TestAStoreOutageSuspendsProductionOfEveryFile(t *testing.T) {
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	captured := loadCapturedSession(t)
	cwd := "/Users/dodgecoates/outage-second-file-probe"
	slug := cwdSlug(cwd)
	other := "60606060-6060-4060-8060-606060606060"
	fake.FailWrites(100, "the store is refusing everything")

	// Act: the outage begins, and only then does a SECOND file appear.
	startSidecar(t, defaultSidecarOptions(t, fake.Socket, tree))
	g := newGrowingFile(t, tree.sessionPath(captured.Slug, captured.Session))
	for _, line := range captured.Lines {
		g.AppendLine(line)
	}
	fake.awaitBatches(ctx, t, 2)

	second := newGrowingFile(t, tree.sessionPath(slug, other))
	second.AppendLine(encodeRecord(t, retargetSession(t, decodeRecord(t, captured.Lines[7]), other, cwd)))
	fake.awaitBatches(ctx, t, 4)

	// Assert: nothing was produced for the second file while the store refused.
	if latestCursorFor(fake.Batches(), second.Path()) != nil {
		t.Errorf("a store outage must suspend production of EVERY file; %s was still being written", second.Path())
	}
}

// TestRecoveryReadsCursorsBeforeWritingAgain asserts the store-unreachable
// invariant: production resumes with cursor-then-rescan, so a GetSidecarCursors
// precedes the next WriteBatch.
func TestRecoveryReadsCursorsBeforeWritingAgain(t *testing.T) {
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	captured := loadCapturedSession(t)
	fake.FailWrites(2, "a transient store failure")

	// Act.
	startSidecar(t, defaultSidecarOptions(t, fake.Socket, tree))
	g := newGrowingFile(t, tree.sessionPath(captured.Slug, captured.Session))
	for _, line := range captured.Lines {
		g.AppendLine(line)
	}
	awaitCursorInBatches(ctx, t, fake, g.Path(), g.Offset())

	// Assert: between the last failing write and the first succeeding one there
	// is a cursor read.
	calls := fake.Calls()
	lastFailingWrite := nthCall(calls, "WriteBatch", 2) // the second failure
	if lastFailingWrite < 0 {
		t.Fatalf("expected at least two WriteBatch calls; the calls were %v", calls)
	}
	var sawCursorRead bool
	for _, name := range calls[lastFailingWrite+1:] {
		if name == "GetSidecarCursors" {
			sawCursorRead = true
			break
		}
		if name == "WriteBatch" {
			t.Fatalf("production resumed with a WriteBatch before recovering cursors; the calls were %v", calls)
		}
	}
	if !sawCursorRead {
		t.Fatalf("no GetSidecarCursors followed the outage; the calls were %v", calls)
	}
}

// TestEveryProductionCycleBeginsWithACursorRead asserts the very first rpc the
// sidecar makes is GetSidecarCursors — never a write from a position the store
// did not hand it.
func TestEveryProductionCycleBeginsWithACursorRead(t *testing.T) {
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	captured := loadCapturedSession(t)

	// Act.
	startSidecar(t, defaultSidecarOptions(t, fake.Socket, tree))
	g := newGrowingFile(t, tree.sessionPath(captured.Slug, captured.Session))
	for _, line := range captured.Lines {
		g.AppendLine(line)
	}
	fake.awaitBatches(ctx, t, 1)

	// Assert.
	calls := fake.Calls()
	if len(calls) == 0 || calls[0] != "GetSidecarCursors" {
		t.Fatalf("the first rpc was %v, wanted GetSidecarCursors", calls)
	}
}

// TestAFailedCursorReadProducesNothing asserts a refused cursor read suspends
// production just as a refused write does.
func TestAFailedCursorReadProducesNothing(t *testing.T) {
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	captured := loadCapturedSession(t)
	opts := defaultSidecarOptions(t, fake.Socket, tree)
	fake.FailCursors("the store cannot read its cursors")

	// Act.
	startSidecar(t, opts)
	g := newGrowingFile(t, tree.sessionPath(captured.Slug, captured.Session))
	for _, line := range captured.Lines {
		g.AppendLine(line)
	}
	awaitLog(ctx, t, opts.LogPath, "the suspension warning after a refused cursor read", func(r logRecord) bool {
		return r.Level == "warn"
	})

	// Assert.
	for _, b := range fake.Batches() {
		if len(b.GetBatch().GetEntries()) > 0 {
			t.Fatalf("the sidecar wrote %d entries while its cursor read was refused; it must produce NOTHING",
				len(b.GetBatch().GetEntries()))
		}
	}
}

// nthCall answers the index of the nth (1-based) call with a name, or -1.
func nthCall(calls []string, name string, n int) int {
	seen := 0
	for i, c := range calls {
		if c != name {
			continue
		}
		seen++
		if seen == n {
			return i
		}
	}
	return -1
}
