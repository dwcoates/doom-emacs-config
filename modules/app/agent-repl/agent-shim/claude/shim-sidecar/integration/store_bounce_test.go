package integration

import (
	storev1 "agentrepl/proto/store/v1"
	"testing"

	"google.golang.org/protobuf/proto"
)

// CRITIQUE 3 — a REAL store bounce in the middle of an ingest.
//
// The store-unreachable invariant is written against a store that dies and comes
// back, and every other subject about it drives that through a fake store that
// merely refuses. This one stops the real store binary and starts it again on
// the SAME socket and the SAME database, which is what a launchd restart of the
// store does to a running sidecar.
//
// WHAT MUST HOLD ACROSS IT IS EXACTLY WHAT HOLDS ACROSS A RESTART: no gap and no
// repeat. The cursor rides its records in one transaction, so the position the
// recovered store hands back is the position those records committed at; the
// bytes written while the store was gone are re-read from there; and the records
// re-read on the way (the boot rewind's, and the batch the outage refused) mint
// IDENTICAL write_ids, so the store absorbs them as the same success arm rather
// than storing them twice.

// TestAStoreBounceMidIngestLeavesNoGapAndNoRepeat stops the store while the
// vendor keeps writing, brings it back with everything it had made durable, and
// asserts the book holds every unit of the file exactly once, each re-sent row
// under the write_id it was first sent with (the store absorbs a replay by its
// write_id; that absorption is tested in the store's own suite).
func TestAStoreBounceMidIngestLeavesNoGapAndNoRepeat(t *testing.T) {
	t.Parallel()
	// Arrange: one store, taken down and brought back on the same socket with
	// its durable state, so the second one is genuinely the same store.
	ctx, cancel := testContext(t)
	defer cancel()
	socket := shortSocketPath(t, "bounce")
	store := startFakeStoreAt(t, socket)
	tree := newVendorTree(t)
	captured := loadCapturedSession(t)
	opts := defaultSidecarOptions(t, socket, tree)
	cut := 9 // the first response's blocks are on disk; its result is not

	// Act: ingest the head, take the store away, keep WRITING, bring it back.
	startSidecar(t, opts)
	g := newGrowingFile(t, tree.sessionPath(captured.Slug, captured.Session))
	for _, line := range captured.Lines[:cut] {
		g.AppendLine(line)
	}
	awaitCursorAtLeast(ctx, t, store.Client, g.Path(), 1)
	store.Stop()

	for _, line := range captured.Lines[cut:] {
		g.AppendLine(line)
	}
	store.Resume()
	lines := store.awaitBookUnits(ctx, t, captured.Session,
		capturedThinking1, capturedBashCall1, capturedThinking2, capturedBashCall2)

	// Assert: every unit is present, and none of them twice.
	seen := map[string]int{}
	for _, line := range lines {
		if a := activityOf(line); a != nil {
			seen[a.GetActivityId().GetValue()]++
		}
	}
	// And every row the replay re-sent kept its write_id: the same row (its
	// key and content alike) is never sent under two. A start and its settle
	// share a key but are different rows, each with its own write_id.
	writeIDs := map[string]map[string]bool{}
	for _, e := range entriesOf(store.Batches()) {
		content := proto.Clone(e).(*storev1.StoreEntry)
		content.WriteId = ""
		raw, err := proto.MarshalOptions{Deterministic: true}.Marshal(content)
		if err != nil {
			t.Fatalf("marshal a written entry: %v", err)
		}
		if writeIDs[string(raw)] == nil {
			writeIDs[string(raw)] = map[string]bool{}
		}
		writeIDs[string(raw)][e.GetWriteId()] = true
	}
	for _, ids := range writeIDs {
		if len(ids) != 1 {
			t.Errorf("one row was sent under %d write_ids across the bounce, %v; a replay must re-mint the same one", len(ids), sortedStrings(keysOf(ids)))
		}
	}
	for id, n := range seen {
		if n != 1 {
			t.Errorf("unit %q appears %d times in the book after a store bounce; the replay must be absorbed by its write_id", id, n)
		}
	}
	for _, want := range []string{capturedThinking1, capturedBashCall1, capturedThinking2, capturedBashCall2} {
		if seen[want] == 0 {
			t.Errorf("unit %q is missing after a store bounce; the book holds %v", want, sortedStrings(keysOf(toSet(seen))))
		}
	}
}

// TestAStoreBounceLeavesOneCursorRowAtTheFilesFullLength asserts the OTHER half
// of "no gap": the recovered store ends holding a single cursor row for the
// file, at the length the vendor actually wrote — so nothing between the outage
// and the recovery was skipped, and the file was not re-keyed as a second one.
func TestAStoreBounceLeavesOneCursorRowAtTheFilesFullLength(t *testing.T) {
	t.Parallel()
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	socket := shortSocketPath(t, "bounce-cursor")
	store := startFakeStoreAt(t, socket)
	tree := newVendorTree(t)
	captured := loadCapturedSession(t)
	opts := defaultSidecarOptions(t, socket, tree)
	cut := 9

	// Act.
	startSidecar(t, opts)
	g := newGrowingFile(t, tree.sessionPath(captured.Slug, captured.Session))
	for _, line := range captured.Lines[:cut] {
		g.AppendLine(line)
	}
	awaitCursorAtLeast(ctx, t, store.Client, g.Path(), 1)
	store.Stop()

	for _, line := range captured.Lines[cut:] {
		g.AppendLine(line)
	}
	store.Resume()
	awaitCursorAtLeast(ctx, t, store.Client, g.Path(), g.Offset())

	// Assert.
	rows := cursorsForPath(ctx, t, store.Client, g.Path())
	if len(rows) != 1 {
		t.Fatalf("the store holds %d cursor rows for one file after a bounce: %v", len(rows), describeCursors(rows))
	}
	if got := rows[0].GetOffset(); got != g.Offset() {
		t.Errorf("the cursor stands at %d after the bounce, wanted the file's full length %d", got, g.Offset())
	}
}
