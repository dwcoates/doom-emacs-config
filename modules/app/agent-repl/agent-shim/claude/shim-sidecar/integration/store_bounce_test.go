package integration

import (
	"path/filepath"
	"testing"
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
// vendor keeps writing, starts it again over the same database, and asserts the
// book holds every unit of the file exactly once.
func TestAStoreBounceMidIngestLeavesNoGapAndNoRepeat(t *testing.T) {
	// Arrange: a store whose socket and database outlive the process holding
	// them, so the second one is genuinely the same store.
	ctx, cancel := testContext(t)
	defer cancel()
	socket := shortSocketPath(t, "bounce")
	dbPath := filepath.Join(t.TempDir(), "store.db")
	store := startRealStoreAt(t, socket, dbPath)
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
	recovered := startRealStoreAt(t, socket, dbPath)
	lines := awaitBookUnits(ctx, t, recovered.Client, captured.Session,
		capturedThinking1, capturedBashCall1, capturedThinking2, capturedBashCall2)

	// Assert: every unit is present, and none of them twice.
	seen := map[string]int{}
	for _, at := range lines {
		if a := activityOf(at.GetLine()); a != nil {
			seen[a.GetActivityId().GetValue()]++
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
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	socket := shortSocketPath(t, "bounce-cursor")
	dbPath := filepath.Join(t.TempDir(), "store.db")
	store := startRealStoreAt(t, socket, dbPath)
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
	recovered := startRealStoreAt(t, socket, dbPath)
	awaitCursorAtLeast(ctx, t, recovered.Client, g.Path(), g.Offset())

	// Assert.
	rows := cursorsForPath(ctx, t, recovered.Client, g.Path())
	if len(rows) != 1 {
		t.Fatalf("the store holds %d cursor rows for one file after a bounce: %v", len(rows), describeCursors(rows))
	}
	if got := rows[0].GetOffset(); got != g.Offset() {
		t.Errorf("the cursor stands at %d after the bounce, wanted the file's full length %d", got, g.Offset())
	}
}
