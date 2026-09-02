package integration

import (
	"testing"
)

// SUBJECT — a keep-alive turn that was IN PROGRESS when the process died.
//
// THE KEEP-ALIVE BIT IS IN MEMORY, one remembered bool per file, and a restart
// loses it. What is supposed to restore it is the BOOT REWIND: the restored
// cursor moves back to the first record of the in-progress turn, which for a
// keep-alive turn is the marked prompt itself, so re-reading it re-warms the bit
// before any of the turn's later work is converted again.
//
// WITHOUT THAT, EVERY RECORD WRITTEN AFTER THE RESTART IS SERVED. A keep-alive
// turn is machinery the user never asked for, so its work appearing in the feed
// is not a cosmetic slip: it is the one outcome the whole marker exists to
// prevent, and it would appear only after a crash — the case nobody is watching.
//
// The store is REAL so the cursor genuinely survives the restart, and a proxy in
// front of it records the entries both runs wrote, which is the only way to see
// which arm each record landed on.

// TestAKeepAliveTurnInProgressAtRestartStaysWithheld stops the sidecar mid
// keep-alive turn, appends the rest of the turn's assistant work, restarts, and
// asserts every record of the turn is still an unserved keep-alive item.
func TestAKeepAliveTurnInProgressAtRestartStaysWithheld(t *testing.T) {
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	store := startRealStore(t)
	proxy := startProxyStore(t, store.Socket)
	tree := newVendorTree(t)
	captured := loadCapturedSession(t)
	cwd := "/Users/dodgecoates/keepalive-restart-probe"
	slug := cwdSlug(cwd)
	session := "f4f4f4f4-f4f4-4f4f-8f4f-f4f4f4f4f4f4"
	opts := defaultSidecarOptions(t, proxy.Socket, tree)
	prompt := setUserText(t,
		retargetSession(t, decodeRecord(t, captured.Lines[3]), session, cwd),
		keepaliveMarker+"cache ping")

	// Act: the marked prompt and the first of the turn's work, then the process
	// dies mid-turn...
	first := startSidecar(t, opts)
	g := newGrowingFile(t, tree.sessionPath(slug, session))
	g.AppendLine(encodeRecord(t, prompt))
	g.AppendLine(encodeRecord(t, retargetSession(t, decodeRecord(t, captured.Lines[7]), session, cwd)))
	awaitCursorAtLeast(ctx, t, store.Client, g.Path(), g.Offset())
	first.Stop()

	// ...the rest of the same turn is written while nothing is reading...
	for _, i := range []int{8, 12, 13} {
		g.AppendLine(encodeRecord(t, retargetSession(t, decodeRecord(t, captured.Lines[i]), session, cwd)))
	}

	// ...and a fresh process picks the file up from the store's cursor.
	startSidecar(t, opts)
	awaitCursorAtLeast(ctx, t, store.Client, g.Path(), g.Offset())

	// Assert: the turn's work is on the keepalive arm, and none of it is a page
	// line of anybody's book.
	entries := entriesOf(proxy.Batches())
	if len(keepalivesOf(entries)) == 0 {
		t.Fatalf("nothing landed on the keepalive arm across either run, so the bit was never set at all")
	}
	for _, line := range pageLinesOf(entries) {
		if a := activityOf(line); a != nil {
			t.Errorf("unit %q reached a page line of book %q after a restart inside a keep-alive turn; the boot rewind must re-warm the bit before the turn's work is re-converted",
				a.GetActivityId().GetValue(), line.GetPageAgentId().GetValue())
		}
	}
}
