package integration

import (
	"strings"
	"testing"
)

// SUBJECT — the `EXIT=<code>` terminator arriving across TWO reads.
//
// A shell spool is raw bytes framed by RawTextCodec, which carries NOTHING: a
// batch may begin mid-line, so the marker parser cannot trust a marker it finds
// at the start of a mid-file batch. `trailingExitCode` therefore requires the
// marker to be the LAST thing in the batch, newline-terminated, and to begin a
// line — which it does when the PREVIOUS batch ended on a newline.
//
// The two halves of that rule are the two subjects here, and they pull in
// opposite directions: a marker cut IN THE MIDDLE is deliberately not matched
// (the run is left to the staleness policy, exactly as the ~91% of spools with
// no marker at all are), while a marker arriving as its OWN read after a
// newline-terminated batch IS matched and ends the run exactly once.

// TestASpoolMarkerOnItsOwnReadEndsTheRunExactlyOnce writes the run's output and
// its terminator in SEPARATE appends — the ordinary shape for a command that
// finishes between two polls — and asserts one terminal row.
func TestASpoolMarkerOnItsOwnReadEndsTheRunExactlyOnce(t *testing.T) {
	t.Parallel()
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	fx := seedDetachedShell(t, tree, "/Users/dodgecoates/spool-marker-own-read-probe",
		"1a1a1a1a-1a1a-41a1-81a1-1a1a1a1a1a1a")

	// Act.
	startSidecar(t, defaultSidecarOptions(t, fake.Socket, tree))
	awaitCursorInBatches(ctx, t, fake, fx.Parent.Path(), fx.Parent.Offset())
	spool := newGrowingFile(t, fx.SpoolPath)
	spool.AppendRaw([]byte("some output\n"))
	awaitCursorInBatches(ctx, t, fake, fx.SpoolPath, spool.Offset())
	spool.AppendRaw([]byte("EXIT=0\n"))
	awaitCursorInBatches(ctx, t, fake, fx.SpoolPath, spool.Offset())

	// Assert.
	rows := awaitBashRunTerminal(ctx, t, storeClient(fake.Socket), fx.CallID)
	requireBashReplayOrder(t, fx.CallID, rows)
	if n := countUpsertKey(fake.Entries(), "bash:"+fx.CallID+":terminal"); n != 1 {
		t.Errorf("the run wrote %d terminal rows, wanted exactly one; a run has one terminal however often it is restated", n)
	}
	if got := rows[len(rows)-1].GetSuccess().GetCompleted().GetTermination().GetExited(); got == nil {
		t.Fatalf("the run did not settle on its marker: %v", describeBashRows(rows))
	}
}

// TestASpoolMarkerCutMidTokenIsNotMatched asserts the strictness half: a marker
// whose own bytes are split across two reads is NOT a marker.
//
// IT IS DELIBERATE, NOT A GAP. The batch that holds `EXI` does not end on a
// newline, and the batch that holds `T=0\n` does not begin a line, so neither
// can be trusted to be the harness's terminator rather than the tail of a line
// of ordinary output. Ending the run on it would be a completion nobody
// observed; the staleness policy owns the outcome instead. The bytes themselves
// are never lost — they reach the consumer as ordinary deltas.
func TestASpoolMarkerCutMidTokenIsNotMatched(t *testing.T) {
	t.Parallel()
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	fx := seedDetachedShell(t, tree, "/Users/dodgecoates/spool-marker-cut-probe",
		"1b1b1b1b-1b1b-41b1-81b1-1b1b1b1b1b1b")

	// Act.
	startSidecar(t, defaultSidecarOptions(t, fake.Socket, tree))
	awaitCursorInBatches(ctx, t, fake, fx.Parent.Path(), fx.Parent.Offset())
	spool := newGrowingFile(t, fx.SpoolPath)
	spool.AppendRaw([]byte("some output\nEXI"))
	awaitCursorInBatches(ctx, t, fake, fx.SpoolPath, spool.Offset())
	spool.AppendRaw([]byte("T=0\n"))
	awaitCursorInBatches(ctx, t, fake, fx.SpoolPath, spool.Offset())

	// Assert.
	entries := fake.Entries()
	if n := countUpsertKey(entries, "bash:"+fx.CallID+":terminal"); n != 0 {
		t.Errorf("a marker cut mid-token ended the run (%d terminal rows); a split marker is not evidence of completion", n)
	}
	if got := requireLatestTail(t, fx.CallID, bashFramesForRun(entries, fx.CallID)); !strings.Contains(got, "some output\nEXIT=0\n") {
		t.Errorf("the split bytes reached the consumer as %q; nothing may be dropped just because it was not a marker", got)
	}
}
