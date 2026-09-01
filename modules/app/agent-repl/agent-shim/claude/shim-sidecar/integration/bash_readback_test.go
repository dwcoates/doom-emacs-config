package integration

import (
	"strings"
	"testing"
)

// CRITIQUE 4 — a detached run read back through the REAL store's WatchBashRun.
//
// Every other bash subject reads the fake store's own re-implementation of the
// endpoint, which proves what the SIDECAR wrote and nothing about what a
// consumer can get out of the store. These two drive the real endpoint: the
// sidecar writes `bash:<run>:<from_offset>` rows and finally `bash:<run>:terminal`
// under the spawning call's identity, and the store must hand them back over ONE
// stream — the stored rows replayed first, then the rows that land afterwards
// followed live, the terminal LAST, and the stream ending after it.

// TestABashRunReplaysThenFollowsOnOneStream opens the stream after some output
// is already durable and appends the rest while it is open, so both phases run
// on one stream and the boundary between them is crossed under the subject's
// control.
func TestABashRunReplaysThenFollowsOnOneStream(t *testing.T) {
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	store := startRealStore(t)
	tree := newVendorTree(t)
	fx := seedDetachedShell(t, tree, "/Users/dodgecoates/bash-readback-probe",
		"c1c1c1c1-c1c1-4c1c-8c1c-c1c1c1c1c1c1")
	before := "output written before the consumer opened its stream\n"
	after := "output written while the stream was already open\n"

	// Act: durable replay bytes first...
	startSidecar(t, defaultSidecarOptions(t, store.Socket, tree))
	awaitCursorAtLeast(ctx, t, store.Client, fx.Parent.Path(), 1)
	spool := newGrowingFile(t, fx.SpoolPath)
	spool.AppendRaw([]byte(before))
	awaitCursorAtLeast(ctx, t, store.Client, fx.SpoolPath, spool.Offset())

	// ...then ONE stream, opened on what the store already holds...
	stream, first := awaitBashRunStream(ctx, t, store.Client, fx.CallID)
	defer stream.Close()

	// ...and the rest of the run appended while it follows.
	spool.AppendRaw([]byte(after))
	spool.AppendRaw([]byte("EXIT=0\n"))
	rows := drainBashRunToTerminal(t, fx.CallID, stream, first)

	// Assert: the endpoint's ordering contract across the boundary, contiguous
	// offsets throughout, and every byte of both phases in order.
	requireBashReplayOrder(t, fx.CallID, rows)
	joined := requireContiguousDeltas(t, fx.CallID, rows)
	if !strings.Contains(joined, strings.TrimSpace(before)) {
		t.Errorf("the replay phase lost the bytes written before the stream opened; the run read back as %q", joined)
	}
	if !strings.Contains(joined, strings.TrimSpace(after)) {
		t.Errorf("the follow phase lost the bytes written while the stream was open; the run read back as %q", joined)
	}
	if strings.Index(joined, strings.TrimSpace(before)) > strings.Index(joined, strings.TrimSpace(after)) {
		t.Errorf("the follow phase's bytes were delivered BEFORE the replay's; the run read back as %q", joined)
	}
}

// TestABashRunsStreamEndsAfterItsTerminalRow asserts the stream ENDS once the
// run is settled: the terminal is the last row and the endpoint closes rather
// than holding a finished run's consumer open forever.
func TestABashRunsStreamEndsAfterItsTerminalRow(t *testing.T) {
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	store := startRealStore(t)
	tree := newVendorTree(t)
	fx := seedDetachedShell(t, tree, "/Users/dodgecoates/bash-terminal-probe",
		"c2c2c2c2-c2c2-4c2c-8c2c-c2c2c2c2c2c2")

	// Act: a run that is finished on disk before anyone reads it.
	startSidecar(t, defaultSidecarOptions(t, store.Socket, tree))
	awaitCursorAtLeast(ctx, t, store.Client, fx.Parent.Path(), 1)
	spool := newGrowingFile(t, fx.SpoolPath)
	spool.AppendRaw([]byte("all of it at once\nEXIT=0\n"))
	awaitCursorAtLeast(ctx, t, store.Client, fx.SpoolPath, spool.Offset())

	stream, first := awaitBashRunStream(ctx, t, store.Client, fx.CallID)
	defer stream.Close()
	rows := drainBashRunToTerminal(t, fx.CallID, stream, first)

	// Assert: the terminal came last, and the stream sent nothing after it.
	if !isTerminalFrame(rows[len(rows)-1]) {
		t.Fatalf("the run did not end on its terminal row: %v", describeBashRows(rows))
	}
	if stream.Receive() {
		t.Errorf("the stream delivered a row AFTER the terminal: %v", stream.Msg().GetRow().GetFrame())
	}
	if err := stream.Err(); err != nil {
		t.Errorf("the stream failed rather than ending after the terminal: %v", err)
	}
}
