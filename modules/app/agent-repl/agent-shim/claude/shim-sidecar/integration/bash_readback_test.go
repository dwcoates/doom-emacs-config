package integration

import (
	"testing"

	"agentrepl/shim-claude-sidecar/internal/testclose"
)

// exitMarker is the vendor's own spool terminator: a line-start,
// newline-terminated `EXIT=<code>` as the last line of a batch.
const exitMarker = "EXIT=0\n"

// CRITIQUE 4 — a detached run read back through the REAL store's WatchBashRun.
//
// Every other bash subject reads the fake store's own re-implementation of the
// endpoint, which proves what the SIDECAR wrote and nothing about what a
// consumer can get out of the store. These two drive the real endpoint: the
// sidecar supersedes its one `bash:<run>:tail` row and finally writes
// `bash:<run>:terminal` under the spawning call's identity, and the store must hand them back over ONE
// stream — the stored rows replayed first, then the rows that land afterwards
// followed live, the terminal LAST, and the stream ending after it.

// TestABashRunReplaysThenFollowsOnOneStream opens the stream after some output
// is already durable and appends the rest while it is open, so both phases run
// on one stream and the boundary between them is crossed under the subject's
// control.
func TestABashRunReplaysThenFollowsOnOneStream(t *testing.T) {
	t.Parallel()
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	store := startRealStore(t)
	tree := newVendorTree(t)
	fx := seedDetachedShell(t, tree, "/work/bash-readback-probe",
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
	defer testclose.OrFail(t, stream)

	// ...and the rest of the run appended while it follows.
	spool.AppendRaw([]byte(after))
	spool.AppendRaw([]byte(exitMarker))
	rows := drainBashRunToTerminal(t, fx.CallID, stream, first)

	// Assert: the endpoint's ordering contract across the boundary, contiguous
	// offsets throughout, and every byte of both phases in order.
	requireBashReplayOrder(t, fx.CallID, rows)
	joined := requireLatestTail(t, fx.CallID, rows)
	// EXACT EQUALITY, NOT CONTAINMENT. The three containment-and-index checks
	// this replaced were all satisfied by a read-back that had also DUPLICATED a
	// phase, dropped a newline, or interleaved bytes the run never wrote; the
	// only statement worth making about a concatenated stream is that it IS the
	// concatenation.
	//
	// The `EXIT=0` line is part of it. The marker settles the run's terminal, and
	// the raw codec carries every byte of the spool through as output as well —
	// AGENTS.md pins what the marker MEANS, not that it is withheld — so a
	// consumer replaying this run sees the spool's bytes whole.
	if want := before + after + exitMarker; joined != want {
		t.Errorf("the run read back as %q, wanted exactly the spool's bytes in order, %q", joined, want)
	}
}

// TestABashRunsStreamEndsAfterItsTerminalRow asserts the stream ENDS once the
// run is settled: the terminal is the last row and the endpoint closes rather
// than holding a finished run's consumer open forever.
func TestABashRunsStreamEndsAfterItsTerminalRow(t *testing.T) {
	t.Parallel()
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	store := startRealStore(t)
	tree := newVendorTree(t)
	fx := seedDetachedShell(t, tree, "/work/bash-terminal-probe",
		"c2c2c2c2-c2c2-4c2c-8c2c-c2c2c2c2c2c2")

	// Act: a run that is finished on disk before anyone reads it.
	startSidecar(t, defaultSidecarOptions(t, store.Socket, tree))
	awaitCursorAtLeast(ctx, t, store.Client, fx.Parent.Path(), 1)
	spool := newGrowingFile(t, fx.SpoolPath)
	spool.AppendRaw([]byte("all of it at once\nEXIT=0\n"))
	awaitCursorAtLeast(ctx, t, store.Client, fx.SpoolPath, spool.Offset())

	stream, first := awaitBashRunStream(ctx, t, store.Client, fx.CallID)
	defer testclose.OrFail(t, stream)
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
