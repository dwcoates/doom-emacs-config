package integration

import (
	"testing"

	"agentrepl/shim-claude-sidecar/internal/testclose"
)

// exitMarker is the vendor's own spool terminator: a line-start,
// newline-terminated `EXIT=<code>` as the last line of a batch.
const exitMarker = "EXIT=0\n"

// CRITIQUE 4 — a detached run read back over ONE WatchBashRun stream.
//
// These drive the FAKE store's WatchBashRun (helpers_test.go), never the real
// store: this suite runs the sidecar alone and every peer is a mock (AGENTS.md,
// "A system's integration tests run that system alone"). The fake serves the
// endpoint's contract -- the stored rows replayed first, then the rows that
// land afterwards followed live, the terminal LAST, and the stream ending after
// it -- so what these pin is that the rows the SIDECAR writes (its one
// superseded `bash:<run>:tail` row, then `bash:<run>:terminal` under the
// spawning call's identity) read back whole and in order through it. The real
// endpoint's own side of the contract is shim-store's
// integration/bashwatch_test.go.

// TestABashRunReplaysThenFollowsOnOneStream opens the stream after some output
// is already durable and appends the rest while it is open, so both phases run
// on one stream and the boundary between them is crossed under the subject's
// control.
func TestABashRunReplaysThenFollowsOnOneStream(t *testing.T) {
	t.Parallel()
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	store := startFakeStore(t)
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
