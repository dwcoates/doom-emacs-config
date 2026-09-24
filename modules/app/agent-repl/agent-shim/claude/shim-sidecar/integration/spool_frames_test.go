package integration

import (
	"testing"
)

// SUBJECTS — what an a* spool's FRAMES say, and what a spool with no terminator
// must not be settled by.
//
// spool_routing_test.go proves an a* spool is ROUTED as a transcript into the
// spawning call's book. Routing is only half of the attribution: every frame
// also names a `top_level`, and for a BACKGROUNDED spawn that value is the
// subagent ITSELF rather than the owning session's main agent — its stream
// outlives the turn, so a consumer that drew it under the parent's turn would
// have nowhere to put anything it says afterwards.

// TestABackgroundedAgentSpoolsFramesNameTheSubagentAsTopLevel asserts that
// every page line an a* spool produces names the backgrounded subagent as its
// top level, not the session's main agent.
func TestABackgroundedAgentSpoolsFramesNameTheSubagentAsTopLevel(t *testing.T) {
	t.Parallel()
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	session := "a5a5a5a5-a5a5-4a5a-8a5a-a5a5a5a5a5a5"
	_, spoolPath, parent := seedBackgroundedAgent(t, tree, "/Users/dodgecoates/agent-spool-toplevel-probe", session)

	// Act.
	startSidecar(t, defaultSidecarOptions(t, fake.Socket, tree))
	awaitCursorInBatches(ctx, t, fake, parent.Path(), parent.Offset())
	spool := newGrowingFile(t, spoolPath)
	for _, line := range corpusLines(t, "sidechain/agent-"+corpusSubagentID+".jsonl") {
		spool.AppendLine(line)
	}
	awaitCursorInBatches(ctx, t, fake, spoolPath, spool.Offset())

	// Assert: the subagent is its own top level on EVERY line of its book.
	var checked int
	for _, e := range fake.Entries() {
		up := e.GetAgentUpdate()
		if up.GetServeableFrame().GetPageAgentId().GetValue() != capturedBashCall1 {
			continue
		}
		checked++
		if got := up.GetTopLevel().GetValue(); got != capturedBashCall1 {
			t.Errorf("a backgrounded subagent's frame %q names top_level %q, wanted the subagent itself %q; its stream outlives the spawning turn",
				e.GetUpsertKey(), got, capturedBashCall1)
		}
	}
	if checked == 0 {
		t.Fatalf("no a* spool page line was written, so top_level was never stated")
	}
}

// TestTheMidOutputCorpusSpoolIsNeverSettled drives the REAL mid-output spool
// capture — a still-writing shell spool, truncated mid-line — and asserts the
// run is left open.
//
// NAMING NOTE. The subject was commissioned as "not settled by its MID-LINE
// marker". The fixture carries no `EXIT=` token at all (grep it: zero
// occurrences), so there is no mid-line marker to be misread here; what it
// actually proves is the broader rule the marker strictness exists to serve —
// a spool that never states a terminator settles NOTHING, and its bytes reach
// the consumer whole regardless. The split-token case has its own subject in
// spool_exit_marker_test.go, where the marker is constructed rather than
// captured.
func TestTheMidOutputCorpusSpoolIsNeverSettled(t *testing.T) {
	t.Parallel()
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	fx := seedDetachedShell(t, tree, "/Users/dodgecoates/spool-midoutput-probe",
		"1c1c1c1c-1c1c-41c1-81c1-1c1c1c1c1c1c")
	payload := corpusBytes(t, "spools/bash-midoutput.output")

	// Act.
	startSidecar(t, defaultSidecarOptions(t, fake.Socket, tree))
	awaitCursorInBatches(ctx, t, fake, fx.Parent.Path(), fx.Parent.Offset())
	spool := newGrowingFile(t, fx.SpoolPath)
	spool.AppendRaw(payload)
	awaitCursorInBatches(ctx, t, fake, fx.SpoolPath, spool.Offset())

	// Assert: nothing settled the run...
	entries := fake.Entries()
	if n := countUpsertKey(entries, "bash:"+fx.CallID+":terminal"); n != 0 {
		t.Errorf("the mid-output spool wrote %d terminal row(s); a spool that states no terminator settles nothing", n)
	}
	// ...and every byte of it reached the consumer as deltas, in order.
	joined := requireLatestTail(t, fx.CallID, bashFramesForRun(entries, fx.CallID))
	if joined != string(payload) {
		t.Errorf("the spool's deltas joined to %d bytes, wanted the capture's %d verbatim", len(joined), len(payload))
	}
}
