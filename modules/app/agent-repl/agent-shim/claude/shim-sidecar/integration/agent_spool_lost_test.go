package integration

import (
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	storev1 "agentrepl/proto/store/v1"
)

// SUBJECT — a BACKGROUNDED SUBAGENT is a detached run too, and the LOST policy
// owes it a terminal on the wire.
//
// A backgrounded agent's transcript arrives through an `a*` task spool. When
// that spool stops growing past `--stale-agent-silence` the reader concludes
// LOST exactly as it does for a shell spool — but the unit left open downstream
// is not a bash row: it is the SPAWNING CALL's unit, `activity:<tool_use_id>`, a
// line in the PARENT's book. Only the a* spool's own converter can spell that
// settle, and until it could the reader wrote `lost-terminal-unsupported` and
// the spawn drew as still running in every consumer for the life of the store.

// TestASilentAgentSpoolSettlesItsSpawnUnitLostOnTheWire asserts the whole path:
// the conclusion is reached under the AGENT silence window, and it reaches the
// wire as AgentSubagentFailure.cause.lost naming `went_silent`.
func TestASilentAgentSpoolSettlesItsSpawnUnitLostOnTheWire(t *testing.T) {
	t.Parallel()
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	session := "e1e1e1e1-e1e1-4e1e-8e1e-e1e1e1e1e1e1"
	_, spoolPath, parent := seedBackgroundedAgent(t, tree, "/work/agent-spool-lost-probe", session)
	opts := lostOptions(t, fake.Socket, tree)
	opts.StaleAgentSilence = shortSilence

	// Act: the spawn is observed, the subagent's transcript arrives through its
	// spool, and then the spool says nothing more.
	startSidecar(t, opts)
	awaitCursorInBatches(ctx, t, fake, parent.Path(), parent.Offset())
	spool := newGrowingFile(t, spoolPath)
	for _, line := range corpusLines(t, "sidechain/agent-"+corpusSubagentID+".jsonl") {
		spool.AppendLine(line)
	}
	awaitCursorInBatches(ctx, t, fake, spoolPath, spool.Offset())

	// Assert: the spawn unit settles, on the failure arm, naming the arm the
	// reader concluded with.
	settle := fake.awaitEntry(ctx, t, "the LOST settle of the spawn unit", func(e *storev1.StoreEntry) bool {
		return e.GetUpsertKey() == "activity:"+capturedBashCall1 &&
			subagentFailureOf(e).GetLost() != nil
	})
	failure := subagentFailureOf(settle)
	if failure.GetStoppedByUser() != nil {
		t.Errorf("the LOST settle blames a person; we only stopped seeing the subagent's transcript")
	}
	if got := lostArmName(failure.GetLost()); got != "went_silent" {
		t.Errorf("the LOST settle names the arm %q, wanted went_silent", got)
	}
	if got := settle.GetAgentUpdate().GetServeableFrame().GetPageAgentId().GetValue(); got != session {
		t.Errorf("the LOST settle is a line in book %q, wanted the spawning agent's book %q", got, session)
	}
}

// subagentFailureOf reads the spawn unit's failure off an entry, or nil when the
// entry is not one.
func subagentFailureOf(e *storev1.StoreEntry) *conversationv1.AgentSubagentFailure {
	return e.GetAgentUpdate().GetServeableFrame().GetAgentItem().GetAgentFrame().
		GetUpdate().GetActivity().GetSubagent().GetFailure()
}
