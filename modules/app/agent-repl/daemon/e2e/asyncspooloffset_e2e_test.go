// A shell bubble's spool CURSOR: appends are contiguous, and the reconnect
// snapshot's through_offset is exactly where they left off.
//
// async-bubble.proto makes the cursor the gap detector, twice over.
// AsyncOutputAppend.from_offset "MUST equal the bubble's current
// AsyncOutputSpool.through_offset; anything else is a gap ... Carried
// explicitly so a gap is detectable at all — a bare append cannot tell a lost
// chunk from a quiet one." And AsyncOutputSpool.through_offset is "Bytes
// delivered so far, so a reconnecting client resumes rather than re-fetches."
//
// Those are one guarantee viewed from the live stream and from a reconnect, so
// this test checks them against each other rather than each against a constant:
// a daemon that miscounts consistently in both places would satisfy either half
// alone.
package e2e

import (
	"testing"

	datav1 "agentrepl/proto/agentshim/data/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// TestE2EAShellBubblesAppendsAreContiguousThroughTheSnapshotCursor covers the
// OFFSET CONTINUITY edge.
func TestE2EAShellBubblesAppendsAreContiguousThroughTheSnapshotCursor(t *testing.T) {
	// Arrange
	h := newUDSHarness(t)
	cwd := t.TempDir()
	id, conn, vendorID, store := liveSession(t, h, cwd)
	const (
		launchToolUseID = "toolu_e2e_offset_bash"
		taskID          = "task_e2e_offset_shell"
		command         = "tail -f build.log"
		// THREE retrievals of GROWING output, which is how the spool actually
		// arrives: each retrieval reports everything so far, and the daemon owes
		// the client only the new bytes at the right offset. Chunks of unequal
		// length so an off-by-one in the cursor cannot cancel out.
		first  = "compiling\n"
		second = "compiling\nlinking the whole thing\n"
		third  = "compiling\nlinking the whole thing\ndone\n"

		barrierPrompt = "e2e-offset-barrier: every retrieval is now fully processed"
	)

	// Act
	store.write(vendorLineEvent(t, vendorID, asyncToolCallLine("e2e-offset-call", launchToolUseID, "Bash")))
	store.write(vendorLineEvent(t, vendorID, asyncToolResultLine(
		"e2e-offset-launch", launchToolUseID, "Command running in background",
		bashBackgroundOutcome(taskID))))
	for _, retrieval := range []struct {
		uuid   string
		tool   string
		output string
	}{
		{"e2e-offset-poll-1", "toolu_e2e_offset_poll_1", first},
		{"e2e-offset-poll-2", "toolu_e2e_offset_poll_2", second},
		{"e2e-offset-poll-3", "toolu_e2e_offset_poll_3", third},
	} {
		store.write(vendorLineEvent(t, vendorID, asyncToolResultLine(
			retrieval.uuid, retrieval.tool, "task output",
			bashTaskOutcome(taskID, command, retrieval.output, datav1.RawTaskStatus_RAW_TASK_STATUS_RUNNING, 0, false))))
	}
	store.write(sidecarUserLineEvent(t, vendorID, "e2e-offset-barrier-line", barrierPrompt))

	// Assert
	seen := drainUntilItem(t, conn, cwd, "the barrier prompt's user item", func(it *frontendv1.ConversationItem) bool {
		return it.GetUserMessage().GetContentString() == barrierPrompt
	})

	bubbleID := gateOnAnchor(t, seen, launchToolUseID)

	appends := shellAppends(seen, bubbleID)
	if len(appends) == 0 {
		t.Fatalf("no shell appends arrived for bubble %q (saw %d updates for it): the spool never reached the client at all", bubbleID, len(seen.updatesFor(bubbleID)))
	}
	var cursor uint64
	for i, app := range appends {
		if got := app.GetFromOffset(); got != cursor {
			t.Fatalf("append %d for bubble %q starts at from_offset %d, want the spool's current through_offset %d: the proto states anything else is a GAP and must be rejected loudly rather than applied",
				i, bubbleID, got, cursor)
		}
		cursor += uint64(len(app.GetText()))
	}

	// The RECONNECT half: a fresh client's snapshot must resume exactly where
	// the live appends left off.
	fresh := h.dial(t, id)
	snapshot := readFrame(t, fresh).GetSnapshot()
	if snapshot == nil {
		t.Fatal("the reconnecting client's first frame was not a StateSnapshot")
	}
	bubble := openedBubble(snapshot.GetAsyncBubbles(), bubbleID)
	if bubble == nil {
		t.Fatalf("the reconnect StateSnapshot carries no bubble %q among its %d async_bubbles: a reconnecting client would restart a running shell's fold from nothing",
			bubbleID, len(snapshot.GetAsyncBubbles()))
	}
	spool := bubble.GetShell().GetOutput()
	if spool == nil {
		t.Fatalf("the snapshot's bubble %q carries no shell output spool: there is no cursor for a reconnecting client to resume from", bubbleID)
	}
	if got := spool.GetThroughOffset(); got != cursor {
		t.Errorf("the snapshot spool's through_offset = %d, want %d — the byte total the live appends delivered: a reconnecting client would resume at the wrong offset and reject every subsequent append as a gap",
			got, cursor)
	}
}

// shellAppends returns the shell-arm appends addressed to bubbleID, in arrival
// order. The unclassified arm is deliberately NOT folded in here: it carries
// the same message, but a shell bubble receiving its output on the
// unclassified arm is a kind/arm mismatch the proto states is a daemon bug and
// is rejected rather than coerced.
func shellAppends(seen asyncTraffic, bubbleID string) []*frontendv1.AsyncOutputAppend {
	var out []*frontendv1.AsyncOutputAppend
	for _, update := range seen.updatesFor(bubbleID) {
		if app := update.GetShell(); app != nil {
			out = append(out, app)
		}
	}
	return out
}
