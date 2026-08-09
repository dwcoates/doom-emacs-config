// A bubble's SETTLE, typed.
//
// async-bubble.proto states the invariant in the message itself: "Live-or-
// settled, expressed as arms so that 'settled' and 'settled with what outcome'
// are one indivisible fact. A settled bubble with no outcome is
// unrepresentable." And for work that IS a process, AsyncSettled.shell_exit
// carries the exit status BESIDE the outcome, so a shell's card can show
// "exited 137" rather than an unexplained red dot.
//
// A settled bubble arriving with neither arm set is the shape this test exists
// to make impossible: it renders as a stopped spinner with nothing to say.
package e2e

import (
	"testing"

	datav1 "agentrepl/proto/agentshim/data/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// TestE2EASettledShellBubbleCarriesItsOutcomeAndExitStatus covers the SETTLE
// edge for the process-shaped kind.
func TestE2EASettledShellBubbleCarriesItsOutcomeAndExitStatus(t *testing.T) {
	// Arrange
	h := newUDSHarness(t)
	cwd := t.TempDir()
	_, conn, vendorID, store := liveSession(t, h, cwd)
	const (
		launchToolUseID = "toolu_e2e_settle_bash"
		pollToolUseID   = "toolu_e2e_settle_poll"
		taskID          = "task_e2e_settle_shell"
		command         = "sleep 1 && echo done"
		// A NONZERO exit, deliberately. Zero would let a daemon that always
		// resolves `done` pass, and 0 is also the proto3 default for the exit
		// code — so the assertion could not distinguish a resolved status from an
		// unset one.
		exitCode      = int32(3)
		barrierPrompt = "e2e-settle-barrier: the shell's ending is now fully processed"
	)

	// Act — the backgrounded launch, then the retrieval that reports it finished.
	store.write(vendorLineEvent(t, vendorID, asyncToolCallLine("e2e-settle-call", launchToolUseID, "Bash")))
	store.write(vendorLineEvent(t, vendorID, asyncToolResultLine(
		"e2e-settle-launch", launchToolUseID, "Command running in background",
		bashBackgroundOutcome(taskID))))
	store.write(vendorLineEvent(t, vendorID, asyncToolResultLine(
		"e2e-settle-final", pollToolUseID, "task finished",
		bashTaskOutcome(taskID, command, "done\n", datav1.RawTaskStatus_RAW_TASK_STATUS_FAILED, exitCode, true))))
	store.write(sidecarUserLineEvent(t, vendorID, "e2e-settle-barrier-line", barrierPrompt))

	// Assert
	seen := drainUntilItem(t, conn, cwd, "the barrier prompt's user item", func(it *frontendv1.ConversationItem) bool {
		return it.GetUserMessage().GetContentString() == barrierPrompt
	})

	call := spawningCall(seen.items, launchToolUseID)
	if call == nil {
		t.Fatalf("no top-level tool_call item for the backgrounding call %q arrived: the daemon never published its classification verdict", launchToolUseID)
	}
	bubbleID := call.GetSpawnedBubbleId()
	if bubbleID == "" {
		t.Fatalf("the backgrounding call %q carries an empty spawned_bubble_id: a Bash whose BashResult.background_task_id is set detached work and must have a bubble", launchToolUseID)
	}

	settled := lastSettledLiveness(seen, bubbleID)
	if settled == nil {
		t.Fatalf("bubble %q never settled: no liveness update carrying the settled arm arrived (saw %d updates for it)", bubbleID, len(seen.updatesFor(bubbleID)))
	}
	if settled.GetOutcome() == nil {
		t.Errorf("bubble %q settled with NO outcome arm set: async-bubble.proto states exactly one arm is always set, and a settled bubble with no outcome is unrepresentable", bubbleID)
	}
	if settled.GetShellExit() == nil {
		t.Fatalf("bubble %q settled without shell_exit: it is a process, and the proto states absence is the only reading of 'this work did not exit, it concluded'", bubbleID)
	}
	if got := settled.GetShellExit().GetCode(); got != exitCode {
		t.Errorf("bubble %q settled with shell_exit.code = %d, want the process's own %d", bubbleID, got, exitCode)
	}
	if settled.GetError() == nil {
		t.Errorf("bubble %q settled on outcome %T for a nonzero exit, want the error arm: the proto states the daemon resolves the outcome FROM the exit code, and that mapping is not a client's to make",
			bubbleID, settled.GetOutcome())
	}
}

// lastSettledLiveness returns the settled liveness of the LAST liveness update
// addressed to bubbleID, or nil when none settled it.
//
// The last rather than the first: the proto admits a settled outcome CHANGING
// (a running agent that is then killed), so the bubble's ending is whatever the
// most recent transition says it is.
func lastSettledLiveness(seen asyncTraffic, bubbleID string) *frontendv1.AsyncSettled {
	var settled *frontendv1.AsyncSettled
	for _, update := range seen.updatesFor(bubbleID) {
		if s := update.GetLiveness().GetLiveness().GetSettled(); s != nil {
			settled = s
		}
	}
	return settled
}
