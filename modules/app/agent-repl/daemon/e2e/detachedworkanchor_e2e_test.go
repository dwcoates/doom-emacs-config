// A detached launch's ANCHOR in the feed: the work opens as a top-level
// Message on arm 38, addressed to the same id the spawning call
// published and pointed back at that call.
//
// The anchor is what lets a frontend draw the work attached to its
// originating card rather than free-standing (async-work.proto
// DetachedWork.origin_tool_use_id). Without it the work exists but has no
// place in the conversation, and the reader cannot tell which call started it.
package e2e

import (
	"testing"

	frontendv1 "agentrepl/proto/frontend/v1"
)

// TestE2EALaunchAnchorsItsDetachedWorkInTheFeedWithLiveLiveness covers the OPEN edge:
// the launch's own item.
func TestE2EALaunchAnchorsItsDetachedWorkInTheFeedWithLiveLiveness(t *testing.T) {
	// Arrange
	h := newUDSHarness(t)
	cwd := t.TempDir()
	_, conn, vendorID, store := liveSession(t, h, cwd)
	const (
		toolUseID     = "toolu_e2e_anchor_dispatch"
		agentID       = "agent_e2e_anchor"
		barrierPrompt = "e2e-anchor-barrier: the launch is now fully processed"
	)

	// Act
	store.write(vendorLineEvent(t, vendorID, asyncToolCallLine("e2e-anchor-call", toolUseID, "Task")))
	store.write(vendorLineEvent(t, vendorID, asyncToolResultLine(
		"e2e-anchor-launch", toolUseID, "Launched agent",
		agentAsyncLaunchOutcome(agentID, "anchor the work"))))
	store.write(sidecarUserLineEvent(t, vendorID, "e2e-anchor-barrier-line", barrierPrompt))

	// Assert
	seen := drainUntilItem(t, conn, cwd, "the barrier prompt's user item", func(it *frontendv1.Message) bool {
		return it.GetUserMessage().GetContentString() == barrierPrompt
	})

	// gateOnOpenedWork IS this test's subject, not merely its gate: it asserts that
	// exactly one anchor names the launching call and that the id it carries is
	// non-empty — the work's existence in the feed and its addressability.
	// What remains below is the one fact the lookup itself cannot establish.
	messageID := gateOnOpenedWork(t, seen, toolUseID)
	// Resolved across BOTH publication sites, so the liveness assertion below
	// still runs when the anchor is missing but the async push opened the work
	// — gateOnOpenedWork has already recorded that gap, and re-reporting it here as
	// a self-contradicting verdict would misdescribe it.
	anchor := openedDetachedWork(detachedWorkItems(seen.items), messageID)
	if anchor == nil {
		anchor = openedDetachedWork(seen.work(), messageID)
	}
	if anchor == nil {
		t.Fatalf("work id %q was resolved for call %q but no anchor and no opened work carries it: the verdict contradicts itself", messageID, toolUseID)
	}
	if anchor.GetDetachedWork().GetLiveness().GetLive() == nil {
		t.Errorf("anchored work %q opened with liveness %v, want the live arm: a launch that opens already-settled is unrepresentable while its agent is still running",
			messageID, anchor.GetDetachedWork().GetLiveness().GetState())
	}
}
