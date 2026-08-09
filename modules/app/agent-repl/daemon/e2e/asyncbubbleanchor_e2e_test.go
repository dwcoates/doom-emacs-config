// A detached launch's ANCHOR in the feed: the bubble opens as a top-level
// ConversationItem on arm 38, addressed to the same id the spawning call
// published and pointed back at that call.
//
// The anchor is what lets a frontend draw the bubble attached to its
// originating card rather than free-standing (async-bubble.proto
// AsyncBubble.origin_tool_use_id). Without it the bubble exists but has no
// place in the conversation, and the reader cannot tell which call started it.
package e2e

import (
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// TestE2EALaunchAnchorsItsBubbleInTheFeedWithLiveLiveness covers the OPEN edge:
// the launch's own item.
func TestE2EALaunchAnchorsItsBubbleInTheFeedWithLiveLiveness(t *testing.T) {
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
		agentAsyncLaunchOutcome(agentID, "anchor the bubble"))))
	store.write(sidecarUserLineEvent(t, vendorID, "e2e-anchor-barrier-line", barrierPrompt))

	// Assert
	seen := drainUntilItem(t, conn, cwd, "the barrier prompt's user item", func(it *frontendv1.ConversationItem) bool {
		return it.GetUserMessage().GetContentString() == barrierPrompt
	})

	// gateOnAnchor IS this test's subject, not merely its gate: it asserts that
	// exactly one anchor names the launching call and that the id it carries is
	// non-empty — the bubble's existence in the feed and its addressability.
	// What remains below is the one fact the lookup itself cannot establish.
	bubbleID := gateOnAnchor(t, seen, toolUseID)
	// Resolved across BOTH publication sites, so the liveness assertion below
	// still runs when the anchor is missing but the async push opened the bubble
	// — gateOnAnchor has already recorded that gap, and re-reporting it here as
	// a self-contradicting verdict would misdescribe it.
	anchor := openedBubble(asyncBubbleItems(seen.items), bubbleID)
	if anchor == nil {
		anchor = openedBubble(seen.bubbles(), bubbleID)
	}
	if anchor == nil {
		t.Fatalf("bubble id %q was resolved for call %q but no anchor and no opened bubble carries it: the verdict contradicts itself", bubbleID, toolUseID)
	}
	if anchor.GetLiveness().GetLive() == nil {
		t.Errorf("anchored bubble %q opened with liveness %v, want the live arm: a launch that opens already-settled is unrepresentable while its agent is still running",
			bubbleID, anchor.GetLiveness().GetState())
	}
}
