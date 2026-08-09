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

	call := spawningCall(seen.items, toolUseID)
	if call == nil {
		t.Fatalf("no top-level tool_call item for the launching call %q arrived: the daemon never published its classification verdict", toolUseID)
	}
	bubbleID := call.GetSpawnedBubbleId()
	if bubbleID == "" {
		t.Fatalf("the launching call %q carries an empty spawned_bubble_id: async-bubble.proto states an empty id is unrepresentable for work that detached", toolUseID)
	}

	anchors := asyncBubbleItems(seen.items)
	anchor := openedBubble(anchors, bubbleID)
	if anchor == nil {
		t.Fatalf("no ConversationItem.async_bubble anchored bubble %q in the feed (saw %d async_bubble items): the bubble has no place in the conversation that started it",
			bubbleID, len(anchors))
	}
	if got, want := anchor.GetOriginToolUseId(), toolUseID; got != want {
		t.Errorf("anchored bubble origin_tool_use_id = %q, want the launching call's %q: a frontend cannot attach the bubble to its originating card", got, want)
	}
	if anchor.GetLiveness().GetLive() == nil {
		t.Errorf("anchored bubble %q opened with liveness %v, want the live arm: a launch that opens already-settled is unrepresentable while its agent is still running",
			bubbleID, anchor.GetLiveness().GetState())
	}
}
