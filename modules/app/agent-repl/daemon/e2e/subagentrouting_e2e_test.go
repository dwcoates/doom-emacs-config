// THE ACCEPTANCE CRITERION of the figma→idl reshape, end to end over the real
// processes.
//
// A detached agent's response used to land in the TOP-LEVEL FEED. The cause was
// client-side derivation: each frontend worked out for itself, from a mix of
// structured metadata and free-text prose, which utterances belonged to which
// dispatched agent — and got it wrong. The reshape removes the derivation
// entirely: the daemon classifies the spawning call, MINTS the bubble id,
// stamps that id on the call's AgentToolCall.spawned_bubble_id, and addresses
// every subsequent update to it (async-bubble.proto §"THE ROUTING HANDLE").
//
// So the criterion is a single indivisible statement about one record: the
// subagent's response arrives INSIDE its bubble's AsyncAgentUpdate, addressed
// by the id the spawning call published, and DOES NOT appear as a top-level
// ConversationItem. Both halves are asserted from one drain, because a record
// that satisfies one observer and is missed by the other proves nothing.
package e2e

import (
	"strings"
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// TestE2EASubagentResponseIsRoutedToItsBubbleAndNeverToTheFeed is the
// acceptance test.
func TestE2EASubagentResponseIsRoutedToItsBubbleAndNeverToTheFeed(t *testing.T) {
	// Arrange
	h := newUDSHarness(t)
	cwd := t.TempDir()
	_, conn, vendorID, store := liveSession(t, h, cwd)
	const (
		toolUseID = "toolu_e2e_subagent_dispatch"
		agentID   = "agent_e2e_subagent"
		// The utterance under test. Distinctive so its presence anywhere in the
		// top-level feed is unambiguous rather than a substring coincidence.
		subagentText = "e2e-subagent-utterance: this belongs inside the bubble"
		// The BARRIER: a real user prompt written after the subagent's record.
		// The store preserves per-session write order and the daemon curates in
		// that order, so once this prompt's item has arrived the subagent record
		// has been through the whole pipeline — whatever the daemon decided to do
		// with it. That is what makes the negative half a proof rather than a
		// race with a timeout.
		barrierPrompt = "e2e-barrier: the subagent record is now fully processed"
	)

	// Act — the dispatch, its async-launch outcome, the subagent's own sidechain
	// utterance, then the barrier.
	store.write(vendorLineEvent(t, vendorID, asyncToolCallLine("e2e-subagent-call", toolUseID, "Task")))
	store.write(vendorLineEvent(t, vendorID, asyncToolResultLine(
		"e2e-subagent-launch", toolUseID, "Launched agent",
		agentAsyncLaunchOutcome(agentID, "investigate the routing defect"))))
	store.write(vendorLineEvent(t, vendorID, sidechainResponseLine(
		"e2e-subagent-response", "e2e-subagent-launch", toolUseID, agentID, subagentText)))
	store.write(sidecarUserLineEvent(t, vendorID, "e2e-subagent-barrier", barrierPrompt))

	// Assert
	seen := drainUntilItem(t, conn, cwd, "the barrier prompt's user item", func(it *frontendv1.ConversationItem) bool {
		return it.GetUserMessage().GetContentString() == barrierPrompt
	})

	call := spawningCall(seen.items, toolUseID)
	if call == nil {
		t.Fatalf("no top-level tool_call item for the dispatching call %q arrived: the daemon never published its classification verdict", toolUseID)
	}
	bubbleID := call.GetSpawnedBubbleId()
	if bubbleID == "" {
		t.Fatalf("the dispatching call %q carries an empty spawned_bubble_id: the daemon classified a Task launch as detaching nothing, so no update can ever be routed", toolUseID)
	}

	// THE POSITIVE HALF: the utterance is inside the bubble's agent-arm updates.
	routed := false
	for _, emission := range seen.agentEmissions(bubbleID) {
		if emissionCarriesText(emission, subagentText) {
			routed = true
			break
		}
	}
	if !routed {
		t.Errorf("the subagent's response never arrived in an AsyncAgentUpdate addressed to bubble %q (saw %d updates for it across %d async pushes)",
			bubbleID, len(seen.updatesFor(bubbleID)), len(seen.deltas))
	}

	// THE NEGATIVE HALF: it is nowhere in the top-level feed. This is the
	// regression the whole reshape exists to make impossible.
	for _, item := range seen.items {
		if itemCarriesText(item, subagentText) {
			t.Errorf("the subagent's response reached the TOP-LEVEL FEED as ConversationItem uuid=%q: detached-agent output must be routed to bubble %q, never rendered in the conversation that dispatched it",
				item.GetUuid(), bubbleID)
		}
	}
}

// emissionCarriesText reports whether an agent emission's response body carries
// text. Only the response arm is inspected: the criterion is about the agent's
// spoken output, and matching any arm would let a tool card's echo of the same
// prose pass for the response itself.
func emissionCarriesText(emission *frontendv1.AgentEmission, text string) bool {
	for _, block := range emission.GetResponse().GetBody().GetContent() {
		if strings.Contains(block.GetText().GetText(), text) {
			return true
		}
	}
	return false
}

// itemCarriesText reports whether a top-level conversation item shows text
// anywhere a reader would see it: an assistant response, a user bubble, or a
// tool result's string body. Deliberately BROADER than emissionCarriesText —
// the negative half must not be satisfiable by the daemon merely moving the
// utterance to a different feed arm.
func itemCarriesText(item *frontendv1.ConversationItem, text string) bool {
	if emissionCarriesText(item.GetAgent(), text) {
		return true
	}
	if strings.Contains(item.GetUserMessage().GetContentString(), text) {
		return true
	}
	return strings.Contains(item.GetAgent().GetToolResult().GetResult().GetContentString(), text)
}
