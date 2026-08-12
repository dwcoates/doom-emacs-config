// NOTHING renders a prompt before its round trip, end to end over the REAL
// processes: a frontend submits, and no prompt bubble for that submit reaches
// the socket ahead of the turn's first store-planed frame.
//
// WHAT THIS FILE USED TO ASSERT, AND WHY IT IS INVERTED. It used to pin the
// daemon-authored prompt RECEIPT arriving before any store-planed frame of the
// turn — the receipt existed precisely to fill the window the round trip leaves
// empty. FROZEN-slash-command-durability.md Part 2 removes prompt receipts
// entirely: a prompt renders when its durable line round-trips through the SDK,
// never before, because an instant render is a second identity for one prompt
// and reconciling the two is what produced the duplicate bubbles. The ordering
// claim survives with its sign flipped, and that is what is asserted here.
//
// WHY THIS IS AN E2E RATHER THAN A UNIT TEST. The claim is about ORDER ACROSS
// PROCESSES: anything daemon-composed at submit would reach the socket
// immediately, while everything else in the turn travels prompt → shim → store
// → seq → back to the daemon. Only the real chain can say which of those
// reaches the socket first.
//
// Shares e2e_test.go's package and reuses its helpers READ-ONLY (newUDSHarness,
// createSession, dial, readFrame, writeCmd, frameTimeout) plus
// clearcompact_e2e_test.go's liveSession/deltaItems.
package e2e

import (
	"testing"
	"time"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"github.com/gorilla/websocket"
)

// firstStorePlanedDelta reads frames until this workspace's first delta
// carrying a STORE SEQ (through_seq > 0), returning every workspace delta seen
// strictly before it, in arrival order.
func firstStorePlanedDelta(t *testing.T, conn *websocket.Conn, workspace string) []*frontendv1.ConversationDelta {
	t.Helper()
	var before []*frontendv1.ConversationDelta
	deadline := time.Now().Add(frameTimeout)
	for time.Now().Before(deadline) {
		frame := readFrame(t, conn)
		cd, ok := frame.GetFrame().(*frontendv1.FrontendFrame_ConversationDelta)
		if !ok || cd.ConversationDelta.GetWorkspace() != workspace {
			continue
		}
		if cd.ConversationDelta.GetThroughSeq() > 0 {
			return before
		}
		before = append(before, cd.ConversationDelta)
	}
	t.Fatalf("no store-planed conversation delta arrived for workspace %s before the deadline", workspace)
	return nil
}

// TestE2ENoPromptBubblePrecedesTheTurnsFirstStoreFrame is the ordering claim,
// inverted by contract Part 2: nothing the daemon composed at submit stands in
// for the prompt ahead of anything the store stamped for that turn.
//
// The store-planed delta is the synchronization point, not a duration: it is
// the first frame of the turn that came back through the store, so every frame
// the submit alone could have drawn has necessarily already arrived by then.
func TestE2ENoPromptBubblePrecedesTheTurnsFirstStoreFrame(t *testing.T) {
	// Arrange
	h := newUDSHarness(t)
	cwd := t.TempDir()
	_, live, _, _ := liveSession(t, h, cwd)

	// Act
	writeCmd(t, live, `{"requestId":"r-echo","submitPrompt":{"text":"the prompt itself","promptOrigin":"PROMPT_ORIGIN_USER_SENT"}}`)

	// Assert — no user message for this submit is among the seq-less deltas
	// that precede the turn's first store-planed one.
	for _, cd := range firstStorePlanedDelta(t, live, cwd) {
		for _, item := range cd.GetMessages() {
			if item.GetUserMessage() == nil {
				continue
			}
			if item.GetRequestId() == "r-echo" || item.GetUserMessage().GetContentString() == "the prompt itself" {
				t.Errorf("a prompt bubble uuid=%q request_id=%q reached the frontend BEFORE the turn's first store-planed frame — the optimistic render Part 2 removed is back, and reconciling it against the durable line is what produced the duplicate bubbles",
					item.GetUuid(), item.GetRequestId())
			}
		}
	}
}
