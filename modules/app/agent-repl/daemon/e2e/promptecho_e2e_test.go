// The daemon-authored prompt receipt, end to end over the REAL processes: a
// frontend submits, and the daemon pushes the prompt bubble ITSELF — before any
// store-planed frame of the turn that submit starts.
//
// WHY THIS IS AN E2E RATHER THAN A UNIT TEST. The claim is about ORDER ACROSS
// PROCESSES: the receipt is composed by the daemon at submit, while everything
// else in the turn travels prompt → shim → store → seq → back to the daemon.
// Only the real chain can say which of those reaches the socket first, and that
// order is the entire point of the receipt — it exists to fill the window the
// round trip leaves empty.
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

// TestE2EThePromptReceiptPrecedesTheTurnsFirstStoreFrame is the ordering claim
// itself: the bubble the daemon authored arrives before anything the store
// stamped for that turn.
func TestE2EThePromptReceiptPrecedesTheTurnsFirstStoreFrame(t *testing.T) {
	// Arrange
	h := newUDSHarness(t)
	cwd := t.TempDir()
	_, live, _, _ := liveSession(t, h, cwd)

	// Act
	writeCmd(t, live, `{"requestId":"r-echo","submitPrompt":{"text":"the prompt itself"}}`)

	// Assert — the receipt is among the seq-less deltas that precede the
	// turn's first store-planed one, keyed on the submit's own request id.
	found := false
	for _, cd := range firstStorePlanedDelta(t, live, cwd) {
		for _, item := range cd.GetItems() {
			if item.GetRequestId() != "r-echo" || item.GetUserMessage() == nil {
				continue
			}
			found = true
			if got := item.GetUserMessage().GetContentString(); got != "the prompt itself" {
				t.Errorf("receipt text = %q, want the submitted prompt", got)
			}
		}
	}
	if !found {
		t.Fatal("no prompt receipt arrived before the turn's first store-planed frame")
	}
}
