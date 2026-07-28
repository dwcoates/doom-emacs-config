// THE FRONTEND'S SIDE OF A VENDOR SESSION ROTATION, over the real processes:
// a client that was CONNECTED AND PAINTING across the rotation still holds a
// replay mark counted in the store seq space the rotation retired, and asks the
// daemon for history with it.
//
// This is the last hop of the defect rotation_e2e_test.go covers the rest of.
// The shim re-keys, the daemon resets its cursor and resubscribes from the new
// space's beginning — and then the frontend asks from 1060 while the new space
// has reached 12, which read as an ordinary client mark means "already past
// everything" and is answered with nothing at all: no clear line, no
// post-rotation history, on a connection that never dropped.
//
// The painting client is not incidental. `painter.throughSeq` only ever rises,
// so across a rotation its paint acks keep carrying a seq of the RETIRED space
// — exactly what the production webview did — and these tests run with that
// condition standing rather than arranged.
//
// These tests share e2e_test.go's package and reuse its helpers READ-ONLY
// (newUDSHarness, dial, readFrame, writeCmd, frameTimeout), clearcompact's
// liveSession / deltaItems / isClear / sidecarClearEvent / clearDedupKey,
// interrupt_e2e_test.go's painter / awaitAll / assistantText / echoOf, and
// rotation_e2e_test.go's rotateSession.
package e2e

import (
	"fmt"
	"strings"
	"testing"
	"time"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"github.com/gorilla/websocket"
)

// retiredSpaceMark is a replay mark from a store seq space the vendor retired:
// far above anything the rotated conversation has produced, and therefore
// impossible in the space the daemon is now serving. The value is the one the
// production webview acked (`through_seq=1060`) while the new space stood at 12.
const retiredSpaceMark = 1060

// replayItemsFrom is clearcompact's replayItems with the client mark under test
// and a painter kept fed: the connection stays a PAINTING one for the whole
// exchange, so the workspace cannot fall out from under the replay.
func replayItemsFrom(t *testing.T, conn *websocket.Conn, p *painter, workspace, requestID string, fromSeq uint64) []*frontendv1.ConversationItem {
	t.Helper()
	writeCmd(t, conn, fmt.Sprintf(`{"requestId":%q,"resync":{"fromSeq":"%d"}}`, requestID, fromSeq))
	var out []*frontendv1.ConversationItem
	deadline := time.Now().Add(frameTimeout)
	for time.Now().Before(deadline) {
		frame := readFrame(t, conn)
		p.observe(frame)
		if ack, ok := frame.GetFrame().(*frontendv1.FrontendFrame_CommandAck); ok && ack.CommandAck.GetRequestId() == requestID {
			if !ack.CommandAck.GetOk() {
				t.Fatalf("resync nacked: %s", ack.CommandAck.GetError())
			}
			return out
		}
		out = append(out, deltaItems(frame, workspace)...)
	}
	t.Fatalf("no CommandAck for resync %s arrived before the deadline", requestID)
	return nil
}

// TestE2EResyncFromARetiredSpaceMarkStillReplaysTheClear covers THE CLEAR A
// STALE MARK USED TO FLOOR AWAY. The sidecar files its ContextCleared under the
// ROTATED uuid, so the clear lives at a low seq in the new space — below every
// mark the client carries out of the retired one. Answering that mark literally
// serves the frontend nothing, which is the "footer recovers, but no red line
// and no truncation, ever" the whole rotation work ends at.
func TestE2EResyncFromARetiredSpaceMarkStillReplaysTheClear(t *testing.T) {
	// Arrange — see rotation_e2e_test.go for why the tempdir precedes the
	// harness, and for why the clear is injected rather than produced.
	cwd := t.TempDir()
	h := newUDSHarness(t)
	id, conn, _, store := liveSession(t, h, cwd)
	p := newPainter(t, conn, cwd)
	rot := rotateSession(t, h, conn, p, id, cwd)
	const lineUUID = "e2e-retired-mark-clear-1"
	store.write(sidecarClearEvent(rot.next, lineUUID))
	awaitAll(t, conn, p, nil, map[string]func(*frontendv1.FrontendFrame) bool{
		"the clear's live push": func(frame *frontendv1.FrontendFrame) bool {
			for _, item := range deltaItems(frame, cwd) {
				if isClear(item) && item.GetUuid() == clearDedupKey(lineUUID) {
					return true
				}
			}
			return false
		},
	})

	// Act — the client asks with the mark it carried out of the retired space.
	items := replayItemsFrom(t, conn, p, cwd, "r-retired-mark", retiredSpaceMark)

	// Assert — arm 32, replayed, on a mark that claimed to be past it.
	var clears int
	for _, item := range items {
		if isClear(item) && item.GetUuid() == clearDedupKey(lineUUID) {
			clears++
		}
	}
	if clears != 1 {
		t.Fatalf("replayed %d ContextCleared items for %s, want 1 — a mark from the retired seq space must not be read as 'past everything'", clears, clearDedupKey(lineUUID))
	}
}

// TestE2ETurnAfterARetiredSpaceResyncStillFlows covers the OTHER half: the
// stale-mark resync is a ruling about one replay, not a state the connection is
// left in. The very next turn's conversation must still reach the same painting
// client, live.
func TestE2ETurnAfterARetiredSpaceResyncStillFlows(t *testing.T) {
	// Arrange — see the cleanup-order note in the first test.
	cwd := t.TempDir()
	h := newUDSHarness(t)
	id, conn, _, _ := liveSession(t, h, cwd)
	p := newPainter(t, conn, cwd)
	rotateSession(t, h, conn, p, id, cwd)
	replayItemsFrom(t, conn, p, cwd, "r-retired-mark", retiredSpaceMark)

	// Act
	writeCmd(t, conn, `{"requestId":"r-after-resync","submitPrompt":{"text":"after-the-stale-resync"}}`)

	// Assert
	awaitAll(t, conn, p, nil, map[string]func(*frontendv1.FrontendFrame) bool{
		"the post-resync turn's reply": func(frame *frontendv1.FrontendFrame) bool {
			for _, item := range deltaItems(frame, cwd) {
				if strings.Contains(assistantText(item), echoOf("after-the-stale-resync")) {
					return true
				}
			}
			return false
		},
	})
}
