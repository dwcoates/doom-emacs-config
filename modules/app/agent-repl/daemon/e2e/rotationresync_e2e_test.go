// THE FRONTEND'S SIDE OF A VENDOR SESSION ROTATION, over the real processes:
// a client that was CONNECTED AND PAINTING across the rotation still holds a
// replay mark counted in the store seq space the rotation retired, and asks the
// daemon for history with it.
//
// This is the last hop of the defect rotation_e2e_test.go covers the rest of.
// The shim re-keys, the daemon resets its cursor and resubscribes from the new
// space's beginning — and then the frontend asks from 1060 while the new space
// has reached 12.
//
// THE MARK IS UNANSWERABLE, AND IS NOW REFUSED. Read as an ordinary client mark
// it means "already past everything" and serves nothing at all. Floored — which
// is what the daemon used to do — it means "serve from the newest clear", which
// for a conversation the rotation just restarted is the WHOLE conversation: the
// full replay paging exists to end, re-inflicted on every bounce, for every
// workspace. Neither is an answer to the question the client asked, so the
// daemon refuses the read (`rejection_cause=retired_seq_space`) and the client
// re-anchors from a bounded TAIL PAGE, which is where the clear reaches it.
//
// These tests share e2e_test.go's package and reuse its helpers READ-ONLY
// (newUDSHarness, dial, readFrame, writeCmd, frameTimeout), clearcompact's
// liveSession / deltaItems / isClear / sidecarClearEvent / clearDedupKey,
// interrupt_e2e_test.go's awaitAll / assistantText / echoOf, and
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

// replayItemsFrom is clearcompact's replayItems with the client mark under
// test: it drives a resync the daemon is expected to SERVE, and collects the
// conversation it replays. A mark from a retired seq space is no longer such a
// request — see resyncRefusalFrom below.
func replayItemsFrom(t *testing.T, conn *websocket.Conn, state *frontendv1.WorkspaceState, workspace, requestID string, fromSeq uint64) []*frontendv1.ConversationItem {
	t.Helper()
	writeCmd(t, conn, fmt.Sprintf(`{"requestId":%q,"resync":{"fromSeq":"%d","fence":%q}}`,
		requestID, fromSeq, state.GetFence()))
	var out []*frontendv1.ConversationItem
	deadline := time.Now().Add(frameTimeout)
	for time.Now().Before(deadline) {
		frame := readFrame(t, conn)
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

// resyncRefusalFrom sends one resync with the mark under test and returns the
// nack's error text, failing if the daemon ACCEPTED the request or pushed any
// conversation for it.
//
// The pushed-item count is asserted here rather than in one caller because it is
// the user-visible property the whole change exists for: a refusal that still
// backfilled the conversation would be the old bug wearing an error.
func resyncRefusalFrom(t *testing.T, conn *websocket.Conn, state *frontendv1.WorkspaceState, workspace, requestID string, fromSeq uint64) string {
	t.Helper()
	writeCmd(t, conn, fmt.Sprintf(`{"requestId":%q,"resync":{"fromSeq":"%d","fence":%q}}`,
		requestID, fromSeq, state.GetFence()))
	var pushed int
	deadline := time.Now().Add(frameTimeout)
	for time.Now().Before(deadline) {
		frame := readFrame(t, conn)
		if ack, ok := frame.GetFrame().(*frontendv1.FrontendFrame_CommandAck); ok && ack.CommandAck.GetRequestId() == requestID {
			if ack.CommandAck.GetOk() {
				t.Fatalf("resync from a retired-space mark was ACCEPTED; it must be refused so the client re-anchors instead of being backfilled")
			}
			if pushed != 0 {
				t.Fatalf("a REFUSED resync pushed %d conversation delta(s); the refusal exists precisely so that nothing is replayed", pushed)
			}
			return ack.CommandAck.GetError()
		}
		pushed += len(deltaItems(frame, workspace))
	}
	t.Fatalf("no CommandAck for resync %s arrived before the deadline", requestID)
	return ""
}

// shimReplayIdle is the window a shim-served replay must go quiet for before the
// shim calls it complete (agent-shim/claude/shim/src/uds/uds-session.ts,
// `replayIdleMs ?? 5000`).
//
// It is the ONLY terminator an UNBOUNDED replay has, and a TAIL page's replay is
// deliberately unbounded above: conversationpage.go caps a tail read at nothing,
// because capping it at last_seen_seq served an EMPTY page over a store holding
// the whole conversation. A tail page through a LIVE shim therefore cannot land
// before this window elapses, whatever the machine is doing.
const shimReplayIdle = 5 * time.Second

// tailPageBudget is what a tail page served THROUGH A LIVE SHIM is bounded by:
// the shim's replay idle window, plus the suite's ordinary frame budget for
// assembling and pushing the page once the replay ends.
//
// The bare frame budget is NOT the bound here — it is the same 5s as the idle
// window it would have to wait out first, so an await under it can only ever
// expire. This is the case readFrameWithin exists for, and the bound is named
// rather than padded.
var tailPageBudget = shimReplayIdle + frameTimeout

// tailPageFrom asks for the conversation's tail — the re-anchor a client takes
// when its mark is refused — and returns the page.
func tailPageFrom(t *testing.T, conn *websocket.Conn, state *frontendv1.WorkspaceState, workspace, requestID string, limit int) *frontendv1.ConversationPage {
	t.Helper()
	writeCmd(t, conn, fmt.Sprintf(`{"requestId":%q,"workspace":%q,"conversationPage":{"tail":{"limit":%d},"fence":%q}}`,
		requestID, workspace, limit, state.GetFence()))
	deadline := time.Now().Add(tailPageBudget)
	for time.Now().Before(deadline) {
		frame := readFrameWithin(t, conn, tailPageBudget)
		switch f := frame.GetFrame().(type) {
		case *frontendv1.FrontendFrame_CommandAck:
			if f.CommandAck.GetRequestId() == requestID && !f.CommandAck.GetOk() {
				t.Fatalf("the re-anchor's tail page was refused: %s", f.CommandAck.GetError())
			}
		case *frontendv1.FrontendFrame_ConversationPage:
			if f.ConversationPage.GetRequestId() == requestID {
				return f.ConversationPage
			}
		}
	}
	t.Fatalf("no ConversationPage for %s arrived before the deadline", requestID)
	return nil
}

// TestE2EResyncFromARetiredSpaceMarkIsREFUSED covers THE REPLAY THAT NO LONGER
// HAPPENS. The reconnect path must have no route to a whole-conversation
// backfill at all: a mark the live space cannot honor earns a typed refusal
// naming the cause the client re-anchors on, and not one conversation item.
func TestE2EResyncFromARetiredSpaceMarkIsREFUSED(t *testing.T) {
	// Arrange — see rotation_e2e_test.go for why the tempdir precedes the
	// harness, and for why the clear is injected rather than produced.
	cwd := t.TempDir()
	h := newUDSHarness(t)
	id, conn, _, store := liveSession(t, h, cwd)
	rot := rotateSession(t, h, conn, id, cwd)
	_, state := dialForReplay(t, h, id, cwd)
	const lineUUID = "e2e-retired-mark-clear-1"
	store.write(sidecarClearEvent(rot.next, lineUUID))
	awaitAll(t, conn, nil, map[string]func(*frontendv1.FrontendFrame) bool{
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
	refusal := resyncRefusalFrom(t, conn, state, cwd, "r-retired-mark", retiredSpaceMark)

	// Assert — the cause token the webapp matches on to decide to re-anchor.
	if !strings.Contains(refusal, "retired_seq_space") {
		t.Fatalf("refusal = %q, want it to name rejection_cause=retired_seq_space", refusal)
	}
}

// TestE2ETheReAnchorAfterARefusedMarkIsABoundedTailPage covers WHERE THE CLEAR
// REACHES THE CLIENT NOW. Refusing the mark would be a regression on its own —
// the rotation's clear still has to be drawn — so the re-anchor is asserted to
// deliver it, and to deliver it in a page the SIZE THE CLIENT ASKED FOR rather
// than the size of the conversation.
func TestE2ETheReAnchorAfterARefusedMarkIsABoundedTailPage(t *testing.T) {
	// Arrange — a conversation with more history than the page asks for, plus
	// the rotation's clear at its newest end.
	cwd := t.TempDir()
	h := newUDSHarness(t)
	id, conn, _, store := liveSession(t, h, cwd)
	rot := rotateSession(t, h, conn, id, cwd)
	_, state := dialForReplay(t, h, id, cwd)
	const lineUUID = "e2e-retired-mark-clear-2"
	store.write(sidecarClearEvent(rot.next, lineUUID))
	awaitAll(t, conn, nil, map[string]func(*frontendv1.FrontendFrame) bool{
		"the clear's live push": func(frame *frontendv1.FrontendFrame) bool {
			for _, item := range deltaItems(frame, cwd) {
				if isClear(item) && item.GetUuid() == clearDedupKey(lineUUID) {
					return true
				}
			}
			return false
		},
	})
	resyncRefusalFrom(t, conn, state, cwd, "r-retired-mark", retiredSpaceMark)

	// Act — the re-anchor: the same request a page with no history makes.
	page := tailPageFrom(t, conn, state, cwd, "r-retired-mark-tail", 2)

	// Assert — BOUNDED. The page carries at most what was asked for, which is
	// the property a full replay violates by definition.
	if got := len(page.GetItems()); got > 2 {
		t.Fatalf("the re-anchor's tail page carried %d items for a limit of 2 — the reconnect path must never serve the whole conversation", got)
	}
	var clears int
	for _, item := range page.GetItems() {
		if isClear(item) && item.GetUuid() == clearDedupKey(lineUUID) {
			clears++
		}
	}
	if clears != 1 {
		t.Fatalf("the re-anchor's tail page carried %d ContextCleared items for %s, want 1 — the rotation's clear must still reach the client", clears, clearDedupKey(lineUUID))
	}
}

// TestE2ETurnAfterARetiredSpaceResyncStillFlows covers the OTHER half: the
// stale-mark refusal is a ruling about one replay, not a state the connection is
// left in. The very next turn's conversation must still reach the same client,
// live.
func TestE2ETurnAfterARetiredSpaceResyncStillFlows(t *testing.T) {
	// Arrange — see the cleanup-order note in the first test.
	cwd := t.TempDir()
	h := newUDSHarness(t)
	id, conn, _, _ := liveSession(t, h, cwd)
	rotateSession(t, h, conn, id, cwd)
	// THE ROTATED TURN IS DRAINED TO ITS END BEFORE THE REFUSAL IS ASKED FOR.
	// rotateSession returns on the rotated turn's REPLY, which is mid-turn: the
	// result delta and the turn's own end are still on the wire behind it. The
	// refusal asserts that NOTHING was pushed for the resync, and a delta the
	// rotation produced before the resync was ever written would be counted
	// against it — the sibling tests get this barrier from awaiting the injected
	// clear, and this one had none. The SSM's own resolution to DONE is caused by
	// the turn's end, so it cannot precede any of that turn's conversation.
	awaitAll(t, conn, nil, map[string]func(*frontendv1.FrontendFrame) bool{
		"the rotated turn RESOLVED to DONE, which is every one of its deltas drained": func(frame *frontendv1.FrontendFrame) bool {
			return ssmResolved(frame, cwd, frontendv1.RenderState_RENDER_STATE_DONE, "turn_ended")
		},
	})
	_, state := dialForReplay(t, h, id, cwd)
	resyncRefusalFrom(t, conn, state, cwd, "r-retired-mark", retiredSpaceMark)

	// Act
	writeCmd(t, conn, `{"requestId":"r-after-resync","submitPrompt":{"text":"after-the-stale-resync","promptOrigin":"PROMPT_ORIGIN_USER_SENT"}}`)

	// Assert
	awaitAll(t, conn, nil, map[string]func(*frontendv1.FrontendFrame) bool{
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
