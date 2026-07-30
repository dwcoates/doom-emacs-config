// THE BRING-UP GATE, end to end over the REAL processes.
//
// The chain this pins — daemon listener, ShimHello, DaemonHello carrying
// from_seq, the shim's standing store subscription, ShimReady — used to
// complete in stages, and a health probe landing in any gap between them was
// answered with store_subscribed=false about a session the daemon had itself
// just brought up. The two tests here are the exact live sequences that
// failed: `SPC o c`, which probes health the instant a session opens, and a
// mid-session rotation, whose deliberate bounce re-runs the whole gate.
//
// These share e2e_test.go's package and reuse its helpers READ-ONLY
// (newUDSHarness, createSession, dial, readFrame, writeCmd, frameTimeout),
// clearcompact's waitAttachedSession, interrupt_e2e_test.go's assistantText /
// echoOf, and rotation_e2e_test.go's rotateSession.
package e2e

import (
	"fmt"
	"strings"
	"testing"
	"time"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"github.com/gorilla/websocket"
)

// unwiredHealthRouter is the harness gap these probes cannot close from here:
// newUDSHarness builds its AgentShimConfig without `Health: controller`, so the
// frontend session-health command has nothing to route to. The controller already
// satisfies server.SessionHealthRouter, so the fix is that one field — until
// it lands, a probe here proves nothing and says so rather than passing.
const unwiredHealthRouter = "session health router is not wired"

// awaitSessionHealth issues ONE SessionHealth command and returns its view.
//
// Deliberately NO RETRY. The whole point of the gate is that the answer to the
// first probe is the true one, so a retry here would hide exactly the defect
// this test exists to catch.
func awaitSessionHealth(t *testing.T, conn *websocket.Conn, workspace, sessionID, requestID string) *frontendv1.SessionHealthView {
	t.Helper()
	writeCmd(t, conn, fmt.Sprintf(
		`{"requestId":%q,"workspace":%q,"sessionHealth":{"sessionId":%q}}`,
		requestID, workspace, sessionID))
	deadline := time.Now().Add(frameTimeout)
	for time.Now().Before(deadline) {
		frame := readFrame(t, conn)
		if f, ok := frame.GetFrame().(*frontendv1.FrontendFrame_SessionHealth); ok {
			if f.SessionHealth.GetRequestId() == requestID {
				return f.SessionHealth
			}
		}
		if f, ok := frame.GetFrame().(*frontendv1.FrontendFrame_CommandAck); ok {
			if f.CommandAck.GetRequestId() == requestID && !f.CommandAck.GetOk() {
				t.Fatalf("sessionHealth nacked: %s", f.CommandAck.GetError())
			}
		}
	}
	t.Fatalf("no SessionHealthView for %s arrived before the deadline", requestID)
	return nil
}

// TestE2EFirstHealthProbeAfterOpenIsHealthy is the `SPC o c` sequence: open a
// session and probe its health at once, with nothing in between.
//
// It is the live failure verbatim. The daemon acks the create the instant the
// spawn is ISSUED and Emacs probes immediately after; before the gate, the
// probe reached a shim that had handshaked but not yet subscribed to the
// store, and came back unhealthy with "standing store subscription is not
// live". Only a retry made it pass, which is the tell that readiness was
// asserted too early rather than the probe being too eager.
func TestE2EFirstHealthProbeAfterOpenIsHealthy(t *testing.T) {
	// Arrange
	h := newUDSHarness(t)
	cwd := t.TempDir()
	id := h.createSession(t, cwd)
	conn := h.dial(t, id)
	if first := readFrame(t, conn); first.GetSnapshot() == nil {
		t.Fatalf("first frame = %T, want a StateSnapshot", first.GetFrame())
	}

	// Act — the very first probe of this session's life.
	view := awaitSessionHealth(t, conn, cwd, id, "e2e-first-health-1")
	if strings.Contains(view.GetReason(), unwiredHealthRouter) {
		t.Skipf("harness gap, not a product failure: newUDSHarness (e2e_test.go) omits `Health: controller` from its AgentShimConfig, so no probe can reach the session. Add that one field to make this test real.")
	}

	// Assert
	if !view.GetHealthy() {
		t.Fatalf("first health probe after open: healthy=false reason=%q — readiness resolved before the shim was wired", view.GetReason())
	}
	if got := view.GetSessionId(); got != id {
		t.Errorf("health view session_id = %q, want %q", got, id)
	}
}

// TestE2ERotationResubscribesAndKeepsTheConversationFlowing covers the OTHER
// end of the same gate: a rotation bounces the daemon link deliberately, so the
// whole exchange runs again — new uuid announced, cursor reset, store
// re-subscribed in the new seq space, ack — and the conversation must still
// flow afterwards.
func TestE2ERotationResubscribesAndKeepsTheConversationFlowing(t *testing.T) {
	// Arrange
	h := newUDSHarness(t)
	cwd := t.TempDir()
	id := h.createSession(t, cwd)
	conn := h.dial(t, id)
	if first := readFrame(t, conn); first.GetSnapshot() == nil {
		t.Fatalf("first frame = %T, want a StateSnapshot", first.GetFrame())
	}
	waitAttachedSession(t, conn, id, cwd)

	// Act — rotate mid-session, then prompt across the bounce.
	rot := rotateSession(t, h, conn, id, cwd)

	// Assert — the re-handshaked session still carries a turn, which is the
	// part of the gate this harness can prove end to end today.
	writeCmd(t, conn, `{"requestId":"r-after-rotation","submitPrompt":{"text":"after the rotation"}}`)
	deadline := time.Now().Add(frameTimeout)
	for time.Now().Before(deadline) {
		frame := readFrame(t, conn)
		for _, item := range deltaItems(frame, cwd) {
			if strings.Contains(assistantText(item), echoOf("after the rotation")) {
				return
			}
		}
	}
	t.Fatalf("no post-rotation reply arrived after %s -> %s: the re-handshake's subscription never carried the new seq space", rot.previous, rot.next)
}

// TestE2EHealthProbeAfterRotationIsHealthy is the rotation's half of the same
// SPC-o-c question: after the deliberate bounce re-runs the gate, the first
// probe must be answered healthy exactly as a fresh session's is.
func TestE2EHealthProbeAfterRotationIsHealthy(t *testing.T) {
	// Arrange
	h := newUDSHarness(t)
	cwd := t.TempDir()
	id := h.createSession(t, cwd)
	conn := h.dial(t, id)
	if first := readFrame(t, conn); first.GetSnapshot() == nil {
		t.Fatalf("first frame = %T, want a StateSnapshot", first.GetFrame())
	}
	waitAttachedSession(t, conn, id, cwd)
	rot := rotateSession(t, h, conn, id, cwd)

	// Act
	view := awaitSessionHealth(t, conn, cwd, id, "e2e-rotated-health-1")
	if strings.Contains(view.GetReason(), unwiredHealthRouter) {
		t.Skipf("harness gap, not a product failure: newUDSHarness (e2e_test.go) omits `Health: controller` from its AgentShimConfig, so no probe can reach the session. Add that one field to make this test real.")
	}

	// Assert
	if !view.GetHealthy() {
		t.Fatalf("health probe after the rotation %s -> %s: healthy=false reason=%q", rot.previous, rot.next, view.GetReason())
	}
}
