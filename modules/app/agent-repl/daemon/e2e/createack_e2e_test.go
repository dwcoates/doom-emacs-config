// THE createSession ACK, end to end over the REAL processes.
//
// The ack used to mean "a spawn was issued", and every client bridged the gap
// between that and a usable session by polling health. The poll lost races the
// client could not see — probe before the shim attached, probe before the
// shim's standing store subscription settled — so the gap was closed in the
// daemon instead: the ack is written only once the new session's shim answers a
// health probe healthy over the wired connection.
//
// These two tests are the claim and its failure mode. The first spends the ack
// immediately, with no probe and no retry between it and a prompt, because that
// is exactly what a client does now that the poll is gone. The second wedges
// the bring-up so the handshake can never complete, and requires a loud nack
// naming the pending link rather than a hang.
//
// Shares e2e_test.go's package and reuses newUDSHarness, dialFrontend,
// readFrame, writeCmd, frameTimeout, the harness options, clearcompact's
// deltaItems, and interrupt_e2e_test.go's assistantText / echoOf.
package e2e

import (
	"fmt"
	"strings"
	"testing"
	"time"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"github.com/gorilla/websocket"
)

// createAck sends ONE createSession and returns its ack plus the new session id
// (correlated off the pushed SessionView, which the daemon sends before the
// ack). Deliberately no retry and no health probe: the whole point is that the
// ack alone is sufficient.
func createAck(t *testing.T, conn *websocket.Conn, cwd, requestID string) (*frontendv1.CommandAck, string) {
	t.Helper()
	known := map[string]bool{}
	snap := readFrame(t, conn)
	if snap.GetSnapshot() == nil {
		t.Fatalf("first /frontend frame = %T, want a StateSnapshot", snap.GetFrame())
	}
	for _, sv := range snap.GetSnapshot().GetSessions() {
		known[sv.GetSessionId()] = true
	}
	writeCmd(t, conn, fmt.Sprintf(`{"requestId":%q,"createSession":{"cwd":%q,"fake":true}}`, requestID, cwd))

	var id string
	deadline := time.Now().Add(frameTimeout)
	for time.Now().Before(deadline) {
		frame := readFrame(t, conn)
		switch f := frame.GetFrame().(type) {
		case *frontendv1.FrontendFrame_SessionView:
			sv := f.SessionView
			if sv.GetCwd() == cwd && sv.GetSessionId() != "" && !known[sv.GetSessionId()] {
				id = sv.GetSessionId()
			}
		case *frontendv1.FrontendFrame_CommandAck:
			if f.CommandAck.GetRequestId() == requestID {
				return f.CommandAck, id
			}
		}
	}
	t.Fatalf("no CommandAck for %s arrived before the deadline", requestID)
	return nil, ""
}

// TestE2ECreateAckMeansThePromptPathIsOpen spends the ack the way a client now
// does: submit a prompt the instant it lands, with nothing in between, and
// require the turn to flow.
//
// Before the gate this raced — the ack preceded the shim's attach, so the very
// next command could reach a session controller with no live connection — and the client
// covered it with a health poll. The poll is what this replaces, so its absence
// here is the assertion.
func TestE2ECreateAckMeansThePromptPathIsOpen(t *testing.T) {
	// Arrange
	h := newUDSHarness(t)
	cwd := t.TempDir()
	conn := h.dialFrontend(t)
	defer conn.Close()

	// Act — the ack, then immediately the prompt.
	ack, id := createAck(t, conn, cwd, "e2e-createack-1")
	if !ack.GetOk() {
		t.Fatalf("createSession nacked: %s", ack.GetError())
	}
	if id == "" {
		t.Fatal("createSession acked ok but pushed no SessionView for the new session")
	}
	writeCmd(t, conn, fmt.Sprintf(
		`{"requestId":"e2e-createack-prompt","workspace":%q,"submitPrompt":{"text":"straight after the ack","promptOrigin":"PROMPT_ORIGIN_USER_SENT"}}`, cwd))

	// Assert — the turn flows on the first attempt.
	deadline := time.Now().Add(frameTimeout)
	for time.Now().Before(deadline) {
		frame := readFrame(t, conn)
		if f, ok := frame.GetFrame().(*frontendv1.FrontendFrame_CommandAck); ok {
			if f.CommandAck.GetRequestId() == "e2e-createack-prompt" && !f.CommandAck.GetOk() {
				t.Fatalf("the prompt submitted straight after the ack was refused: %s — the ack promised an established session",
					f.CommandAck.GetError())
			}
		}
		for _, item := range deltaItems(frame, cwd) {
			if strings.Contains(assistantText(item), echoOf("straight after the ack")) {
				return
			}
		}
	}
	t.Fatal("no reply to the prompt submitted straight after the create ack")
}

// TestE2ECreateNacksLoudlyOnAWedgedBringUp is the failure half. The spawned
// process never dials the daemon, so the handshake can never complete, and the
// create must end on its deadline with the pending link NAMED — the one thing a
// bare timeout cannot tell anybody.
func TestE2ECreateNacksLoudlyOnAWedgedBringUp(t *testing.T) {
	// Arrange — a shim that spawns and then does nothing, under a short bound.
	h := newUDSHarness(t, withWedgedShim(), withEstablishTimeout(250*time.Millisecond))
	cwd := t.TempDir()
	conn := h.dialFrontend(t)
	defer conn.Close()

	// Act
	ack, _ := createAck(t, conn, cwd, "e2e-createack-wedged")

	// Assert — a nack, naming the link that never came up.
	if ack.GetOk() {
		t.Fatal("createSession acked ok against a shim that never completed its handshake")
	}
	if !strings.Contains(ack.GetError(), "never completed its handshake") {
		t.Fatalf("nack error = %q, want it to name the pending handshake link", ack.GetError())
	}
	if got := ack.GetFailure().GetErrorType(); got != "shim.handshake_incomplete" {
		t.Fatalf("nack failure type = %q, want shim.handshake_incomplete", got)
	}
}
