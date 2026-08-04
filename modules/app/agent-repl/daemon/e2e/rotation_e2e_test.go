// The VENDOR SESSION UUID ROTATION, end to end over the REAL processes: the
// fake engine retires its transcript identity mid-turn, the real TS shim
// re-keys its store subscription and bounces the daemon link, the real daemon
// resets its store cursor off the re-handshake's announcement and resubscribes
// from the new seq space's beginning, and the post-rotation conversation
// arrives on the frontend WebSocket.
//
// WHY THE FAKE ROTATES AT ALL. A `/clear` makes the real SDK mint a new
// transcript identity mid-stream and announce it with a fresh `system:init`;
// everything after that point — the turn's own `result` included — belongs to
// the new identity. Nothing else in the fake produces that split, so
// `!rotate` (fake-query.ts runRotateTurn) reproduces it. It simulates the
// VENDOR'S behavior, not any part of the code under test: the shim's detection,
// the re-handshake, the daemon's cursor reset, and the turn reconciliation are
// all exercised for real.
//
// WHY THE CLEAR IS INJECTED. The shim-claude-sidecar is the sole producer of
// ContextCleared and it produces it by tailing a real vendor transcript, which
// does not exist in a `--fake` run — see clearcompact_e2e_test.go's header. The
// injection here writes exactly what the sidecar writes, under the ROTATED
// uuid, which is the identity the whole defect turned on: before this work
// every such event was filed in a seq space the daemon never subscribed to.
//
// These tests share e2e_test.go's package and reuse its helpers READ-ONLY
// (newUDSHarness, dial, readFrame, writeCmd, frameTimeout), clearcompact's
// liveSession / deltaItems / isClear / dialStoreProducer / sidecarClearEvent /
// clearDedupKey, and interrupt_e2e_test.go's awaitAll / assistantText /
// echoOf / workspaceStateFor.
package e2e

import (
	"encoding/json"
	"fmt"
	"net/http"
	"strings"
	"testing"
	"time"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"github.com/gorilla/websocket"
)

// --- reading the daemon's record of the conversation identity ---------------

// vendorSessionID reads the session's claude_session_id off GET /sessions —
// the daemon's own account of which conversation the session is filing under,
// and the value a re-handshake's announcement is compared against.
//
// It WAITS because the rotation is asynchronous end to end (the shim bounces,
// the daemon re-handshakes, and only then is the record rewritten), and it
// takes a `want` predicate rather than a fixed wait so the test names the
// condition it is waiting for instead of a duration.
//
// The wait is driven by the daemon's own vendor-uuid WRITE (h.vendors), not by
// a poll interval: each attempt takes the next-write signal, re-reads
// /sessions, and blocks on that signal if the value has not arrived yet. Taking
// the signal BEFORE the read is what makes it airtight — a write landing
// between the read and the block has already closed the channel the block is
// about to select on, so it cannot be missed.
//
// /sessions remains the value actually inspected. The notification says only
// WHEN to look; what the test asserts on is still the daemon's published
// account of the session, exactly as before.
func vendorSessionID(t *testing.T, h *e2eHarness, sessionID string, want func(string) bool, what string) string {
	t.Helper()
	timeout := time.After(frameTimeout)
	last := ""
	for {
		written := h.vendors.pending()
		last = readVendorSessionID(t, h, sessionID)
		if want(last) {
			return last
		}
		select {
		case <-written:
		case <-timeout:
			t.Fatalf("session %s never reported %s (last claude_session_id = %q)", sessionID, what, last)
			return ""
		}
	}
}

// readVendorSessionID reads sessionID's claude_session_id off GET /sessions
// once, reporting "" when the daemon has not recorded one (or does not know the
// session yet).
func readVendorSessionID(t *testing.T, h *e2eHarness, sessionID string) string {
	t.Helper()
	resp, err := http.Get(h.ts.URL + "/sessions")
	if err != nil {
		t.Fatalf("GET /sessions: %v", err)
	}
	defer resp.Body.Close()
	var body struct {
		Sessions []struct {
			SessionID       string `json:"session_id"`
			ClaudeSessionID string `json:"claude_session_id"`
		} `json:"sessions"`
	}
	if err := json.NewDecoder(resp.Body).Decode(&body); err != nil {
		t.Fatalf("decode /sessions: %v", err)
	}
	for _, s := range body.Sessions {
		if s.SessionID == sessionID {
			return s.ClaudeSessionID
		}
	}
	return ""
}

// --- driving the rotation ---------------------------------------------------

// rotated is what a completed mid-session rotation left behind.
type rotated struct {
	previous string
	next     string
}

// rotateSession runs one ordinary turn (so the daemon has adopted a
// conversation identity for the announcement to DIFFER from), then submits
// `!rotate` and returns once the rotated turn's own reply — written under the
// NEW identity, and therefore only reachable through a reset cursor and a fresh
// subscription — has arrived on the frontend.
func rotateSession(t *testing.T, h *e2eHarness, conn *websocket.Conn, sessionID, cwd string) rotated {
	t.Helper()
	// A turn's worth of PERSISTENT store events is what teaches the daemon the
	// conversation's uuid off the live stream.
	writeCmd(t, conn, `{"requestId":"r-warmup","submitPrompt":{"text":"warmup","promptOrigin":"PROMPT_ORIGIN_USER_SENT"}}`)
	awaitAll(t, conn, nil, map[string]func(*frontendv1.FrontendFrame) bool{
		"the warmup turn's reply": func(frame *frontendv1.FrontendFrame) bool {
			for _, item := range deltaItems(frame, cwd) {
				if strings.Contains(assistantText(item), echoOf("warmup")) {
					return true
				}
			}
			return false
		},
	})
	previous := vendorSessionID(t, h, sessionID, func(id string) bool { return id != "" }, "any conversation identity")

	writeCmd(t, conn, `{"requestId":"r-rotate","submitPrompt":{"text":"!rotate","promptOrigin":"PROMPT_ORIGIN_USER_SENT"}}`)
	awaitAll(t, conn, nil, map[string]func(*frontendv1.FrontendFrame) bool{
		"the rotated turn's own reply": func(frame *frontendv1.FrontendFrame) bool {
			for _, item := range deltaItems(frame, cwd) {
				if strings.Contains(assistantText(item), echoOf("!rotate")) {
					return true
				}
			}
			return false
		},
	})
	next := vendorSessionID(t, h, sessionID, func(id string) bool { return id != "" && id != previous },
		fmt.Sprintf("a conversation identity other than %q", previous))
	return rotated{previous: previous, next: next}
}

// --- tests ------------------------------------------------------------------

// TestE2ERotationDeliversTheRotatedTurnsOwnReply covers THE DEFECT ITSELF. The
// rotated turn's assistant message and result are written under the NEW uuid,
// in a store seq space that starts at 1. Reaching the frontend at all requires
// every part of the chain: the shim noticing the new identity, re-keying, and
// bouncing the daemon link; the daemon resetting its high-water mark off the
// re-handshake's announcement; and the resubscribe asking the new space from
// its beginning. Before this work the reply was filed under a uuid nothing
// subscribed to and simply never appeared.
func TestE2ERotationDeliversTheRotatedTurnsOwnReply(t *testing.T) {
	// Arrange
	// The workspace tempdir is created BEFORE the harness on purpose: cleanups
	// run LIFO, so this ordering tears the harness (and its shim processes)
	// down before the tempdir is removed.
	cwd := t.TempDir()
	h := newUDSHarness(t)
	id, conn, _, _ := liveSession(t, h, cwd)

	// Act
	rot := rotateSession(t, h, conn, id, cwd)

	// Assert — the identity really moved, which is what makes the reply above
	// evidence of a resubscribe rather than of nothing having happened.
	if rot.next == rot.previous || rot.next == "" {
		t.Fatalf("conversation identity = %q, want a rotation away from %q", rot.next, rot.previous)
	}
}

// TestE2ERotationLeavesThinking covers THE STUCK FOOTER. The turn that was
// running when the uuid changed reports its end under the NEW identity, so the
// daemon's `thinking` row has nothing arriving to supersede it until the reset
// cursor lets the new space through. The symptom was a workspace that stayed
// red forever.
//
// The resolved state is read off a FRESH connection's StateSnapshot rather than
// waited for as a pushed frame. The daemon pushes a WorkspaceState only when
// the resolution CHANGES, and the rotation may legitimately settle the turn
// before the rotated reply this test arranges after — so waiting for a push
// would be waiting for a frame the contract does not promise. The snapshot is
// the same fact, asked for directly.
func TestE2ERotationLeavesThinking(t *testing.T) {
	// Arrange — see the cleanup-order note in the first test.
	cwd := t.TempDir()
	h := newUDSHarness(t)
	id, live, _, _ := liveSession(t, h, cwd)
	rotateSession(t, h, live, id, cwd)

	// Act
	fresh := h.dial(t, id)
	snap := readFrame(t, fresh).GetSnapshot()
	if snap == nil {
		t.Fatal("first frame on a fresh connection was not a StateSnapshot")
	}

	// Assert
	var state *frontendv1.WorkspaceState
	for _, ws := range snap.GetWorkspaces() {
		if ws.GetWorkspace() == cwd {
			state = ws
		}
	}
	if state == nil {
		t.Fatalf("snapshot carried no WorkspaceState for %s at all", cwd)
	}
	if state.GetTurnActive() {
		t.Errorf("workspace %s still reports turn_active after the rotation: the rotated turn's end can never arrive under the retired identity", cwd)
	}
	if state.GetState() == frontendv1.RenderState_RENDER_STATE_THINKING {
		t.Errorf("workspace %s = RENDER_STATE_THINKING after the rotation, want any settled state", cwd)
	}
}

// TestE2EPromptAfterRotationRoundTrips covers THE SESSION SURVIVING: the
// controlled re-handshake is not a teardown, so the very next prompt is
// submitted over the re-established link and its reply comes back through the
// NEW seq space.
func TestE2EPromptAfterRotationRoundTrips(t *testing.T) {
	// Arrange — see the cleanup-order note in the first test.
	cwd := t.TempDir()
	h := newUDSHarness(t)
	id, conn, _, _ := liveSession(t, h, cwd)
	rotateSession(t, h, conn, id, cwd)

	// Act
	writeCmd(t, conn, `{"requestId":"r-after","submitPrompt":{"text":"after-the-rotation","promptOrigin":"PROMPT_ORIGIN_USER_SENT"}}`)

	// Assert
	awaitAll(t, conn, nil, map[string]func(*frontendv1.FrontendFrame) bool{
		"the post-rotation turn's reply": func(frame *frontendv1.FrontendFrame) bool {
			for _, item := range deltaItems(frame, cwd) {
				if strings.Contains(assistantText(item), echoOf("after-the-rotation")) {
					return true
				}
			}
			return false
		},
	})
}

// TestE2EClearUnderTheRotatedIdentityReachesTheFrontend covers THE CLEAR LINE
// THAT NEVER RENDERED. A `/clear` is what rotates the uuid in the first place,
// and the sidecar files its ContextCleared under the identity that rotation
// produced — so the clear is always in the NEW seq space. This injects exactly
// what the sidecar writes, under the rotated uuid, and asserts it arrives as
// ConversationItem arm 32.
func TestE2EClearUnderTheRotatedIdentityReachesTheFrontend(t *testing.T) {
	// Arrange — see the cleanup-order note in the first test.
	cwd := t.TempDir()
	h := newUDSHarness(t)
	id, conn, _, store := liveSession(t, h, cwd)
	rot := rotateSession(t, h, conn, id, cwd)
	const lineUUID = "e2e-rotated-clear-1"

	// Act
	store.write(sidecarClearEvent(rot.next, lineUUID))

	// Assert
	awaitAll(t, conn, nil, map[string]func(*frontendv1.FrontendFrame) bool{
		"a ContextCleared item under the rotated identity": func(frame *frontendv1.FrontendFrame) bool {
			for _, item := range deltaItems(frame, cwd) {
				if isClear(item) && item.GetUuid() == clearDedupKey(lineUUID) {
					return true
				}
			}
			return false
		},
	})
}
