// Package e2e exercises the full daemon⇄shim stack end to end: a real
// claude-repld server spawning the real TS shim subprocess (in --fake
// offline mode), driven by a real WebSocket client.
//
// Prerequisite: the shim must be built (`npm run build` in shim/). The
// tests skip with an explicit message when node or shim/dist/main.js is
// unavailable, since they cannot run at all without them.
package e2e

import (
	"bytes"
	"encoding/json"
	"fmt"
	"net/http"
	"net/http/httptest"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"github.com/gorilla/websocket"

	"claude-repld/internal/server"
	"claude-repld/internal/session"
	"claude-repld/internal/shim"
)

const frameTimeout = 15 * time.Second

func shimScriptPath(t *testing.T) string {
	t.Helper()
	path, err := filepath.Abs(filepath.Join("..", "..", "shim", "dist", "main.js"))
	if err != nil {
		t.Fatalf("resolve shim path: %v", err)
	}
	if _, err := os.Stat(path); err != nil {
		t.Skipf("shim not built (%s missing): run `npm run build` in shim/", path)
	}
	return path
}

func nodePath(t *testing.T) string {
	t.Helper()
	node, err := exec.LookPath("node")
	if err != nil {
		t.Skip("node not found in PATH")
	}
	return node
}

type e2eHarness struct {
	ts *httptest.Server
}

func newE2EHarness(t *testing.T) *e2eHarness {
	t.Helper()
	node := nodePath(t)
	script := shimScriptPath(t)
	srv := server.New(server.Config{
		DaemonVersion: "0.1.0-e2e",
		Retention:     256,
		Logf:          t.Logf,
		Spawn: func(sessionID string, opts server.CreateOpts) (session.ShimHandle, error) {
			proc, err := shim.Spawn(shim.Options{
				Argv: []string{node, script, "--fake", "--session-id", sessionID},
				Logf: t.Logf,
			})
			if err != nil {
				return nil, err
			}
			t.Cleanup(func() {
				// Closing stdin is the implicit-shutdown signal: the
				// shim drains and exits, letting Wait reap it.
				_ = proc.CloseStdin()
				_ = proc.Wait()
			})
			return proc, nil
		},
	})
	ts := httptest.NewServer(srv.Handler())
	t.Cleanup(ts.Close)
	return &e2eHarness{ts: ts}
}

func (h *e2eHarness) createSession(t *testing.T) string {
	t.Helper()
	resp, err := http.Post(h.ts.URL+"/sessions", "application/json",
		bytes.NewBufferString(`{"fake":true}`))
	if err != nil {
		t.Fatalf("POST /sessions: %v", err)
	}
	defer resp.Body.Close()
	if resp.StatusCode != http.StatusCreated {
		t.Fatalf("status = %d", resp.StatusCode)
	}
	var body struct {
		SessionID string `json:"session_id"`
	}
	if err := json.NewDecoder(resp.Body).Decode(&body); err != nil {
		t.Fatalf("decode: %v", err)
	}
	return body.SessionID
}

func (h *e2eHarness) dial(t *testing.T, sessionID string) *websocket.Conn {
	t.Helper()
	wsURL := "ws" + strings.TrimPrefix(h.ts.URL, "http") + "/sessions/" + sessionID + "/stream"
	conn, resp, err := websocket.DefaultDialer.Dial(wsURL, nil)
	if err != nil {
		t.Fatalf("dial: %v", err)
	}
	if resp != nil {
		defer resp.Body.Close()
	}
	t.Cleanup(func() { _ = conn.Close() })
	return conn
}

func readFrame(t *testing.T, conn *websocket.Conn) map[string]any {
	t.Helper()
	if err := conn.SetReadDeadline(time.Now().Add(frameTimeout)); err != nil {
		t.Fatalf("deadline: %v", err)
	}
	_, data, err := conn.ReadMessage()
	if err != nil {
		t.Fatalf("read: %v", err)
	}
	var frame map[string]any
	if err := json.Unmarshal(data, &frame); err != nil {
		t.Fatalf("unmarshal %s: %v", data, err)
	}
	return frame
}

// readUntil collects frames until one of type target arrives (inclusive).
func readUntil(t *testing.T, conn *websocket.Conn, target string) []map[string]any {
	t.Helper()
	var frames []map[string]any
	for {
		frame := readFrame(t, conn)
		frames = append(frames, frame)
		if frame["type"] == target {
			return frames
		}
	}
}

func writeCmd(t *testing.T, conn *websocket.Conn, cmd string) {
	t.Helper()
	if err := conn.WriteMessage(websocket.TextMessage, []byte(cmd)); err != nil {
		t.Fatalf("write: %v", err)
	}
}

func frameTypes(frames []map[string]any) []string {
	types := make([]string, len(frames))
	for i, f := range frames {
		types[i] = f["type"].(string)
	}
	return types
}

func hasType(frames []map[string]any, typ string) bool {
	for _, f := range frames {
		if f["type"] == typ {
			return true
		}
	}
	return false
}

func TestE2ETextTurnStreamsThroughTheFullStack(t *testing.T) {
	// Arrange
	h := newE2EHarness(t)
	id := h.createSession(t)
	conn := h.dial(t, id)
	hello := readFrame(t, conn)
	if hello["type"] != "hello" || hello["session_id"] != id {
		t.Fatalf("hello = %v", hello)
	}

	// Act
	writeCmd(t, conn, `{"type":"user-message","request_id":"r1","content":"hello e2e"}`)
	frames := readUntil(t, conn, "result")

	// Assert — the full §2 frame progression arrived.
	for _, want := range []string{"user-turn", "text-start", "text-delta", "text-end", "result"} {
		if !hasType(frames, want) {
			t.Errorf("missing %s frame in %v", want, frameTypes(frames))
		}
	}
	var finalText string
	prevSeq := float64(hello["seq"].(float64))
	for _, f := range frames {
		seq := f["seq"].(float64)
		if seq != prevSeq+1 {
			t.Errorf("seq %v followed %v (must be strictly consecutive)", seq, prevSeq)
		}
		prevSeq = seq
		if f["type"] == "text-end" {
			finalText = f["final_text"].(string)
		}
	}
	if !strings.Contains(finalText, "hello e2e") {
		t.Errorf("final text = %q, want the echo", finalText)
	}
	last := frames[len(frames)-1]
	if last["subtype"] != "success" || last["is_error"] != false {
		t.Errorf("result = %v", last)
	}
}

func TestE2EToolTurnWithPermissionAllow(t *testing.T) {
	// Arrange
	h := newE2EHarness(t)
	id := h.createSession(t)
	conn := h.dial(t, id)
	readFrame(t, conn) // hello

	// Act — the fake SDK's !tool turn raises a real permission round-trip.
	writeCmd(t, conn, `{"type":"user-message","request_id":"r1","content":"!tool echo hi"}`)
	frames := readUntil(t, conn, "permission-request")
	request := frames[len(frames)-1]
	if request["tool_name"] != "Bash" {
		t.Fatalf("permission-request = %v", request)
	}
	preview := request["preview"].(map[string]any)
	if preview["kind"] != "bash" || preview["command"] != "echo hi" {
		t.Fatalf("preview = %v", preview)
	}
	decision := fmt.Sprintf(
		`{"type":"permission-decision","request_id":%q,"decision":{"behavior":"allow"}}`,
		request["request_id"])
	writeCmd(t, conn, decision)
	rest := readUntil(t, conn, "result")

	// Assert — the spec fixes no relative order between the tool-input
	// frames and the permission prompt (they ride different SDK channels),
	// so assert over the whole turn.
	turn := append(append([]map[string]any{}, frames...), rest...)
	for _, want := range []string{"tool-use-start", "tool-use-input-end", "permission-resolved", "tool-use-result"} {
		if !hasType(turn, want) {
			t.Errorf("missing %s frame in turn: %v", want, frameTypes(turn))
		}
	}
	for _, f := range rest {
		if f["type"] == "tool-use-result" {
			if f["is_error"] != false {
				t.Errorf("tool-use-result = %v", f)
			}
			render := f["render"].(map[string]any)
			if render["kind"] != "bash" {
				t.Errorf("render = %v", render)
			}
		}
	}
}

func TestE2EToolTurnWithPermissionDeny(t *testing.T) {
	// Arrange
	h := newE2EHarness(t)
	id := h.createSession(t)
	conn := h.dial(t, id)
	readFrame(t, conn) // hello
	writeCmd(t, conn, `{"type":"user-message","request_id":"r1","content":"!tool rm -rf /"}`)
	frames := readUntil(t, conn, "permission-request")
	request := frames[len(frames)-1]

	// Act
	decision := fmt.Sprintf(
		`{"type":"permission-decision","request_id":%q,"decision":{"behavior":"deny","message":"not today"}}`,
		request["request_id"])
	writeCmd(t, conn, decision)
	rest := readUntil(t, conn, "result")

	// Assert — the tool result reflects the denial.
	for _, f := range rest {
		if f["type"] == "tool-use-result" && f["is_error"] != true {
			t.Errorf("denied tool result should be an error: %v", f)
		}
		if f["type"] == "permission-resolved" && f["decision"] != "deny" {
			t.Errorf("resolved = %v", f)
		}
	}
}

func TestE2EPermissionModeRoundTrip(t *testing.T) {
	// Arrange
	h := newE2EHarness(t)
	id := h.createSession(t)
	conn := h.dial(t, id)
	readFrame(t, conn) // hello

	// Act
	writeCmd(t, conn, `{"type":"set-permission-mode","request_id":"m1","mode":"plan"}`)
	frames := readUntil(t, conn, "permission-mode-changed")

	// Assert — the shim ack propagated back as the §2.8 frame, and the
	// fake SDK observes the mode on the next turn.
	changed := frames[len(frames)-1]
	if changed["mode"] != "plan" {
		t.Fatalf("changed = %v", changed)
	}
	writeCmd(t, conn, `{"type":"user-message","request_id":"r1","content":"check"}`)
	turn := readUntil(t, conn, "result")
	for _, f := range turn {
		if f["type"] == "text-end" && !strings.Contains(f["final_text"].(string), "mode=plan") {
			t.Errorf("echo did not reflect the mode: %v", f["final_text"])
		}
	}
}

func TestE2EReplayReturnsIdenticalFrames(t *testing.T) {
	// Arrange
	h := newE2EHarness(t)
	id := h.createSession(t)
	conn := h.dial(t, id)
	readFrame(t, conn) // hello
	writeCmd(t, conn, `{"type":"user-message","request_id":"r1","content":"replay me"}`)
	original := readUntil(t, conn, "result")

	// Act
	writeCmd(t, conn, `{"type":"replay-request","from_seq":1}`)
	replayed := readUntil(t, conn, "result")

	// Assert — §2.10: original seq and ts preserved.
	if len(replayed) < len(original) {
		t.Fatalf("replayed %d frames, original %d", len(replayed), len(original))
	}
	tail := replayed[len(replayed)-len(original):]
	for i, orig := range original {
		if tail[i]["seq"] != orig["seq"] || tail[i]["ts"] != orig["ts"] || tail[i]["type"] != orig["type"] {
			t.Errorf("replay[%d] = %v, want %v", i, tail[i], orig)
		}
	}
}

func TestE2ESecondTabJoinsAndReplaysHistory(t *testing.T) {
	// Arrange — one tab completes a turn.
	h := newE2EHarness(t)
	id := h.createSession(t)
	conn1 := h.dial(t, id)
	readFrame(t, conn1) // hello
	writeCmd(t, conn1, `{"type":"user-message","request_id":"r1","content":"tab one"}`)
	readUntil(t, conn1, "result")

	// Act — a second tab joins and requests the retained history, as the
	// SPA store does on a mid-conversation hello.
	conn2 := h.dial(t, id)
	hello := readFrame(t, conn2)
	resumeFrom := hello["resume_from_seq"].(float64)
	if resumeFrom != 1 {
		t.Fatalf("resume_from_seq = %v, want 1", resumeFrom)
	}
	writeCmd(t, conn2, fmt.Sprintf(`{"type":"replay-request","from_seq":%d}`, int(resumeFrom)))
	frames := readUntil(t, conn2, "result")

	// Assert — the second tab reconstructed the full turn.
	for _, want := range []string{"user-turn", "text-start", "text-end", "result"} {
		if !hasType(frames, want) {
			t.Errorf("missing %s in second tab's replay: %v", want, frameTypes(frames))
		}
	}
}

func TestE2EDeleteSessionShutsDownCleanly(t *testing.T) {
	// Arrange
	h := newE2EHarness(t)
	id := h.createSession(t)
	conn := h.dial(t, id)
	readFrame(t, conn) // hello

	// Act
	req, err := http.NewRequest(http.MethodDelete, h.ts.URL+"/sessions/"+id, nil)
	if err != nil {
		t.Fatal(err)
	}
	resp, err := http.DefaultClient.Do(req)
	if err != nil {
		t.Fatalf("DELETE: %v", err)
	}
	resp.Body.Close()

	// Assert — the socket closes once the shim drains and exits; a clean
	// shutdown must not surface a shim_died error frame first.
	if err := conn.SetReadDeadline(time.Now().Add(frameTimeout)); err != nil {
		t.Fatal(err)
	}
	for {
		_, data, err := conn.ReadMessage()
		if err != nil {
			return // closed — success
		}
		if strings.Contains(string(data), "shim_died") {
			t.Fatalf("clean shutdown surfaced shim_died: %s", data)
		}
	}
}
