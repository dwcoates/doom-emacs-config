package server

// Tests for the workspace sidebar stream surface (shared/protocol.md,
// "Workspace sidebar stream"): snapshot ingest, one-shot fetch, the
// broadcast WebSocket, and the sidebar-action relay.

import (
	"encoding/json"
	"io"
	"net/http"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"github.com/gorilla/websocket"
)

const testSnapshot = `{"type":"workspace-snapshot","sidebar_version":1,"current_ws":"doom","marks":[]}`

// postWorkspaceStatus ingests body as the latest sidebar snapshot.
func (h *harness) postWorkspaceStatus(t *testing.T, body string) *http.Response {
	t.Helper()
	resp, err := http.Post(h.ts.URL+"/workspaces/status", "application/json", strings.NewReader(body))
	if err != nil {
		t.Fatalf("POST /workspaces/status: %v", err)
	}
	t.Cleanup(func() { _ = resp.Body.Close() })
	return resp
}

// dialWorkspaces opens the sidebar stream WebSocket.
func (h *harness) dialWorkspaces(t *testing.T) *websocket.Conn {
	t.Helper()
	wsURL := "ws" + strings.TrimPrefix(h.ts.URL, "http") + "/workspaces/stream"
	conn, resp, err := websocket.DefaultDialer.Dial(wsURL, nil)
	if err != nil {
		t.Fatalf("dial %s: %v", wsURL, err)
	}
	if resp != nil {
		defer resp.Body.Close()
	}
	t.Cleanup(func() { _ = conn.Close() })
	return conn
}

func TestWorkspaceStatusRoundtripsThroughIngest(t *testing.T) {
	// Arrange
	h := newHarness(t)
	// Act
	if resp := h.postWorkspaceStatus(t, testSnapshot); resp.StatusCode != http.StatusNoContent {
		t.Fatalf("ingest status = %d, want 204", resp.StatusCode)
	}
	resp, err := http.Get(h.ts.URL + "/workspaces/status")
	if err != nil {
		t.Fatalf("GET /workspaces/status: %v", err)
	}
	defer resp.Body.Close()
	// Assert — the snapshot comes back verbatim as JSON.
	if resp.StatusCode != http.StatusOK {
		t.Fatalf("status = %d, want 200", resp.StatusCode)
	}
	if got := resp.Header.Get("Content-Type"); got != "application/json" {
		t.Errorf("Content-Type = %q, want application/json", got)
	}
	body, err := io.ReadAll(resp.Body)
	if err != nil {
		t.Fatalf("read body: %v", err)
	}
	if string(body) != testSnapshot {
		t.Errorf("body = %s, want %s", body, testSnapshot)
	}
}

func TestWorkspaceStatusGet404sBeforeAnyIngest(t *testing.T) {
	// Arrange
	h := newHarness(t)
	// Act
	resp, err := http.Get(h.ts.URL + "/workspaces/status")
	if err != nil {
		t.Fatalf("GET /workspaces/status: %v", err)
	}
	defer resp.Body.Close()
	// Assert
	if resp.StatusCode != http.StatusNotFound {
		t.Errorf("status = %d, want 404", resp.StatusCode)
	}
}

func TestWorkspaceStatusIngestRejectsAWrongType(t *testing.T) {
	// Arrange
	h := newHarness(t)
	// Act
	resp := h.postWorkspaceStatus(t, `{"type":"not-a-snapshot"}`)
	// Assert
	if resp.StatusCode != http.StatusBadRequest {
		t.Errorf("status = %d, want 400", resp.StatusCode)
	}
}

func TestWorkspaceStatusIngestRejectsInvalidJSON(t *testing.T) {
	// Arrange
	h := newHarness(t)
	// Act
	resp := h.postWorkspaceStatus(t, `{"type":`)
	// Assert
	if resp.StatusCode != http.StatusBadRequest {
		t.Errorf("status = %d, want 400", resp.StatusCode)
	}
}

func TestWorkspaceStatusIngestRejectsAnOversizedSnapshot(t *testing.T) {
	// Arrange — a valid-looking snapshot padded past the 8 MiB cap.
	h := newHarness(t)
	body := `{"type":"workspace-snapshot","pad":"` + strings.Repeat("a", maxSnapshotBytes) + `"}`
	// Act
	resp := h.postWorkspaceStatus(t, body)
	// Assert
	if resp.StatusCode != http.StatusRequestEntityTooLarge {
		t.Errorf("status = %d, want 413", resp.StatusCode)
	}
}

func TestWorkspaceStreamSendsTheLatestSnapshotOnConnect(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.postWorkspaceStatus(t, testSnapshot)
	// Act
	conn := h.dialWorkspaces(t)
	// Assert
	frame := readFrame(t, conn)
	if frame["type"] != "workspace-snapshot" || frame["current_ws"] != "doom" {
		t.Errorf("frame = %v, want the ingested snapshot", frame)
	}
}

func TestWorkspaceStreamBroadcastsIngestedSnapshots(t *testing.T) {
	// Arrange — connected before any snapshot exists, so the first frame
	// can only come from the broadcast.
	h := newHarness(t)
	conn := h.dialWorkspaces(t)
	// Act
	h.postWorkspaceStatus(t, testSnapshot)
	// Assert
	frame := readFrame(t, conn)
	if frame["type"] != "workspace-snapshot" {
		t.Errorf("frame = %v, want the broadcast snapshot", frame)
	}
}

func TestWorkspaceActionEmitsAFileAndReturnsItsID(t *testing.T) {
	// Arrange
	root := t.TempDir()
	t.Setenv("AGENT_REPL_STATE_DIR", root)
	h := newHarness(t)
	// Act
	resp, err := http.Post(h.ts.URL+"/workspaces/action", "application/json",
		strings.NewReader(`{"action":"send-prompt","targets":["ws-a"],"args":{"prompt":"hi"},"confirmed":true}`))
	if err != nil {
		t.Fatalf("POST /workspaces/action: %v", err)
	}
	defer resp.Body.Close()
	// Assert — 202 carries the minted id and the file landed under the
	// state root with the request's fields plus a stamped ts.
	if resp.StatusCode != http.StatusAccepted {
		t.Fatalf("status = %d, want 202", resp.StatusCode)
	}
	var body struct {
		ID string `json:"id"`
	}
	if err := json.NewDecoder(resp.Body).Decode(&body); err != nil {
		t.Fatalf("decode: %v", err)
	}
	if body.ID == "" {
		t.Fatal("response id is empty")
	}
	path := filepath.Join(root, "sidebar-actions", "sidebar_action_"+body.ID+".json")
	raw, err := os.ReadFile(path)
	if err != nil {
		t.Fatalf("action file %s: %v", path, err)
	}
	var got struct {
		ID        string         `json:"id"`
		Action    string         `json:"action"`
		Targets   []string       `json:"targets"`
		Args      map[string]any `json:"args"`
		Confirmed bool           `json:"confirmed"`
		TS        int64          `json:"ts"`
	}
	if err := json.Unmarshal(raw, &got); err != nil {
		t.Fatalf("unmarshal %s: %v", raw, err)
	}
	if got.ID != body.ID || got.Action != "send-prompt" || !got.Confirmed {
		t.Errorf("file = %+v, want the request's id/action/confirmed", got)
	}
	if len(got.Targets) != 1 || got.Targets[0] != "ws-a" {
		t.Errorf("targets = %v, want [ws-a]", got.Targets)
	}
	if got.Args["prompt"] != "hi" {
		t.Errorf("args = %v, want prompt hi", got.Args)
	}
	if now := time.Now().Unix(); got.TS < now-60 || got.TS > now+60 {
		t.Errorf("ts = %d, want within a minute of %d", got.TS, now)
	}
}

func TestWorkspaceActionRejectsAnEmptyAction(t *testing.T) {
	// Arrange
	t.Setenv("AGENT_REPL_STATE_DIR", t.TempDir())
	h := newHarness(t)
	// Act
	resp, err := http.Post(h.ts.URL+"/workspaces/action", "application/json",
		strings.NewReader(`{"targets":["ws-a"]}`))
	if err != nil {
		t.Fatalf("POST /workspaces/action: %v", err)
	}
	defer resp.Body.Close()
	// Assert
	if resp.StatusCode != http.StatusBadRequest {
		t.Errorf("status = %d, want 400", resp.StatusCode)
	}
}
