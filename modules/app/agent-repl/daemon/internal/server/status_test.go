package server

import (
	"encoding/json"
	"fmt"
	"net/http"
	"os"
	"path/filepath"
	"testing"
	"time"
)

// getStatus fetches GET /sessions/{id}/status, decoding the snapshot and the
// account block the panel renders. Returns the raw response so callers can
// assert the status code.
func (h *harness) getStatus(t *testing.T, sessionID string) (*http.Response, json.RawMessage, map[string]string) {
	t.Helper()
	resp, err := http.Get(h.ts.URL + "/sessions/" + sessionID + "/status")
	if err != nil {
		t.Fatalf("GET status: %v", err)
	}
	t.Cleanup(func() { _ = resp.Body.Close() })
	if resp.StatusCode != http.StatusOK {
		return resp, nil, nil
	}
	var body struct {
		Snapshot json.RawMessage   `json:"snapshot"`
		Account  map[string]string `json:"account"`
	}
	if err := json.NewDecoder(resp.Body).Decode(&body); err != nil {
		t.Fatalf("decode status: %v", err)
	}
	return resp, body.Snapshot, body.Account
}

func TestGetStatusServesTheSnapshotTheInitWarmed(t *testing.T) {
	// Arrange
	h := newHarness(t)
	id, shim := h.createSession(t)
	// Act — the live init IS a status snapshot, so it warms the cache for free.
	shim.pushEvent(t, fmt.Sprintf(`{"type":"system","session_id":%q,"uuid":"u","subtype":"init","data":{"fast_mode_state":"on"}}`, id))
	// Assert — the snapshot crosses a channel, so poll until it lands.
	var snap map[string]any
	deadline := time.Now().Add(recvTimeout)
	for time.Now().Before(deadline) {
		if _, raw, _ := h.getStatus(t, id); len(raw) > 0 && string(raw) != "null" {
			if err := json.Unmarshal(raw, &snap); err != nil {
				t.Fatalf("snapshot is not valid JSON: %v", err)
			}
			break
		}
		time.Sleep(5 * time.Millisecond)
	}
	if snap["fast_mode_state"] != "on" {
		t.Errorf("snapshot = %v, want it to carry the init's fast_mode_state", snap)
	}
}

func TestGetStatusReportsNullSnapshotBeforeInit(t *testing.T) {
	// Arrange
	h := newHarness(t)
	id, _ := h.createSession(t)
	// Act — ask before any init has landed.
	resp, raw, _ := h.getStatus(t, id)
	// Assert — early is not an error; the snapshot is null, which the panel
	// refreshes away when it opens.
	if resp.StatusCode != http.StatusOK {
		t.Fatalf("status = %d, want 200", resp.StatusCode)
	}
	if string(raw) != "null" {
		t.Errorf("snapshot = %s, want null before init", raw)
	}
}

func TestGetStatusIncludesTheLoggedInAccount(t *testing.T) {
	// Arrange — the account block is the one /status section with no supported
	// non-interactive surface, so it is read fresh from the config dir.
	cfgDir := t.TempDir()
	doc := `{"oauthAccount":{"emailAddress":"dodge@chess.com"}}`
	if err := os.WriteFile(filepath.Join(cfgDir, ".claude.json"), []byte(doc), 0o600); err != nil {
		t.Fatalf("write identity: %v", err)
	}
	h, _, _ := loginHarness(t, true)
	id := postCreate(t, h, fmt.Sprintf(`{"cwd":"/w","config_dir":%q}`, cfgDir))
	// Act
	resp, _, acct := h.getStatus(t, id)
	// Assert
	if resp.StatusCode != http.StatusOK {
		t.Fatalf("status = %d, want 200", resp.StatusCode)
	}
	if acct["email"] != "dodge@chess.com" {
		t.Errorf("account email = %q, want dodge@chess.com", acct["email"])
	}
}

func TestGetStatusIs404ForAnUnknownSession(t *testing.T) {
	// Arrange
	h := newHarness(t)
	// Act
	resp, _, _ := h.getStatus(t, "s_nope")
	// Assert
	if resp.StatusCode != http.StatusNotFound {
		t.Errorf("status = %d, want 404", resp.StatusCode)
	}
}

func TestRefreshStatusForwardsTheCommandToTheShim(t *testing.T) {
	// Arrange
	h := newHarness(t)
	id, shim := h.createSession(t)
	// Act
	resp, err := http.Post(h.ts.URL+"/sessions/"+id+"/status/refresh", "application/json", nil)
	if err != nil {
		t.Fatalf("POST refresh: %v", err)
	}
	defer resp.Body.Close()
	// Assert — accepted without waiting, and the shim was actually asked.
	if resp.StatusCode != http.StatusAccepted {
		t.Fatalf("status = %d, want 202", resp.StatusCode)
	}
	select {
	case line := <-shim.sent:
		var cmd struct {
			Type      string `json:"type"`
			RequestID string `json:"request_id"`
		}
		if err := json.Unmarshal(line, &cmd); err != nil {
			t.Fatalf("unmarshal forwarded command: %v", err)
		}
		if cmd.Type != "refresh-status" {
			t.Errorf("forwarded type = %q, want refresh-status", cmd.Type)
		}
		if cmd.RequestID == "" {
			t.Error("forwarded refresh-status carries no request_id, so it could never be acked")
		}
	case <-time.After(recvTimeout):
		t.Fatal("refresh-status never reached the shim")
	}
}

func TestRefreshStatusIs404ForAnUnknownSession(t *testing.T) {
	// Arrange
	h := newHarness(t)
	// Act
	resp, err := http.Post(h.ts.URL+"/sessions/s_nope/status/refresh", "application/json", nil)
	if err != nil {
		t.Fatalf("POST refresh: %v", err)
	}
	defer resp.Body.Close()
	// Assert
	if resp.StatusCode != http.StatusNotFound {
		t.Errorf("status = %d, want 404", resp.StatusCode)
	}
}
