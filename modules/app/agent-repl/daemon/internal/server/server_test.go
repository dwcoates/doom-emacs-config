package server

import (
	"bytes"
	"encoding/json"
	"fmt"
	"net/http"
	"net/http/httptest"
	"os"
	"path/filepath"
	"slices"
	"strings"
	"sync"
	"testing"
	"time"

	"github.com/gorilla/websocket"

	"claude-repld/internal/protocol"
	"claude-repld/internal/registry"
	"claude-repld/internal/session"
)

const recvTimeout = 5 * time.Second

// fakeShim mirrors the in-memory shim used by the session tests.
type fakeShim struct {
	events chan *protocol.L1Event
	sent   chan []byte

	mu           sync.Mutex
	eventsClosed bool
}

func newFakeShim() *fakeShim {
	return &fakeShim{
		events: make(chan *protocol.L1Event, 64),
		sent:   make(chan []byte, 64),
	}
}

func (f *fakeShim) Events() <-chan *protocol.L1Event { return f.events }

func (f *fakeShim) SendRaw(line []byte) error {
	f.sent <- line
	return nil
}

func (f *fakeShim) Send(cmd any) error {
	line, err := protocol.EncodeNDJSON(cmd)
	if err != nil {
		return err
	}
	return f.SendRaw(line)
}

func (f *fakeShim) end() {
	f.mu.Lock()
	defer f.mu.Unlock()
	if !f.eventsClosed {
		f.eventsClosed = true
		close(f.events)
	}
}

func (f *fakeShim) pushEvent(t *testing.T, raw string) {
	t.Helper()
	e, err := protocol.DecodeL1Event([]byte(raw))
	if err != nil || e == nil {
		t.Fatalf("bad test event %s: %v", raw, err)
	}
	f.events <- e
}

type harness struct {
	ts    *httptest.Server
	srv   *Server
	shims chan *fakeShim
}

func newHarness(t *testing.T) *harness {
	t.Helper()
	return newHarnessWith(t, nil)
}

// newHarnessWith builds a harness whose daemon dispatches "session gone"
// remediation through remediator (nil = the capability is unconfigured).
func newHarnessWith(t *testing.T, remediator Remediator) *harness {
	t.Helper()
	shims := make(chan *fakeShim, 8)
	srv := New(Config{
		DaemonVersion: "0.1.0-test",
		Retention:     64,
		Logf:          func(string, ...any) {},
		Spawn: func(sessionID string, opts CreateOpts) (session.ShimHandle, error) {
			shim := newFakeShim()
			shims <- shim
			return shim, nil
		},
		Remediator: remediator,
	})
	ts := httptest.NewServer(srv.Handler())
	t.Cleanup(ts.Close)
	return &harness{ts: ts, srv: srv, shims: shims}
}

// fakeRemediator records the ids remediation was dispatched for.
type fakeRemediator struct {
	mu   sync.Mutex
	ids  []string
	err  error
	stop bool // report the dedupe no-op instead of a launch
}

func (f *fakeRemediator) Start(sessionID string) (bool, error) {
	f.mu.Lock()
	defer f.mu.Unlock()
	if f.err != nil {
		return false, f.err
	}
	f.ids = append(f.ids, sessionID)
	return !f.stop, nil
}

func (f *fakeRemediator) dispatched() []string {
	f.mu.Lock()
	defer f.mu.Unlock()
	return slices.Clone(f.ids)
}

// postRemediation asks the daemon to remediate sessionID.
func (h *harness) postRemediation(t *testing.T, sessionID string) *http.Response {
	t.Helper()
	body := fmt.Sprintf(`{"session_id":%q}`, sessionID)
	resp, err := http.Post(h.ts.URL+"/remediation", "application/json", strings.NewReader(body))
	if err != nil {
		t.Fatalf("POST /remediation: %v", err)
	}
	t.Cleanup(func() { _ = resp.Body.Close() })
	return resp
}

func (h *harness) createSession(t *testing.T) (string, *fakeShim) {
	t.Helper()
	resp, err := http.Post(h.ts.URL+"/sessions", "application/json", bytes.NewBufferString(`{}`))
	if err != nil {
		t.Fatalf("POST /sessions: %v", err)
	}
	defer resp.Body.Close()
	if resp.StatusCode != http.StatusCreated {
		t.Fatalf("status = %d, want 201", resp.StatusCode)
	}
	var body struct {
		SessionID string `json:"session_id"`
		StreamURL string `json:"stream_url"`
	}
	if err := json.NewDecoder(resp.Body).Decode(&body); err != nil {
		t.Fatalf("decode: %v", err)
	}
	select {
	case shim := <-h.shims:
		t.Cleanup(shim.end)
		return body.SessionID, shim
	case <-time.After(recvTimeout):
		t.Fatal("spawn was not invoked")
		return "", nil
	}
}

func (h *harness) dial(t *testing.T, sessionID string) *websocket.Conn {
	t.Helper()
	wsURL := "ws" + strings.TrimPrefix(h.ts.URL, "http") + "/sessions/" + sessionID + "/stream"
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

func readFrame(t *testing.T, conn *websocket.Conn) map[string]any {
	t.Helper()
	if err := conn.SetReadDeadline(time.Now().Add(recvTimeout)); err != nil {
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

// --- session CRUD -----------------------------------------------------------

func TestCreateSessionReturnsIDAndStreamURL(t *testing.T) {
	// Arrange
	h := newHarness(t)
	// Act
	id, _ := h.createSession(t)
	// Assert
	if id == "" {
		t.Fatal("empty session id")
	}
}

func TestCreateSessionRejectsInvalidPermissionMode(t *testing.T) {
	// Arrange
	h := newHarness(t)
	// Act
	resp, err := http.Post(h.ts.URL+"/sessions", "application/json",
		bytes.NewBufferString(`{"permission_mode":"yolo"}`))
	// Assert
	if err != nil {
		t.Fatalf("POST: %v", err)
	}
	defer resp.Body.Close()
	if resp.StatusCode != http.StatusBadRequest {
		t.Errorf("status = %d, want 400", resp.StatusCode)
	}
}

func TestListSessionsIncludesCreatedSession(t *testing.T) {
	// Arrange
	h := newHarness(t)
	id, _ := h.createSession(t)
	// Act
	resp, err := http.Get(h.ts.URL + "/sessions")
	// Assert
	if err != nil {
		t.Fatalf("GET: %v", err)
	}
	defer resp.Body.Close()
	var body struct {
		Sessions []struct {
			SessionID string `json:"session_id"`
		} `json:"sessions"`
	}
	if err := json.NewDecoder(resp.Body).Decode(&body); err != nil {
		t.Fatalf("decode: %v", err)
	}
	if len(body.Sessions) != 1 || body.Sessions[0].SessionID != id {
		t.Errorf("sessions = %+v", body.Sessions)
	}
}

func TestListSessionsCarriesIntrospectionFields(t *testing.T) {
	// Arrange — create with explicit cwd/model, then feed an init that
	// carries the durable CLI session uuid.
	h := newHarness(t)
	resp, err := http.Post(h.ts.URL+"/sessions", "application/json",
		bytes.NewBufferString(`{"cwd":"/req/cwd","model":"haiku"}`))
	if err != nil {
		t.Fatalf("POST /sessions: %v", err)
	}
	defer resp.Body.Close()
	var created struct {
		SessionID string `json:"session_id"`
	}
	if err := json.NewDecoder(resp.Body).Decode(&created); err != nil {
		t.Fatalf("decode: %v", err)
	}
	shim := <-h.shims
	t.Cleanup(shim.end)
	// Synchronize WITHOUT sleeping: the translator mutates its mirror
	// under the session lock BEFORE the frame is broadcast, so once a
	// WS client has received the init's system frame, the list snapshot
	// is guaranteed current.
	conn := h.dial(t, created.SessionID)
	readFrame(t, conn) // hello
	shim.pushEvent(t, `{"type":"system","session_id":"`+created.SessionID+`","uuid":"u","subtype":"init","data":{"session_id":"cli-uuid-9"}}`)
	readFrame(t, conn) // the init system frame: state is now visible
	// Act
	listResp, err := http.Get(h.ts.URL + "/sessions")
	if err != nil {
		t.Fatalf("GET: %v", err)
	}
	defer listResp.Body.Close()
	var body struct {
		Sessions []struct {
			SessionID       string `json:"session_id"`
			CWD             string `json:"cwd"`
			Model           string `json:"model"`
			ClaudeSessionID string `json:"claude_session_id"`
		} `json:"sessions"`
	}
	if err := json.NewDecoder(listResp.Body).Decode(&body); err != nil {
		t.Fatalf("decode list: %v", err)
	}
	// Assert
	if len(body.Sessions) != 1 {
		t.Fatalf("sessions = %+v", body.Sessions)
	}
	e := body.Sessions[0]
	if e.CWD != "/req/cwd" || e.Model != "haiku" || e.ClaudeSessionID != "cli-uuid-9" {
		t.Errorf("entry = %+v", e)
	}
}

func TestDeleteSessionSendsShutdownToShim(t *testing.T) {
	// Arrange
	h := newHarness(t)
	id, shim := h.createSession(t)
	req, err := http.NewRequest(http.MethodDelete, h.ts.URL+"/sessions/"+id, nil)
	if err != nil {
		t.Fatal(err)
	}
	// Act
	resp, err := http.DefaultClient.Do(req)
	// Assert
	if err != nil {
		t.Fatalf("DELETE: %v", err)
	}
	defer resp.Body.Close()
	if resp.StatusCode != http.StatusNoContent {
		t.Errorf("status = %d, want 204", resp.StatusCode)
	}
	select {
	case line := <-shim.sent:
		if !strings.Contains(string(line), `"shutdown"`) {
			t.Errorf("sent = %s", line)
		}
	case <-time.After(recvTimeout):
		t.Fatal("no shutdown forwarded to shim")
	}
}

func TestUnknownSessionRoutesReturn404(t *testing.T) {
	// Arrange
	h := newHarness(t)
	// Act
	resp, err := http.Get(h.ts.URL + "/sessions/nope/stream")
	// Assert
	if err != nil {
		t.Fatalf("GET: %v", err)
	}
	defer resp.Body.Close()
	if resp.StatusCode != http.StatusNotFound {
		t.Errorf("status = %d, want 404", resp.StatusCode)
	}
}

// --- websocket stream ---------------------------------------------------------

func TestStreamSendsHelloFirst(t *testing.T) {
	// Arrange
	h := newHarness(t)
	id, _ := h.createSession(t)
	// Act
	conn := h.dial(t, id)
	// Assert
	hello := readFrame(t, conn)
	if hello["type"] != "hello" || hello["session_id"] != id || hello["daemon_version"] != "0.1.0-test" {
		t.Errorf("hello = %v", hello)
	}
}

func TestStreamBroadcastsShimFrames(t *testing.T) {
	// Arrange
	h := newHarness(t)
	id, shim := h.createSession(t)
	conn := h.dial(t, id)
	readFrame(t, conn) // hello
	// Act
	shim.pushEvent(t, fmt.Sprintf(`{"type":"system","session_id":%q,"uuid":"u","subtype":"init","data":{"model":"m"}}`, id))
	// Assert
	frame := readFrame(t, conn)
	if frame["type"] != "system" || frame["seq"] != float64(1) {
		t.Errorf("frame = %v", frame)
	}
}

func TestStreamForwardsClientCommandsToShim(t *testing.T) {
	// Arrange
	h := newHarness(t)
	id, shim := h.createSession(t)
	conn := h.dial(t, id)
	readFrame(t, conn) // hello
	// Act
	if err := conn.WriteMessage(websocket.TextMessage,
		[]byte(`{"type":"user-message","request_id":"r1","content":"hi"}`)); err != nil {
		t.Fatalf("write: %v", err)
	}
	// Assert — user-turn broadcast comes back and the command reaches the shim.
	turn := readFrame(t, conn)
	if turn["type"] != "user-turn" {
		t.Errorf("turn = %v", turn)
	}
	select {
	case line := <-shim.sent:
		if !strings.Contains(string(line), `"user-message"`) {
			t.Errorf("forwarded = %s", line)
		}
	case <-time.After(recvTimeout):
		t.Fatal("command never reached the shim")
	}
}

func TestStreamReplayRequestOverWebSocket(t *testing.T) {
	// Arrange
	h := newHarness(t)
	id, shim := h.createSession(t)
	conn := h.dial(t, id)
	readFrame(t, conn) // hello
	shim.pushEvent(t, fmt.Sprintf(`{"type":"system","session_id":%q,"uuid":"u","subtype":"init","data":{}}`, id))
	readFrame(t, conn) // seq 1
	// Act
	if err := conn.WriteMessage(websocket.TextMessage, []byte(`{"type":"replay-request","from_seq":1}`)); err != nil {
		t.Fatalf("write: %v", err)
	}
	// Assert — the retained frame is re-sent with its original seq.
	frame := readFrame(t, conn)
	if frame["type"] != "system" || frame["seq"] != float64(1) {
		t.Errorf("frame = %v", frame)
	}
}

func TestStreamSecondTabConvergesOnSameFrames(t *testing.T) {
	// Arrange
	h := newHarness(t)
	id, shim := h.createSession(t)
	conn1 := h.dial(t, id)
	conn2 := h.dial(t, id)
	readFrame(t, conn1) // hello
	readFrame(t, conn2) // hello
	// Act
	shim.pushEvent(t, fmt.Sprintf(`{"type":"system","session_id":%q,"uuid":"u","subtype":"init","data":{}}`, id))
	// Assert
	f1 := readFrame(t, conn1)
	f2 := readFrame(t, conn2)
	if f1["seq"] != f2["seq"] || f1["type"] != f2["type"] {
		t.Errorf("tabs diverged: %v vs %v", f1, f2)
	}
}

func TestStreamSocketClosesWhenSessionEnds(t *testing.T) {
	// Arrange
	h := newHarness(t)
	id, shim := h.createSession(t)
	conn := h.dial(t, id)
	readFrame(t, conn) // hello
	// Act — shim dies; session broadcasts shim_died then closes clients.
	shim.end()
	// Assert
	frame := readFrame(t, conn)
	if frame["type"] != "error" || frame["code"] != "shim_died" {
		t.Errorf("frame = %v", frame)
	}
	if err := conn.SetReadDeadline(time.Now().Add(recvTimeout)); err != nil {
		t.Fatal(err)
	}
	if _, _, err := conn.ReadMessage(); err == nil {
		t.Error("socket should close after the session ends")
	}
}

func TestShutdownAllReachesEverySession(t *testing.T) {
	// Arrange
	h := newHarness(t)
	_, shim1 := h.createSession(t)
	_, shim2 := h.createSession(t)
	// Act
	h.srv.ShutdownAll()
	// Assert
	for i, shim := range []*fakeShim{shim1, shim2} {
		select {
		case line := <-shim.sent:
			if !strings.Contains(string(line), `"shutdown"`) {
				t.Errorf("shim%d sent = %s", i+1, line)
			}
		case <-time.After(recvTimeout):
			t.Fatalf("shim%d never received shutdown", i+1)
		}
	}
}

func TestShimArgvAssemblesAllCreateOpts(t *testing.T) {
	// Arrange
	cases := []struct {
		name      string
		forceFake bool
		opts      CreateOpts
		want      []string
	}{
		{
			name: "defaults",
			opts: CreateOpts{},
			want: []string{"node", "shim.js", "--session-id", "s1"},
		},
		{
			name:      "force fake wins over opts",
			forceFake: true,
			opts:      CreateOpts{Fake: false},
			want:      []string{"node", "shim.js", "--session-id", "s1", "--fake"},
		},
		{
			name: "full opts",
			opts: CreateOpts{Fake: true, PermissionMode: "plan", CWD: "/w", Model: "haiku", Resume: "cli-1"},
			want: []string{"node", "shim.js", "--session-id", "s1", "--fake",
				"--permission-mode", "plan", "--cwd", "/w", "--model", "haiku", "--resume", "cli-1"},
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := ShimArgv("node", "shim.js", "s1", tc.forceFake, tc.opts)
			// Assert
			if !slices.Equal(got, tc.want) {
				t.Errorf("argv = %v, want %v", got, tc.want)
			}
		})
	}
}

// --- HTTP message / interrupt injection ---------------------------------------

func TestSendMessageRouteBroadcastsUserTurnAndForwards(t *testing.T) {
	// Arrange — a WS tab is attached; the message arrives over HTTP.
	h := newHarness(t)
	id, shim := h.createSession(t)
	conn := h.dial(t, id)
	readFrame(t, conn) // hello
	// Act
	resp, err := http.Post(h.ts.URL+"/sessions/"+id+"/message", "application/json",
		bytes.NewBufferString(`{"content":"hello from emacs"}`))
	if err != nil {
		t.Fatalf("POST message: %v", err)
	}
	defer resp.Body.Close()
	// Assert — 202 with a minted request id, user-turn broadcast, shim forward.
	if resp.StatusCode != http.StatusAccepted {
		t.Fatalf("status = %d, want 202", resp.StatusCode)
	}
	var body struct {
		RequestID string `json:"request_id"`
	}
	if err := json.NewDecoder(resp.Body).Decode(&body); err != nil {
		t.Fatalf("decode: %v", err)
	}
	if !strings.HasPrefix(body.RequestID, "r_") {
		t.Errorf("request_id = %q", body.RequestID)
	}
	turn := readFrame(t, conn)
	if turn["type"] != "user-turn" || turn["request_id"] != body.RequestID {
		t.Errorf("turn = %v", turn)
	}
	select {
	case line := <-shim.sent:
		if !strings.Contains(string(line), `"user-message"`) ||
			!strings.Contains(string(line), "hello from emacs") {
			t.Errorf("forwarded = %s", line)
		}
	case <-time.After(recvTimeout):
		t.Fatal("message not forwarded to shim")
	}
}

func TestSendMessageRouteRejectsEmptyContent(t *testing.T) {
	// Arrange
	h := newHarness(t)
	id, _ := h.createSession(t)
	// Act
	resp, err := http.Post(h.ts.URL+"/sessions/"+id+"/message", "application/json",
		bytes.NewBufferString(`{"content":"   "}`))
	if err != nil {
		t.Fatalf("POST: %v", err)
	}
	defer resp.Body.Close()
	// Assert
	if resp.StatusCode != http.StatusBadRequest {
		t.Errorf("status = %d, want 400", resp.StatusCode)
	}
}

func TestSendMessageRoute404sOnUnknownSession(t *testing.T) {
	// Arrange
	h := newHarness(t)
	// Act
	resp, err := http.Post(h.ts.URL+"/sessions/nope/message", "application/json",
		bytes.NewBufferString(`{"content":"x"}`))
	if err != nil {
		t.Fatalf("POST: %v", err)
	}
	defer resp.Body.Close()
	// Assert
	if resp.StatusCode != http.StatusNotFound {
		t.Errorf("status = %d, want 404", resp.StatusCode)
	}
}

func TestInterruptRouteForwardsToShim(t *testing.T) {
	// Arrange
	h := newHarness(t)
	id, shim := h.createSession(t)
	// Act
	resp, err := http.Post(h.ts.URL+"/sessions/"+id+"/interrupt", "application/json", nil)
	if err != nil {
		t.Fatalf("POST interrupt: %v", err)
	}
	defer resp.Body.Close()
	// Assert
	if resp.StatusCode != http.StatusAccepted {
		t.Fatalf("status = %d, want 202", resp.StatusCode)
	}
	select {
	case line := <-shim.sent:
		if !strings.Contains(string(line), `"interrupt"`) {
			t.Errorf("forwarded = %s", line)
		}
	case <-time.After(recvTimeout):
		t.Fatal("interrupt not forwarded to shim")
	}
}

// --- resume viability gate ---------------------------------------------------

// resumeGateHarness is a harness whose spawn captures CreateOpts, for
// asserting what the shim would actually be launched with.
func resumeGateHarness(t *testing.T) (*harness, *CreateOpts) {
	t.Helper()
	shims := make(chan *fakeShim, 8)
	var captured CreateOpts
	srv := New(Config{
		DaemonVersion: "0.1.0-test",
		Retention:     64,
		Logf:          func(string, ...any) {},
		Spawn: func(sessionID string, opts CreateOpts) (session.ShimHandle, error) {
			captured = opts
			shim := newFakeShim()
			shims <- shim
			return shim, nil
		},
	})
	ts := httptest.NewServer(srv.Handler())
	t.Cleanup(ts.Close)
	return &harness{ts: ts, srv: srv, shims: shims}, &captured
}

func postCreate(t *testing.T, h *harness, body string) string {
	t.Helper()
	resp, err := http.Post(h.ts.URL+"/sessions", "application/json", bytes.NewBufferString(body))
	if err != nil {
		t.Fatalf("POST /sessions: %v", err)
	}
	defer resp.Body.Close()
	if resp.StatusCode != http.StatusCreated {
		t.Fatalf("status = %d, want 201", resp.StatusCode)
	}
	var out struct {
		SessionID string `json:"session_id"`
	}
	if err := json.NewDecoder(resp.Body).Decode(&out); err != nil {
		t.Fatalf("decode: %v", err)
	}
	select {
	case shim := <-h.shims:
		t.Cleanup(shim.end)
	case <-time.After(recvTimeout):
		t.Fatal("spawn was not invoked")
	}
	return out.SessionID
}

func TestCreateSessionDropsUnresumableResume(t *testing.T) {
	// Arrange: a config dir with no transcript for the resume target.
	t.Setenv("CLAUDE_CONFIG_DIR", t.TempDir())
	h, captured := resumeGateHarness(t)
	// Act
	id := postCreate(t, h, `{"cwd":"/w","resume":"uuid-gone"}`)
	// Assert: the shim was spawned WITHOUT the doomed --resume.
	if captured.Resume != "" {
		t.Fatalf("spawn opts.Resume = %q, want dropped (empty)", captured.Resume)
	}
	// And the drop is visible in-band as a recoverable error frame.
	conn := h.dial(t, id)
	hello := readFrame(t, conn)
	if hello["claude_session_id"] != nil && hello["claude_session_id"] != "" {
		t.Fatalf("hello claude_session_id = %v, want empty for a fresh session", hello["claude_session_id"])
	}
	req, _ := json.Marshal(map[string]any{"type": "replay-request", "from_seq": hello["resume_from_seq"]})
	if err := conn.WriteMessage(websocket.TextMessage, req); err != nil {
		t.Fatalf("replay-request: %v", err)
	}
	frame := readFrame(t, conn)
	if frame["type"] != "error" || frame["code"] != "resume_unavailable" || frame["recoverable"] != true {
		t.Fatalf("frame = %v, want recoverable resume_unavailable error", frame)
	}
}

func TestCreateSessionKeepsResumableResume(t *testing.T) {
	// Arrange: a config dir WITH the resume target's transcript.
	cfg := t.TempDir()
	dir := cfg + "/projects/-w"
	if err := os.MkdirAll(dir, 0o755); err != nil {
		t.Fatal(err)
	}
	transcript := `{"type":"user","message":{"role":"user","content":"hello"}}` + "\n"
	if err := os.WriteFile(dir+"/uuid-1.jsonl", []byte(transcript), 0o600); err != nil {
		t.Fatal(err)
	}
	t.Setenv("CLAUDE_CONFIG_DIR", cfg)
	h, captured := resumeGateHarness(t)
	// Act
	id := postCreate(t, h, `{"cwd":"/w","resume":"uuid-1"}`)
	// Assert: resume passed through and identity stamped.
	if captured.Resume != "uuid-1" {
		t.Fatalf("spawn opts.Resume = %q, want uuid-1", captured.Resume)
	}
	conn := h.dial(t, id)
	hello := readFrame(t, conn)
	if hello["claude_session_id"] != "uuid-1" {
		t.Fatalf("hello claude_session_id = %v, want uuid-1", hello["claude_session_id"])
	}
}

// ---- POST /remediation ("session gone" analyst dispatch) ------------------

func TestRemediationDispatchesForAVanishedSession(t *testing.T) {
	// Arrange — an id the daemon has never heard of, exactly as the
	// webapp's existence probe reports when it goes "session gone".
	rem := &fakeRemediator{}
	h := newHarnessWith(t, rem)
	// Act
	resp := h.postRemediation(t, "s_ghost")
	// Assert
	if resp.StatusCode != http.StatusAccepted {
		t.Fatalf("status = %d, want 202", resp.StatusCode)
	}
	if got := rem.dispatched(); !slices.Equal(got, []string{"s_ghost"}) {
		t.Fatalf("dispatched = %v, want [s_ghost]", got)
	}
}

func TestRemediationReportsWhetherItLaunchedTheAnalyst(t *testing.T) {
	// Arrange — a repeat request for an already-dispatched session.
	rem := &fakeRemediator{stop: true}
	h := newHarnessWith(t, rem)
	// Act
	resp := h.postRemediation(t, "s_ghost")
	// Assert
	var body struct {
		Started bool `json:"started"`
	}
	if err := json.NewDecoder(resp.Body).Decode(&body); err != nil {
		t.Fatalf("decode: %v", err)
	}
	if body.Started {
		t.Fatal("started = true, want false for the dedupe no-op")
	}
}

func TestRemediationRefusesALiveSession(t *testing.T) {
	// Arrange — a session the daemon still serves is not gone.
	rem := &fakeRemediator{}
	h := newHarnessWith(t, rem)
	id, _ := h.createSession(t)
	// Act
	resp := h.postRemediation(t, id)
	// Assert
	if resp.StatusCode != http.StatusConflict {
		t.Fatalf("status = %d, want 409", resp.StatusCode)
	}
	if got := rem.dispatched(); len(got) != 0 {
		t.Fatalf("dispatched = %v, want no analyst for a live session", got)
	}
}

func TestRemediationRejectsAnEmptySessionId(t *testing.T) {
	// Arrange
	h := newHarnessWith(t, &fakeRemediator{})
	// Act
	resp := h.postRemediation(t, "")
	// Assert
	if resp.StatusCode != http.StatusBadRequest {
		t.Fatalf("status = %d, want 400", resp.StatusCode)
	}
}

func TestRemediationSurfacesADispatchFailure(t *testing.T) {
	// Arrange
	h := newHarnessWith(t, &fakeRemediator{err: fmt.Errorf("claude not on PATH")})
	// Act
	resp := h.postRemediation(t, "s_ghost")
	// Assert
	if resp.StatusCode != http.StatusInternalServerError {
		t.Fatalf("status = %d, want 500", resp.StatusCode)
	}
}

func TestRemediationReportsAnUnconfiguredRunner(t *testing.T) {
	// Arrange — no Remediator wired into the daemon.
	h := newHarness(t)
	// Act
	resp := h.postRemediation(t, "s_ghost")
	// Assert
	if resp.StatusCode != http.StatusServiceUnavailable {
		t.Fatalf("status = %d, want 503", resp.StatusCode)
	}
}

// --- boot identity -----------------------------------------------------------

func TestListSessionsEnvelopeCarriesBootIdentity(t *testing.T) {
	// Arrange
	h := newHarness(t)
	// Act
	resp, err := http.Get(h.ts.URL + "/sessions")
	if err != nil {
		t.Fatalf("GET /sessions: %v", err)
	}
	defer resp.Body.Close()
	var body struct {
		BootID          string `json:"boot_id"`
		ProtocolVersion int    `json:"protocol_version"`
	}
	if err := json.NewDecoder(resp.Body).Decode(&body); err != nil {
		t.Fatalf("decode: %v", err)
	}
	// Assert
	if !strings.HasPrefix(body.BootID, "b_") || len(body.BootID) < 8 {
		t.Fatalf("boot_id = %q, want b_<hex>", body.BootID)
	}
	if body.ProtocolVersion != protocol.Layer2Version {
		t.Fatalf("protocol_version = %d, want %d", body.ProtocolVersion, protocol.Layer2Version)
	}
}

func TestHelloCarriesServerBootID(t *testing.T) {
	// Arrange: two sessions in one server share the boot id.
	h := newHarness(t)
	idA, _ := h.createSession(t)
	idB, _ := h.createSession(t)
	// Act
	helloA := readFrame(t, h.dial(t, idA))
	helloB := readFrame(t, h.dial(t, idB))
	// Assert
	bootA, _ := helloA["boot_id"].(string)
	if !strings.HasPrefix(bootA, "b_") {
		t.Fatalf("hello boot_id = %v, want b_<hex>", helloA["boot_id"])
	}
	if helloB["boot_id"] != bootA {
		t.Fatalf("boot ids differ across sessions of one server: %v vs %v", bootA, helloB["boot_id"])
	}
	if int(helloA["protocol_version"].(float64)) != protocol.Layer2Version {
		t.Fatalf("hello protocol_version = %v, want %d", helloA["protocol_version"], protocol.Layer2Version)
	}
}

// ---- session registry write-through ----------------------------------------

// registryHarness builds a harness whose server persists session records
// into a fresh on-disk registry rooted in a test temp dir.
func registryHarness(t *testing.T) (*harness, *registry.Registry) {
	t.Helper()
	reg := registry.Open(filepath.Join(t.TempDir(), "sessions.json"), func(string, ...any) {})
	shims := make(chan *fakeShim, 8)
	srv := New(Config{
		DaemonVersion: "0.1.0-test",
		Retention:     64,
		Logf:          func(string, ...any) {},
		Now:           func() time.Time { return time.Date(2026, 7, 12, 10, 0, 0, 0, time.UTC) },
		Spawn: func(sessionID string, opts CreateOpts) (session.ShimHandle, error) {
			shim := newFakeShim()
			shims <- shim
			return shim, nil
		},
		Registry: reg,
	})
	ts := httptest.NewServer(srv.Handler())
	t.Cleanup(ts.Close)
	drainSessionsOnCleanup(t, srv)
	return &harness{ts: ts, srv: srv, shims: shims}, reg
}

// drainSessionsOnCleanup waits for every session's Run goroutine to
// finish before the test's temp dir is removed. Registrar write-through
// happens on those goroutines, so without the wait a session reacting to
// its shim's death can still be writing the registry (and its lock file)
// while t.TempDir's RemoveAll is deleting the directory — a flake, and a
// misleading one. Cleanups run LIFO, so this (registered after t.TempDir,
// before any shim's own cleanup) lands exactly between the two.
func drainSessionsOnCleanup(t *testing.T, srv *Server) {
	t.Helper()
	t.Cleanup(func() {
		srv.mu.Lock()
		sessions := make([]*session.Session, 0, len(srv.sessions))
		for _, sess := range srv.sessions {
			sessions = append(sessions, sess)
		}
		srv.mu.Unlock()
		for _, sess := range sessions {
			select {
			case <-sess.Done():
			case <-time.After(recvTimeout):
				t.Errorf("session %s never drained; a registry write may race cleanup", sess.ID)
			}
		}
	})
}

// awaitSocketClose reads until the WebSocket closes, which strictly
// follows the session's registrar notifications (the session closes
// client channels only after notifying) — a sleep-free sync point.
func awaitSocketClose(t *testing.T, conn *websocket.Conn) {
	t.Helper()
	if err := conn.SetReadDeadline(time.Now().Add(recvTimeout)); err != nil {
		t.Fatal(err)
	}
	for {
		if _, _, err := conn.ReadMessage(); err != nil {
			return
		}
	}
}

func TestCreateSessionPersistsARegistryRecord(t *testing.T) {
	// Arrange
	h, reg := registryHarness(t)
	// Act
	resp, err := http.Post(h.ts.URL+"/sessions", "application/json",
		bytes.NewBufferString(`{"cwd":"/w","model":"haiku","permission_mode":"plan"}`))
	if err != nil {
		t.Fatalf("POST /sessions: %v", err)
	}
	defer resp.Body.Close()
	var created struct {
		SessionID string `json:"session_id"`
	}
	if err := json.NewDecoder(resp.Body).Decode(&created); err != nil {
		t.Fatalf("decode: %v", err)
	}
	shim := <-h.shims
	t.Cleanup(shim.end)
	// Assert — the record is on disk before the create response returns.
	rec, ok := reg.Get(created.SessionID)
	if !ok {
		t.Fatalf("no registry record for %s", created.SessionID)
	}
	if rec.CWD != "/w" || rec.Model != "haiku" || rec.PermissionMode != "plan" ||
		rec.CreatedAt != "2026-07-12T10:00:00Z" || rec.Terminal {
		t.Errorf("record = %+v", rec)
	}
}

func TestSystemInitWritesClaudeSessionIDThroughToRegistry(t *testing.T) {
	// Arrange
	h, reg := registryHarness(t)
	id, shim := h.createSession(t)
	conn := h.dial(t, id)
	readFrame(t, conn) // hello
	// Act — init supplies the uuid; the FILLER event that follows is the
	// sync point (Run handles events serially, so receiving the filler's
	// frame proves the init iteration, registrar write included, is done).
	shim.pushEvent(t, `{"type":"system","session_id":"`+id+`","uuid":"u1","subtype":"init","data":{"session_id":"cli-uuid-42"}}`)
	shim.pushEvent(t, `{"type":"system","session_id":"`+id+`","uuid":"u2","subtype":"slash_command","data":{}}`)
	readFrame(t, conn) // init frame
	readFrame(t, conn) // filler frame: init iteration fully processed
	// Assert
	rec, ok := reg.Get(id)
	if !ok || rec.ClaudeSessionID != "cli-uuid-42" {
		t.Errorf("record = %+v, ok=%v", rec, ok)
	}
}

func TestSessionEndMarksRegistryRecordTerminal(t *testing.T) {
	// Arrange
	h, reg := registryHarness(t)
	id, shim := h.createSession(t)
	conn := h.dial(t, id)
	readFrame(t, conn) // hello
	// Act — graceful per-session end, then shim exit.
	shim.pushEvent(t, `{"type":"closed","session_id":"`+id+`","uuid":"u","reason":"sdk_end","exit_code":0}`)
	shim.end()
	awaitSocketClose(t, conn)
	// Assert
	rec, ok := reg.Get(id)
	if !ok || !rec.Terminal || rec.DeathReason != "sdk_end" {
		t.Errorf("record = %+v, ok=%v", rec, ok)
	}
}

func TestShutdownAllLeavesRegistryRecordsRehydratable(t *testing.T) {
	// Arrange — a live session, then a daemon-wide drain (the routine
	// restart path): its deaths must NOT be recorded as terminal.
	h, reg := registryHarness(t)
	id, shim := h.createSession(t)
	conn := h.dial(t, id)
	readFrame(t, conn) // hello
	// Act
	h.srv.ShutdownAll()
	shim.end()
	awaitSocketClose(t, conn)
	// Assert
	rec, ok := reg.Get(id)
	if !ok {
		t.Fatalf("record for %s vanished", id)
	}
	if rec.Terminal || rec.DeathReason != "" {
		t.Errorf("drained record marked terminal: %+v", rec)
	}
}

func TestFakeSessionIsNeverRegistered(t *testing.T) {
	// Arrange
	h, reg := registryHarness(t)
	// Act
	resp, err := http.Post(h.ts.URL+"/sessions", "application/json",
		bytes.NewBufferString(`{"fake":true}`))
	if err != nil {
		t.Fatalf("POST /sessions: %v", err)
	}
	defer resp.Body.Close()
	var created struct {
		SessionID string `json:"session_id"`
	}
	if err := json.NewDecoder(resp.Body).Decode(&created); err != nil {
		t.Fatalf("decode: %v", err)
	}
	shim := <-h.shims
	t.Cleanup(shim.end)
	// Assert
	if _, ok := reg.Get(created.SessionID); ok {
		t.Error("fake session was registered; it can never rehydrate")
	}
}

// ---- restart rehydration ----------------------------------------------------

// spawnRecorder captures shim spawn invocations for lazy-spawn asserts.
type spawnRecorder struct {
	mu    sync.Mutex
	calls []CreateOpts
	ids   []string
}

func (r *spawnRecorder) record(id string, opts CreateOpts) {
	r.mu.Lock()
	defer r.mu.Unlock()
	r.ids = append(r.ids, id)
	r.calls = append(r.calls, opts)
}

func (r *spawnRecorder) count() int {
	r.mu.Lock()
	defer r.mu.Unlock()
	return len(r.calls)
}

func (r *spawnRecorder) last() (string, CreateOpts) {
	r.mu.Lock()
	defer r.mu.Unlock()
	if len(r.calls) == 0 {
		return "", CreateOpts{}
	}
	return r.ids[len(r.ids)-1], r.calls[len(r.calls)-1]
}

// writeTranscript plants a minimal transcript for uuid rooted at cwd /w
// inside cfg (the CLAUDE_CONFIG_DIR under test).
func writeTranscript(t *testing.T, cfg, uuid string) {
	t.Helper()
	dir := cfg + "/projects/-w"
	if err := os.MkdirAll(dir, 0o755); err != nil {
		t.Fatal(err)
	}
	transcript := `{"type":"user","message":{"role":"user","content":"hello from before the restart"}}` + "\n"
	if err := os.WriteFile(dir+"/"+uuid+".jsonl", []byte(transcript), 0o600); err != nil {
		t.Fatal(err)
	}
}

// rehydrationHarness boots a server over a pre-populated registry file,
// exactly as a restarted daemon would find it.
func rehydrationHarness(t *testing.T, forceFake bool, records ...registry.Record) (*harness, *registry.Registry, *spawnRecorder) {
	t.Helper()
	regPath := filepath.Join(t.TempDir(), "sessions.json")
	seed := registry.Open(regPath, func(string, ...any) {})
	for _, rec := range records {
		if err := seed.Put(rec); err != nil {
			t.Fatal(err)
		}
	}
	reg := registry.Open(regPath, func(string, ...any) {})
	rec := &spawnRecorder{}
	shims := make(chan *fakeShim, 8)
	srv := New(Config{
		DaemonVersion: "0.1.0-test",
		Retention:     64,
		Logf:          func(string, ...any) {},
		ForceFake:     forceFake,
		Spawn: func(sessionID string, opts CreateOpts) (session.ShimHandle, error) {
			rec.record(sessionID, opts)
			shim := newFakeShim()
			shims <- shim
			return shim, nil
		},
		Registry: reg,
	})
	ts := httptest.NewServer(srv.Handler())
	t.Cleanup(ts.Close)
	drainSessionsOnCleanup(t, srv)
	return &harness{ts: ts, srv: srv, shims: shims}, reg, rec
}

// awaitShim takes the next spawned fake shim, registering its cleanup.
func (h *harness) awaitShim(t *testing.T) *fakeShim {
	t.Helper()
	select {
	case shim := <-h.shims:
		t.Cleanup(shim.end)
		return shim
	case <-time.After(recvTimeout):
		t.Fatal("no shim was spawned")
		return nil
	}
}

func listSessions(t *testing.T, h *harness) []map[string]any {
	t.Helper()
	resp, err := http.Get(h.ts.URL + "/sessions")
	if err != nil {
		t.Fatalf("GET /sessions: %v", err)
	}
	defer resp.Body.Close()
	var body struct {
		Sessions []map[string]any `json:"sessions"`
	}
	if err := json.NewDecoder(resp.Body).Decode(&body); err != nil {
		t.Fatalf("decode: %v", err)
	}
	return body.Sessions
}

func TestRestartListsDormantSessionUnderItsOriginalID(t *testing.T) {
	// Arrange
	cfg := t.TempDir()
	t.Setenv("CLAUDE_CONFIG_DIR", cfg)
	writeTranscript(t, cfg, "uuid-1")
	h, _, _ := rehydrationHarness(t, false,
		registry.Record{SessionID: "s_before", CWD: "/w", ClaudeSessionID: "uuid-1"})
	// Act
	sessions := listSessions(t, h)
	// Assert — the pre-restart id resolves, flagged as a cold session.
	if len(sessions) != 1 || sessions[0]["session_id"] != "s_before" || sessions[0]["rehydratable"] != true {
		t.Fatalf("sessions = %v", sessions)
	}
}

func TestRestartSpawnsNoShimBeforeFirstAccess(t *testing.T) {
	// Arrange
	cfg := t.TempDir()
	t.Setenv("CLAUDE_CONFIG_DIR", cfg)
	writeTranscript(t, cfg, "uuid-1")
	h, _, spawns := rehydrationHarness(t, false,
		registry.Record{SessionID: "s_before", CWD: "/w", ClaudeSessionID: "uuid-1"})
	// Act — boot plus a listing are NOT real accesses.
	listSessions(t, h)
	// Assert — a restart never fans out shims eagerly.
	if n := spawns.count(); n != 0 {
		t.Fatalf("spawn count = %d before first access, want 0", n)
	}
}

func TestFirstStreamAccessRehydratesWithResume(t *testing.T) {
	// Arrange
	cfg := t.TempDir()
	t.Setenv("CLAUDE_CONFIG_DIR", cfg)
	writeTranscript(t, cfg, "uuid-1")
	h, _, spawns := rehydrationHarness(t, false,
		registry.Record{SessionID: "s_before", CWD: "/w", Model: "haiku", PermissionMode: "plan", ClaudeSessionID: "uuid-1"})
	// Act
	conn := h.dial(t, "s_before")
	h.awaitShim(t)
	// Assert — one shim, resuming the durable uuid with the record's opts.
	id, opts := spawns.last()
	if spawns.count() != 1 || id != "s_before" {
		t.Fatalf("spawns = %d for id %q", spawns.count(), id)
	}
	if opts.Resume != "uuid-1" || opts.CWD != "/w" || opts.Model != "haiku" || opts.PermissionMode != "plan" {
		t.Fatalf("spawn opts = %+v", opts)
	}
	hello := readFrame(t, conn)
	if hello["type"] != "hello" || hello["session_id"] != "s_before" || hello["claude_session_id"] != "uuid-1" {
		t.Fatalf("hello = %v", hello)
	}
}

func TestRehydratedSessionReplaysPreRestartHistory(t *testing.T) {
	// Arrange
	cfg := t.TempDir()
	t.Setenv("CLAUDE_CONFIG_DIR", cfg)
	writeTranscript(t, cfg, "uuid-1")
	h, _, _ := rehydrationHarness(t, false,
		registry.Record{SessionID: "s_before", CWD: "/w", ClaudeSessionID: "uuid-1"})
	// Act
	conn := h.dial(t, "s_before")
	h.awaitShim(t)
	hello := readFrame(t, conn)
	req, _ := json.Marshal(map[string]any{"type": "replay-request", "from_seq": hello["resume_from_seq"]})
	if err := conn.WriteMessage(websocket.TextMessage, req); err != nil {
		t.Fatalf("replay-request: %v", err)
	}
	// Assert — the transcript-seeded user turn comes back.
	frame := readFrame(t, conn)
	if frame["type"] != "user-turn" {
		t.Fatalf("frame = %v, want the pre-restart user-turn", frame)
	}
}

func TestSecondAccessReusesTheRehydratedSession(t *testing.T) {
	// Arrange
	cfg := t.TempDir()
	t.Setenv("CLAUDE_CONFIG_DIR", cfg)
	writeTranscript(t, cfg, "uuid-1")
	h, _, spawns := rehydrationHarness(t, false,
		registry.Record{SessionID: "s_before", CWD: "/w", ClaudeSessionID: "uuid-1"})
	conn1 := h.dial(t, "s_before")
	h.awaitShim(t)
	readFrame(t, conn1) // hello
	// Act — a second tab attaches to the SAME id.
	conn2 := h.dial(t, "s_before")
	readFrame(t, conn2) // hello
	// Assert
	if n := spawns.count(); n != 1 {
		t.Fatalf("spawn count = %d after second access, want 1", n)
	}
}

func TestFirstMessageAccessRehydrates(t *testing.T) {
	// Arrange
	cfg := t.TempDir()
	t.Setenv("CLAUDE_CONFIG_DIR", cfg)
	writeTranscript(t, cfg, "uuid-1")
	h, _, spawns := rehydrationHarness(t, false,
		registry.Record{SessionID: "s_before", CWD: "/w", ClaudeSessionID: "uuid-1"})
	// Act — the Emacs HTTP send path is a first access too.
	resp, err := http.Post(h.ts.URL+"/sessions/s_before/message", "application/json",
		bytes.NewBufferString(`{"content":"still here?"}`))
	if err != nil {
		t.Fatalf("POST message: %v", err)
	}
	defer resp.Body.Close()
	h.awaitShim(t)
	// Assert
	if resp.StatusCode != http.StatusAccepted {
		t.Fatalf("status = %d, want 202", resp.StatusCode)
	}
	if n := spawns.count(); n != 1 {
		t.Fatalf("spawn count = %d, want 1", n)
	}
}

func TestBootPrunesRecordWhoseTranscriptIsGone(t *testing.T) {
	// Arrange — a registry record whose transcript never made it to disk.
	t.Setenv("CLAUDE_CONFIG_DIR", t.TempDir())
	h, reg, _ := rehydrationHarness(t, false,
		registry.Record{SessionID: "s_gone", CWD: "/w", ClaudeSessionID: "uuid-lost"})
	// Act / Assert — not listed, and pruned from the registry file.
	if sessions := listSessions(t, h); len(sessions) != 0 {
		t.Fatalf("sessions = %v, want none", sessions)
	}
	if _, ok := reg.Get("s_gone"); ok {
		t.Error("record with a missing transcript survived the boot prune")
	}
}

func TestBootPrunesRecordWithoutClaudeSessionID(t *testing.T) {
	// Arrange — a session that died before system:init ever arrived.
	t.Setenv("CLAUDE_CONFIG_DIR", t.TempDir())
	h, reg, _ := rehydrationHarness(t, false,
		registry.Record{SessionID: "s_noinit", CWD: "/w"})
	// Act / Assert
	if sessions := listSessions(t, h); len(sessions) != 0 {
		t.Fatalf("sessions = %v, want none", sessions)
	}
	if _, ok := reg.Get("s_noinit"); ok {
		t.Error("record without a resume target survived the boot prune")
	}
}

func TestBootLeavesTerminalRecordsDormantless(t *testing.T) {
	// Arrange — a conversation that ENDED before the restart.
	cfg := t.TempDir()
	t.Setenv("CLAUDE_CONFIG_DIR", cfg)
	writeTranscript(t, cfg, "uuid-dead")
	h, reg, _ := rehydrationHarness(t, false,
		registry.Record{SessionID: "s_dead", CWD: "/w", ClaudeSessionID: "uuid-dead",
			Terminal: true, DeathReason: "shim_died"})
	// Act / Assert — not rehydratable, but the record is kept (it is
	// history, not garbage; it prunes once its transcript goes away).
	if sessions := listSessions(t, h); len(sessions) != 0 {
		t.Fatalf("sessions = %v, want none", sessions)
	}
	if _, ok := reg.Get("s_dead"); !ok {
		t.Error("terminal record was pruned at boot")
	}
}

func TestAccessPrunesWhenTranscriptVanishedSinceBoot(t *testing.T) {
	// Arrange — viable at boot, transcript deleted afterwards.
	cfg := t.TempDir()
	t.Setenv("CLAUDE_CONFIG_DIR", cfg)
	writeTranscript(t, cfg, "uuid-1")
	h, reg, spawns := rehydrationHarness(t, false,
		registry.Record{SessionID: "s_before", CWD: "/w", ClaudeSessionID: "uuid-1"})
	if err := os.Remove(cfg + "/projects/-w/uuid-1.jsonl"); err != nil {
		t.Fatal(err)
	}
	// Act — first access re-runs the viability gate.
	resp, err := http.Post(h.ts.URL+"/sessions/s_before/message", "application/json",
		bytes.NewBufferString(`{"content":"x"}`))
	if err != nil {
		t.Fatalf("POST message: %v", err)
	}
	defer resp.Body.Close()
	// Assert — 404 (rebind territory), no shim, record pruned.
	if resp.StatusCode != http.StatusNotFound {
		t.Fatalf("status = %d, want 404", resp.StatusCode)
	}
	if n := spawns.count(); n != 0 {
		t.Fatalf("spawn count = %d, want 0 (a doomed --resume must not launch)", n)
	}
	if _, ok := reg.Get("s_before"); ok {
		t.Error("record survived the access-time prune")
	}
}

func TestDeleteDormantSessionDropsTheRecord(t *testing.T) {
	// Arrange
	cfg := t.TempDir()
	t.Setenv("CLAUDE_CONFIG_DIR", cfg)
	writeTranscript(t, cfg, "uuid-1")
	h, reg, spawns := rehydrationHarness(t, false,
		registry.Record{SessionID: "s_before", CWD: "/w", ClaudeSessionID: "uuid-1"})
	req, err := http.NewRequest(http.MethodDelete, h.ts.URL+"/sessions/s_before", nil)
	if err != nil {
		t.Fatal(err)
	}
	// Act
	resp, err := http.DefaultClient.Do(req)
	// Assert
	if err != nil {
		t.Fatalf("DELETE: %v", err)
	}
	defer resp.Body.Close()
	if resp.StatusCode != http.StatusNoContent {
		t.Fatalf("status = %d, want 204", resp.StatusCode)
	}
	if _, ok := reg.Get("s_before"); ok {
		t.Error("deleted dormant session still has a registry record")
	}
	if n := spawns.count(); n != 0 {
		t.Fatalf("spawn count = %d, want 0 (deletion must not rehydrate)", n)
	}
}

func TestFakeDaemonLeavesRegistryRecordsUntouched(t *testing.T) {
	// Arrange — a -fake boot must not prune REAL records just because
	// their transcripts are invisible to the offline daemon.
	t.Setenv("CLAUDE_CONFIG_DIR", t.TempDir())
	h, reg, _ := rehydrationHarness(t, true,
		registry.Record{SessionID: "s_real", CWD: "/w", ClaudeSessionID: "uuid-real"})
	// Act / Assert — not listed (no rehydration), but preserved on disk.
	if sessions := listSessions(t, h); len(sessions) != 0 {
		t.Fatalf("sessions = %v, want none under -fake", sessions)
	}
	if _, ok := reg.Get("s_real"); !ok {
		t.Error("-fake boot destroyed a real registry record")
	}
}

func TestRemediationRefusesADormantSession(t *testing.T) {
	// Arrange — a rehydratable session is NOT gone.
	cfg := t.TempDir()
	t.Setenv("CLAUDE_CONFIG_DIR", cfg)
	writeTranscript(t, cfg, "uuid-1")
	h, _, _ := rehydrationHarness(t, false,
		registry.Record{SessionID: "s_before", CWD: "/w", ClaudeSessionID: "uuid-1"})
	h.srv.remediator = &fakeRemediator{}
	// Act
	resp := h.postRemediation(t, "s_before")
	// Assert
	if resp.StatusCode != http.StatusConflict {
		t.Fatalf("status = %d, want 409 for a rehydratable session", resp.StatusCode)
	}
}

// --- per-session account selection (CLAUDE_CONFIG_DIR) ------------------------

func TestShimEnvAlwaysMarksOwnership(t *testing.T) {
	// Arrange / Act
	env := ShimEnv(CreateOpts{})
	// Assert
	if !slices.Contains(env, "AGENT_REPL_OWNED=1") {
		t.Fatalf("env = %v, want the ownership marker", env)
	}
}

func TestShimEnvExportsSessionConfigDir(t *testing.T) {
	// Arrange / Act
	env := ShimEnv(CreateOpts{ConfigDir: "/home/u/.claude-chesscom"})
	// Assert
	if !slices.Contains(env, "CLAUDE_CONFIG_DIR=/home/u/.claude-chesscom") {
		t.Fatalf("env = %v, want the session's CLAUDE_CONFIG_DIR", env)
	}
}

func TestShimEnvOmitsConfigDirWhenUnset(t *testing.T) {
	// Arrange / Act — an empty ConfigDir must leave the inherited value
	// alone, NOT export CLAUDE_CONFIG_DIR="" (a config root named "").
	env := ShimEnv(CreateOpts{})
	// Assert
	for _, e := range env {
		if strings.HasPrefix(e, "CLAUDE_CONFIG_DIR=") {
			t.Fatalf("env = %v, want no CLAUDE_CONFIG_DIR entry", env)
		}
	}
}

func TestCreateSessionForwardsConfigDirToTheShimSpawn(t *testing.T) {
	// Arrange
	h, captured := resumeGateHarness(t)
	// Act
	postCreate(t, h, `{"cwd":"/w","config_dir":"/home/u/.claude-chesscom"}`)
	// Assert — the account reaches the process that launches the CLI.
	if captured.ConfigDir != "/home/u/.claude-chesscom" {
		t.Fatalf("spawn opts.ConfigDir = %q, want /home/u/.claude-chesscom", captured.ConfigDir)
	}
}

func TestCreateSessionPersistsConfigDirOnTheRegistryRecord(t *testing.T) {
	// Arrange
	h, reg := registryHarness(t)
	// Act
	id := postCreate(t, h, `{"cwd":"/w","config_dir":"/home/u/.claude-chesscom"}`)
	// Assert — a restart must resolve this session's transcript under the
	// SAME account root, so the dir has to survive on disk.
	rec, ok := reg.Get(id)
	if !ok {
		t.Fatalf("no registry record for %s", id)
	}
	if rec.ConfigDir != "/home/u/.claude-chesscom" {
		t.Fatalf("record.ConfigDir = %q, want /home/u/.claude-chesscom", rec.ConfigDir)
	}
}

func TestResumeGateStatsTranscriptUnderTheSessionConfigDir(t *testing.T) {
	// Arrange — the transcript lives under the SESSION's config dir, while
	// the daemon's own CLAUDE_CONFIG_DIR points somewhere else entirely.
	// Resolving against the daemon's env would find nothing and silently
	// downgrade the resume into a fresh conversation.
	sessionCfg := t.TempDir()
	dir := filepath.Join(sessionCfg, "projects", "-w")
	if err := os.MkdirAll(dir, 0o755); err != nil {
		t.Fatal(err)
	}
	transcript := `{"type":"user","message":{"role":"user","content":"hello"}}` + "\n"
	if err := os.WriteFile(filepath.Join(dir, "uuid-1.jsonl"), []byte(transcript), 0o600); err != nil {
		t.Fatal(err)
	}
	t.Setenv("CLAUDE_CONFIG_DIR", t.TempDir())
	h, captured := resumeGateHarness(t)
	// Act
	postCreate(t, h, fmt.Sprintf(`{"cwd":"/w","resume":"uuid-1","config_dir":%q}`, sessionCfg))
	// Assert
	if captured.Resume != "uuid-1" {
		t.Fatalf("spawn opts.Resume = %q, want uuid-1 (the gate looked in the wrong config dir)", captured.Resume)
	}
}

// --- POST /sessions/{id}/login ------------------------------------------------

// recordingSentinel captures the sentinel side-channel calls the daemon
// makes on Emacs's behalf.
type recordingSentinel struct {
	mu     sync.Mutex
	logins [][2]string
}

func (r *recordingSentinel) PermissionRequested(string, string, string) {}
func (r *recordingSentinel) PermissionResolved(string, string, string)  {}
func (r *recordingSentinel) SessionDead(string, string)                 {}

func (r *recordingSentinel) LoginRequested(cwd, sid string) {
	r.mu.Lock()
	defer r.mu.Unlock()
	r.logins = append(r.logins, [2]string{cwd, sid})
}

func (r *recordingSentinel) requested() [][2]string {
	r.mu.Lock()
	defer r.mu.Unlock()
	return slices.Clone(r.logins)
}

// loginHarness is a harness whose sentinel side channel is observable.
func loginHarness(t *testing.T, sink session.SentinelSink) *harness {
	t.Helper()
	shims := make(chan *fakeShim, 8)
	srv := New(Config{
		DaemonVersion: "0.1.0-test",
		Retention:     64,
		Logf:          func(string, ...any) {},
		Spawn: func(string, CreateOpts) (session.ShimHandle, error) {
			shim := newFakeShim()
			shims <- shim
			return shim, nil
		},
		Sentinel: sink,
	})
	ts := httptest.NewServer(srv.Handler())
	t.Cleanup(ts.Close)
	return &harness{ts: ts, srv: srv, shims: shims}
}

func postLogin(t *testing.T, h *harness, sessionID string) *http.Response {
	t.Helper()
	resp, err := http.Post(h.ts.URL+"/sessions/"+sessionID+"/login", "application/json", nil)
	if err != nil {
		t.Fatalf("POST /sessions/%s/login: %v", sessionID, err)
	}
	t.Cleanup(func() { _ = resp.Body.Close() })
	return resp
}

func TestLoginHandsTheSessionCWDToEmacs(t *testing.T) {
	// Arrange
	sink := &recordingSentinel{}
	h := loginHarness(t, sink)
	id := postCreate(t, h, `{"cwd":"/w"}`)
	// Act
	resp := postLogin(t, h, id)
	// Assert — the cwd is what selects the account, so it is the payload.
	if resp.StatusCode != http.StatusAccepted {
		t.Fatalf("status = %d, want 202", resp.StatusCode)
	}
	got := sink.requested()
	if len(got) != 1 || got[0][0] != "/w" {
		t.Fatalf("login requests = %v, want one for cwd /w", got)
	}
}

func TestLoginOnAnUnknownSessionIs404(t *testing.T) {
	// Arrange
	h := loginHarness(t, &recordingSentinel{})
	// Act
	resp := postLogin(t, h, "s_nope")
	// Assert
	if resp.StatusCode != http.StatusNotFound {
		t.Fatalf("status = %d, want 404", resp.StatusCode)
	}
}

func TestLoginWithoutASentinelSinkIs503(t *testing.T) {
	// Arrange — no sink means no channel to Emacs, so the button cannot
	// work and must say so rather than report a login that never happened.
	h := loginHarness(t, nil)
	id := postCreate(t, h, `{"cwd":"/w"}`)
	// Act
	resp := postLogin(t, h, id)
	// Assert
	if resp.StatusCode != http.StatusServiceUnavailable {
		t.Fatalf("status = %d, want 503", resp.StatusCode)
	}
}

func TestLoginOnACWDlessSessionIs409(t *testing.T) {
	// Arrange — no cwd means no account can be derived.
	sink := &recordingSentinel{}
	h := loginHarness(t, sink)
	id := postCreate(t, h, `{}`)
	// Act
	resp := postLogin(t, h, id)
	// Assert
	if resp.StatusCode != http.StatusConflict {
		t.Fatalf("status = %d, want 409", resp.StatusCode)
	}
	if got := sink.requested(); len(got) != 0 {
		t.Fatalf("login requests = %v, want none", got)
	}
}
