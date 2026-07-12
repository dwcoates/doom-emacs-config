package server

import (
	"bytes"
	"encoding/json"
	"fmt"
	"net/http"
	"net/http/httptest"
	"os"
	"slices"
	"strings"
	"sync"
	"testing"
	"time"

	"github.com/gorilla/websocket"

	"claude-repld/internal/protocol"
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
