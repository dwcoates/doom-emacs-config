package server

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	"net/http/httptest"
	"net/url"
	"os"
	"path/filepath"
	"slices"
	"strings"
	"sync"
	"testing"
	"time"

	"github.com/gorilla/websocket"

	"claude-repld/internal/login"
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

// Kill models a real process kill: the event stream dies, which is what
// Run observes. Server-level hibernation tests synchronize on the shim's
// forwarded shutdown command and on spawn events, not on the kill itself,
// so this fake need only end the stream.
func (f *fakeShim) Kill() error {
	f.end()
	return nil
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
	// sink records sentinel side-channel writes; only switchHarness sets it.
	sink *recordingSentinel
}

// recordingSentinel is a thread-safe session.SentinelSink recording calls.
type recordingSentinel struct {
	mu    sync.Mutex
	calls []string
}

func (r *recordingSentinel) record(s string) {
	r.mu.Lock()
	defer r.mu.Unlock()
	r.calls = append(r.calls, s)
}

func (r *recordingSentinel) PermissionRequested(cwd, sid, reqID string) {
	r.record("requested " + cwd + " " + sid + " " + reqID)
}

func (r *recordingSentinel) PermissionResolved(cwd, sid, reqID string) {
	r.record("resolved " + cwd + " " + sid + " " + reqID)
}

func (r *recordingSentinel) SessionDead(cwd, sid string) {
	r.record("dead " + cwd + " " + sid)
}

func (r *recordingSentinel) AccountChanged(cwd, sid string) {
	r.record("account-changed " + cwd + " " + sid)
}

func (r *recordingSentinel) snapshot() []string {
	r.mu.Lock()
	defer r.mu.Unlock()
	return append([]string(nil), r.calls...)
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
	// Assert — init announces the model it reports, then the system frame.
	if frame := readFrame(t, conn); frame["type"] != "model-changed" || frame["seq"] != float64(1) {
		t.Errorf("frame = %v, want model-changed at seq 1", frame)
	}
	if frame := readFrame(t, conn); frame["type"] != "system" || frame["seq"] != float64(2) {
		t.Errorf("frame = %v, want system at seq 2", frame)
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

func TestSendMessageRouteCarriesOriginOntoUserTurn(t *testing.T) {
	// Arrange — a merge-remediation POST tagged origin "merge".
	h := newHarness(t)
	id, _ := h.createSession(t)
	conn := h.dial(t, id)
	readFrame(t, conn) // hello
	// Act
	resp, err := http.Post(h.ts.URL+"/sessions/"+id+"/message", "application/json",
		bytes.NewBufferString(`{"content":"rebase onto local master","origin":"merge"}`))
	if err != nil {
		t.Fatalf("POST message: %v", err)
	}
	defer resp.Body.Close()
	if resp.StatusCode != http.StatusAccepted {
		t.Fatalf("status = %d, want 202", resp.StatusCode)
	}
	// Assert — the broadcast user-turn carries the origin.
	turn := readFrame(t, conn)
	if turn["type"] != "user-turn" {
		t.Fatalf("turn = %v", turn)
	}
	if turn["origin"] != "merge" {
		t.Errorf("origin = %v, want merge", turn["origin"])
	}
}

func TestSendMessageRouteOmitsOriginWhenAbsent(t *testing.T) {
	// Arrange — an ordinary prompt carries no origin.
	h := newHarness(t)
	id, _ := h.createSession(t)
	conn := h.dial(t, id)
	readFrame(t, conn) // hello
	// Act
	resp, err := http.Post(h.ts.URL+"/sessions/"+id+"/message", "application/json",
		bytes.NewBufferString(`{"content":"a normal prompt"}`))
	if err != nil {
		t.Fatalf("POST message: %v", err)
	}
	defer resp.Body.Close()
	// Assert — omitempty drops origin from the user-turn frame.
	turn := readFrame(t, conn)
	if turn["type"] != "user-turn" {
		t.Fatalf("turn = %v", turn)
	}
	if _, present := turn["origin"]; present {
		t.Errorf("origin present on an untagged turn: %v", turn["origin"])
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

// --- in-flight message queue (§2.13) HTTP routes ---------------------------

// postMessage submits a user message over HTTP, asserting the 202.
func postMessage(t *testing.T, h *harness, id, content string) {
	t.Helper()
	resp, err := http.Post(h.ts.URL+"/sessions/"+id+"/message", "application/json",
		bytes.NewBufferString(fmt.Sprintf(`{"content":%q}`, content)))
	if err != nil {
		t.Fatalf("POST message: %v", err)
	}
	defer resp.Body.Close()
	if resp.StatusCode != http.StatusAccepted {
		t.Fatalf("message status = %d, want 202", resp.StatusCode)
	}
}

// readFrameOfType reads frames until one of typ arrives.
func readFrameOfType(t *testing.T, conn *websocket.Conn, typ string) map[string]any {
	t.Helper()
	for {
		f := readFrame(t, conn)
		if f["type"] == typ {
			return f
		}
	}
}

// enqueueOverHTTP drives a session BUSY with a first turn, then parks a
// second message on the queue (classification is off in the test harness,
// so it resolves to an immediate wait) and returns the parked item's id.
func enqueueOverHTTP(t *testing.T, h *harness, id string, shim *fakeShim, conn *websocket.Conn) string {
	t.Helper()
	postMessage(t, h, id, "first task")
	readFrameOfType(t, conn, "user-turn")
	<-shim.sent // forwarded first turn
	postMessage(t, h, id, "second task")
	added := readFrameOfType(t, conn, "queue-added")
	readFrameOfType(t, conn, "queue-classified") // the immediate wait
	return added["queue_id"].(string)
}

func TestQueueCancelRouteRemovesItem(t *testing.T) {
	// Arrange
	h := newHarness(t)
	id, shim := h.createSession(t)
	conn := h.dial(t, id)
	readFrame(t, conn) // hello
	qid := enqueueOverHTTP(t, h, id, shim, conn)
	// Act
	resp, err := http.Post(h.ts.URL+"/sessions/"+id+"/queue/"+qid+"/cancel", "application/json", nil)
	if err != nil {
		t.Fatalf("POST cancel: %v", err)
	}
	defer resp.Body.Close()
	// Assert
	if resp.StatusCode != http.StatusAccepted {
		t.Fatalf("status = %d, want 202", resp.StatusCode)
	}
	removed := readFrameOfType(t, conn, "queue-removed")
	if removed["reason"] != "cancelled" || removed["queue_id"] != qid {
		t.Errorf("removed = %v", removed)
	}
}

func TestQueueRunNowRouteEscalates(t *testing.T) {
	// Arrange
	h := newHarness(t)
	id, shim := h.createSession(t)
	conn := h.dial(t, id)
	readFrame(t, conn) // hello
	qid := enqueueOverHTTP(t, h, id, shim, conn)
	// Act
	resp, err := http.Post(h.ts.URL+"/sessions/"+id+"/queue/"+qid+"/run-now", "application/json", nil)
	if err != nil {
		t.Fatalf("POST run-now: %v", err)
	}
	defer resp.Body.Close()
	// Assert — 202, a user-sourced interrupt verdict, and an interrupt sent.
	if resp.StatusCode != http.StatusAccepted {
		t.Fatalf("status = %d, want 202", resp.StatusCode)
	}
	reclassified := readFrameOfType(t, conn, "queue-classified")
	if reclassified["verdict"] != "interrupt" || reclassified["source"] != "user" || reclassified["queue_id"] != qid {
		t.Errorf("reclassified = %v", reclassified)
	}
	select {
	case line := <-shim.sent:
		if !strings.Contains(string(line), `"interrupt"`) {
			t.Errorf("escalation sent = %s", line)
		}
	case <-time.After(recvTimeout):
		t.Fatal("run-now did not interrupt the shim")
	}
}

func TestQueueOverrideRoutes404OnUnknownSession(t *testing.T) {
	// Arrange
	h := newHarness(t)
	// Act + Assert
	for _, path := range []string{"run-now", "cancel"} {
		resp, err := http.Post(h.ts.URL+"/sessions/nope/queue/q1/"+path, "application/json", nil)
		if err != nil {
			t.Fatalf("POST %s: %v", path, err)
		}
		resp.Body.Close()
		if resp.StatusCode != http.StatusNotFound {
			t.Errorf("%s status = %d, want 404", path, resp.StatusCode)
		}
	}
}

func TestQueueCancelRouteStaleIdIsAccepted(t *testing.T) {
	// Arrange — a live session, but a queue_id that was never issued.
	h := newHarness(t)
	id, _ := h.createSession(t)
	// Act — a stale id is a no-op ack, not an error.
	resp, err := http.Post(h.ts.URL+"/sessions/"+id+"/queue/ghost/cancel", "application/json", nil)
	if err != nil {
		t.Fatalf("POST cancel: %v", err)
	}
	defer resp.Body.Close()
	// Assert
	if resp.StatusCode != http.StatusAccepted {
		t.Errorf("status = %d, want 202", resp.StatusCode)
	}
}

func TestListSessionsCarriesQueue(t *testing.T) {
	// Arrange
	h := newHarness(t)
	id, shim := h.createSession(t)
	conn := h.dial(t, id)
	readFrame(t, conn) // hello
	qid := enqueueOverHTTP(t, h, id, shim, conn)
	// Act
	listResp, err := http.Get(h.ts.URL + "/sessions")
	if err != nil {
		t.Fatalf("GET: %v", err)
	}
	defer listResp.Body.Close()
	var body struct {
		Sessions []struct {
			SessionID string `json:"session_id"`
			Queue     []struct {
				QueueID string `json:"queue_id"`
				Status  string `json:"status"`
			} `json:"queue"`
		} `json:"sessions"`
	}
	if err := json.NewDecoder(listResp.Body).Decode(&body); err != nil {
		t.Fatalf("decode: %v", err)
	}
	// Assert
	if len(body.Sessions) != 1 || len(body.Sessions[0].Queue) != 1 {
		t.Fatalf("sessions = %+v", body.Sessions)
	}
	item := body.Sessions[0].Queue[0]
	if item.QueueID != qid || item.Status != "waiting" {
		t.Errorf("queued item = %+v", item)
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

func TestCreateSessionHardFailsUnresumableResume(t *testing.T) {
	// Arrange: a config dir with no transcript for the resume target.
	t.Setenv("CLAUDE_CONFIG_DIR", t.TempDir())
	h, captured := resumeGateHarness(t)
	// Act
	resp, err := http.Post(h.ts.URL+"/sessions", "application/json",
		bytes.NewBufferString(`{"cwd":"/w","resume":"uuid-gone"}`))
	if err != nil {
		t.Fatalf("POST /sessions: %v", err)
	}
	defer resp.Body.Close()
	// Assert: the create HARD-FAILS (no fresh start, no doomed --resume).
	if resp.StatusCode != http.StatusUnprocessableEntity {
		t.Fatalf("status = %d, want 422", resp.StatusCode)
	}
	var body struct {
		Code          string   `json:"code"`
		ResumeID      string   `json:"resume_id"`
		SearchedPaths []string `json:"searched_paths"`
		Error         string   `json:"error"`
	}
	if err := json.NewDecoder(resp.Body).Decode(&body); err != nil {
		t.Fatalf("decode: %v", err)
	}
	if body.Code != "resume_transcript_missing" {
		t.Fatalf("code = %q, want resume_transcript_missing", body.Code)
	}
	if body.ResumeID != "uuid-gone" {
		t.Fatalf("resume_id = %q, want uuid-gone", body.ResumeID)
	}
	if len(body.SearchedPaths) == 0 {
		t.Fatalf("searched_paths empty, want the stat'd transcript path")
	}
	if body.Error == "" {
		t.Fatalf("error message empty, want a loud human-readable message")
	}
	// And NO shim was spawned: the create was rejected before launch, so
	// nothing took the lost session's place.
	select {
	case <-h.shims:
		t.Fatal("a shim was spawned; want the create rejected before any launch")
	default:
	}
	if captured.Resume != "" {
		t.Fatalf("spawn captured Resume = %q, want no spawn at all", captured.Resume)
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

func TestListSessionsEnvelopeCarriesBinaryMTime(t *testing.T) {
	// Arrange — a server launched from a binary with a known mtime.
	const wantMTime int64 = 1_700_000_000
	srv := New(Config{
		DaemonVersion: "0.1.0-test",
		BinaryMTime:   wantMTime,
		Retention:     64,
		Logf:          func(string, ...any) {},
	})
	ts := httptest.NewServer(srv.Handler())
	t.Cleanup(ts.Close)
	// Act
	resp, err := http.Get(ts.URL + "/sessions")
	if err != nil {
		t.Fatalf("GET /sessions: %v", err)
	}
	defer resp.Body.Close()
	var body struct {
		BinaryMTime int64 `json:"daemon_binary_mtime"`
	}
	if err := json.NewDecoder(resp.Body).Decode(&body); err != nil {
		t.Fatalf("decode: %v", err)
	}
	// Assert
	if body.BinaryMTime != wantMTime {
		t.Fatalf("daemon_binary_mtime = %d, want %d", body.BinaryMTime, wantMTime)
	}
}

func TestListSessionsEnvelopeReportsZeroBinaryMTimeWhenUnknown(t *testing.T) {
	// Arrange — a server whose boot-time stat failed leaves BinaryMTime unset.
	// The field must still be present and zero so Emacs never treats a daemon
	// that cannot report its mtime as stale on a guess.
	h := newHarness(t)
	// Act
	resp, err := http.Get(h.ts.URL + "/sessions")
	if err != nil {
		t.Fatalf("GET /sessions: %v", err)
	}
	defer resp.Body.Close()
	var raw map[string]json.RawMessage
	if err := json.NewDecoder(resp.Body).Decode(&raw); err != nil {
		t.Fatalf("decode: %v", err)
	}
	// Assert — the key is present (not omitted) and decodes to 0.
	field, ok := raw["daemon_binary_mtime"]
	if !ok {
		t.Fatalf("daemon_binary_mtime absent from envelope; want present and zero")
	}
	if got := strings.TrimSpace(string(field)); got != "0" {
		t.Fatalf("daemon_binary_mtime = %s, want 0", got)
	}
}

// --- graceful shutdown over HTTP --------------------------------------------

func TestShutdownEndpointTriggersRequestShutdown(t *testing.T) {
	// Arrange — a server whose shutdown hook signals a channel.
	fired := make(chan struct{}, 1)
	srv := New(Config{
		DaemonVersion:   "0.1.0-test",
		Retention:       64,
		Logf:            func(string, ...any) {},
		RequestShutdown: func() { fired <- struct{}{} },
	})
	ts := httptest.NewServer(srv.Handler())
	t.Cleanup(ts.Close)
	// Act
	resp, err := http.Post(ts.URL+"/shutdown", "", nil)
	if err != nil {
		t.Fatalf("POST /shutdown: %v", err)
	}
	defer resp.Body.Close()
	// Assert — 202 acknowledged, and the teardown hook fires.
	if resp.StatusCode != http.StatusAccepted {
		t.Fatalf("status = %d, want 202", resp.StatusCode)
	}
	select {
	case <-fired:
	case <-time.After(2 * time.Second):
		t.Fatal("RequestShutdown was never invoked")
	}
}

func TestShutdownEndpointReportsUnconfiguredWhenNoHook(t *testing.T) {
	// Arrange — the default harness wires no RequestShutdown.
	h := newHarness(t)
	// Act
	resp, err := http.Post(h.ts.URL+"/shutdown", "", nil)
	if err != nil {
		t.Fatalf("POST /shutdown: %v", err)
	}
	defer resp.Body.Close()
	// Assert — the capability is reported unconfigured, never half-acted.
	if resp.StatusCode != http.StatusNotImplemented {
		t.Fatalf("status = %d, want 501", resp.StatusCode)
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
			// A live session drains when its fake shim's LIFO end() cleanup
			// (registered later, so run earlier) closes the event stream. A
			// HIBERNATED session has no shim to end, so nothing would ever
			// close its Done() — shut it down explicitly to reach the same
			// terminal state before the wait.
			if sess.Hibernated() {
				_ = sess.Shutdown("test cleanup")
			}
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

func TestAgentModelSwitchWritesThroughToRegistry(t *testing.T) {
	// Arrange — a session created with no model, so its record starts
	// empty and any observed model is a genuine move.
	h, reg := registryHarness(t)
	id, shim := h.createSession(t)
	conn := h.dial(t, id)
	readFrame(t, conn) // hello
	// Act — the agent answers on opus; the filler event that follows is
	// the sync point (Run handles events serially, so the filler's frame
	// arriving proves the assistant iteration, registrar write included,
	// is done).
	shim.pushEvent(t, `{"type":"assistant-message","session_id":"`+id+`","uuid":"a1","message":{"id":"m1","role":"assistant","model":"opus","stop_reason":"end_turn","content":[{"type":"text","text":"hi"}],"usage":{"input_tokens":1,"output_tokens":1}}}`)
	shim.pushEvent(t, `{"type":"system","session_id":"`+id+`","uuid":"u2","subtype":"slash_command","data":{}}`)
	// Drain the assistant message's frames (model-changed plus its block
	// synthesis) until the filler's system frame arrives: Run processes
	// events serially, so the filler frame proves the assistant iteration,
	// registrar write included, is fully done.
	for {
		frame := readFrame(t, conn)
		if frame["type"] == "system" && frame["subtype"] == "slash_command" {
			break
		}
	}
	// Assert — the durable record now holds the live model so a restart
	// resumes on it.
	rec, ok := reg.Get(id)
	if !ok || rec.Model != "opus" {
		t.Errorf("record = %+v, ok=%v, want model opus", rec, ok)
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

func TestFirstStreamAccessMaterializesWithoutSpawn(t *testing.T) {
	// Arrange
	cfg := t.TempDir()
	t.Setenv("CLAUDE_CONFIG_DIR", cfg)
	writeTranscript(t, cfg, "uuid-1")
	h, _, spawns := rehydrationHarness(t, false,
		registry.Record{SessionID: "s_before", CWD: "/w", ClaudeSessionID: "uuid-1"})
	// Act — attaching is OBSERVING, not acting: it materializes the record
	// into a hibernated session (history from the transcript) with no CLI.
	conn := h.dial(t, "s_before")
	hello := readFrame(t, conn)
	// Assert — the conversation is answerable (hello carries the resume
	// uuid), yet no process was spawned to serve a mere look.
	if hello["type"] != "hello" || hello["session_id"] != "s_before" || hello["claude_session_id"] != "uuid-1" {
		t.Fatalf("hello = %v", hello)
	}
	if n := spawns.count(); n != 0 {
		t.Fatalf("spawn count = %d on stream attach, want 0", n)
	}
}

func TestFirstMessageAccessRevivesWithResumeOpts(t *testing.T) {
	// Arrange
	cfg := t.TempDir()
	t.Setenv("CLAUDE_CONFIG_DIR", cfg)
	writeTranscript(t, cfg, "uuid-1")
	h, _, spawns := rehydrationHarness(t, false,
		registry.Record{SessionID: "s_before", CWD: "/w", Model: "haiku", PermissionMode: "plan", ClaudeSessionID: "uuid-1"})
	// Act — the ACT path (an HTTP send) is what earns the CLI back.
	resp, err := http.Post(h.ts.URL+"/sessions/s_before/message", "application/json",
		bytes.NewBufferString(`{"content":"still here?"}`))
	if err != nil {
		t.Fatalf("POST message: %v", err)
	}
	defer resp.Body.Close()
	h.awaitShim(t)
	// Assert — one shim, resuming the durable uuid under the record's opts.
	id, opts := spawns.last()
	if spawns.count() != 1 || id != "s_before" {
		t.Fatalf("spawns = %d for id %q", spawns.count(), id)
	}
	if opts.Resume != "uuid-1" || opts.CWD != "/w" || opts.Model != "haiku" || opts.PermissionMode != "plan" {
		t.Fatalf("spawn opts = %+v", opts)
	}
}

func TestRehydratedSessionHelloCarriesTheRecordModel(t *testing.T) {
	// Arrange — a restart finds a record whose model is the one the
	// write-through kept current; the materialized hello must carry it, so a
	// client connecting after the restart sees the right model at once.
	cfg := t.TempDir()
	t.Setenv("CLAUDE_CONFIG_DIR", cfg)
	writeTranscript(t, cfg, "uuid-1")
	h, _, _ := rehydrationHarness(t, false,
		registry.Record{SessionID: "s_before", CWD: "/w", Model: "haiku", ClaudeSessionID: "uuid-1"})
	// Act — attaching MATERIALIZES without spawning; the hello is served
	// from the record, so no shim is awaited.
	conn := h.dial(t, "s_before")
	hello := readFrame(t, conn)
	// Assert
	if hello["type"] != "hello" || hello["model"] != "haiku" {
		t.Fatalf("hello = %v, want model haiku", hello)
	}
}

func TestMaterializedSessionReplaysPreRestartHistoryWithoutSpawn(t *testing.T) {
	// Arrange
	cfg := t.TempDir()
	t.Setenv("CLAUDE_CONFIG_DIR", cfg)
	writeTranscript(t, cfg, "uuid-1")
	h, _, spawns := rehydrationHarness(t, false,
		registry.Record{SessionID: "s_before", CWD: "/w", ClaudeSessionID: "uuid-1"})
	// Act — attach and replay, both pure observation.
	conn := h.dial(t, "s_before")
	hello := readFrame(t, conn)
	req, _ := json.Marshal(map[string]any{"type": "replay-request", "from_seq": hello["resume_from_seq"]})
	if err := conn.WriteMessage(websocket.TextMessage, req); err != nil {
		t.Fatalf("replay-request: %v", err)
	}
	// Assert — the transcript-seeded user turn comes back, still no CLI.
	frame := readFrame(t, conn)
	if frame["type"] != "user-turn" {
		t.Fatalf("frame = %v, want the pre-restart user-turn", frame)
	}
	if n := spawns.count(); n != 0 {
		t.Fatalf("spawn count = %d serving replay, want 0", n)
	}
}

func TestSecondStreamAccessReusesMaterializedSession(t *testing.T) {
	// Arrange
	cfg := t.TempDir()
	t.Setenv("CLAUDE_CONFIG_DIR", cfg)
	writeTranscript(t, cfg, "uuid-1")
	h, _, spawns := rehydrationHarness(t, false,
		registry.Record{SessionID: "s_before", CWD: "/w", ClaudeSessionID: "uuid-1"})
	conn1 := h.dial(t, "s_before")
	readFrame(t, conn1) // hello
	// Act — a second tab attaches to the SAME id.
	conn2 := h.dial(t, "s_before")
	readFrame(t, conn2) // hello
	// Assert — both observers share one materialized session, still no CLI.
	if n := spawns.count(); n != 0 {
		t.Fatalf("spawn count = %d after second attach, want 0", n)
	}
	if sessions := listSessions(t, h); len(sessions) != 1 {
		t.Fatalf("sessions = %v, want exactly one materialized session", sessions)
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
	env := ShimEnv(CreateOpts{}, "")
	// Assert
	if !slices.Contains(env, "AGENT_REPL_OWNED=1") {
		t.Fatalf("env = %v, want the ownership marker", env)
	}
}

func TestShimEnvExportsSessionConfigDir(t *testing.T) {
	// Arrange / Act
	env := ShimEnv(CreateOpts{ConfigDir: "/home/u/.claude-chesscom"}, "")
	// Assert
	if !slices.Contains(env, "CLAUDE_CONFIG_DIR=/home/u/.claude-chesscom") {
		t.Fatalf("env = %v, want the session's CLAUDE_CONFIG_DIR", env)
	}
}

func TestShimEnvOmitsConfigDirWhenUnset(t *testing.T) {
	// Arrange / Act — an empty ConfigDir must leave the inherited value
	// alone, NOT export CLAUDE_CONFIG_DIR="" (a config root named "").
	env := ShimEnv(CreateOpts{}, "")
	// Assert
	for _, e := range env {
		if strings.HasPrefix(e, "CLAUDE_CONFIG_DIR=") {
			t.Fatalf("env = %v, want no CLAUDE_CONFIG_DIR entry", env)
		}
	}
}

func TestShimEnvExportsDaemonAddr(t *testing.T) {
	// Arrange / Act — a non-empty addr rides down as AGENT_REPL_DAEMON_ADDR
	// so a session's tools can reach this daemon's HTTP surface.
	env := ShimEnv(CreateOpts{}, "127.0.0.1:8787")
	// Assert
	if !slices.Contains(env, "AGENT_REPL_DAEMON_ADDR=127.0.0.1:8787") {
		t.Fatalf("env = %v, want the daemon addr export", env)
	}
}

func TestShimEnvOmitsDaemonAddrWhenEmpty(t *testing.T) {
	// Arrange / Act — an empty addr must not export AGENT_REPL_DAEMON_ADDR="".
	env := ShimEnv(CreateOpts{}, "")
	// Assert
	for _, e := range env {
		if strings.HasPrefix(e, "AGENT_REPL_DAEMON_ADDR=") {
			t.Fatalf("env = %v, want no AGENT_REPL_DAEMON_ADDR entry", env)
		}
	}
}

// capabilities is the decoded GET /capabilities envelope.
type capabilities struct {
	WidgetAssets bool   `json:"widget_assets"`
	WidgetDir    string `json:"widget_assets_dir"`
	WidgetBundle bool   `json:"widget_bundle_present"`
}

func getCapabilities(t *testing.T, cfg Config) capabilities {
	t.Helper()
	srv := New(cfg)
	ts := httptest.NewServer(srv.Handler())
	t.Cleanup(ts.Close)
	resp, err := http.Get(ts.URL + "/capabilities")
	if err != nil {
		t.Fatalf("GET /capabilities: %v", err)
	}
	defer resp.Body.Close()
	if resp.StatusCode != http.StatusOK {
		t.Fatalf("status = %d, want 200", resp.StatusCode)
	}
	var caps capabilities
	if err := json.NewDecoder(resp.Body).Decode(&caps); err != nil {
		t.Fatalf("decode: %v", err)
	}
	return caps
}

func TestCapabilitiesReportsWidgetAssetsOffWhenUnconfigured(t *testing.T) {
	// Arrange / Act — no WidgetAssetsDir means the capability is off.
	caps := getCapabilities(t, Config{Logf: func(string, ...any) {}})
	// Assert
	if caps.WidgetAssets || caps.WidgetDir != "" || caps.WidgetBundle {
		t.Fatalf("caps = %+v, want widget capability off", caps)
	}
}

func TestCapabilitiesReportsWidgetBundlePresent(t *testing.T) {
	// Arrange — a dist dir that actually holds the mount bundle.
	dir := t.TempDir()
	if err := os.WriteFile(filepath.Join(dir, "chess-widget.js"), []byte("export default 0"), 0o644); err != nil {
		t.Fatal(err)
	}
	// Act
	caps := getCapabilities(t, Config{Logf: func(string, ...any) {}, WidgetAssetsDir: dir})
	// Assert
	if !caps.WidgetAssets || caps.WidgetDir != dir || !caps.WidgetBundle {
		t.Fatalf("caps = %+v, want widget on with bundle present at %q", caps, dir)
	}
}

func TestCapabilitiesReportsBundleMissingWhenDistLacksIt(t *testing.T) {
	// Arrange — a configured dir that does NOT contain chess-widget.js:
	// the route would mount but the mount import would 404.
	dir := t.TempDir()
	// Act
	caps := getCapabilities(t, Config{Logf: func(string, ...any) {}, WidgetAssetsDir: dir})
	// Assert — capability "on" but bundle missing, the distinguishable case.
	if !caps.WidgetAssets || caps.WidgetBundle {
		t.Fatalf("caps = %+v, want widget on but bundle absent", caps)
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

// --- the login terminal, and the account it targets ---------------------------

// fakeTerminal is a login child that records what it was asked to do.
type fakeTerminal struct {
	emit      chan []byte
	leftover  []byte
	closeOnce sync.Once

	mu      sync.Mutex
	written []byte
	rows    uint16
	cols    uint16
}

func newFakeTerminal() *fakeTerminal { return &fakeTerminal{emit: make(chan []byte, 16)} }

func (f *fakeTerminal) Read(p []byte) (int, error) {
	if len(f.leftover) == 0 {
		chunk, ok := <-f.emit
		if !ok {
			return 0, io.EOF
		}
		f.leftover = chunk
	}
	n := copy(p, f.leftover)
	f.leftover = f.leftover[n:]
	return n, nil
}

func (f *fakeTerminal) Write(p []byte) (int, error) {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.written = append(f.written, p...)
	return len(p), nil
}

func (f *fakeTerminal) Resize(rows, cols uint16) error {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.rows, f.cols = rows, cols
	return nil
}

func (f *fakeTerminal) Wait() error { return nil }

func (f *fakeTerminal) Close() error {
	f.closeOnce.Do(func() { close(f.emit) })
	return nil
}

func (f *fakeTerminal) say(s string) { f.emit <- []byte(s) }

func (f *fakeTerminal) keystrokes() string {
	f.mu.Lock()
	defer f.mu.Unlock()
	return string(f.written)
}

func (f *fakeTerminal) size() (uint16, uint16) {
	f.mu.Lock()
	defer f.mu.Unlock()
	return f.rows, f.cols
}

// loginHarness is a harness whose login terminals are fakes. The returned
// map is keyed by ACCOUNT (the CLAUDE_CONFIG_DIR), and accounts records the
// accounts a terminal was actually opened for, in order.
func loginHarness(t *testing.T, withLogins bool) (*harness, map[string]*fakeTerminal, *[]string) {
	t.Helper()
	var mu sync.Mutex
	terms := map[string]*fakeTerminal{}
	accounts := []string{}
	shims := make(chan *fakeShim, 8)

	cfg := Config{
		DaemonVersion: "0.1.0-test",
		Retention:     64,
		Logf:          func(string, ...any) {},
		Spawn: func(string, CreateOpts) (session.ShimHandle, error) {
			shim := newFakeShim()
			shims <- shim
			return shim, nil
		},
	}
	if withLogins {
		cfg.Logins = login.NewManager(login.Config{
			Logf: func(string, ...any) {},
			Start: func(acct string) (login.Proc, error) {
				mu.Lock()
				defer mu.Unlock()
				accounts = append(accounts, acct)
				term := newFakeTerminal()
				terms[acct] = term
				return term, nil
			},
		})
		t.Cleanup(cfg.Logins.CloseAll)
	}

	srv := New(cfg)
	ts := httptest.NewServer(srv.Handler())
	t.Cleanup(ts.Close)
	return &harness{ts: ts, srv: srv, shims: shims}, terms, &accounts
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

// dialLoginTerminal attaches a viewer to the session's login terminal.
func dialLoginTerminal(t *testing.T, h *harness, sessionID string) *websocket.Conn {
	t.Helper()
	url := "ws" + strings.TrimPrefix(h.ts.URL, "http") + "/sessions/" + sessionID + "/login/terminal"
	conn, _, err := websocket.DefaultDialer.Dial(url, nil)
	if err != nil {
		t.Fatalf("dial login terminal: %v", err)
	}
	t.Cleanup(func() { _ = conn.Close() })
	return conn
}

func TestLoginOpensATerminalForTheSessionsAccount(t *testing.T) {
	// Arrange: the ACCOUNT is the config dir, not the cwd. A login aimed at
	// the wrong account would log into the wrong one and leave the real
	// problem in place.
	h, _, accounts := loginHarness(t, true)
	id := postCreate(t, h, `{"cwd":"/w","config_dir":"/root/.claude-chesscom"}`)

	// Act
	resp := postLogin(t, h, id)

	// Assert
	if resp.StatusCode != http.StatusAccepted {
		t.Fatalf("status = %d, want 202", resp.StatusCode)
	}
	if len(*accounts) != 1 || (*accounts)[0] != "/root/.claude-chesscom" {
		t.Fatalf("terminals opened for %v, want one for /root/.claude-chesscom", *accounts)
	}
}

func TestLoginOnADefaultAccountSessionOpensATerminal(t *testing.T) {
	// Arrange: an empty config dir is a REAL account (the CLI's own default
	// root), so it must open a terminal rather than be refused as accountless
	// — which is what the cwd-derived predecessor did.
	h, _, accounts := loginHarness(t, true)
	id := postCreate(t, h, `{"cwd":"/w"}`)

	// Act
	resp := postLogin(t, h, id)

	// Assert
	if resp.StatusCode != http.StatusAccepted {
		t.Fatalf("status = %d, want 202", resp.StatusCode)
	}
	if len(*accounts) != 1 || (*accounts)[0] != "" {
		t.Fatalf("terminals opened for %v, want one for the default account", *accounts)
	}
}

func TestLoginIsIdempotentForOneAccount(t *testing.T) {
	// Arrange: a second click must join the terminal already open, not race
	// a second OAuth flow against the first.
	h, _, accounts := loginHarness(t, true)
	id := postCreate(t, h, `{"cwd":"/w","config_dir":"/root/.claude"}`)

	// Act
	_ = postLogin(t, h, id)
	_ = postLogin(t, h, id)

	// Assert
	if len(*accounts) != 1 {
		t.Fatalf("terminals opened = %v, want exactly one", *accounts)
	}
}

func TestLoginOnAnUnknownSessionIs404(t *testing.T) {
	// Arrange
	h, _, _ := loginHarness(t, true)

	// Act
	resp := postLogin(t, h, "s_nope")

	// Assert
	if resp.StatusCode != http.StatusNotFound {
		t.Fatalf("status = %d, want 404", resp.StatusCode)
	}
}

func TestLoginWithoutAManagerIs503(t *testing.T) {
	// Arrange: no manager means the button cannot work, and must say so
	// rather than report a login that never happened.
	h, _, _ := loginHarness(t, false)
	id := postCreate(t, h, `{"cwd":"/w"}`)

	// Act
	resp := postLogin(t, h, id)

	// Assert
	if resp.StatusCode != http.StatusServiceUnavailable {
		t.Fatalf("status = %d, want 503", resp.StatusCode)
	}
}

func TestLoginTerminalCarriesTheScreenToTheViewer(t *testing.T) {
	// Arrange
	h, terms, _ := loginHarness(t, true)
	id := postCreate(t, h, `{"cwd":"/w","config_dir":"/root/.claude"}`)
	_ = postLogin(t, h, id)
	conn := dialLoginTerminal(t, h, id)

	// Act
	terms["/root/.claude"].say("Paste code here >")

	// Assert
	if err := conn.SetReadDeadline(time.Now().Add(3 * time.Second)); err != nil {
		t.Fatalf("set deadline: %v", err)
	}
	kind, data, err := conn.ReadMessage()
	if err != nil {
		t.Fatalf("read terminal: %v", err)
	}
	if kind != websocket.BinaryMessage {
		t.Errorf("frame kind = %d, want binary (raw pty bytes)", kind)
	}
	if string(data) != "Paste code here >" {
		t.Errorf("terminal = %q", data)
	}
}

func TestLoginTerminalCarriesKeystrokesToTheChild(t *testing.T) {
	// Arrange
	h, terms, _ := loginHarness(t, true)
	id := postCreate(t, h, `{"cwd":"/w","config_dir":"/root/.claude"}`)
	_ = postLogin(t, h, id)
	conn := dialLoginTerminal(t, h, id)

	// Act — the pasted OAuth code.
	if err := conn.WriteMessage(websocket.BinaryMessage, []byte("code-123\r")); err != nil {
		t.Fatalf("write keystrokes: %v", err)
	}

	// Assert
	term := terms["/root/.claude"]
	deadline := time.Now().Add(3 * time.Second)
	for term.keystrokes() != "code-123\r" && time.Now().Before(deadline) {
		time.Sleep(5 * time.Millisecond)
	}
	if got := term.keystrokes(); got != "code-123\r" {
		t.Errorf("child stdin = %q, want %q", got, "code-123\r")
	}
}

func TestLoginTerminalCarriesGeometryToTheChild(t *testing.T) {
	// Arrange: the TUI hard-wraps at the column count, so the viewer's real
	// width is what keeps the OAuth URL readable.
	h, terms, _ := loginHarness(t, true)
	id := postCreate(t, h, `{"cwd":"/w","config_dir":"/root/.claude"}`)
	_ = postLogin(t, h, id)
	conn := dialLoginTerminal(t, h, id)

	// Act
	if err := conn.WriteMessage(websocket.TextMessage, []byte(`{"resize":{"rows":40,"cols":180}}`)); err != nil {
		t.Fatalf("write resize: %v", err)
	}

	// Assert
	term := terms["/root/.claude"]
	deadline := time.Now().Add(3 * time.Second)
	for time.Now().Before(deadline) {
		if rows, cols := term.size(); rows == 40 && cols == 180 {
			return
		}
		time.Sleep(5 * time.Millisecond)
	}
	rows, cols := term.size()
	t.Errorf("child geometry = %dx%d, want 40x180", rows, cols)
}

func TestLoginTerminalBeforeAnyLoginIs409(t *testing.T) {
	// Arrange: attaching to a terminal nobody opened must say so rather than
	// silently open one, which would start an OAuth flow the user never asked
	// for.
	h, _, _ := loginHarness(t, true)
	id := postCreate(t, h, `{"cwd":"/w"}`)

	// Act
	url := "ws" + strings.TrimPrefix(h.ts.URL, "http") + "/sessions/" + id + "/login/terminal"
	_, resp, err := websocket.DefaultDialer.Dial(url, nil)

	// Assert
	if err == nil {
		t.Fatal("dial succeeded, want a refusal")
	}
	if resp == nil || resp.StatusCode != http.StatusConflict {
		t.Fatalf("status = %v, want 409", resp)
	}
}

func TestLoginCloseEndsTheTerminal(t *testing.T) {
	// Arrange
	h, terms, accounts := loginHarness(t, true)
	id := postCreate(t, h, `{"cwd":"/w","config_dir":"/root/.claude"}`)
	_ = postLogin(t, h, id)

	// Act
	req, err := http.NewRequest(http.MethodDelete, h.ts.URL+"/sessions/"+id+"/login", nil)
	if err != nil {
		t.Fatalf("build request: %v", err)
	}
	resp, err := http.DefaultClient.Do(req)
	if err != nil {
		t.Fatalf("DELETE login: %v", err)
	}
	defer func() { _ = resp.Body.Close() }()

	// Assert — closed, and a later login starts a fresh terminal.
	if resp.StatusCode != http.StatusNoContent {
		t.Fatalf("status = %d, want 204", resp.StatusCode)
	}
	term := terms["/root/.claude"]
	deadline := time.Now().Add(3 * time.Second)
	for time.Now().Before(deadline) {
		if _, err := term.Read(make([]byte, 1)); err == io.EOF {
			break
		}
	}
	_ = postLogin(t, h, id)
	if len(*accounts) != 2 {
		t.Errorf("terminals opened = %v, want a fresh one after the close", *accounts)
	}
}

// --- GET /sessions/{id}/account -----------------------------------------------

func getAccount(t *testing.T, h *harness, sessionID string) (*http.Response, map[string]string) {
	t.Helper()
	resp, err := http.Get(h.ts.URL + "/sessions/" + sessionID + "/account")
	if err != nil {
		t.Fatalf("GET account: %v", err)
	}
	t.Cleanup(func() { _ = resp.Body.Close() })
	var body map[string]string
	if resp.StatusCode == http.StatusOK {
		if err := json.NewDecoder(resp.Body).Decode(&body); err != nil {
			t.Fatalf("decode account: %v", err)
		}
	}
	return resp, body
}

func TestAccountNamesTheLoggedInEmail(t *testing.T) {
	// Arrange: the topbar's whole job is naming the account a session is
	// about to spend tokens as.
	cfgDir := t.TempDir()
	doc := `{"oauthAccount":{"emailAddress":"dodge@chess.com"}}`
	if err := os.WriteFile(filepath.Join(cfgDir, ".claude.json"), []byte(doc), 0o600); err != nil {
		t.Fatalf("write identity: %v", err)
	}
	h, _, _ := loginHarness(t, true)
	id := postCreate(t, h, fmt.Sprintf(`{"cwd":"/w","config_dir":%q}`, cfgDir))

	// Act
	resp, body := getAccount(t, h, id)

	// Assert
	if resp.StatusCode != http.StatusOK {
		t.Fatalf("status = %d, want 200", resp.StatusCode)
	}
	if body["email"] != "dodge@chess.com" {
		t.Errorf("email = %q, want dodge@chess.com", body["email"])
	}
	if body["config_dir"] != cfgDir {
		t.Errorf("config_dir = %q, want %q", body["config_dir"], cfgDir)
	}
}

func TestAccountReportsLoggedOutRatherThanFailing(t *testing.T) {
	// Arrange: a config root with no identity file is logged out, which the
	// topbar renders. It is not a server failure.
	h, _, _ := loginHarness(t, true)
	id := postCreate(t, h, fmt.Sprintf(`{"cwd":"/w","config_dir":%q}`, t.TempDir()))

	// Act
	resp, body := getAccount(t, h, id)

	// Assert
	if resp.StatusCode != http.StatusOK {
		t.Fatalf("status = %d, want 200", resp.StatusCode)
	}
	if body["email"] != "" {
		t.Errorf("email = %q, want empty for a logged-out root", body["email"])
	}
}

func TestAccountOnAnUnknownSessionIs404(t *testing.T) {
	// Arrange
	h, _, _ := loginHarness(t, true)

	// Act
	resp, _ := getAccount(t, h, "s_nope")

	// Assert
	if resp.StatusCode != http.StatusNotFound {
		t.Fatalf("status = %d, want 404", resp.StatusCode)
	}
}

// --- GET /accounts ----------------------------------------------------------

// accountsHarness builds a daemon configured with the given canonical
// account roster.
func accountsHarness(t *testing.T, accounts []Account) *harness {
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
		Accounts: accounts,
	})
	ts := httptest.NewServer(srv.Handler())
	t.Cleanup(ts.Close)
	return &harness{ts: ts, srv: srv, shims: shims}
}

// getAccounts reads the canonical account roster back over HTTP.
func getAccounts(t *testing.T, h *harness) (*http.Response, []map[string]string) {
	t.Helper()
	resp, err := http.Get(h.ts.URL + "/accounts")
	if err != nil {
		t.Fatalf("GET /accounts: %v", err)
	}
	defer resp.Body.Close()
	var body struct {
		Accounts []map[string]string `json:"accounts"`
	}
	if resp.StatusCode == http.StatusOK {
		if err := json.NewDecoder(resp.Body).Decode(&body); err != nil {
			t.Fatalf("decode /accounts body: %v", err)
		}
	}
	return resp, body.Accounts
}

// writeIdentity plants a .claude.json naming EMAIL under dir.
func writeIdentity(t *testing.T, dir, email string) {
	t.Helper()
	doc := fmt.Sprintf(`{"oauthAccount":{"emailAddress":%q}}`, email)
	if err := os.WriteFile(filepath.Join(dir, ".claude.json"), []byte(doc), 0o600); err != nil {
		t.Fatalf("write identity: %v", err)
	}
}

func TestAccountsIsUnconfiguredWithoutARoster(t *testing.T) {
	// Arrange: no -accounts flag means the menu has nothing to offer.
	h := accountsHarness(t, nil)

	// Act
	resp, _ := getAccounts(t, h)

	// Assert
	if resp.StatusCode != http.StatusServiceUnavailable {
		t.Fatalf("status = %d, want 503", resp.StatusCode)
	}
}

func TestAccountsReportsEachRootsIdentityInRosterOrder(t *testing.T) {
	// Arrange: two canonical roots, each logged into its own account.
	workDir := t.TempDir()
	writeIdentity(t, workDir, "dodge@chess.com")
	personalDir := t.TempDir()
	writeIdentity(t, personalDir, "dodge.w.coates@gmail.com")
	h := accountsHarness(t, []Account{
		{Label: "personal", ConfigDir: personalDir},
		{Label: "work", ConfigDir: workDir},
	})

	// Act
	resp, roster := getAccounts(t, h)

	// Assert
	if resp.StatusCode != http.StatusOK {
		t.Fatalf("status = %d, want 200", resp.StatusCode)
	}
	if len(roster) != 2 {
		t.Fatalf("roster size = %d, want 2", len(roster))
	}
	if roster[0]["label"] != "personal" || roster[0]["email"] != "dodge.w.coates@gmail.com" {
		t.Errorf("roster[0] = %v, want personal/dodge.w.coates@gmail.com", roster[0])
	}
	if roster[1]["label"] != "work" || roster[1]["email"] != "dodge@chess.com" {
		t.Errorf("roster[1] = %v, want work/dodge@chess.com", roster[1])
	}
}

func TestAccountsReportsALoggedOutRootWithAnEmptyEmail(t *testing.T) {
	// Arrange: a root with no identity file is logged out — a state the
	// menu renders, not an error.
	h := accountsHarness(t, []Account{{Label: "personal", ConfigDir: t.TempDir()}})

	// Act
	resp, roster := getAccounts(t, h)

	// Assert
	if resp.StatusCode != http.StatusOK {
		t.Fatalf("status = %d, want 200", resp.StatusCode)
	}
	if roster[0]["email"] != "" {
		t.Errorf("email = %q, want empty for a logged-out root", roster[0]["email"])
	}
	if roster[0]["error"] != "" {
		t.Errorf("error = %q, want none for a logged-out root", roster[0]["error"])
	}
}

// --- POST /sessions/{id}/account (switch) -----------------------------------

// switchHarness builds a registry-backed daemon with the given account
// roster — the configuration an account switch requires.
func switchHarness(t *testing.T, accounts []Account) *harness {
	t.Helper()
	shims := make(chan *fakeShim, 8)
	sink := &recordingSentinel{}
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
		Registry: registry.Open(filepath.Join(t.TempDir(), "sessions.json"), func(string, ...any) {}),
		Accounts: accounts,
	})
	ts := httptest.NewServer(srv.Handler())
	t.Cleanup(ts.Close)
	// Wait for every session to drain before the registry's TempDir is
	// removed: a session ending during cleanup fires a registrar
	// write-through, and that write racing RemoveAll flakes the test.
	// LIFO ordering makes this safe — the per-test shim.end cleanups run
	// first, so each Done here is already closing.
	t.Cleanup(func() {
		srv.mu.Lock()
		open := make([]*session.Session, 0, len(srv.sessions))
		for _, sess := range srv.sessions {
			open = append(open, sess)
		}
		srv.mu.Unlock()
		for _, sess := range open {
			select {
			case <-sess.Done():
			case <-time.After(recvTimeout):
				t.Errorf("session %s did not drain during cleanup", sess.ID)
			}
		}
	})
	return &harness{ts: ts, srv: srv, shims: shims, sink: sink}
}

// postSwitch asks the daemon to move sessionID onto configDir.
func postSwitch(t *testing.T, h *harness, sessionID, configDir string) (*http.Response, map[string]any) {
	t.Helper()
	body := fmt.Sprintf(`{"config_dir":%q}`, configDir)
	resp, err := http.Post(h.ts.URL+"/sessions/"+sessionID+"/account", "application/json", strings.NewReader(body))
	if err != nil {
		t.Fatalf("POST /sessions/%s/account: %v", sessionID, err)
	}
	defer resp.Body.Close()
	var out map[string]any
	if err := json.NewDecoder(resp.Body).Decode(&out); err != nil && err != io.EOF {
		t.Fatalf("decode switch body: %v", err)
	}
	return resp, out
}

// drainOnShutdown ends the fake shim when the daemon asks it to shut
// down, standing in for a real shim's graceful exit.
func drainOnShutdown(shim *fakeShim) {
	go func() {
		<-shim.sent
		shim.end()
	}()
}

// createForSwitch creates a session and hands back its shim — which
// postCreate would otherwise consume from h.shims — so a switch test can
// wire the shim's shutdown behavior itself.
func createForSwitch(t *testing.T, h *harness, body string) (string, *fakeShim) {
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
		return out.SessionID, shim
	case <-time.After(recvTimeout):
		t.Fatal("spawn was not invoked")
		return "", nil
	}
}

// plantTranscript writes a transcript for claudeSessionID under
// configDir/cwd and returns its path and content.
func plantTranscript(t *testing.T, configDir, cwd, claudeSessionID string) (string, string) {
	t.Helper()
	path := session.TranscriptPath(configDir, cwd, claudeSessionID)
	if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
		t.Fatalf("mkdir transcript dir: %v", err)
	}
	content := `{"type":"user","timestamp":"2026-01-01T00:00:00Z"}` + "\n"
	if err := os.WriteFile(path, []byte(content), 0o644); err != nil {
		t.Fatalf("write transcript: %v", err)
	}
	return path, content
}

func TestSwitchMigratesTheTranscriptAndRelaunchesUnderTheTargetRoot(t *testing.T) {
	// Arrange: a live resumed session on root A, switching to root B.
	cfgA, cfgB := t.TempDir(), t.TempDir()
	writeIdentity(t, cfgB, "dodge@chess.com")
	_, content := plantTranscript(t, cfgA, "/w", "u_1")
	h := switchHarness(t, []Account{
		{Label: "personal", ConfigDir: cfgA},
		{Label: "work", ConfigDir: cfgB},
	})
	id, shim := createForSwitch(t, h, fmt.Sprintf(`{"cwd":"/w","config_dir":%q,"resume":"u_1"}`, cfgA))
	drainOnShutdown(shim)

	// Act
	resp, body := postSwitch(t, h, id, cfgB)

	// Assert: accepted, renamed to the target identity...
	if resp.StatusCode != http.StatusAccepted {
		t.Fatalf("status = %d, want 202", resp.StatusCode)
	}
	acct, _ := body["account"].(map[string]any)
	if acct["email"] != "dodge@chess.com" {
		t.Errorf("account.email = %v, want dodge@chess.com", acct["email"])
	}
	// ...the transcript rode over...
	data, err := os.ReadFile(session.TranscriptPath(cfgB, "/w", "u_1"))
	if err != nil {
		t.Fatalf("migrated transcript: %v", err)
	}
	if string(data) != content {
		t.Errorf("migrated transcript = %q, want %q", data, content)
	}
	// ...a fresh shim spawned...
	select {
	case relaunched := <-h.shims:
		t.Cleanup(relaunched.end)
	case <-time.After(recvTimeout):
		t.Fatal("no relaunch shim spawned")
	}
	// ...and the record persisted the new root, non-terminally.
	rec, ok := h.srv.registry.Get(id)
	if !ok {
		t.Fatal("registry record vanished")
	}
	if rec.ConfigDir != cfgB {
		t.Errorf("record.ConfigDir = %q, want %q", rec.ConfigDir, cfgB)
	}
	if rec.Terminal {
		t.Errorf("record went terminal across a planned switch (reason %q)", rec.DeathReason)
	}
}

func TestSwitchOnAnUnknownSessionIs404(t *testing.T) {
	// Arrange
	h := switchHarness(t, []Account{{Label: "work", ConfigDir: "/w"}})

	// Act
	resp, _ := postSwitch(t, h, "s_nope", "/w")

	// Assert
	if resp.StatusCode != http.StatusNotFound {
		t.Fatalf("status = %d, want 404", resp.StatusCode)
	}
}

func TestSwitchRejectsARootOffTheRoster(t *testing.T) {
	// Arrange: a free-form target dir would strand the session on an
	// unauthenticated root, so only roster entries are switchable.
	cfgA := t.TempDir()
	h := switchHarness(t, []Account{{Label: "personal", ConfigDir: cfgA}})
	id := postCreate(t, h, fmt.Sprintf(`{"cwd":"/w","config_dir":%q}`, cfgA))

	// Act
	resp, _ := postSwitch(t, h, id, "/not/on/the/roster")

	// Assert
	if resp.StatusCode != http.StatusBadRequest {
		t.Fatalf("status = %d, want 400", resp.StatusCode)
	}
}

func TestSwitchToTheCurrentRootIsANoOp(t *testing.T) {
	// Arrange
	cfgA := t.TempDir()
	h := switchHarness(t, []Account{{Label: "personal", ConfigDir: cfgA}})
	id := postCreate(t, h, fmt.Sprintf(`{"cwd":"/w","config_dir":%q}`, cfgA))

	// Act
	resp, body := postSwitch(t, h, id, cfgA)

	// Assert: 200 not 202, switched:false, and no relaunch shim.
	if resp.StatusCode != http.StatusOK {
		t.Fatalf("status = %d, want 200", resp.StatusCode)
	}
	if body["switched"] != false {
		t.Errorf("switched = %v, want false", body["switched"])
	}
	select {
	case <-h.shims:
		t.Fatal("a no-op switch relaunched the shim")
	default:
	}
}

func TestSwitchRefusesWhileATurnIsInFlight(t *testing.T) {
	// Arrange: a mid-generation shim bounce would kill the turn, so the
	// switch refuses exactly as the Emacs daemon-stop guard does.
	cfgA, cfgB := t.TempDir(), t.TempDir()
	h := switchHarness(t, []Account{
		{Label: "personal", ConfigDir: cfgA},
		{Label: "work", ConfigDir: cfgB},
	})
	id := postCreate(t, h, fmt.Sprintf(`{"cwd":"/w","config_dir":%q}`, cfgA))
	resp, err := http.Post(h.ts.URL+"/sessions/"+id+"/message", "application/json",
		strings.NewReader(`{"content":"hi"}`))
	if err != nil || resp.StatusCode != http.StatusAccepted {
		t.Fatalf("prime turn: %v (status %d)", err, resp.StatusCode)
	}
	resp.Body.Close()

	// Act
	resp2, _ := postSwitch(t, h, id, cfgB)

	// Assert
	if resp2.StatusCode != http.StatusConflict {
		t.Fatalf("status = %d, want 409", resp2.StatusCode)
	}
}

func TestSwitchOnAnEndedSessionIs409(t *testing.T) {
	// Arrange: an abnormally dead shim marks the conversation terminal;
	// there is nothing left to switch.
	cfgA, cfgB := t.TempDir(), t.TempDir()
	h := switchHarness(t, []Account{
		{Label: "personal", ConfigDir: cfgA},
		{Label: "work", ConfigDir: cfgB},
	})
	id, shim := createForSwitch(t, h, fmt.Sprintf(`{"cwd":"/w","config_dir":%q}`, cfgA))
	shim.end()
	<-h.srv.lookup(id).Done()

	// Act
	resp, _ := postSwitch(t, h, id, cfgB)

	// Assert
	if resp.StatusCode != http.StatusConflict {
		t.Fatalf("status = %d, want 409", resp.StatusCode)
	}
}

func TestSwitchIsUnconfiguredWithoutARoster(t *testing.T) {
	// Arrange
	h := switchHarness(t, nil)

	// Act
	resp, _ := postSwitch(t, h, "s_any", "/w")

	// Assert
	if resp.StatusCode != http.StatusServiceUnavailable {
		t.Fatalf("status = %d, want 503", resp.StatusCode)
	}
}

func TestSwitchIsUnconfiguredWithoutARegistry(t *testing.T) {
	// Arrange: no registry means no durable record to move — the
	// accountsHarness deliberately carries none.
	h := accountsHarness(t, []Account{{Label: "work", ConfigDir: "/w"}})

	// Act
	resp, _ := postSwitch(t, h, "s_any", "/w")

	// Assert
	if resp.StatusCode != http.StatusServiceUnavailable {
		t.Fatalf("status = %d, want 503", resp.StatusCode)
	}
}

func TestSwitchOfAFreshSessionRelaunchesWithoutATranscript(t *testing.T) {
	// Arrange: a session that never reported a claude_session_id has no
	// transcript to migrate; the switch relaunches it fresh rather than
	// failing on a copy it does not need.
	cfgA, cfgB := t.TempDir(), t.TempDir()
	h := switchHarness(t, []Account{
		{Label: "personal", ConfigDir: cfgA},
		{Label: "work", ConfigDir: cfgB},
	})
	id, shim := createForSwitch(t, h, fmt.Sprintf(`{"cwd":"/w","config_dir":%q}`, cfgA))
	drainOnShutdown(shim)

	// Act
	resp, _ := postSwitch(t, h, id, cfgB)

	// Assert
	if resp.StatusCode != http.StatusAccepted {
		t.Fatalf("status = %d, want 202", resp.StatusCode)
	}
	select {
	case relaunched := <-h.shims:
		t.Cleanup(relaunched.end)
	case <-time.After(recvTimeout):
		t.Fatal("no relaunch shim spawned")
	}
	if rec, _ := h.srv.registry.Get(id); rec.ConfigDir != cfgB {
		t.Errorf("record.ConfigDir = %q, want %q", rec.ConfigDir, cfgB)
	}
}

func TestSwitchPokesEmacsOverTheSentinelChannel(t *testing.T) {
	// Arrange: Emacs's per-workspace config-dir override must follow the
	// switch, and the sentinel side channel is how it learns.
	cfgA, cfgB := t.TempDir(), t.TempDir()
	plantTranscript(t, cfgA, "/w", "u_1")
	h := switchHarness(t, []Account{
		{Label: "personal", ConfigDir: cfgA},
		{Label: "work", ConfigDir: cfgB},
	})
	id, shim := createForSwitch(t, h, fmt.Sprintf(`{"cwd":"/w","config_dir":%q,"resume":"u_1"}`, cfgA))
	drainOnShutdown(shim)

	// Act
	resp, _ := postSwitch(t, h, id, cfgB)

	// Assert
	if resp.StatusCode != http.StatusAccepted {
		t.Fatalf("status = %d, want 202", resp.StatusCode)
	}
	if got := h.sink.snapshot(); !slices.Contains(got, "account-changed /w u_1") {
		t.Errorf("sentinel calls = %v, want account-changed /w u_1", got)
	}
	select {
	case relaunched := <-h.shims:
		t.Cleanup(relaunched.end)
	case <-time.After(recvTimeout):
		t.Fatal("no relaunch shim spawned")
	}
}

func TestSwitchTimesOutWhenTheOldShimNeverDrains(t *testing.T) {
	// Arrange: a shim that ignores its shutdown must not hang the switch
	// forever, and a timed-out switch must leave the session unchanged.
	old := switchDrainTimeout
	switchDrainTimeout = 50 * time.Millisecond
	t.Cleanup(func() { switchDrainTimeout = old })
	cfgA, cfgB := t.TempDir(), t.TempDir()
	h := switchHarness(t, []Account{
		{Label: "personal", ConfigDir: cfgA},
		{Label: "work", ConfigDir: cfgB},
	})
	id, shim := createForSwitch(t, h, fmt.Sprintf(`{"cwd":"/w","config_dir":%q}`, cfgA))

	// Act: nobody drains the shim.
	resp, _ := postSwitch(t, h, id, cfgB)

	// Assert
	if resp.StatusCode != http.StatusGatewayTimeout {
		t.Fatalf("status = %d, want 504", resp.StatusCode)
	}
	if rec, _ := h.srv.registry.Get(id); rec.ConfigDir != cfgA {
		t.Errorf("record.ConfigDir = %q, want unchanged %q", rec.ConfigDir, cfgA)
	}
	// End the ignored shim so the harness cleanup's drain-wait is
	// deterministic rather than riding its timeout.
	shim.end()
}

func TestAccountsSurfacesACorruptRootWithoutHidingTheHealthyOne(t *testing.T) {
	// Arrange: one corrupt identity file beside one healthy root. The
	// corrupt one must carry its error in-band, never masquerade as
	// logged out, and never fail the whole roster.
	corruptDir := t.TempDir()
	if err := os.WriteFile(filepath.Join(corruptDir, ".claude.json"), []byte("{not json"), 0o600); err != nil {
		t.Fatalf("write corrupt identity: %v", err)
	}
	healthyDir := t.TempDir()
	writeIdentity(t, healthyDir, "dodge@chess.com")
	h := accountsHarness(t, []Account{
		{Label: "personal", ConfigDir: corruptDir},
		{Label: "work", ConfigDir: healthyDir},
	})

	// Act
	resp, roster := getAccounts(t, h)

	// Assert
	if resp.StatusCode != http.StatusOK {
		t.Fatalf("status = %d, want 200", resp.StatusCode)
	}
	if roster[0]["error"] == "" {
		t.Errorf("corrupt root carried no error")
	}
	if roster[1]["email"] != "dodge@chess.com" {
		t.Errorf("healthy root email = %q, want dodge@chess.com", roster[1]["email"])
	}
}

// --- GET /sessions/{id}/commands and POST .../commands/refresh -------------------

// getCommands reads the session's slash-command menu back over HTTP.
func (h *harness) getCommands(t *testing.T, sessionID string) (*http.Response, []protocol.SlashCommand) {
	t.Helper()
	resp, err := http.Get(h.ts.URL + "/sessions/" + sessionID + "/commands")
	if err != nil {
		t.Fatalf("GET commands: %v", err)
	}
	t.Cleanup(func() { _ = resp.Body.Close() })
	if resp.StatusCode != http.StatusOK {
		return resp, nil
	}
	var body struct {
		Commands []protocol.SlashCommand `json:"commands"`
	}
	if err := json.NewDecoder(resp.Body).Decode(&body); err != nil {
		t.Fatalf("decode commands: %v", err)
	}
	return resp, body.Commands
}

// awaitCommands polls the menu until it is non-empty: the shim's `commands`
// event crosses a channel, so it lands a moment after pushEvent returns.
func (h *harness) awaitCommands(t *testing.T, sessionID string) []protocol.SlashCommand {
	t.Helper()
	deadline := time.Now().Add(recvTimeout)
	for time.Now().Before(deadline) {
		if _, cmds := h.getCommands(t, sessionID); len(cmds) > 0 {
			return cmds
		}
		time.Sleep(5 * time.Millisecond)
	}
	t.Fatal("the commands menu never landed")
	return nil
}

func TestGetCommandsServesTheMenuTheShimPublished(t *testing.T) {
	// Arrange
	h := newHarness(t)
	id, shim := h.createSession(t)
	// Act
	shim.pushEvent(t, `{"type":"commands","session_id":"s1","commands":[{"name":"debug-logs","description":"read the log","argumentHint":""}]}`)
	cmds := h.awaitCommands(t, id)
	// Assert
	if len(cmds) != 1 || cmds[0].Name != "debug-logs" {
		t.Errorf("commands = %+v, want one debug-logs entry", cmds)
	}
}

func TestGetCommandsCarriesTheArgumentHint(t *testing.T) {
	// Arrange — the hint is what the completion annotation renders, so it
	// has to survive the trip rather than being dropped as cosmetic.
	h := newHarness(t)
	id, shim := h.createSession(t)
	// Act
	shim.pushEvent(t, `{"type":"commands","session_id":"s1","commands":[{"name":"compact","description":"d","argumentHint":"<how>"}]}`)
	cmds := h.awaitCommands(t, id)
	// Assert
	if cmds[0].ArgumentHint != "<how>" {
		t.Errorf("argument hint = %q, want %q", cmds[0].ArgumentHint, "<how>")
	}
}

func TestGetCommandsAnswersAnEmptyListBeforeTheMenuLands(t *testing.T) {
	// Arrange — the menu resolves asynchronously off the SDK init handshake.
	h := newHarness(t)
	id, _ := h.createSession(t)
	// Act — ask before any `commands` event has been pushed.
	resp, cmds := h.getCommands(t, id)
	// Assert — early is not an error, and the list is [] rather than null so
	// the reader never has to tell the two apart.
	if resp.StatusCode != http.StatusOK {
		t.Errorf("status = %d, want 200", resp.StatusCode)
	}
	if len(cmds) != 0 {
		t.Errorf("commands = %+v, want empty", cmds)
	}
}

func TestGetCommandsIs404ForAnUnknownSession(t *testing.T) {
	// Arrange
	h := newHarness(t)
	// Act
	resp, _ := h.getCommands(t, "s_nope")
	// Assert
	if resp.StatusCode != http.StatusNotFound {
		t.Errorf("status = %d, want 404", resp.StatusCode)
	}
}

func TestRefreshCommandsForwardsTheCommandToTheShim(t *testing.T) {
	// Arrange
	h := newHarness(t)
	id, shim := h.createSession(t)
	// Act
	resp, err := http.Post(h.ts.URL+"/sessions/"+id+"/commands/refresh", "application/json", nil)
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
		if cmd.Type != "refresh-commands" {
			t.Errorf("forwarded type = %q, want refresh-commands", cmd.Type)
		}
		if cmd.RequestID == "" {
			t.Error("forwarded refresh-commands carries no request_id, so it could never be acked")
		}
	case <-time.After(recvTimeout):
		t.Fatal("refresh-commands never reached the shim")
	}
}

func TestRefreshCommandsReplacesTheCachedMenu(t *testing.T) {
	// Arrange — a session whose menu predates a skill being added.
	h := newHarness(t)
	id, shim := h.createSession(t)
	shim.pushEvent(t, `{"type":"commands","session_id":"s1","commands":[{"name":"old","description":"d","argumentHint":""}]}`)
	h.awaitCommands(t, id)
	// Act — the re-probe reports the new skill.
	shim.pushEvent(t, `{"type":"commands","session_id":"s1","commands":[{"name":"old","description":"d","argumentHint":""},{"name":"brand-new","description":"d","argumentHint":""}]}`)
	deadline := time.Now().Add(recvTimeout)
	var cmds []protocol.SlashCommand
	for time.Now().Before(deadline) {
		if _, c := h.getCommands(t, id); len(c) == 2 {
			cmds = c
			break
		}
		time.Sleep(5 * time.Millisecond)
	}
	// Assert
	if len(cmds) != 2 || cmds[1].Name != "brand-new" {
		t.Errorf("commands = %+v, want the refreshed pair including brand-new", cmds)
	}
}

func TestRefreshCommandsIs404ForAnUnknownSession(t *testing.T) {
	// Arrange
	h := newHarness(t)
	// Act
	resp, err := http.Post(h.ts.URL+"/sessions/s_nope/commands/refresh", "application/json", nil)
	if err != nil {
		t.Fatalf("POST refresh: %v", err)
	}
	defer resp.Body.Close()
	// Assert
	if resp.StatusCode != http.StatusNotFound {
		t.Errorf("status = %d, want 404", resp.StatusCode)
	}
}

// getTaskOutput requests a detached task's tail, decoding the JSON body on
// a 200 and leaving it nil otherwise (so status-only cases stay terse).
func (h *harness) getTaskOutput(t *testing.T, sessionID, taskID, query string) (*http.Response, map[string]any) {
	t.Helper()
	resp, err := http.Get(h.ts.URL + "/sessions/" + sessionID + "/tasks/" + taskID + "/output" + query)
	if err != nil {
		t.Fatalf("GET task output: %v", err)
	}
	t.Cleanup(func() { _ = resp.Body.Close() })
	if resp.StatusCode != http.StatusOK {
		return resp, nil
	}
	var body map[string]any
	if err := json.NewDecoder(resp.Body).Decode(&body); err != nil {
		t.Fatalf("decode task output: %v", err)
	}
	return resp, body
}

func TestTaskOutputIs404ForAnUnknownSession(t *testing.T) {
	// Arrange
	h := newHarness(t)
	// Act
	resp, _ := h.getTaskOutput(t, "s_nope", "bg1", "")
	// Assert
	if resp.StatusCode != http.StatusNotFound {
		t.Errorf("status = %d, want 404", resp.StatusCode)
	}
}

func TestTaskOutputIs404ForAnUnknownTask(t *testing.T) {
	// Arrange
	h := newHarness(t)
	id, _ := h.createSession(t)
	// Act — the session exists but recorded no such task.
	resp, _ := h.getTaskOutput(t, id, "bg-nope", "")
	// Assert
	if resp.StatusCode != http.StatusNotFound {
		t.Errorf("status = %d, want 404", resp.StatusCode)
	}
}

func TestTaskOutputRejectsANegativeOffset(t *testing.T) {
	// Arrange — the offset is a byte cursor, so a negative one is malformed.
	h := newHarness(t)
	id, _ := h.createSession(t)
	// Act
	resp, _ := h.getTaskOutput(t, id, "bg1", "?offset=-1")
	// Assert
	if resp.StatusCode != http.StatusBadRequest {
		t.Errorf("status = %d, want 400", resp.StatusCode)
	}
}

func TestTaskOutputServesARecordedTask(t *testing.T) {
	// Arrange — announce a spawn so the session records the task's spool path.
	h := newHarness(t)
	id, shim := h.createSession(t)
	shim.pushEvent(t, `{"type":"tool-result","tool_use_id":"tu1","content":"with ID: bg1. Output is being written to: /tmp/claude-0/srv/tasks/bg1.output."}`)
	// Act — poll until the recording crosses the shim channel.
	var body map[string]any
	deadline := time.Now().Add(recvTimeout)
	for time.Now().Before(deadline) {
		if resp, b := h.getTaskOutput(t, id, "bg1", ""); resp.StatusCode == http.StatusOK {
			body = b
			break
		}
		time.Sleep(5 * time.Millisecond)
	}
	// Assert — a well-formed poll response, still live (no notification yet).
	if body == nil {
		t.Fatal("task output never became available")
	}
	if _, ok := body["offset"]; !ok {
		t.Errorf("body missing offset: %v", body)
	}
	if body["done"] != false {
		t.Errorf("done = %v, want false", body["done"])
	}
}

// createSessionAt creates a session whose shim runs in cwd.
func (h *harness) createSessionAt(t *testing.T, cwd string) string {
	t.Helper()
	body := fmt.Sprintf(`{"cwd":%q}`, cwd)
	resp, err := http.Post(h.ts.URL+"/sessions", "application/json", strings.NewReader(body))
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
		t.Fatalf("decode create response: %v", err)
	}
	select {
	case shim := <-h.shims:
		t.Cleanup(shim.end)
	case <-time.After(recvTimeout):
		t.Fatal("spawn was not invoked")
	}
	return out.SessionID
}

// getChessGame fetches the chess-game route for path on session id.
func (h *harness) getChessGame(t *testing.T, id, path string) *http.Response {
	t.Helper()
	resp, err := http.Get(h.ts.URL + "/sessions/" + id + "/chess-game?path=" + url.QueryEscape(path))
	if err != nil {
		t.Fatalf("GET chess-game: %v", err)
	}
	t.Cleanup(func() { _ = resp.Body.Close() })
	return resp
}

func TestChessGamePathValidation(t *testing.T) {
	// Arrange
	cwd := filepath.Join(string(filepath.Separator), "ws", "root")
	dir := filepath.Join(cwd, ".claude", "emacs", "cee-web-widget")
	tests := []struct {
		name    string
		raw     string
		wantErr bool
	}{
		{"file directly inside the widget dir", filepath.Join(dir, "chess-game-abc123.pgn"), false},
		{"file at the worktree root", filepath.Join(cwd, "chess-game-abc123.pgn"), true},
		{"traversal escaping the widget dir", filepath.Join(dir, "..", "..", "..", "chess-game-abc123.pgn"), true},
		{"file nested one level deeper", filepath.Join(dir, "sub", "chess-game-abc123.pgn"), true},
		{"name missing the chess-game- prefix", filepath.Join(dir, "game-abc123.pgn"), true},
		{"another worktree's widget dir", filepath.Join(string(filepath.Separator), "other", ".claude", "emacs", "cee-web-widget", "chess-game-abc123.pgn"), true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			_, err := chessGamePath(cwd, tt.raw)
			// Assert
			if gotErr := err != nil; gotErr != tt.wantErr {
				t.Errorf("chessGamePath(%q) error = %v, wantErr %t", tt.raw, err, tt.wantErr)
			}
		})
	}
}

func TestChessGameFileServesAValidatedFile(t *testing.T) {
	// Arrange
	h := newHarness(t)
	cwd := t.TempDir()
	dir := filepath.Join(cwd, ".claude", "emacs", "cee-web-widget")
	if err := os.MkdirAll(dir, 0o755); err != nil {
		t.Fatalf("mkdir: %v", err)
	}
	path := filepath.Join(dir, "chess-game-abc123.pgn")
	if err := os.WriteFile(path, []byte("1. e4 e5 *"), 0o644); err != nil {
		t.Fatalf("write: %v", err)
	}
	id := h.createSessionAt(t, cwd)
	// Act
	resp := h.getChessGame(t, id, path)
	// Assert
	if resp.StatusCode != http.StatusOK {
		t.Fatalf("status = %d, want 200", resp.StatusCode)
	}
	body, err := io.ReadAll(resp.Body)
	if err != nil {
		t.Fatalf("read body: %v", err)
	}
	if string(body) != "1. e4 e5 *" {
		t.Errorf("body = %q, want the file content", body)
	}
}

func TestChessGameFileIs404ForAnUnknownSession(t *testing.T) {
	// Arrange
	h := newHarness(t)
	// Act
	resp := h.getChessGame(t, "s_nope", "/anywhere/chess-game-x.pgn")
	// Assert
	if resp.StatusCode != http.StatusNotFound {
		t.Errorf("status = %d, want 404", resp.StatusCode)
	}
}

func TestChessGameFileIs400WithoutAPath(t *testing.T) {
	// Arrange
	h := newHarness(t)
	id := h.createSessionAt(t, t.TempDir())
	// Act
	resp := h.getChessGame(t, id, "")
	// Assert
	if resp.StatusCode != http.StatusBadRequest {
		t.Errorf("status = %d, want 400", resp.StatusCode)
	}
}

func TestChessGameFileIs403OutsideTheSessionWorktree(t *testing.T) {
	// Arrange
	h := newHarness(t)
	id := h.createSessionAt(t, t.TempDir())
	// Act
	resp := h.getChessGame(t, id, filepath.Join(t.TempDir(), ".claude", "emacs", "cee-web-widget", "chess-game-x.pgn"))
	// Assert
	if resp.StatusCode != http.StatusForbidden {
		t.Errorf("status = %d, want 403", resp.StatusCode)
	}
}

func TestChessGameFileIs404ForAMissingFile(t *testing.T) {
	// Arrange
	h := newHarness(t)
	cwd := t.TempDir()
	id := h.createSessionAt(t, cwd)
	// Act
	resp := h.getChessGame(t, id, filepath.Join(cwd, ".claude", "emacs", "cee-web-widget", "chess-game-missing.pgn"))
	// Assert
	if resp.StatusCode != http.StatusNotFound {
		t.Errorf("status = %d, want 404", resp.StatusCode)
	}
}

// --- interrupt-with-retract (§2.3 user-turn-retracted) ------------------------

// openTurn sends a prompt over HTTP and returns its request id, leaving the
// session with that turn in flight and unanswered.
func openTurn(t *testing.T, h *harness, id string, shim *fakeShim) string {
	t.Helper()
	resp, err := http.Post(h.ts.URL+"/sessions/"+id+"/message", "application/json",
		bytes.NewBufferString(`{"content":"draft prompt"}`))
	if err != nil {
		t.Fatalf("POST message: %v", err)
	}
	defer resp.Body.Close()
	var body struct {
		RequestID string `json:"request_id"`
	}
	if err := json.NewDecoder(resp.Body).Decode(&body); err != nil {
		t.Fatalf("decode: %v", err)
	}
	<-shim.sent // the forwarded user-message
	return body.RequestID
}

// postInterrupt interrupts over HTTP with an optional retract target, and
// reports what the route said about the retraction.
func postInterrupt(t *testing.T, h *harness, id, retractID string) bool {
	t.Helper()
	resp, err := http.Post(h.ts.URL+"/sessions/"+id+"/interrupt", "application/json",
		bytes.NewBufferString(`{"retract_request_id":"`+retractID+`"}`))
	if err != nil {
		t.Fatalf("POST interrupt: %v", err)
	}
	defer resp.Body.Close()
	if resp.StatusCode != http.StatusAccepted {
		t.Fatalf("status = %d, want 202", resp.StatusCode)
	}
	var body struct {
		Retracted bool `json:"retracted"`
	}
	if err := json.NewDecoder(resp.Body).Decode(&body); err != nil {
		t.Fatalf("decode: %v", err)
	}
	return body.Retracted
}

func TestInterruptRouteRetractsAnUnansweredTurn(t *testing.T) {
	// Arrange — a prompt in flight that the agent never answered.
	h := newHarness(t)
	id, shim := h.createSession(t)
	conn := h.dial(t, id)
	readFrame(t, conn) // hello
	rid := openTurn(t, h, id, shim)
	readFrame(t, conn) // user-turn
	// Act
	retracted := postInterrupt(t, h, id, rid)
	// Assert — the route reports the retraction and the feed sees it.
	if !retracted {
		t.Fatal(`"retracted" = false for an unanswered turn, want true`)
	}
	readFrame(t, conn) // interrupt
	frame := readFrame(t, conn)
	if frame["type"] != "user-turn-retracted" || frame["request_id"] != rid {
		t.Errorf("frame = %v, want user-turn-retracted for %s", frame, rid)
	}
}

func TestInterruptRouteReportsNoRetractionForAStaleRequestID(t *testing.T) {
	// Arrange — a turn in flight under a different id than Emacs believes.
	h := newHarness(t)
	id, shim := h.createSession(t)
	openTurn(t, h, id, shim)
	// Act
	retracted := postInterrupt(t, h, id, "r_stale")
	// Assert — Emacs must not restore a prompt the feed still shows.
	if retracted {
		t.Error(`"retracted" = true for a stale request id, want false`)
	}
}

func TestInterruptRouteWithoutRetractTargetReportsNoRetraction(t *testing.T) {
	// Arrange — a retractable turn, interrupted by the plain gesture.
	h := newHarness(t)
	id, shim := h.createSession(t)
	openTurn(t, h, id, shim)
	// Act — the body names no turn, so no undo is being asked for.
	retracted := postInterrupt(t, h, id, "")
	// Assert — a plain interrupt never withdraws a bubble.
	if retracted {
		t.Error(`"retracted" = true for an untargeted interrupt, want false`)
	}
}

func TestInterruptRouteStillForwardsWhenRetracting(t *testing.T) {
	// Arrange — a retractable turn.
	h := newHarness(t)
	id, shim := h.createSession(t)
	rid := openTurn(t, h, id, shim)
	// Act
	postInterrupt(t, h, id, rid)
	// Assert — an undo stops the turn too, so the shim still hears the gesture.
	select {
	case line := <-shim.sent:
		if !strings.Contains(string(line), `"interrupt"`) {
			t.Errorf("forwarded = %s", line)
		}
	case <-time.After(recvTimeout):
		t.Fatal("interrupt not forwarded to shim")
	}
}

func TestInterruptRouteRejectsAMalformedBody(t *testing.T) {
	// Arrange
	h := newHarness(t)
	id, _ := h.createSession(t)
	// Act
	resp, err := http.Post(h.ts.URL+"/sessions/"+id+"/interrupt", "application/json",
		bytes.NewBufferString(`{"retract_request_id":`))
	if err != nil {
		t.Fatalf("POST interrupt: %v", err)
	}
	defer resp.Body.Close()
	// Assert — a body the client mangled is the client's bug to hear.
	if resp.StatusCode != http.StatusBadRequest {
		t.Errorf("status = %d, want 400", resp.StatusCode)
	}
}
