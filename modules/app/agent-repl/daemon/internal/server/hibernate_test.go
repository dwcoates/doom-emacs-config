package server

import (
	"bytes"
	"encoding/json"
	"net/http"
	"net/http/httptest"
	"strings"
	"sync/atomic"
	"testing"
	"time"

	"github.com/gorilla/websocket"

	"claude-repld/internal/session"
)

// sweepHarness is a server with a controllable clock and an UNBUFFERED
// idle-sweep tick channel. The unbuffered channel is the synchronization
// backbone: the sweeper receives a tick only when it is idle between
// sweeps, so a second accepted send is a happens-before barrier proving
// the first sweep fully completed (see sweepOnce).
type sweepHarness struct {
	ts    *httptest.Server
	srv   *Server
	shims chan *fakeShim
	ticks chan time.Time
	nowNs *atomic.Int64
}

func newSweepHarness(t *testing.T, idleTimeout time.Duration) *sweepHarness {
	t.Helper()
	shims := make(chan *fakeShim, 8)
	ticks := make(chan time.Time) // unbuffered on purpose
	var nowNs atomic.Int64
	nowNs.Store(time.Date(2026, 5, 24, 12, 0, 0, 0, time.UTC).UnixNano())
	srv := New(Config{
		DaemonVersion:  "0.1.0-test",
		Retention:      64,
		Logf:           func(string, ...any) {},
		Now:            func() time.Time { return time.Unix(0, nowNs.Load()).UTC() },
		IdleTimeout:    idleTimeout,
		IdleSweepTicks: ticks,
		Spawn: func(string, CreateOpts) (session.ShimHandle, error) {
			shim := newFakeShim()
			shims <- shim
			return shim, nil
		},
	})
	ts := httptest.NewServer(srv.Handler())
	t.Cleanup(ts.Close)
	t.Cleanup(srv.ShutdownAll) // closes the sweeper and drains sessions
	return &sweepHarness{ts: ts, srv: srv, shims: shims, ticks: ticks, nowNs: &nowNs}
}

// advance moves the shared clock forward.
func (h *sweepHarness) advance(d time.Duration) {
	h.nowNs.Add(int64(d))
}

// sweepOnce triggers a sweep and blocks until it has fully completed. The
// first send hands a tick to the sweeper; the second is accepted only
// after the first sweepIdle() returns, so when it lands the first sweep's
// effects are settled.
func (h *sweepHarness) sweepOnce(t *testing.T) {
	t.Helper()
	for range 2 {
		select {
		case h.ticks <- time.Time{}:
		case <-time.After(recvTimeout):
			t.Fatal("sweeper is not receiving ticks")
		}
	}
}

// createSession over a sweepHarness (mirrors the main harness helper).
func (h *sweepHarness) createSession(t *testing.T) (string, *fakeShim) {
	t.Helper()
	resp, err := http.Post(h.ts.URL+"/sessions", "application/json", bytes.NewBufferString(`{}`))
	if err != nil {
		t.Fatalf("POST /sessions: %v", err)
	}
	defer resp.Body.Close()
	var body struct {
		SessionID string `json:"session_id"`
	}
	if err := json.NewDecoder(resp.Body).Decode(&body); err != nil {
		t.Fatalf("decode: %v", err)
	}
	return body.SessionID, <-h.shims
}

func (h *sweepHarness) dial(t *testing.T, id string) *websocket.Conn {
	t.Helper()
	wsURL := "ws" + strings.TrimPrefix(h.ts.URL, "http") + "/sessions/" + id + "/stream"
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

// initOverStream attaches a client and drives system:init so the session
// gains a claude_session_id (the resume target hibernation requires),
// reading through to the init frame so the translator has processed it.
func initOverStream(t *testing.T, conn *websocket.Conn, shim *fakeShim, claudeID string) {
	t.Helper()
	readFrame(t, conn) // hello
	shim.pushEvent(t, `{"type":"system","session_id":"s","uuid":"u","subtype":"init","data":{"session_id":"`+claudeID+`"}}`)
	for {
		if readFrame(t, conn)["type"] == "system" {
			return
		}
	}
}

// recvShutdown asserts the next forwarded command to shim is a shutdown.
func recvShutdown(t *testing.T, shim *fakeShim) {
	t.Helper()
	select {
	case line := <-shim.sent:
		if !strings.Contains(string(line), "shutdown") {
			t.Fatalf("forwarded = %s, want shutdown", line)
		}
	case <-time.After(recvTimeout):
		t.Fatal("no shutdown command was forwarded")
	}
}

// expectNoForward fails if any command reaches shim (used to prove a skip).
func expectNoForward(t *testing.T, shim *fakeShim) {
	t.Helper()
	select {
	case line := <-shim.sent:
		t.Fatalf("unexpected forwarded command %s (session should have been skipped)", line)
	default:
	}
}

func TestIdleSweepHibernatesIdleSession(t *testing.T) {
	// Arrange — an initialized, idle session.
	h := newSweepHarness(t, 10*time.Minute)
	id, shim := h.createSession(t)
	conn := h.dial(t, id)
	initOverStream(t, conn, shim, "cli-uuid-1")
	// Act — idle past the timeout, then sweep.
	h.advance(11 * time.Minute)
	h.sweepOnce(t)
	// Assert — the sweeper asked the shim to shut down (hibernation).
	recvShutdown(t, shim)
}

func TestIdleSweepSkipsSessionWithinTimeout(t *testing.T) {
	// Arrange — an initialized session that is NOT yet idle.
	h := newSweepHarness(t, 10*time.Minute)
	id, shim := h.createSession(t)
	conn := h.dial(t, id)
	initOverStream(t, conn, shim, "cli-uuid-1")
	// Act — advance only a little, then sweep.
	h.advance(1 * time.Minute)
	h.sweepOnce(t)
	// Assert — untouched.
	expectNoForward(t, shim)
}

func TestIdleSweepSkipsSessionWithoutResumeTarget(t *testing.T) {
	// Arrange — an idle session that never received system:init, so it has
	// no resume target and hibernating it would destroy it.
	h := newSweepHarness(t, 10*time.Minute)
	_, shim := h.createSession(t)
	// Act — idle past the timeout, then sweep.
	h.advance(11 * time.Minute)
	h.sweepOnce(t)
	// Assert — skipped, not hibernated.
	expectNoForward(t, shim)
}

func TestActRevivesHibernatedSession(t *testing.T) {
	// Arrange — an initialized session, hibernated by a sweep.
	h := newSweepHarness(t, 10*time.Minute)
	id, shim := h.createSession(t)
	conn := h.dial(t, id)
	initOverStream(t, conn, shim, "cli-uuid-1")
	h.advance(11 * time.Minute)
	h.sweepOnce(t)
	recvShutdown(t, shim)
	shim.end() // CLI honors shutdown
	sess := h.srv.lookup(id)
	select {
	case <-sess.HibernateDone():
	case <-time.After(recvTimeout):
		t.Fatal("session never settled into hibernation")
	}
	// Act — an HTTP send is an act, so it must revive the CLI.
	resp, err := http.Post(h.ts.URL+"/sessions/"+id+"/message", "application/json",
		bytes.NewBufferString(`{"content":"back?"}`))
	if err != nil {
		t.Fatalf("POST message: %v", err)
	}
	defer resp.Body.Close()
	// Assert — a fresh shim was spawned (revive), and the session is live.
	select {
	case <-h.shims:
	case <-time.After(recvTimeout):
		t.Fatal("act did not revive: no shim was spawned")
	}
	if h.srv.lookup(id).Hibernated() {
		t.Fatal("session still hibernated after an act")
	}
}

func TestStreamAttachDoesNotReviveHibernatedSession(t *testing.T) {
	// Arrange — an initialized session, hibernated by a sweep.
	h := newSweepHarness(t, 10*time.Minute)
	id, shim := h.createSession(t)
	conn := h.dial(t, id)
	initOverStream(t, conn, shim, "cli-uuid-1")
	h.advance(11 * time.Minute)
	h.sweepOnce(t)
	recvShutdown(t, shim)
	shim.end()
	sess := h.srv.lookup(id)
	select {
	case <-sess.HibernateDone():
	case <-time.After(recvTimeout):
		t.Fatal("session never settled into hibernation")
	}
	// Act — a NEW observer attaches (a workspace switch). resolveForAttach
	// runs fully in the handler before the hello is written, so once the
	// hello lands the spawn decision has already been made.
	conn2 := h.dial(t, id)
	hello := readFrame(t, conn2)
	// Assert — history is served (non-zero cursor) with NO new CLI.
	if hello["type"] != "hello" {
		t.Fatalf("frame = %v, want hello", hello)
	}
	select {
	case <-h.shims:
		t.Fatal("stream attach revived a hibernated session; observation must stay free")
	default:
	}
	if !h.srv.lookup(id).Hibernated() {
		t.Fatal("attach un-hibernated the session")
	}
}

func TestHibernatedSessionListedAsNonTerminal(t *testing.T) {
	// Arrange — an initialized session, hibernated by a sweep.
	h := newSweepHarness(t, 10*time.Minute)
	id, shim := h.createSession(t)
	conn := h.dial(t, id)
	initOverStream(t, conn, shim, "cli-uuid-1")
	h.advance(11 * time.Minute)
	h.sweepOnce(t)
	recvShutdown(t, shim)
	shim.end()
	sess := h.srv.lookup(id)
	select {
	case <-sess.HibernateDone():
	case <-time.After(recvTimeout):
		t.Fatal("session never settled into hibernation")
	}
	// Act
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
	// Assert — a hibernated session must stay listed and NON-terminal, or
	// the Emacs reattach sweep and the webapp probe would resurrect it.
	if len(body.Sessions) != 1 {
		t.Fatalf("sessions = %v, want exactly one", body.Sessions)
	}
	s := body.Sessions[0]
	if s["session_id"] != id || s["hibernated"] != true || s["terminal"] != false {
		t.Fatalf("listed session = %v, want hibernated non-terminal", s)
	}
}

func TestWebappActingFrameOverStreamRevives(t *testing.T) {
	// Arrange — an initialized session, hibernated by a sweep.
	h := newSweepHarness(t, 10*time.Minute)
	id, shim := h.createSession(t)
	conn := h.dial(t, id)
	initOverStream(t, conn, shim, "cli-uuid-1")
	h.advance(11 * time.Minute)
	h.sweepOnce(t)
	recvShutdown(t, shim)
	shim.end()
	sess := h.srv.lookup(id)
	select {
	case <-sess.HibernateDone():
	case <-time.After(recvTimeout):
		t.Fatal("session never settled into hibernation")
	}
	// Act — the webapp's own composer sends a user-message OVER the stream
	// socket (not HTTP). That acting frame must revive the session.
	conn2 := h.dial(t, id)
	readFrame(t, conn2) // hello
	msg := `{"type":"user-message","request_id":"r1","content":"back?"}`
	if err := conn2.WriteMessage(websocket.TextMessage, []byte(msg)); err != nil {
		t.Fatalf("write: %v", err)
	}
	// Assert — a fresh shim was spawned for the revive.
	select {
	case <-h.shims:
	case <-time.After(recvTimeout):
		t.Fatal("acting frame over stream did not revive: no shim spawned")
	}
}
