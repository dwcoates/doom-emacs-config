package session

import (
	"encoding/json"
	"slices"
	"strings"
	"sync"
	"testing"
	"time"

	"claude-repld/internal/protocol"
)

const recvTimeout = 5 * time.Second

// fakeShim is an in-memory ShimHandle: tests push Layer-1 events and
// observe forwarded command lines.
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
	shim *fakeShim
	sess *Session
}

func newHarness(t *testing.T, retention int) *harness {
	t.Helper()
	shim := newFakeShim()
	sess := New(Config{
		ID:            "sess-1",
		DaemonVersion: "0.1.0-test",
		Shim:          shim,
		Retention:     retention,
		Now:           func() time.Time { return time.Date(2026, 5, 24, 12, 34, 56, 789e6, time.UTC) },
		Logf:          func(string, ...any) {},
	})
	go sess.Run()
	t.Cleanup(func() {
		shim.end()
		<-sess.Done()
	})
	return &harness{shim: shim, sess: sess}
}

// endShim closes the fake shim's event stream (simulates shim exit).
func (h *harness) endShim() {
	h.shim.end()
}

func recvFrame(t *testing.T, c *Client) map[string]any {
	t.Helper()
	select {
	case data, ok := <-c.Send:
		if !ok {
			t.Fatal("client Send channel closed while expecting a frame")
		}
		var frame map[string]any
		if err := json.Unmarshal(data, &frame); err != nil {
			t.Fatalf("unmarshal frame %s: %v", data, err)
		}
		return frame
	case <-time.After(recvTimeout):
		t.Fatal("timed out waiting for a frame")
		return nil
	}
}

func recvSent(t *testing.T, f *fakeShim) []byte {
	t.Helper()
	select {
	case line := <-f.sent:
		return line
	case <-time.After(recvTimeout):
		t.Fatal("timed out waiting for a forwarded command")
		return nil
	}
}

func expectClosed(t *testing.T, c *Client) {
	t.Helper()
	for {
		select {
		case _, ok := <-c.Send:
			if !ok {
				return
			}
		case <-time.After(recvTimeout):
			t.Fatal("timed out waiting for client channel to close")
		}
	}
}

// --- hello -------------------------------------------------------------------

func TestSessionHelloIsFirstFrameOnAttach(t *testing.T) {
	// Arrange
	h := newHarness(t, 16)
	c := NewClient()
	// Act
	h.sess.Attach(c)
	// Assert
	hello := recvFrame(t, c)
	if hello["type"] != "hello" || hello["seq"] != float64(0) ||
		hello["resume_from_seq"] != float64(0) || hello["session_id"] != "sess-1" ||
		hello["daemon_version"] != "0.1.0-test" {
		t.Errorf("hello = %v", hello)
	}
}

func TestSessionHelloTimestampIsISO8601(t *testing.T) {
	// Arrange
	h := newHarness(t, 16)
	c := NewClient()
	// Act
	h.sess.Attach(c)
	// Assert
	hello := recvFrame(t, c)
	if hello["ts"] != "2026-05-24T12:34:56.789Z" {
		t.Errorf("ts = %v", hello["ts"])
	}
}

func TestSessionLateJoinerHelloCarriesSessionInfoAndCursor(t *testing.T) {
	// Arrange — an init frame has already been broadcast.
	h := newHarness(t, 16)
	early := NewClient()
	h.sess.Attach(early)
	recvFrame(t, early) // hello
	h.shim.pushEvent(t, `{"type":"system","session_id":"sess-1","uuid":"u","subtype":"init","data":{"model":"opus-x","cwd":"/w","permissionMode":"plan"}}`)
	recvFrame(t, early) // system frame, seq 1 now retained
	// Act
	late := NewClient()
	h.sess.Attach(late)
	// Assert
	hello := recvFrame(t, late)
	if hello["resume_from_seq"] != float64(1) || hello["model"] != "opus-x" ||
		hello["cwd"] != "/w" || hello["permission_mode"] != "plan" {
		t.Errorf("hello = %v", hello)
	}
}

func TestSessionHelloCarriesRequestedCwdModelBeforeInit(t *testing.T) {
	// Arrange — CreateOpts-requested values seed the hello mirror.
	shim := newFakeShim()
	sess := New(Config{
		ID:            "sess-1",
		DaemonVersion: "0.1.0-test",
		Shim:          shim,
		CWD:           "/req/cwd",
		Model:         "haiku",
		Retention:     16,
		Now:           func() time.Time { return time.Date(2026, 5, 24, 12, 34, 56, 789e6, time.UTC) },
		Logf:          func(string, ...any) {},
	})
	go sess.Run()
	t.Cleanup(func() {
		shim.end()
		<-sess.Done()
	})
	c := NewClient()
	// Act — attach BEFORE any system:init arrives.
	sess.Attach(c)
	// Assert
	hello := recvFrame(t, c)
	if hello["cwd"] != "/req/cwd" || hello["model"] != "haiku" {
		t.Errorf("hello = %v", hello)
	}
}

func TestSessionInitCapturesClaudeSessionID(t *testing.T) {
	// Arrange
	h := newHarness(t, 16)
	early := NewClient()
	h.sess.Attach(early)
	recvFrame(t, early)
	// Act — system:init carries the CLI session uuid.
	h.shim.pushEvent(t, `{"type":"system","session_id":"sess-1","uuid":"u","subtype":"init","data":{"model":"m","cwd":"/w","permissionMode":"default","session_id":"cli-uuid-1"}}`)
	recvFrame(t, early)
	// Assert — Info and a late joiner's hello both expose it.
	if got := h.sess.Info().ClaudeSessionID; got != "cli-uuid-1" {
		t.Errorf("Info().ClaudeSessionID = %q", got)
	}
	late := NewClient()
	h.sess.Attach(late)
	hello := recvFrame(t, late)
	if hello["claude_session_id"] != "cli-uuid-1" {
		t.Errorf("hello = %v", hello)
	}
}

func TestSessionReplayAllowedOnTerminalSession(t *testing.T) {
	// Arrange — history exists, then the session ends gracefully.
	h := newHarness(t, 16)
	early := NewClient()
	h.sess.Attach(early)
	recvFrame(t, early)
	h.shim.pushEvent(t, `{"type":"system","session_id":"sess-1","uuid":"u","subtype":"init","data":{}}`)
	recvFrame(t, early)
	// A graceful sdk_end closed event emits no L2 frame; it just marks
	// the session terminal once the stream drains.
	h.shim.pushEvent(t, `{"type":"closed","session_id":"sess-1","exit_code":0,"reason":"sdk_end"}`)
	h.endShim()
	<-h.sess.Done()
	// Act — a late joiner replays on the now-terminal session.
	late := NewClient()
	h.sess.Attach(late)
	recvFrame(t, late) // hello
	if err := h.sess.HandleClientFrame(late, []byte(`{"type":"replay-request","from_seq":1}`)); err != nil {
		t.Fatalf("replay on terminal session: %v", err)
	}
	// Assert — retained history frames stream back.
	replayed := recvFrame(t, late)
	if replayed["type"] != "system" || replayed["seq"] != float64(1) {
		t.Errorf("replayed = %v", replayed)
	}
}

func TestSessionReplayFromDrainedClientDoesNotPanic(t *testing.T) {
	// Arrange — a client attached when the shim hard-dies: Run's drain
	// closes its channel and removes it from the client set, but the
	// server's reader goroutine may still deliver one more frame.
	h := newHarness(t, 16)
	c := NewClient()
	h.sess.Attach(c)
	recvFrame(t, c)
	h.shim.pushEvent(t, `{"type":"system","session_id":"sess-1","uuid":"u","subtype":"init","data":{}}`)
	recvFrame(t, c)
	h.endShim()
	<-h.sess.Done()
	expectClosed(t, c)
	// Act / Assert — the terminal carve-out honors the replay, and the
	// stale client's closed channel is skipped instead of panicking.
	if err := h.sess.HandleClientFrame(c, []byte(`{"type":"replay-request","from_seq":1}`)); err != nil {
		t.Fatalf("replay from drained client: %v", err)
	}
}

func TestSessionInitOverwritesRequestedCwdModel(t *testing.T) {
	// Arrange — requested values seed the mirror.
	shim := newFakeShim()
	sess := New(Config{
		ID:            "sess-1",
		DaemonVersion: "0.1.0-test",
		Shim:          shim,
		CWD:           "/requested",
		Model:         "requested-model",
		Retention:     16,
		Now:           func() time.Time { return time.Date(2026, 5, 24, 12, 34, 56, 789e6, time.UTC) },
		Logf:          func(string, ...any) {},
	})
	go sess.Run()
	t.Cleanup(func() {
		shim.end()
		<-sess.Done()
	})
	early := NewClient()
	sess.Attach(early)
	recvFrame(t, early)
	// Act — the authoritative init reports different values.
	e, err := protocol.DecodeL1Event([]byte(`{"type":"system","session_id":"sess-1","uuid":"u","subtype":"init","data":{"model":"authoritative-model","cwd":"/authoritative"}}`))
	if err != nil {
		t.Fatalf("decode: %v", err)
	}
	shim.events <- e
	recvFrame(t, early)
	// Assert — a late hello carries the authoritative values.
	late := NewClient()
	sess.Attach(late)
	hello := recvFrame(t, late)
	if hello["cwd"] != "/authoritative" || hello["model"] != "authoritative-model" {
		t.Errorf("hello = %v", hello)
	}
}

// --- broadcast / seq -----------------------------------------------------------

func TestSessionSeqIsStrictlyIncreasingAcrossFrames(t *testing.T) {
	// Arrange
	h := newHarness(t, 16)
	c := NewClient()
	h.sess.Attach(c)
	recvFrame(t, c) // hello
	// Act — one event producing multiple frames (text start+delta arrive
	// as separate events here for simplicity).
	h.shim.pushEvent(t, `{"type":"stream-event","session_id":"sess-1","uuid":"u","event":{"type":"content_block_start","index":0,"content_block":{"type":"text","text":""}}}`)
	h.shim.pushEvent(t, `{"type":"stream-event","session_id":"sess-1","uuid":"u","event":{"type":"content_block_delta","index":0,"delta":{"type":"text_delta","text":"x"}}}`)
	h.shim.pushEvent(t, `{"type":"stream-event","session_id":"sess-1","uuid":"u","event":{"type":"content_block_stop","index":0}}`)
	// Assert
	prev := float64(0)
	for range 3 {
		frame := recvFrame(t, c)
		seq := frame["seq"].(float64)
		if seq != prev+1 {
			t.Fatalf("seq = %v after %v", seq, prev)
		}
		prev = seq
	}
}

func TestSessionBroadcastReachesAllClients(t *testing.T) {
	// Arrange
	h := newHarness(t, 16)
	c1, c2 := NewClient(), NewClient()
	h.sess.Attach(c1)
	h.sess.Attach(c2)
	recvFrame(t, c1)
	recvFrame(t, c2)
	// Act
	h.shim.pushEvent(t, `{"type":"system","session_id":"sess-1","uuid":"u","subtype":"init","data":{}}`)
	// Assert
	if recvFrame(t, c1)["type"] != "system" || recvFrame(t, c2)["type"] != "system" {
		t.Error("both clients should receive the system frame")
	}
}

func TestSessionSlowClientIsDroppedNotBlocking(t *testing.T) {
	// Arrange — a client with a full 1-slot buffer.
	h := newHarness(t, 16)
	slow := &Client{Send: make(chan []byte, 1)}
	h.sess.Attach(slow) // hello fills the only slot
	healthy := NewClient()
	h.sess.Attach(healthy)
	recvFrame(t, healthy)
	// Act — the second broadcast overflows the slow client.
	h.shim.pushEvent(t, `{"type":"system","session_id":"sess-1","uuid":"u","subtype":"init","data":{}}`)
	h.shim.pushEvent(t, `{"type":"system","session_id":"sess-1","uuid":"u","subtype":"slash_command","data":{}}`)
	// Assert — healthy client still gets both; slow client's channel closes.
	recvFrame(t, healthy)
	recvFrame(t, healthy)
	expectClosed(t, slow)
}

// --- client commands -------------------------------------------------------------

func TestSessionUserMessageBroadcastsUserTurnAndForwards(t *testing.T) {
	// Arrange
	h := newHarness(t, 16)
	c := NewClient()
	h.sess.Attach(c)
	recvFrame(t, c)
	raw := []byte(`{"type":"user-message","request_id":"r1","content":"hi"}`)
	// Act
	if err := h.sess.HandleClientFrame(c, raw); err != nil {
		t.Fatalf("HandleClientFrame: %v", err)
	}
	// Assert
	turn := recvFrame(t, c)
	if turn["type"] != "user-turn" || turn["request_id"] != "r1" {
		t.Errorf("turn = %v", turn)
	}
	forwarded := recvSent(t, h.shim)
	if !strings.HasSuffix(string(forwarded), "\n") || !strings.Contains(string(forwarded), `"user-message"`) {
		t.Errorf("forwarded = %q", forwarded)
	}
}

func TestSessionPermissionRoundTrip(t *testing.T) {
	// Arrange
	h := newHarness(t, 16)
	c := NewClient()
	h.sess.Attach(c)
	recvFrame(t, c)
	h.shim.pushEvent(t, `{"type":"permission-request","session_id":"sess-1","request_id":"p1","tool_use_id":"t1","tool_name":"Bash","input":{"command":"ls"}}`)
	request := recvFrame(t, c)
	if request["type"] != "permission-request" {
		t.Fatalf("request = %v", request)
	}
	// Act
	if err := h.sess.HandleClientFrame(c, []byte(`{"type":"permission-decision","request_id":"p1","decision":{"behavior":"allow"}}`)); err != nil {
		t.Fatalf("HandleClientFrame: %v", err)
	}
	// Assert
	resolved := recvFrame(t, c)
	if resolved["type"] != "permission-resolved" || resolved["decision"] != "allow" {
		t.Errorf("resolved = %v", resolved)
	}
	if !strings.Contains(string(recvSent(t, h.shim)), `"permission-decision"`) {
		t.Error("decision not forwarded to shim")
	}
}

func TestSessionStalePermissionDecisionIsRejectedNotForwarded(t *testing.T) {
	// Arrange
	h := newHarness(t, 16)
	c := NewClient()
	h.sess.Attach(c)
	recvFrame(t, c)
	// Act
	err := h.sess.HandleClientFrame(c, []byte(`{"type":"permission-decision","request_id":"ghost","decision":{"behavior":"allow"}}`))
	// Assert
	if err == nil || !strings.Contains(err.Error(), "unknown request_id") {
		t.Fatalf("err = %v", err)
	}
	select {
	case line := <-h.shim.sent:
		t.Fatalf("unexpected forward: %s", line)
	default:
	}
}

func TestSessionInterruptCancelsPromptsAndForwards(t *testing.T) {
	// Arrange
	h := newHarness(t, 16)
	c := NewClient()
	h.sess.Attach(c)
	recvFrame(t, c)
	h.shim.pushEvent(t, `{"type":"permission-request","session_id":"sess-1","request_id":"p1","tool_use_id":"t1","tool_name":"Bash","input":{}}`)
	recvFrame(t, c)
	// Act
	if err := h.sess.HandleClientFrame(c, []byte(`{"type":"interrupt","request_id":"r1"}`)); err != nil {
		t.Fatalf("HandleClientFrame: %v", err)
	}
	// Assert
	cancel := recvFrame(t, c)
	if cancel["type"] != "permission-resolved" || cancel["decision"] != "cancel" {
		t.Errorf("cancel = %v", cancel)
	}
	if !strings.Contains(string(recvSent(t, h.shim)), `"interrupt"`) {
		t.Error("interrupt not forwarded")
	}
}

func TestSessionSetPermissionModeEmitsModeChangedOnAck(t *testing.T) {
	// Arrange
	h := newHarness(t, 16)
	c := NewClient()
	h.sess.Attach(c)
	recvFrame(t, c)
	// Act
	if err := h.sess.HandleClientFrame(c, []byte(`{"type":"set-permission-mode","request_id":"m1","mode":"acceptEdits"}`)); err != nil {
		t.Fatalf("HandleClientFrame: %v", err)
	}
	recvSent(t, h.shim) // forwarded command
	h.shim.pushEvent(t, `{"type":"ack","session_id":"sess-1","request_id":"m1"}`)
	// Assert — §1.2: the frame appears only after the shim ack.
	changed := recvFrame(t, c)
	if changed["type"] != "permission-mode-changed" || changed["mode"] != "acceptEdits" {
		t.Errorf("changed = %v", changed)
	}
}

func TestSessionUnknownClientFrameTypeIsIgnored(t *testing.T) {
	// Arrange
	h := newHarness(t, 16)
	c := NewClient()
	h.sess.Attach(c)
	recvFrame(t, c)
	// Act + Assert
	if err := h.sess.HandleClientFrame(c, []byte(`{"type":"hover-intent","request_id":"r1"}`)); err != nil {
		t.Fatalf("unknown frame should be ignored, got %v", err)
	}
}

func TestSessionMalformedClientFrameErrors(t *testing.T) {
	// Arrange
	h := newHarness(t, 16)
	c := NewClient()
	h.sess.Attach(c)
	recvFrame(t, c)
	// Act + Assert
	if err := h.sess.HandleClientFrame(c, []byte(`{"type":"interrupt"}`)); err == nil {
		t.Fatal("missing request_id should error")
	}
}

// --- replay -----------------------------------------------------------------------

func TestSessionReplayResendsRetainedFramesWithOriginalSeq(t *testing.T) {
	// Arrange
	h := newHarness(t, 16)
	c := NewClient()
	h.sess.Attach(c)
	recvFrame(t, c)
	for range 3 {
		h.shim.pushEvent(t, `{"type":"system","session_id":"sess-1","uuid":"u","subtype":"init","data":{}}`)
		recvFrame(t, c)
	}
	// Act — replay from seq 2.
	if err := h.sess.HandleClientFrame(c, []byte(`{"type":"replay-request","from_seq":2}`)); err != nil {
		t.Fatalf("HandleClientFrame: %v", err)
	}
	// Assert
	first := recvFrame(t, c)
	second := recvFrame(t, c)
	if first["seq"] != float64(2) || second["seq"] != float64(3) {
		t.Errorf("replayed seqs = %v, %v", first["seq"], second["seq"])
	}
}

func TestSessionReplayOfEvictedSeqSendsFreshHello(t *testing.T) {
	// Arrange — retention 2, so seq 1 gets evicted.
	h := newHarness(t, 2)
	c := NewClient()
	h.sess.Attach(c)
	recvFrame(t, c)
	for range 3 {
		h.shim.pushEvent(t, `{"type":"system","session_id":"sess-1","uuid":"u","subtype":"init","data":{}}`)
		recvFrame(t, c)
	}
	// Act
	if err := h.sess.HandleClientFrame(c, []byte(`{"type":"replay-request","from_seq":1}`)); err != nil {
		t.Fatalf("HandleClientFrame: %v", err)
	}
	// Assert — §2.10: fresh hello naming the earliest retained frame.
	hello := recvFrame(t, c)
	if hello["type"] != "hello" || hello["resume_from_seq"] != float64(2) {
		t.Errorf("hello = %v", hello)
	}
}

// --- lifecycle ---------------------------------------------------------------------

func TestSessionClosedEventMakesSessionTerminal(t *testing.T) {
	// Arrange
	h := newHarness(t, 16)
	c := NewClient()
	h.sess.Attach(c)
	recvFrame(t, c)
	// Act
	h.shim.pushEvent(t, `{"type":"closed","session_id":"sess-1","exit_code":0,"reason":"shutdown"}`)
	h.endShim()
	<-h.sess.Done()
	// Assert
	if !h.sess.Terminal() {
		t.Error("session should be terminal")
	}
	if err := h.sess.HandleClientFrame(c, []byte(`{"type":"interrupt","request_id":"r1"}`)); err == nil {
		t.Error("commands on a terminal session should error")
	}
}

func TestSessionShimDeathWithoutClosedEmitsShimDied(t *testing.T) {
	// Arrange
	h := newHarness(t, 16)
	c := NewClient()
	h.sess.Attach(c)
	recvFrame(t, c)
	// Act — stdout closes with no closed event (hard crash).
	h.endShim()
	<-h.sess.Done()
	// Assert
	frame := recvFrame(t, c)
	if frame["type"] != "error" || frame["code"] != "shim_died" {
		t.Errorf("frame = %v", frame)
	}
	expectClosed(t, c)
}

func TestSessionHardShimDeathCancelsPendingPermissions(t *testing.T) {
	// Arrange — a permission prompt is pending when the shim hard-dies.
	h := newHarness(t, 16)
	c := NewClient()
	h.sess.Attach(c)
	recvFrame(t, c)
	h.shim.pushEvent(t, `{"type":"permission-request","session_id":"sess-1","request_id":"p1","tool_use_id":"t1","tool_name":"Bash","input":{}}`)
	recvFrame(t, c)
	// Act — stdout closes with no closed event (hard crash).
	h.endShim()
	<-h.sess.Done()
	// Assert — cancel precedes the shim_died error frame (§2.7).
	cancel := recvFrame(t, c)
	if cancel["type"] != "permission-resolved" || cancel["decision"] != "cancel" || cancel["request_id"] != "p1" {
		t.Errorf("cancel = %v", cancel)
	}
	errFrame := recvFrame(t, c)
	if errFrame["type"] != "error" || errFrame["code"] != "shim_died" {
		t.Errorf("error frame = %v", errFrame)
	}
	expectClosed(t, c)
}

func TestSessionClientShutdownFrameIsIgnoredNotForwarded(t *testing.T) {
	// Arrange — client-sent shutdown is outside the §2 command set.
	h := newHarness(t, 16)
	c := NewClient()
	h.sess.Attach(c)
	recvFrame(t, c)
	// Act
	err := h.sess.HandleClientFrame(c, []byte(`{"type":"shutdown","request_id":"r1"}`))
	// Assert — ignored without error and never forwarded to the shim.
	if err != nil {
		t.Fatalf("HandleClientFrame: %v", err)
	}
	select {
	case line := <-h.shim.sent:
		t.Fatalf("unexpected forward: %s", line)
	default:
	}
}

func TestSessionShutdownSendsShutdownCommand(t *testing.T) {
	// Arrange
	h := newHarness(t, 16)
	// Act
	if err := h.sess.Shutdown("test teardown"); err != nil {
		t.Fatalf("Shutdown: %v", err)
	}
	// Assert
	line := recvSent(t, h.shim)
	var cmd map[string]any
	if err := json.Unmarshal(line, &cmd); err != nil {
		t.Fatalf("unmarshal: %v", err)
	}
	if cmd["type"] != "shutdown" || cmd["reason"] != "test teardown" || cmd["request_id"] == "" {
		t.Errorf("cmd = %v", cmd)
	}
}

func TestSessionDetachIsIdempotent(t *testing.T) {
	// Arrange
	h := newHarness(t, 16)
	c := NewClient()
	h.sess.Attach(c)
	recvFrame(t, c)
	// Act + Assert — second detach must not panic on double close.
	h.sess.Detach(c)
	h.sess.Detach(c)
}

// ---------------------------------------------------------------------------
// Sentinel side-channel tap (broadcastLocked -> SentinelSink)
// ---------------------------------------------------------------------------

// recordingSink is a thread-safe SentinelSink that records calls.
type recordingSink struct {
	mu    sync.Mutex
	calls []string
}

func (r *recordingSink) record(s string) {
	r.mu.Lock()
	defer r.mu.Unlock()
	r.calls = append(r.calls, s)
}

func (r *recordingSink) PermissionRequested(cwd, sid, reqID string) {
	r.record("requested " + cwd + " " + sid + " " + reqID)
}

func (r *recordingSink) PermissionResolved(cwd, sid, reqID string) {
	r.record("resolved " + cwd + " " + sid + " " + reqID)
}

func (r *recordingSink) SessionDead(cwd, sid string) {
	r.record("dead " + cwd + " " + sid)
}

func (r *recordingSink) snapshot() []string {
	r.mu.Lock()
	defer r.mu.Unlock()
	return append([]string(nil), r.calls...)
}

func newSinkHarness(t *testing.T) (*harness, *recordingSink) {
	t.Helper()
	shim := newFakeShim()
	sink := &recordingSink{}
	sess := New(Config{
		ID:            "sess-1",
		DaemonVersion: "0.1.0-test",
		Shim:          shim,
		Retention:     16,
		Now:           func() time.Time { return time.Date(2026, 5, 24, 12, 34, 56, 789e6, time.UTC) },
		Logf:          func(string, ...any) {},
		Sentinel:      sink,
	})
	go sess.Run()
	t.Cleanup(func() {
		shim.end()
		<-sess.Done()
	})
	return &harness{shim: shim, sess: sess}, sink
}

// initAndPermission drives the session through system:init (seeding
// cwd/sid) and one permission request.
func initAndPermission(t *testing.T, h *harness) {
	t.Helper()
	h.shim.pushEvent(t, `{"type":"system","session_id":"sess-1","uuid":"u","subtype":"init","data":{"model":"m","cwd":"/w","permissionMode":"default","session_id":"cli-uuid-1"}}`)
	h.shim.pushEvent(t, `{"type":"permission-request","session_id":"sess-1","request_id":"p1","tool_use_id":"t1","tool_name":"Bash","input":{"command":"ls"}}`)
}

func TestSentinelTapPermissionRequested(t *testing.T) {
	// Arrange
	h, sink := newSinkHarness(t)
	// Act
	initAndPermission(t, h)
	h.endShim()
	<-h.sess.Done()
	// Assert — cwd/sid come from the translator's post-init mirror.
	calls := sink.snapshot()
	if len(calls) == 0 || calls[0] != "requested /w cli-uuid-1 p1" {
		t.Fatalf("calls = %v", calls)
	}
}

func TestSentinelTapWebappResolution(t *testing.T) {
	// Arrange
	h, sink := newSinkHarness(t)
	c := NewClient()
	h.sess.Attach(c)
	initAndPermission(t, h)
	// Wait for the request frame so the decision is not racing the event.
	for {
		f := recvFrame(t, c)
		if f["type"] == "permission-request" {
			break
		}
	}
	// Act — webapp decision through the client-command path.
	if err := h.sess.HandleClientFrame(c, []byte(`{"type":"permission-decision","request_id":"p1","decision":{"behavior":"allow"}}`)); err != nil {
		t.Fatalf("HandleClientFrame: %v", err)
	}
	h.endShim()
	<-h.sess.Done()
	// Assert
	calls := sink.snapshot()
	want := "resolved /w cli-uuid-1 p1"
	found := false
	for _, c := range calls {
		if c == want {
			found = true
		}
	}
	if !found {
		t.Fatalf("missing %q in calls %v", want, calls)
	}
}

func TestSentinelTapTurnEndAutoCancel(t *testing.T) {
	// Arrange — pending permission, then the turn's result frame.
	h, sink := newSinkHarness(t)
	initAndPermission(t, h)
	h.shim.pushEvent(t, `{"type":"result","session_id":"sess-1","uuid":"u2","subtype":"success","duration_ms":1,"num_turns":1}`)
	// Act
	h.endShim()
	<-h.sess.Done()
	// Assert — the auto-cancel resolution reaches the sink.
	calls := sink.snapshot()
	found := false
	for _, c := range calls {
		if c == "resolved /w cli-uuid-1 p1" {
			found = true
		}
	}
	if !found {
		t.Fatalf("missing turn-end cancel in calls %v", calls)
	}
}

func TestSentinelTapHardShimDeath(t *testing.T) {
	// Arrange — stdout closes WITHOUT a closed event.
	h, sink := newSinkHarness(t)
	initAndPermission(t, h)
	// Act
	h.endShim()
	<-h.sess.Done()
	// Assert — cancel for the pending prompt AND a session-dead write.
	calls := sink.snapshot()
	foundDead := false
	for _, c := range calls {
		if c == "dead /w cli-uuid-1" {
			foundDead = true
		}
	}
	if !foundDead {
		t.Fatalf("missing session-dead in calls %v", calls)
	}
}

func TestSentinelTapFatalErrorClose(t *testing.T) {
	// Arrange — graceful closed event with reason fatal_error.
	h, sink := newSinkHarness(t)
	h.shim.pushEvent(t, `{"type":"system","session_id":"sess-1","uuid":"u","subtype":"init","data":{"model":"m","cwd":"/w","permissionMode":"default","session_id":"cli-uuid-1"}}`)
	h.shim.pushEvent(t, `{"type":"closed","session_id":"sess-1","reason":"fatal_error","exit_code":3}`)
	// Act
	h.endShim()
	<-h.sess.Done()
	// Assert
	calls := sink.snapshot()
	found := false
	for _, c := range calls {
		if c == "dead /w cli-uuid-1" {
			found = true
		}
	}
	if !found {
		t.Fatalf("missing session-dead in calls %v", calls)
	}
}

func TestSentinelTapCleanShutdownNoDead(t *testing.T) {
	// Arrange — graceful closed with a non-fatal reason (clean DELETE).
	h, sink := newSinkHarness(t)
	h.shim.pushEvent(t, `{"type":"system","session_id":"sess-1","uuid":"u","subtype":"init","data":{"model":"m","cwd":"/w","permissionMode":"default","session_id":"cli-uuid-1"}}`)
	h.shim.pushEvent(t, `{"type":"closed","session_id":"sess-1","reason":"shutdown","exit_code":0}`)
	// Act
	h.endShim()
	<-h.sess.Done()
	// Assert — no session-dead write for a deliberate teardown.
	for _, c := range sink.snapshot() {
		if strings.HasPrefix(c, "dead") {
			t.Fatalf("unexpected session-dead on clean shutdown: %v", sink.snapshot())
		}
	}
}

// ---------------------------------------------------------------------------
// Info reconcile fields (death_reason, turn_active, pending_permissions)
// ---------------------------------------------------------------------------

func TestInfoTurnActiveLifecycle(t *testing.T) {
	// Arrange
	h := newHarness(t, 16)
	c := NewClient()
	h.sess.Attach(c)
	recvFrame(t, c) // hello
	if h.sess.Info().TurnActive {
		t.Fatal("turn_active before any user message")
	}
	// Act — user message starts a turn.
	if err := h.sess.HandleClientFrame(c, []byte(`{"type":"user-message","request_id":"u1","content":"hi"}`)); err != nil {
		t.Fatalf("HandleClientFrame: %v", err)
	}
	// Assert
	if !h.sess.Info().TurnActive {
		t.Fatal("turn_active should be true after user-message")
	}
	// Act — the result frame ends the turn.
	h.shim.pushEvent(t, `{"type":"result","session_id":"sess-1","uuid":"u2","subtype":"success","duration_ms":1,"num_turns":1}`)
	waitForFrameType(t, c, "result")
	// Assert
	if h.sess.Info().TurnActive {
		t.Fatal("turn_active should be false after result")
	}
}

func TestInfoPendingPermissions(t *testing.T) {
	// Arrange
	h := newHarness(t, 16)
	c := NewClient()
	h.sess.Attach(c)
	recvFrame(t, c) // hello
	h.shim.pushEvent(t, `{"type":"permission-request","session_id":"sess-1","request_id":"p1","tool_use_id":"t1","tool_name":"Bash","input":{}}`)
	waitForFrameType(t, c, "permission-request")
	// Assert — pending while unresolved.
	if got := h.sess.Info().PendingPermissions; len(got) != 1 || got[0] != "p1" {
		t.Fatalf("pending = %v, want [p1]", got)
	}
	// Act — resolve it.
	if err := h.sess.HandleClientFrame(c, []byte(`{"type":"permission-decision","request_id":"p1","decision":{"behavior":"allow"}}`)); err != nil {
		t.Fatalf("HandleClientFrame: %v", err)
	}
	// Assert — cleared.
	if got := h.sess.Info().PendingPermissions; len(got) != 0 {
		t.Fatalf("pending after resolve = %v, want empty", got)
	}
}

func TestInfoDeathReasonHardDeath(t *testing.T) {
	// Arrange
	h := newHarness(t, 16)
	// Act — stdout closes without a closed event.
	h.endShim()
	<-h.sess.Done()
	// Assert
	if got := h.sess.Info().DeathReason; got != "shim_died" {
		t.Fatalf("death_reason = %q, want shim_died", got)
	}
}

func TestInfoDeathReasonGracefulClose(t *testing.T) {
	// Arrange — table over closed reasons.
	tests := []struct {
		reason string
	}{
		{reason: "shutdown"},
		{reason: "fatal_error"},
		{reason: "sdk_end"},
	}
	for _, tt := range tests {
		t.Run(tt.reason, func(t *testing.T) {
			h := newHarness(t, 16)
			// Act
			h.shim.pushEvent(t, `{"type":"closed","session_id":"sess-1","reason":"`+tt.reason+`","exit_code":0}`)
			h.endShim()
			<-h.sess.Done()
			// Assert
			if got := h.sess.Info().DeathReason; got != tt.reason {
				t.Fatalf("death_reason = %q, want %q", got, tt.reason)
			}
		})
	}
}

// waitForFrameType receives frames until one of the given type arrives.
func waitForFrameType(t *testing.T, c *Client, typ string) map[string]any {
	t.Helper()
	for {
		f := recvFrame(t, c)
		if f["type"] == typ {
			return f
		}
	}
}

func TestReplayClientAbsorbsFullRingBurst(t *testing.T) {
	// Arrange: a ring far larger than the fixed 256-frame NewClient
	// buffer (the transcript-seeded norm).
	shim := newFakeShim()
	sess := New(Config{ID: "s_test", Shim: shim, Logf: func(string, ...any) {}})
	const ringSize = 1500
	sess.mu.Lock()
	for i := 0; i < ringSize; i++ {
		sess.broadcastLocked([]protocol.L2Frame{&protocol.TextDeltaFrame{
			Envelope: protocol.Envelope{Type: "text-delta"}, BlockID: "b1", Text: "x",
		}})
	}
	sess.mu.Unlock()
	client := sess.NewReplayClient()
	sess.Attach(client)
	<-client.Send // hello
	// Act: request the full ring with no concurrent reader draining.
	if err := sess.HandleClientFrame(client, []byte(`{"type":"replay-request","from_seq":1}`)); err != nil {
		t.Fatalf("replay-request: %v", err)
	}
	// Assert: every frame was queued and the client was not dropped.
	for i := 0; i < ringSize; i++ {
		select {
		case _, ok := <-client.Send:
			if !ok {
				t.Fatalf("client dropped (channel closed) after %d of %d frames", i, ringSize)
			}
		default:
			t.Fatalf("only %d of %d frames queued", i, ringSize)
		}
	}
}

// ---------------------------------------------------------------------------
// Registrar notifications (durable-state transitions for the registry)
// ---------------------------------------------------------------------------

// recordingRegistrar is a thread-safe Registrar that records calls.
type recordingRegistrar struct {
	mu    sync.Mutex
	calls []string
}

func (r *recordingRegistrar) record(s string) {
	r.mu.Lock()
	defer r.mu.Unlock()
	r.calls = append(r.calls, s)
}

func (r *recordingRegistrar) ClaudeSessionIDChanged(sessionID, claudeSessionID string) {
	r.record("claude-id " + sessionID + " " + claudeSessionID)
}

func (r *recordingRegistrar) ModelChanged(sessionID, model string) {
	r.record("model " + sessionID + " " + model)
}

func (r *recordingRegistrar) SessionTerminal(sessionID, deathReason string) {
	r.record("terminal " + sessionID + " " + deathReason)
}

func (r *recordingRegistrar) snapshot() []string {
	r.mu.Lock()
	defer r.mu.Unlock()
	return append([]string(nil), r.calls...)
}

func newRegistrarHarness(t *testing.T) (*harness, *recordingRegistrar) {
	t.Helper()
	shim := newFakeShim()
	reg := &recordingRegistrar{}
	sess := New(Config{
		ID:            "sess-1",
		DaemonVersion: "0.1.0-test",
		Shim:          shim,
		Retention:     16,
		Now:           func() time.Time { return time.Date(2026, 5, 24, 12, 34, 56, 789e6, time.UTC) },
		Logf:          func(string, ...any) {},
		Registrar:     reg,
	})
	go sess.Run()
	t.Cleanup(func() {
		shim.end()
		<-sess.Done()
	})
	return &harness{shim: shim, sess: sess}, reg
}

func TestRegistrarNotifiedWhenInitSuppliesClaudeSessionID(t *testing.T) {
	// Arrange
	h, reg := newRegistrarHarness(t)
	// Act
	h.shim.pushEvent(t, `{"type":"system","session_id":"sess-1","uuid":"u","subtype":"init","data":{"session_id":"cli-uuid-7"}}`)
	h.endShim()
	<-h.sess.Done()
	// Assert
	calls := reg.snapshot()
	if len(calls) == 0 || calls[0] != "claude-id sess-1 cli-uuid-7" {
		t.Fatalf("calls = %v", calls)
	}
}

func TestRegistrarClaudeSessionIDReportedOncePerValue(t *testing.T) {
	// Arrange — two inits carrying the SAME uuid.
	h, reg := newRegistrarHarness(t)
	// Act
	h.shim.pushEvent(t, `{"type":"system","session_id":"sess-1","uuid":"u1","subtype":"init","data":{"session_id":"cli-uuid-7"}}`)
	h.shim.pushEvent(t, `{"type":"system","session_id":"sess-1","uuid":"u2","subtype":"init","data":{"session_id":"cli-uuid-7"}}`)
	h.endShim()
	<-h.sess.Done()
	// Assert — exactly one claude-id call.
	count := 0
	for _, c := range reg.snapshot() {
		if c == "claude-id sess-1 cli-uuid-7" {
			count++
		}
	}
	if count != 1 {
		t.Fatalf("claude-id reported %d times, want 1 (calls = %v)", count, reg.snapshot())
	}
}

func TestRegistrarNotifiedOnClosedWithReason(t *testing.T) {
	// Arrange
	h, reg := newRegistrarHarness(t)
	// Act
	h.shim.pushEvent(t, `{"type":"closed","session_id":"sess-1","uuid":"u","reason":"sdk_end","exit_code":0}`)
	h.endShim()
	<-h.sess.Done()
	// Assert
	calls := reg.snapshot()
	if len(calls) != 1 || calls[0] != "terminal sess-1 sdk_end" {
		t.Fatalf("calls = %v", calls)
	}
}

func TestRegistrarNotifiedOnHardShimDeath(t *testing.T) {
	// Arrange
	h, reg := newRegistrarHarness(t)
	// Act — stream ends with no closed event.
	h.endShim()
	<-h.sess.Done()
	// Assert
	calls := reg.snapshot()
	if len(calls) != 1 || calls[0] != "terminal sess-1 shim_died" {
		t.Fatalf("calls = %v", calls)
	}
}

func TestRegistrarNotifiedWhenAssistantMessageMovesTheModel(t *testing.T) {
	// Arrange — the agent answers on a model the mirror has not seen.
	h, reg := newRegistrarHarness(t)
	// Act
	h.shim.pushEvent(t, assistantMsg("m1", "haiku", ""))
	h.endShim()
	<-h.sess.Done()
	// Assert — the switch is written through so a restart resumes on it.
	if !slices.Contains(reg.snapshot(), "model sess-1 haiku") {
		t.Fatalf("calls = %v, want a model write-through", reg.snapshot())
	}
}

func TestRegistrarNotifiedWhenInitSuppliesModel(t *testing.T) {
	// Arrange
	h, reg := newRegistrarHarness(t)
	// Act — system:init names the startup model.
	h.shim.pushEvent(t, `{"type":"system","session_id":"sess-1","uuid":"u","subtype":"init","data":{"model":"opus"}}`)
	h.endShim()
	<-h.sess.Done()
	// Assert
	if !slices.Contains(reg.snapshot(), "model sess-1 opus") {
		t.Fatalf("calls = %v, want a model write-through", reg.snapshot())
	}
}

func TestRegistrarModelReportedOncePerValue(t *testing.T) {
	// Arrange — two turns answered on the SAME model.
	h, reg := newRegistrarHarness(t)
	// Act
	h.shim.pushEvent(t, assistantMsg("m1", "haiku", ""))
	h.shim.pushEvent(t, assistantMsg("m2", "haiku", ""))
	h.endShim()
	<-h.sess.Done()
	// Assert — the steady state writes through exactly once.
	count := 0
	for _, c := range reg.snapshot() {
		if c == "model sess-1 haiku" {
			count++
		}
	}
	if count != 1 {
		t.Fatalf("model reported %d times, want 1 (calls = %v)", count, reg.snapshot())
	}
}

func TestRegistrarNotSentAModelWhenNoneIsEverKnown(t *testing.T) {
	// Arrange — a session that only ever sees non-model events must never
	// write an empty model through (it would clobber a good record).
	h, reg := newRegistrarHarness(t)
	// Act
	h.shim.pushEvent(t, `{"type":"system","session_id":"sess-1","uuid":"u","subtype":"init","data":{"session_id":"cli-uuid-7"}}`)
	h.endShim()
	<-h.sess.Done()
	// Assert
	for _, c := range reg.snapshot() {
		if strings.HasPrefix(c, "model ") {
			t.Fatalf("an empty model was written through: %v", reg.snapshot())
		}
	}
}

func TestRegistrarNotifiedByTranscriptSeedStamp(t *testing.T) {
	// Arrange — no Run needed: SeedFromTranscript stamps synchronously.
	shim := newFakeShim()
	reg := &recordingRegistrar{}
	sess := New(Config{ID: "sess-1", Shim: shim, Logf: func(string, ...any) {}, Registrar: reg})
	// Act — the transcript path is absent; the stamp still happens.
	err := sess.SeedFromTranscript("/nonexistent/transcript.jsonl", "cli-uuid-9")
	// Assert
	if err == nil {
		t.Fatal("expected open error for a missing transcript")
	}
	calls := reg.snapshot()
	if len(calls) != 1 || calls[0] != "claude-id sess-1 cli-uuid-9" {
		t.Fatalf("calls = %v", calls)
	}
}

func TestRegistrarNotifiedOfModelByTranscriptSeed(t *testing.T) {
	// Arrange — a resumed session whose transcript's last main-chain model
	// is haiku: the seed adopts it, and that must be written through so a
	// record predating the write-through (or drifted before restart) is
	// corrected to the model it is actually resuming on.
	shim := newFakeShim()
	reg := &recordingRegistrar{}
	sess := New(Config{ID: "sess-1", Shim: shim, Logf: func(string, ...any) {}, Registrar: reg})
	configDir := writeTranscript(t, "/w", "uuid", assistantLine("haiku"))
	sess.translator.CWD = "/w"
	path := TranscriptPath(configDir, "/w", "uuid")
	// Act
	if err := sess.SeedFromTranscript(path, "uuid"); err != nil {
		t.Fatalf("seed: %v", err)
	}
	// Assert
	if !slices.Contains(reg.snapshot(), "model sess-1 haiku") {
		t.Fatalf("calls = %v, want a model write-through", reg.snapshot())
	}
}
