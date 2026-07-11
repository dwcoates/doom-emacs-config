package session

import (
	"encoding/json"
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
	for i := 0; i < 3; i++ {
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
	for i := 0; i < 3; i++ {
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
	for i := 0; i < 3; i++ {
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
