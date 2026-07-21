package session

import (
	"encoding/json"
	"fmt"
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
	killCh chan struct{}

	mu           sync.Mutex
	eventsClosed bool
	killed       bool
}

func newFakeShim() *fakeShim {
	return &fakeShim{
		events: make(chan *protocol.L1Event, 64),
		sent:   make(chan []byte, 64),
		killCh: make(chan struct{}),
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
// Run observes. Recorded (and signaled on killed-close) so hibernation
// tests can synchronize on the escalation path without polling.
func (f *fakeShim) Kill() error {
	f.mu.Lock()
	if !f.killed {
		f.killed = true
		close(f.killCh)
	}
	f.mu.Unlock()
	f.end()
	return nil
}

func (f *fakeShim) wasKilled() bool {
	f.mu.Lock()
	defer f.mu.Unlock()
	return f.killed
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
	cleanupSession(t, shim, sess)
	return &harness{shim: shim, sess: sess}
}

// cleanupSession drives a session to a terminal state and waits for its
// Run goroutine to finish. A HIBERNATED session's Done() never fires on
// its own (suspension is not death), so it must be shut down explicitly
// first; a live session terminalizes when its shim's stream closes.
func cleanupSession(t *testing.T, shim *fakeShim, sess *Session) {
	t.Helper()
	t.Cleanup(func() {
		shim.end()
		if sess.Hibernated() {
			_ = sess.Shutdown("test cleanup")
		}
		<-sess.Done()
	})
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

// newSummaryHarness is newHarness with the §2.14 per-turn summarizer wired
// to an injected fn, so a completed turn's summary is deterministic and no
// subprocess is ever spawned.
func newSummaryHarness(t *testing.T, fn summarizeFn) *harness {
	t.Helper()
	shim := newFakeShim()
	sess := New(Config{
		ID:            "sess-1",
		DaemonVersion: "0.1.0-test",
		Shim:          shim,
		Retention:     64,
		Now:           func() time.Time { return time.Date(2026, 5, 24, 12, 34, 56, 789e6, time.UTC) },
		Logf:          func(string, ...any) {},
		SummarizeFn:   fn,
	})
	go sess.Run()
	cleanupSession(t, shim, sess)
	return &harness{shim: shim, sess: sess}
}

// recvFrameOfType drains frames until one of the wanted type arrives,
// skipping the ordinary turn frames (user-turn, text-*, result) that
// precede an async task-summary.
func recvFrameOfType(t *testing.T, c *Client, typ string) map[string]any {
	t.Helper()
	for range 64 {
		f := recvFrame(t, c)
		if f["type"] == typ {
			return f
		}
	}
	t.Fatalf("did not see a %q frame", typ)
	return nil
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

func TestSessionCompletedTurnBroadcastsTaskSummary(t *testing.T) {
	// Arrange — an injected summarizer records the inputs it is handed and
	// returns a deterministic label.
	gotInputs := make(chan [2]string, 1)
	h := newSummaryHarness(t, func(prompt, responses string) string {
		gotInputs <- [2]string{prompt, responses}
		return "Objective X."
	})
	c := NewClient()
	h.sess.Attach(c)
	recvFrame(t, c) // hello
	// Act — a full turn: prompt, streamed answer, result.
	if err := h.sess.HandleClientFrame(c, []byte(`{"type":"user-message","request_id":"r1","content":"build the thing"}`)); err != nil {
		t.Fatalf("HandleClientFrame: %v", err)
	}
	recvSent(t, h.shim)
	h.shim.pushEvent(t, `{"type":"stream-event","session_id":"sess-1","uuid":"u","event":{"type":"content_block_start","index":0,"content_block":{"type":"text"}}}`)
	h.shim.pushEvent(t, `{"type":"stream-event","session_id":"sess-1","uuid":"u","event":{"type":"content_block_delta","index":0,"delta":{"type":"text_delta","text":"building the widget"}}}`)
	h.shim.pushEvent(t, `{"type":"result","session_id":"sess-1","uuid":"u2","subtype":"success","duration_ms":1,"num_turns":1}`)
	// Assert — the summary reaches the client, and the summarizer saw the
	// driving prompt plus the turn's assistant text.
	frame := recvFrameOfType(t, c, "task-summary")
	if frame["summary"] != "Objective X." {
		t.Errorf("summary = %v, want %q", frame["summary"], "Objective X.")
	}
	inputs := <-gotInputs
	if inputs[0] != "build the thing" {
		t.Errorf("prompt = %q, want %q", inputs[0], "build the thing")
	}
	if !strings.Contains(inputs[1], "building the widget") {
		t.Errorf("responses = %q, want it to contain the turn's assistant text", inputs[1])
	}
}

func TestSessionEmptyTurnSummaryBroadcastsNoFrame(t *testing.T) {
	// Arrange — a summarizer that returns "" (its best-effort failure /
	// blank contract): no frame should reach the client, so the last good
	// label stands.
	h := newSummaryHarness(t, func(string, string) string { return "" })
	c := NewClient()
	h.sess.Attach(c)
	recvFrame(t, c) // hello
	if err := h.sess.HandleClientFrame(c, []byte(`{"type":"user-message","request_id":"r1","content":"build the thing"}`)); err != nil {
		t.Fatalf("HandleClientFrame: %v", err)
	}
	recvFrame(t, c) // user-turn
	recvSent(t, h.shim)
	h.shim.pushEvent(t, `{"type":"stream-event","session_id":"sess-1","uuid":"u","event":{"type":"content_block_start","index":0,"content_block":{"type":"text"}}}`)
	h.shim.pushEvent(t, `{"type":"stream-event","session_id":"sess-1","uuid":"u","event":{"type":"content_block_delta","index":0,"delta":{"type":"text_delta","text":"working"}}}`)
	// Act
	h.shim.pushEvent(t, `{"type":"result","session_id":"sess-1","uuid":"u2","subtype":"success","duration_ms":1,"num_turns":1}`)
	// Assert — the client sees text-start/text-delta/result but never a
	// task-summary, even after the async summarizer has run.
	for range 8 {
		select {
		case data, ok := <-c.Send:
			if !ok {
				return // channel closed on cleanup; no summary was seen
			}
			var f map[string]any
			if err := json.Unmarshal(data, &f); err != nil {
				t.Fatalf("unmarshal: %v", err)
			}
			if f["type"] == "task-summary" {
				t.Fatalf("empty summary should broadcast no frame, got %v", f)
			}
		case <-time.After(recvTimeout):
			return // no more frames: the empty summary emitted nothing
		}
	}
}

func TestSessionStaleTurnSummaryIsDropped(t *testing.T) {
	// Arrange — a session whose summary epoch has already advanced past the
	// one a slow summarizer captured (a newer turn fired its own summary
	// while this one was still running).
	h := newSummaryHarness(t, func(string, string) string { return "stale objective" })
	c := NewClient()
	h.sess.Attach(c)
	recvFrame(t, c) // hello
	h.sess.mu.Lock()
	h.sess.summaryEpoch = 5
	h.sess.mu.Unlock()
	// Act — a summary captured at epoch 3 returns after epoch moved to 5.
	h.sess.summarize(3, "p", "r")
	// Assert — nothing broadcast: the stale label must not paint over the
	// current objective.
	select {
	case data := <-c.Send:
		t.Fatalf("stale summary was broadcast: %s", data)
	default:
	}
	// A summary matching the current epoch DOES broadcast.
	h.sess.summarize(5, "p", "r")
	frame := recvFrameOfType(t, c, "task-summary")
	if frame["summary"] != "stale objective" {
		t.Errorf("summary = %v", frame["summary"])
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

func TestSessionShimDeathLogsADaemonSideLine(t *testing.T) {
	// Arrange
	h, lc := newLoggedHarness(t, 16)
	c := NewClient()
	h.sess.Attach(c)
	recvFrame(t, c)
	// Act — stdout closes with no closed event (hard crash).
	h.endShim()
	<-h.sess.Done()
	// Assert — the death is named in the log, not only in the broadcast
	// error frame and the sentinel side-channel.
	got := lc.containing("shim exited without a closed event (reason=shim_died)")
	if len(got) != 1 {
		t.Fatalf("shim-death log lines = %v, want exactly one", got)
	}
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

func (r *recordingSink) AccountChanged(cwd, sid string) {
	r.record("account-changed " + cwd + " " + sid)
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

func TestSessionHelloTurnActiveFalseBeforeTurn(t *testing.T) {
	// Arrange — no turn has run, so a fresh join must not paint a running one.
	h := newHarness(t, 16)
	c := NewClient()
	// Act
	h.sess.Attach(c)
	// Assert
	hello := recvFrame(t, c)
	if hello["turn_active"] != false {
		t.Errorf("hello turn_active = %v, want false", hello["turn_active"])
	}
}

func TestSessionHelloTurnActiveTrueDuringTurn(t *testing.T) {
	// Arrange — a turn is genuinely in flight (a live session's own state).
	h := newHarness(t, 16)
	c := NewClient()
	h.sess.Attach(c)
	recvFrame(t, c) // hello
	if err := h.sess.HandleClientFrame(c, []byte(`{"type":"user-message","request_id":"u1","content":"hi"}`)); err != nil {
		t.Fatalf("HandleClientFrame: %v", err)
	}
	// Act — a second tab joins mid-turn and must pick the running turn up.
	late := NewClient()
	h.sess.Attach(late)
	// Assert
	hello := recvFrame(t, late)
	if hello["turn_active"] != true {
		t.Errorf("hello turn_active = %v, want true", hello["turn_active"])
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
	for range ringSize {
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
	for i := range ringSize {
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

// ---------------------------------------------------------------------------
// In-flight message queue (§2.13)
// ---------------------------------------------------------------------------

// gatedClassifier is a deterministic fake classifyFn: each call records
// its arguments and blocks until the test releases a verdict, so a test
// controls exactly when an async classification resolves — no time.Sleep.
type gatedClassifier struct {
	calls   chan classifyCall
	verdict chan classifierVerdict
}

type classifyCall struct{ task, msg string }

func newGatedClassifier() *gatedClassifier {
	return &gatedClassifier{
		calls:   make(chan classifyCall, 8),
		verdict: make(chan classifierVerdict, 8),
	}
}

func (g *gatedClassifier) fn(task, msg string) classifierVerdict {
	g.calls <- classifyCall{task, msg}
	return <-g.verdict
}

// awaitCall blocks until the classifier goroutine has started for the next
// queued item and returns the arguments it was invoked with.
func (g *gatedClassifier) awaitCall(t *testing.T) classifyCall {
	t.Helper()
	select {
	case c := <-g.calls:
		return c
	case <-time.After(recvTimeout):
		t.Fatal("classifier was not invoked")
		return classifyCall{}
	}
}

func (g *gatedClassifier) release(v classifierVerdict) { g.verdict <- v }

// newClassifyHarness builds a running session whose queue classifier is fn.
func newClassifyHarness(t *testing.T, fn classifyFn) *harness {
	t.Helper()
	shim := newFakeShim()
	sess := New(Config{
		ID:            "sess-1",
		DaemonVersion: "0.1.0-test",
		Shim:          shim,
		Retention:     64,
		Now:           func() time.Time { return time.Date(2026, 5, 24, 12, 34, 56, 789e6, time.UTC) },
		Logf:          func(string, ...any) {},
		ClassifyFn:    fn,
	})
	go sess.Run()
	t.Cleanup(func() {
		shim.end()
		<-sess.Done()
	})
	return &harness{shim: shim, sess: sess}
}

// startTurn attaches a client, submits an idle first turn, and drains that
// turn's user-turn broadcast and shim forward — leaving the session BUSY.
func startTurn(t *testing.T, h *harness) *Client {
	t.Helper()
	c := NewClient()
	h.sess.Attach(c)
	recvFrame(t, c) // hello
	if err := h.sess.HandleClientFrame(c, []byte(`{"type":"user-message","request_id":"turnA","content":"first task"}`)); err != nil {
		t.Fatalf("start turn: %v", err)
	}
	turn := recvFrame(t, c)
	if turn["type"] != "user-turn" || turn["request_id"] != "turnA" {
		t.Fatalf("turn A = %v", turn)
	}
	recvSent(t, h.shim) // forwarded user-message A
	return c
}

// enqueueBusy submits a message while a turn is in flight and returns the
// queue_id after the classifier goroutine has started (but not resolved).
func enqueueBusy(t *testing.T, h *harness, c *Client, g *gatedClassifier, reqID, text string) string {
	t.Helper()
	raw := fmt.Sprintf(`{"type":"user-message","request_id":%q,"content":%q}`, reqID, text)
	if err := h.sess.HandleClientFrame(c, []byte(raw)); err != nil {
		t.Fatalf("enqueue %s: %v", reqID, err)
	}
	added := waitForFrameType(t, c, "queue-added")
	if added["status"] != "classifying" || added["request_id"] != reqID {
		t.Fatalf("queue-added = %v", added)
	}
	g.awaitCall(t)
	return added["queue_id"].(string)
}

// enqueueWait fully drives a busy message to a wait verdict and returns
// its queue_id.
func enqueueWait(t *testing.T, h *harness, c *Client, g *gatedClassifier, reqID, text string) string {
	t.Helper()
	qid := enqueueBusy(t, h, c, g, reqID, text)
	g.release(classifierVerdict{verdict: "wait", reason: "handle after the current turn", source: "classifier"})
	classified := waitForFrameType(t, c, "queue-classified")
	if classified["queue_id"] != qid || classified["verdict"] != "wait" || classified["source"] != "classifier" {
		t.Fatalf("queue-classified = %v", classified)
	}
	return qid
}

func pushResult(t *testing.T, h *harness) {
	t.Helper()
	h.shim.pushEvent(t, `{"type":"result","session_id":"sess-1","uuid":"u2","subtype":"success","duration_ms":1,"num_turns":1}`)
}

func drainFrames(t *testing.T, c *Client) []map[string]any {
	t.Helper()
	var frames []map[string]any
	for {
		select {
		case data, ok := <-c.Send:
			if !ok {
				return frames
			}
			var m map[string]any
			if err := json.Unmarshal(data, &m); err != nil {
				t.Fatalf("unmarshal: %v", err)
			}
			frames = append(frames, m)
		case <-time.After(recvTimeout):
			t.Fatal("timed out draining frames")
			return frames
		}
	}
}

func TestQueueForwardsImmediatelyWhenIdle(t *testing.T) {
	// Arrange
	g := newGatedClassifier()
	h := newClassifyHarness(t, g.fn)
	c := NewClient()
	h.sess.Attach(c)
	recvFrame(t, c) // hello
	// Act — an idle submit.
	if err := h.sess.HandleClientFrame(c, []byte(`{"type":"user-message","request_id":"r1","content":"hi"}`)); err != nil {
		t.Fatalf("HandleClientFrame: %v", err)
	}
	// Assert — a user-turn (not queue-added), forwarded, classifier untouched.
	turn := recvFrame(t, c)
	if turn["type"] != "user-turn" {
		t.Fatalf("frame = %v, want user-turn", turn)
	}
	if !strings.Contains(string(recvSent(t, h.shim)), `"user-message"`) {
		t.Error("idle message not forwarded")
	}
	select {
	case call := <-g.calls:
		t.Fatalf("idle submit must not classify: %+v", call)
	default:
	}
}

func TestQueueEnqueuesWhenBusyAndDoesNotForward(t *testing.T) {
	// Arrange
	g := newGatedClassifier()
	h := newClassifyHarness(t, g.fn)
	c := startTurn(t, h)
	// Act — a second message while the first turn is in flight.
	if err := h.sess.HandleClientFrame(c, []byte(`{"type":"user-message","request_id":"r2","content":"second"}`)); err != nil {
		t.Fatalf("busy submit: %v", err)
	}
	// Assert — queue-added(classifying), classifier fired with the running
	// task, and NOTHING forwarded to the shim.
	added := recvFrame(t, c)
	if added["type"] != "queue-added" || added["status"] != "classifying" || added["request_id"] != "r2" {
		t.Fatalf("added = %v", added)
	}
	call := g.awaitCall(t)
	if call.task != "first task" || call.msg != "second" {
		t.Errorf("classifier call = %+v", call)
	}
	select {
	case line := <-h.shim.sent:
		t.Fatalf("busy message must not be forwarded: %s", line)
	default:
	}
}

func TestQueueDrainsInFIFOOrderOnResult(t *testing.T) {
	// Arrange — two messages queued behind the running turn, both "wait".
	g := newGatedClassifier()
	h := newClassifyHarness(t, g.fn)
	c := startTurn(t, h)
	enqueueWait(t, h, c, g, "rB", "second")
	enqueueWait(t, h, c, g, "rC", "third")
	// Act — the running turn ends; the front item drains.
	pushResult(t, h)
	// Assert — B drains first (FIFO), broadcasting its user-turn +
	// queue-removed(drained), and forwarding B to the shim.
	waitForFrameType(t, c, "result")
	turnB := waitForFrameType(t, c, "user-turn")
	if turnB["request_id"] != "rB" {
		t.Fatalf("first drain = %v, want rB", turnB)
	}
	removedB := waitForFrameType(t, c, "queue-removed")
	if removedB["reason"] != "drained" || removedB["request_id"] != "rB" {
		t.Fatalf("removed B = %v", removedB)
	}
	if !strings.Contains(string(recvSent(t, h.shim)), "second") {
		t.Error("B not forwarded to shim")
	}
	// Act — B's turn ends; C drains next.
	pushResult(t, h)
	waitForFrameType(t, c, "result")
	turnC := waitForFrameType(t, c, "user-turn")
	if turnC["request_id"] != "rC" {
		t.Fatalf("second drain = %v, want rC", turnC)
	}
	if !strings.Contains(string(recvSent(t, h.shim)), "third") {
		t.Error("C not forwarded to shim")
	}
}

func TestQueueClassifierInterruptPreemptsRunningTurn(t *testing.T) {
	// Arrange
	g := newGatedClassifier()
	h := newClassifyHarness(t, g.fn)
	c := startTurn(t, h)
	qid := enqueueBusy(t, h, c, g, "rB", "stop, wrong file")
	// Act — the classifier says interrupt.
	g.release(classifierVerdict{verdict: "interrupt", reason: "countermands the running task", source: "classifier"})
	// Assert — queue-classified(interrupt), and an interrupt to the shim.
	classified := waitForFrameType(t, c, "queue-classified")
	if classified["verdict"] != "interrupt" || classified["source"] != "classifier" || classified["queue_id"] != qid {
		t.Fatalf("classified = %v", classified)
	}
	if !strings.Contains(string(recvSent(t, h.shim)), `"interrupt"`) {
		t.Error("escalation did not interrupt the shim")
	}
	// The aborted turn's result then drains the now-front item.
	pushResult(t, h)
	turnB := waitForFrameType(t, c, "user-turn")
	if turnB["request_id"] != "rB" {
		t.Fatalf("drained turn = %v, want rB", turnB)
	}
	if !strings.Contains(string(recvSent(t, h.shim)), "stop, wrong file") {
		t.Error("promoted item not forwarded after abort")
	}
}

func TestQueueClassifierWaitLeavesItemQueued(t *testing.T) {
	// Arrange
	g := newGatedClassifier()
	h := newClassifyHarness(t, g.fn)
	c := startTurn(t, h)
	qid := enqueueWait(t, h, c, g, "rB", "second")
	// Assert — still queued in "waiting"; the shim was not touched.
	info := h.sess.Info()
	if len(info.Queue) != 1 || info.Queue[0].QueueID != qid ||
		info.Queue[0].Status != "waiting" || info.Queue[0].Verdict != "wait" {
		t.Fatalf("queue = %+v", info.Queue)
	}
	select {
	case line := <-h.shim.sent:
		t.Fatalf("a wait verdict must not touch the shim: %s", line)
	default:
	}
}

func TestQueueFailClosedVerdictLeavesItemQueued(t *testing.T) {
	// Arrange — a classifier that fails closed yields wait/fallback; the
	// session must treat it like any other wait (item stays queued).
	fallback := func(task, msg string) classifierVerdict {
		return classifierVerdict{verdict: "wait", reason: "classifier unavailable", source: "fallback"}
	}
	h := newClassifyHarness(t, fallback)
	c := startTurn(t, h)
	// Act
	if err := h.sess.HandleClientFrame(c, []byte(`{"type":"user-message","request_id":"rB","content":"second"}`)); err != nil {
		t.Fatalf("busy submit: %v", err)
	}
	// Assert
	waitForFrameType(t, c, "queue-added")
	classified := waitForFrameType(t, c, "queue-classified")
	if classified["verdict"] != "wait" || classified["source"] != "fallback" {
		t.Fatalf("classified = %v, want wait/fallback", classified)
	}
	if len(h.sess.Info().Queue) != 1 {
		t.Fatalf("queue = %+v, want the item still parked", h.sess.Info().Queue)
	}
}

func TestQueueDisabledClassificationDefaultsToWait(t *testing.T) {
	// Arrange — classifyFn nil (classification disabled).
	h := newHarness(t, 64)
	c := startTurn(t, h)
	// Act
	if err := h.sess.HandleClientFrame(c, []byte(`{"type":"user-message","request_id":"rB","content":"second"}`)); err != nil {
		t.Fatalf("busy submit: %v", err)
	}
	// Assert — an immediate defaulted wait, no subprocess, item stays queued.
	waitForFrameType(t, c, "queue-added")
	classified := waitForFrameType(t, c, "queue-classified")
	if classified["verdict"] != "wait" || classified["source"] != "fallback" {
		t.Fatalf("classified = %v, want wait/fallback", classified)
	}
	if len(h.sess.Info().Queue) != 1 {
		t.Fatalf("queue = %+v", h.sess.Info().Queue)
	}
}

func TestQueueCancelRemovesWithoutForwarding(t *testing.T) {
	// Arrange
	g := newGatedClassifier()
	h := newClassifyHarness(t, g.fn)
	c := startTurn(t, h)
	qid := enqueueWait(t, h, c, g, "rB", "second")
	// Act
	raw := fmt.Sprintf(`{"type":"queue-cancel","request_id":"x","queue_id":%q}`, qid)
	if err := h.sess.HandleClientFrame(c, []byte(raw)); err != nil {
		t.Fatalf("cancel: %v", err)
	}
	// Assert
	removed := waitForFrameType(t, c, "queue-removed")
	if removed["reason"] != "cancelled" || removed["queue_id"] != qid {
		t.Fatalf("removed = %v", removed)
	}
	if len(h.sess.Info().Queue) != 0 {
		t.Fatalf("queue = %+v, want empty", h.sess.Info().Queue)
	}
}

func TestQueueRunNowEscalatesLikeInterrupt(t *testing.T) {
	// Arrange
	g := newGatedClassifier()
	h := newClassifyHarness(t, g.fn)
	c := startTurn(t, h)
	qid := enqueueWait(t, h, c, g, "rB", "second")
	// Act — the user runs it now.
	raw := fmt.Sprintf(`{"type":"queue-run-now","request_id":"x","queue_id":%q}`, qid)
	if err := h.sess.HandleClientFrame(c, []byte(raw)); err != nil {
		t.Fatalf("run-now: %v", err)
	}
	// Assert — queue-classified(interrupt, source user) + interrupt to shim.
	classified := waitForFrameType(t, c, "queue-classified")
	if classified["verdict"] != "interrupt" || classified["source"] != "user" || classified["queue_id"] != qid {
		t.Fatalf("classified = %v", classified)
	}
	if !strings.Contains(string(recvSent(t, h.shim)), `"interrupt"`) {
		t.Error("run-now did not interrupt the shim")
	}
	// The aborted result drains the promoted item.
	pushResult(t, h)
	turnB := waitForFrameType(t, c, "user-turn")
	if turnB["request_id"] != "rB" {
		t.Fatalf("drained = %v, want rB", turnB)
	}
}

func TestQueueUserInterruptDropsEveryParkedItem(t *testing.T) {
	// Arrange — two items parked behind a running turn.
	g := newGatedClassifier()
	h := newClassifyHarness(t, g.fn)
	c := startTurn(t, h)
	qidB := enqueueWait(t, h, c, g, "rB", "second")
	qidC := enqueueWait(t, h, c, g, "rC", "third")
	// Act — a user interrupt (webapp Esc / Emacs C-c C-k): stop everything.
	if err := h.sess.HandleClientFrame(c, []byte(`{"type":"interrupt","request_id":"x"}`)); err != nil {
		t.Fatalf("interrupt: %v", err)
	}
	// Assert — a queue-removed(interrupted) per parked item, front-to-back.
	removedB := waitForFrameType(t, c, "queue-removed")
	if removedB["reason"] != "interrupted" || removedB["queue_id"] != qidB {
		t.Fatalf("removed B = %v", removedB)
	}
	removedC := waitForFrameType(t, c, "queue-removed")
	if removedC["reason"] != "interrupted" || removedC["queue_id"] != qidC {
		t.Fatalf("removed C = %v", removedC)
	}
	if len(h.sess.Info().Queue) != 0 {
		t.Fatalf("queue = %+v, want empty", h.sess.Info().Queue)
	}
}

func TestQueueDoesNotDrainAfterUserInterrupt(t *testing.T) {
	// Arrange — a parked item behind a running turn, then a user interrupt.
	g := newGatedClassifier()
	h := newClassifyHarness(t, g.fn)
	c := startTurn(t, h)
	enqueueWait(t, h, c, g, "rB", "second")
	if err := h.sess.HandleClientFrame(c, []byte(`{"type":"interrupt","request_id":"x"}`)); err != nil {
		t.Fatalf("interrupt: %v", err)
	}
	if !strings.Contains(string(recvSent(t, h.shim)), `"interrupt"`) {
		t.Fatal("interrupt not forwarded")
	}
	// Act — the aborted turn's result lands.
	pushResult(t, h)
	waitForFrameType(t, c, "result")
	// Info serializes with Run's locked event section, so once it returns
	// any drain the result triggered has fully broadcast and forwarded.
	_ = h.sess.Info()
	// Assert — nothing auto-starts: no further frame, nothing forwarded.
	select {
	case data := <-c.Send:
		var m map[string]any
		_ = json.Unmarshal(data, &m)
		t.Fatalf("post-interrupt frame = %v, want silence", m)
	default:
	}
	select {
	case line := <-h.shim.sent:
		t.Fatalf("post-interrupt forward = %s, want none", line)
	default:
	}
}

func TestQueueStaleOverrideIsNoOpNotError(t *testing.T) {
	// Arrange
	g := newGatedClassifier()
	h := newClassifyHarness(t, g.fn)
	c := startTurn(t, h)
	// Act + Assert — a queue_id that was never issued: no-op ack, no frame.
	if err := h.sess.HandleClientFrame(c, []byte(`{"type":"queue-cancel","request_id":"x","queue_id":"ghost"}`)); err != nil {
		t.Fatalf("stale cancel should be a no-op ack: %v", err)
	}
	if err := h.sess.HandleClientFrame(c, []byte(`{"type":"queue-run-now","request_id":"y","queue_id":"ghost"}`)); err != nil {
		t.Fatalf("stale run-now should be a no-op ack: %v", err)
	}
	select {
	case data := <-c.Send:
		var m map[string]any
		_ = json.Unmarshal(data, &m)
		t.Fatalf("stale override emitted a frame: %v", m)
	default:
	}
}

func TestQueueSessionEndClearsQueue(t *testing.T) {
	// Arrange — two items parked behind a running turn.
	g := newGatedClassifier()
	h := newClassifyHarness(t, g.fn)
	c := startTurn(t, h)
	qidB := enqueueWait(t, h, c, g, "rB", "second")
	qidC := enqueueWait(t, h, c, g, "rC", "third")
	// Act — hard shim death ends the session.
	h.endShim()
	<-h.sess.Done()
	// Assert — a queue-removed(session_end) for each parked item.
	dropped := map[string]bool{}
	for _, f := range drainFrames(t, c) {
		if f["type"] == "queue-removed" && f["reason"] == "session_end" {
			dropped[f["queue_id"].(string)] = true
		}
	}
	if !dropped[qidB] || !dropped[qidC] {
		t.Fatalf("session_end drops = %v, want both %s and %s", dropped, qidB, qidC)
	}
}

func TestQueueSnapshotOnHelloAndInfo(t *testing.T) {
	// Arrange
	g := newGatedClassifier()
	h := newClassifyHarness(t, g.fn)
	c := startTurn(t, h)
	qid := enqueueWait(t, h, c, g, "rB", "second task")
	// Assert — Info carries the snapshot.
	info := h.sess.Info()
	if len(info.Queue) != 1 || info.Queue[0].QueueID != qid || info.Queue[0].Status != "waiting" {
		t.Fatalf("Info queue = %+v", info.Queue)
	}
	// A late joiner's hello carries the same queue snapshot.
	late := NewClient()
	h.sess.Attach(late)
	hello := recvFrame(t, late)
	queue, ok := hello["queue"].([]any)
	if !ok || len(queue) != 1 {
		t.Fatalf("hello queue = %v", hello["queue"])
	}
	item := queue[0].(map[string]any)
	if item["queue_id"] != qid || item["status"] != "waiting" || item["verdict"] != "wait" || item["request_id"] != "rB" {
		t.Fatalf("hello queue item = %v", item)
	}
}

// --- turn retraction (§2.3 user-turn-retracted) ------------------------------

// submitTurn opens a turn carrying reqID and drains the forwarded command,
// leaving the session with that turn in flight and unanswered.
func submitTurn(t *testing.T, h *harness, c *Client, reqID string) {
	t.Helper()
	raw := []byte(`{"type":"user-message","request_id":"` + reqID + `","content":"hi"}`)
	if err := h.sess.HandleClientFrame(c, raw); err != nil {
		t.Fatalf("submit %s: %v", reqID, err)
	}
	waitForFrameType(t, c, "user-turn")
	recvSent(t, h.shim)
}

func TestRetractActiveTurnWithdrawsAnUnansweredTurn(t *testing.T) {
	// Arrange — a turn in flight that the agent has not answered.
	h := newHarness(t, 64)
	c := NewClient()
	h.sess.Attach(c)
	recvFrame(t, c) // hello
	submitTurn(t, h, c, "r1")
	// Act
	got := h.sess.RetractActiveTurn("r1")
	// Assert — the retraction happened and named the withdrawn turn.
	if !got {
		t.Fatal("RetractActiveTurn(r1) = false for an unanswered turn, want true")
	}
	frame := waitForFrameType(t, c, "user-turn-retracted")
	if frame["request_id"] != "r1" {
		t.Errorf("request_id = %v, want r1", frame["request_id"])
	}
}

func TestRetractActiveTurnRefusesAnAnsweredTurn(t *testing.T) {
	// Arrange — a turn in flight whose agent has opened a text bubble.
	h := newHarness(t, 64)
	c := NewClient()
	h.sess.Attach(c)
	recvFrame(t, c) // hello
	submitTurn(t, h, c, "r1")
	h.shim.pushEvent(t, `{"type":"stream-event","session_id":"s1","uuid":"u","event":{"type":"content_block_start","index":0,"content_block":{"type":"text","text":""}}}`)
	waitForFrameType(t, c, "text-start")
	// Act
	got := h.sess.RetractActiveTurn("r1")
	// Assert — the prompt has an answer on screen, so it stays.
	if got {
		t.Error("RetractActiveTurn(r1) = true after text-start, want false")
	}
}

func TestRetractActiveTurnRefusesAStaleRequestID(t *testing.T) {
	// Arrange — a turn in flight under a different id than the caller names.
	h := newHarness(t, 64)
	c := NewClient()
	h.sess.Attach(c)
	recvFrame(t, c) // hello
	submitTurn(t, h, c, "r1")
	// Act — the caller raced: the turn it means to withdraw is not this one.
	got := h.sess.RetractActiveTurn("r-stale")
	// Assert
	if got {
		t.Error("RetractActiveTurn(r-stale) = true, want false")
	}
}

func TestRetractActiveTurnRefusesWhenIdle(t *testing.T) {
	// Arrange — no turn ever opened.
	h := newHarness(t, 64)
	c := NewClient()
	h.sess.Attach(c)
	recvFrame(t, c) // hello
	// Act
	got := h.sess.RetractActiveTurn("r1")
	// Assert
	if got {
		t.Error("RetractActiveTurn on an idle session = true, want false")
	}
}

func TestRetractActiveTurnRefusesAnEmptyRequestID(t *testing.T) {
	// Arrange — a retractable turn, but a caller that names no turn at all
	// (the plain interrupt's empty field).
	h := newHarness(t, 64)
	c := NewClient()
	h.sess.Attach(c)
	recvFrame(t, c) // hello
	submitTurn(t, h, c, "r1")
	// Act
	got := h.sess.RetractActiveTurn("")
	// Assert — an unnamed retraction must never fall through to the active turn.
	if got {
		t.Error(`RetractActiveTurn("") = true, want false`)
	}
}

func TestRetractActiveTurnIsRefusedTwiceForTheSameTurn(t *testing.T) {
	// Arrange — a turn already withdrawn once.
	h := newHarness(t, 64)
	c := NewClient()
	h.sess.Attach(c)
	recvFrame(t, c) // hello
	submitTurn(t, h, c, "r1")
	if !h.sess.RetractActiveTurn("r1") {
		t.Fatal("first RetractActiveTurn(r1) = false, want true")
	}
	waitForFrameType(t, c, "user-turn-retracted")
	// Act — a duplicate C-c C-k on a bubble that is already gone.
	got := h.sess.RetractActiveTurn("r1")
	// Assert — the retraction frame is broadcast once, never twice.
	if got {
		t.Error("second RetractActiveTurn(r1) = true, want false")
	}
}

func TestRetractedTurnIsWithdrawnForReplayingClients(t *testing.T) {
	// Arrange — a turn withdrawn while one client watched.
	h := newHarness(t, 64)
	c := NewClient()
	h.sess.Attach(c)
	recvFrame(t, c) // hello
	submitTurn(t, h, c, "r1")
	h.sess.RetractActiveTurn("r1")
	waitForFrameType(t, c, "user-turn-retracted")
	// Act — a fresh client rebuilds the feed from the retained ring.
	late := NewClient()
	h.sess.Attach(late)
	recvFrame(t, late) // hello
	if err := h.sess.HandleClientFrame(late, []byte(`{"type":"replay-request","from_seq":1}`)); err != nil {
		t.Fatalf("replay-request: %v", err)
	}
	// Assert — the retraction is retained, so the replayed feed drops the
	// bubble exactly as the live one did.
	frame := waitForFrameType(t, late, "user-turn-retracted")
	if frame["request_id"] != "r1" {
		t.Errorf("replayed request_id = %v, want r1", frame["request_id"])
	}
}

// --- delivery-path logging -----------------------------------------------------

// logCapture collects Logf lines for assertions, safely across the
// session's goroutines.
type logCapture struct {
	mu    sync.Mutex
	lines []string
}

func (lc *logCapture) logf(format string, args ...any) {
	lc.mu.Lock()
	defer lc.mu.Unlock()
	lc.lines = append(lc.lines, fmt.Sprintf(format, args...))
}

// containing returns the captured lines that contain SUB.
func (lc *logCapture) containing(sub string) []string {
	lc.mu.Lock()
	defer lc.mu.Unlock()
	var out []string
	for _, l := range lc.lines {
		if strings.Contains(l, sub) {
			out = append(out, l)
		}
	}
	return out
}

// newLoggedHarness is newHarness with Logf captured for assertions.
func newLoggedHarness(t *testing.T, retention int) (*harness, *logCapture) {
	t.Helper()
	lc := &logCapture{}
	shim := newFakeShim()
	sess := New(Config{
		ID:            "sess-1",
		DaemonVersion: "0.1.0-test",
		Shim:          shim,
		Retention:     retention,
		Now:           func() time.Time { return time.Date(2026, 5, 24, 12, 34, 56, 789e6, time.UTC) },
		Logf:          lc.logf,
	})
	go sess.Run()
	cleanupSession(t, shim, sess)
	return &harness{shim: shim, sess: sess}, lc
}

func TestAttachLogsClientCountAndHelloWatermarks(t *testing.T) {
	// Arrange
	h, lc := newLoggedHarness(t, 16)
	c := NewClient()
	// Act
	h.sess.Attach(c)
	// Assert
	got := lc.containing("client attached (1 attached); hello resume_from_seq=0 seq=0")
	if len(got) != 1 {
		t.Fatalf("attach log lines = %v, want exactly one", got)
	}
}

func TestDetachLogsRemainingClientCount(t *testing.T) {
	// Arrange
	h, lc := newLoggedHarness(t, 16)
	c := NewClient()
	h.sess.Attach(c)
	// Act
	h.sess.Detach(c)
	// Assert
	got := lc.containing("client detached (0 attached)")
	if len(got) != 1 {
		t.Fatalf("detach log lines = %v, want exactly one", got)
	}
}

func TestReplayRequestLogsServedFrameCount(t *testing.T) {
	// Arrange — one retained frame in the ring.
	h, lc := newLoggedHarness(t, 16)
	c := NewClient()
	h.sess.Attach(c)
	recvFrame(t, c) // hello
	h.shim.pushEvent(t, `{"type":"system","session_id":"sess-1","uuid":"u","subtype":"init","data":{}}`)
	recvFrame(t, c)
	// Act
	if err := h.sess.HandleClientFrame(c, []byte(`{"type":"replay-request","from_seq":1}`)); err != nil {
		t.Fatalf("replay-request: %v", err)
	}
	// Assert
	got := lc.containing("replay-request from_seq=1 served 1 frames (ring [1..1])")
	if len(got) != 1 {
		t.Fatalf("replay log lines = %v, want exactly one", got)
	}
}

func TestReplayRequestBeforeRingLogsTheFreshHelloFallback(t *testing.T) {
	// Arrange — an empty ring cannot serve any from_seq.
	h, lc := newLoggedHarness(t, 16)
	c := NewClient()
	h.sess.Attach(c)
	recvFrame(t, c) // hello
	// Act
	if err := h.sess.HandleClientFrame(c, []byte(`{"type":"replay-request","from_seq":1}`)); err != nil {
		t.Fatalf("replay-request: %v", err)
	}
	// Assert — the fallback hello went out and the fallback was logged.
	hello := recvFrame(t, c)
	if hello["type"] != "hello" {
		t.Fatalf("fallback frame type = %v, want hello", hello["type"])
	}
	got := lc.containing("replay-request from_seq=1 predates ring [0..0] — answering with a fresh hello")
	if len(got) != 1 {
		t.Fatalf("fallback log lines = %v, want exactly one", got)
	}
}

func TestClientLogLandsInTheDaemonLog(t *testing.T) {
	// Arrange
	h, lc := newLoggedHarness(t, 16)
	c := NewClient()
	h.sess.Attach(c)
	recvFrame(t, c) // hello
	// Act
	err := h.sess.HandleClientFrame(c, []byte(`{"type":"client-log","level":"warn","message":"render stall: rAF pending 2000ms"}`))
	// Assert
	if err != nil {
		t.Fatalf("client-log: %v", err)
	}
	got := lc.containing("webapp warn: render stall: rAF pending 2000ms")
	if len(got) != 1 {
		t.Fatalf("client-log lines = %v, want exactly one", got)
	}
}

func TestClientLogWorksOnAHibernatedSessionWithoutReviving(t *testing.T) {
	// Arrange — a hibernated session; observation must stay free.
	h, lc := newLoggedHarness(t, 16)
	initEvent(t, h)
	hibernateNow(t, h)
	late := NewClient()
	h.sess.Attach(late)
	recvFrame(t, late) // hello
	// Act
	err := h.sess.HandleClientFrame(late, []byte(`{"type":"client-log","level":"info","message":"m"}`))
	// Assert
	if err != nil {
		t.Fatalf("client-log on hibernated session: %v", err)
	}
	if !h.sess.Hibernated() {
		t.Fatal("client-log revived the session; diagnostics must stay free")
	}
	if got := lc.containing("webapp info: m"); len(got) != 1 {
		t.Fatalf("client-log lines = %v, want exactly one", got)
	}
}

func TestClientLogWorksOnATerminalSession(t *testing.T) {
	// Arrange — the session ends, then a wedged client reports in.
	h, lc := newLoggedHarness(t, 16)
	c := NewClient()
	h.sess.Attach(c)
	recvFrame(t, c) // hello
	h.shim.pushEvent(t, `{"type":"closed","session_id":"sess-1","exit_code":0,"reason":"sdk_end"}`)
	h.endShim()
	<-h.sess.Done()
	late := NewClient()
	h.sess.Attach(late)
	recvFrame(t, late) // hello
	// Act
	err := h.sess.HandleClientFrame(late, []byte(`{"type":"client-log","level":"error","message":"m"}`))
	// Assert
	if err != nil {
		t.Fatalf("client-log on terminal session: %v", err)
	}
	if got := lc.containing("webapp error: m"); len(got) != 1 {
		t.Fatalf("client-log lines = %v, want exactly one", got)
	}
}

func TestClientLogClampsAnOversizedMessage(t *testing.T) {
	// Arrange — one frame larger than the daemon is willing to mirror.
	h, lc := newLoggedHarness(t, 16)
	c := NewClient()
	h.sess.Attach(c)
	recvFrame(t, c) // hello
	huge := strings.Repeat("x", clientLogMaxLen+100)
	// Act
	err := h.sess.HandleClientFrame(c, []byte(`{"type":"client-log","level":"info","message":"`+huge+`"}`))
	// Assert
	if err != nil {
		t.Fatalf("client-log: %v", err)
	}
	got := lc.containing("…[clamped]")
	if len(got) != 1 {
		t.Fatal("oversized client-log was not clamped")
	}
	if len(got[0]) > clientLogMaxLen+100 {
		t.Fatalf("clamped line still %d bytes", len(got[0]))
	}
}

func TestSendToDetachedClientLogsTheSkip(t *testing.T) {
	// Arrange — a client that was never attached.
	h, lc := newLoggedHarness(t, 16)
	c := NewClient()
	// Act
	h.sess.mu.Lock()
	sent := h.sess.sendToClientLocked(c, []byte(`{}`))
	h.sess.mu.Unlock()
	// Assert
	if sent {
		t.Fatal("send to a detached client reported success")
	}
	got := lc.containing("send skipped — client already detached")
	if len(got) != 1 {
		t.Fatalf("skip log lines = %v, want exactly one", got)
	}
}
