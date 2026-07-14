package session

import (
	"errors"
	"strings"
	"testing"
	"time"
)

// initEvent drives system:init so the session has a durable resume target
// (the CLI session uuid), which every hibernation gate but the refusals
// requires. Returns the attached client so callers can drive more frames.
func initEvent(t *testing.T, h *harness) *Client {
	t.Helper()
	c := NewClient()
	h.sess.Attach(c)
	recvFrame(t, c) // hello
	h.shim.pushEvent(t, `{"type":"system","session_id":"sess-1","uuid":"u","subtype":"init","data":{"model":"m","cwd":"/w","permissionMode":"default","session_id":"cli-uuid-1"}}`)
	waitForFrameType(t, c, "system")
	return c
}

// hibernateNow suspends the harness's session cooperatively and blocks
// until it has settled.
func hibernateNow(t *testing.T, h *harness) {
	t.Helper()
	driveHibernate(t, h.shim, h.sess)
}

// driveHibernate suspends sess cooperatively and blocks until it settles,
// synchronizing on the shim's shutdown command and the session's hibernate
// signal (no polling). It mirrors the REAL shim's shutdown response: the
// shim emits its own `closed` (reason "shutdown") and THEN closes stdout,
// so a hibernation drain must never be mistaken for a death.
func driveHibernate(t *testing.T, shim *fakeShim, sess *Session) {
	t.Helper()
	if err := sess.Hibernate("idle"); err != nil {
		t.Fatalf("Hibernate: %v", err)
	}
	if line := recvSent(t, shim); !strings.Contains(string(line), "shutdown") {
		t.Fatalf("forwarded = %s, want a shutdown command", line)
	}
	sig := sess.HibernateDone()
	shim.pushEvent(t, `{"type":"closed","session_id":"sess-1","reason":"shutdown","exit_code":0}`)
	shim.end()
	select {
	case <-sig:
	case <-time.After(recvTimeout):
		t.Fatal("session never reached the hibernated state")
	}
}

func TestHibernateRefusedWithoutResumeTarget(t *testing.T) {
	// Arrange — no system:init, so no claude_session_id exists yet.
	h := newHarness(t, 16)
	// Act
	err := h.sess.Hibernate("idle")
	// Assert — suspending would destroy an unrecoverable session.
	if !errors.Is(err, ErrNoResumeTarget) {
		t.Fatalf("Hibernate err = %v, want ErrNoResumeTarget", err)
	}
	if h.sess.Hibernated() {
		t.Fatal("session hibernated despite refusal")
	}
}

func TestHibernateRefusedWithTurnActive(t *testing.T) {
	// Arrange — a turn is in flight.
	h := newHarness(t, 16)
	c := initEvent(t, h)
	if err := h.sess.HandleClientFrame(c, []byte(`{"type":"user-message","request_id":"u1","content":"hi"}`)); err != nil {
		t.Fatalf("HandleClientFrame: %v", err)
	}
	// Act
	err := h.sess.Hibernate("idle")
	// Assert — the in-flight response lives only in the CLI.
	if !errors.Is(err, ErrTurnActive) {
		t.Fatalf("Hibernate err = %v, want ErrTurnActive", err)
	}
}

func TestHibernateRefusedWithPendingPermission(t *testing.T) {
	// Arrange — an unresolved permission request.
	h := newHarness(t, 16)
	c := initEvent(t, h)
	h.shim.pushEvent(t, `{"type":"permission-request","session_id":"sess-1","request_id":"p1","tool_use_id":"t1","tool_name":"Bash","input":{}}`)
	waitForFrameType(t, c, "permission-request")
	// Act
	err := h.sess.Hibernate("idle")
	// Assert — the request exists only in memory, never in the transcript.
	if !errors.Is(err, ErrPermissionPending) {
		t.Fatalf("Hibernate err = %v, want ErrPermissionPending", err)
	}
}

func TestHibernateRefusedWhenTerminal(t *testing.T) {
	// Arrange — a dead session.
	h := newHarness(t, 16)
	h.endShim()
	<-h.sess.Done()
	// Act
	err := h.sess.Hibernate("idle")
	// Assert
	if !errors.Is(err, ErrNotHibernatable) {
		t.Fatalf("Hibernate err = %v, want ErrNotHibernatable", err)
	}
}

func TestHibernateCooperativeSuspendsWithoutKill(t *testing.T) {
	// Arrange
	h := newHarness(t, 16)
	initEvent(t, h)
	// Act
	hibernateNow(t, h)
	// Assert — suspended, NOT terminal, and honoring the cooperative
	// shutdown means the escalation kill never fired.
	if !h.sess.Hibernated() {
		t.Fatal("session not hibernated after cooperative shutdown")
	}
	if h.sess.Terminal() {
		t.Fatal("cooperative hibernation marked the session terminal")
	}
	if h.shim.wasKilled() {
		t.Fatal("shim was killed despite honoring the cooperative shutdown")
	}
}

func TestHibernateRetainsHistory(t *testing.T) {
	// Arrange — a session with a broadcast frame in its ring.
	h := newHarness(t, 16)
	initEvent(t, h)
	// Act
	hibernateNow(t, h)
	// Assert — a client attaching to the hibernated session still gets a
	// non-zero cursor, i.e. the retained history survived.
	c := NewClient()
	h.sess.Attach(c)
	hello := recvFrame(t, c)
	if hello["type"] != "hello" || hello["resume_from_seq"] == float64(0) {
		t.Fatalf("hello = %v, want a non-zero resume cursor from retained history", hello)
	}
}

func TestHibernateEscalatesToKillWhenShutdownIgnored(t *testing.T) {
	// Arrange — a shim that will NOT honor shutdown, with a tiny grace so
	// the escalation fires promptly.
	shim := newFakeShim()
	sess := New(Config{
		ID:             "sess-1",
		DaemonVersion:  "0.1.0-test",
		Shim:           shim,
		Retention:      16,
		Now:            func() time.Time { return time.Unix(0, 0).UTC() },
		Logf:           func(string, ...any) {},
		HibernateGrace: 20 * time.Millisecond,
	})
	go sess.Run()
	cleanupSession(t, shim, sess)
	c := NewClient()
	sess.Attach(c)
	recvFrame(t, c)
	shim.pushEvent(t, `{"type":"system","session_id":"sess-1","uuid":"u","subtype":"init","data":{"session_id":"cli-uuid-1"}}`)
	waitForFrameType(t, c, "system")
	// Act — hibernate, then never close the event stream ourselves.
	if err := sess.Hibernate("idle"); err != nil {
		t.Fatalf("Hibernate: %v", err)
	}
	sig := sess.HibernateDone()
	// Assert — the grace elapses and the shim is killed.
	select {
	case <-shim.killCh:
	case <-time.After(recvTimeout):
		t.Fatal("shim ignoring shutdown was never killed")
	}
	// Let the suspension settle so cleanup sees a hibernated session.
	select {
	case <-sig:
	case <-time.After(recvTimeout):
		t.Fatal("session never settled into hibernation after the kill")
	}
}

func TestReviveRestoresLiveShimAndKeepsHistory(t *testing.T) {
	// Arrange — a hibernated session.
	h := newHarness(t, 16)
	initEvent(t, h)
	hibernateNow(t, h)
	// Act — revive with a fresh shim.
	newShim := newFakeShim()
	t.Cleanup(func() { newShim.end() })
	if err := h.sess.Revive(newShim); err != nil {
		t.Fatalf("Revive: %v", err)
	}
	// Assert — live again, and a new event flows through the fresh shim.
	if h.sess.Hibernated() {
		t.Fatal("session still hibernated after Revive")
	}
	c := NewClient()
	h.sess.Attach(c)
	recvFrame(t, c) // hello with preserved cursor
	newShim.pushEvent(t, `{"type":"result","session_id":"sess-1","uuid":"u9","subtype":"success","duration_ms":1,"num_turns":1}`)
	waitForFrameType(t, c, "result")
}

func TestReviveRefusedWhenNotHibernated(t *testing.T) {
	// Arrange — a live (non-hibernated) session.
	h := newHarness(t, 16)
	initEvent(t, h)
	// Act
	err := h.sess.Revive(newFakeShim())
	// Assert
	if err == nil {
		t.Fatal("Revive on a live session should fail")
	}
}

func TestActingCommandRefusedOnHibernatedSession(t *testing.T) {
	// Arrange — a hibernated session.
	h := newHarness(t, 16)
	initEvent(t, h)
	hibernateNow(t, h)
	// Act — an acting command with no live shim must be rejected loudly,
	// never nil-panic. (The server revives BEFORE handing the frame over;
	// this guards the contract.)
	c := NewClient()
	h.sess.Attach(c)
	recvFrame(t, c) // hello
	err := h.sess.HandleClientFrame(c, []byte(`{"type":"user-message","request_id":"u1","content":"hi"}`))
	// Assert
	if err == nil {
		t.Fatal("acting on a hibernated session should error")
	}
}

func TestHibernateDoesNotMarkSessionTerminal(t *testing.T) {
	// Arrange — the REAL shim emits a `closed` (reason "shutdown") as it
	// drains, which must NOT be read as a death.
	h := newHarness(t, 16)
	initEvent(t, h)
	// Act
	hibernateNow(t, h)
	// Assert — suspended, explicitly NOT terminal.
	if h.sess.Terminal() {
		t.Fatal("hibernation drain (closed reason=shutdown) marked the session terminal")
	}
	if !h.sess.Hibernated() {
		t.Fatal("session not hibernated")
	}
}

func TestHibernateDoesNotNotifyRegistrarTerminal(t *testing.T) {
	// Arrange — a session with a recording registrar. A terminal
	// notification here would mark the registry record terminal and stop
	// it rehydrating on the next daemon boot.
	shim := newFakeShim()
	reg := &recordingRegistrar{}
	sess := New(Config{
		ID: "sess-1", DaemonVersion: "0.1.0-test", Shim: shim, Retention: 16,
		Now:       func() time.Time { return time.Unix(0, 0).UTC() },
		Logf:      func(string, ...any) {},
		Registrar: reg,
	})
	go sess.Run()
	cleanupSession(t, shim, sess)
	c := NewClient()
	sess.Attach(c)
	recvFrame(t, c) // hello
	shim.pushEvent(t, `{"type":"system","session_id":"sess-1","uuid":"u","subtype":"init","data":{"session_id":"cli-uuid-1"}}`)
	waitForFrameType(t, c, "system")
	// Act
	driveHibernate(t, shim, sess)
	// Assert — the registrar learned the claude id but was NEVER told
	// terminal.
	for _, call := range reg.snapshot() {
		if strings.HasPrefix(call, "terminal ") {
			t.Fatalf("hibernation notified the registrar terminal: %v", reg.snapshot())
		}
	}
}

func TestReplayRequestDoesNotErrorOnHibernatedSession(t *testing.T) {
	// Arrange — a hibernated session (replay must stay free-to-serve).
	h := newHarness(t, 16)
	initEvent(t, h)
	hibernateNow(t, h)
	c := NewClient()
	h.sess.Attach(c)
	recvFrame(t, c) // hello
	// Act — replay-request is observation, not an act.
	err := h.sess.HandleClientFrame(c, []byte(`{"type":"replay-request","from_seq":0}`))
	// Assert — served without error and without reviving.
	if err != nil {
		t.Fatalf("replay-request on hibernated session errored: %v", err)
	}
	if !h.sess.Hibernated() {
		t.Fatal("replay-request revived the session; observation must stay free")
	}
}

func TestIdleForGrowsWithoutActivity(t *testing.T) {
	// Arrange — a fixed clock at construction time T.
	base := time.Date(2026, 5, 24, 12, 0, 0, 0, time.UTC)
	shim := newFakeShim()
	sess := New(Config{
		ID: "sess-1", DaemonVersion: "0.1.0-test", Shim: shim, Retention: 16,
		Now:  func() time.Time { return base },
		Logf: func(string, ...any) {},
	})
	go sess.Run()
	cleanupSession(t, shim, sess)
	// Act / Assert — idleness is measured from lastActive == T.
	if got := sess.IdleFor(base.Add(10 * time.Minute)); got != 10*time.Minute {
		t.Fatalf("IdleFor = %v, want 10m", got)
	}
}

func TestActivityResetsIdle(t *testing.T) {
	// Arrange — a movable clock.
	now := time.Date(2026, 5, 24, 12, 0, 0, 0, time.UTC)
	clock := func() time.Time { return now }
	shim := newFakeShim()
	sess := New(Config{
		ID: "sess-1", DaemonVersion: "0.1.0-test", Shim: shim, Retention: 16,
		Now:  clock,
		Logf: func(string, ...any) {},
	})
	go sess.Run()
	cleanupSession(t, shim, sess)
	c := NewClient()
	sess.Attach(c)
	recvFrame(t, c) // hello
	// Act — advance the clock, then a shim event counts as activity.
	// lastActive is stamped BEFORE the frame is broadcast, so once the
	// frame arrives the reset has already happened (no polling needed).
	now = now.Add(30 * time.Minute)
	shim.pushEvent(t, `{"type":"result","session_id":"sess-1","uuid":"u2","subtype":"success","duration_ms":1,"num_turns":1}`)
	waitForFrameType(t, c, "result")
	// Assert — idleness resets to the moment of the event.
	if got := sess.IdleFor(now); got != 0 {
		t.Fatalf("IdleFor after activity = %v, want 0", got)
	}
}
