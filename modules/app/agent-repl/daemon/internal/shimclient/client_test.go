package shimclient

import (
	"context"
	"errors"
	"fmt"
	"net"
	"os"
	"path/filepath"
	"strings"
	"sync"
	"testing"
	"time"

	corev1 "agentrepl/proto/agentshim/core/v1"
	"agentrepl/wire"

	"google.golang.org/protobuf/proto"
	"google.golang.org/protobuf/types/known/anypb"
)

// ---------------------------------------------------------------------------
// Test doubles for the injected sinks.
// ---------------------------------------------------------------------------

type memSeqStore struct {
	mu sync.Mutex
	m  map[string]uint64
}

func newMemSeqStore() *memSeqStore { return &memSeqStore{m: map[string]uint64{}} }

func (s *memSeqStore) LastSeq(id string) uint64 {
	s.mu.Lock()
	defer s.mu.Unlock()
	return s.m[id]
}

func (s *memSeqStore) SetLastSeq(id string, seq uint64) {
	s.mu.Lock()
	defer s.mu.Unlock()
	s.m[id] = seq
}

// chanState / chanFrame capture routed events on buffered channels so tests
// synchronize on delivery instead of sleeping.
type chanState struct{ ch chan *corev1.Event }

func newChanState() *chanState              { return &chanState{ch: make(chan *corev1.Event, 256)} }
func (s *chanState) Apply(ev *corev1.Event) { s.ch <- ev }

type chanFrame struct{ ch chan *corev1.Event }

func newChanFrame() *chanFrame                { return &chanFrame{ch: make(chan *corev1.Event, 256)} }
func (f *chanFrame) Consume(ev *corev1.Event) { f.ch <- ev }

type chanDegraded struct {
	ds        chan *corev1.DegradedState
	degraded  chan string
	recovered chan struct{}
}

func newChanDegraded() *chanDegraded {
	return &chanDegraded{
		ds:        make(chan *corev1.DegradedState, 16),
		degraded:  make(chan string, 16),
		recovered: make(chan struct{}, 16),
	}
}

func (d *chanDegraded) Degraded(_ string, ds *corev1.DegradedState) { d.ds <- ds }
func (d *chanDegraded) ConnectionDegraded(_, reason string)         { d.degraded <- reason }
func (d *chanDegraded) ConnectionRecovered(_ string)                { d.recovered <- struct{}{} }

// funcPerm adapts a func to PermissionHandler.
type funcPerm func(sessionID string, req *corev1.PermissionRequest) *corev1.PermissionResponse

func (f funcPerm) HandlePermission(id string, req *corev1.PermissionRequest) *corev1.PermissionResponse {
	return f(id, req)
}

// harness bundles the doubles and builds a Config with test-friendly tunables.
type harness struct {
	seq   *memSeqStore
	state *chanState
	frame *chanFrame
	deg   *chanDegraded
	perm  PermissionHandler
}

func newHarness() *harness {
	return &harness{
		seq:   newMemSeqStore(),
		state: newChanState(),
		frame: newChanFrame(),
		deg:   newChanDegraded(),
		perm: funcPerm(func(_ string, req *corev1.PermissionRequest) *corev1.PermissionResponse {
			return &corev1.PermissionResponse{RequestId: req.GetRequestId(), Decision: corev1.PermissionDecision_PERMISSION_DECISION_ALLOW}
		}),
	}
}

func (h *harness) config(t *testing.T, sessionID, path string) Config {
	t.Helper()
	return Config{
		SessionID:         sessionID,
		Source:            dialSource{path: path},
		DaemonVersion:     "test-daemon",
		ProtocolVersion:   "1",
		SeqStore:          h.seq,
		StateSink:         h.state,
		FrameSink:         h.frame,
		Degraded:          h.deg,
		Permissions:       h.perm,
		Logf:              func(f string, a ...any) { t.Logf("[shimclient] "+f, a...) },
		HeartbeatInterval: time.Hour, // no spurious heartbeats unless a test wants them
		HeartbeatTimeout:  time.Hour,
		AckTimeout:        2 * time.Second,
		BackoffMin:        5 * time.Millisecond,
		BackoffMax:        20 * time.Millisecond,
	}
}

// dialSource adapts the fake shim peer (which LISTENS) to the ConnSource the
// client now takes. Production is the other way round — shims dial the daemon
// and its listener reads the identifying ShimHello — so this does the same two
// steps against the fake: connect, then consume the opening hello. Everything
// downstream of the handshake is byte-for-byte what production sees.
type dialSource struct{ path string }

func (d dialSource) Next(ctx context.Context, _ string) (net.Conn, *corev1.ShimHello, error) {
	var dl net.Dialer
	conn, err := dl.DialContext(ctx, "unix", d.path)
	if err != nil {
		return nil, nil, err
	}
	payload, err := wire.ReadFrame(conn)
	if err != nil {
		conn.Close()
		return nil, nil, err
	}
	var env anypb.Any
	if err := proto.Unmarshal(payload, &env); err != nil {
		conn.Close()
		return nil, nil, err
	}
	msg, err := env.UnmarshalNew()
	if err != nil {
		conn.Close()
		return nil, nil, err
	}
	hello, ok := msg.(*corev1.ShimHello)
	if !ok {
		conn.Close()
		return nil, nil, fmt.Errorf("fake shim opened with %T, want ShimHello", msg)
	}
	return conn, hello, nil
}

// ---------------------------------------------------------------------------
// Fake shim peer: an in-test UDS listener speaking the protocol.
// ---------------------------------------------------------------------------

func startFakeShim(t *testing.T, handler func(conn net.Conn)) string {
	t.Helper()
	// A SHORT temp dir, not t.TempDir(): macOS caps a unix socket path at 104
	// bytes and t.TempDir() embeds the test name, so a descriptive name alone
	// used to fail the bind with "invalid argument".
	dir, err := os.MkdirTemp("/tmp", "sc")
	if err != nil {
		t.Fatalf("MkdirTemp: %v", err)
	}
	t.Cleanup(func() { os.RemoveAll(dir) })
	path := filepath.Join(dir, "s")
	ln, err := net.Listen("unix", path)
	if err != nil {
		t.Fatalf("listen on %s: %v", path, err)
	}
	t.Cleanup(func() { ln.Close() })
	go func() {
		for {
			conn, err := ln.Accept()
			if err != nil {
				return
			}
			go handler(conn)
		}
	}()
	return path
}

// mustWriteMsg is the FAKE SHIM's encoder, and it stays hand-rolled on
// purpose rather than calling wire.WriteAny.
//
// Every production site now shares one encoder, so a regression in it would
// move both ends of these tests together and stay invisible. Keeping the test
// peer independent means these tests interop a real client against a separately
// written implementation of the convention — the only place that cross-check
// exists, since wire's own byte-identity test is the only other one.
func mustWriteMsg(t *testing.T, conn net.Conn, msg proto.Message) {
	t.Helper()
	env, err := anypb.New(msg)
	if err != nil {
		t.Fatalf("anypb.New(%T): %v", msg, err)
	}
	b, err := proto.Marshal(env)
	if err != nil {
		t.Fatalf("marshal frame: %v", err)
	}
	if err := wire.WriteFrame(conn, b); err != nil {
		t.Fatalf("write frame: %v", err)
	}
}

// fakeServerHandshake runs the SHIM side of the whole bring-up gate — hello,
// the daemon's hello, and the ShimReady that closes it — and returns the
// DaemonHello, whose from_seq is the daemon's resume position.
//
// Acking is what makes this a usable session: nothing else releases
// AwaitReady, so a fake that skipped it would hang every caller.
func fakeServerHandshake(t *testing.T, conn net.Conn, sessionID, protoVer string, turnInFlight bool) *corev1.DaemonHello {
	t.Helper()
	mustWriteMsg(t, conn, &corev1.ShimHello{
		SessionId:       sessionID,
		Vendor:          "claude",
		ShimVersion:     "test-shim",
		ProtocolVersion: protoVer,
		TurnInFlight:    turnInFlight,
	})
	m, err := wire.ReadAny(conn)
	if err != nil {
		t.Fatalf("shim reading DaemonHello: %v", err)
	}
	dh, ok := m.(*corev1.DaemonHello)
	if !ok {
		t.Fatalf("shim expected DaemonHello, got %T", m)
	}
	mustWriteMsg(t, conn, &corev1.ShimReady{SessionId: sessionID, FromSeq: dh.GetFromSeq()})
	return dh
}

// recvEvent waits for one event on ch or fails.
func recvEvent(t *testing.T, ch chan *corev1.Event) *corev1.Event {
	t.Helper()
	select {
	case ev := <-ch:
		return ev
	case <-time.After(2 * time.Second):
		t.Fatal("timed out waiting for an event")
		return nil
	}
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

func TestHandshakeHappyPath(t *testing.T) {
	// Arrange
	h := newHarness()
	gotHello := make(chan *corev1.DaemonHello, 1)
	path := startFakeShim(t, func(conn net.Conn) {
		mustWriteMsg(t, conn, &corev1.ShimHello{
			SessionId: "sess-1", Vendor: "claude", ShimVersion: "test-shim",
			ProtocolVersion: "1", TurnInFlight: true,
		})
		m, err := wire.ReadAny(conn)
		if err != nil {
			t.Errorf("read DaemonHello: %v", err)
			return
		}
		dh, ok := m.(*corev1.DaemonHello)
		if !ok {
			t.Errorf("expected DaemonHello, got %T", m)
			return
		}
		gotHello <- dh
		mustWriteMsg(t, conn, &corev1.ShimReady{SessionId: "sess-1", FromSeq: dh.GetFromSeq()})
		// Hold the connection open.
		_, _ = wire.ReadAny(conn)
	})

	cfg := h.config(t, "sess-1", path)
	connected := make(chan *corev1.ShimHello, 1)
	cfg.OnConnected = func(hello *corev1.ShimHello) { connected <- hello }
	c := New(cfg)
	ctx, cancel := context.WithCancel(context.Background())
	errCh := make(chan error, 1)
	go func() { errCh <- c.Run(ctx) }()

	// Act
	var hello *corev1.ShimHello
	select {
	case hello = <-connected:
	case <-time.After(2 * time.Second):
		t.Fatal("never connected")
	}

	// Assert
	dh := <-gotHello
	if dh.GetProtocolVersion() != "1" || dh.GetDaemonVersion() != "test-daemon" {
		t.Fatalf("DaemonHello mismatch: %+v", dh)
	}
	if dh.GetFromSeq() != 0 {
		t.Fatalf("DaemonHello from_seq = %d, want 0 for a session never consumed", dh.GetFromSeq())
	}
	if !hello.GetTurnInFlight() {
		t.Fatal("OnConnected should carry turn_in_flight=true")
	}

	cancel()
	if err := <-errCh; err != nil {
		t.Fatalf("Run returned non-nil on cancel: %v", err)
	}
}

func TestHandshakeVersionMismatchIsTerminal(t *testing.T) {
	// Arrange: shim announces an incompatible protocol version.
	h := newHarness()
	path := startFakeShim(t, func(conn net.Conn) {
		mustWriteMsg(t, conn, &corev1.ShimHello{
			SessionId: "sess-1", Vendor: "claude", ShimVersion: "test-shim",
			ProtocolVersion: "99",
		})
		_, _ = wire.ReadAny(conn)
	})
	cfg := h.config(t, "sess-1", path)
	c := New(cfg)

	// Act: Run returns quickly because a version mismatch is not retryable.
	ctx, cancel := context.WithTimeout(context.Background(), 2*time.Second)
	defer cancel()
	err := c.Run(ctx)

	// Assert
	if !errors.Is(err, ErrVersionMismatch) {
		t.Fatalf("want ErrVersionMismatch, got %v", err)
	}
}

func TestReconnectAndResumeMidStream(t *testing.T) {
	// Arrange: first connection serves seq 1..3 then drops; the daemon must
	// reconnect to the LIVE shim and resume Subscribe from_seq=3.
	h := newHarness()
	var attempt int
	var mu sync.Mutex
	secondSubFrom := make(chan uint64, 1)
	path := startFakeShim(t, func(conn net.Conn) {
		mu.Lock()
		attempt++
		n := attempt
		mu.Unlock()
		dh := fakeServerHandshake(t, conn, "sess-1", "1", false)
		if n == 1 {
			for seq := uint64(1); seq <= 3; seq++ {
				mustWriteMsg(t, conn, persistentTurnEnd("sess-1", seq))
			}
			// Drop the connection; the unix stream delivers the three frames
			// before EOF, so the daemon consumes them and then reconnects.
			conn.Close()
			return
		}
		// Second connection: report the resumed from_seq and serve 4..5.
		secondSubFrom <- dh.GetFromSeq()
		for seq := uint64(4); seq <= 5; seq++ {
			mustWriteMsg(t, conn, persistentTurnEnd("sess-1", seq))
		}
		_, _ = wire.ReadAny(conn)
	})

	c := New(h.config(t, "sess-1", path))
	ctx, cancel := context.WithCancel(context.Background())
	errCh := make(chan error, 1)
	go func() { errCh <- c.Run(ctx) }()

	// Act / Assert: five TurnEnded events arrive in order across the reconnect.
	for want := uint64(1); want <= 3; want++ {
		if got := recvEvent(t, h.state.ch).GetSeq(); got != want {
			t.Fatalf("pre-drop seq: got %d want %d", got, want)
		}
	}
	select {
	case from := <-secondSubFrom:
		if from != 3 {
			t.Fatalf("resume from_seq: got %d want 3", from)
		}
	case <-time.After(2 * time.Second):
		t.Fatal("never reconnected")
	}
	for want := uint64(4); want <= 5; want++ {
		if got := recvEvent(t, h.state.ch).GetSeq(); got != want {
			t.Fatalf("post-reconnect seq: got %d want %d", got, want)
		}
	}
	if last := h.seq.LastSeq("sess-1"); last != 5 {
		t.Fatalf("seq store: got %d want 5", last)
	}

	cancel()
	if err := <-errCh; err != nil {
		t.Fatalf("Run returned non-nil: %v", err)
	}
}

func TestHeartbeatMissSurfacesDegraded(t *testing.T) {
	// Arrange: shim completes the handshake then goes silent (no heartbeats).
	h := newHarness()
	path := startFakeShim(t, func(conn net.Conn) {
		_ = fakeServerHandshake(t, conn, "sess-1", "1", false)
		_, _ = wire.ReadAny(conn) // block; never send anything else
	})
	cfg := h.config(t, "sess-1", path)
	cfg.HeartbeatInterval = time.Hour
	cfg.HeartbeatTimeout = 40 * time.Millisecond
	c := New(cfg)
	ctx, cancel := context.WithCancel(context.Background())
	errCh := make(chan error, 1)
	go func() { errCh <- c.Run(ctx) }()

	// Act / Assert: the missed-heartbeat window opens a degraded report.
	select {
	case reason := <-h.deg.degraded:
		if reason == "" {
			t.Fatal("degraded reason should be non-empty")
		}
	case <-time.After(2 * time.Second):
		t.Fatal("never surfaced connection-degraded")
	}

	cancel()
	if err := <-errCh; err != nil {
		t.Fatalf("Run returned non-nil: %v", err)
	}
}

// persistentTurnEnd builds a PERSISTENT TurnEnded event at seq.
func persistentTurnEnd(session string, seq uint64) *corev1.Event {
	return &corev1.Event{
		SessionId: session,
		Seq:       seq,
		Plane:     corev1.Plane_PLANE_STREAM,
		Class:     corev1.EventClass_EVENT_CLASS_PERSISTENT,
		Payload:   &corev1.Event_TurnEnded{TurnEnded: &corev1.TurnEnded{StopReason: "end_turn"}},
	}
}

// --- readiness latch ---------------------------------------------------------
//
// Bring-up is asynchronous: the daemon spawns the shim and starts connecting in
// a goroutine, so for a few hundred milliseconds there is no connection and
// every control send fails with ErrNotConnected. AwaitReady lets a caller wait
// for the connection EVENT rather than guess a duration.

func TestAwaitReadyBlocksUntilConnected(t *testing.T) {
	// Arrange: a client that has never connected.
	c := New(Config{SessionID: "s1"})

	// Act: waiting must not return while there is no connection.
	done := make(chan error, 1)
	go func() { done <- c.AwaitReady(context.Background()) }()
	select {
	case err := <-done:
		t.Fatalf("AwaitReady returned %v before any connection existed", err)
	case <-time.After(20 * time.Millisecond):
	}

	// Act: publish a connection AND the shim's ack, exactly as the attach path
	// and the gate's closing frame do.
	c.mu.Lock()
	c.active = &activeConn{}
	c.wired = true
	c.markReadyLocked()
	c.mu.Unlock()

	// Assert
	select {
	case err := <-done:
		if err != nil {
			t.Fatalf("AwaitReady: %v", err)
		}
	case <-time.After(2 * time.Second):
		t.Fatal("AwaitReady did not return after the connection was published")
	}
}

func TestAwaitReadyReturnsImmediatelyWhenAlreadyConnected(t *testing.T) {
	// Arrange
	c := New(Config{SessionID: "s1"})
	c.mu.Lock()
	c.active = &activeConn{}
	c.wired = true
	c.markReadyLocked()
	c.mu.Unlock()

	// Act / Assert: an already-usable connection must not make callers wait.
	ctx, cancel := context.WithTimeout(context.Background(), time.Second)
	defer cancel()
	if err := c.AwaitReady(ctx); err != nil {
		t.Fatalf("AwaitReady: %v", err)
	}
}

func TestAwaitReadyBlocksAgainAfterDisconnect(t *testing.T) {
	// Arrange: connect, then drop — the reconnect window a workspace already
	// in byWS can sit in, where a send would otherwise sail through on a latch
	// left closed by the dead connection.
	c := New(Config{SessionID: "s1"})
	ac := &activeConn{}
	c.mu.Lock()
	c.active = ac
	c.wired = true
	c.markReadyLocked()
	c.mu.Unlock()

	c.mu.Lock()
	c.active = nil
	c.wired = false
	c.markNotReadyLocked()
	c.mu.Unlock()

	// Act / Assert: waiting must block again, not return stale readiness.
	ctx, cancel := context.WithTimeout(context.Background(), 30*time.Millisecond)
	defer cancel()
	if err := c.AwaitReady(ctx); err == nil {
		t.Fatal("AwaitReady returned ready while disconnected")
	}
}

func TestAwaitReadyFailsOnContextExpiry(t *testing.T) {
	// Arrange: a shim that never comes up.
	c := New(Config{SessionID: "s1"})

	// Act
	ctx, cancel := context.WithTimeout(context.Background(), 20*time.Millisecond)
	defer cancel()
	err := c.AwaitReady(ctx)

	// Assert: the bound is a FAILURE bound, surfaced loudly and naming the session.
	if err == nil {
		t.Fatal("AwaitReady must fail when the shim never connects")
	}
	if !strings.Contains(err.Error(), "s1") {
		t.Fatalf("err = %v, want it to name the session", err)
	}
}
