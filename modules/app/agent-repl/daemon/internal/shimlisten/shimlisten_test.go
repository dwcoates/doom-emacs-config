package shimlisten

import (
	"context"
	"net"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	corev1 "agentrepl/proto/agentshim/core/v1"
	"agentrepl/wire"

	"google.golang.org/protobuf/proto"
	"google.golang.org/protobuf/types/known/anypb"
)

// serve starts a Server on a temp socket.
//
// The directory is made under /tmp rather than via t.TempDir(): macOS caps a
// unix socket path at 104 bytes, and t.TempDir() embeds the test name in an
// already-long $TMPDIR, which overruns it as `bind: invalid argument`.
func serve(t *testing.T) (*Server, string) {
	t.Helper()
	dir, err := os.MkdirTemp("/tmp", "sl")
	if err != nil {
		t.Fatalf("mkdtemp: %v", err)
	}
	t.Cleanup(func() { _ = os.RemoveAll(dir) })
	path := filepath.Join(dir, "d.sock")
	s := New(func(string, ...any) {})
	if err := s.Listen(path); err != nil {
		t.Fatalf("Listen: %v", err)
	}
	t.Cleanup(func() { _ = s.Close() })
	return s, path
}

// dialAsShim connects and announces itself the way a shim does: a ShimHello as
// the first frame, carrying the session it is claiming.
func dialAsShim(t *testing.T, path, sessionID string) net.Conn {
	t.Helper()
	conn, err := net.Dial("unix", path)
	if err != nil {
		t.Fatalf("dial: %v", err)
	}
	t.Cleanup(func() { conn.Close() })
	hello := &corev1.ShimHello{SessionId: sessionID, Vendor: "claude", ShimVersion: "test", ProtocolVersion: "1"}
	env, err := anypb.New(hello)
	if err != nil {
		t.Fatalf("anypb: %v", err)
	}
	payload, err := proto.Marshal(env)
	if err != nil {
		t.Fatalf("marshal: %v", err)
	}
	if err := wire.WriteFrame(conn, payload); err != nil {
		t.Fatalf("write hello: %v", err)
	}
	return conn
}

func TestNextReturnsAConnectionThatDialsInLater(t *testing.T) {
	// Arrange: a claimer waiting before the shim exists — the ordinary spawn
	// case, where the daemon starts the shim and then waits for it.
	s, path := serve(t)
	got := make(chan *Conn, 1)
	go func() {
		c, err := s.Next(context.Background(), "s_abc")
		if err != nil {
			t.Errorf("Next: %v", err)
			return
		}
		got <- c
	}()

	// Act
	dialAsShim(t, path, "s_abc")

	// Assert
	select {
	case c := <-got:
		if c.Hello.GetSessionId() != "s_abc" {
			t.Fatalf("hello session = %q, want s_abc", c.Hello.GetSessionId())
		}
	case <-time.After(2 * time.Second):
		t.Fatal("Next never received the connection")
	}
}

func TestNextReturnsAParkedConnectionImmediately(t *testing.T) {
	// Arrange: the daemon-restart case — a survivor dials in as soon as the
	// socket exists, long before anything claims it.
	s, path := serve(t)
	dialAsShim(t, path, "s_abc")
	waitConnected(t, s, "s_abc")

	// Act
	ctx, cancel := context.WithTimeout(context.Background(), time.Second)
	defer cancel()
	c, err := s.Next(ctx, "s_abc")

	// Assert: claimed without waiting for anything further.
	if err != nil {
		t.Fatalf("Next: %v", err)
	}
	if c.Hello.GetSessionId() != "s_abc" {
		t.Fatalf("hello session = %q, want s_abc", c.Hello.GetSessionId())
	}
}

func TestConnectedReportsAParkedShim(t *testing.T) {
	// Arrange: this is what replaces ReattachDecision's dial-and-handshake.
	s, path := serve(t)
	if s.Connected("s_abc") {
		t.Fatal("Connected = true before any shim dialed in")
	}

	// Act
	dialAsShim(t, path, "s_abc")

	// Assert
	waitConnected(t, s, "s_abc")
}

func TestClaimingRemovesTheParkedConnection(t *testing.T) {
	// Arrange: a claimed connection is being driven, so it must not also look
	// parked — Connected answers "is one waiting to be claimed".
	s, path := serve(t)
	dialAsShim(t, path, "s_abc")
	waitConnected(t, s, "s_abc")

	// Act
	if _, err := s.Next(context.Background(), "s_abc"); err != nil {
		t.Fatalf("Next: %v", err)
	}

	// Assert
	if s.Connected("s_abc") {
		t.Fatal("Connected = true after the connection was claimed")
	}
}

func TestReconnectSupersedesTheParkedConnection(t *testing.T) {
	// Arrange: a shim that dials twice (its reconnect loop) must not leave the
	// daemon holding a corpse that answers Connected.
	s, path := serve(t)
	first := dialAsShim(t, path, "s_abc")
	waitConnected(t, s, "s_abc")

	// Act
	dialAsShim(t, path, "s_abc")

	// Assert: the older connection is dropped by the server.
	deadline := time.Now().Add(2 * time.Second)
	for {
		_ = first.SetReadDeadline(time.Now().Add(50 * time.Millisecond))
		if _, err := wire.ReadFrame(first); err != nil {
			break // closed (or at least no longer readable), as intended
		}
		if time.Now().After(deadline) {
			t.Fatal("the superseded connection was left open")
		}
	}
	if !s.Connected("s_abc") {
		t.Fatal("Connected = false after the reconnect")
	}
}

func TestConnectionWithoutAHelloIsRejected(t *testing.T) {
	// Arrange: something that is not a shim. Filing it under a guessed session
	// would bind a connection to the wrong conversation.
	s, path := serve(t)
	conn, err := net.Dial("unix", path)
	if err != nil {
		t.Fatalf("dial: %v", err)
	}
	defer conn.Close()

	// Act: a well-formed frame carrying the wrong message type.
	env, err := anypb.New(&corev1.DaemonHello{DaemonVersion: "d1"})
	if err != nil {
		t.Fatalf("anypb: %v", err)
	}
	payload, err := proto.Marshal(env)
	if err != nil {
		t.Fatalf("marshal: %v", err)
	}
	if err := wire.WriteFrame(conn, payload); err != nil {
		t.Fatalf("write: %v", err)
	}

	// Assert: never parked under any session.
	time.Sleep(100 * time.Millisecond)
	if s.Connected("s_abc") || s.Connected("") {
		t.Fatal("a non-ShimHello connection was accepted as a shim")
	}
}

func TestNextFailsWhenTheShimNeverConnects(t *testing.T) {
	// Arrange
	s, _ := serve(t)

	// Act
	ctx, cancel := context.WithTimeout(context.Background(), 50*time.Millisecond)
	defer cancel()
	_, err := s.Next(ctx, "s_ghost")

	// Assert: surfaced loudly, naming the session.
	if err == nil {
		t.Fatal("Next must fail when no shim connects")
	}
	if !strings.Contains(err.Error(), "s_ghost") {
		t.Fatalf("err = %v, want it to name the session", err)
	}
}

func waitConnected(t *testing.T, s *Server, sessionID string) {
	t.Helper()
	deadline := time.Now().Add(2 * time.Second)
	for !s.Connected(sessionID) {
		if time.Now().After(deadline) {
			t.Fatalf("session %s never registered as connected", sessionID)
		}
		time.Sleep(5 * time.Millisecond)
	}
}
