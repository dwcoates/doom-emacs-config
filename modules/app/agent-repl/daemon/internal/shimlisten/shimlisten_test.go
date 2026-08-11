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
	if connected(t, s, "s_abc") {
		t.Fatal("Connected = true before any shim dialed in")
	}

	// Act
	dialAsShim(t, path, "s_abc")

	// Assert
	waitConnected(t, s, "s_abc")
}

func TestClaimingRemovesTheParkedConnection(t *testing.T) {
	// Arrange: a claimed connection is owned by a session controller, so it must
	// not also sit in the parked index waiting to be handed to a second claimer.
	s, path := serve(t)
	dialAsShim(t, path, "s_abc")
	waitConnected(t, s, "s_abc")

	// Act
	if _, err := s.Next(context.Background(), "s_abc"); err != nil {
		t.Fatalf("Next: %v", err)
	}

	// Assert
	s.mu.Lock()
	_, stillParked := s.parked["s_abc"]
	s.mu.Unlock()
	if stillParked {
		t.Fatal("the parked index still holds a connection a claimer owns")
	}
}

// TestAClaimedShimIsStillReportedConnected is THE ADOPTION SEAM. Connected is
// what the workspace-ownership gate reads to tell a survivor it should adopt
// from a squatter it should kill, and a shim whose connection is claimed by an
// EARLIER controller generation is a survivor: it dialled in, it is talking to
// this daemon, and the kernel says its socket is open. Answering "no shim is
// connected" about it is what got a ready shim SIGTERM'd.
func TestAClaimedShimIsStillReportedConnected(t *testing.T) {
	// Arrange
	s, path := serve(t)
	dialAsShim(t, path, "s_abc")
	waitConnected(t, s, "s_abc")

	// Act — a controller claims the connection and keeps it.
	if _, err := s.Next(context.Background(), "s_abc"); err != nil {
		t.Fatalf("Next: %v", err)
	}

	// Assert
	if !connected(t, s, "s_abc") {
		t.Fatal("Connected = false for a session whose live shim is claimed; a survivor under a retired generation must still satisfy the adoption gate")
	}
}

// TestAClaimedShimWhoseProcessDiedIsNotReportedConnected: the claim record must
// never outlive the transport it describes. A corpse answering Connected would
// adopt a shim that cannot drive anything.
func TestAClaimedShimWhoseProcessDiedIsNotReportedConnected(t *testing.T) {
	// Arrange
	s, path := serve(t)
	shim := dialAsShim(t, path, "s_abc")
	waitConnected(t, s, "s_abc")
	if _, err := s.Next(context.Background(), "s_abc"); err != nil {
		t.Fatalf("Next: %v", err)
	}

	// Act — the shim process goes away, which closes its socket. The kernel
	// records that at the instant of the close, so the probe below needs no
	// wait of any kind.
	if err := shim.Close(); err != nil {
		t.Fatalf("closing the shim peer: %v", err)
	}

	// Assert
	if connected(t, s, "s_abc") {
		t.Fatal("Connected = true for a claimed connection whose peer is gone")
	}
}

// TestAForeignSessionsClaimedShimIsNotReportedConnected: adoption is decided
// per session, and a claimed connection announcing some other session must not
// answer for this one.
func TestAForeignSessionsClaimedShimIsNotReportedConnected(t *testing.T) {
	// Arrange
	s, path := serve(t)
	dialAsShim(t, path, "s_foreign")
	waitConnected(t, s, "s_foreign")
	if _, err := s.Next(context.Background(), "s_foreign"); err != nil {
		t.Fatalf("Next: %v", err)
	}

	// Act/Assert
	if connected(t, s, "s_abc") {
		t.Fatal("Connected = true for s_abc while only a foreign session's shim is claimed")
	}
}

// TestARedialSupersedesTheClaimRecord: one session has one connection here. A
// shim that redialled has abandoned the socket its old claim names, so the
// claim must not go on speaking for it.
func TestARedialSupersedesTheClaimRecord(t *testing.T) {
	// Arrange — a claimed connection.
	s, path := serve(t)
	dialAsShim(t, path, "s_abc")
	waitConnected(t, s, "s_abc")
	claimed, err := s.Next(context.Background(), "s_abc")
	if err != nil {
		t.Fatalf("Next: %v", err)
	}

	// Act — the same shim redials, and the next claimer takes it. Next is the
	// rendezvous the assertion synchronizes on: it returns only once the
	// redialled connection has been delivered.
	dialAsShim(t, path, "s_abc")
	ctx, cancel := context.WithTimeout(context.Background(), 2*time.Second)
	defer cancel()
	redialled, err := s.Next(ctx, "s_abc")
	if err != nil {
		t.Fatalf("Next after the redial: %v", err)
	}

	// Assert — the claim record names the newest connection, never the one it
	// superseded.
	s.mu.Lock()
	current := s.claimed["s_abc"]
	s.mu.Unlock()
	if current == claimed {
		t.Fatal("the superseded claim record survived the redial that replaced it")
	}
	if current != redialled {
		t.Fatalf("claim record = %p, want the redialled connection %p", current, redialled)
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
	if !connected(t, s, "s_abc") {
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
	if connected(t, s, "s_abc") || connected(t, s, "") {
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
	for !connected(t, s, sessionID) {
		if time.Now().After(deadline) {
			t.Fatalf("session %s never registered as connected", sessionID)
		}
		time.Sleep(5 * time.Millisecond)
	}
}

func connected(t *testing.T, s *Server, sessionID string) bool {
	t.Helper()
	connected, err := s.Connected(sessionID)
	if err != nil {
		t.Fatalf("Connected(%s): %v", sessionID, err)
	}
	return connected
}

func TestDisconnectedParkedShimIsEvictedBeforeItIsAdvertised(t *testing.T) {
	// Arrange — the hibernate race parked a reconnect just before the shim
	// process exited, leaving exactly this closed peer in the map.
	s, path := serve(t)
	peer := dialAsShim(t, path, "s_dead")
	waitConnected(t, s, "s_dead")
	if err := peer.Close(); err != nil {
		t.Fatal(err)
	}

	// Act / Assert — the kernel closure proof is synchronous and consumes no
	// protocol data. The corpse is removed before Connected answers.
	if connected(t, s, "s_dead") {
		t.Fatal("a disconnected parked shim was still advertised as connected")
	}
	s.mu.Lock()
	_, retained := s.parked["s_dead"]
	s.mu.Unlock()
	if retained {
		t.Fatal("a disconnected parked shim remained in the registry")
	}
}

// THE LISTENER ITSELF evicts a parked connection whose socket dies, with
// nobody probing it. That is what makes a dead parked transport
// unrepresentable rather than merely unlikely: the kernel closes the socket at
// process death, so the eviction happens at the death, not at the next question
// somebody thinks to ask about it.
func TestAParkedConnectionIsEvictedWhenItsSocketDies(t *testing.T) {
	// Arrange — parked and watched.
	s, path := serve(t)
	peer := dialAsShim(t, path, "s_watched")
	waitConnected(t, s, "s_watched")
	s.mu.Lock()
	watched := s.parked["s_watched"]
	s.mu.Unlock()
	if watched == nil || watched.watchDone == nil {
		t.Fatal("the parked connection is not watched")
	}

	// Act — the peer dies. Nothing probes; the rendezvous is the watch's own
	// exit.
	if err := peer.Close(); err != nil {
		t.Fatal(err)
	}
	<-watched.watchDone

	// Assert — evicted by the listener, and no longer advertised.
	s.mu.Lock()
	_, retained := s.parked["s_watched"]
	s.mu.Unlock()
	if retained {
		t.Fatal("a parked connection whose socket died was left in the registry")
	}
	if connected(t, s, "s_watched") {
		t.Fatal("a parked connection whose socket died was still advertised as connected")
	}
}

func TestExplicitStopEvictsOnlyTheNamedParkedSession(t *testing.T) {
	// Arrange.
	s, path := serve(t)
	dialAsShim(t, path, "s_stop")
	dialAsShim(t, path, "s_keep")
	waitConnected(t, s, "s_stop")
	waitConnected(t, s, "s_keep")

	// Act.
	if !s.Evict("s_stop", "explicit_stop_completed") {
		t.Fatal("the named parked session was not evicted")
	}

	// Assert.
	if connected(t, s, "s_stop") {
		t.Fatal("the stopped session remained advertised")
	}
	if !connected(t, s, "s_keep") {
		t.Fatal("evicting one session disturbed another parked connection")
	}
}

func TestConnectedSurfacesAnUnprobeableConnectionWithoutEvictingIt(t *testing.T) {
	// Arrange — net.Pipe deliberately lacks syscall.Conn, exercising the loud
	// boundary when kernel socket state cannot be inspected.
	s := New(func(string, ...any) {})
	server, peer := net.Pipe()
	t.Cleanup(func() { _ = server.Close(); _ = peer.Close() })
	s.parked["s_unknown"] = &Conn{Net: server, Hello: &corev1.ShimHello{SessionId: "s_unknown"}}

	// Act.
	connected, err := s.Connected("s_unknown")

	// Assert — unknown is never rewritten as disconnected.
	if err == nil || connected {
		t.Fatalf("Connected = %v, %v, want false plus a probe error", connected, err)
	}
	s.mu.Lock()
	_, retained := s.parked["s_unknown"]
	s.mu.Unlock()
	if !retained {
		t.Fatal("an unprobeable connection was silently evicted as dead")
	}
}
