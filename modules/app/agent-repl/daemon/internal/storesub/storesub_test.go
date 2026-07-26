package storesub

import (
	"context"
	"errors"
	"net"
	"os"
	"path/filepath"
	"sync"
	"testing"
	"time"

	corev1 "agentrepl/proto/agentshim/core/v1"
	"agentrepl/wire"
)

// fakeStore is a one-connection stand-in for shim-store's subscriber side: it
// reads the Subscribe frame, records it, then writes the scripted frames.
type fakeStore struct {
	mu        sync.Mutex
	subscribe *corev1.Subscribe

	// events are written back in order after the Subscribe lands.
	events []*corev1.Event
	// extra, when non-nil, is written INSTEAD of events (a protocol-drift frame).
	extra func(conn net.Conn)
	// hold, when true, keeps the connection open and silent after the scripted
	// frames, which is what a drained replay in live-tail looks like.
	hold bool

	done chan struct{}
}

// shortSocketDir returns a SHORT temp dir: macOS caps a unix socket path at
// 104 bytes and t.TempDir() embeds the test name, which overruns it.
func shortSocketDir(t *testing.T) string {
	t.Helper()
	dir, err := os.MkdirTemp("/tmp", "ss")
	if err != nil {
		t.Fatalf("MkdirTemp: %v", err)
	}
	t.Cleanup(func() { os.RemoveAll(dir) })
	return dir
}

func startFakeStore(t *testing.T, fs *fakeStore) string {
	t.Helper()
	path := filepath.Join(shortSocketDir(t), "store.sock")
	ln, err := net.Listen("unix", path)
	if err != nil {
		t.Fatalf("listen: %v", err)
	}
	fs.done = make(chan struct{})
	t.Cleanup(func() {
		ln.Close()
		<-fs.done
	})
	go func() {
		defer close(fs.done)
		conn, aerr := ln.Accept()
		if aerr != nil {
			return
		}
		defer conn.Close()
		msg, rerr := wire.ReadAny(conn)
		if rerr != nil {
			return
		}
		sub, ok := msg.(*corev1.Subscribe)
		if !ok {
			return
		}
		fs.mu.Lock()
		fs.subscribe = sub
		fs.mu.Unlock()
		if fs.extra != nil {
			fs.extra(conn)
			return
		}
		for _, ev := range fs.events {
			if werr := wire.WriteAny(conn, ev); werr != nil {
				return
			}
		}
		if fs.hold {
			// Park until the client goes away, so the reader sees "no more
			// frames" rather than a close.
			buf := make([]byte, 1)
			_, _ = conn.Read(buf)
		}
	}()
	return path
}

func (fs *fakeStore) sentSubscribe() *corev1.Subscribe {
	fs.mu.Lock()
	defer fs.mu.Unlock()
	return fs.subscribe
}

func newTestClient(t *testing.T, socket string, tweak func(*Config)) *Client {
	t.Helper()
	cfg := Config{SocketPath: socket, IdleTimeout: 250 * time.Millisecond}
	if tweak != nil {
		tweak(&cfg)
	}
	c, err := New(cfg)
	if err != nil {
		t.Fatalf("New: %v", err)
	}
	return c
}

func vendorEvent(seq uint64) *corev1.Event {
	return &corev1.Event{SessionId: "vendor-uuid", Seq: seq}
}

func TestNewRejectsAnEmptySocketPath(t *testing.T) {
	// Arrange + Act
	_, err := New(Config{})
	// Assert
	if err == nil {
		t.Fatal("New with no socket path must error")
	}
}

func TestReplaySubscribesUnderTheVendorSessionID(t *testing.T) {
	// Arrange
	fs := &fakeStore{events: []*corev1.Event{vendorEvent(1), vendorEvent(9)}}
	c := newTestClient(t, startFakeStore(t, fs), nil)
	// Act
	if _, err := c.Replay(context.Background(), "vendor-uuid", 0, 9, func(*corev1.Event) {}); err != nil {
		t.Fatalf("Replay: %v", err)
	}
	// Assert
	if got := fs.sentSubscribe().GetSessionId(); got != "vendor-uuid" {
		t.Fatalf("Subscribe session_id = %q, want %q", got, "vendor-uuid")
	}
}

func TestReplaySubscribesFromTheRequestedSeq(t *testing.T) {
	// Arrange
	fs := &fakeStore{events: []*corev1.Event{vendorEvent(101)}}
	c := newTestClient(t, startFakeStore(t, fs), nil)
	// Act
	if _, err := c.Replay(context.Background(), "vendor-uuid", 100, 101, func(*corev1.Event) {}); err != nil {
		t.Fatalf("Replay: %v", err)
	}
	// Assert
	if got := fs.sentSubscribe().GetFromSeq(); got != 100 {
		t.Fatalf("Subscribe from_seq = %d, want 100", got)
	}
}

func TestReplayStopsAtTheCallersRetainedWindow(t *testing.T) {
	// Arrange — seq 5 is the caller's floor, so only 1..4 belong to the gap.
	fs := &fakeStore{events: []*corev1.Event{vendorEvent(1), vendorEvent(2), vendorEvent(5), vendorEvent(6)}}
	c := newTestClient(t, startFakeStore(t, fs), nil)
	var got []uint64
	// Act
	n, err := c.Replay(context.Background(), "vendor-uuid", 0, 5, func(ev *corev1.Event) {
		got = append(got, ev.GetSeq())
	})
	// Assert
	if err != nil {
		t.Fatalf("Replay: %v", err)
	}
	if n != 2 || len(got) != 2 || got[0] != 1 || got[1] != 2 {
		t.Fatalf("delivered %d %v, want 2 [1 2]", n, got)
	}
}

func TestReplayTruncatesAtTheEventCap(t *testing.T) {
	// Arrange
	fs := &fakeStore{events: []*corev1.Event{vendorEvent(1), vendorEvent(2), vendorEvent(3)}, hold: true}
	c := newTestClient(t, startFakeStore(t, fs), func(cfg *Config) { cfg.MaxEvents = 2 })
	// Act
	n, err := c.Replay(context.Background(), "vendor-uuid", 0, 99, func(*corev1.Event) {})
	// Assert
	if !errors.Is(err, ErrTruncated) {
		t.Fatalf("err = %v, want ErrTruncated", err)
	}
	if n != 2 {
		t.Fatalf("delivered %d, want 2 (the cap)", n)
	}
}

func TestReplayTruncatesWhenTheReplayGoesIdleBeforeTheFloor(t *testing.T) {
	// Arrange — the store never reaches seq 99, so the gap is never closed.
	fs := &fakeStore{events: []*corev1.Event{vendorEvent(1)}, hold: true}
	c := newTestClient(t, startFakeStore(t, fs), nil)
	// Act
	_, err := c.Replay(context.Background(), "vendor-uuid", 0, 99, func(*corev1.Event) {})
	// Assert
	if !errors.Is(err, ErrTruncated) {
		t.Fatalf("err = %v, want ErrTruncated", err)
	}
}

func TestReplayWithNoFloorEndsCleanlyOnADrainedReplay(t *testing.T) {
	// Arrange — stopAtSeq 0 means "pull until the replay drains".
	fs := &fakeStore{events: []*corev1.Event{vendorEvent(1), vendorEvent(2)}, hold: true}
	c := newTestClient(t, startFakeStore(t, fs), nil)
	// Act
	n, err := c.Replay(context.Background(), "vendor-uuid", 0, 0, func(*corev1.Event) {})
	// Assert
	if err != nil {
		t.Fatalf("Replay: %v", err)
	}
	if n != 2 {
		t.Fatalf("delivered %d, want 2", n)
	}
}

func TestReplayRejectsAnEmptyVendorSessionID(t *testing.T) {
	// Arrange
	c := newTestClient(t, startFakeStore(t, &fakeStore{}), nil)
	// Act
	_, err := c.Replay(context.Background(), "", 0, 1, func(*corev1.Event) {})
	// Assert
	if err == nil {
		t.Fatal("Replay under an empty vendor session id must error")
	}
}

func TestReplayRejectsANonEventFrame(t *testing.T) {
	// Arrange — the store sends nothing but Events on a subscriber connection.
	fs := &fakeStore{extra: func(conn net.Conn) {
		_ = wire.WriteAny(conn, &corev1.Heartbeat{SentAtMs: 1})
	}}
	c := newTestClient(t, startFakeStore(t, fs), nil)
	// Act
	_, err := c.Replay(context.Background(), "vendor-uuid", 0, 9, func(*corev1.Event) {})
	// Assert
	if err == nil {
		t.Fatal("a non-Event replay frame must error")
	}
}

func TestReplayReportsADialFailure(t *testing.T) {
	// Arrange
	c := newTestClient(t, filepath.Join(shortSocketDir(t), "absent.sock"), nil)
	// Act
	_, err := c.Replay(context.Background(), "vendor-uuid", 0, 9, func(*corev1.Event) {})
	// Assert
	if err == nil {
		t.Fatal("dialing an absent store socket must error")
	}
}

func TestReplayStopsOnACancelledContext(t *testing.T) {
	// Arrange
	fs := &fakeStore{events: []*corev1.Event{vendorEvent(1)}, hold: true}
	c := newTestClient(t, startFakeStore(t, fs), func(cfg *Config) { cfg.IdleTimeout = time.Minute })
	ctx, cancel := context.WithCancel(context.Background())
	// Act
	go func() {
		time.Sleep(50 * time.Millisecond)
		cancel()
	}()
	_, err := c.Replay(ctx, "vendor-uuid", 0, 99, func(*corev1.Event) {})
	// Assert
	if err == nil {
		t.Fatal("a cancelled re-pull must report why it stopped")
	}
}

func TestDefaultSocketPathHonorsXDGCacheHome(t *testing.T) {
	// Arrange
	t.Setenv("XDG_CACHE_HOME", "/tmp/xdg")
	// Act
	got, err := DefaultSocketPath()
	// Assert
	if err != nil {
		t.Fatalf("DefaultSocketPath: %v", err)
	}
	if want := "/tmp/xdg/agent-repl/sock/store.sock"; got != want {
		t.Fatalf("DefaultSocketPath() = %q, want %q", got, want)
	}
}
