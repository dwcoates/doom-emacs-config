package harness

import (
	"context"
	"fmt"
	"net"
	"path/filepath"
	"sync"
	"testing"

	"agent-repld/integration/grantedclock"
	"agent-repld/internal/workspace"
)

// WatcherStartHold is a test's hold on a daemon's FIRST session watcher start
// (workspace.EnvTestHoldFirstWatcherStart): the daemon dials it after marking
// the session started and before opening its watcher, and waits there until
// the test releases it. It pins open a window that is otherwise microseconds
// wide, so a test lands a call inside it on every run, with nothing timed.
type WatcherStartHold struct {
	t       testing.TB
	socket  string
	ln      net.Listener
	conn    net.Conn
	release sync.Once
}

// HoldFirstWatcherStart listens for a daemon's watcher-start hold. Hand Env()
// to the daemon's ExtraEnv. The hold is released at cleanup if the test has
// not released it, so a failed test never leaves its daemon parked.
func HoldFirstWatcherStart(t testing.TB) *WatcherStartHold {
	t.Helper()
	socket := filepath.Join(ShortTempDir(t), "hold.sock")
	ln, err := net.Listen("unix", socket)
	if err != nil {
		t.Fatalf("harness: listen for the watcher-start hold at %s: %v", socket, err)
	}
	h := &WatcherStartHold{t: t, socket: socket, ln: ln}
	t.Cleanup(func() {
		h.Release()
		_ = ln.Close()
	})
	return h
}

// Env is the daemon environment entry that names this hold.
func (h *WatcherStartHold) Env() string {
	return workspace.EnvTestHoldFirstWatcherStart + "=" + h.socket
}

// AwaitHeld waits, within DefaultTimeout granted of CTX, for the daemon to reach the
// hold: once it answers, the session is marked started and its watcher is not
// open, and it stays that way until Release.
func (h *WatcherStartHold) AwaitHeld(ctx context.Context) {
	h.t.Helper()
	ctx, cancel := grantedclock.WithBound(ctx, DefaultTimeout)
	defer cancel()
	conn, err := h.accept(ctx)
	if err != nil {
		h.t.Fatalf("harness: %v", err)
	}
	h.conn = conn
}

// accept takes the daemon's connection to the hold, or fails once CTX ends.
// An ended wait closes the listener, so the hold cannot be reached after it.
func (h *WatcherStartHold) accept(ctx context.Context) (net.Conn, error) {
	stop := context.AfterFunc(ctx, func() { _ = h.ln.Close() })
	conn, err := h.ln.Accept()
	if !stop() {
		if conn != nil {
			_ = conn.Close()
		}
		return nil, fmt.Errorf("no daemon reached the watcher-start hold at %s: %w", h.socket, grantedclock.Explain(ctx))
	}
	if err != nil {
		return nil, fmt.Errorf("accept the watcher-start hold at %s: %w", h.socket, err)
	}
	return conn, nil
}

// Release lets the held watcher start go ahead. It is idempotent.
func (h *WatcherStartHold) Release() {
	h.release.Do(func() {
		if h.conn != nil {
			_ = h.conn.Close()
		}
	})
}
