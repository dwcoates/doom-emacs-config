package shimclient

import (
	"context"
	"net"
	"sync/atomic"
	"testing"
	"time"

	corev1 "agentrepl/proto/agentshim/core/v1"
	"agentrepl/wire"
)

// THE LINK-LOSS EDGE (Config.OnLinkLost).
//
// OnConnected reports the bring-up gate CLOSING and had no counterpart for it
// re-opening, so a session whose link died and reconnected went right on
// claiming to be fully wired. These pin the three answers that matter: a
// genuine loss fires, a teardown does not, and a connection that never earned
// the wiring does not retract one.

// TestALostLinkFiresOnLinkLost — a wired connection dropping while the run
// context is live is the edge itself.
func TestALostLinkFiresOnLinkLost(t *testing.T) {
	// Arrange — the shim handshakes, acks, then drops the connection; every
	// later reconnect is held open so the loop does not churn.
	h := newHarness()
	var conns atomic.Int64
	path := startFakeShim(t, func(conn net.Conn) {
		fakeServerHandshake(t, conn, "sess-1", "1", false)
		if conns.Add(1) == 1 {
			conn.Close()
			return
		}
		_, _ = wire.ReadAny(conn) // hold it open
	})
	cfg := h.config(t, "sess-1", path)
	lost := make(chan error, 8)
	cfg.OnLinkLost = func(cause error) { lost <- cause }
	c := New(cfg)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	errCh := make(chan error, 1)

	// Act.
	go func() { errCh <- c.Run(ctx) }()

	// Assert — the callback IS the rendezvous; nothing here waits on a duration.
	if cause := <-lost; cause == nil {
		t.Fatal("OnLinkLost fired with a nil cause; the reason the link died must travel with the edge")
	}
	cancel()
	<-errCh
}

// TestATeardownDoesNotFireOnLinkLost — a cancelled run context is the session controller
// going away, not a link loss, and its own exit is the honest edge for it.
// Firing here would race that exit over one teardown.
func TestATeardownDoesNotFireOnLinkLost(t *testing.T) {
	// Arrange — a shim that stays up until the client lets go.
	h := newHarness()
	path := startFakeShim(t, func(conn net.Conn) {
		fakeServerHandshake(t, conn, "sess-1", "1", false)
		_, _ = wire.ReadAny(conn)
	})
	cfg := h.config(t, "sess-1", path)
	lost := make(chan error, 8)
	cfg.OnLinkLost = func(cause error) { lost <- cause }
	c := New(cfg)
	ctx, cancel := context.WithCancel(context.Background())
	errCh := make(chan error, 1)
	go func() { errCh <- c.Run(ctx) }()
	readyCtx, readyCancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer readyCancel()
	if err := c.AwaitReady(readyCtx); err != nil {
		t.Fatalf("AwaitReady: %v", err)
	}

	// Act — the teardown.
	cancel()

	// Assert — Run returning is the rendezvous: every teardown callback that
	// was ever going to fire has fired by then.
	if err := <-errCh; err != nil {
		t.Fatalf("Run after a clean cancel: %v", err)
	}
	select {
	case cause := <-lost:
		t.Fatalf("a teardown fired OnLinkLost (%v); the session controller exit owns that edge", cause)
	default:
	}
}

// TestAConnectionThatNeverWiredDoesNotFireOnLinkLost — a connection that died
// mid-gate never earned the wiring it would now be retracting.
func TestAConnectionThatNeverWiredDoesNotFireOnLinkLost(t *testing.T) {
	// Arrange — the first connection dies AFTER the handshake but BEFORE any
	// ShimReady; the second completes the gate.
	h := newHarness()
	var conns atomic.Int64
	path := startFakeShim(t, func(conn net.Conn) {
		if conns.Add(1) == 1 {
			mustWriteMsg(t, conn, &corev1.ShimHello{
				SessionId: "sess-1", Vendor: "claude", ShimVersion: "test-shim", ProtocolVersion: "1",
			})
			if _, err := wire.ReadAny(conn); err != nil {
				t.Errorf("shim reading DaemonHello: %v", err)
			}
			conn.Close()
			return
		}
		fakeServerHandshake(t, conn, "sess-1", "1", false)
		_, _ = wire.ReadAny(conn)
	})
	cfg := h.config(t, "sess-1", path)
	lost := make(chan error, 8)
	cfg.OnLinkLost = func(cause error) { lost <- cause }
	c := New(cfg)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	errCh := make(chan error, 1)
	go func() { errCh <- c.Run(ctx) }()

	// Act — readiness resolving on the SECOND connection happens strictly after
	// the first one's teardown ran, which is what makes the negative assertion
	// below a fact rather than a race.
	readyCtx, readyCancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer readyCancel()
	if err := c.AwaitReady(readyCtx); err != nil {
		t.Fatalf("AwaitReady: %v", err)
	}

	// Assert.
	select {
	case cause := <-lost:
		t.Fatalf("a never-wired connection fired OnLinkLost (%v); there was no wiring to retract", cause)
	default:
	}
	cancel()
	<-errCh
}
