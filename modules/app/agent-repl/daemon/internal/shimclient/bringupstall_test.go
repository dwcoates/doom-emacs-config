package shimclient

import (
	"context"
	"errors"
	"net"
	"strings"
	"testing"
	"time"

	corev1 "agentrepl/proto/agentshim/core/v1"
	"agentrepl/wire"
)

// THE BRING-UP BOUND IS SILENCE, NOT ELAPSED TIME (Config.BringUpStall).
//
// ShimReady is the LAST frame of the gate, so it is ordered behind everything
// the shim wrote before it on the same stream — for a workspace with a long
// transcript, thousands of replayed events that this daemon's single read loop
// drains one sink call at a time. Under the old absolute deadline the daemon
// declared "the shim process is STILL ALIVE but never dialled in" about a shim
// that had dialled in, handshaked, and was feeding it at full rate. The bigger
// the conversation, the more certainly it tripped, so the workspaces with the
// most history were the ones that could never be opened.

// TestABusyShimIsNotTimedOutWhileItIsStillFeedingUs is the regression: frames
// keep arriving for several stall windows before the ack, and the bring-up must
// survive all of it.
func TestABusyShimIsNotTimedOutWhileItIsStillFeedingUs(t *testing.T) {
	// Arrange — a shim that streams a backlog for well past one stall window
	// and only then closes its gate.
	const stall = 60 * time.Millisecond
	h := newHarness()
	path := startFakeShim(t, func(conn net.Conn) {
		mustWriteMsg(t, conn, &corev1.ShimHello{
			SessionId: "sess-1", Vendor: "claude", ShimVersion: "test-shim", ProtocolVersion: "1",
		})
		if _, err := wire.ReadAny(conn); err != nil {
			t.Errorf("read DaemonHello: %v", err)
			return
		}
		backlog := time.NewTicker(stall / 6)
		defer backlog.Stop()
		for sent := 0; sent < 24; sent++ {
			<-backlog.C
			mustWriteMsg(t, conn, &corev1.Heartbeat{SentAtMs: time.Now().UnixMilli()})
		}
		mustWriteMsg(t, conn, &corev1.ShimReady{SessionId: "sess-1"})
		_, _ = wire.ReadAny(conn) // hold the connection open
	})
	cfg := h.config(t, "sess-1", path)
	cfg.BringUpStall = stall
	c := New(cfg)
	runCtx, cancelRun := context.WithCancel(context.Background())
	defer cancelRun()
	runErr := make(chan error, 1)
	go func() { runErr <- c.Run(runCtx) }()

	// Act — the absolute cap is far beyond the whole backlog, so only the
	// silence rule could end this wait early.
	waitCtx, cancelWait := context.WithTimeout(context.Background(), 30*time.Second)
	defer cancelWait()
	err := c.AwaitReady(waitCtx)

	// Assert.
	if err != nil {
		t.Fatalf("AwaitReady = %v, want the busy shim's bring-up to survive its own backlog", err)
	}
	cancelRun()
	<-runErr
}

// TestASilentShimStillFailsItsBringUp: the failure detection the bound exists
// for is intact. A shim that connects and then says nothing is exactly the
// wedge the timeout was written to catch.
func TestASilentShimStillFailsItsBringUp(t *testing.T) {
	// Arrange — handshake, then silence, and never an ack.
	const stall = 60 * time.Millisecond
	h := newHarness()
	held := make(chan struct{})
	path := startFakeShim(t, func(conn net.Conn) {
		mustWriteMsg(t, conn, &corev1.ShimHello{
			SessionId: "sess-1", Vendor: "claude", ShimVersion: "test-shim", ProtocolVersion: "1",
		})
		if _, err := wire.ReadAny(conn); err != nil {
			t.Errorf("read DaemonHello: %v", err)
			return
		}
		<-held
	})
	defer close(held)
	cfg := h.config(t, "sess-1", path)
	cfg.BringUpStall = stall
	c := New(cfg)
	runCtx, cancelRun := context.WithCancel(context.Background())
	defer cancelRun()
	runErr := make(chan error, 1)
	go func() { runErr <- c.Run(runCtx) }()

	// Act — the caller's own cap is far away, so the silence rule is the only
	// thing that can resolve this.
	waitCtx, cancelWait := context.WithTimeout(context.Background(), 30*time.Second)
	defer cancelWait()
	err := c.AwaitReady(waitCtx)

	// Assert — it fails, and it fails as a deadline so the ladder still
	// classifies it as a timeout rather than a transport fault.
	if !errors.Is(err, context.DeadlineExceeded) {
		t.Fatalf("AwaitReady = %v, want a deadline for a shim that went silent mid-gate", err)
	}
	if !strings.Contains(err.Error(), "of silence") {
		t.Fatalf("AwaitReady error %q does not name silence as the reason", err)
	}
	cancelRun()
	<-runErr
}

// TestTheCallersCapStillBoundsAWaitWithNoStallRule: BringUpStall is additive.
// Zero restores the pure-context bound, so no existing caller changed meaning.
func TestTheCallersCapStillBoundsAWaitWithNoStallRule(t *testing.T) {
	// Arrange — no stall rule configured, and a shim that never connects.
	h := newHarness()
	cfg := h.config(t, "sess-1", "/nonexistent-shim.sock")
	cfg.BringUpStall = 0
	c := New(cfg)

	// Act.
	ctx, cancel := context.WithTimeout(context.Background(), 20*time.Millisecond)
	defer cancel()
	err := c.AwaitReady(ctx)

	// Assert.
	if !errors.Is(err, context.DeadlineExceeded) {
		t.Fatalf("AwaitReady = %v, want the caller's context to bound it", err)
	}
	if strings.Contains(err.Error(), "of silence") {
		t.Fatalf("AwaitReady error %q claims a silence bound that was never configured", err)
	}
}
