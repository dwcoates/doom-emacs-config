package shimclient

import (
	"context"
	"net"
	"testing"
	"time"

	corev1 "agentrepl/proto/agentshim/core/v1"
	"agentrepl/wire"
)

// THE BRING-UP GATE, daemon side (core.proto ShimHello).
//
// AwaitReady used to resolve on the CONNECTION, which proved only that frames
// could be written. Everything the daemon does next — a health probe above all
// — depends instead on the shim having finished its own wiring, and the gap
// between those two facts is where the store_subscribed=false failures lived.
// These pin the new meaning: readiness IS the shim's ShimReady, and nothing
// issued after it has to be retried.

func TestAwaitReadyWaitsForTheShimReadyAckNotTheConnection(t *testing.T) {
	// Arrange — a shim that handshakes and then deliberately WITHHOLDS its ack,
	// exactly as one whose store subscription failed would.
	h := newHarness()
	ack := make(chan struct{})
	path := startFakeShim(t, func(conn net.Conn) {
		mustWriteMsg(t, conn, &corev1.ShimHello{
			SessionId: "sess-1", Vendor: "claude", ShimVersion: "test-shim", ProtocolVersion: "1",
		})
		if _, err := wire.ReadAny(conn); err != nil {
			t.Errorf("read DaemonHello: %v", err)
			return
		}
		<-ack
		mustWriteMsg(t, conn, &corev1.ShimReady{SessionId: "sess-1"})
		_, _ = wire.ReadAny(conn) // hold the connection open
	})
	c := New(h.config(t, "sess-1", path))
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	errCh := make(chan error, 1)
	go func() { errCh <- c.Run(ctx) }()

	// Act — the connection exists (the client answered the hello), yet
	// readiness must not resolve on it.
	waitCtx, waitCancel := context.WithTimeout(context.Background(), 150*time.Millisecond)
	defer waitCancel()
	if err := c.AwaitReady(waitCtx); err == nil {
		t.Fatal("AwaitReady resolved on the connection alone; only the ShimReady may release it")
	}

	// Assert — and it resolves the moment the ack lands.
	close(ack)
	readyCtx, readyCancel := context.WithTimeout(context.Background(), 2*time.Second)
	defer readyCancel()
	if err := c.AwaitReady(readyCtx); err != nil {
		t.Fatalf("AwaitReady after the ack: %v", err)
	}
	cancel()
	<-errCh
}

func TestHealthProbeImmediatelyAfterAwaitReadyAnswersOnTheFirstTry(t *testing.T) {
	// Arrange — the SPC-o-c shape: probe the instant readiness returns, with no
	// retry to hide a race. The fake shim answers a HealthCheck only after it
	// has acked, so a probe that outran the gate would time out here.
	h := newHarness()
	path := startFakeShim(t, func(conn net.Conn) {
		fakeServerHandshake(t, conn, "sess-1", "1", false)
		msg, err := wire.ReadAny(conn)
		if err != nil {
			t.Errorf("read HealthCheck: %v", err)
			return
		}
		check, ok := msg.(*corev1.HealthCheck)
		if !ok {
			t.Errorf("expected HealthCheck, got %T", msg)
			return
		}
		mustWriteMsg(t, conn, &corev1.HealthStatus{
			RequestId: check.GetRequestId(), Healthy: true, Component: "claude-shim",
		})
		_, _ = wire.ReadAny(conn)
	})
	c := New(h.config(t, "sess-1", path))
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	errCh := make(chan error, 1)
	go func() { errCh <- c.Run(ctx) }()

	// Act
	probeCtx, probeCancel := context.WithTimeout(context.Background(), 2*time.Second)
	defer probeCancel()
	if err := c.AwaitReady(probeCtx); err != nil {
		t.Fatalf("AwaitReady: %v", err)
	}
	status, err := c.Health(probeCtx, "probe-1")

	// Assert — first attempt, healthy.
	if err != nil {
		t.Fatalf("Health right after AwaitReady: %v", err)
	}
	if !status.GetHealthy() || status.GetRequestId() != "probe-1" {
		t.Fatalf("health status = %+v, want healthy probe-1", status)
	}
	cancel()
	<-errCh
}
