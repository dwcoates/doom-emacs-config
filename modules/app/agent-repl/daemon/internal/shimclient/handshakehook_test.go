package shimclient

import (
	"context"
	"net"
	"testing"
	"time"

	corev1 "agentrepl/proto/agentshim/core/v1"
	"agentrepl/wire"
)

// Config.OnHandshake — the hook that must run BEFORE the Subscribe reads its
// from_seq.
//
// A shim announcing a ROTATED vendor session id is telling the daemon its
// high-water mark counts in a store seq space that no longer exists. The reset
// therefore has to land ahead of the read, or this connection subscribes at a
// meaningless position and then reads the new space's seq=1 as a terminal
// regression. A hook running after the Subscribe could only fix the NEXT
// connection, which is why this is not folded into OnConnected.

// helloThenSubscribe stands up a fake shim that opens with hello, answers the
// DaemonHello, and reports the Subscribe the client sends.
func helloThenSubscribe(t *testing.T, hello *corev1.ShimHello) (string, chan *corev1.Subscribe) {
	t.Helper()
	subs := make(chan *corev1.Subscribe, 1)
	path := startFakeShim(t, func(conn net.Conn) {
		mustWriteMsg(t, conn, hello)
		if _, err := wire.ReadAny(conn); err != nil {
			t.Errorf("read DaemonHello: %v", err)
			return
		}
		msg, err := wire.ReadAny(conn)
		if err != nil {
			t.Errorf("read Subscribe: %v", err)
			return
		}
		sub, ok := msg.(*corev1.Subscribe)
		if !ok {
			t.Errorf("expected Subscribe, got %T", msg)
			return
		}
		subs <- sub
		_, _ = wire.ReadAny(conn) // hold the connection open
	})
	return path, subs
}

// awaitSubscribe takes the Subscribe the client sent, or fails at the deadline.
func awaitSubscribe(t *testing.T, subs chan *corev1.Subscribe) *corev1.Subscribe {
	t.Helper()
	select {
	case sub := <-subs:
		return sub
	case <-time.After(2 * time.Second):
		t.Fatal("the client never sent a Subscribe")
		return nil
	}
}

func TestOnHandshakeRunsBeforeTheSubscribeReadsItsPosition(t *testing.T) {
	// Arrange — a mark of 5990 counted in the RETIRED seq space, and a hook
	// that resets it exactly as the rotation path does.
	h := newHarness()
	h.seq.SetLastSeq("sess-1", 5990)
	path, subs := helloThenSubscribe(t, &corev1.ShimHello{
		SessionId: "sess-1", Vendor: "claude", ShimVersion: "test-shim",
		ProtocolVersion: "1", VendorSessionId: "uuid-new",
	})
	cfg := h.config(t, "sess-1", path)
	cfg.OnHandshake = func(hello *corev1.ShimHello) {
		if hello.GetVendorSessionId() == "uuid-new" {
			h.seq.SetLastSeq("sess-1", 0)
		}
	}
	c := New(cfg)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	errCh := make(chan error, 1)
	go func() { errCh <- c.Run(ctx) }()

	// Act
	sub := awaitSubscribe(t, subs)

	// Assert — THIS connection subscribed from the reset position, not the
	// retired space's mark.
	if sub.GetFromSeq() != 0 {
		t.Fatalf("Subscribe from_seq = %d, want 0: the handshake hook's reset must land before the position is read", sub.GetFromSeq())
	}
	cancel()
	<-errCh
}

func TestOnHandshakeCarriesTheAnnouncedVendorSessionID(t *testing.T) {
	// Arrange — the announcement is the ONLY thing that tells the daemon which
	// seq space it is about to serve.
	h := newHarness()
	path, subs := helloThenSubscribe(t, &corev1.ShimHello{
		SessionId: "sess-1", Vendor: "claude", ShimVersion: "test-shim",
		ProtocolVersion: "1", VendorSessionId: "uuid-new",
	})
	cfg := h.config(t, "sess-1", path)
	seen := make(chan string, 1)
	cfg.OnHandshake = func(hello *corev1.ShimHello) { seen <- hello.GetVendorSessionId() }
	c := New(cfg)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	errCh := make(chan error, 1)
	go func() { errCh <- c.Run(ctx) }()

	// Act
	var got string
	select {
	case got = <-seen:
	case <-time.After(2 * time.Second):
		t.Fatal("OnHandshake never fired")
	}
	awaitSubscribe(t, subs)

	// Assert
	if got != "uuid-new" {
		t.Fatalf("OnHandshake saw vendor_session_id = %q, want %q", got, "uuid-new")
	}
	cancel()
	<-errCh
}

func TestSubscribeKeepsItsPositionWhenTheHookResetsNothing(t *testing.T) {
	// Arrange — an ordinary reattach: the shim re-announces the uuid it always
	// had, so the mark stands and the tail resumes where it left off.
	h := newHarness()
	h.seq.SetLastSeq("sess-1", 5990)
	path, subs := helloThenSubscribe(t, &corev1.ShimHello{
		SessionId: "sess-1", Vendor: "claude", ShimVersion: "test-shim",
		ProtocolVersion: "1", VendorSessionId: "uuid-old",
	})
	cfg := h.config(t, "sess-1", path)
	cfg.OnHandshake = func(hello *corev1.ShimHello) {
		if hello.GetVendorSessionId() != "uuid-old" {
			h.seq.SetLastSeq("sess-1", 0)
		}
	}
	c := New(cfg)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	errCh := make(chan error, 1)
	go func() { errCh <- c.Run(ctx) }()

	// Act
	sub := awaitSubscribe(t, subs)

	// Assert
	if sub.GetFromSeq() != 5990 {
		t.Fatalf("Subscribe from_seq = %d, want 5990 preserved", sub.GetFromSeq())
	}
	cancel()
	<-errCh
}
