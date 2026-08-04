package shimclient

import (
	"context"
	"errors"
	"net"
	"testing"
	"time"

	corev1 "agentrepl/proto/agentshim/core/v1"
	"agentrepl/wire"
)

// Config.OnHandshake — the hook that must run BEFORE the DaemonHello reads its
// from_seq off the SeqStore.
//
// A shim announcing a ROTATED vendor session id is telling the daemon its
// high-water mark counts in a store seq space that no longer exists. The reset
// therefore has to land ahead of the read, or this connection tells the shim to
// subscribe at a meaningless position and then reads the new space's seq=1 as a
// terminal regression. A hook running after the hello could only fix the NEXT
// connection, which is why this is not folded into OnConnected — and the
// ordering survived the move of from_seq onto the hello unchanged.

// helloThenDaemonHello stands up a fake shim that opens with hello, reports the
// DaemonHello the client answers with, and closes the gate with a ShimReady.
func helloThenDaemonHello(t *testing.T, hello *corev1.ShimHello) (string, chan *corev1.DaemonHello) {
	t.Helper()
	hellos := make(chan *corev1.DaemonHello, 1)
	path := startFakeShim(t, func(conn net.Conn) {
		mustWriteMsg(t, conn, hello)
		msg, err := wire.ReadAny(conn)
		if err != nil {
			t.Errorf("read DaemonHello: %v", err)
			return
		}
		dh, ok := msg.(*corev1.DaemonHello)
		if !ok {
			t.Errorf("expected DaemonHello, got %T", msg)
			return
		}
		// Ack BEFORE publishing: the test cancels as soon as it has the hello,
		// and a write racing that cancel would fail on a closed connection.
		mustWriteMsg(t, conn, &corev1.ShimReady{SessionId: hello.GetSessionId(), FromSeq: dh.GetFromSeq()})
		hellos <- dh
		_, _ = wire.ReadAny(conn) // hold the connection open
	})
	return path, hellos
}

// awaitDaemonHello takes the DaemonHello the client sent, or fails at the deadline.
func awaitDaemonHello(t *testing.T, hellos chan *corev1.DaemonHello) *corev1.DaemonHello {
	t.Helper()
	select {
	case dh := <-hellos:
		return dh
	case <-time.After(2 * time.Second):
		t.Fatal("the client never sent a DaemonHello")
		return nil
	}
}

func TestOnHandshakeRunsBeforeTheDaemonHelloReadsItsPosition(t *testing.T) {
	// Arrange — a mark of 5990 counted in the RETIRED seq space, and a hook
	// that resets it exactly as the rotation path does.
	h := newHarness()
	h.seq.SetLastSeq("sess-1", 5990)
	path, hellos := helloThenDaemonHello(t, &corev1.ShimHello{
		SessionId: "sess-1", Vendor: "claude", ShimVersion: "test-shim",
		ProtocolVersion: "1", VendorSessionId: "uuid-new",
	})
	cfg := h.config(t, "sess-1", path)
	cfg.OnHandshake = func(hello *corev1.ShimHello) error {
		if hello.GetVendorSessionId() == "uuid-new" {
			h.seq.SetLastSeq("sess-1", 0)
		}
		return nil
	}
	c := New(cfg)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	errCh := make(chan error, 1)
	go func() { errCh <- c.Run(ctx) }()

	// Act
	dh := awaitDaemonHello(t, hellos)

	// Assert — THIS connection told the shim to subscribe from the reset
	// position, not the retired space's mark.
	if dh.GetFromSeq() != 0 {
		t.Fatalf("DaemonHello from_seq = %d, want 0: the handshake hook's reset must land before the position is read", dh.GetFromSeq())
	}
	cancel()
	<-errCh
}

func TestOnHandshakeCarriesTheAnnouncedVendorSessionID(t *testing.T) {
	// Arrange — the announcement is the ONLY thing that tells the daemon which
	// seq space it is about to serve.
	h := newHarness()
	path, hellos := helloThenDaemonHello(t, &corev1.ShimHello{
		SessionId: "sess-1", Vendor: "claude", ShimVersion: "test-shim",
		ProtocolVersion: "1", VendorSessionId: "uuid-new",
	})
	cfg := h.config(t, "sess-1", path)
	seen := make(chan string, 1)
	cfg.OnHandshake = func(hello *corev1.ShimHello) error {
		seen <- hello.GetVendorSessionId()
		return nil
	}
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
	awaitDaemonHello(t, hellos)

	// Assert
	if got != "uuid-new" {
		t.Fatalf("OnHandshake saw vendor_session_id = %q, want %q", got, "uuid-new")
	}
	cancel()
	<-errCh
}

func TestDaemonHelloKeepsItsPositionWhenTheHookResetsNothing(t *testing.T) {
	// Arrange — an ordinary reattach: the shim re-announces the uuid it always
	// had, so the mark stands and the tail resumes where it left off.
	h := newHarness()
	h.seq.SetLastSeq("sess-1", 5990)
	path, hellos := helloThenDaemonHello(t, &corev1.ShimHello{
		SessionId: "sess-1", Vendor: "claude", ShimVersion: "test-shim",
		ProtocolVersion: "1", VendorSessionId: "uuid-old",
	})
	cfg := h.config(t, "sess-1", path)
	cfg.OnHandshake = func(hello *corev1.ShimHello) error {
		if hello.GetVendorSessionId() != "uuid-old" {
			h.seq.SetLastSeq("sess-1", 0)
		}
		return nil
	}
	c := New(cfg)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	errCh := make(chan error, 1)
	go func() { errCh <- c.Run(ctx) }()

	// Act
	dh := awaitDaemonHello(t, hellos)

	// Assert
	if dh.GetFromSeq() != 5990 {
		t.Fatalf("DaemonHello from_seq = %d, want 5990 preserved", dh.GetFromSeq())
	}
	cancel()
	<-errCh
}

func TestRejectedHandshakeIsTerminalBeforeDaemonHelloAndReady(t *testing.T) {
	h := newHarness()
	frames := make(chan any, 1)
	closed := make(chan error, 1)
	path := startFakeShim(t, func(conn net.Conn) {
		mustWriteMsg(t, conn, &corev1.ShimHello{
			SessionId: "sess-1", Vendor: "claude", ShimVersion: "test-shim",
			ProtocolVersion: "1", ActiveTurnIds: []string{"turn-other"},
		})
		msg, err := wire.ReadAny(conn)
		if err != nil {
			closed <- err
			return
		}
		frames <- msg
	})
	cfg := h.config(t, "sess-1", path)
	reconcileErr := errors.New("active turn identities disagree")
	cfg.OnHandshake = func(*corev1.ShimHello) error { return reconcileErr }
	connected := make(chan struct{}, 1)
	cfg.OnConnected = func(*corev1.ShimHello) bool { connected <- struct{}{}; return false }
	c := New(cfg)

	ctx, cancel := context.WithTimeout(context.Background(), 2*time.Second)
	defer cancel()
	err := c.Run(ctx)
	if !errors.Is(err, ErrHandshakeRejected) {
		t.Fatalf("Run err = %v, want ErrHandshakeRejected", err)
	}
	select {
	case msg := <-frames:
		t.Fatalf("rejected handshake sent %T; want connection closed before DaemonHello", msg)
	case <-closed:
	case <-time.After(time.Second):
		t.Fatal("shim did not observe the rejected connection close")
	}
	select {
	case <-connected:
		t.Fatal("OnConnected ran for a rejected handshake")
	default:
	}
	if err := c.AwaitReady(context.Background()); !errors.Is(err, ErrHandshakeRejected) || !errors.Is(err, reconcileErr) {
		t.Fatalf("AwaitReady err = %v, want exact terminal handshake rejection", err)
	}
}
