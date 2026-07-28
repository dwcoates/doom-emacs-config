package shimclient

import (
	"context"
	"net"
	"testing"
	"time"

	corev1 "agentrepl/proto/agentshim/core/v1"
	"agentrepl/wire"

	"claude-repld/internal/protocol"
)

// HANDSHAKE-BORNE PERMISSION MODE, daemon side (core.proto
// DaemonHello.permission_mode).
//
// The defect these pin: a session created with no explicit mode spawned a shim
// with no --permission-mode flag, whose parser then defaulted to "default" —
// prompting mode — and the generated first prompt ran with nobody there to
// answer. The posture now travels in the gate, resolved from the record at one
// site, so the shim's mode is whatever the record says by construction.

// modeStore is a fixed ModeStore: whatever the record is said to carry.
type modeStore struct{ mode string }

func (m modeStore) PermissionMode(string) string { return m.mode }

// readDaemonHello runs one fake shim through the gate and hands back the
// DaemonHello it received.
func readDaemonHello(t *testing.T, cfg Config) *corev1.DaemonHello {
	t.Helper()
	got := make(chan *corev1.DaemonHello, 1)
	path := startFakeShim(t, func(conn net.Conn) {
		mustWriteMsg(t, conn, &corev1.ShimHello{
			SessionId: cfg.SessionID, Vendor: "claude", ShimVersion: "test-shim", ProtocolVersion: "1",
		})
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
		got <- dh
		// No ShimReady: the claim is about the hello alone, and the caller
		// cancels the moment it has one. Acking would race that cancel and
		// fail on a closed pipe rather than on anything under test.
		_, _ = wire.ReadAny(conn) // hold the connection open until cancel
	})
	cfg.Source = dialSource{path: path}
	c := New(cfg)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	errCh := make(chan error, 1)
	go func() { errCh <- c.Run(ctx) }()

	select {
	case dh := <-got:
		cancel()
		<-errCh
		return dh
	case <-time.After(2 * time.Second):
		cancel()
		<-errCh
		t.Fatal("no DaemonHello arrived within the deadline")
		return nil
	}
}

func TestDaemonHelloCarriesTheRecordsPermissionMode(t *testing.T) {
	// Arrange
	h := newHarness()
	cfg := h.config(t, "sess-1", "")
	cfg.PermissionModes = modeStore{mode: "plan"}

	// Act
	dh := readDaemonHello(t, cfg)

	// Assert
	if got := dh.GetPermissionMode(); got != "plan" {
		t.Fatalf("DaemonHello permission_mode = %q, want the record's %q", got, "plan")
	}
}

func TestDaemonHelloResolvesAnEmptyRecordToTheSessionDefault(t *testing.T) {
	// Arrange — the defect's own case: a record carrying no mode at all.
	h := newHarness()
	cfg := h.config(t, "sess-1", "")
	cfg.PermissionModes = modeStore{mode: ""}

	// Act
	dh := readDaemonHello(t, cfg)

	// Assert — never empty on the wire; empty is reserved for an older daemon.
	if got, want := dh.GetPermissionMode(), string(protocol.DefaultSessionPermissionMode); got != want {
		t.Fatalf("DaemonHello permission_mode = %q, want the resolved default %q", got, want)
	}
}

func TestDaemonHelloResolvesANilModeStoreToTheSessionDefault(t *testing.T) {
	// Arrange — an unwired ModeStore must take the same branch as an empty
	// record, so omission can never hand a session a posture nobody chose.
	h := newHarness()
	cfg := h.config(t, "sess-1", "")
	cfg.PermissionModes = nil

	// Act
	dh := readDaemonHello(t, cfg)

	// Assert
	if got, want := dh.GetPermissionMode(), string(protocol.DefaultSessionPermissionMode); got != want {
		t.Fatalf("DaemonHello permission_mode = %q, want the resolved default %q", got, want)
	}
}
