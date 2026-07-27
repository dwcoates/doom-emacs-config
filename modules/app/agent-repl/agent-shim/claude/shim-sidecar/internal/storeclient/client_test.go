package storeclient

import (
	"errors"
	"net"
	"os"
	"os/exec"
	"path/filepath"
	"testing"
	"time"

	corev1 "agentrepl/proto/agentshim/core/v1"
	"agentrepl/wire"
)

// shimStoreDir walks up from the test's working directory to the sibling
// shim-store module (…/agent-shim/shim-store).
func shimStoreDir(t *testing.T) string {
	t.Helper()
	dir, _ := os.Getwd()
	for {
		cand := filepath.Join(dir, "agent-shim", "shim-store")
		if _, err := os.Stat(filepath.Join(cand, "main.go")); err == nil {
			return cand
		}
		p := filepath.Dir(dir)
		if p == dir {
			t.Fatalf("shim-store module not found above %s", dir)
		}
		dir = p
	}
}

// startRealStore builds and spawns the REAL shim-store binary on a temp socket
// and returns the socket path. The store, db, and log all live under temp dirs
// so nothing touches the real ~/.cache locations.
func startRealStore(t *testing.T) string {
	t.Helper()
	srcDir := shimStoreDir(t)
	tmp := t.TempDir()
	bin := filepath.Join(tmp, "shim-store")
	build := exec.Command("go", "build", "-o", bin, ".")
	build.Dir = srcDir
	if out, err := build.CombinedOutput(); err != nil {
		t.Fatalf("building shim-store: %v\n%s", err, out)
	}
	// macOS UDS sun_path is short; keep the socket path under /tmp.
	sockDir, err := os.MkdirTemp("/tmp", "sidecarstore")
	if err != nil {
		t.Fatalf("mkdtemp: %v", err)
	}
	t.Cleanup(func() { os.RemoveAll(sockDir) })
	sock := filepath.Join(sockDir, "s")

	cmd := exec.Command(bin,
		"-socket", sock,
		"-db", filepath.Join(tmp, "events.db"),
		"-log", filepath.Join(tmp, "store.log"),
	)
	cmd.Stderr = os.Stderr
	if err := cmd.Start(); err != nil {
		t.Fatalf("starting shim-store: %v", err)
	}
	t.Cleanup(func() {
		_ = cmd.Process.Kill()
		_, _ = cmd.Process.Wait()
	})

	// Readiness poll: retry dialing until the store is listening. This waits on
	// an EXTERNAL process's socket (no in-process channel can signal it); the
	// small backoff is readiness polling, not goroutine synchronization.
	deadline := time.Now().Add(10 * time.Second)
	for time.Now().Before(deadline) {
		if c, err := net.Dial("unix", sock); err == nil {
			c.Close()
			return sock
		}
		time.Sleep(20 * time.Millisecond)
	}
	t.Fatalf("shim-store did not start listening on %s", sock)
	return ""
}

func TestIntegrationRecoverCursorsEmpty(t *testing.T) {
	// Arrange
	sock := startRealStore(t)
	c := New(sock, nil)
	defer c.Close()
	// Act
	cursors, err := c.RecoverCursors("")
	// Assert
	if err != nil {
		t.Fatalf("RecoverCursors: %v", err)
	}
	if len(cursors) != 0 {
		t.Fatalf("cursors = %d, want 0 on a fresh store", len(cursors))
	}
}

func TestRecoverRejectsStoreWithoutAuthoritativeOpenTaskState(t *testing.T) {
	socketDir, err := os.MkdirTemp("/tmp", "sidecar-recovery")
	if err != nil {
		t.Fatalf("create short socket dir: %v", err)
	}
	t.Cleanup(func() { os.RemoveAll(socketDir) })
	sock := filepath.Join(socketDir, "s")
	ln, err := net.Listen("unix", sock)
	if err != nil {
		t.Fatalf("listen: %v", err)
	}
	defer ln.Close()
	served := make(chan error, 1)
	go func() {
		conn, err := ln.Accept()
		if err != nil {
			served <- err
			return
		}
		defer conn.Close()
		if _, err := wire.ReadAny(conn); err != nil {
			served <- err
			return
		}
		served <- wire.WriteAny(conn, &corev1.CursorList{})
	}()

	_, err = New(sock, nil).Recover("")
	if err == nil {
		t.Fatal("Recover accepted a CursorList with no authoritative open-task attestation")
	}
	if serveErr := <-served; serveErr != nil {
		t.Fatalf("serve recovery response: %v", serveErr)
	}
}

func TestIntegrationWriteAckThenCursorRecovery(t *testing.T) {
	// Arrange
	sock := startRealStore(t)
	c := New(sock, nil)
	defer c.Close()
	if err := c.Connect(); err != nil {
		t.Fatalf("Connect: %v", err)
	}
	ev := &corev1.Event{
		SessionId: "s1",
		Plane:     corev1.Plane_PLANE_FILE,
		Class:     corev1.EventClass_EVENT_CLASS_PERSISTENT,
		Payload:   &corev1.Event_TurnEnded{TurnEnded: &corev1.TurnEnded{StopReason: "done"}},
		DedupKey:  "turn:s1:u1",
	}
	batch := &corev1.EventBatch{
		Events:        []*corev1.Event{ev},
		CursorAdvance: &corev1.CursorState{FileId: "7:7", Path: "/p/s1.jsonl", Offset: 99, Carry: []byte("z")},
	}
	// Act: write the batch.
	ack, err := c.Write("shim-claude-sidecar", batch)
	if err != nil {
		t.Fatalf("Write: %v", err)
	}
	// Assert: one accepted event, a seq assigned.
	if ack.GetAccepted() != 1 || ack.GetLastSeq() == 0 {
		t.Fatalf("ack = %+v, want accepted=1 last_seq>0", ack)
	}
	// Act: recover the cursor committed atomically with the batch.
	cursors, err := c.RecoverCursors("")
	if err != nil {
		t.Fatalf("RecoverCursors: %v", err)
	}
	// Assert
	if len(cursors) != 1 || cursors[0].GetOffset() != 99 || cursors[0].GetFileId() != "7:7" {
		t.Fatalf("recovered cursors = %+v", cursors)
	}
}

func TestIntegrationDedupOnReplay(t *testing.T) {
	// Arrange: the same producer-keyed batch written twice (a crash-replay).
	sock := startRealStore(t)
	c := New(sock, nil)
	defer c.Close()
	if err := c.Connect(); err != nil {
		t.Fatalf("Connect: %v", err)
	}
	mk := func() *corev1.EventBatch {
		return &corev1.EventBatch{Events: []*corev1.Event{{
			SessionId: "s1",
			Class:     corev1.EventClass_EVENT_CLASS_PERSISTENT,
			Plane:     corev1.Plane_PLANE_FILE,
			Payload:   &corev1.Event_TurnEnded{TurnEnded: &corev1.TurnEnded{StopReason: "x"}},
			DedupKey:  "turn:s1:same",
		}}}
	}
	// Act
	a1, err := c.Write("shim-claude-sidecar", mk())
	if err != nil {
		t.Fatalf("write1: %v", err)
	}
	a2, err := c.Write("shim-claude-sidecar", mk())
	if err != nil {
		t.Fatalf("write2: %v", err)
	}
	// Assert: first accepted, second fully deduped (exactly-once under replay).
	if a1.GetAccepted() != 1 || a2.GetDeduped() != 1 || a2.GetAccepted() != 0 {
		t.Fatalf("a1=%+v a2=%+v, want accept-then-dedup", a1, a2)
	}
}

func TestIntegrationHeartbeat(t *testing.T) {
	// Arrange: an established producer connection before any file change has
	// produced the StoreWrite that ordinarily declares its role.
	sock := startRealStore(t)
	c := New(sock, nil)
	defer c.Close()
	if err := c.Connect(); err != nil {
		t.Fatalf("Connect: %v", err)
	}

	// Act / Assert: heartbeat itself can open the producer preamble.
	if err := c.Heartbeat(); err != nil {
		t.Fatalf("Heartbeat: %v", err)
	}
	// The same connection must still accept its eventual first write.
	if _, err := c.Write("shim-claude-sidecar", &corev1.EventBatch{}); err != nil {
		t.Fatalf("first write after heartbeat: %v", err)
	}
}

func TestWriteErrorSurfacedOnDeadStore(t *testing.T) {
	// Arrange: a client pointed at a socket with no server (honest sad path).
	c := New(filepath.Join(t.TempDir(), "nonexistent.sock"), nil)
	// Act
	_, err := c.Write("shim-claude-sidecar", &corev1.EventBatch{})
	// Assert: the failure is surfaced, never swallowed.
	if err == nil {
		t.Fatal("expected an error writing to a dead store")
	}
}

func TestWriteNeverDialsImplicitly(t *testing.T) {
	// Arrange: a LIVE store, but a client that was never connected. The dial
	// would succeed, which is exactly why the write must not attempt one: a
	// connection born under a write skipped cursor recovery.
	sock := startRealStore(t)
	c := New(sock, nil)
	defer c.Close()

	// Act
	_, err := c.Write("shim-claude-sidecar", &corev1.EventBatch{})

	// Assert
	if !errors.Is(err, ErrNotConnected) {
		t.Fatalf("Write err = %v, want ErrNotConnected", err)
	}
	if c.Connected() {
		t.Fatal("Write opened a producer connection; it must never dial")
	}
}

func TestHeartbeatOnADownConnectionIsAnErrorNotSilence(t *testing.T) {
	// Arrange: never connected. A heartbeat exists to detect a dead link, so
	// answering "fine" here would hide the very outage it is asked about.
	c := New(filepath.Join(t.TempDir(), "nonexistent.sock"), nil)

	// Act / Assert
	if err := c.Heartbeat(); !errors.Is(err, ErrNotConnected) {
		t.Fatalf("Heartbeat err = %v, want ErrNotConnected", err)
	}
}

func TestConnectedGoesFalseAfterTheStoreDies(t *testing.T) {
	// Arrange: an established producer connection to a real store.
	sock := startRealStore(t)
	c := New(sock, nil)
	defer c.Close()
	if err := c.Connect(); err != nil {
		t.Fatalf("Connect: %v", err)
	}
	if _, err := c.Write("shim-claude-sidecar", &corev1.EventBatch{}); err != nil {
		t.Fatalf("prime write: %v", err)
	}

	// Act: break the socket underneath the client (same-package reach-in, so the
	// connection stays non-nil and the failure has to come from the transport),
	// then write.
	c.conn.Close()
	_, err := c.Write("shim-claude-sidecar", &corev1.EventBatch{})

	// Assert: the caller learns the LINK is gone, not merely that a write failed.
	if err == nil {
		t.Fatal("expected an error writing on a dropped connection")
	}
	if c.Connected() {
		t.Fatal("Connected() stayed true after the connection was dropped")
	}
}
