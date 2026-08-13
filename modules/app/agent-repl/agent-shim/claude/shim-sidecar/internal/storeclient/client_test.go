package storeclient

import (
	"bytes"
	"errors"
	"io"
	"net"
	"os"
	"path/filepath"
	"strings"
	"testing"

	agentshimv1 "agentrepl/proto/agentshim/v1"
	protocolv1 "agentrepl/proto/protocol/v1"
	"agentrepl/shim-claude-sidecar/internal/logging"
	"agentrepl/wire"
)

func testLog() *logging.Bound {
	return logging.New(io.Discard, io.Discard).With(logging.Context{Component: "test"})
}

// shortSocket returns an unused path under /tmp. macOS caps sun_path well below
// what a t.TempDir() path costs, so the socket cannot live in the test's dir.
func shortSocket(t *testing.T) string {
	t.Helper()
	dir, err := os.MkdirTemp("/tmp", "sidecarstore")
	if err != nil {
		t.Fatalf("mkdtemp: %v", err)
	}
	t.Cleanup(func() { os.RemoveAll(dir) })
	return filepath.Join(dir, "s")
}

// serveOnce accepts one connection, hands it to serve, and reports serve's
// error on the returned channel. The channel IS the synchronization: a test
// reads it to know the server finished.
func serveOnce(t *testing.T, sock string, serve func(net.Conn) error) <-chan error {
	t.Helper()
	ln, err := net.Listen("unix", sock)
	if err != nil {
		t.Fatalf("listen: %v", err)
	}
	t.Cleanup(func() { ln.Close() })
	served := make(chan error, 1)
	go func() {
		conn, err := ln.Accept()
		if err != nil {
			served <- err
			return
		}
		defer conn.Close()
		served <- serve(conn)
	}()
	return served
}

// pipedClient wires a client straight onto an in-memory connection, so a test
// exercises the frame exchange without a listener at all. The reach-in is
// deliberate: Connect is the only thing that dials, and these tests are about
// what happens on an ALREADY established connection.
func pipedClient(t *testing.T, log *logging.Bound) (*Client, net.Conn) {
	t.Helper()
	c := New("/tmp/test-store.sock", log)
	client, server := net.Pipe()
	c.conn = client
	t.Cleanup(func() {
		_ = c.Close()
		_ = server.Close()
	})
	return c, server
}

func TestRecoverReturnsCursorsAndOpenTasks(t *testing.T) {
	// Arrange
	sock := shortSocket(t)
	served := serveOnce(t, sock, func(conn net.Conn) error {
		msg, err := wire.ReadAny(conn)
		if err != nil {
			return err
		}
		if _, ok := msg.(*agentshimv1.CursorQuery); !ok {
			return errors.New("expected CursorQuery")
		}
		return wire.WriteAny(conn, &agentshimv1.CursorList{
			Cursors:                []*agentshimv1.CursorState{{FileId: "7:7", Path: "/p/s1.jsonl", Offset: 99}},
			OpenTasks:              []*agentshimv1.OpenTaskState{{LastActivityAtMs: 5}},
			OpenTasksAuthoritative: true,
		})
	})

	// Act
	recovery, err := New(sock, testLog()).Recover("")

	// Assert
	if err != nil {
		t.Fatalf("Recover: %v", err)
	}
	if len(recovery.Cursors) != 1 || recovery.Cursors[0].GetOffset() != 99 {
		t.Fatalf("recovered cursors = %+v", recovery.Cursors)
	}
	if len(recovery.OpenTasks) != 1 || recovery.OpenTasks[0].GetLastActivityAtMs() != 5 {
		t.Fatalf("recovered open tasks = %+v", recovery.OpenTasks)
	}
	if serveErr := <-served; serveErr != nil {
		t.Fatalf("serve recovery response: %v", serveErr)
	}
}

func TestRecoverRejectsStoreWithoutAuthoritativeOpenTaskState(t *testing.T) {
	// Arrange: a store that answers without attesting its open-task set.
	sock := shortSocket(t)
	served := serveOnce(t, sock, func(conn net.Conn) error {
		if _, err := wire.ReadAny(conn); err != nil {
			return err
		}
		return wire.WriteAny(conn, &agentshimv1.CursorList{})
	})

	// Act
	_, err := New(sock, testLog()).Recover("")

	// Assert
	if err == nil {
		t.Fatal("Recover accepted a CursorList with no authoritative open-task attestation")
	}
	if serveErr := <-served; serveErr != nil {
		t.Fatalf("serve recovery response: %v", serveErr)
	}
}

func TestWriteSendsAStoreEntryWriteAndDoesNotWaitForAnAck(t *testing.T) {
	// Arrange — StoreWriteAck was retired with no successor, so the write is
	// one-way: the frame goes out and nothing is read back. A server that never
	// replies must therefore leave Write succeeding rather than blocking.
	c, server := pipedClient(t, testLog())
	served := make(chan error, 1)
	go func() {
		msg, err := wire.ReadAny(server)
		if err != nil {
			served <- err
			return
		}
		write, ok := msg.(*agentshimv1.StoreEntryWrite)
		if !ok {
			served <- errors.New("expected StoreEntryWrite")
			return
		}
		if write.GetProducer() != "shim-claude-sidecar" {
			served <- errors.New("producer = " + write.GetProducer())
			return
		}
		if write.GetBatch().GetCursorAdvance().GetOffset() != 99 {
			served <- errors.New("cursor advance did not ride with the records")
			return
		}
		served <- nil
	}()

	// Act
	err := c.Write("shim-claude-sidecar", &agentshimv1.EntryBatch{
		Entries:       []*agentshimv1.Entry{{Internal: &agentshimv1.InternalEntry{WriteId: "w1"}}},
		CursorAdvance: &agentshimv1.CursorState{FileId: "7:7", Path: "/p/s1.jsonl", Offset: 99, Carry: []byte("z")},
	})

	// Assert
	if err != nil {
		t.Fatalf("Write: %v", err)
	}
	if serveErr := <-served; serveErr != nil {
		t.Fatalf("serve StoreEntryWrite: %v", serveErr)
	}
	if !c.Connected() {
		t.Fatal("a successful write dropped the producer connection")
	}
}

func TestWriteTransportFailureDropsTheConnection(t *testing.T) {
	// Arrange: an established connection broken underneath the client, so the
	// failure has to come from the transport rather than from a nil conn.
	c, server := pipedClient(t, testLog())
	if err := server.Close(); err != nil {
		t.Fatalf("close server: %v", err)
	}

	// Act
	err := c.Write("shim-claude-sidecar", &agentshimv1.EntryBatch{})

	// Assert: the caller learns the LINK is gone, not merely that a write failed.
	if err == nil {
		t.Fatal("expected an error writing on a broken connection")
	}
	if c.Connected() {
		t.Fatal("Connected() stayed true after the transport failed")
	}
}

func TestWriteErrorSurfacedWithoutAGlobalErrorLog(t *testing.T) {
	// Arrange: never connected, so the write cannot land (honest sad path). The
	// error belongs to the caller, which owns the dropped-batch report.
	var logs bytes.Buffer
	c := New(shortSocket(t), logging.New(&logs, &logs).With(logging.Context{Component: "test"}))

	// Act
	err := c.Write("shim-claude-sidecar", &agentshimv1.EntryBatch{})

	// Assert
	if err == nil {
		t.Fatal("expected an error writing with no producer connection")
	}
	if strings.Contains(logs.String(), `"level":"error"`) {
		t.Fatalf("storeclient globally logged caller-owned write error: %q", logs.String())
	}
}

func TestWriteNeverDialsImplicitly(t *testing.T) {
	// Arrange: a LIVE listener, but a client that was never connected. The dial
	// would succeed, which is exactly why the write must not attempt one: a
	// connection born under a write skipped cursor recovery.
	sock := shortSocket(t)
	ln, err := net.Listen("unix", sock)
	if err != nil {
		t.Fatalf("listen: %v", err)
	}
	defer ln.Close()
	c := New(sock, testLog())
	defer c.Close()

	// Act
	err = c.Write("shim-claude-sidecar", &agentshimv1.EntryBatch{})

	// Assert
	if !errors.Is(err, ErrNotConnected) {
		t.Fatalf("Write err = %v, want ErrNotConnected", err)
	}
	if c.Connected() {
		t.Fatal("Write opened a producer connection; it must never dial")
	}
}

func TestHeartbeatSendsAConnectionHeartbeatAndReadsTheEcho(t *testing.T) {
	// Arrange
	c, server := pipedClient(t, testLog())
	served := make(chan error, 1)
	go func() {
		msg, err := wire.ReadAny(server)
		if err != nil {
			served <- err
			return
		}
		if _, ok := msg.(*protocolv1.ConnectionHeartbeat); !ok {
			served <- errors.New("expected ConnectionHeartbeat")
			return
		}
		served <- wire.WriteAny(server, &protocolv1.ConnectionHeartbeat{SentAtMs: 1})
	}()

	// Act
	err := c.Heartbeat()

	// Assert
	if err != nil {
		t.Fatalf("Heartbeat: %v", err)
	}
	if serveErr := <-served; serveErr != nil {
		t.Fatalf("serve heartbeat echo: %v", serveErr)
	}
}

func TestHeartbeatOnADownConnectionIsAnErrorNotSilence(t *testing.T) {
	// Arrange: never connected. A heartbeat exists to detect a dead link, so
	// answering "fine" here would hide the very outage it is asked about.
	c := New(filepath.Join(t.TempDir(), "nonexistent.sock"), testLog())

	// Act / Assert
	if err := c.Heartbeat(); !errors.Is(err, ErrNotConnected) {
		t.Fatalf("Heartbeat err = %v, want ErrNotConnected", err)
	}
}

func TestHealthAcceptsACorrelatedHealthyStatus(t *testing.T) {
	// Arrange
	c, server := pipedClient(t, testLog())
	served := make(chan error, 1)
	go func() {
		msg, err := wire.ReadAny(server)
		if err != nil {
			served <- err
			return
		}
		check, ok := msg.(*protocolv1.HealthCheck)
		if !ok {
			served <- errors.New("expected HealthCheck")
			return
		}
		served <- wire.WriteAny(server, &protocolv1.HealthStatus{RequestId: check.GetRequestId(), Healthy: true})
	}()

	// Act
	err := c.Health("sidecar-health-test")

	// Assert
	if err != nil {
		t.Fatalf("Health: %v", err)
	}
	if serveErr := <-served; serveErr != nil {
		t.Fatalf("serve HealthStatus: %v", serveErr)
	}
	if !c.Connected() {
		t.Fatal("a healthy store dropped the producer connection")
	}
}

func TestHealthOnADownConnectionIsAnErrorNotSilence(t *testing.T) {
	// Arrange: a health assertion cannot treat an absent producer connection as
	// healthy, because that would permit the shim to render a dead session.
	c := New(filepath.Join(t.TempDir(), "nonexistent.sock"), testLog())

	// Act / Assert
	if err := c.Health("health-down"); !errors.Is(err, ErrNotConnected) {
		t.Fatalf("Health err = %v, want ErrNotConnected", err)
	}
}

func TestHealthRequiresCorrelationID(t *testing.T) {
	// Arrange: an established connection, so the request-id invariant rather
	// than a missing transport is the error this test exercises.
	c, _ := pipedClient(t, testLog())

	// Act / Assert
	if err := c.Health(""); err == nil {
		t.Fatal("Health accepted an empty request_id")
	}
}

func TestHealthRejectsMismatchedResponseAndLogsContext(t *testing.T) {
	// Arrange
	var logs bytes.Buffer
	c, server := pipedClient(t, logging.New(&logs, &logs).With(logging.Context{Component: "test"}))
	served := make(chan error, 1)
	go func() {
		msg, err := wire.ReadAny(server)
		if err != nil {
			served <- err
			return
		}
		if _, ok := msg.(*protocolv1.HealthCheck); !ok {
			served <- errors.New("expected HealthCheck")
			return
		}
		served <- wire.WriteAny(server, &protocolv1.HealthStatus{
			RequestId: "wrong-request",
			Healthy:   true,
		})
	}()

	// Act
	err := c.Health("expected-request")

	// Assert
	if err == nil || !strings.Contains(err.Error(), "request_id") {
		t.Fatalf("Health err = %v, want request_id mismatch", err)
	}
	if c.Connected() {
		t.Fatal("mismatched HealthStatus left the producer connection established")
	}
	if serveErr := <-served; serveErr != nil {
		t.Fatalf("serve HealthStatus: %v", serveErr)
	}
	if strings.Contains(logs.String(), `"level":"error"`) || strings.Contains(logs.String(), "wrong-request") {
		t.Fatalf("storeclient globally logged caller-owned health mismatch: %q", logs.String())
	}
}

func TestHealthRejectsUnhealthyResponseAndLogsReason(t *testing.T) {
	// Arrange
	var logs bytes.Buffer
	c, server := pipedClient(t, logging.New(&logs, &logs).With(logging.Context{Component: "test"}))
	served := make(chan error, 1)
	go func() {
		if _, err := wire.ReadAny(server); err != nil {
			served <- err
			return
		}
		served <- wire.WriteAny(server, &protocolv1.HealthStatus{
			RequestId: "health-unhealthy",
			Healthy:   false,
			Reason:    "database unavailable",
		})
	}()

	// Act
	err := c.Health("health-unhealthy")

	// Assert
	if err == nil || !strings.Contains(err.Error(), "database unavailable") {
		t.Fatalf("Health err = %v, want store health reason", err)
	}
	if c.Connected() {
		t.Fatal("unhealthy HealthStatus left the producer connection established")
	}
	if serveErr := <-served; serveErr != nil {
		t.Fatalf("serve HealthStatus: %v", serveErr)
	}
	if strings.Contains(logs.String(), `"level":"error"`) || strings.Contains(logs.String(), "database unavailable") {
		t.Fatalf("storeclient globally logged caller-owned unhealthy response: %q", logs.String())
	}
}

func TestHealthRejectsAResponseThatIsNotAHealthStatus(t *testing.T) {
	// Arrange: a peer that answers a health probe with something else has not
	// asserted health, so the connection cannot be treated as proven.
	c, server := pipedClient(t, testLog())
	served := make(chan error, 1)
	go func() {
		if _, err := wire.ReadAny(server); err != nil {
			served <- err
			return
		}
		served <- wire.WriteAny(server, &protocolv1.ConnectionHeartbeat{SentAtMs: 1})
	}()

	// Act
	err := c.Health("health-wrong-type")

	// Assert
	if err == nil || !strings.Contains(err.Error(), "expected HealthStatus") {
		t.Fatalf("Health err = %v, want an expected-HealthStatus rejection", err)
	}
	if c.Connected() {
		t.Fatal("a non-HealthStatus reply left the producer connection established")
	}
	if serveErr := <-served; serveErr != nil {
		t.Fatalf("serve reply: %v", serveErr)
	}
}

func TestConnectEstablishesTheProducerConnectionWithoutAFrame(t *testing.T) {
	// Arrange: the store fixes a connection's role by its FIRST frame, so
	// Connect must send none.
	sock := shortSocket(t)
	served := serveOnce(t, sock, func(conn net.Conn) error {
		_, err := wire.ReadAny(conn)
		if err == nil {
			return errors.New("Connect sent a frame; the first Write must declare the role")
		}
		return nil
	})
	c := New(sock, testLog())

	// Act
	err := c.Connect()

	// Assert
	if err != nil {
		t.Fatalf("Connect: %v", err)
	}
	if !c.Connected() {
		t.Fatal("Connect did not establish the producer connection")
	}
	if err := c.Close(); err != nil {
		t.Fatalf("Close: %v", err)
	}
	if serveErr := <-served; serveErr != nil {
		t.Fatalf("serve connect: %v", serveErr)
	}
}
