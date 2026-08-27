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

	storev1 "agentrepl/proto/store/v1"
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

func TestRecoverReturnsCursors(t *testing.T) {
	// Arrange
	sock := shortSocket(t)
	served := serveOnce(t, sock, func(conn net.Conn) error {
		msg, err := wire.ReadAny(conn)
		if err != nil {
			return err
		}
		if _, ok := msg.(*storev1.GetSidecarCursorsRequest); !ok {
			return errors.New("expected GetSidecarCursorsRequest")
		}
		return wire.WriteAny(conn, &storev1.GetSidecarCursorsResponse{
			Result: &storev1.GetSidecarCursorsResponse_Success{Success: &storev1.GetSidecarCursorsSuccess{
				Cursors: []*storev1.CursorState{{FileId: "7:7", Path: "/p/s1.jsonl", Offset: 99}},
			}},
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
	if serveErr := <-served; serveErr != nil {
		t.Fatalf("serve recovery response: %v", serveErr)
	}
}

// A store that REFUSES recovery must not be read as a store with no cursors:
// an empty cursor set is the honest cold-start path, and taking a refusal for
// one re-reads every watched file from offset 0.
func TestRecoverSurfacesAFailureRatherThanReadingItAsAColdStart(t *testing.T) {
	// Arrange
	sock := shortSocket(t)
	served := serveOnce(t, sock, func(conn net.Conn) error {
		if _, err := wire.ReadAny(conn); err != nil {
			return err
		}
		return wire.WriteAny(conn, &storev1.GetSidecarCursorsResponse{
			Result: &storev1.GetSidecarCursorsResponse_Failure{Failure: &storev1.GetSidecarCursorsFailure{
				Detail: "cursor table is locked",
			}},
		})
	})

	// Act
	recovery, err := New(sock, testLog()).Recover("")

	// Assert
	if err == nil {
		t.Fatal("Recover read a store refusal as a cold start")
	}
	if len(recovery.Cursors) != 0 {
		t.Fatalf("a refused recovery returned %d cursor(s)", len(recovery.Cursors))
	}
	if serveErr := <-served; serveErr != nil {
		t.Fatalf("serve recovery response: %v", serveErr)
	}
}

// A response carrying NEITHER arm is a store this client cannot read, and it is
// refused rather than treated as an empty success.
func TestRecoverRejectsAResponseWithNoResultArm(t *testing.T) {
	// Arrange
	sock := shortSocket(t)
	served := serveOnce(t, sock, func(conn net.Conn) error {
		if _, err := wire.ReadAny(conn); err != nil {
			return err
		}
		return wire.WriteAny(conn, &storev1.GetSidecarCursorsResponse{})
	})

	// Act
	_, err := New(sock, testLog()).Recover("")

	// Assert
	if err == nil {
		t.Fatal("Recover accepted a GetSidecarCursorsResponse with no result arm")
	}
	if serveErr := <-served; serveErr != nil {
		t.Fatalf("serve recovery response: %v", serveErr)
	}
}

func TestWriteSendsAWriteBatchRequestAndDoesNotWaitForAnAck(t *testing.T) {
	// Arrange — this client does not read WriteBatchResponse, so the write is
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
		write, ok := msg.(*storev1.WriteBatchRequest)
		if !ok {
			served <- errors.New("expected WriteBatchRequest")
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
	err := c.Write("shim-claude-sidecar", &storev1.EntryBatch{
		Entries:       []*storev1.StoreEntry{{WriteId: "w1"}},
		CursorAdvance: &storev1.CursorState{FileId: "7:7", Path: "/p/s1.jsonl", Offset: 99, Carry: []byte("z")},
	})

	// Assert
	if err != nil {
		t.Fatalf("Write: %v", err)
	}
	if serveErr := <-served; serveErr != nil {
		t.Fatalf("serve WriteBatchRequest: %v", serveErr)
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
	err := c.Write("shim-claude-sidecar", &storev1.EntryBatch{})

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
	err := c.Write("shim-claude-sidecar", &storev1.EntryBatch{})

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
	err = c.Write("shim-claude-sidecar", &storev1.EntryBatch{})

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
	c := New(filepath.Join(t.TempDir(), "nonexistent.sock"), testLog())

	// Act / Assert
	if err := c.Heartbeat(); !errors.Is(err, ErrNotConnected) {
		t.Fatalf("Heartbeat err = %v, want ErrNotConnected", err)
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

// A probe that CANNOT BE PERFORMED is not a healthy store. store.v1 declares no
// health or heartbeat RPC, so both report the gap rather than passing — the one
// answer that would let ingestion keep running against a link nothing tested.
func TestHealthOnAConnectedStoreReportsTheMissingProbeRatherThanPassing(t *testing.T) {
	// Arrange: an established connection, so only the missing frame is at issue.
	c, _ := pipedClient(t, testLog())

	// Act / Assert
	if err := c.Health("sidecar-health-test"); !errors.Is(err, ErrNoHealthProbe) {
		t.Fatalf("Health err = %v, want ErrNoHealthProbe", err)
	}
}

func TestHeartbeatOnAConnectedStoreReportsTheMissingProbeRatherThanPassing(t *testing.T) {
	// Arrange: an established connection, so only the missing frame is at issue.
	c, _ := pipedClient(t, testLog())

	// Act / Assert
	if err := c.Heartbeat(); !errors.Is(err, ErrNoHealthProbe) {
		t.Fatalf("Heartbeat err = %v, want ErrNoHealthProbe", err)
	}
}
