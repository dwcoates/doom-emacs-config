// Package storeclient is the sidecar's UDS client to the shim-store: it recovers
// cursors plus authoritative open-task lifecycle state (CursorQuery →
// CursorList), writes StoreWrite batches over a long-lived producer connection
// (reading each StoreWriteAck), and heartbeats.
//
// Transport is the system-wide convention (agentrepl/wire WriteAny/ReadAny): a
// 4-byte length prefix wrapping a serialized google.protobuf.Any whose type_url
// discriminates the message. The store fixes a connection's role by its FIRST
// frame, so the producer connection opens with a StoreWrite; cursor recovery uses
// its own short-lived connection.
//
// THE CONNECTION IS NEVER OPENED IMPLICITLY. Connect is the only thing that
// dials the producer connection, and every operation that needs it fails with
// ErrNotConnected when it is down. That is deliberate and load-bearing: the
// sidecar's link state machine (link.go) makes cursor recovery the first act of
// every established connection, and a connection that sprang into existence
// under a Write would have skipped that recovery — which is exactly the silent
// cold start the state machine exists to make unreachable. Redialing is the
// state machine's job, not this client's.
//
// Sad path (§4.4/§8/§12): a write that cannot reach the store returns an error —
// it is NEVER spilled or silently retried-forever here. The caller loud-logs the
// dropped batch and does NOT commit the tailer cursor, so the batch replays on
// recovery and the store's dedup absorbs the overlap.
package storeclient

import (
	"errors"
	"fmt"
	"net"
	"sync"
	"time"

	corev1 "agentrepl/proto/agentshim/core/v1"
	"agentrepl/shim-claude-sidecar/internal/logging"
	"agentrepl/wire"
)

// ErrNotConnected is returned by every operation needing the producer
// connection while it is down. Callers distinguish it from a store REJECTION
// (which arrives on a healthy connection) to decide whether the link is lost.
var ErrNotConnected = errors.New("storeclient: no producer connection to the store")

// Client holds the (lazily-opened) producer connection to the store.
type Client struct {
	socket string
	log    *logging.Bound

	mu   sync.Mutex
	conn net.Conn
}

// New builds a Client for the store at socket.
func New(socket string, log *logging.Bound) *Client {
	log.With(logging.Context{Operation: "storeclient-new", StoreSocket: socket}).LogVerbose("constructing store client")
	return &Client{socket: socket, log: log}
}

// Connect dials the producer connection. It is a no-op when one is already
// open, and the ONLY thing in this package that dials it.
//
// The store fixes a connection's role by its first frame, so no frame is sent
// here: the socket is merely established, and the first Write is what declares
// this a producer connection.
func (c *Client) Connect() error {
	c.log.With(logging.Context{Operation: "storeclient-connect", StoreSocket: c.socket}).LogVerbose("connect requested")
	c.mu.Lock()
	defer c.mu.Unlock()
	if c.conn != nil {
		c.log.With(logging.Context{Operation: "storeclient-connect", StoreSocket: c.socket}).LogVerbose("producer connection already established")
		return nil
	}
	conn, err := net.Dial("unix", c.socket)
	if err != nil {
		return fmt.Errorf("storeclient: dial %s: %w", c.socket, err)
	}
	c.conn = conn
	c.log.With(logging.Context{Operation: "storeclient-connect", StoreSocket: c.socket}).Log("producer connection established")
	return nil
}

// Connected reports whether the producer connection is currently established.
// It goes false the moment a transport failure drops the connection, which is
// how the caller tells a dead link from a store that merely rejected a batch.
func (c *Client) Connected() bool {
	c.mu.Lock()
	defer c.mu.Unlock()
	return c.conn != nil
}

// RecoveryState is the store's durable startup snapshot. OpenTasks is the
// authoritative live-task set: artifact existence is not lifecycle evidence.
type RecoveryState struct {
	Cursors   []*corev1.CursorState
	OpenTasks []*corev1.OpenTaskState
}

// Recover asks the store for persisted startup state (§7.3). An empty fileID
// recovers all cursors plus authoritative open tasks. It uses a dedicated
// short-lived connection (CursorQuery is its own connection role).
func (c *Client) Recover(fileID string) (RecoveryState, error) {
	c.log.With(logging.Context{Operation: "storeclient-recover", StoreSocket: c.socket}).LogVerbose("recover requested file_id=%q", fileID)
	conn, err := net.Dial("unix", c.socket)
	if err != nil {
		return RecoveryState{}, fmt.Errorf("storeclient: dial %s: %w", c.socket, err)
	}
	defer conn.Close()
	if err := wire.WriteAny(conn, &corev1.CursorQuery{FileId: fileID}); err != nil {
		return RecoveryState{}, err
	}
	msg, err := wire.ReadAny(conn)
	if err != nil {
		return RecoveryState{}, fmt.Errorf("storeclient: reading CursorList: %w", err)
	}
	list, ok := msg.(*corev1.CursorList)
	if !ok {
		return RecoveryState{}, fmt.Errorf("storeclient: expected CursorList, got %T", msg)
	}
	if fileID == "" && !list.GetOpenTasksAuthoritative() {
		return RecoveryState{}, fmt.Errorf("storeclient: CursorList lacks authoritative open-task state; refusing startup against an incompatible store")
	}
	c.log.With(logging.Context{Operation: "storeclient-recover", StoreSocket: c.socket}).LogVerbose("recovered file_id=%q cursors=%d open_tasks=%d authoritative=%t", fileID, len(list.GetCursors()), len(list.GetOpenTasks()), list.GetOpenTasksAuthoritative())
	return RecoveryState{
		Cursors:   list.GetCursors(),
		OpenTasks: list.GetOpenTasks(),
	}, nil
}

// RecoverCursors returns only the cursor portion for callers that do not own
// task liveness.
func (c *Client) RecoverCursors(fileID string) ([]*corev1.CursorState, error) {
	recovery, err := c.Recover(fileID)
	if err != nil {
		return nil, err
	}
	return recovery.Cursors, nil
}

// Write sends one StoreWrite batch and returns the store's ack. It NEVER dials:
// a down connection yields ErrNotConnected, because reopening one here would
// bypass the cursor recovery the link state machine performs on every
// connection. On any transport error the connection is dropped (so Connected
// goes false and the state machine redials); the error is returned to the
// caller, never swallowed.
func (c *Client) Write(producer string, batch *corev1.EventBatch) (*corev1.StoreWriteAck, error) {
	eventCount := 0
	if batch != nil {
		eventCount = len(batch.GetEvents())
	}
	c.log.With(logging.Context{Operation: "storeclient-write", StoreSocket: c.socket, Producer: producer}).LogVerbose("write requested events=%d cursor_advance=%t", eventCount, batch != nil && batch.GetCursorAdvance() != nil)
	c.mu.Lock()
	defer c.mu.Unlock()
	conn := c.conn
	if conn == nil {
		return nil, ErrNotConnected
	}
	if err := wire.WriteAny(conn, &corev1.StoreWrite{Producer: producer, Batch: batch}); err != nil {
		c.dropConn()
		return nil, fmt.Errorf("storeclient: sending StoreWrite: %w", err)
	}
	msg, err := wire.ReadAny(conn)
	if err != nil {
		c.dropConn()
		return nil, fmt.Errorf("storeclient: reading StoreWriteAck: %w", err)
	}
	ack, ok := msg.(*corev1.StoreWriteAck)
	if !ok {
		c.dropConn()
		return nil, fmt.Errorf("storeclient: expected StoreWriteAck, got %T", msg)
	}
	if ack.GetError() != "" {
		// The caller owns the diagnostic because it alone knows whether this
		// batch belongs to a Claude session. Logging here would leak a
		// session-owned rejection into the global sidecar file.
		return ack, fmt.Errorf("storeclient: batch rejected: %s", ack.GetError())
	}
	c.log.With(logging.Context{Operation: "storeclient-write", StoreSocket: c.socket, Producer: producer}).LogVerbose("StoreWrite acknowledged events=%d", eventCount)
	return ack, nil
}

// Heartbeat sends a liveness ping on the producer connection and waits for the
// store's echo. A down connection is ErrNotConnected rather than a silent
// no-op: the caller heartbeats precisely to learn the link is dead, so
// answering "fine" for a connection that does not exist would hide the outage
// this ping exists to find.
func (c *Client) Heartbeat() error {
	c.log.With(logging.Context{Operation: "storeclient-heartbeat", StoreSocket: c.socket}).LogVerbose("heartbeat requested")
	c.mu.Lock()
	defer c.mu.Unlock()
	if c.conn == nil {
		c.log.With(logging.Context{Operation: "storeclient-heartbeat", StoreSocket: c.socket, Level: "error"}).Log("heartbeat rejected because producer connection is down")
		return ErrNotConnected
	}
	if err := wire.WriteAny(c.conn, &corev1.Heartbeat{SentAtMs: time.Now().UnixMilli()}); err != nil {
		c.dropConn()
		c.log.With(logging.Context{Operation: "storeclient-heartbeat", StoreSocket: c.socket, Level: "error"}).Log("Heartbeat send failed: %v", err)
		return fmt.Errorf("storeclient: sending Heartbeat: %w", err)
	}
	if _, err := wire.ReadAny(c.conn); err != nil {
		c.dropConn()
		c.log.With(logging.Context{Operation: "storeclient-heartbeat", StoreSocket: c.socket, Level: "error"}).Log("heartbeat echo read failed: %v", err)
		return fmt.Errorf("storeclient: reading Heartbeat echo: %w", err)
	}
	return nil
}

// Health sends a correlated health probe over the recovered producer
// connection.  Connected only means a file descriptor exists; this method
// proves the store can parse and answer a protocol frame.  A failed assertion
// drops the connection so link.go repeats its mandatory recovery before this
// sidecar reads another file.
func (c *Client) Health(requestID string) error {
	c.log.With(logging.Context{Operation: "storeclient-health", StoreSocket: c.socket, RequestID: requestID}).LogVerbose("health requested")
	c.mu.Lock()
	defer c.mu.Unlock()
	if c.conn == nil {
		return ErrNotConnected
	}
	if requestID == "" {
		c.log.With(logging.Context{Operation: "storeclient-health", StoreSocket: c.socket, Level: "error"}).Log("health rejected because request_id is empty")
		return errors.New("storeclient: health check requires request_id")
	}
	if err := wire.WriteAny(c.conn, &corev1.HealthCheck{RequestId: requestID}); err != nil {
		c.dropConn()
		return fmt.Errorf("storeclient: sending HealthCheck: %w", err)
	}
	msg, err := wire.ReadAny(c.conn)
	if err != nil {
		c.dropConn()
		return fmt.Errorf("storeclient: reading HealthStatus: %w", err)
	}
	status, ok := msg.(*corev1.HealthStatus)
	if !ok {
		c.dropConn()
		return fmt.Errorf("storeclient: expected HealthStatus, got %T", msg)
	}
	if status.GetRequestId() != requestID {
		c.dropConn()
		return fmt.Errorf("storeclient: HealthStatus request_id=%q, want %q", status.GetRequestId(), requestID)
	}
	if !status.GetHealthy() {
		c.dropConn()
		return fmt.Errorf("storeclient: store health failed: %s", status.GetReason())
	}
	c.log.With(logging.Context{Operation: "storeclient-health", StoreSocket: c.socket, RequestID: requestID}).LogVerbose("store reported healthy")
	return nil
}

// Close closes the producer connection.
func (c *Client) Close() error {
	c.log.With(logging.Context{Operation: "storeclient-close", StoreSocket: c.socket}).LogVerbose("close requested")
	c.mu.Lock()
	defer c.mu.Unlock()
	if c.conn != nil {
		err := c.conn.Close()
		c.conn = nil
		if err != nil {
			c.log.With(logging.Context{Operation: "storeclient-close", StoreSocket: c.socket, Level: "error"}).Log("producer close failed: %v", err)
		} else {
			c.log.With(logging.Context{Operation: "storeclient-close", StoreSocket: c.socket}).Log("producer connection closed")
		}
		return err
	}
	return nil
}

// dropConn closes and clears the producer connection. Caller holds mu.
func (c *Client) dropConn() {
	if c.conn != nil {
		c.conn.Close()
		c.conn = nil
	}
}
