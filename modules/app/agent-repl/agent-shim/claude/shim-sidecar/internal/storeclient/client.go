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
	"agentrepl/wire"
)

// ErrNotConnected is returned by every operation needing the producer
// connection while it is down. Callers distinguish it from a store REJECTION
// (which arrives on a healthy connection) to decide whether the link is lost.
var ErrNotConnected = errors.New("storeclient: no producer connection to the store")

// Logf is the loud-logging sink (§12).
type Logf = func(format string, args ...any)

// Client holds the (lazily-opened) producer connection to the store.
type Client struct {
	socket string
	log    Logf

	mu   sync.Mutex
	conn net.Conn
}

// New builds a Client for the store at socket.
func New(socket string, log Logf) *Client {
	if log == nil {
		log = func(string, ...any) {}
	}
	return &Client{socket: socket, log: log}
}

// Connect dials the producer connection. It is a no-op when one is already
// open, and the ONLY thing in this package that dials it.
//
// The store fixes a connection's role by its first frame, so no frame is sent
// here: the socket is merely established, and the first Write is what declares
// this a producer connection.
func (c *Client) Connect() error {
	c.mu.Lock()
	defer c.mu.Unlock()
	if c.conn != nil {
		return nil
	}
	conn, err := net.Dial("unix", c.socket)
	if err != nil {
		return fmt.Errorf("storeclient: dial %s: %w", c.socket, err)
	}
	c.conn = conn
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
		// A rejected batch is a loud, surfaced failure, not a silent drop.
		c.log("storeclient: store REJECTED batch (producer=%s): %s", producer, ack.GetError())
		return ack, fmt.Errorf("storeclient: batch rejected: %s", ack.GetError())
	}
	return ack, nil
}

// Heartbeat sends a liveness ping on the producer connection and waits for the
// store's echo. A down connection is ErrNotConnected rather than a silent
// no-op: the caller heartbeats precisely to learn the link is dead, so
// answering "fine" for a connection that does not exist would hide the outage
// this ping exists to find.
func (c *Client) Heartbeat() error {
	c.mu.Lock()
	defer c.mu.Unlock()
	if c.conn == nil {
		return ErrNotConnected
	}
	if err := wire.WriteAny(c.conn, &corev1.Heartbeat{SentAtMs: time.Now().UnixMilli()}); err != nil {
		c.dropConn()
		return fmt.Errorf("storeclient: sending Heartbeat: %w", err)
	}
	if _, err := wire.ReadAny(c.conn); err != nil {
		c.dropConn()
		return fmt.Errorf("storeclient: reading Heartbeat echo: %w", err)
	}
	return nil
}

// Close closes the producer connection.
func (c *Client) Close() error {
	c.mu.Lock()
	defer c.mu.Unlock()
	if c.conn != nil {
		err := c.conn.Close()
		c.conn = nil
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
