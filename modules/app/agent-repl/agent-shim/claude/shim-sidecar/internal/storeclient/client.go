// Package storeclient is the sidecar's UDS client to the shim-store: it recovers
// cursors at startup (CursorQuery → CursorList), writes StoreWrite batches over a
// long-lived producer connection (reading each StoreWriteAck), and heartbeats.
//
// Transport is the system-wide convention (agentrepl/wire WriteAny/ReadAny): a
// 4-byte length prefix wrapping a serialized google.protobuf.Any whose type_url
// discriminates the message. The store fixes a connection's role by its FIRST
// frame, so the producer connection opens with a StoreWrite; cursor recovery uses
// its own short-lived connection.
//
// Sad path (§4.4/§8/§12): a write that cannot reach the store returns an error —
// it is NEVER spilled or silently retried-forever here. The caller loud-logs the
// dropped batch and does NOT commit the tailer cursor, so the batch replays on
// recovery and the store's dedup absorbs the overlap.
package storeclient

import (
	"fmt"
	"net"
	"sync"
	"time"

	corev1 "agentrepl/proto/agentshim/core/v1"
	"agentrepl/wire"
)

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

// RecoverCursors asks the store for persisted cursors (§7.3). An empty fileID
// recovers all. It uses a dedicated short-lived connection (CursorQuery is its
// own connection role).
func (c *Client) RecoverCursors(fileID string) ([]*corev1.CursorState, error) {
	conn, err := net.Dial("unix", c.socket)
	if err != nil {
		return nil, fmt.Errorf("storeclient: dial %s: %w", c.socket, err)
	}
	defer conn.Close()
	if err := wire.WriteAny(conn, &corev1.CursorQuery{FileId: fileID}); err != nil {
		return nil, err
	}
	msg, err := wire.ReadAny(conn)
	if err != nil {
		return nil, fmt.Errorf("storeclient: reading CursorList: %w", err)
	}
	list, ok := msg.(*corev1.CursorList)
	if !ok {
		return nil, fmt.Errorf("storeclient: expected CursorList, got %T", msg)
	}
	return list.GetCursors(), nil
}

// Write sends one StoreWrite batch and returns the store's ack. It opens (or
// reopens) the producer connection as needed. On any transport error the
// connection is dropped so the next call redials; the error is returned to the
// caller (never swallowed).
func (c *Client) Write(producer string, batch *corev1.EventBatch) (*corev1.StoreWriteAck, error) {
	c.mu.Lock()
	defer c.mu.Unlock()
	conn, err := c.ensureConn()
	if err != nil {
		return nil, err
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
// store's echo. It is a no-op (nil) if the producer connection is not yet open.
func (c *Client) Heartbeat() error {
	c.mu.Lock()
	defer c.mu.Unlock()
	if c.conn == nil {
		return nil
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

// ensureConn returns the producer connection, dialing if needed. Caller holds mu.
func (c *Client) ensureConn() (net.Conn, error) {
	if c.conn != nil {
		return c.conn, nil
	}
	conn, err := net.Dial("unix", c.socket)
	if err != nil {
		return nil, fmt.Errorf("storeclient: dial %s: %w", c.socket, err)
	}
	c.conn = conn
	return conn, nil
}

// dropConn closes and clears the producer connection. Caller holds mu.
func (c *Client) dropConn() {
	if c.conn != nil {
		c.conn.Close()
		c.conn = nil
	}
}
