// Package storeclient is the sidecar's UDS client to the shim-store: it recovers
// cursors (GetSidecarCursorsRequest → GetSidecarCursorsResponse) and writes
// WriteBatchRequest batches over a long-lived producer connection.
//
// THE STORE IS NOW DECLARED AS A CONNECT SERVICE (store.v1 ShimStore) while this
// client still speaks the length-prefixed Any framing below. Only the MESSAGES
// are repointed here; moving the transport to Connect is an implementation
// decision, not a reconciliation, and is reported as a gap.
//
// THE WRITE IS STILL ONE-WAY HERE. store.v1 does define a WriteBatchResponse
// with success/failure arms — a successor the retired StoreWriteAck did not
// have — but reading it would change what this client puts on the wire, so the
// restoration is reported rather than taken unilaterally. See Write.
//
// Transport is the system-wide convention (agentrepl/wire WriteAny/ReadAny): a
// 4-byte length prefix wrapping a serialized google.protobuf.Any whose type_url
// discriminates the message. The store fixes a connection's role by its FIRST
// frame, so the producer connection opens with a StoreEntryWrite; cursor
// recovery uses its own short-lived connection.
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
// Sad path: a write that cannot reach the store returns an error — it is NEVER
// spilled or silently retried-forever here. The caller loud-logs the dropped
// batch and does NOT commit the tailer cursor, so the batch replays on recovery
// and the deterministic write_id on every record absorbs the overlap.
package storeclient

import (
	"errors"
	"fmt"
	"net"
	"sync"

	storev1 "agentrepl/proto/store/v1"
	"agentrepl/shim-claude-sidecar/internal/logging"
	"agentrepl/wire"
)

// ErrNoHealthProbe is returned by Health and Heartbeat. The redesigned contract
// deleted protocol.v1 HealthCheck/HealthStatus and ConnectionHeartbeat outright
// and store.v1 declares no liveness RPC in their place, so there is no frame
// this client can send to prove the store can parse and answer one.
//
// IT FAILS RATHER THAN ANSWERING "fine". The caller probes precisely to learn
// the link is dead; a probe that cannot be performed is not a healthy store, and
// reporting one would be exactly the silent degradation the probe exists to
// prevent.
var ErrNoHealthProbe = errors.New("storeclient: store.v1 declares no health or heartbeat RPC; the store link cannot be proven live")

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

// RecoveryState is the store's durable startup snapshot.
//
// IT NO LONGER CARRIES OPEN TASKS. agentshim.v1 OpenTaskState and the CursorList
// that delivered it were deleted, and store.v1 GetSidecarCursorsResponse returns
// cursors and nothing else — so the authoritative live-task set this sidecar
// restored its staleness tracker and spool-owner index from has no successor on
// the contract. See stale.Restore and the sidecar's seedOwners for the cost.
type RecoveryState struct {
	Cursors []*storev1.CursorState
}

// Recover asks the store for persisted startup state. An empty fileID recovers
// all cursors. It uses a dedicated short-lived connection.
func (c *Client) Recover(fileID string) (RecoveryState, error) {
	c.log.With(logging.Context{Operation: "storeclient-recover", StoreSocket: c.socket}).LogVerbose("recover requested file_id=%q", fileID)
	conn, err := net.Dial("unix", c.socket)
	if err != nil {
		return RecoveryState{}, fmt.Errorf("storeclient: dial %s: %w", c.socket, err)
	}
	defer conn.Close()
	request := &storev1.GetSidecarCursorsRequest{}
	if fileID != "" {
		request.FileId = &fileID
	}
	if err := wire.WriteAny(conn, request); err != nil {
		return RecoveryState{}, err
	}
	msg, err := wire.ReadAny(conn)
	if err != nil {
		return RecoveryState{}, fmt.Errorf("storeclient: reading GetSidecarCursorsResponse: %w", err)
	}
	response, ok := msg.(*storev1.GetSidecarCursorsResponse)
	if !ok {
		return RecoveryState{}, fmt.Errorf("storeclient: expected GetSidecarCursorsResponse, got %T", msg)
	}
	// A store that answers with a FAILURE is a rejection on a healthy
	// connection, and it is surfaced rather than read as an empty snapshot: an
	// empty cursor set is the cold-start path, and taking a refusal for one is
	// the silent re-read of every watched file this client exists to prevent.
	if failure := response.GetFailure(); failure != nil {
		return RecoveryState{}, fmt.Errorf("storeclient: store refused cursor recovery: %s", failure.GetDetail())
	}
	success := response.GetSuccess()
	if success == nil {
		return RecoveryState{}, fmt.Errorf("storeclient: GetSidecarCursorsResponse carries neither success nor failure")
	}
	c.log.With(logging.Context{Operation: "storeclient-recover", StoreSocket: c.socket}).LogVerbose("recovered file_id=%q cursors=%d", fileID, len(success.GetCursors()))
	return RecoveryState{Cursors: success.GetCursors()}, nil
}

// RecoverCursors returns only the cursor portion for callers that do not own
// task liveness.
func (c *Client) RecoverCursors(fileID string) ([]*storev1.CursorState, error) {
	recovery, err := c.Recover(fileID)
	if err != nil {
		return nil, err
	}
	return recovery.Cursors, nil
}

// Write sends one WriteBatchRequest batch. It NEVER dials: a down connection
// yields ErrNotConnected, because reopening one here would bypass the cursor
// recovery the link state machine performs on every connection. On a transport
// error the connection is dropped (so Connected goes false and the state
// machine redials); the error is returned to the caller, never swallowed.
//
// NO ACKNOWLEDGEMENT IS READ, AND THAT IS A LOSS THIS RECONCILIATION DOES NOT
// REPAIR ON ITS OWN. store.v1 now defines WriteBatchResponse with success and
// failure arms, so a successor to the retired StoreWriteAck EXISTS — but
// reading it changes what travels on this connection and when, which is an
// implementation decision. Until it is taken, two consequences hold, both
// recorded as gaps rather than papered over:
//
//   - A successful return now means "the batch reached the socket", not "the
//     store made it durable". The caller commits its cursor on that weaker
//     signal, so a store that accepted the bytes and then failed to persist them
//     loses those records silently. Nothing available here can detect it.
//   - A batch the store REJECTS is indistinguishable from one it accepted. The
//     rejection branch is not deleted because it looked unreachable — it is
//     unreachable because nothing here reads the response that would carry it.
//
// What still holds is the replay side of the contract: every record carries a
// deterministic `write_id`, so the batch a lost connection forces us to replay
// is a no-op at the store rather than a duplicate.
func (c *Client) Write(producer string, batch *storev1.EntryBatch) error {
	entryCount := 0
	if batch != nil {
		entryCount = len(batch.GetEntries())
	}
	c.log.With(logging.Context{Operation: "storeclient-write", StoreSocket: c.socket, Producer: producer}).
		LogVerbose("write requested entries=%d cursor_advance=%t", entryCount, batch != nil && batch.GetCursorAdvance() != nil)
	c.mu.Lock()
	defer c.mu.Unlock()
	conn := c.conn
	if conn == nil {
		return ErrNotConnected
	}
	if err := wire.WriteAny(conn, &storev1.WriteBatchRequest{Producer: producer, Batch: batch}); err != nil {
		c.dropConn()
		return fmt.Errorf("storeclient: sending WriteBatchRequest: %w", err)
	}
	c.log.With(logging.Context{Operation: "storeclient-write", StoreSocket: c.socket, Producer: producer}).
		LogVerbose("WriteBatchRequest sent entries=%d (no durability acknowledgement is read)", entryCount)
	return nil
}

// Heartbeat has NO FRAME LEFT TO SEND. protocol.v1 ConnectionHeartbeat was
// deleted and store.v1 declares no successor, so this cannot ping anything.
//
// It fails loudly rather than answering "fine" for a link it did not test — the
// caller heartbeats precisely to learn the link is dead. The down-connection
// check is kept ahead of the gap so a genuinely absent connection is still
// reported as ErrNotConnected rather than masked by the missing frame.
func (c *Client) Heartbeat() error {
	c.log.With(logging.Context{Operation: "storeclient-heartbeat", StoreSocket: c.socket}).LogVerbose("heartbeat requested")
	c.mu.Lock()
	defer c.mu.Unlock()
	if c.conn == nil {
		c.log.With(logging.Context{Operation: "storeclient-heartbeat", StoreSocket: c.socket, Level: "error"}).Log("heartbeat rejected because producer connection is down")
		return ErrNotConnected
	}
	c.log.With(logging.Context{Operation: "storeclient-heartbeat", StoreSocket: c.socket, Level: "error"}).
		Log("heartbeat cannot be sent: %v", ErrNoHealthProbe)
	return ErrNoHealthProbe
}

// Health has NO FRAME LEFT TO SEND EITHER. protocol.v1 HealthCheck and
// HealthStatus were deleted and store.v1 declares no probe RPC, so nothing here
// can prove the store parses and answers a protocol frame.
//
// EVERY GUARD AHEAD OF THE GAP IS KEPT, because each rejects a distinct caller
// error that still exists: a down connection is ErrNotConnected, and an empty
// request id is still refused. Only the probe itself is gone, and it reports
// that rather than passing.
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
	c.log.With(logging.Context{Operation: "storeclient-health", StoreSocket: c.socket, RequestID: requestID, Level: "error"}).
		Log("health check cannot be performed: %v", ErrNoHealthProbe)
	return ErrNoHealthProbe
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
