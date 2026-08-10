package frontend

// closecause.go — WHY a frontend connection ended, and the two records that owe
// an answer for it.
//
// The defect this file exists for: a connected Emacs host received its full
// 162-workspace snapshot and then had its socket closed by the daemon with
// NOTHING in the daemon log naming the close, and a webapp stream saw
// `close=1005` — the WebSocket code for "the peer never sent a close frame at
// all". Both are the same fault in two transports: `conn.close()` took no
// argument, so the reason a connection was given up on lived only in whichever
// call site happened to log before calling it, and nothing ever reached the
// wire.
//
// A closeCause makes the reason a VALUE that travels with the teardown. Every
// daemon-initiated close now carries one, `disconnect` records it on the client
// before the writer can observe `done`, the writer's teardown logs it, and the
// WebSocket transport sends it as a real close frame with a code and a reason
// string. A client that is dropped therefore learns what happened from the
// protocol, and an operator learns it from one greppable line.

import (
	"fmt"
	"time"

	"github.com/gorilla/websocket"
)

// closeFrameBudget bounds how long the WebSocket close handshake may hold the
// teardown. It is a FAILURE bound, not a wait: gorilla's WriteControl may be
// called concurrently with an in-flight WriteMessage, so the only way this
// budget is reached is a socket that is not accepting even a 125-byte control
// frame — in which case the close frame is unsendable and the record says so.
const closeFrameBudget = 2 * time.Second

// closeReasonMax is the WebSocket protocol's limit on a close frame's reason
// payload (125 bytes of control payload minus the 2-byte status code).
const closeReasonMax = 123

// Close reasons. Each names one TERMINATION PATH, not a family of them: an
// operator reading `cause=` must be able to go to exactly one place in this
// package. They are stable, lowercase, and greppable.
const (
	// closeReasonServerShutdown — Server.Close tore every connection down.
	closeReasonServerShutdown = "server_shutdown"
	// closeReasonServerClosed — the connection was accepted, but the server had
	// already closed before it could be registered.
	closeReasonServerClosed = "server_closed"
	// closeReasonSnapshotMarshal — the connect snapshot could not be marshalled,
	// so the connection can never be given the state it is defined by.
	closeReasonSnapshotMarshal = "connect_snapshot_marshal_failed"
	// closeReasonSnapshotRefused — an empty outbox refused the connect snapshot.
	// That is a programmer error, not a slow consumer.
	closeReasonSnapshotRefused = "connect_snapshot_refused"
	// closeReasonRosterMarshal — the retained roster could not be marshalled.
	closeReasonRosterMarshal = "connect_roster_marshal_failed"
	// closeReasonRosterRefused — a near-empty outbox refused the connect roster.
	closeReasonRosterRefused = "connect_roster_refused"
	// closeReasonLeaseMarshal — a GUI stream's snapshot lease could not be
	// marshalled, so its bounded freshness proof cannot be renewed.
	closeReasonLeaseMarshal = "snapshot_lease_marshal_failed"
	// closeReasonOverflow — the outbound queue refused a frame. The overflow
	// reason (hard_ceiling / stalled) and the phase ride in the detail.
	closeReasonOverflow = "outbound_overflow"
	// closeReasonWriteFailed — a write to the socket failed.
	closeReasonWriteFailed = "write_failed"
	// closeReasonPeerClosed — the client hung up cleanly (EOF or a WebSocket
	// close frame). The daemon did not initiate this one.
	closeReasonPeerClosed = "peer_closed"
	// closeReasonReadFailed — the inbound framing broke.
	closeReasonReadFailed = "read_failed"
	// closeReasonUnrecorded — the accountability backstop. A teardown that
	// reaches the transport without a recorded cause is a BUG in this package,
	// and it is reported as one rather than closing quietly.
	closeReasonUnrecorded = "unrecorded"
)

// closeCause is one connection teardown's reason, in the two forms the two
// audiences need: a greppable token plus detail for the log, and a WebSocket
// status code plus a short reason for the wire.
//
// The zero value is deliberately NOT a valid cause — see recorded — so a path
// that forgets to name its reason is detectable rather than indistinguishable
// from a normal shutdown.
type closeCause struct {
	reason string
	detail string
	code   int
}

// recorded reports whether this cause was actually set by a call site. It is
// what lets the transport teardown tell "the writer exited on a cause the
// disconnect recorded" from "something tore this connection down without ever
// saying why".
func (c closeCause) recorded() bool { return c.reason != "" }

// String is the log form: the token plus its detail when there is one.
func (c closeCause) String() string {
	if c.detail == "" {
		return c.reason
	}
	return c.reason + ": " + c.detail
}

// wireReason is the close frame's reason payload, truncated to what the
// protocol allows. Truncation is on BYTES because that is what the frame
// budgets; the reason tokens above are ASCII, so no rune is split.
func (c closeCause) wireReason() string {
	r := c.String()
	if len(r) > closeReasonMax {
		return r[:closeReasonMax]
	}
	return r
}

// causeUnrecorded is the backstop cause. It is never constructed by a
// termination path; reaching it means one did not name itself.
var causeUnrecorded = closeCause{
	reason: closeReasonUnrecorded,
	detail: "connection torn down without a recorded cause",
	code:   websocket.CloseInternalServerErr,
}

// causeServerShutdown is the whole-server teardown. CloseGoingAway is exactly
// what it means: the endpoint is going away, reconnect later.
var causeServerShutdown = closeCause{reason: closeReasonServerShutdown, code: websocket.CloseGoingAway}

// causeServerClosed is an accept that lost the race with Server.Close.
var causeServerClosed = closeCause{reason: closeReasonServerClosed, code: websocket.CloseGoingAway}

// causeInternal builds a cause for a daemon-side failure the client did
// nothing to provoke: a marshal that failed, a queue that refused a frame an
// empty queue must accept. CloseInternalServerErr says so on the wire.
func causeInternal(reason string, detail error) closeCause {
	c := closeCause{reason: reason, code: websocket.CloseInternalServerErr}
	if detail != nil {
		c.detail = detail.Error()
	}
	return c
}

// causeOverflow builds the SLOW-CONSUMER cause. CloseTryAgainLater is the
// protocol's "the server is dropping you for load reasons, come back": the
// client's reconnect replays a full snapshot, so nothing is lost by taking it.
//
// The detail carries both numbers an operator needs to tell a memory-bound
// eviction from a wedged one: which limit was hit, and how long the queue had
// gone without a single observable act of drain.
func causeOverflow(res pushResult, phase string) closeCause {
	return closeCause{
		reason: closeReasonOverflow,
		detail: fmt.Sprintf("limit=%s phase=%s depth=%d soft=%d hard=%d stalled_for_ms=%d",
			res.reason, phase, res.depth, res.soft, res.hard, res.stalledFor.Milliseconds()),
		code: websocket.CloseTryAgainLater,
	}
}

// causeWriteFailed is a socket that stopped accepting bytes. The close frame is
// almost certainly unsendable in this state, and the attempt's failure is
// itself reported rather than swallowed.
func causeWriteFailed(err error) closeCause {
	return closeCause{reason: closeReasonWriteFailed, detail: errText(err), code: websocket.CloseAbnormalClosure}
}

// causeInboundEnded classifies the read loop's exit. A clean EOF or a
// client-sent close frame is the CLIENT's decision and is recorded as such; any
// other error is a broken frame stream.
func causeInboundEnded(err error) closeCause {
	if isPeerClose(err) {
		return closeCause{reason: closeReasonPeerClosed, detail: errText(err), code: websocket.CloseNormalClosure}
	}
	return closeCause{reason: closeReasonReadFailed, detail: errText(err), code: websocket.CloseProtocolError}
}

func errText(err error) string {
	if err == nil {
		return ""
	}
	return err.Error()
}

// closeConn is the ONE place a serving connection's socket is closed.
//
// It writes the record first and closes second, so the log cannot be missing
// for a connection that is already gone, and it hands the cause to the
// transport so a WebSocket peer receives a real status code instead of the 1005
// "no close frame was ever sent" that a bare Close produces.
//
// cl may be nil for a connection abandoned before it was ever registered; the
// record then carries client_id=0 and the kind it was accepted as.
func (s *Server) closeConn(c conn, id uint64, kind ClientKind, cause closeCause) {
	if !cause.recorded() {
		// An unnamed teardown is the exact defect this file exists to make
		// impossible, so it is reported at WARN as the bug it is rather than
		// papered over with a plausible-looking reason.
		s.warn("frontend: connection closed WITHOUT A RECORDED CAUSE client_id=%d kind=%s — this is a bug in the frontend teardown paths", id, kind)
		cause = causeUnrecorded
	}
	s.logf("frontend: closing connection client_id=%d kind=%s cause=%s ws_close_code=%d",
		id, kind, cause.String(), cause.code)
	if err := c.close(cause); err != nil {
		s.logf("frontend: connection close client_id=%d kind=%s cause=%s: %v", id, kind, cause.reason, err)
	}
}
