// Package server is the shim-store UDS front end: it accepts producer and
// subscriber connections, ingests StoreWrite batches through the db layer, and
// serves Subscribe replay-then-live-tail subscriptions via the fanout.
//
// Socket protocol (the system-wide convention every agent-shim UDS hop uses).
// Transport is UDS with `agentrepl/wire` framing: a 4-byte big-endian length
// prefix followed by exactly one serialized google.protobuf.Any. The Any wraps
// the actual message (StoreWrite, StoreWriteAck, Subscribe, Heartbeat,
// core.v1.Event for subscription delivery, ...) and its type_url is THE message
// discriminator, resolved against the proto registry. Both halves of that
// envelope live in `agentrepl/wire` (WriteAny / ReadAny), so this server, the
// sidecar's store client, and the daemon cannot drift; the TS shim speaks the
// same convention.
//
// Connection roles follow from the first frame's wrapped message:
//
//   - StoreWrite → PRODUCER connection: the store ingests the batch and replies
//     with one StoreWriteAck frame, then loops (further StoreWrite frames each
//     get an ack; Heartbeat frames get a Heartbeat reply).
//   - Heartbeat → PRODUCER-PREAMBLE connection: the store echoes heartbeats
//     until the first StoreWrite declares the producer. This keeps an idle
//     sidecar link alive after startup recovery but before any file changes.
//   - HealthCheck → PRODUCER-PREAMBLE connection: the store returns the
//     correlated HealthStatus, then continues to await the first StoreWrite.
//     This lets a recovered idle sidecar prove store health without losing its
//     producer connection.
//   - Subscribe → SUBSCRIBER connection: the store replays persisted events with
//     seq > from_seq, then live-tails Event frames until the client disconnects
//     or falls behind.
package server

import (
	"errors"
	"fmt"
	"io"
	"net"
	"os"
	"sync"
	"time"

	corev1 "agentrepl/proto/agentshim/core/v1"
	"agentrepl/shim-store/internal/db"
	"agentrepl/shim-store/internal/logging"
	"agentrepl/wire"
)

// Server serves the shim-store protocol over a UDS listener.
type Server struct {
	db  *db.DB
	fan *fanout
	log *logging.Logger

	mu     sync.Mutex
	ln     net.Listener
	conns  map[net.Conn]struct{}
	closed bool
	wg     sync.WaitGroup
}

// New builds a Server over an open db. buffer<=0 uses the default fanout buffer.
func New(database *db.DB, log *logging.Logger, buffer int) *Server {
	if database == nil || log == nil {
		panic("shim-store server: nil database or logger")
	}
	return &Server{
		db:    database,
		fan:   newFanout(buffer, log),
		log:   log,
		conns: make(map[net.Conn]struct{}),
	}
}

// Listen removes any stale socket file and opens a UDS listener at path.
func Listen(path string, log *logging.Logger) (net.Listener, error) {
	if log == nil {
		panic("shim-store server: nil logger")
	}
	log.LogVerbose(logging.Fields{Operation: "listen", Socket: path}, "opening UDS listener")
	if err := os.Remove(path); err != nil && !errors.Is(err, os.ErrNotExist) {
		log.Log(logging.Fields{Operation: "remove-stale-socket", Socket: path, Level: "error"}, "removing stale socket failed: %v", err)
		return nil, fmt.Errorf("shim-store server: removing stale socket %q: %w", path, err)
	} else if err == nil {
		log.Log(logging.Fields{Operation: "remove-stale-socket", Socket: path}, "removed stale UDS socket")
	} else {
		log.LogVerbose(logging.Fields{Operation: "remove-stale-socket", Socket: path}, "no stale UDS socket present")
	}
	ln, err := net.Listen("unix", path)
	if err != nil {
		log.Log(logging.Fields{Operation: "listen", Socket: path, Level: "error"}, "opening UDS listener failed: %v", err)
		return nil, fmt.Errorf("shim-store server: listening on %q: %w", path, err)
	}
	log.Log(logging.Fields{Operation: "listen", Socket: path}, "UDS listener ready")
	return ln, nil
}

// Serve accepts connections until the listener is closed (via Close). It
// blocks; run it in its own goroutine.
func (s *Server) Serve(ln net.Listener) error {
	s.log.Log(logging.Fields{Operation: "serve"}, "accept loop starting listener=%s", listenerName(ln))
	s.mu.Lock()
	if s.closed {
		s.mu.Unlock()
		return errors.New("shim-store server: Serve after Close")
	}
	s.ln = ln
	s.mu.Unlock()

	for {
		conn, err := ln.Accept()
		if err != nil {
			s.mu.Lock()
			closed := s.closed
			s.mu.Unlock()
			if closed {
				s.log.Log(logging.Fields{Operation: "serve"}, "accept loop stopped by server close")
				return nil
			}
			return fmt.Errorf("shim-store server: accept: %w", err)
		}
		s.log.LogVerbose(logging.Fields{Operation: "accept", Subscriber: conn.RemoteAddr().String()}, "accepted UDS connection")
		s.trackConn(conn)
		s.wg.Add(1)
		go func() {
			defer s.wg.Done()
			s.handleConn(conn)
		}()
	}
}

// Close stops accepting, closes all live connections, and waits for handlers.
func (s *Server) Close() error {
	s.log.Log(logging.Fields{Operation: "close"}, "server shutdown requested")
	s.mu.Lock()
	if s.closed {
		s.mu.Unlock()
		s.log.LogVerbose(logging.Fields{Operation: "close"}, "server already closed")
		return nil
	}
	s.closed = true
	ln := s.ln
	conns := make([]net.Conn, 0, len(s.conns))
	for c := range s.conns {
		conns = append(conns, c)
	}
	s.mu.Unlock()

	var closeErrs []error
	if ln != nil {
		if err := ln.Close(); err != nil {
			s.log.Log(logging.Fields{Operation: "close-listener", Level: "error"}, "closing UDS listener failed: %v", err)
			closeErrs = append(closeErrs, fmt.Errorf("closing UDS listener: %w", err))
		}
	}
	for _, c := range conns {
		if err := c.Close(); err != nil {
			s.log.Log(logging.Fields{Operation: "close-connection", Subscriber: c.RemoteAddr().String(), Level: "error"}, "closing UDS connection failed: %v", err)
			closeErrs = append(closeErrs, fmt.Errorf("closing UDS connection %s: %w", c.RemoteAddr(), err))
		}
	}
	s.wg.Wait()
	if err := errors.Join(closeErrs...); err != nil {
		return err
	}
	s.log.Log(logging.Fields{Operation: "close"}, "server shutdown complete connections=%d", len(conns))
	return nil
}

func (s *Server) trackConn(c net.Conn) {
	s.mu.Lock()
	s.conns[c] = struct{}{}
	s.mu.Unlock()
}

func (s *Server) untrackConn(c net.Conn) {
	s.mu.Lock()
	delete(s.conns, c)
	s.mu.Unlock()
}

func (s *Server) handleConn(conn net.Conn) {
	defer conn.Close()
	defer s.untrackConn(conn)
	peer := conn.RemoteAddr().String()
	s.log.LogVerbose(logging.Fields{Operation: "connection", Subscriber: peer}, "reading initial protocol frame")

	msg, err := wire.ReadAny(conn)
	if err != nil {
		if !errors.Is(err, io.EOF) {
			s.log.Log(logging.Fields{Operation: "read-first-frame", Subscriber: peer, Level: "error"}, "protocol frame read failed: %v", err)
		} else {
			s.log.LogVerbose(logging.Fields{Operation: "read-first-frame", Subscriber: peer}, "connection closed before initial frame")
		}
		return
	}
	switch m := msg.(type) {
	case *corev1.StoreWrite:
		s.log.Log(logging.Fields{Operation: "classify-connection", Producer: m.GetProducer(), Subscriber: peer}, "classified producer connection")
		s.serveProducer(conn, m)
	case *corev1.Heartbeat:
		if err := s.echoHeartbeat(conn, m); err != nil {
			return
		}
		s.log.Log(logging.Fields{Operation: "producer-preamble", Subscriber: peer}, "connection opened with heartbeat; awaiting first StoreWrite")
		s.serveProducerPreamble(conn)
	case *corev1.Subscribe:
		s.log.Log(logging.Fields{Operation: "classify-connection", Session: m.GetSessionId(), Subscriber: peer}, "classified subscriber connection from_seq=%d", m.GetFromSeq())
		s.serveSubscriber(conn, m)
	case *corev1.CursorQuery:
		s.log.Log(logging.Fields{Operation: "classify-connection", Subscriber: peer}, "classified cursor query file_id=%q", m.GetFileId())
		s.serveCursorQuery(conn, m)
	case *corev1.HealthCheck:
		s.serveHealth(conn, m)
		s.log.Log(logging.Fields{Operation: "producer-preamble", Subscriber: peer, RequestID: m.GetRequestId()}, "connection opened with health check; awaiting first StoreWrite")
		s.serveProducerPreamble(conn)
	default:
		s.log.Log(logging.Fields{Operation: "classify-connection", Subscriber: peer, Level: "error"},
			"protocol frame is %T; expected StoreWrite, Heartbeat, Subscribe, CursorQuery, or HealthCheck", m)
	}
}

// serveHealth proves that the store is accepting framed protocol traffic after
// its database-backed server has been constructed.  A socket file alone can be
// stale or merely listening; only this correlated response is health.
func (s *Server) serveHealth(conn net.Conn, check *corev1.HealthCheck) {
	s.log.LogVerbose(logging.Fields{Operation: "health", Subscriber: conn.RemoteAddr().String(), RequestID: check.GetRequestId()}, "processing health check")
	if check.GetRequestId() == "" {
		s.log.Log(logging.Fields{Operation: "health", Subscriber: conn.RemoteAddr().String(), Level: "error"}, "health check rejected: empty request_id")
		return
	}
	status := &corev1.HealthStatus{
		RequestId: check.GetRequestId(),
		Healthy:   true,
		Component: "shim-store",
	}
	if err := wire.WriteAny(conn, status); err != nil {
		s.log.Log(logging.Fields{Operation: "health-reply", Subscriber: conn.RemoteAddr().String(), RequestID: check.GetRequestId(), Level: "error"}, "health reply failed: %v", err)
		return
	}
	s.log.Log(logging.Fields{Operation: "health", Subscriber: conn.RemoteAddr().String(), RequestID: check.GetRequestId()}, "health PASS")
}

// serveCursorQuery answers a sidecar's startup cursor-recovery request (§7.3):
// an empty file_id returns all persisted cursors, a set file_id returns just
// that one (or an empty list when absent). One CursorList reply, then the
// connection is done.
func (s *Server) serveCursorQuery(conn net.Conn, q *corev1.CursorQuery) {
	peer := conn.RemoteAddr().String()
	s.log.LogVerbose(logging.Fields{Operation: "cursor-query", Subscriber: peer}, "processing cursor query file_id=%q", q.GetFileId())
	var cursors []*corev1.CursorState
	var openTasks []*corev1.OpenTaskState
	if id := q.GetFileId(); id != "" {
		c, err := s.db.Cursor(id)
		if err != nil {
			return
		}
		if c != nil {
			cursors = append(cursors, c)
		}
	} else {
		all, err := s.db.Cursors()
		if err != nil {
			return
		}
		cursors = all
		openTasks, err = s.db.OpenTasks()
		if err != nil {
			return
		}
	}
	s.log.Log(logging.Fields{Operation: "cursor-query", Subscriber: peer},
		"startup recovery snapshot: cursors=%d open_tasks=%d file_id=%q", len(cursors), len(openTasks), q.GetFileId())
	if err := wire.WriteAny(conn, &corev1.CursorList{
		Cursors:                cursors,
		OpenTasks:              openTasks,
		OpenTasksAuthoritative: q.GetFileId() == "",
	}); err != nil {
		s.log.Log(logging.Fields{Operation: "cursor-query-reply", Subscriber: peer, Level: "error"}, "protocol cursor reply write failed: %v", err)
	}
}

// ---- producer side --------------------------------------------------------

// serveProducerPreamble keeps a recovered-but-idle producer connection alive
// until its first StoreWrite identifies the producer. A sidecar can legitimately
// have no event to write for hours after startup, so requiring a write before
// its first heartbeat turns healthy idleness into a reconnect loop.
func (s *Server) serveProducerPreamble(conn net.Conn) {
	peer := conn.RemoteAddr().String()
	s.log.LogVerbose(logging.Fields{Operation: "producer-preamble", Subscriber: peer}, "awaiting producer declaration")
	for {
		msg, err := wire.ReadAny(conn)
		if err != nil {
			if !errors.Is(err, io.EOF) {
				s.log.Log(logging.Fields{Operation: "producer-preamble-read", Subscriber: peer, Level: "error"}, "connection dropped: %v", err)
			} else {
				s.log.LogVerbose(logging.Fields{Operation: "producer-preamble-read", Subscriber: peer}, "producer preamble closed cleanly")
			}
			return
		}
		switch m := msg.(type) {
		case *corev1.StoreWrite:
			s.log.Log(logging.Fields{Operation: "producer-preamble", Producer: m.GetProducer(), Subscriber: peer}, "producer declared by StoreWrite")
			s.serveProducer(conn, m)
			return
		case *corev1.Heartbeat:
			if err := s.echoHeartbeat(conn, m); err != nil {
				return
			}
		case *corev1.HealthCheck:
			s.serveHealth(conn, m)
		default:
			s.log.Log(logging.Fields{Operation: "producer-preamble-read", Subscriber: peer, Level: "error"}, "unrecognized frame %T; disconnecting", m)
			return
		}
	}
}

func (s *Server) serveProducer(conn net.Conn, first *corev1.StoreWrite) {
	peer := conn.RemoteAddr().String()
	producer := first.GetProducer()
	s.log.LogVerbose(logging.Fields{Operation: "producer", Producer: producer, Subscriber: peer}, "serving producer connection")
	if err := s.processWrite(conn, first); err != nil {
		return
	}
	for {
		msg, err := wire.ReadAny(conn)
		if err != nil {
			if !errors.Is(err, io.EOF) {
				s.log.Log(logging.Fields{Operation: "producer-read", Producer: producer, Subscriber: peer, Level: "error"}, "protocol producer frame read failed: %v", err)
			} else {
				s.log.LogVerbose(logging.Fields{Operation: "producer-read", Producer: producer, Subscriber: peer}, "producer connection closed cleanly")
			}
			return
		}
		switch m := msg.(type) {
		case *corev1.StoreWrite:
			if err := s.processWrite(conn, m); err != nil {
				return
			}
		case *corev1.Heartbeat:
			if err := s.echoHeartbeat(conn, m); err != nil {
				return
			}
		case *corev1.HealthCheck:
			s.serveHealth(conn, m)
		default:
			s.log.Log(logging.Fields{Operation: "producer-read", Producer: producer, Subscriber: peer, Level: "error"}, "protocol frame is %T; disconnecting producer", m)
			return
		}
	}
}

func (s *Server) echoHeartbeat(conn net.Conn, heartbeat *corev1.Heartbeat) error {
	if err := wire.WriteAny(conn, &corev1.Heartbeat{SentAtMs: heartbeat.GetSentAtMs()}); err != nil {
		s.log.Log(logging.Fields{Operation: "heartbeat-reply", Subscriber: conn.RemoteAddr().String(), Level: "error"}, "protocol heartbeat reply failed sent_at_ms=%d: %v", heartbeat.GetSentAtMs(), err)
		return err
	}
	s.log.LogVerbose(logging.Fields{Operation: "heartbeat-reply", Subscriber: conn.RemoteAddr().String()}, "echoed heartbeat sent_at_ms=%d", heartbeat.GetSentAtMs())
	return nil
}

// processWrite ingests one batch and fans out its events, then acks. A rejected
// batch acks with a non-empty error and a loud log; it is never silently
// dropped.
func (s *Server) processWrite(conn net.Conn, sw *corev1.StoreWrite) error {
	events := sw.GetBatch().GetEvents()
	ack, durable := s.ingestAndFan(sw)
	if durable {
		s.log.LogVerbose(logging.Fields{Operation: "store-write", Producer: sw.GetProducer(), Subscriber: conn.RemoteAddr().String()}, "processing StoreWrite events=%d cursor_advance=%t", len(events), sw.GetBatch().GetCursorAdvance() != nil)
	}
	if err := wire.WriteAny(conn, ack); err != nil {
		s.log.Log(logging.Fields{Operation: "store-write-ack", Producer: sw.GetProducer(), Subscriber: conn.RemoteAddr().String(), Level: "error"}, "protocol StoreWriteAck failed accepted=%d deduped=%d last_seq=%d rejected=%t: %v", ack.GetAccepted(), ack.GetDeduped(), ack.GetLastSeq(), ack.GetError() != "", err)
		return err
	}
	if durable {
		s.log.LogVerbose(logging.Fields{Operation: "store-write-ack", Producer: sw.GetProducer(), Subscriber: conn.RemoteAddr().String()}, "StoreWriteAck sent accepted=%d deduped=%d last_seq=%d rejected=%t", ack.GetAccepted(), ack.GetDeduped(), ack.GetLastSeq(), ack.GetError() != "")
	}
	return nil
}

func (s *Server) ingestAndFan(sw *corev1.StoreWrite) (*corev1.StoreWriteAck, bool) {
	batch := sw.GetBatch()
	events := batch.GetEvents()

	// Split ephemeral out: they never touch the DB but still fan out in
	// arrival position (§4.3, §6.5).
	persistent := make([]*corev1.Event, 0, len(events))
	for _, ev := range events {
		if ev.GetClass() != corev1.EventClass_EVENT_CLASS_EPHEMERAL {
			persistent = append(persistent, ev)
		}
	}
	if len(persistent) == 0 && batch.GetCursorAdvance() == nil {
		// EPHEMERAL batches are a hot live-tail path. They neither persist nor
		// change a cursor, so a store log would bury the durable outcomes the
		// store owns without adding diagnostic value.
		for _, ev := range events {
			s.fan.publish(ev)
		}
		return &corev1.StoreWriteAck{}, false
	}
	s.log.LogVerbose(logging.Fields{Operation: "ingest-classify", Producer: sw.GetProducer()}, "classified batch total_events=%d persistent_events=%d ephemeral_events=%d", len(events), len(persistent), len(events)-len(persistent))

	start := time.Now()
	res, err := s.db.Ingest(sw.GetProducer(), persistent, batch.GetCursorAdvance())
	ingestMs := time.Since(start).Milliseconds()
	if err != nil {
		return &corev1.StoreWriteAck{Error: err.Error()}, true
	}

	// Fan out in arrival order. Ingest stamped accepted persistent events with
	// seq>0 and reset deduped ones to seq==0; deduped losers are already
	// durable and were delivered by the first writer, so we skip them.
	for _, ev := range events {
		if ev.GetClass() == corev1.EventClass_EVENT_CLASS_EPHEMERAL {
			s.fan.publish(ev)
			continue
		}
		if ev.GetSeq() > 0 {
			s.fan.publish(ev)
		}
	}

	// One line per persisted batch, so the store hop of the prompt round trip is
	// visible end to end. Ephemeral-only batches (and heartbeats) stay silent;
	// this line also carries what the old dedup-only line reported.
	if len(persistent) > 0 {
		s.log.Log(logging.Fields{
			Operation: "ingest", Producer: sw.GetProducer(), Session: persistent[0].GetSessionId(),
		}, "persisted batch events=%d accepted=%d deduped=%d last_seq=%d ingest_ms=%d",
			len(persistent), res.Accepted, res.Deduped, res.LastSeq, ingestMs)
	}
	return &corev1.StoreWriteAck{Accepted: res.Accepted, Deduped: res.Deduped, LastSeq: res.LastSeq}, true
}

// ---- subscriber side ------------------------------------------------------

func (s *Server) serveSubscriber(conn net.Conn, sub *corev1.Subscribe) {
	sessionID := sub.GetSessionId()
	peer := conn.RemoteAddr().String()
	s.log.LogVerbose(logging.Fields{Operation: "subscribe", Session: sessionID, Subscriber: peer}, "starting replay-then-tail from_seq=%d", sub.GetFromSeq())
	if sessionID == "" {
		s.log.Log(logging.Fields{Operation: "subscribe", Subscriber: peer, Level: "error"}, "protocol subscription rejected: empty session_id")
		return
	}

	subr := s.fan.subscribe(sessionID)
	defer s.fan.unsubscribe(subr)

	// When the subscriber is dropped (slow-consumer disconnect via the fanout,
	// or normal teardown), close the conn so a write blocked on a stuck socket
	// is unblocked and the tail loop returns promptly.
	go func() {
		<-subr.done
		conn.Close()
	}()

	// Register (above) BEFORE replay so live events arriving during replay are
	// buffered, then de-overlapped by seq afterwards.
	replayed, err := s.db.ReplayFrom(sessionID, sub.GetFromSeq())
	if err != nil {
		return
	}
	var lastReplaySeq uint64
	for _, ev := range replayed {
		if err := wire.WriteAny(conn, ev); err != nil {
			s.log.Log(logging.Fields{Operation: "subscribe-replay", Session: sessionID, Subscriber: peer, Level: "error"}, "protocol replay write failed seq=%d: %v", ev.GetSeq(), err)
			return
		}
		if ev.GetSeq() > lastReplaySeq {
			lastReplaySeq = ev.GetSeq()
		}
	}
	s.log.Log(logging.Fields{Operation: "subscribe-replay", Session: sessionID, Subscriber: peer}, "replay completed events=%d last_seq=%d", len(replayed), lastReplaySeq)

	// Detect client disconnect / drain client heartbeats without ever writing
	// from a second goroutine (the tail loop below owns all writes).
	go s.subReadLoop(conn, subr)

	for {
		select {
		case <-subr.done:
			s.log.LogVerbose(logging.Fields{Operation: "subscribe-tail", Session: sessionID, Subscriber: peer}, "live tail stopped")
			return
		case ev := <-subr.ch:
			// Skip persistent events already covered by replay (overlap window).
			if ev.GetClass() != corev1.EventClass_EVENT_CLASS_EPHEMERAL &&
				ev.GetSeq() > 0 && ev.GetSeq() <= lastReplaySeq {
				s.log.LogVerbose(logging.Fields{Operation: "subscribe-tail", Session: sessionID, Subscriber: peer}, "skipped replay overlap seq=%d", ev.GetSeq())
				continue
			}
			if err := wire.WriteAny(conn, ev); err != nil {
				s.log.Log(logging.Fields{Operation: "subscribe-tail", Session: sessionID, Subscriber: peer, Level: "error"}, "protocol live-tail write failed seq=%d class=%s: %v", ev.GetSeq(), ev.GetClass(), err)
				return
			}
			// Successful live delivery is a per-event hot path which can fire
			// hundreds of times per second. Subscription transitions and write
			// failures retain the useful store-owned diagnostics.
		}
	}
}

// subReadLoop reads (and discards, apart from close detection) frames from a
// subscriber connection so a client close unblocks the tail loop.
func (s *Server) subReadLoop(conn net.Conn, subr *subscriber) {
	for {
		if _, err := wire.ReadAny(conn); err != nil {
			if errors.Is(err, io.EOF) {
				s.log.LogVerbose(logging.Fields{Operation: "subscriber-read", Session: subr.sessionID, Subscriber: conn.RemoteAddr().String()}, "subscriber closed cleanly")
			} else {
				s.log.Log(logging.Fields{Operation: "subscriber-read", Session: subr.sessionID, Subscriber: conn.RemoteAddr().String(), Level: "error"}, "protocol subscriber frame read failed: %v", err)
			}
			subr.close()
			return
		}
	}
}

func listenerName(ln net.Listener) string {
	if ln == nil {
		return "<nil>"
	}
	return ln.Addr().String()
}

// ---- Any framing ----------------------------------------------------------
//
// The encode/decode pair lives in agentrepl/wire (WriteAny / ReadAny). It used
// to be copy-pasted here and in three other packages; one wire contract with
// four hand-maintained copies is the drift that package exists to prevent.
// ReadAny still returns ReadFrame's error VERBATIM, which is what lets the
// handlers below tell a clean io.EOF close from a fault.
