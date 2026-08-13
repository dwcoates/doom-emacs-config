// Package server is the shim-store UDS front end: it accepts producer and
// subscriber connections, ingests StoreEntryWrite batches through the db layer,
// and serves Subscribe replay-then-live-tail subscriptions via the fanout.
//
// THE WRITE HALF AND THE READ HALF SPEAK DIFFERENT SURFACES, on purpose. A
// producer writes `agentshim.v1.StoreEntryWrite`, carrying whole `Entry`
// records — both halves, including the observation plane and anything the
// producer could not convert. A subscriber receives `protocol.v1.EntryDelivery`,
// carrying only the external half. The store is one of the three runtimes
// entitled to the internal half, and nothing it serves carries it.
//
// Socket protocol (the system-wide convention every agent-shim UDS hop uses).
// Transport is UDS with `agentrepl/wire` framing: a 4-byte big-endian length
// prefix followed by exactly one serialized google.protobuf.Any. The Any wraps
// the actual message (StoreEntryWrite, Subscribe, ConnectionHeartbeat,
// EntryDelivery for subscription delivery, ...) and its type_url is THE message
// discriminator, resolved against the proto registry. Both halves of that
// envelope live in `agentrepl/wire` (WriteAny / ReadAny), so this server, the
// sidecar's store client, and the daemon cannot drift; the TS shim speaks the
// same convention.
//
// Connection roles follow from the first frame's wrapped message:
//
//   - StoreEntryWrite → PRODUCER connection: the store ingests the batch and
//     sends NOTHING BACK, then loops. There is no ack message on any surface
//     (see processWrite), so this direction is write-only; a rejected batch
//     ends the connection because that is the only signal left.
//   - ConnectionHeartbeat → PRODUCER-PREAMBLE connection: the store echoes
//     heartbeats until the first StoreEntryWrite declares the producer. This
//     keeps an idle sidecar link alive after startup recovery but before any
//     file changes.
//   - HealthCheck → PRODUCER-PREAMBLE connection: the store returns the
//     correlated HealthStatus, then continues to await the first
//     StoreEntryWrite. This lets a recovered idle sidecar prove store health
//     without losing its producer connection.
//   - Subscribe → SUBSCRIBER connection: the store replays persisted records
//     with seq > from_seq, then live-tails EntryDelivery frames until the
//     client disconnects or falls behind.
package server

import (
	"context"
	"errors"
	"fmt"
	"io"
	"net"
	"os"
	"sync"
	"sync/atomic"
	"syscall"
	"time"

	agentshimv1 "agentrepl/proto/agentshim/v1"
	protocolv1 "agentrepl/proto/protocol/v1"
	"agentrepl/shim-store/internal/db"
	"agentrepl/shim-store/internal/logging"
	"agentrepl/wire"
)

// Server serves the shim-store protocol over a UDS listener.
type Server struct {
	db  *db.DB
	fan *fanout
	log *logging.Logger

	mu    sync.Mutex
	ln    net.Listener
	conns map[net.Conn]struct{}
	// subscribers records connection-owned terminal state.  Server.Close uses
	// this map instead of closing a subscriber socket directly, making a
	// shutdown terminal cause structurally unavoidable for every registered
	// subscription.
	subscribers map[net.Conn]*subscriptionTerminal
	closed      bool
	wg          sync.WaitGroup

	subscriberHooks   subscriberHooks
	subscriberHooksMu sync.RWMutex

	// ingestMu serializes the whole ASSIGN-THEN-ANNOUNCE region of
	// ingestAndFan, so a session's fan-out order is its seq order.
	//
	// It is DELIBERATELY NOT `mu`. That one guards the listener/conns/closed
	// lifecycle, and a batch ingest holding it would block every accept and
	// close for the duration of a SQLite transaction; the two critical sections
	// share nothing and must not be conflated.
	//
	// WHY IT IS NEEDED. Seq assignment is already totally ordered — db.Ingest
	// runs under BEGIN IMMEDIATE (see internal/db/db.go), which serializes every
	// writer globally. The PUBLISH was not: it ran after the transaction, on the
	// producer's own goroutine, holding nothing. A session can have two
	// concurrent producers (the shim's stream plane and the sidecar's file
	// plane), so two goroutines
	// could commit as 1043-then-1044 and publish as 1044-then-1043. The daemon
	// reads a non-increasing seq as a terminal protocol violation and kills the
	// session — observed twice on 2026-07-29, both mid-turn.
	//
	// MUTUAL EXCLUSION, not a narrowed race window: while one batch holds this,
	// no other batch can be between its own commit and its own publish, so the
	// inversion is UNREPRESENTABLE rather than merely unlikely.
	//
	// It is nearly free for the same reason it is correct: BEGIN IMMEDIATE
	// already serialized the expensive half, so the only contention this adds
	// covers a loop of non-blocking channel sends (fanout.publish).
	//
	// STORE-WIDE rather than per-session, because one batch may span sessions
	// (db.Ingest's per-session seq map), and a per-session scheme would need to
	// hold several locks per batch with the lock-ordering hazard that implies.
	ingestMu sync.Mutex
}

// New builds a Server over an open db. buffer<=0 uses the default fanout buffer.
func New(database *db.DB, log *logging.Logger, buffer int) *Server {
	if database == nil || log == nil {
		panic("shim-store server: nil database or logger")
	}
	return &Server{
		db:          database,
		fan:         newFanout(buffer, log),
		log:         log,
		conns:       make(map[net.Conn]struct{}),
		subscribers: make(map[net.Conn]*subscriptionTerminal),
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
		conn = &onceConn{Conn: conn}
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
	subscribers := make([]*subscriptionTerminal, 0, len(s.subscribers))
	for c := range s.conns {
		if terminal := s.subscribers[c]; terminal != nil {
			subscribers = append(subscribers, terminal)
		} else {
			conns = append(conns, c)
		}
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
	for _, terminal := range subscribers {
		terminal.terminate("server", subscriptionTerminalServerShutdown, nil)
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
	delete(s.subscribers, c)
	s.mu.Unlock()
}

// registerSubscriberTerminal assigns the only terminal owner before replay
// begins.  Close either finds the owner in subscribers or sees no registered
// subscriber yet; it can never directly close a registered subscriber socket.
func (s *Server) registerSubscriberTerminal(conn net.Conn, terminal *subscriptionTerminal) bool {
	s.mu.Lock()
	_, tracked := s.conns[conn]
	if !tracked {
		s.mu.Unlock()
		panic("shim-store server: registering untracked subscriber connection")
	}
	if s.closed {
		s.mu.Unlock()
		terminal.terminate("server", subscriptionTerminalServerShutdown, nil)
		return false
	}
	s.subscribers[conn] = terminal
	s.mu.Unlock()
	return true
}

func (s *Server) unregisterSubscriberTerminal(conn net.Conn, terminal *subscriptionTerminal) {
	s.mu.Lock()
	if current := s.subscribers[conn]; current == terminal {
		delete(s.subscribers, conn)
	}
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
	case *agentshimv1.StoreEntryWrite:
		s.log.Log(logging.Fields{Operation: "classify-connection", Producer: m.GetProducer(), Subscriber: peer}, "classified producer connection")
		s.serveProducer(conn, m)
	case *protocolv1.ConnectionHeartbeat:
		if err := s.echoHeartbeat(conn, m); err != nil {
			return
		}
		s.log.Log(logging.Fields{Operation: "producer-preamble", Subscriber: peer}, "connection opened with heartbeat; awaiting first StoreEntryWrite")
		s.serveProducerPreamble(conn)
	case *protocolv1.Subscribe:
		s.log.Log(logging.Fields{Operation: "classify-connection", Session: m.GetSessionId(), Subscriber: peer}, "classified subscriber connection from_seq=%d", m.GetFromSeq())
		s.serveSubscriber(conn, m)
	case *agentshimv1.CursorQuery:
		s.log.Log(logging.Fields{Operation: "classify-connection", Subscriber: peer}, "classified cursor query file_id=%q", m.GetFileId())
		s.serveCursorQuery(conn, m)
	case *protocolv1.MessagePageRequest:
		s.log.Log(logging.Fields{Operation: "classify-connection", Session: m.GetSessionId(), Subscriber: peer, RequestID: m.GetRequestId()},
			"classified message page request")
		s.serveMessagePage(conn, m)
	case *protocolv1.HealthCheck:
		s.serveHealth(conn, m)
		s.log.Log(logging.Fields{Operation: "producer-preamble", Subscriber: peer, RequestID: m.GetRequestId()}, "connection opened with health check; awaiting first StoreEntryWrite")
		s.serveProducerPreamble(conn)
	default:
		s.log.Log(logging.Fields{Operation: "classify-connection", Subscriber: peer, Level: "error"},
			"protocol frame is %T; expected StoreEntryWrite, ConnectionHeartbeat, Subscribe, CursorQuery, MessagePageRequest, or HealthCheck", m)
	}
}

// serveHealth proves that the store is accepting framed protocol traffic after
// its database-backed server has been constructed.  A socket file alone can be
// stale or merely listening; only this correlated response is health.
func (s *Server) serveHealth(conn net.Conn, check *protocolv1.HealthCheck) {
	s.log.LogVerbose(logging.Fields{Operation: "health", Subscriber: conn.RemoteAddr().String(), RequestID: check.GetRequestId()}, "processing health check")
	if check.GetRequestId() == "" {
		s.log.Log(logging.Fields{Operation: "health", Subscriber: conn.RemoteAddr().String(), Level: "error"}, "health check rejected: empty request_id")
		return
	}
	status := &protocolv1.HealthStatus{
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

// serveCursorQuery answers a sidecar's startup cursor-recovery request: an
// empty file_id returns all persisted cursors, a set file_id returns just that
// one (or an empty list when absent). One CursorList reply, then the connection
// is done.
//
// OPEN TASKS ARE NO LONGER ANSWERABLE, and the reply says so rather than
// implying otherwise. `OpenTaskState.started` carried the `TaskStarted` event
// that opened the task and is retired with the `Event` layer, so the message
// can state when a task was last active but not WHICH task it is. The store
// also has nothing left to derive one from: the `task_id` column was extracted
// from the retired task payloads, and detached work is modelled as messages
// now, with no task-scoped envelope column.
//
// `open_tasks_authoritative` is therefore FALSE on every reply. That field
// exists exactly to distinguish an empty set from a store that cannot compute
// one, and inventing identity-less entries to fill the list would hand the
// sidecar a set it cannot reconcile against anything. Recorded as a gap.
func (s *Server) serveCursorQuery(conn net.Conn, q *agentshimv1.CursorQuery) {
	peer := conn.RemoteAddr().String()
	s.log.LogVerbose(logging.Fields{Operation: "cursor-query", Subscriber: peer}, "processing cursor query file_id=%q", q.GetFileId())
	var cursors []*agentshimv1.CursorState
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
		s.log.Log(logging.Fields{Operation: "cursor-query", Subscriber: peer, Level: "warn"},
			"open-task recovery is UNANSWERABLE: OpenTaskState carries no task identity since OpenTaskState.started was retired, so open_tasks is empty and open_tasks_authoritative is false")
	}
	s.log.Log(logging.Fields{Operation: "cursor-query", Subscriber: peer},
		"startup recovery snapshot: cursors=%d open_tasks=0 open_tasks_authoritative=false file_id=%q", len(cursors), q.GetFileId())
	if err := wire.WriteAny(conn, &agentshimv1.CursorList{
		Cursors:                cursors,
		OpenTasksAuthoritative: false,
	}); err != nil {
		s.log.Log(logging.Fields{Operation: "cursor-query-reply", Subscriber: peer, Level: "error"}, "protocol cursor reply write failed: %v", err)
	}
}

// serveMessagePage answers one MessagePageRequest with exactly one MessagePage
// frame and then the connection is done.
//
// ONE REQUEST, ONE BOUNDED REPLY, never a stream. This is the read Subscribe
// and ReplayRequest cannot express: both walk FORWARD from a lower bound, so a
// reader wanting recent history had to guess a from_seq low enough to cover it
// — and since one message owns arbitrarily many records that guess cannot be
// computed. Here the caller names no position at all on a cold open, and names
// only a seq the store itself minted on a continuation.
//
// This door is ADDITIVE. Subscribe and ReplayRequest keep working exactly as
// they did: the bounded tail read has to demonstrably work before the old ones
// close, and closing them first would trade a slow feed for an empty one.
func (s *Server) serveMessagePage(conn net.Conn, req *protocolv1.MessagePageRequest) {
	peer := conn.RemoteAddr().String()
	fields := logging.Fields{Operation: "message-page", Session: req.GetSessionId(), Subscriber: peer, RequestID: req.GetRequestId()}
	s.log.LogVerbose(fields, "processing message page request")
	page, err := s.db.MessagePage(context.Background(), req)
	if err != nil {
		// The db layer already logged this error with its own context; the
		// caller is answered with silence-then-close rather than a
		// half-truthful page, because a page missing messages is
		// indistinguishable from the top of the conversation.
		s.log.Log(logging.Fields{Operation: "message-page", Session: req.GetSessionId(), Subscriber: peer, RequestID: req.GetRequestId(), Level: "error"},
			"message page refused: %v", err)
		return
	}
	if err := wire.WriteAny(conn, page); err != nil {
		s.log.Log(logging.Fields{Operation: "message-page-reply", Session: req.GetSessionId(), Subscriber: peer, RequestID: req.GetRequestId(), Level: "error"},
			"protocol message page reply write failed: %v", err)
		return
	}
	s.log.Log(fields, "message page delivered last_page_seq=%d", page.GetLastPageSeq())
}

// ---- producer side --------------------------------------------------------

// serveProducerPreamble keeps a recovered-but-idle producer connection alive
// until its first StoreEntryWrite identifies the producer. A sidecar can legitimately
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
		case *agentshimv1.StoreEntryWrite:
			s.log.Log(logging.Fields{Operation: "producer-preamble", Producer: m.GetProducer(), Subscriber: peer}, "producer declared by StoreEntryWrite")
			s.serveProducer(conn, m)
			return
		case *protocolv1.ConnectionHeartbeat:
			if err := s.echoHeartbeat(conn, m); err != nil {
				return
			}
		case *protocolv1.HealthCheck:
			s.serveHealth(conn, m)
		default:
			s.log.Log(logging.Fields{Operation: "producer-preamble-read", Subscriber: peer, Level: "error"}, "unrecognized frame %T; disconnecting", m)
			return
		}
	}
}

func (s *Server) serveProducer(conn net.Conn, first *agentshimv1.StoreEntryWrite) {
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
		case *agentshimv1.StoreEntryWrite:
			if err := s.processWrite(conn, m); err != nil {
				return
			}
		case *protocolv1.ConnectionHeartbeat:
			if err := s.echoHeartbeat(conn, m); err != nil {
				return
			}
		case *protocolv1.HealthCheck:
			s.serveHealth(conn, m)
		default:
			s.log.Log(logging.Fields{Operation: "producer-read", Producer: producer, Subscriber: peer, Level: "error"}, "protocol frame is %T; disconnecting producer", m)
			return
		}
	}
}

func (s *Server) echoHeartbeat(conn net.Conn, heartbeat *protocolv1.ConnectionHeartbeat) error {
	if err := wire.WriteAny(conn, &protocolv1.ConnectionHeartbeat{SentAtMs: heartbeat.GetSentAtMs()}); err != nil {
		s.log.Log(logging.Fields{Operation: "heartbeat-reply", Subscriber: conn.RemoteAddr().String(), Level: "error"}, "protocol heartbeat reply failed sent_at_ms=%d: %v", heartbeat.GetSentAtMs(), err)
		return err
	}
	s.log.LogVerbose(logging.Fields{Operation: "heartbeat-reply", Subscriber: conn.RemoteAddr().String()}, "echoed heartbeat sent_at_ms=%d", heartbeat.GetSentAtMs())
	return nil
}

// processWrite ingests one batch and fans out its records.
//
// THERE IS NO ACK, AND NOTHING REPLACES ONE. `StoreWriteAck` was retired with
// the `Event` layer and `StoreEntryWrite` has no reply message on any surface,
// so the store cannot tell a producer how many records it accepted, which were
// replayed, what seq the batch reached, or that the batch was rejected. No
// substitute message is invented here; the loss is recorded as a gap.
//
// A REJECTED BATCH THEREFORE ENDS THE CONNECTION. That is the only channel
// left: silence and success are indistinguishable on a write-only stream, so a
// producer whose batch was refused would otherwise go on writing into a store
// that discarded it. Dropping the connection is a signal the producer can
// actually observe, and the refusal is loud-logged with its cause here first.
// It is not a substitute for an ack and does not carry one's information.
func (s *Server) processWrite(conn net.Conn, write *agentshimv1.StoreEntryWrite) error {
	peer := conn.RemoteAddr().String()
	batch := write.GetBatch()
	res, err := s.ingestAndFan(write)
	if err != nil {
		s.log.Log(logging.Fields{Operation: "store-write", Producer: write.GetProducer(), Subscriber: peer, Level: "error"},
			"StoreEntryWrite REJECTED entries=%d cursor_advance=%t — the producer cannot be told (StoreEntryWrite has no ack), so the connection is dropped instead: %v",
			len(batch.GetEntries()), batch.GetCursorAdvance() != nil, err)
		return err
	}
	s.log.LogVerbose(logging.Fields{Operation: "store-write", Producer: write.GetProducer(), Subscriber: peer},
		"StoreEntryWrite processed entries=%d accepted=%d replayed=%d unconverted=%d last_seq=%d cursor_advance=%t",
		len(batch.GetEntries()), res.Accepted, res.Replayed, res.Unconverted, res.LastSeq, batch.GetCursorAdvance() != nil)
	return nil
}

// ingestAndFan persists one batch and announces what it persisted.
func (s *Server) ingestAndFan(write *agentshimv1.StoreEntryWrite) (db.Result, error) {
	batch := write.GetBatch()
	entries := batch.GetEntries()

	if len(entries) == 0 && batch.GetCursorAdvance() == nil {
		// Nothing to persist and no cursor to advance. There is no live-only
		// class of record any more — every entry in an EntryBatch is a durable
		// write — so an empty batch is simply a no-op rather than the hot
		// ephemeral fan-out path this branch used to serve.
		s.log.LogVerbose(logging.Fields{Operation: "ingest-classify", Producer: write.GetProducer()}, "empty batch carries neither entries nor a cursor advance")
		return db.Result{}, nil
	}
	s.log.LogVerbose(logging.Fields{Operation: "ingest-classify", Producer: write.GetProducer()}, "classified batch entries=%d cursor_advance=%t", len(entries), batch.GetCursorAdvance() != nil)

	// ASSIGN THEN ANNOUNCE, as one indivisible step (see Server.ingestMu). The
	// lock opens here rather than after the Ingest because it is the ORDER of
	// the two that must hold: a publish that overtakes an earlier batch's
	// publish is exactly the seq inversion the daemon reads as fatal.
	s.ingestMu.Lock()
	start := time.Now()
	res, err := s.db.Ingest(write.GetProducer(), batch)
	ingestMs := time.Since(start).Milliseconds()
	if err != nil {
		// The db layer already logged this with its own context. Only the
		// unlock is done here, so a rejection cannot wedge every later write
		// behind a held lock.
		s.ingestMu.Unlock()
		return res, err
	}

	// Fan out in arrival order. Ingest returns exactly the deliveries to
	// announce: a replayed write produces none, because the original write
	// already fanned that record out and re-fanning it would turn an idempotent
	// store write into a duplicate DELIVERY. An unconverted record produces
	// none either — it has no external half, so there is nothing to deliver.
	for _, delivery := range res.Deliveries {
		s.fan.publish(delivery)
	}
	// The announce is complete, so the ordering guarantee is discharged. The log
	// below is deliberately outside: it reports what already happened and no
	// other batch's correctness depends on it.
	s.ingestMu.Unlock()

	// Successful persisted batches are high-frequency session narration rather
	// than lifecycle or failure evidence. Keep their detailed outcome available
	// in verbose mode without growing the normal global service log.
	s.log.LogVerbose(logging.Fields{
		Operation: "ingest", Producer: write.GetProducer(), Session: batchSession(res),
	}, "persisted batch entries=%d accepted=%d replayed=%d unconverted=%d last_seq=%d ingest_ms=%d",
		len(entries), res.Accepted, res.Replayed, res.Unconverted, res.LastSeq, ingestMs)
	// A REPLAY is a normal-log fact, not narration: it says a producer resent a
	// batch whose outcome it never learned, and that the write identity held. It
	// is rare by construction (one store bounce per deploy), so it never floods.
	if res.Replayed > 0 {
		s.log.Log(logging.Fields{
			Operation: "ingest", Producer: write.GetProducer(), Session: batchSession(res),
		}, "REPLAYED batch absorbed idempotently entries=%d accepted=%d replayed=%d — the producer resent writes whose outcome it never learned, and the (session_id, write_id) identity made them no-ops instead of duplicate rows",
			len(entries), res.Accepted, res.Replayed)
	}
	return res, nil
}

// batchSession names the session a batch's log record is attributed to.
//
// Empty when the batch accepted nothing positioned — a pure cursor advance, or
// a batch of unconverted records, neither of which belongs to a session the
// store can name.
func batchSession(res db.Result) string {
	if len(res.Deliveries) == 0 {
		return ""
	}
	return deliverySession(res.Deliveries[0])
}

// ---- subscriber side ------------------------------------------------------

// subscriptionTerminalReason is the sole classification of a subscriber
// connection's ending.  A candidate is accepted exactly once by its
// subscriptionTerminal, which owns cancellation, deregistration, socket close,
// and the final canonical lifecycle record.
type subscriptionTerminalReason string

const (
	subscriptionTerminalClientEOF        subscriptionTerminalReason = "client-eof"
	subscriptionTerminalClientReset      subscriptionTerminalReason = "client-reset"
	subscriptionTerminalSlowConsumer     subscriptionTerminalReason = "slow-consumer"
	subscriptionTerminalServerShutdown   subscriptionTerminalReason = "server-shutdown"
	subscriptionTerminalReplayFailure    subscriptionTerminalReason = "replay-failure"
	subscriptionTerminalReadinessFailure subscriptionTerminalReason = "readiness-failure"
	subscriptionTerminalTransportFailure subscriptionTerminalReason = "transport-failure"
)

// subscriberHooks supplies deterministic lifecycle observations for focused
// tests. Production leaves every hook nil.
type subscriberHooks struct {
	beforeReplayRow func()
	beforeTailWrite func()
	onTerminal      func(subscriberTerminalRecord)
}

type subscriberTerminalRecord struct {
	Owner          string
	Reason         subscriptionTerminalReason
	SessionID      string
	Peer           string
	FromSeq        uint64
	Delivered      uint64
	FirstReplaySeq uint64
	LastReplaySeq  uint64
	Cause          error
}

type subscriptionTerminal struct {
	once       sync.Once
	terminated atomic.Bool

	conn       net.Conn
	fan        *fanout
	subscriber *subscriber
	cancel     context.CancelFunc
	log        *logging.Logger
	hooks      subscriberHooks

	sessionID string
	peer      string
	fromSeq   uint64
	started   time.Time

	mu             sync.Mutex
	delivered      uint64
	firstReplaySeq uint64
	lastReplaySeq  uint64
}

func newSubscriptionTerminal(conn net.Conn, fan *fanout, log *logging.Logger, sessionID string, fromSeq uint64, cancel context.CancelFunc, hooks subscriberHooks) *subscriptionTerminal {
	if conn == nil || fan == nil || log == nil || cancel == nil {
		panic("shim-store server: invalid subscription terminal dependencies")
	}
	return &subscriptionTerminal{
		conn: conn, fan: fan, cancel: cancel, log: log, hooks: hooks,
		sessionID: sessionID, peer: conn.RemoteAddr().String(), fromSeq: fromSeq, started: time.Now(),
	}
}

func (t *subscriptionTerminal) attach(subscriber *subscriber) {
	if subscriber == nil {
		panic("shim-store server: nil terminal subscriber")
	}
	if t.subscriber != nil {
		panic("shim-store server: terminal subscriber attached twice")
	}
	t.subscriber = subscriber
}

func (t *subscriptionTerminal) setReplayProgress(delivered, firstReplaySeq, lastReplaySeq uint64) {
	t.mu.Lock()
	t.delivered = delivered
	t.firstReplaySeq = firstReplaySeq
	t.lastReplaySeq = lastReplaySeq
	t.mu.Unlock()
}

func (t *subscriptionTerminal) isTerminated() bool { return t.terminated.Load() }

func (t *subscriptionTerminal) terminate(owner string, reason subscriptionTerminalReason, cause error) {
	t.once.Do(func() {
		t.terminated.Store(true)
		t.cancel()
		if t.subscriber == nil {
			panic("shim-store server: terminal without attached subscriber")
		}
		t.fan.remove(t.subscriber)
		t.subscriber.stop()
		closeErr := t.conn.Close()
		t.mu.Lock()
		delivered, firstReplaySeq, lastReplaySeq := t.delivered, t.firstReplaySeq, t.lastReplaySeq
		t.mu.Unlock()
		level := "info"
		switch reason {
		case subscriptionTerminalSlowConsumer:
			level = "warn"
		case subscriptionTerminalReplayFailure, subscriptionTerminalReadinessFailure, subscriptionTerminalTransportFailure:
			level = "error"
		}
		if closeErr != nil && !errors.Is(closeErr, net.ErrClosed) {
			if cause == nil {
				cause = closeErr
			} else {
				cause = fmt.Errorf("%w; socket_close=%v", cause, closeErr)
			}
		}
		record := subscriberTerminalRecord{Owner: owner, Reason: reason, SessionID: t.sessionID, Peer: t.peer, FromSeq: t.fromSeq, Delivered: delivered, FirstReplaySeq: firstReplaySeq, LastReplaySeq: lastReplaySeq, Cause: cause}
		fields := logging.Fields{Operation: "subscribe-terminal", Session: t.sessionID, Subscriber: t.peer, ReplayFromSeq: t.fromSeq, ReplayFirstSeq: firstReplaySeq, ReplayLastSeq: lastReplaySeq, Delivered: delivered, TerminalOwner: owner, TerminalReason: string(reason), Level: level}
		if cause != nil {
			fields.ErrorCause = cause.Error()
		}
		t.log.Log(fields, "subscription terminal owner=%s reason=%s elapsed_ms=%d", owner, reason, time.Since(t.started).Milliseconds())
		if t.hooks.onTerminal != nil {
			t.hooks.onTerminal(record)
		}
	})
}

func (s *Server) subscriberHooksSnapshot() subscriberHooks {
	s.subscriberHooksMu.RLock()
	defer s.subscriberHooksMu.RUnlock()
	return s.subscriberHooks
}

func terminalReasonForRead(err error) subscriptionTerminalReason {
	switch {
	case errors.Is(err, io.EOF):
		return subscriptionTerminalClientEOF
	case errors.Is(err, syscall.ECONNRESET), errors.Is(err, syscall.EPIPE):
		return subscriptionTerminalClientReset
	default:
		return subscriptionTerminalTransportFailure
	}
}

func (s *Server) serveSubscriber(conn net.Conn, sub *protocolv1.Subscribe) {
	sessionID := sub.GetSessionId()
	peer := conn.RemoteAddr().String()
	s.log.LogVerbose(logging.Fields{Operation: "subscribe", Session: sessionID, Subscriber: peer}, "starting streaming replay-then-tail from_seq=%d", sub.GetFromSeq())
	if sessionID == "" {
		s.log.Log(logging.Fields{Operation: "subscribe", Subscriber: peer, Level: "error"}, "protocol subscription rejected: empty session_id")
		return
	}

	replayCtx, cancelReplay := context.WithCancel(context.Background())
	terminal := newSubscriptionTerminal(conn, s.fan, s.log, sessionID, sub.GetFromSeq(), cancelReplay, s.subscriberHooksSnapshot())
	subr := s.fan.subscribe(sessionID, func(reason subscriberDropReason) {
		if reason == subscriberDropSlowConsumer {
			terminal.terminate("fanout", subscriptionTerminalSlowConsumer, nil)
		}
	}, terminal.attach)
	if !s.registerSubscriberTerminal(conn, terminal) {
		return
	}
	defer s.unregisterSubscriberTerminal(conn, terminal)
	defer terminal.terminate("handler", subscriptionTerminalTransportFailure, errors.New("subscriber handler returned without a terminal candidate"))
	go s.subReadLoop(terminal)

	// Register (above) BEFORE replay so live events arriving during replay are
	// buffered, then de-overlapped by seq afterwards. ReplayFrom yields one
	// SQLite row at a time, and this callback writes it before the query advances
	// to the next row. That first-row progress is what keeps the shim's
	// activity deadline alive during large history pulls.
	var delivered, firstReplaySeq, lastReplaySeq uint64
	replayStats, err := s.db.ReplayFrom(replayCtx, sessionID, sub.GetFromSeq(), func(delivery *protocolv1.EntryDelivery) error {
		if terminal.hooks.beforeReplayRow != nil {
			terminal.hooks.beforeReplayRow()
		}
		nextDelivered := delivered + 1
		if err := wire.WriteAny(conn, delivery); err != nil {
			return err
		}
		if delivered == 0 {
			firstReplaySeq = delivery.GetStored().GetSeq()
		}
		delivered = nextDelivered
		lastReplaySeq = delivery.GetStored().GetSeq()
		terminal.setReplayProgress(delivered, firstReplaySeq, lastReplaySeq)
		// One bounded record at first progress and then every 512 events keeps
		// large replays diagnosable without turning this per-event path into a
		// log-volume multiplier.
		if delivered == 1 || delivered%512 == 0 {
			s.log.LogVerbose(logging.Fields{Operation: "subscribe-replay-progress", Session: sessionID, Subscriber: peer},
				"streaming replay progress from_seq=%d delivered=%d first_seq=%d last_seq=%d",
				sub.GetFromSeq(), delivered, firstReplaySeq, lastReplaySeq)
		}
		return nil
	})
	if err != nil {
		if !terminal.isTerminated() {
			terminal.terminate("replay", subscriptionTerminalReplayFailure, err)
		}
		return
	}
	if replayStats.Entries != delivered || replayStats.FirstSeq != firstReplaySeq || replayStats.LastSeq != lastReplaySeq {
		panic(fmt.Sprintf("shim-store server: replay accounting diverged: query=%+v transport={entries:%d first_seq:%d last_seq:%d}",
			replayStats, delivered, firstReplaySeq, lastReplaySeq))
	}
	s.log.Log(logging.Fields{Operation: "subscribe-replay", Session: sessionID, Subscriber: peer},
		"streaming replay completed from_seq=%d delivered=%d first_seq=%d last_seq=%d query_ms=%d",
		sub.GetFromSeq(), delivered, firstReplaySeq, lastReplaySeq, replayStats.Elapsed.Milliseconds())
	// The readiness heartbeat is written only after registration and replay
	// complete. A subscribing shim waits for this frame before asserting its
	// bring-up gate, so a producer write issued immediately after readiness
	// cannot overtake registration on another accepted socket.
	if err := wire.WriteAny(conn, &protocolv1.ConnectionHeartbeat{SentAtMs: time.Now().UnixMilli()}); err != nil {
		terminal.terminate("readiness", subscriptionTerminalReadinessFailure, err)
		return
	}
	s.log.LogVerbose(logging.Fields{Operation: "subscribe-ready", Session: sessionID, Subscriber: peer}, "standing subscription registered and replay complete")

	for {
		select {
		case <-subr.done:
			s.log.LogVerbose(logging.Fields{Operation: "subscribe-tail", Session: sessionID, Subscriber: peer}, "live tail stopped after terminal owner")
			return
		case delivery := <-subr.ch:
			// Skip records already covered by replay (overlap window).
			//
			// THE POSITION IS READ OFF THE `stored` ARM AND ONLY THAT ARM. A
			// live delivery has no seq field at all, so it can never be
			// compared against — or advance — a replay position. The store
			// publishes only stored deliveries today, and this is what keeps
			// that from being a fact a reader has to remember.
			if seq := delivery.GetStored().GetSeq(); seq > 0 && seq <= lastReplaySeq {
				s.log.LogVerbose(logging.Fields{Operation: "subscribe-tail", Session: sessionID, Subscriber: peer}, "skipped replay overlap seq=%d", seq)
				continue
			}
			if terminal.hooks.beforeTailWrite != nil {
				terminal.hooks.beforeTailWrite()
			}
			if err := wire.WriteAny(conn, delivery); err != nil {
				terminal.terminate("tail", subscriptionTerminalTransportFailure, err)
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
func (s *Server) subReadLoop(terminal *subscriptionTerminal) {
	for {
		if _, err := wire.ReadAny(terminal.conn); err != nil {
			if terminal.isTerminated() {
				return
			}
			reason := terminalReasonForRead(err)
			cause := error(nil)
			if reason == subscriptionTerminalTransportFailure {
				cause = err
			}
			terminal.terminate("reader", reason, cause)
			return
		}
	}
}

// onceConn makes physical socket closure a one-owner operation even where a
// generic connection handler and a subscriber terminal both reach teardown.
type onceConn struct {
	net.Conn
	once sync.Once
	err  error
}

func (c *onceConn) Close() error {
	c.once.Do(func() { c.err = c.Conn.Close() })
	return c.err
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
