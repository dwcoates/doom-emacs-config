// Package server is the shim-store UDS front end: it accepts producer
// connections, ingests WriteBatchRequest batches through the db layer, and owns
// the subscriber connection lifecycle.
//
// RECONCILED AGAINST store.v1, NOT REBUILT ON IT. The old surface spoke
// `agentshim.v1.StoreEntryWrite` on the write half and `protocol.v1` on the
// read half. store.v1 deleted the read half outright and respelled the write
// half, so this file speaks the store.v1 messages that are near-renames of what
// it used to speak (WriteBatchRequest, GetSidecarCursorsRequest /
// GetSidecarCursorsResponse) and REFUSES LOUDLY on the paths whose subject
// genuinely changed. Nothing is inferred and no substitute message is invented.
//
// WHAT IS GONE, AND WHY THE HANDLER IS A REFUSAL:
//
//   - protocol.v1.Subscribe / EntryDelivery: replaced by
//     WatchAgentSession, addressed by an opaque store-minted AgentSessionToken
//     over a Connect server stream rather than by (session_id, from_seq) over
//     UDS. See serveSubscriber.
//   - protocol.v1.MessagePageRequest / MessagePage: replaced by
//     OpenAgentSession and ReadAgentPage, paged by StoreItemPointer over a
//     conversation.v1.AgentId book rather than by seq over a session.
//   - protocol.v1.ConnectionHeartbeat: deleted with no replacement, which
//     retires the producer preamble and the subscriber readiness frame.
//   - protocol.v1.HealthCheck / HealthStatus: deleted with no replacement; see
//     internal/healthcheck.
//
// The subscription terminal machinery below is DELIBERATELY RETAINED. It is
// transport lifecycle — one owner per ending, one canonical record, no socket
// closed behind the owner's back — and it names no proto type at all, so it
// survives the redesign intact and is what the Connect stream handler will be
// built on.
//
// Socket protocol: UDS with `agentrepl/wire` framing — a 4-byte big-endian
// length prefix followed by exactly one serialized google.protobuf.Any, whose
// type_url is THE message discriminator.
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

	storev1 "agentrepl/proto/store/v1"
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
	case *storev1.WriteBatchRequest:
		s.log.Log(logging.Fields{Operation: "classify-connection", Producer: m.GetProducer(), Subscriber: peer}, "classified producer connection")
		s.serveProducer(conn, m)
	case *storev1.WatchAgentSessionRequest:
		s.serveSubscriber(conn, m)
	case *storev1.GetSidecarCursorsRequest:
		s.log.Log(logging.Fields{Operation: "classify-connection", Subscriber: peer}, "classified sidecar cursor query file_id=%q", m.GetFileId())
		s.serveCursorQuery(conn, m)
	default:
		s.log.Log(logging.Fields{Operation: "classify-connection", Subscriber: peer, Level: "error"},
			"protocol frame is %T; expected WriteBatchRequest, WatchAgentSessionRequest or GetSidecarCursorsRequest", m)
	}
}

// serveCursorQuery answers a sidecar's startup cursor-recovery request: an
// unset file_id returns all persisted cursors, a set file_id returns just that
// one (or an empty list when absent). One reply, then the connection is done.
//
// A NEAR-RENAME, NOT A REBUILD. `agentshim.v1.CursorQuery` became
// `store.v1.GetSidecarCursorsRequest` (file_id now `optional`, which is the
// same "unset means all" semantics stated in the type) and `CursorList` became
// `GetSidecarCursorsResponse` with a success/failure oneof. The open-task half
// of the old reply is gone from the schema entirely: `open_tasks` and
// `open_tasks_authoritative` have no field on the new response, and the
// obligations they gestured at are GetLiveWork's job now. Nothing is
// synthesized in their place.
//
// The failure arm is REACHED, not merely declared: a query the db layer refused
// is answered with GetSidecarCursorsFailure rather than with silence, because a
// sidecar that cannot distinguish "no cursors" from "could not read cursors"
// resumes every tailed file from zero.
func (s *Server) serveCursorQuery(conn net.Conn, q *storev1.GetSidecarCursorsRequest) {
	peer := conn.RemoteAddr().String()
	s.log.LogVerbose(logging.Fields{Operation: "cursor-query", Subscriber: peer}, "processing cursor query file_id=%q", q.GetFileId())
	var cursors []*storev1.CursorState
	var queryErr error
	if id := q.GetFileId(); id != "" {
		c, err := s.db.Cursor(id)
		if err != nil {
			queryErr = err
		} else if c != nil {
			cursors = append(cursors, c)
		}
	} else {
		all, err := s.db.Cursors()
		if err != nil {
			queryErr = err
		} else {
			cursors = all
		}
	}

	reply := &storev1.GetSidecarCursorsResponse{}
	if queryErr != nil {
		// The db layer already logged the cause with its own context; this
		// record states that the refusal reached the caller.
		s.log.Log(logging.Fields{Operation: "cursor-query", Subscriber: peer, Level: "error"},
			"cursor recovery REFUSED file_id=%q: %v", q.GetFileId(), queryErr)
		reply.Result = &storev1.GetSidecarCursorsResponse_Failure{Failure: &storev1.GetSidecarCursorsFailure{
			Detail: queryErr.Error(),
		}}
	} else {
		s.log.Log(logging.Fields{Operation: "cursor-query", Subscriber: peer},
			"startup recovery snapshot: cursors=%d file_id=%q", len(cursors), q.GetFileId())
		reply.Result = &storev1.GetSidecarCursorsResponse_Success{Success: &storev1.GetSidecarCursorsSuccess{
			Cursors: cursors,
		}}
	}
	if err := wire.WriteAny(conn, reply); err != nil {
		s.log.Log(logging.Fields{Operation: "cursor-query-reply", Subscriber: peer, Level: "error"}, "protocol cursor reply write failed: %v", err)
	}
}

// ---- producer side --------------------------------------------------------
//
// THE PRODUCER PREAMBLE IS GONE WITH ITS FRAME. A connection used to be allowed
// to open on a ConnectionHeartbeat (or a HealthCheck) and only later declare
// itself with a write, which is what kept a recovered-but-idle sidecar link
// alive for the hours it may have nothing to write. store.v1 deleted
// ConnectionHeartbeat and minted no replacement, so there is no frame left for
// an idle producer to send and the preamble has no entry point. Recorded as a
// gap rather than replaced by a keepalive of this layer's invention.

func (s *Server) serveProducer(conn net.Conn, first *storev1.WriteBatchRequest) {
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
		case *storev1.WriteBatchRequest:
			if err := s.processWrite(conn, m); err != nil {
				return
			}
		default:
			s.log.Log(logging.Fields{Operation: "producer-read", Producer: producer, Subscriber: peer, Level: "error"}, "protocol frame is %T; disconnecting producer", m)
			return
		}
	}
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
func (s *Server) processWrite(conn net.Conn, write *storev1.WriteBatchRequest) error {
	peer := conn.RemoteAddr().String()
	batch := write.GetBatch()
	res, err := s.ingestAndFan(write)
	if err != nil {
		s.log.Log(logging.Fields{Operation: "store-write", Producer: write.GetProducer(), Subscriber: peer, Level: "error"},
			"WriteBatchRequest REJECTED entries=%d cursor_advance=%t — the producer cannot be told (WriteBatchResponse is declared but not yet served), so the connection is dropped instead: %v",
			len(batch.GetEntries()), batch.GetCursorAdvance() != nil, err)
		return err
	}
	s.log.LogVerbose(logging.Fields{Operation: "store-write", Producer: write.GetProducer(), Subscriber: peer},
		"WriteBatchRequest processed entries=%d accepted=%d replayed=%d unconverted=%d last_seq=%d cursor_advance=%t",
		len(batch.GetEntries()), res.Accepted, res.Replayed, res.Unconverted, res.LastSeq, batch.GetCursorAdvance() != nil)
	return nil
}

// ingestAndFan persists one batch and announces what it persisted.
func (s *Server) ingestAndFan(write *storev1.WriteBatchRequest) (db.Result, error) {
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

	// NOTHING IS ANNOUNCED, because there is nothing to announce it WITH.
	// db.Ingest used to return the accepted records already wrapped in the
	// `protocol.v1.EntryDelivery` a subscriber receives; store.v1 deleted that
	// envelope, and its replacement frame is addressed by a store-minted
	// StoreItemPointer this layer cannot mint. The lock is still taken across
	// the region so the assign-then-announce ordering guarantee is restored by
	// filling this hole rather than by re-deriving the locking.
	s.ingestMu.Unlock()

	// Successful persisted batches are high-frequency session narration rather
	// than lifecycle or failure evidence. Keep their detailed outcome available
	// in verbose mode without growing the normal global service log.
	s.log.LogVerbose(logging.Fields{
		Operation: "ingest", Producer: write.GetProducer(), Session: "",
	}, "persisted batch entries=%d accepted=%d replayed=%d unconverted=%d last_seq=%d ingest_ms=%d",
		len(entries), res.Accepted, res.Replayed, res.Unconverted, res.LastSeq, ingestMs)
	// A REPLAY is a normal-log fact, not narration: it says a producer resent a
	// batch whose outcome it never learned, and that the write identity held. It
	// is rare by construction (one store bounce per deploy), so it never floods.
	if res.Replayed > 0 {
		s.log.Log(logging.Fields{
			Operation: "ingest", Producer: write.GetProducer(), Session: "",
		}, "REPLAYED batch absorbed idempotently entries=%d accepted=%d replayed=%d — the producer resent writes whose outcome it never learned, and the (session_id, write_id) identity made them no-ops instead of duplicate rows",
			len(entries), res.Accepted, res.Replayed)
	}
	return res, nil
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

// serveSubscriber REFUSES every subscription, loudly.
//
// The read half it served no longer exists in any spelling this layer can
// reach. `protocol.v1.Subscribe{session_id, from_seq}` was replaced by
// `store.v1.WatchAgentSessionRequest{watch}`, whose only field is an opaque,
// store-minted `AgentSessionToken` — a hash of an identity, minted by
// OpenAgentSession, deliberately unconstructable and unparseable by a caller.
// Serving a watch therefore requires OpenAgentSession first (to mint the token,
// serve the opening page, and pin the tail to begin after that page's newest
// item), and requires every stored line to carry a `StoreItemPointer`. Neither
// exists: the store has no OpenAgentSession handler and no pointer minting, and
// the record persistence those would read from is itself refused (see
// db.ErrRecordPersistenceUnreconciled).
//
// A REFUSAL, NOT AN EMPTY TAIL. Registering the subscriber and holding the
// socket open would present a live subscription that can never deliver a line —
// which is the silent-empty-feed failure this store has been bitten by before.
// The connection is dropped so the caller observes something.
func (s *Server) serveSubscriber(conn net.Conn, sub *storev1.WatchAgentSessionRequest) {
	s.log.Log(logging.Fields{Operation: "subscribe", Subscriber: conn.RemoteAddr().String(), Level: "error"},
		"WatchAgentSession REFUSED watch_token_set=%t — the store has no OpenAgentSession handler to mint or resolve an AgentSessionToken and no StoreItemPointer to position a line at, so a registered subscription could only ever be a silent empty tail; dropping the connection instead",
		sub.GetWatch() != nil)
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
