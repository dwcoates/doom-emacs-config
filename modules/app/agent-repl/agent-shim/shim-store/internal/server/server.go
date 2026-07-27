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

	corev1 "agentrepl/proto/agentshim/core/v1"
	"agentrepl/shim-store/internal/db"
	"agentrepl/wire"
)

// Logf is the loud-logging sink (§12), injected for test capture.
type Logf = func(format string, args ...any)

// Server serves the shim-store protocol over a UDS listener.
type Server struct {
	db  *db.DB
	fan *fanout
	log Logf

	mu     sync.Mutex
	ln     net.Listener
	conns  map[net.Conn]struct{}
	closed bool
	wg     sync.WaitGroup
}

// New builds a Server over an open db. buffer<=0 uses the default fanout buffer.
func New(database *db.DB, log Logf, buffer int) *Server {
	if log == nil {
		log = func(string, ...any) {}
	}
	return &Server{
		db:    database,
		fan:   newFanout(log, buffer),
		log:   log,
		conns: make(map[net.Conn]struct{}),
	}
}

// Listen removes any stale socket file and opens a UDS listener at path.
func Listen(path string) (net.Listener, error) {
	if err := os.Remove(path); err != nil && !errors.Is(err, os.ErrNotExist) {
		return nil, fmt.Errorf("shim-store server: removing stale socket %q: %w", path, err)
	}
	ln, err := net.Listen("unix", path)
	if err != nil {
		return nil, fmt.Errorf("shim-store server: listening on %q: %w", path, err)
	}
	return ln, nil
}

// Serve accepts connections until the listener is closed (via Close). It
// blocks; run it in its own goroutine.
func (s *Server) Serve(ln net.Listener) error {
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
				return nil
			}
			return fmt.Errorf("shim-store server: accept: %w", err)
		}
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
	s.mu.Lock()
	if s.closed {
		s.mu.Unlock()
		return nil
	}
	s.closed = true
	ln := s.ln
	conns := make([]net.Conn, 0, len(s.conns))
	for c := range s.conns {
		conns = append(conns, c)
	}
	s.mu.Unlock()

	if ln != nil {
		ln.Close()
	}
	for _, c := range conns {
		c.Close()
	}
	s.wg.Wait()
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

	msg, err := wire.ReadAny(conn)
	if err != nil {
		if !errors.Is(err, io.EOF) {
			s.log("connection dropped before first frame: %v", err)
		}
		return
	}
	switch m := msg.(type) {
	case *corev1.StoreWrite:
		s.serveProducer(conn, m)
	case *corev1.Subscribe:
		s.serveSubscriber(conn, m)
	case *corev1.CursorQuery:
		s.serveCursorQuery(conn, m)
	default:
		s.log("first frame is %T; expected StoreWrite, Subscribe, or CursorQuery", m)
	}
}

// serveCursorQuery answers a sidecar's startup cursor-recovery request (§7.3):
// an empty file_id returns all persisted cursors, a set file_id returns just
// that one (or an empty list when absent). One CursorList reply, then the
// connection is done.
func (s *Server) serveCursorQuery(conn net.Conn, q *corev1.CursorQuery) {
	var cursors []*corev1.CursorState
	var openTasks []*corev1.OpenTaskState
	if id := q.GetFileId(); id != "" {
		c, err := s.db.Cursor(id)
		if err != nil {
			s.log("cursor query failed (file_id=%s): %v", id, err)
			return
		}
		if c != nil {
			cursors = append(cursors, c)
		}
	} else {
		all, err := s.db.Cursors()
		if err != nil {
			s.log("cursor query failed (all): %v", err)
			return
		}
		cursors = all
		openTasks, err = s.db.OpenTasks()
		if err != nil {
			s.log("cursor query failed (open tasks): %v", err)
			return
		}
	}
	s.log("startup recovery snapshot: cursors=%d open_tasks=%d file_id=%q",
		len(cursors), len(openTasks), q.GetFileId())
	if err := wire.WriteAny(conn, &corev1.CursorList{
		Cursors:                cursors,
		OpenTasks:              openTasks,
		OpenTasksAuthoritative: q.GetFileId() == "",
	}); err != nil {
		s.log("cursor query reply failed: %v", err)
	}
}

// ---- producer side --------------------------------------------------------

func (s *Server) serveProducer(conn net.Conn, first *corev1.StoreWrite) {
	if err := s.processWrite(conn, first); err != nil {
		s.log("producer write failed (producer=%s): %v", first.GetProducer(), err)
		return
	}
	for {
		msg, err := wire.ReadAny(conn)
		if err != nil {
			if !errors.Is(err, io.EOF) {
				s.log("producer connection dropped (producer=%s): %v", first.GetProducer(), err)
			}
			return
		}
		switch m := msg.(type) {
		case *corev1.StoreWrite:
			if err := s.processWrite(conn, m); err != nil {
				s.log("producer write failed (producer=%s): %v", m.GetProducer(), err)
				return
			}
		case *corev1.Heartbeat:
			if err := wire.WriteAny(conn, &corev1.Heartbeat{SentAtMs: m.GetSentAtMs()}); err != nil {
				return
			}
		default:
			s.log("producer sent an unrecognized frame (%T, producer=%s); disconnecting", m, first.GetProducer())
			return
		}
	}
}

// processWrite ingests one batch and fans out its events, then acks. A rejected
// batch acks with a non-empty error and a loud log; it is never silently
// dropped.
func (s *Server) processWrite(conn net.Conn, sw *corev1.StoreWrite) error {
	ack := s.ingestAndFan(sw)
	return wire.WriteAny(conn, ack)
}

func (s *Server) ingestAndFan(sw *corev1.StoreWrite) *corev1.StoreWriteAck {
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

	res, err := s.db.Ingest(sw.GetProducer(), persistent, batch.GetCursorAdvance())
	if err != nil {
		s.log("REJECTED batch (producer=%s events=%d): %v", sw.GetProducer(), len(events), err)
		return &corev1.StoreWriteAck{Error: err.Error()}
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

	if res.Deduped > 0 {
		s.log("dedup: producer=%s accepted=%d deduped=%d last_seq=%d", sw.GetProducer(), res.Accepted, res.Deduped, res.LastSeq)
	}
	return &corev1.StoreWriteAck{Accepted: res.Accepted, Deduped: res.Deduped, LastSeq: res.LastSeq}
}

// ---- subscriber side ------------------------------------------------------

func (s *Server) serveSubscriber(conn net.Conn, sub *corev1.Subscribe) {
	sessionID := sub.GetSessionId()
	if sessionID == "" {
		s.log("subscribe rejected: empty session_id")
		return
	}
	s.log("subscribe: session=%s from_seq=%d", sessionID, sub.GetFromSeq())

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
		s.log("subscribe replay failed: session=%s: %v", sessionID, err)
		return
	}
	var lastReplaySeq uint64
	for _, ev := range replayed {
		if err := wire.WriteAny(conn, ev); err != nil {
			s.log("subscribe replay write failed: session=%s: %v", sessionID, err)
			return
		}
		if ev.GetSeq() > lastReplaySeq {
			lastReplaySeq = ev.GetSeq()
		}
	}

	// Detect client disconnect / drain client heartbeats without ever writing
	// from a second goroutine (the tail loop below owns all writes).
	go s.subReadLoop(conn, subr)

	for {
		select {
		case <-subr.done:
			return
		case ev := <-subr.ch:
			// Skip persistent events already covered by replay (overlap window).
			if ev.GetClass() != corev1.EventClass_EVENT_CLASS_EPHEMERAL &&
				ev.GetSeq() > 0 && ev.GetSeq() <= lastReplaySeq {
				continue
			}
			if err := wire.WriteAny(conn, ev); err != nil {
				s.log("subscriber write failed: session=%s: %v", sessionID, err)
				return
			}
		}
	}
}

// subReadLoop reads (and discards, apart from close detection) frames from a
// subscriber connection so a client close unblocks the tail loop.
func (s *Server) subReadLoop(conn net.Conn, subr *subscriber) {
	for {
		if _, err := wire.ReadAny(conn); err != nil {
			subr.close()
			return
		}
	}
}

// ---- Any framing ----------------------------------------------------------
//
// The encode/decode pair lives in agentrepl/wire (WriteAny / ReadAny). It used
// to be copy-pasted here and in three other packages; one wire contract with
// four hand-maintained copies is the drift that package exists to prevent.
// ReadAny still returns ReadFrame's error VERBATIM, which is what lets the
// handlers below tell a clean io.EOF close from a fault.
