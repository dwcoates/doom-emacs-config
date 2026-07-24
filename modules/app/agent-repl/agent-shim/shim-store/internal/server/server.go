// Package server is the shim-store UDS front end: it accepts producer and
// subscriber connections, ingests StoreWrite batches through the db layer, and
// serves Subscribe replay-then-live-tail subscriptions via the fanout.
//
// Socket protocol (the system-wide convention every agent-shim UDS hop uses).
// Transport is UDS with `agentrepl/wire` framing: a 4-byte big-endian length
// prefix followed by exactly one serialized google.protobuf.Any. The Any wraps
// the actual message (StoreWrite, StoreWriteAck, Subscribe, Heartbeat,
// core.v1.Event for subscription delivery, ...) and its type_url is THE message
// discriminator, resolved against the proto registry (anypb.New /
// Any.UnmarshalNew). The daemon's shimclient and the TS shim speak the same
// convention.
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
	"google.golang.org/protobuf/proto"
	"google.golang.org/protobuf/types/known/anypb"
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

	msg, err := readMsg(conn)
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
	}
	if err := writeMsg(conn, &corev1.CursorList{Cursors: cursors}); err != nil {
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
		msg, err := readMsg(conn)
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
			if err := writeMsg(conn, &corev1.Heartbeat{SentAtMs: m.GetSentAtMs()}); err != nil {
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
	return writeMsg(conn, ack)
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
		if err := writeMsg(conn, ev); err != nil {
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
			if err := writeMsg(conn, ev); err != nil {
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
		if _, err := readMsg(conn); err != nil {
			subr.close()
			return
		}
	}
}

// ---- Any framing ----------------------------------------------------------

// writeMsg wraps m in a google.protobuf.Any and writes it as one wire frame.
func writeMsg(conn net.Conn, m proto.Message) error {
	a, err := anypb.New(m)
	if err != nil {
		return fmt.Errorf("shim-store server: wrapping %T in Any: %w", m, err)
	}
	b, err := proto.Marshal(a)
	if err != nil {
		return fmt.Errorf("shim-store server: marshaling Any(%T): %w", m, err)
	}
	return wire.WriteFrame(conn, b)
}

// readMsg reads one wire frame and unwraps the google.protobuf.Any into its
// concrete message via the proto registry. io.EOF at a frame boundary is
// returned verbatim so callers can distinguish a clean close.
func readMsg(conn net.Conn) (proto.Message, error) {
	frame, err := wire.ReadFrame(conn)
	if err != nil {
		return nil, err
	}
	a := &anypb.Any{}
	if err := proto.Unmarshal(frame, a); err != nil {
		return nil, fmt.Errorf("shim-store server: decoding Any frame: %w", err)
	}
	m, err := a.UnmarshalNew()
	if err != nil {
		return nil, fmt.Errorf("shim-store server: resolving Any type %q: %w", a.GetTypeUrl(), err)
	}
	return m, nil
}
