// Package shimlisten is the daemon's listening side of the shim transport:
// one socket every shim dials, and the registry of which shims are connected.
//
// # Why the daemon listens
//
// It used to be the other way round — each shim listened on its own
// session-<id>.sock and the daemon dialled it. That put the dialer (the
// daemon) ahead of the listener (a node process still booting), so for the
// ~300ms before the shim called listen() the dial failed with ENOENT. The
// reconnect churn, the per-session socket files, and ReattachDecision — a
// probe that dialled a path and read a frame to ask "is a shim alive?" — were
// all machinery compensating for that inversion.
//
// Inverting it removes them. The listener is the long-running daemon, the
// dialer is the freshly spawned child, and retrying is the dialer's job, which
// is where retry belongs. "Is a shim alive for session X?" stops being a
// filesystem probe and becomes a lookup here.
//
// This is the pattern the rest of the system already uses: every shim dials
// one store.sock, and Emacs plus the webapp share one daemon-frontend.sock.
//
// # Claiming
//
// A shim announces itself with ShimHello, which carries its session id. The
// connection is then PARKED under that id until a driver claims it. Parking
// matters on daemon restart: survivors dial in as soon as the socket exists,
// long before any prompt causes a driver to be built for them.
package shimlisten

import (
	"context"
	"fmt"
	"net"
	"os"
	"path/filepath"
	"sync"

	corev1 "agentrepl/proto/agentshim/core/v1"
	"agentrepl/wire"

	"google.golang.org/protobuf/proto"
	"google.golang.org/protobuf/types/known/anypb"
)

// DefaultSocketPath is the one socket every shim dials. Fixed and well-known:
// there is exactly one daemon, so a surviving shim reconnects to the same path
// it was given at spawn.
func DefaultSocketPath() (string, error) {
	home, err := os.UserHomeDir()
	if err != nil {
		return "", fmt.Errorf("shimlisten: resolving home dir: %w", err)
	}
	return filepath.Join(home, ".cache", "agent-repl", "sock", "daemon-shim.sock"), nil
}

// Conn is an accepted shim connection plus the hello that identified it.
type Conn struct {
	Net   net.Conn
	Hello *corev1.ShimHello
}

// Server accepts shim connections and routes them to whoever claims them.
type Server struct {
	logf func(string, ...any)

	mu       sync.Mutex
	closed   bool
	parked   map[string]*Conn      // session id -> connection awaiting a claim
	waiters  map[string]chan *Conn // session id -> the claimer blocked in Next
	listener net.Listener
}

// New builds a Server. logf may be nil.
func New(logf func(string, ...any)) *Server {
	if logf == nil {
		logf = func(string, ...any) {}
	}
	return &Server{
		logf:    logf,
		parked:  map[string]*Conn{},
		waiters: map[string]chan *Conn{},
	}
}

// Listen binds path (replacing a stale file) and serves accepts until Close.
func (s *Server) Listen(path string) error {
	if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
		return fmt.Errorf("shimlisten: creating socket dir: %w", err)
	}
	// A stale file from a dead daemon would make bind fail; the daemon owns
	// this path, so removing it is safe and is what makes restart work.
	if err := os.Remove(path); err != nil && !os.IsNotExist(err) {
		return fmt.Errorf("shimlisten: removing stale socket %s: %w", path, err)
	}
	ln, err := net.Listen("unix", path)
	if err != nil {
		return fmt.Errorf("shimlisten: listening on %s: %w", path, err)
	}
	s.mu.Lock()
	s.listener = ln
	s.mu.Unlock()
	s.logf("shimlisten: listening for shims on %s", path)
	go s.acceptLoop(ln)
	return nil
}

func (s *Server) acceptLoop(ln net.Listener) {
	for {
		conn, err := ln.Accept()
		if err != nil {
			s.mu.Lock()
			closed := s.closed
			s.mu.Unlock()
			if closed {
				return
			}
			s.logf("shimlisten: accept failed: %v", err)
			return
		}
		go s.onConn(conn)
	}
}

// onConn reads the identifying ShimHello and files the connection under its
// session id. A connection that does not open with a ShimHello is closed and
// loud-logged: it is not a shim, and guessing what it might be would file it
// under the wrong session.
func (s *Server) onConn(conn net.Conn) {
	hello, err := readHello(conn)
	if err != nil {
		s.logf("shimlisten: rejecting connection: %v", err)
		conn.Close()
		return
	}
	sid := hello.GetSessionId()
	if sid == "" {
		s.logf("shimlisten: rejecting connection: ShimHello carried no session_id")
		conn.Close()
		return
	}
	s.deliver(sid, &Conn{Net: conn, Hello: hello})
}

// deliver hands the connection to a waiting claimer, or parks it.
func (s *Server) deliver(sessionID string, c *Conn) {
	s.mu.Lock()
	if s.closed {
		s.mu.Unlock()
		c.Net.Close()
		return
	}
	if ch, ok := s.waiters[sessionID]; ok {
		delete(s.waiters, sessionID)
		s.mu.Unlock()
		ch <- c
		s.logf("shimlisten: session %s connected (claimed)", sessionID)
		return
	}
	// Supersede any parked connection: the newest dial-in is the live shim,
	// and holding a dead one would answer Connected with a corpse.
	if old, ok := s.parked[sessionID]; ok {
		old.Net.Close()
		s.logf("shimlisten: session %s reconnected, dropping the previous parked connection", sessionID)
	}
	s.parked[sessionID] = c
	s.mu.Unlock()
	s.logf("shimlisten: session %s connected (parked)", sessionID)
}

// Next yields sessionID's connection, waiting for it to dial in if it has not
// yet. It returns a PARKED connection immediately when one is present, which
// is the daemon-restart case: survivors connect before anything claims them.
func (s *Server) Next(ctx context.Context, sessionID string) (*Conn, error) {
	s.mu.Lock()
	if s.closed {
		s.mu.Unlock()
		return nil, fmt.Errorf("shimlisten: server closed")
	}
	if c, ok := s.parked[sessionID]; ok {
		delete(s.parked, sessionID)
		s.mu.Unlock()
		return c, nil
	}
	if _, exists := s.waiters[sessionID]; exists {
		s.mu.Unlock()
		return nil, fmt.Errorf("shimlisten: session %s already has a waiter", sessionID)
	}
	ch := make(chan *Conn, 1)
	s.waiters[sessionID] = ch
	s.mu.Unlock()

	select {
	case c := <-ch:
		return c, nil
	case <-ctx.Done():
		s.mu.Lock()
		// Only remove OUR waiter: deliver may have taken it already, in which
		// case the connection is in flight down the channel and must be closed
		// rather than leaked.
		if cur, ok := s.waiters[sessionID]; ok && cur == ch {
			delete(s.waiters, sessionID)
			s.mu.Unlock()
			return nil, fmt.Errorf("shimlisten: waiting for session %s to connect: %w", sessionID, ctx.Err())
		}
		s.mu.Unlock()
		select {
		case c := <-ch:
			return c, nil
		default:
			return nil, fmt.Errorf("shimlisten: waiting for session %s to connect: %w", sessionID, ctx.Err())
		}
	}
}

// Connected reports whether a shim for sessionID is currently parked here.
// This is the cheap half of the "is a shim alive?" question that
// ReattachDecision used to answer with a dial and a handshake read; the
// session lock covers the other half (a shim alive but not yet dialled in).
func (s *Server) Connected(sessionID string) bool {
	s.mu.Lock()
	defer s.mu.Unlock()
	_, parked := s.parked[sessionID]
	return parked
}

// Close stops accepting and drops every parked connection.
func (s *Server) Close() error {
	s.mu.Lock()
	if s.closed {
		s.mu.Unlock()
		return nil
	}
	s.closed = true
	ln := s.listener
	parked := s.parked
	s.parked = map[string]*Conn{}
	s.waiters = map[string]chan *Conn{}
	s.mu.Unlock()

	for _, c := range parked {
		c.Net.Close()
	}
	if ln != nil {
		return ln.Close()
	}
	return nil
}

// readHello reads the first frame and requires it to be a ShimHello.
func readHello(conn net.Conn) (*corev1.ShimHello, error) {
	payload, err := wire.ReadFrame(conn)
	if err != nil {
		return nil, fmt.Errorf("reading hello frame: %w", err)
	}
	var env anypb.Any
	if err := proto.Unmarshal(payload, &env); err != nil {
		return nil, fmt.Errorf("unmarshalling hello envelope: %w", err)
	}
	msg, err := env.UnmarshalNew()
	if err != nil {
		return nil, fmt.Errorf("resolving hello type %q: %w", env.GetTypeUrl(), err)
	}
	hello, ok := msg.(*corev1.ShimHello)
	if !ok {
		return nil, fmt.Errorf("first frame was %T, expected ShimHello", msg)
	}
	return hello, nil
}
