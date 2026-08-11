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
// connection is then PARKED under that id until a session controller claims it. Parking
// matters on daemon restart: survivors dial in as soon as the socket exists,
// long before any prompt causes a session controller to be built for them.
//
// A claim MOVES the connection from `parked` to `claimed`; it does not make it
// disappear. Both indexes together are the answer to "is a shim connected for
// session X?", and that answer is deliberately independent of which controller
// generation owns the read side — see Connected.
package shimlisten

import (
	"context"
	"errors"
	"fmt"
	"net"
	"os"
	"path/filepath"
	"sync"
	"syscall"
	"time"

	corev1 "agentrepl/proto/agentshim/core/v1"
	"agentrepl/wire"

	"golang.org/x/sys/unix"
)

// SocketEnvVar overrides the shim socket path when set, the way
// stateroot.EnvVar overrides the state root. It exists so a daemon can be
// stood up somewhere OTHER than the operator's live socket — which is what an
// end-to-end test does every time it runs.
//
// Without it the only way to move the socket was to move $HOME, and $HOME
// moves eight other paths with it (the state store, the session locks, the
// account cache). A test that forgot one bound the REAL
// ~/.cache/agent-repl/sock/daemon-shim.sock and stole the live daemon's shims.
// One explicit variable makes the isolation a stated fact rather than a
// side effect of an unrelated one.
const SocketEnvVar = "AGENT_REPL_SHIM_SOCKET"

// DefaultSocketPath is the one socket every shim dials. Fixed and well-known:
// there is exactly one daemon, so a surviving shim reconnects to the same path
// it was given at spawn.
//
// A RELATIVE override is refused rather than resolved against the working
// directory. The daemon and every shim it spawns must agree on this path, and
// they do not share a working directory — a relative one would silently give
// them two different sockets, which presents as shims that dial forever and
// never arrive.
func DefaultSocketPath() (string, error) {
	if sock := os.Getenv(SocketEnvVar); sock != "" {
		if !filepath.IsAbs(sock) {
			return "", fmt.Errorf("shimlisten: %s must be an absolute path, got %q", SocketEnvVar, sock)
		}
		return sock, nil
	}
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
	// watchDone closes when this connection's parked-socket watch has exited,
	// so a claim can take sole ownership of the read side by rendezvous rather
	// than by hoping the watch has noticed. Nil when nothing watches it.
	watchDone chan struct{}
}

// Server accepts shim connections and routes them to whoever claims them.
type Server struct {
	logf func(string, ...any)

	mu      sync.Mutex
	closed  bool
	parked  map[string]*Conn      // session id -> connection awaiting a claim
	waiters map[string]chan *Conn // session id -> the claimer blocked in Next
	// claimed is the OTHER half of "is a shim connected for this session?":
	// the connection a claimer took out of `parked` and still owns.
	//
	// # Why the listener remembers a connection it gave away
	//
	// It used to forget. `Connected` read `parked` alone, so the instant a
	// shimclient claimed a connection the listener answered "no shim is
	// connected for this session" about a shim that was talking to this very
	// daemon — and the workspace-ownership gate (sessioncontroller
	// survivingshim.go) reads exactly that answer to decide whether a lock
	// holder is a survivor to adopt or a squatter to kill. A healthy shim whose
	// connection belonged to an EARLIER controller generation therefore
	// satisfied nothing, was waited out, and was SIGTERM'd while ready.
	//
	// The claim is a fact about WHO IS READING the socket. It is not a fact
	// about whether a shim is connected, and conflating the two is what made
	// adoption turn on which generation happened to hold the read side.
	//
	// # It cannot go stale into a false "connected"
	//
	// Every read of this map re-proves the peer with the same non-consuming
	// kernel probe `parked` entries get (connectionOpen), and a closed one is
	// dropped by the probe that found it. A claimer that exits without closing
	// its socket is therefore indistinguishable from one that closed it, and a
	// redial for the same session supersedes the entry in deliver. Nothing here
	// depends on a claimer remembering to hand anything back.
	//
	// Entries are never CLOSED from here. The claimer owns the read side, and
	// Evict's contract — "this operation cannot close an active controller's
	// route" — stays exactly as it was.
	claimed  map[string]*Conn // session id -> connection a claimer owns
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
		claimed: map[string]*Conn{},
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
	// A NEWER DIAL SUPERSEDES THE OLD CLAIM. The shim on the other end of a
	// previously claimed connection has just redialled, which it only does
	// after its old socket is gone, so the claim record describes a transport
	// that no longer exists. Dropped rather than closed: the read side belongs
	// to whichever claimer took it, and closing it from here would reach into
	// another owner's connection.
	s.dropClaimLocked(sessionID, "superseded_by_redial")
	if ch, ok := s.waiters[sessionID]; ok {
		delete(s.waiters, sessionID)
		s.claimed[sessionID] = c
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
	c.watchDone = make(chan struct{})
	s.parked[sessionID] = c
	s.mu.Unlock()
	s.logf("shimlisten: session %s connected (parked)", sessionID)
	go s.watchParked(sessionID, c)
}

// watchParked makes a DEAD PARKED TRANSPORT UNREPRESENTABLE by binding the
// parked entry's lifetime to its socket's.
//
// # The state this removes
//
// A hibernation drops its claimed connection and then SIGTERMs the shim. The
// still-alive shim notices the drop and redials, and that new connection can be
// parked AFTER the stop's one-shot eviction has already run. Nothing else ever
// looked at it again until the next EnsureShim, which then probed a transport
// whose process was gone — or, worse, probed it in the instant between the
// redial and the process's death and got "open" for a corpse-to-be. "The parked
// connection is still open" and "the shim is still alive" had drifted apart,
// and every consumer of the first was answering with the second.
//
// # Why the listener owns it
//
// The KERNEL closes a process's sockets when the process dies. That is not a
// policy anything here can miss, race, or forget to call: it happens at death,
// exactly once, for every way a process can die. Watching the socket is
// therefore watching the process, and eviction on socket death is the same
// fact arriving from the only authority that always has it.
//
// The point probes at claim and at Connected stay: they answer the question
// SYNCHRONOUSLY for a caller that has one, and this asynchronous watch answers
// it for the long stretches when nobody is asking. Neither replaces the other.
//
// # What it does not consume
//
// MSG_PEEK, always. A parked connection's frames belong to the shimclient that
// eventually claims it, so this proves liveness without taking a byte. When a
// frame DOES arrive while parked the socket stays permanently readable, so
// there is nothing left to wait on: the watch ends, loudly, and the point
// probes carry the session from there.
func (s *Server) watchParked(sessionID string, c *Conn) {
	defer close(c.watchDone)
	sc, ok := c.Net.(syscall.Conn)
	if !ok {
		s.logf("shimlisten: parked lifecycle session=%s decision=unwatched reason=connection_type_%T_exposes_no_syscall_state connection_state=parked", sessionID, c.Net)
		return
	}
	raw, err := sc.SyscallConn()
	if err != nil {
		s.logf("shimlisten: parked lifecycle session=%s decision=unwatched reason=syscall_conn_failed connection_state=parked error=%v", sessionID, err)
		return
	}
	var (
		dead   bool
		detail string
	)
	readErr := raw.Read(func(fd uintptr) bool {
		var one [1]byte
		n, _, recvErr := unix.Recvfrom(int(fd), one[:], unix.MSG_PEEK|unix.MSG_DONTWAIT)
		switch {
		case recvErr == nil && n > 0:
			detail = "frame_pending"
			return true
		case recvErr == nil:
			// Zero bytes readable is the orderly EOF: the peer is gone.
			dead, detail = true, "eof"
			return true
		case errors.Is(recvErr, unix.EAGAIN), errors.Is(recvErr, unix.EWOULDBLOCK), errors.Is(recvErr, unix.EINTR):
			return false // Nothing to see yet; wait for the socket to say something.
		default:
			dead, detail = true, recvErr.Error()
			return true
		}
	})
	switch {
	case dead:
		s.evictIfCurrent(sessionID, c, "socket_closed_while_parked")
		s.logf("shimlisten: parked lifecycle session=%s decision=watch_ended reason=socket closed while parked detail=%s", sessionID, detail)
	case errors.Is(readErr, os.ErrDeadlineExceeded):
		// The claim woke us on purpose: the shimclient owns this socket's read
		// side from here on, and a second reader would fight it for the lock.
		s.logf("shimlisten: parked lifecycle session=%s decision=watch_ended reason=claimed connection_state=open", sessionID)
	case readErr != nil:
		// The descriptor went away underneath the watch — which is what our own
		// Close, Evict and supersede paths do, after removing the entry. The
		// entry is checked rather than assumed: an error here that left a parked
		// connection behind would be exactly the dead transport this watch
		// exists to make impossible.
		s.evictIfCurrent(sessionID, c, "socket_closed_while_parked")
		s.logf("shimlisten: parked lifecycle session=%s decision=watch_ended reason=socket closed while parked detail=%v", sessionID, readErr)
	default:
		s.logf("shimlisten: parked lifecycle session=%s decision=watch_ended reason=peer_sent_while_parked detail=%s connection_state=open", sessionID, detail)
	}
}

// awaitWatchExit hands the read side of a just-claimed connection over from the
// parked-socket watch to its claimer.
//
// The watch sits in the poller holding the connection's read lock, so the
// handover is a RENDEZVOUS rather than a hope: a read deadline in the past
// wakes it, its exit is waited for, and the deadline is then cleared for the
// owner. Both deadline calls are reported as errors, never absorbed — a claim
// that cannot wake the watch, or cannot clear the deadline it set, would hand
// back a connection whose reads are broken in a way the claimer could only
// discover as an unexplained failure later.
func (s *Server) awaitWatchExit(sessionID string, c *Conn) error {
	if c.watchDone == nil {
		return nil
	}
	select {
	case <-c.watchDone:
		return nil // Already gone; nothing to wake and no deadline to clear.
	default:
	}
	if err := c.Net.SetReadDeadline(time.Now()); err != nil {
		return fmt.Errorf("shimlisten: waking the parked watch of session %s to claim it: %w", sessionID, err)
	}
	<-c.watchDone
	if err := c.Net.SetReadDeadline(time.Time{}); err != nil {
		return fmt.Errorf("shimlisten: restoring session %s's read deadline after claiming it: %w", sessionID, err)
	}
	return nil
}

// Next yields sessionID's connection, waiting for it to dial in if it has not
// yet. It returns a PARKED connection immediately when one is present, which
// is the daemon-restart case: survivors connect before anything claims them.
func (s *Server) Next(ctx context.Context, sessionID string) (*Conn, error) {
	for {
		c, err := s.takeParked(sessionID)
		if err != nil {
			return nil, err
		}
		if c != nil {
			return c, nil
		}
		c, err = s.waitForConnection(ctx, sessionID)
		if err != nil {
			return nil, err
		}
		if c != nil {
			return c, nil
		}
	}
}

// takeParked atomically claims a parked connection only after a non-consuming
// kernel probe proves its peer has not disconnected. A dead entry is evicted
// and nil is returned so Next can wait for the shim's replacement dial.
func (s *Server) takeParked(sessionID string) (*Conn, error) {
	for {
		s.mu.Lock()
		if s.closed {
			s.mu.Unlock()
			return nil, fmt.Errorf("shimlisten: server closed")
		}
		c := s.parked[sessionID]
		s.mu.Unlock()
		if c == nil {
			return nil, nil
		}
		open, err := connectionOpen(c.Net)
		if err != nil {
			return nil, fmt.Errorf("shimlisten: probing parked session %s connection: %w", sessionID, err)
		}
		if !open {
			s.evictIfCurrent(sessionID, c, "peer_disconnected_before_claim")
			continue
		}
		s.mu.Lock()
		if s.parked[sessionID] == c {
			delete(s.parked, sessionID)
			// THE CLAIM IS RECORDED IN THE SAME ACQUISITION THAT REMOVES THE
			// PARK, so there is no instant in which this session's connection
			// is in neither index and `Connected` answers "no shim" about a
			// shim that is right here.
			s.claimed[sessionID] = c
			s.mu.Unlock()
			if err := s.awaitWatchExit(sessionID, c); err != nil {
				return nil, err
			}
			s.logf("shimlisten: parked lifecycle session=%s decision=claim connection_state=open", sessionID)
			return c, nil
		}
		s.mu.Unlock()
		// The parked generation changed during the probe. Re-evaluate the new
		// connection instead of claiming an identity the map no longer owns.
	}
}

func (s *Server) waitForConnection(ctx context.Context, sessionID string) (*Conn, error) {
	s.mu.Lock()
	if s.closed {
		s.mu.Unlock()
		return nil, fmt.Errorf("shimlisten: server closed")
	}
	// A connection may have landed between takeParked and this lock. Re-run
	// the liveness proof rather than bypassing it under the waiter path.
	if _, ok := s.parked[sessionID]; ok {
		s.mu.Unlock()
		return nil, nil
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

// Connected reports whether a shim for sessionID has a USABLE CONNECTION to
// this daemon right now — parked awaiting a claim, or already claimed by a
// controller — with the peer proved live by the kernel in either case. A closed
// peer is dropped before false returns, so callers can never advertise a corpse
// as a live shim.
//
// IT IS DELIBERATELY BLIND TO WHICH CONTROLLER GENERATION HOLDS THE CLAIM.
// "A shim is connected for session S" is a fact about the transport and the
// process at the far end of it; which of this daemon's controllers happens to
// own the read side is a fact about the daemon. Answering the first question
// with the second is what let the workspace-ownership gate wait out and kill a
// shim that had dialled in, handshaked, and gone ready — under a generation
// that had since been retired (sessioncontroller survivingshim.go).
//
// WHAT IT STILL REFUSES is unchanged, because the refusals never rested on
// generation identity:
//
//   - A SUPERSEDED shim. Both indexes are keyed by the session id the shim
//     announced in its own ShimHello, and both hold at most ONE entry per
//     session: a redial supersedes the park and drops the claim, so only the
//     newest connection for a session is ever visible here.
//   - A FOREIGN shim. A connection announcing some other session is filed
//     under that session and is invisible to this lookup entirely.
//   - A DEAD shim. Every answer is re-derived from a kernel probe of the
//     socket, so a process that exited answers false however recently its
//     entry was written.
//
// This is the cheap half of the "is a shim alive?" question that
// ReattachDecision used to answer with a dial and a handshake read; the
// session lock covers the other half (a shim alive but not yet dialled in).
func (s *Server) Connected(sessionID string) (bool, error) {
	parked, err := s.parkedConnected(sessionID)
	if err != nil {
		return false, err
	}
	if parked {
		return true, nil
	}
	return s.claimedConnected(sessionID)
}

// parkedConnected answers Connected for a connection still awaiting a claim.
func (s *Server) parkedConnected(sessionID string) (bool, error) {
	for {
		s.mu.Lock()
		c := s.parked[sessionID]
		s.mu.Unlock()
		if c == nil {
			return false, nil
		}
		open, err := connectionOpen(c.Net)
		if err != nil {
			return false, fmt.Errorf("shimlisten: probing parked session %s connection: %w", sessionID, err)
		}
		if !open {
			if s.evictIfCurrent(sessionID, c, "peer_disconnected_while_parked") {
				return false, nil
			}
			continue
		}
		s.mu.Lock()
		current := s.parked[sessionID] == c
		s.mu.Unlock()
		if current {
			return true, nil
		}
	}
}

// claimedConnected answers Connected for a connection a controller already
// owns. It is the parked probe's twin in every respect except the one that
// matters: a dead entry is DROPPED, never closed, because the read side is not
// this listener's to close.
//
// A probe error is returned, never read as "not connected". Failing to observe
// a connection is not evidence of its absence, and the caller that reads this
// answer decides whether to kill the process at the other end of it.
func (s *Server) claimedConnected(sessionID string) (bool, error) {
	for {
		s.mu.Lock()
		c := s.claimed[sessionID]
		s.mu.Unlock()
		if c == nil {
			return false, nil
		}
		open, err := connectionOpen(c.Net)
		if err != nil {
			return false, fmt.Errorf("shimlisten: probing claimed session %s connection: %w", sessionID, err)
		}
		if !open {
			if s.dropClaimIfCurrent(sessionID, c, "peer_disconnected_while_claimed") {
				return false, nil
			}
			continue
		}
		s.mu.Lock()
		current := s.claimed[sessionID] == c
		s.mu.Unlock()
		if current {
			return true, nil
		}
	}
}

// dropClaimIfCurrent deregisters a claimed connection, and reports whether this
// call is the one that did it. The socket is left alone: its claimer owns the
// read side and closes it when it is done with it.
func (s *Server) dropClaimIfCurrent(sessionID string, c *Conn, reason string) bool {
	s.mu.Lock()
	if s.claimed[sessionID] != c {
		s.mu.Unlock()
		return false
	}
	delete(s.claimed, sessionID)
	s.mu.Unlock()
	s.logf("shimlisten: claimed lifecycle session=%s decision=drop reason=%s connection_state=not_closed_by_daemon", sessionID, reason)
	return true
}

// dropClaimLocked is dropClaimIfCurrent for a caller that already holds mu and
// is superseding whatever claim is recorded, whichever connection it names.
func (s *Server) dropClaimLocked(sessionID, reason string) {
	if _, ok := s.claimed[sessionID]; !ok {
		return
	}
	delete(s.claimed, sessionID)
	s.logf("shimlisten: claimed lifecycle session=%s decision=drop reason=%s connection_state=not_closed_by_daemon", sessionID, reason)
}

// Evict closes and removes sessionID's PARKED transport after an explicit
// lifecycle stop.
//
// It reaches the parked index and nothing else. A claimed connection is
// remembered (see Server.claimed) so that "is a shim connected?" can be
// answered honestly about it, but it is owned by the shimclient that took it,
// so this operation still cannot close an active controller's route. A claim
// whose peer the stop killed stops answering `Connected` at the next probe,
// which re-derives the answer from the kernel rather than from this index.
func (s *Server) Evict(sessionID, reason string) bool {
	s.mu.Lock()
	c := s.parked[sessionID]
	if c != nil {
		delete(s.parked, sessionID)
	}
	s.mu.Unlock()
	if c == nil {
		s.logf("shimlisten: parked lifecycle session=%s decision=no_entry reason=%s connection_state=absent", sessionID, reason)
		return false
	}
	_ = c.Net.Close()
	s.logf("shimlisten: parked lifecycle session=%s decision=evict reason=%s connection_state=closed_by_daemon", sessionID, reason)
	return true
}

func (s *Server) evictIfCurrent(sessionID string, c *Conn, reason string) bool {
	s.mu.Lock()
	if s.parked[sessionID] != c {
		s.mu.Unlock()
		return false
	}
	delete(s.parked, sessionID)
	s.mu.Unlock()
	_ = c.Net.Close()
	s.logf("shimlisten: parked lifecycle session=%s decision=evict reason=%s connection_state=closed", sessionID, reason)
	return true
}

// connectionOpen asks the kernel whether the peer has closed without consuming
// even one protocol byte. MSG_PEEK preserves any frame already waiting for the
// eventual shimclient owner. MSG_DONTWAIT makes this a state query rather than
// a timing probe or background poll.
func connectionOpen(conn net.Conn) (bool, error) {
	sc, ok := conn.(syscall.Conn)
	if !ok {
		return false, fmt.Errorf("connection type %T does not expose syscall state", conn)
	}
	raw, err := sc.SyscallConn()
	if err != nil {
		return false, err
	}
	open := false
	var probeErr error
	if err := raw.Control(func(fd uintptr) {
		var one [1]byte
		n, _, recvErr := unix.Recvfrom(int(fd), one[:], unix.MSG_PEEK|unix.MSG_DONTWAIT)
		switch {
		case recvErr == nil:
			open = n > 0
		case errors.Is(recvErr, unix.EAGAIN), errors.Is(recvErr, unix.EWOULDBLOCK):
			open = true
		case errors.Is(recvErr, unix.ECONNRESET), errors.Is(recvErr, unix.ENOTCONN):
			open = false
		default:
			probeErr = recvErr
		}
	}); err != nil {
		return false, err
	}
	if probeErr != nil {
		return false, probeErr
	}
	return open, nil
}

// Close stops accepting and drops every parked connection.
//
// CLAIMED CONNECTIONS ARE FORGOTTEN, NOT CLOSED. The daemon's preserve-on-
// shutdown contract turns on a claimed shim outliving this process, redialling
// and parking for the next boot; closing one here would be this listener
// severing a route it does not own, on the exact path where the shim is meant
// to be kept.
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
	s.claimed = map[string]*Conn{}
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
//
// The frame read stays split from the envelope decode here, unlike the plain
// wire.ReadAny call sites: this one WRAPS the frame error with what it was
// trying to read, and nothing about the listener's error handling should change
// as a side effect of sharing the decode half.
func readHello(conn net.Conn) (*corev1.ShimHello, error) {
	payload, err := wire.ReadFrame(conn)
	if err != nil {
		return nil, fmt.Errorf("reading hello frame: %w", err)
	}
	msg, err := wire.UnmarshalAny(payload)
	if err != nil {
		return nil, err
	}
	hello, ok := msg.(*corev1.ShimHello)
	if !ok {
		return nil, fmt.Errorf("first frame was %T, expected ShimHello", msg)
	}
	return hello, nil
}
