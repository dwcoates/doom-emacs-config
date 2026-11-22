package server

import (
	"errors"
	"io"
	"net"
	"sync"

	"agent-repld/internal/dlog"
)

// SharedAccept is the daemon's listener: its own private one, which every
// forwarded call reaches it on, and the shared one its holder keeps, which
// only the daemon holding the boot claim accepts on
// (docs/investigations/2026-11-18-stable-sockets-design.md, D2).
//
// THE SHARED LISTENER STOPS BEFORE THE CLAIM GOES. StopShared closes this
// daemon's copy of it while the private listener keeps accepting: from then
// on a connect to the shared address waits in the kernel's backlog for the
// daemon that takes the claim next, and none reaches this one.
type SharedAccept struct {
	private net.Listener
	shared  net.Listener
	// returned is the holder's accept stream of the connections a departing
	// daemon handed back (HandBackUnread), nil until AddShared.
	returned Returned
	conns    chan acceptResult
	done     chan struct{}
	once     sync.Once
	stop     sync.Once
	wg       sync.WaitGroup
	mu       sync.Mutex
	stopped  bool
}

type acceptResult struct {
	conn net.Conn
	err  error
}

// NewSharedAccept serves PRIVATE; AddShared adds the shared listener once
// this daemon holds the boot claim.
func NewSharedAccept(private net.Listener) *SharedAccept {
	l := &SharedAccept{private: private, conns: make(chan acceptResult), done: make(chan struct{})}
	l.wg.Add(1)
	// inflight: joined: Close waits for every pump.
	go l.pump(private, false)
	return l
}

// Returned is the holder's accept stream of handed-back connections
// (sockethold.Returned).
type Returned interface {
	Next() (net.Conn, error)
	Stop() error
	Close() error
}

// ErrSharedStopped marks an AddShared refused because StopShared already
// ran: this daemon gave up its accept right on the shared address, and a
// listener added after that would take connects meant for the daemon that
// accepts next.
var ErrSharedStopped = errors.New("the shared listener was stopped, so this daemon accepts on it no more")

// AddShared starts accepting on SHARED, the listener taken from the holder
// under the boot claim, and on RETURNED, the holder's stream of the
// connections a departing daemon handed back. LOG records the stream's end.
// A second shared listener is a programming error.
//
// A STOPPED SHARED LISTENER NEVER ACCEPTS AGAIN. Once StopShared ran,
// AddShared adds nothing, starts no accept loop and answers ErrSharedStopped;
// the check and the add are under l.mu, as StopShared's stop is, so an add
// racing the stop either lands first, and the stop closes it, or is refused.
// No healthy path adds after a stop: the stop is the shutdown's first step
// (beginShutdown) or the handover's yield (yieldBootClaim), and both end
// this daemon's accept right for good.
func (l *SharedAccept) AddShared(shared net.Listener, returned Returned, log dlog.Logger) error {
	l.mu.Lock()
	if l.stopped {
		l.mu.Unlock()
		return ErrSharedStopped
	}
	if l.shared != nil {
		l.mu.Unlock()
		panic("server: SharedAccept was given a second shared listener")
	}
	l.shared = shared
	l.returned = returned
	// Counted under l.mu, before a stop can see the listener: Close stops
	// under this lock and then waits, so its wait never starts ahead of
	// these two pumps' count.
	l.wg.Add(2)
	l.mu.Unlock()
	// inflight: joined: Close waits for every pump.
	go l.pump(shared, true)
	// inflight: joined: Close waits for every pump.
	go l.pumpReturned(returned, log)
	return nil
}

// pumpReturned feeds every handed-back connection to Accept, wrapped as one
// taken from the shared address, until the holder ends the stream. A stream
// that fails is recorded at ERROR and ends: the shared listener goes on.
func (l *SharedAccept) pumpReturned(returned Returned, log dlog.Logger) {
	defer l.wg.Done()
	for {
		conn, err := returned.Next()
		if errors.Is(err, io.EOF) {
			log.Info("daemon.server.returned", "the holder ended its stream of handed-back connections", nil)
			return
		}
		if err != nil {
			if l.sharedStopped() {
				return
			}
			log.Error("daemon.server.returned", "the holder's stream of handed-back connections failed; none more reach this daemon", dlog.Context{"error": err.Error()})
			return
		}
		select {
		case l.conns <- acceptResult{conn: newSharedConn(conn)}:
		case <-l.done:
			conn.Close()
			return
		}
	}
}

// pump feeds IN's connections to Accept until IN fails. The shared
// listener's failure after StopShared is its stop, not an error.
func (l *SharedAccept) pump(in net.Listener, shared bool) {
	defer l.wg.Done()
	for {
		conn, err := in.Accept()
		if err != nil && shared && l.sharedStopped() {
			return
		}
		if err == nil && shared {
			conn = newSharedConn(conn)
		}
		select {
		case l.conns <- acceptResult{conn: conn, err: err}:
		case <-l.done:
			if conn != nil {
				conn.Close()
			}
			return
		}
		if err != nil {
			return
		}
	}
}

func (l *SharedAccept) sharedStopped() bool {
	l.mu.Lock()
	defer l.mu.Unlock()
	return l.stopped
}

// Accept answers the next connection from either listener, or the first
// failure of either that is not the shared listener's stop.
func (l *SharedAccept) Accept() (net.Conn, error) {
	select {
	case r := <-l.conns:
		return r.conn, r.err
	case <-l.done:
		return nil, net.ErrClosed
	}
}

// StopShared closes this daemon's copy of the shared listener; the private
// one goes on. It is idempotent, and a no-op with no shared listener.
func (l *SharedAccept) StopShared() error {
	var err error
	l.stop.Do(func() {
		l.mu.Lock()
		l.stopped = true
		shared, returned := l.shared, l.returned
		l.mu.Unlock()
		if shared != nil {
			err = shared.Close()
		}
		// THE STREAM ENDS WITH THE ACCEPT RIGHT: the holder sends nothing
		// more, and what it sent already is still served here.
		if returned != nil {
			err = errors.Join(err, returned.Stop())
		}
	})
	return err
}

// Close stops both listeners.
func (l *SharedAccept) Close() error {
	var err error
	l.once.Do(func() {
		err = errors.Join(l.StopShared(), l.private.Close())
		close(l.done)
		l.mu.Lock()
		returned := l.returned
		l.mu.Unlock()
		if returned != nil {
			err = errors.Join(err, returned.Close())
		}
		l.wg.Wait()
	})
	return err
}

// Addr is the private listener's address.
func (l *SharedAccept) Addr() net.Addr { return l.private.Addr() }
