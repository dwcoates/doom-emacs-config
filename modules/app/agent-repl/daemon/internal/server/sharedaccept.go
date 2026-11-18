package server

import (
	"errors"
	"net"
	"sync"
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
	conns   chan acceptResult
	done    chan struct{}
	once    sync.Once
	stop    sync.Once
	wg      sync.WaitGroup
	mu      sync.Mutex
	stopped bool
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

// AddShared starts accepting on SHARED, the listener taken from the holder
// under the boot claim. A second shared listener is a programming error.
func (l *SharedAccept) AddShared(shared net.Listener) {
	l.mu.Lock()
	if l.shared != nil {
		l.mu.Unlock()
		panic("server: SharedAccept was given a second shared listener")
	}
	l.shared = shared
	l.mu.Unlock()
	l.wg.Add(1)
	// inflight: joined: Close waits for every pump.
	go l.pump(shared, true)
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
		shared := l.shared
		l.mu.Unlock()
		if shared != nil {
			err = shared.Close()
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
		l.wg.Wait()
	})
	return err
}

// Addr is the private listener's address.
func (l *SharedAccept) Addr() net.Addr { return l.private.Addr() }
