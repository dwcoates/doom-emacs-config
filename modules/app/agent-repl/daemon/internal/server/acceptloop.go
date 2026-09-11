package server

import (
	"errors"
	"net"
	"syscall"
	"time"

	"claude-repld/internal/dlog"
)

// acceptBackoffFloor and acceptBackoffCeiling bound the wait between retries of
// a transient accept failure.
//
// NOT A THROTTLE ON HEALTHY TRAFFIC: nothing waits here unless Accept has
// actually failed. What the wait covers is the one condition that produces a
// storm of them — the process is out of file descriptors — where retrying at
// full speed spins a core and writes a record per attempt without making a
// descriptor available. Doubling from 5ms to a second reaches the ceiling in
// eight attempts, so a momentary exhaustion costs milliseconds and a standing
// one costs one record a second.
const (
	acceptBackoffFloor   = 5 * time.Millisecond
	acceptBackoffCeiling = time.Second
)

// RetryAccept wraps a listener so a TRANSIENT accept failure is reported at
// ERROR and retried with capped backoff, and anything else ends the loop.
//
// THE LOOP MUST NEVER END QUIETLY. `http.Server.Serve` returns on an accept
// error it does not itself consider temporary, and the daemon's only caller
// then returns that error and the process exits — which is the correct
// outcome, because Emacs respawns a daemon that exited and cannot tell a
// listening-but-dead one from a healthy one. What was missing is the RECORD: a
// serve loop that ended on EMFILE, or a listener closed underneath the process,
// left nothing in the run log to say so.
//
// Descriptor exhaustion is the transient case and it is the one that matters
// here: a per-workspace log sink, a shim socket, a watch stream and a store
// connection all hold descriptors, and a leak in any of them surfaces as EMFILE
// at Accept — on the one path whose failure stops the daemon answering anything
// at all.
func RetryAccept(inner net.Listener, log dlog.Logger) net.Listener {
	return &retryAccept{Listener: inner, log: log}
}

// retryAccept is RetryAccept's listener.
type retryAccept struct {
	net.Listener
	log dlog.Logger
	// backoff is the current wait, reset by every accepted connection.
	backoff time.Duration
}

// Accept answers the next connection, retrying the failures that a later
// attempt can succeed at.
func (l *retryAccept) Accept() (net.Conn, error) {
	for attempt := 0; ; attempt++ {
		conn, err := l.Listener.Accept()
		if err == nil {
			l.backoff = 0
			return conn, nil
		}
		// THE DAEMON'S OWN EXIT CLOSES THIS LISTENER. `http.Server.Shutdown`
		// closes it before the process leaves, so net.ErrClosed here is the
		// orderly end of the loop and not a fault: reported at ERROR it would
		// put a record against every clean shutdown in the suite and in
		// production. It is still RETURNED, because Serve must end.
		if errors.Is(err, net.ErrClosed) {
			l.log.Debug("daemon.server.accept", "the listener was closed; the accept loop is ending", dlog.Context{
				"addr":    addrText(l.Listener),
				"attempt": attempt,
			})
			return nil, err
		}
		if !transientAcceptError(err) {
			l.log.Error("daemon.server.accept", "the listener stopped accepting connections", dlog.Context{
				"addr":    addrText(l.Listener),
				"attempt": attempt,
				"error":   err.Error(),
			})
			return nil, err
		}
		if l.backoff == 0 {
			l.backoff = acceptBackoffFloor
		} else if l.backoff < acceptBackoffCeiling {
			l.backoff *= 2
			if l.backoff > acceptBackoffCeiling {
				l.backoff = acceptBackoffCeiling
			}
		}
		l.log.Error("daemon.server.accept", "a connection could not be accepted; retrying", dlog.Context{
			"addr":       addrText(l.Listener),
			"attempt":    attempt,
			"backoff_ms": l.backoff.Milliseconds(),
			"error":      err.Error(),
		})
		time.Sleep(l.backoff)
	}
}

// transientAcceptError reports whether a later Accept can succeed where this
// one failed.
//
// The errnos are named EXPLICITLY rather than read off net.Error.Temporary,
// which is deprecated and which reports true for conditions that are not
// transient at all. Each of these says the kernel could not hand over THIS
// connection, never that the listener is gone:
//
//   - EMFILE, ENFILE: this process, or the host, is out of descriptors.
//   - ENOBUFS, ENOMEM: the kernel could not allocate for the new socket.
//   - ECONNABORTED, ECONNRESET: the peer left between the SYN and the accept.
//   - EINTR: a signal landed in the syscall.
//   - EAGAIN: the runtime's poller was woken with nothing ready.
func transientAcceptError(err error) bool {
	for _, errno := range []syscall.Errno{
		syscall.EMFILE, syscall.ENFILE,
		syscall.ENOBUFS, syscall.ENOMEM,
		syscall.ECONNABORTED, syscall.ECONNRESET,
		syscall.EINTR, syscall.EAGAIN,
	} {
		if errors.Is(err, errno) {
			return true
		}
	}
	return false
}

// addrText names a listener's address for a record, and says so when it has
// none rather than panicking inside the error path.
func addrText(l net.Listener) string {
	if l == nil {
		return ""
	}
	addr := l.Addr()
	if addr == nil {
		return ""
	}
	return addr.String()
}
