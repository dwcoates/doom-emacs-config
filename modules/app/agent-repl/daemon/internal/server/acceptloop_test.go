package server

import (
	"errors"
	"net"
	"sync"
	"syscall"
	"testing"

	"claude-repld/internal/dlog"
)

// scriptedListener answers Accept from a script, one entry per call.
type scriptedListener struct {
	mu      sync.Mutex
	answers []error
	calls   int
}

func (l *scriptedListener) Accept() (net.Conn, error) {
	l.mu.Lock()
	defer l.mu.Unlock()
	if l.calls >= len(l.answers) {
		l.calls++
		return nil, errors.New("the script ran out")
	}
	err := l.answers[l.calls]
	l.calls++
	if err != nil {
		return nil, err
	}
	return &net.TCPConn{}, nil
}

func (l *scriptedListener) Close() error { return nil }

func (l *scriptedListener) Addr() net.Addr {
	return &net.TCPAddr{IP: net.IPv4(127, 0, 0, 1), Port: 1}
}

func (l *scriptedListener) attempts() int {
	l.mu.Lock()
	defer l.mu.Unlock()
	return l.calls
}

// acceptError wraps an errno the way the net package does, so the retry
// decision is made on the shape a real accept failure has.
func acceptError(errno syscall.Errno) error {
	return &net.OpError{Op: "accept", Net: "tcp", Err: errno}
}

// TestDescriptorExhaustionIsRetried pins the failure that would otherwise end
// the daemon's one accept loop: a leak anywhere — a per-workspace log sink, a
// shim socket, a watch stream — surfaces as EMFILE at Accept, and a later
// attempt succeeds once a descriptor comes back.
func TestDescriptorExhaustionIsRetried(t *testing.T) {
	// Arrange.
	inner := &scriptedListener{answers: []error{acceptError(syscall.EMFILE), nil}}
	l := RetryAccept(inner, dlog.NewTestLogger())

	// Act.
	conn, err := l.Accept()

	// Assert.
	if err != nil {
		t.Fatalf("Accept = error %v, want the retried connection", err)
	}
	if conn == nil {
		t.Fatalf("Accept = nil connection, want the one the second attempt answered")
	}
	if got := inner.attempts(); got != 2 {
		t.Fatalf("Accept attempts = %d, want 2: the first failure must be retried", got)
	}
}

// TestARetriedAcceptFailureIsReportedAtError pins that the retry is LOUD. A
// silently retried EMFILE is a descriptor leak nobody learns about until the
// daemon stops answering.
func TestARetriedAcceptFailureIsReportedAtError(t *testing.T) {
	// Arrange.
	log := dlog.NewTestLogger()
	inner := &scriptedListener{answers: []error{acceptError(syscall.EMFILE), nil}}

	// Act.
	if _, err := RetryAccept(inner, log).Accept(); err != nil {
		t.Fatalf("Accept: %v", err)
	}

	// Assert.
	records := log.Records()
	if len(records) != 1 || records[0].Level != dlog.LevelError || records[0].Operation != "daemon.server.accept" {
		t.Fatalf("records = %+v, want one error daemon.server.accept record", records)
	}
}

// TestAClosedListenerEndsTheAcceptLoop pins the other half: a listener that is
// gone is NOT retried. Serve returns, the daemon's spine returns that error,
// and the process exits — which is what lets Emacs respawn it instead of
// leaving a process that listens and answers nothing.
func TestAClosedListenerEndsTheAcceptLoop(t *testing.T) {
	// Arrange.
	inner := &scriptedListener{answers: []error{net.ErrClosed}}

	// Act.
	_, err := RetryAccept(inner, dlog.NewTestLogger()).Accept()

	// Assert.
	if !errors.Is(err, net.ErrClosed) {
		t.Fatalf("Accept = error %v, want net.ErrClosed returned rather than retried", err)
	}
	if got := inner.attempts(); got != 1 {
		t.Fatalf("Accept attempts = %d, want 1: a closed listener is not retried", got)
	}
}

// TestTheDaemonsOwnShutdownIsNotAnAcceptFault pins that the orderly exit stays
// quiet. `http.Server.Shutdown` closes this listener on the way out, so an
// ERROR here would put a record against every clean shutdown.
func TestTheDaemonsOwnShutdownIsNotAnAcceptFault(t *testing.T) {
	// Arrange.
	log := dlog.NewTestLogger()
	inner := &scriptedListener{answers: []error{net.ErrClosed}}

	// Act.
	_, _ = RetryAccept(inner, log).Accept()

	// Assert.
	for _, record := range log.Records() {
		if record.Level == dlog.LevelError {
			t.Fatalf("records = %+v, want no error record for the daemon closing its own listener", log.Records())
		}
	}
}

// TestALostListenerIsReportedAtError pins the record for the end of the loop,
// which is the one thing pid 31984's run log could not have told anyone.
func TestALostListenerIsReportedAtError(t *testing.T) {
	// Arrange.
	log := dlog.NewTestLogger()
	inner := &scriptedListener{answers: []error{acceptError(syscall.EFAULT)}}

	// Act.
	_, _ = RetryAccept(inner, log).Accept()

	// Assert.
	records := log.Records()
	if len(records) != 1 || records[0].Level != dlog.LevelError {
		t.Fatalf("records = %+v, want one error record naming the lost listener", records)
	}
	if records[0].Message == "" {
		t.Fatalf("record = %+v, want a message", records[0])
	}
}

// TestAnAbandonedPeerIsRetried pins ECONNABORTED: the client left between the
// SYN and the accept, which says nothing about the listener.
func TestAnAbandonedPeerIsRetried(t *testing.T) {
	// Arrange.
	inner := &scriptedListener{answers: []error{acceptError(syscall.ECONNABORTED), nil}}

	// Act.
	if _, err := RetryAccept(inner, dlog.NewTestLogger()).Accept(); err != nil {
		t.Fatalf("Accept = error %v, want the retried connection", err)
	}

	// Assert.
	if got := inner.attempts(); got != 2 {
		t.Fatalf("Accept attempts = %d, want 2", got)
	}
}

// TestAnAcceptedConnectionResetsTheBackoff pins that a burst of failures does
// not leave the listener waiting a second before every later connection.
func TestAnAcceptedConnectionResetsTheBackoff(t *testing.T) {
	// Arrange.
	inner := &scriptedListener{answers: []error{acceptError(syscall.EMFILE), nil, acceptError(syscall.EMFILE), nil}}
	l := RetryAccept(inner, dlog.NewTestLogger()).(*retryAccept)

	// Act.
	if _, err := l.Accept(); err != nil {
		t.Fatalf("first Accept: %v", err)
	}
	first := l.backoff
	if _, err := l.Accept(); err != nil {
		t.Fatalf("second Accept: %v", err)
	}

	// Assert.
	if first != 0 {
		t.Fatalf("backoff after an accepted connection = %v, want it reset to zero", first)
	}
	if l.backoff != 0 {
		t.Fatalf("backoff after the second accepted connection = %v, want it reset to zero", l.backoff)
	}
}
