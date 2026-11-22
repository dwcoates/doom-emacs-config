package server

import (
	"errors"
	"io"
	"net"
	"sync"
	"testing"

	"agent-repld/internal/dlog"
)

// sharedPair answers a listener standing for the holder's shared socket and a
// second copy of it, as a successor would take it: two listeners on one
// socket.
func sharedPair(t *testing.T) (net.Listener, net.Listener) {
	t.Helper()
	first, err := net.Listen("tcp", "127.0.0.1:0")
	if err != nil {
		t.Fatal(err)
	}
	f, err := first.(*net.TCPListener).File()
	if err != nil {
		t.Fatal(err)
	}
	defer f.Close()
	second, err := net.FileListener(f)
	if err != nil {
		t.Fatal(err)
	}
	return first, second
}

func dialTCP(t *testing.T, addr net.Addr) net.Conn {
	t.Helper()
	c, err := net.Dial("tcp", addr.String())
	if err != nil {
		t.Fatalf("dial %s: %v", addr, err)
	}
	t.Cleanup(func() { c.Close() })
	return c
}

func TestSharedAcceptAcceptsOnBothListeners(t *testing.T) {
	// Arrange
	private, err := net.Listen("tcp", "127.0.0.1:0")
	if err != nil {
		t.Fatal(err)
	}
	shared, successorCopy := sharedPair(t)
	defer successorCopy.Close()
	l := NewSharedAccept(private)
	l.AddShared(shared, newFakeReturned(), dlog.NewTestLogger())
	defer l.Close()

	// Act
	dialTCP(t, private.Addr())
	dialTCP(t, shared.Addr())
	first, err1 := l.Accept()
	second, err2 := l.Accept()

	// Assert
	if err1 != nil || err2 != nil {
		t.Fatalf("Accept = %v, %v; want both connections", err1, err2)
	}
	first.Close()
	second.Close()
}

func TestAStoppedSharedListenerLeavesItsConnectsToTheNextDaemon(t *testing.T) {
	// Arrange
	private, err := net.Listen("tcp", "127.0.0.1:0")
	if err != nil {
		t.Fatal(err)
	}
	shared, successorCopy := sharedPair(t)
	defer successorCopy.Close()
	sharedAddr := shared.Addr()
	l := NewSharedAccept(private)
	l.AddShared(shared, newFakeReturned(), dlog.NewTestLogger())
	defer l.Close()

	// Act
	stopErr := l.StopShared()
	dialTCP(t, sharedAddr)
	conn, acceptErr := successorCopy.Accept()
	dialTCP(t, private.Addr())
	own, ownErr := l.Accept()

	// Assert: the next daemon accepted the shared connect, and this one still
	// accepts on its private listener.
	if stopErr != nil {
		t.Fatalf("StopShared: %v", stopErr)
	}
	if acceptErr != nil {
		t.Fatalf("the next daemon's accept = %v, want the queued connect", acceptErr)
	}
	conn.Close()
	if ownErr != nil {
		t.Fatalf("the private accept after the stop = %v, want it served", ownErr)
	}
	own.Close()
}

func TestSharedAcceptClosesBoth(t *testing.T) {
	// Arrange
	private, err := net.Listen("tcp", "127.0.0.1:0")
	if err != nil {
		t.Fatal(err)
	}
	l := NewSharedAccept(private)

	// Act
	closeErr := l.Close()
	_, acceptErr := l.Accept()

	// Assert
	if closeErr != nil {
		t.Fatalf("Close: %v", closeErr)
	}
	if !errors.Is(acceptErr, net.ErrClosed) {
		t.Fatalf("Accept after Close = %v, want net.ErrClosed", acceptErr)
	}
	if l.StopShared() != nil {
		t.Fatal("StopShared with no shared listener must be a no-op")
	}
}

// fakeReturned is a holder's accept stream the test feeds: Next answers each
// connection put on conns, then io.EOF once the stream is stopped.
type fakeReturned struct {
	conns chan net.Conn
	stop  chan struct{}
	once  sync.Once
}

func newFakeReturned() *fakeReturned {
	return &fakeReturned{conns: make(chan net.Conn, 4), stop: make(chan struct{})}
}

func (f *fakeReturned) Next() (net.Conn, error) {
	select {
	case c := <-f.conns:
		return c, nil
	case <-f.stop:
		return nil, io.EOF
	}
}

func (f *fakeReturned) Stop() error { f.once.Do(func() { close(f.stop) }); return nil }

func (f *fakeReturned) Close() error { return f.Stop() }
