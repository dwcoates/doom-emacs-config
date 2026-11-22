package server

import (
	"errors"
	"io"
	"net"
	"sync"
	"testing"
	"time"

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
	if err := l.AddShared(shared, newFakeReturned(), dlog.NewTestLogger()); err != nil {
		t.Fatalf("AddShared: %v", err)
	}
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
	if err := l.AddShared(shared, newFakeReturned(), dlog.NewTestLogger()); err != nil {
		t.Fatalf("AddShared: %v", err)
	}
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

// TestAStoppedSharedListenerNeverAcceptsAgain pins that the accept right on
// the shared address, once given up, is never taken back: an AddShared after
// StopShared adds nothing and starts no accept loop, while one before it
// still serves.
func TestAStoppedSharedListenerNeverAcceptsAgain(t *testing.T) {
	cases := []struct {
		name      string
		stopFirst bool
		wantErr   error
	}{
		{name: "an add before the stop accepts on the shared listener"},
		{name: "an add after the stop is refused and adds nothing", stopFirst: true, wantErr: ErrSharedStopped},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			private, err := net.Listen("tcp", "127.0.0.1:0")
			if err != nil {
				t.Fatal(err)
			}
			shared, err := net.Listen("tcp", "127.0.0.1:0")
			if err != nil {
				t.Fatal(err)
			}
			defer shared.Close()
			l := NewSharedAccept(private)
			defer l.Close()
			if tc.stopFirst {
				if err := l.StopShared(); err != nil {
					t.Fatalf("StopShared: %v", err)
				}
			}

			// Act
			addErr := l.AddShared(shared, newFakeReturned(), dlog.NewTestLogger())

			// Assert
			if !errors.Is(addErr, tc.wantErr) || (tc.wantErr == nil && addErr != nil) {
				t.Fatalf("AddShared = %v, want %v", addErr, tc.wantErr)
			}
			l.mu.Lock()
			added := l.shared != nil || l.returned != nil
			l.mu.Unlock()
			if added == tc.stopFirst {
				t.Fatalf("shared listener added = %v after a stop first = %v", added, tc.stopFirst)
			}
			if tc.stopFirst {
				return
			}
			dialTCP(t, shared.Addr())
			conn, err := l.Accept()
			if err != nil {
				t.Fatalf("Accept on the added shared listener = %v", err)
			}
			conn.Close()
		})
	}
}

// TestAStopRacingAnAddLeavesNoSharedAccept pins that the add and the stop
// are one critical section: whichever wins, no shared listener is left
// accepting once both returned. The add either landed first, and the stop
// closed it, or was refused.
func TestAStopRacingAnAddLeavesNoSharedAccept(t *testing.T) {
	for round := 0; round < 32; round++ {
		// Arrange
		private, err := net.Listen("tcp", "127.0.0.1:0")
		if err != nil {
			t.Fatal(err)
		}
		shared, err := net.Listen("tcp", "127.0.0.1:0")
		if err != nil {
			t.Fatal(err)
		}
		l := NewSharedAccept(private)
		start := make(chan struct{})
		var wg sync.WaitGroup
		var addErr, stopErr error
		wg.Add(2)
		go func() {
			defer wg.Done()
			<-start
			addErr = l.AddShared(shared, newFakeReturned(), dlog.NewTestLogger())
		}()
		go func() { defer wg.Done(); <-start; stopErr = l.StopShared() }()

		// Act
		close(start)
		wg.Wait()

		// Assert
		if stopErr != nil {
			t.Fatalf("round %d: StopShared = %v", round, stopErr)
		}
		switch {
		case addErr == nil:
			// The add won: the stop must have closed the listener it added
			// (a deadline set on an open listener succeeds; on a closed one
			// it answers net.ErrClosed, without blocking either way).
			if err := shared.(*net.TCPListener).SetDeadline(time.Now()); !errors.Is(err, net.ErrClosed) {
				t.Fatalf("round %d: the added shared listener is still open after the stop (SetDeadline = %v)", round, err)
			}
		case errors.Is(addErr, ErrSharedStopped):
			l.mu.Lock()
			added := l.shared != nil
			l.mu.Unlock()
			if added {
				t.Fatalf("round %d: a refused add left a shared listener", round)
			}
			shared.Close()
		default:
			t.Fatalf("round %d: AddShared = %v, want nil or ErrSharedStopped", round, addErr)
		}
		l.Close()
	}
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
