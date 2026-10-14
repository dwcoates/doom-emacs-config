package hangup

import (
	"context"
	"errors"
	"io"
	"net"
	"os"
	"testing"
	"time"
)

// pipeListener accepts one end of a net.Pipe once, then answers err.
type pipeListener struct {
	conns chan net.Conn
	err   error
}

func (l *pipeListener) Accept() (net.Conn, error) {
	select {
	case c := <-l.conns:
		return c, nil
	default:
		return nil, l.err
	}
}
func (l *pipeListener) Close() error   { return nil }
func (l *pipeListener) Addr() net.Addr { return nil }

// acceptedPipe answers a connection Listener accepted, its request context, and
// the peer's end.
func acceptedPipe(t *testing.T) (net.Conn, context.Context, net.Conn) {
	t.Helper()
	local, peer := net.Pipe()
	t.Cleanup(func() { _ = local.Close(); _ = peer.Close() })
	inner := &pipeListener{conns: make(chan net.Conn, 1)}
	inner.conns <- local
	conn, err := Listener(inner).Accept()
	if err != nil {
		t.Fatalf("Accept() error = %v", err)
	}
	return conn, ConnContext(context.Background(), conn), peer
}

// hangUpWait bounds a wait the test expects to end at once.
const hangUpWait = 5 * time.Second

func TestHungUpIsOpenWhileThePeerSpeaks(t *testing.T) {
	// Arrange.
	conn, ctx, peer := acceptedPipe(t)
	go func() { _, _ = peer.Write([]byte("x")) }()

	// Act.
	_, err := conn.Read(make([]byte, 1))

	// Assert.
	if err != nil {
		t.Fatalf("Read() error = %v", err)
	}
	select {
	case <-Of(ctx):
		t.Fatal("Of() closed while the peer was still connected")
	default:
	}
}

func TestHungUpClosesOnThePeersEOF(t *testing.T) {
	// Arrange.
	conn, ctx, peer := acceptedPipe(t)

	// Act.
	if err := peer.Close(); err != nil {
		t.Fatalf("peer Close() error = %v", err)
	}
	_, err := conn.Read(make([]byte, 1))

	// Assert.
	if !errors.Is(err, io.EOF) {
		t.Fatalf("Read() error = %v, want io.EOF", err)
	}
	select {
	case <-Of(ctx):
	case <-time.After(hangUpWait):
		t.Fatal("Of() stayed open after the peer hung up")
	}
}

func TestHungUpIsOpenAfterAReadDeadlinePasses(t *testing.T) {
	// Arrange.
	conn, ctx, _ := acceptedPipe(t)
	if err := conn.SetReadDeadline(time.Unix(1, 0)); err != nil {
		t.Fatalf("SetReadDeadline() error = %v", err)
	}

	// Act.
	_, err := conn.Read(make([]byte, 1))

	// Assert.
	if !errors.Is(err, os.ErrDeadlineExceeded) {
		t.Fatalf("Read() error = %v, want os.ErrDeadlineExceeded", err)
	}
	select {
	case <-Of(ctx):
		t.Fatal("Of() closed on a passed read deadline, with the peer still connected")
	default:
	}
}

func TestHungUpClosesOnceForRepeatedFailedReads(t *testing.T) {
	// Arrange.
	conn, ctx, peer := acceptedPipe(t)
	_ = peer.Close()
	_, _ = conn.Read(make([]byte, 1))

	// Act.
	_, err := conn.Read(make([]byte, 1))

	// Assert.
	if err == nil {
		t.Fatal("a second Read() after the hang-up succeeded")
	}
	<-Of(ctx)
}

func TestHangUpListenerAnswersTheInnerAcceptError(t *testing.T) {
	// Arrange.
	want := errors.New("accept failed")
	l := Listener(&pipeListener{conns: make(chan net.Conn), err: want})

	// Act.
	_, err := l.Accept()

	// Assert.
	if !errors.Is(err, want) {
		t.Fatalf("Accept() error = %v, want %v", err, want)
	}
}

func TestConnContextPanicsOnAConnectionItsListenerDidNotAccept(t *testing.T) {
	// Arrange.
	local, peer := net.Pipe()
	defer local.Close()
	defer peer.Close()
	defer func() {
		// Assert.
		if recover() == nil {
			t.Fatal("ConnContext() accepted a foreign connection")
		}
	}()

	// Act.
	ConnContext(context.Background(), local)
}

func TestHungUpPanicsOnARequestWithoutAConnection(t *testing.T) {
	// Arrange.
	defer func() {
		// Assert.
		if recover() == nil {
			t.Fatal("Of() answered for a context no Listener and ConnContext wiring made")
		}
	}()

	// Act.
	Of(context.Background())
}
