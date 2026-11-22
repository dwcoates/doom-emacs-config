package server

import (
	"errors"
	"io"
	"net"
	"net/http"
	"os"
	"testing"
	"time"

	"agent-repld/internal/dlog"
)

// tcpPair answers a client's end and the server's accepted end of one
// loopback connection.
func tcpPair(t *testing.T) (client, server net.Conn) {
	t.Helper()
	ln, err := net.Listen("tcp", "127.0.0.1:0")
	if err != nil {
		t.Fatal(err)
	}
	defer ln.Close()
	client, err = net.Dial("tcp", ln.Addr().String())
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { client.Close() })
	server, err = ln.Accept()
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { server.Close() })
	return client, server
}

// keep is a give that keeps the handed file as a connection, for the test to
// read the client's call from, as the next daemon would.
func keep(t *testing.T, into *net.Conn) func(*os.File) error {
	return func(f *os.File) error {
		conn, err := net.FileConn(f)
		if err != nil {
			return err
		}
		t.Cleanup(func() { conn.Close() })
		*into = conn
		return nil
	}
}

// callReaches asserts CONN reads what CLIENT writes.
func callReaches(t *testing.T, client, conn net.Conn) {
	t.Helper()
	if _, err := client.Write([]byte("call")); err != nil {
		t.Fatalf("the client's write: %v", err)
	}
	buf := make([]byte, 4)
	if _, err := io.ReadFull(conn, buf); err != nil || string(buf) != "call" {
		t.Fatalf("read %q (%v), want the client's call", buf, err)
	}
}

func TestHandBackUnread(t *testing.T) {
	cases := []struct {
		name    string
		arrange func(t *testing.T, client net.Conn, c *sharedConn) net.Conn
		want    HandBackOutcome
	}{
		{name: "a private connection is left as it was", arrange: func(_ *testing.T, _ net.Conn, c *sharedConn) net.Conn { return c.Conn }, want: NotShared},
		{name: "an unread shared connection is handed back", arrange: func(_ *testing.T, _ net.Conn, c *sharedConn) net.Conn { return c }, want: HandedBack},
		{name: "a shared connection read from is this daemon's", arrange: func(t *testing.T, client net.Conn, c *sharedConn) net.Conn {
			if _, err := client.Write([]byte("x")); err != nil {
				t.Fatal(err)
			}
			if _, err := io.ReadFull(c, make([]byte, 1)); err != nil {
				t.Fatal(err)
			}
			return c
		}, want: ReadFrom},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			client, accepted := tcpPair(t)
			c := newSharedConn(accepted)
			conn := tc.arrange(t, client, c)
			var next net.Conn

			// Act
			got, err := HandBackUnread(conn, keep(t, &next))

			// Assert
			if err != nil || got != tc.want {
				t.Fatalf("HandBackUnread = (%v, %v), want %v", got, err, tc.want)
			}
			switch got {
			case HandedBack:
				callReaches(t, client, next)
				if _, err := c.Read(make([]byte, 1)); err == nil {
					t.Fatal("the handed-back connection still reads here")
				}
			case ReadFrom:
				callReaches(t, client, c)
			}
		})
	}
}

// exchangeBound bounds a loopback exchange; it guards against a hang and is
// never a wait.
const exchangeBound = 5 * time.Second

// oneConn is a listener that accepts CONN once, then nothing until closed.
type oneConn struct {
	conn   net.Conn
	given  bool
	closed chan struct{}
}

func (l *oneConn) Accept() (net.Conn, error) {
	if !l.given {
		l.given = true
		return l.conn, nil
	}
	<-l.closed
	return nil, net.ErrClosed
}

func (l *oneConn) Close() error   { close(l.closed); return nil }
func (l *oneConn) Addr() net.Addr { return l.conn.LocalAddr() }

// serveUntilClosed serves CONN with a real http.Server until net/http has
// closed it, which the server's ConnState hook tells.
func serveUntilClosed(t *testing.T, conn net.Conn) {
	t.Helper()
	ended := make(chan struct{})
	srv := &http.Server{
		Handler: http.HandlerFunc(func(w http.ResponseWriter, _ *http.Request) { w.Write([]byte("departing")) }),
		ConnState: func(_ net.Conn, state http.ConnState) {
			if state == http.StateClosed {
				close(ended)
			}
		},
	}
	l := &oneConn{conn: conn, closed: make(chan struct{})}
	served := make(chan error, 1)
	go func() { served <- srv.Serve(l) }()
	<-ended
	if err := srv.Close(); err != nil {
		t.Fatalf("close the server: %v", err)
	}
	if err := <-served; !errors.Is(err, http.ErrServerClosed) {
		t.Fatalf("Serve = %v, want its close", err)
	}
}

func TestAHandedBackConnectionLeavesItsSocketToTheNextDaemon(t *testing.T) {
	cases := []struct {
		name string
		// act does one thing on the handed-back connection C, or on NEXT,
		// the next daemon's, and answers its error. whileGiving runs it
		// while the holder is being given the connection: decided, and this
		// daemon's descriptor not yet closed.
		act         func(t *testing.T, c *sharedConn, next, client net.Conn) error
		whileGiving bool
		wantErr     func(error) bool
	}{
		{name: "net/http serving it here answers the client nothing", whileGiving: true, act: func(t *testing.T, c *sharedConn, _, _ net.Conn) error {
			serveUntilClosed(t, c)
			return nil
		}, wantErr: func(err error) bool { return err == nil }},
		{name: "a write here fails without a byte written", whileGiving: true, act: func(_ *testing.T, c *sharedConn, _, _ net.Conn) error {
			n, err := c.Write([]byte("departing"))
			if n != 0 {
				return errors.New("wrote bytes")
			}
			return err
		}, wantErr: func(err error) bool {
			var op *net.OpError
			return errors.As(err, &op) && op.Op == "write" && errors.Is(err, net.ErrClosed)
		}},
		{name: "the next daemon's descriptor reads the call after the original closed", act: func(_ *testing.T, _ *sharedConn, next, client net.Conn) error {
			if _, err := client.Write([]byte("call")); err != nil {
				return err
			}
			buf := make([]byte, 4)
			if _, err := io.ReadFull(next, buf); err != nil {
				return err
			}
			if string(buf) != "call" {
				return errors.New("read " + string(buf))
			}
			return nil
		}, wantErr: func(err error) bool { return err == nil }},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			client, accepted := tcpPair(t)
			c := newSharedConn(accepted)
			if err := client.SetReadDeadline(time.Now().Add(exchangeBound)); err != nil {
				t.Fatal(err)
			}
			var next net.Conn
			var actErr error
			give := func(f *os.File) error {
				if err := keep(t, &next)(f); err != nil {
					return err
				}
				if tc.whileGiving {
					actErr = tc.act(t, c, next, client)
				}
				return nil
			}

			// Act
			got, err := HandBackUnread(c, give)
			if err != nil || got != HandedBack {
				t.Fatalf("HandBackUnread = (%v, %v), want HandedBack", got, err)
			}
			if !tc.whileGiving {
				actErr = tc.act(t, c, next, client)
			}

			// Assert: the client receives the next daemon's answer and not a
			// byte before it.
			if !tc.wantErr(actErr) {
				t.Fatalf("act = %v", actErr)
			}
			if _, err := next.Write([]byte("answer")); err != nil {
				t.Fatalf("the next daemon's answer: %v", err)
			}
			if err := next.Close(); err != nil {
				t.Fatal(err)
			}
			received, err := io.ReadAll(client)
			if err != nil || string(received) != "answer" {
				t.Fatalf("the client received %q (%v), want only the next daemon's answer", received, err)
			}
		})
	}
}

func TestAReadInTheKernelIsCutAndTheConnectionHandedBack(t *testing.T) {
	// Arrange: the server is blocked reading a connection its client has not
	// written to yet.
	client, accepted := tcpPair(t)
	c := newSharedConn(accepted)
	readErr := make(chan error, 1)
	inKernel := make(chan struct{})
	go func() {
		c.mu.Lock()
		for !c.reading {
			c.cond.Wait()
		}
		c.mu.Unlock()
		close(inKernel)
	}()
	go func() {
		_, err := c.Read(make([]byte, 1))
		readErr <- err
	}()
	<-inKernel
	var next net.Conn

	// Act
	got, err := HandBackUnread(c, keep(t, &next))

	// Assert: the blocked read ended with nothing, and the call reaches the
	// next daemon.
	if err != nil || got != HandedBack {
		t.Fatalf("HandBackUnread = (%v, %v), want HandedBack", got, err)
	}
	if err := <-readErr; err == nil {
		t.Fatal("the blocked read answered a byte, want its cut")
	}
	callReaches(t, client, next)
}

func TestAFailedHandBackClosesTheConnection(t *testing.T) {
	// Arrange
	_, accepted := tcpPair(t)
	c := newSharedConn(accepted)
	refused := errors.New("the holder refused")

	// Act
	got, err := HandBackUnread(c, func(*os.File) error { return refused })

	// Assert
	if !errors.Is(err, refused) || got != HandedBack {
		t.Fatalf("HandBackUnread = (%v, %v), want the refusal", got, err)
	}
	if _, err := accepted.Read(make([]byte, 1)); err == nil {
		t.Fatal("the connection is still open after a failed hand-back")
	}
}

func TestAHandedBackConnectionIsAcceptedFromTheStream(t *testing.T) {
	// Arrange
	private, err := net.Listen("tcp", "127.0.0.1:0")
	if err != nil {
		t.Fatal(err)
	}
	shared, successorCopy := sharedPair(t)
	defer successorCopy.Close()
	returned := newFakeReturned()
	log := dlog.NewTestLogger()
	l := NewSharedAccept(private)
	l.AddShared(shared, returned, log)
	defer l.Close()
	client, handed := tcpPair(t)

	// Act
	returned.conns <- handed
	conn, acceptErr := l.Accept()
	stopErr := l.StopShared()

	// Assert: accepted as a connection of the shared address, and the
	// stream's end recorded once stopped.
	if acceptErr != nil || stopErr != nil {
		t.Fatalf("Accept = %v, StopShared = %v", acceptErr, stopErr)
	}
	if _, ok := conn.(*sharedConn); !ok {
		t.Fatalf("the accepted connection is a %T, want a shared one", conn)
	}
	callReaches(t, client, conn)
	l.Close()
	if _, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: "info", Message: "the holder ended its stream of handed-back connections"}); !ok {
		t.Fatalf("records = %+v, want the stream's end recorded", log.Records())
	}
}

// addressless is a connection with no remote address, as net.FileConn makes
// one whose peer reset before it was handed over.
type addressless struct{ net.Conn }

func (addressless) RemoteAddr() net.Addr { return nil }

func TestRemoteAddress(t *testing.T) {
	cases := []struct {
		name string
		conn func(t *testing.T) (conn net.Conn, want string)
	}{
		{name: "a connection with a remote address answers it", conn: func(t *testing.T) (net.Conn, string) {
			client, accepted := tcpPair(t)
			return accepted, client.LocalAddr().String()
		}},
		{name: "a connection with no remote address answers unknown", conn: func(t *testing.T) (net.Conn, string) {
			_, accepted := tcpPair(t)
			return addressless{accepted}, "unknown"
		}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			conn, want := tc.conn(t)

			// Act
			got := RemoteAddress(conn)

			// Assert
			if got != want {
				t.Fatalf("RemoteAddress = %q, want %q", got, want)
			}
		})
	}
}
