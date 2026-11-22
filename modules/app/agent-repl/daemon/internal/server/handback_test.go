package server

import (
	"errors"
	"io"
	"net"
	"os"
	"testing"

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
