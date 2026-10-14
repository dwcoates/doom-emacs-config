// Package hangup lets a server's handler wait for its peer to hang up the
// very connection a request came in on.
//
// A HANG-UP is the read side of the request's connection ending: the peer's
// EOF, or any other read failure but a deadline.
//
// A SHIM EXITS ON ITS DAEMON'S HANG-UP, NOT ON ITS ANSWER LEAVING. A shim that
// ended its process the moment its KillSession answer reached the socket left
// the daemon's http2 transport writing to a dead peer: an RST_STREAM or a
// WINDOW_UPDATE hit EPIPE, and `golang.org/x/net/http2`'s ClientConn closed
// itself on that write error and aborted every in-flight stream, the
// KillSession whose answer was still unread in the socket buffer among them.
// The daemon recorded "the session kill did not answer" with cause
// `http2: client connection force closed via ClientConn.Close`. The daemon
// hangs up only once it has READ the answer, so an exit ordered after the
// hang-up cannot overtake it. No timer: the hang-up comes from the daemon
// closing the connection or from the daemon dying, and the daemon's stop
// escalation covers a shim that never sees either.
//
// A server is wired with both Listener and ConnContext; Of then answers for
// any request it serves.
package hangup

import (
	"context"
	"errors"
	"net"
	"os"
	"sync"
)

// hangUpKey is the context key a request's connection rides under.
type hangUpKey struct{}

// Listener wraps a listener so every connection it accepts reports its
// hang-up.
func Listener(inner net.Listener) net.Listener {
	return &hangUpListener{Listener: inner}
}

// ConnContext is the http.Server's ConnContext: it puts the accepted
// connection where Of finds it. Every request served on the connection,
// h2c's included (h2c serves its hijacked connection under the request
// context the server built), carries it.
func ConnContext(ctx context.Context, c net.Conn) context.Context {
	conn, ok := c.(*hangUpConn)
	if !ok {
		panic("hangup: ConnContext was given a connection Listener did not accept")
	}
	return context.WithValue(ctx, hangUpKey{}, conn)
}

// Of answers a channel closed once the peer has hung up the connection
// ctx's request came in on.
//
// A request context without one is a server wired without Listener
// and ConnContext, which is a wiring defect, so it panics.
func Of(ctx context.Context) <-chan struct{} {
	conn, ok := ctx.Value(hangUpKey{}).(*hangUpConn)
	if !ok {
		panic("hangup: Of a request whose server was not wired with Listener and ConnContext")
	}
	return conn.gone
}

type hangUpListener struct {
	net.Listener
}

func (l *hangUpListener) Accept() (net.Conn, error) {
	c, err := l.Listener.Accept()
	if err != nil {
		return nil, err
	}
	return &hangUpConn{Conn: c, gone: make(chan struct{})}, nil
}

// hangUpConn closes gone the first time a Read fails for any reason but a
// deadline: the peer's EOF, or the connection closed under the reader.
//
// A DEADLINE IS NOT A HANG-UP. net/http stops its background read with a read
// deadline already past when h2c hijacks the connection, so every h2c
// connection's first read fails with a timeout while its peer is still there.
type hangUpConn struct {
	net.Conn
	gone chan struct{}
	once sync.Once
}

func (c *hangUpConn) Read(p []byte) (int, error) {
	n, err := c.Conn.Read(p)
	if err != nil && !errors.Is(err, os.ErrDeadlineExceeded) {
		c.once.Do(func() { close(c.gone) })
	}
	return n, err
}
