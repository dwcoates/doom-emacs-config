package server

import (
	"errors"
	"fmt"
	"net"
	"os"
	"sync"
	"time"
)

// A CONNECTION THIS DAEMON ACCEPTED ON THE SHARED ADDRESS AND NEVER READ IS
// THE NEXT DAEMON'S. A departing daemon used to close one as requestless, and
// a client that had dialed it and was about to write its call hit a broken
// pipe (2026-11-21, an adoption call during a handover). Every connection the
// shared listener, or the holder's accept stream, hands this daemon is a
// sharedConn, which counts what was read of it; at the exit, one nothing was
// ever read from goes back to the holder (HandBackUnread), which hands it to
// the daemon accepting next, with the client's bytes still in the kernel.

// sharedConn is one connection taken from the shared address.
//
// ONCE HANDED BACK, THIS DAEMON DOES NOTHING TO THE SOCKET. The socket is
// the next daemon's from the decision on, and every one of this daemon's
// methods that could reach it is cut: Read fails with a closed connection's
// read error (net/http answers a read error of its own connection nothing,
// where an error of no known kind is answered 400 Bad Request, written on
// the client's socket ahead of the next daemon's answer: 2026-11-21); Write
// fails without writing a byte; and Close closes only this daemon's
// descriptor. Nothing that acts on the socket rather than the descriptor
// (CloseWrite, CloseRead, SetLinger, File, SyscallConn) is exposed at all:
// only net.Conn's methods are promoted from the interface it embeds.
type sharedConn struct {
	net.Conn

	mu   sync.Mutex
	cond *sync.Cond
	// read and wrote are how many bytes were ever read and written; reading
	// and writing are whether a Read or a Write is in the kernel now.
	read    int64
	wrote   int64
	reading bool
	writing bool
	// deciding holds every new Read and Write while a hand-back is decided,
	// so none can move bytes between the decision and the hand-back; handed
	// is the decision to hand it back, after which every Read and Write
	// fails.
	deciding bool
	handed   bool

	// closeOnce closes this daemon's descriptor once, whichever of net/http
	// and the hand-back closes first; closeErr is that close's result,
	// answered to both.
	closeOnce sync.Once
	closeErr  error
}

func newSharedConn(conn net.Conn) *sharedConn {
	c := &sharedConn{Conn: conn}
	c.cond = sync.NewCond(&c.mu)
	return c
}

func (c *sharedConn) Read(p []byte) (int, error) {
	if err := c.begin("read", &c.reading); err != nil {
		return 0, err
	}
	n, err := c.Conn.Read(p)
	c.end(&c.reading, &c.read, n)
	return n, err
}

func (c *sharedConn) Write(p []byte) (int, error) {
	if err := c.begin("write", &c.writing); err != nil {
		return 0, err
	}
	n, err := c.Conn.Write(p)
	c.end(&c.writing, &c.wrote, n)
	return n, err
}

// Close closes this daemon's descriptor, and only it: the socket stays open
// on every duplicate, the next daemon's included. Net/http closes a
// connection whose Read the hand-back cut while the hand-back closes it too;
// the first close is the one, and both are answered its result.
func (c *sharedConn) Close() error {
	c.closeOnce.Do(func() { c.closeErr = c.Conn.Close() })
	return c.closeErr
}

// begin starts a Read or a Write (OP), marking BUSY, once no hand-back is
// being decided; on a handed-back connection it answers OP's failure on a
// closed connection instead.
func (c *sharedConn) begin(op string, busy *bool) error {
	c.mu.Lock()
	defer c.mu.Unlock()
	for c.deciding {
		c.cond.Wait()
	}
	if c.handed {
		return c.handedError(op)
	}
	*busy = true
	return nil
}

// end ends a Read or a Write that moved N bytes, counted in COUNT.
func (c *sharedConn) end(busy *bool, count *int64, n int) {
	c.mu.Lock()
	*count += int64(n)
	*busy = false
	c.cond.Broadcast()
	c.mu.Unlock()
}

// handedError is OP's failure on a handed-back connection: a *net.OpError
// of net.ErrClosed, the error a closed connection's own Read and Write
// answer, which net/http's serve loop recognizes as a read error of its
// connection and answers with nothing.
func (c *sharedConn) handedError(op string) error {
	e := &net.OpError{Op: op, Source: c.Conn.LocalAddr(), Addr: c.Conn.RemoteAddr(), Err: net.ErrClosed}
	if e.Source != nil {
		e.Net = e.Source.Network()
	}
	return e
}

// errReadAlready is a hand-back declined: the connection was read from or
// written to, so its call is this daemon's to answer.
var errReadAlready = errors.New("server: the connection was read from; its call is this daemon's")

// handBack gives the connection to GIVE when no byte of it was ever read or
// written, and closes it here. A Read or a Write in the kernel is ended
// first by a past deadline, and none starts while the decision is made, so
// the decision is on everything ever moved. A connection read from is left
// as it was, and answered errReadAlready.
func (c *sharedConn) handBack(give func(*os.File) error) error {
	filer, ok := c.Conn.(interface{ File() (*os.File, error) })
	if !ok {
		return fmt.Errorf("server: a shared connection of type %T has no descriptor to hand back", c.Conn)
	}
	c.mu.Lock()
	if c.read > 0 || c.wrote > 0 || c.handed {
		c.mu.Unlock()
		return errReadAlready
	}
	c.deciding = true
	c.mu.Unlock()
	// THE DESCRIPTOR IS DUPLICATED FIRST: the interrupted Read's failure lets
	// net/http close the connection, and this copy outlives that close.
	file, err := filer.File()
	if err != nil {
		c.decided(false)
		return fmt.Errorf("server: duplicate the shared connection's descriptor: %w", err)
	}
	defer file.Close()
	if err := c.Conn.SetDeadline(time.Unix(1, 0)); err != nil {
		c.decided(false)
		return fmt.Errorf("server: interrupt the shared connection's read and write: %w", err)
	}
	c.mu.Lock()
	for c.reading || c.writing {
		c.cond.Wait()
	}
	unread := c.read == 0 && c.wrote == 0
	c.mu.Unlock()
	if !unread {
		c.decided(false)
		if err := c.Conn.SetDeadline(time.Time{}); err != nil {
			return fmt.Errorf("server: restore the shared connection's read and write: %w", err)
		}
		return errReadAlready
	}
	c.decided(true)
	if err := give(file); err != nil {
		return errors.Join(err, c.Close())
	}
	return c.Close()
}

// decided ends the hand-back's decision: HANDED, or the connection reads on.
func (c *sharedConn) decided(handed bool) {
	c.mu.Lock()
	c.handed = handed
	c.deciding = false
	c.cond.Broadcast()
	c.mu.Unlock()
}

// unknownRemote is the remote address of a connection that has none.
const unknownRemote = "unknown"

// RemoteAddress answers CONN's remote address for a record, or "unknown" when
// it has none.
//
// A CONNECTION HANDED OVER BY THE HOLDER CAN HAVE NO REMOTE ADDRESS.
// sockethold.Returned.Next makes it with net.FileConn, which asks the kernel
// for the peer's address (getpeername) and, when that fails, leaves
// RemoteAddr nil rather than failing; on darwin getpeername fails with EINVAL
// once the client's end reset the connection before it was handed over
// (2026-11-22: a successor's requestless close of such a connection
// dereferenced the nil address and crashed the daemon).
func RemoteAddress(conn net.Conn) string {
	addr := conn.RemoteAddr()
	if addr == nil {
		return unknownRemote
	}
	return addr.String()
}

// HandBackOutcome is what HandBackUnread did with a connection.
type HandBackOutcome int

const (
	// NotShared is a connection of the private listener, left as it was.
	NotShared HandBackOutcome = iota
	// ReadFrom is a shared connection something was read from: its call is
	// this daemon's, and it is left to be answered.
	ReadFrom
	// HandedBack is a shared connection nothing was read from, now the
	// holder's for the daemon accepting next.
	HandedBack
)

// HandBackUnread gives CONN to GIVE (the holder's Return) when it is a
// connection taken from the shared address that nothing was ever read from.
// A failed hand-back closes the connection and answers the error.
func HandBackUnread(conn net.Conn, give func(*os.File) error) (HandBackOutcome, error) {
	if b, ok := conn.(*barrierConn); ok {
		conn = b.Conn
	}
	shared, ok := conn.(*sharedConn)
	if !ok {
		return NotShared, nil
	}
	if err := shared.handBack(give); err != nil {
		if errors.Is(err, errReadAlready) {
			return ReadFrom, nil
		}
		return HandedBack, err
	}
	return HandedBack, nil
}
