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
type sharedConn struct {
	net.Conn

	mu   sync.Mutex
	cond *sync.Cond
	// read is how many bytes were ever read; reading is whether a Read is in
	// the kernel now.
	read    int64
	reading bool
	// deciding holds every new Read while a hand-back is decided, so none
	// can consume bytes between the decision and the hand-back; handed is
	// the decision to hand it back, after which every Read fails.
	deciding bool
	handed   bool
}

func newSharedConn(conn net.Conn) *sharedConn {
	c := &sharedConn{Conn: conn}
	c.cond = sync.NewCond(&c.mu)
	return c
}

func (c *sharedConn) Read(p []byte) (int, error) {
	c.mu.Lock()
	for c.deciding {
		c.cond.Wait()
	}
	if c.handed {
		c.mu.Unlock()
		return 0, net.ErrClosed
	}
	c.reading = true
	c.cond.Broadcast()
	c.mu.Unlock()
	n, err := c.Conn.Read(p)
	c.mu.Lock()
	c.read += int64(n)
	c.reading = false
	c.cond.Broadcast()
	c.mu.Unlock()
	return n, err
}

// errReadAlready is a hand-back declined: the connection was read from, so
// its call is this daemon's to answer.
var errReadAlready = errors.New("server: the connection was read from; its call is this daemon's")

// handBack gives the connection to GIVE when no byte of it was ever read,
// and closes it here. A Read in the kernel is ended first by a past
// deadline, and none starts while the decision is made, so the decision is
// on everything ever read. A connection read from is left as it was, and
// answered errReadAlready.
func (c *sharedConn) handBack(give func(*os.File) error) error {
	filer, ok := c.Conn.(interface{ File() (*os.File, error) })
	if !ok {
		return fmt.Errorf("server: a shared connection of type %T has no descriptor to hand back", c.Conn)
	}
	c.mu.Lock()
	if c.read > 0 || c.handed {
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
	if err := c.Conn.SetReadDeadline(time.Unix(1, 0)); err != nil {
		c.decided(false)
		return fmt.Errorf("server: interrupt the shared connection's read: %w", err)
	}
	c.mu.Lock()
	for c.reading {
		c.cond.Wait()
	}
	unread := c.read == 0
	c.mu.Unlock()
	if !unread {
		c.decided(false)
		if err := c.Conn.SetReadDeadline(time.Time{}); err != nil {
			return fmt.Errorf("server: restore the shared connection's read: %w", err)
		}
		return errReadAlready
	}
	c.decided(true)
	if err := give(file); err != nil {
		c.Conn.Close()
		return err
	}
	return c.Conn.Close()
}

// decided ends the hand-back's decision: HANDED, or the connection reads on.
func (c *sharedConn) decided(handed bool) {
	c.mu.Lock()
	c.handed = handed
	c.deciding = false
	c.cond.Broadcast()
	c.mu.Unlock()
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
