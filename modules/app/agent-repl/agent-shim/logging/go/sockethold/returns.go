package sockethold

import (
	"bufio"
	"context"
	"errors"
	"fmt"
	"io"
	"net"
	"os"
	"strings"
	"sync"
	"syscall"
	"time"
)

// A CONNECTION ACCEPTED ON A HELD SOCKET OUTLIVES THE SERVER THAT ACCEPTED IT.
// A server stopping hands back every connection it accepted and never read a
// byte of (Return); the holder keeps each for the server accepting on that
// socket next, and hands it over on that server's accept stream (Subscribe).
// The client's bytes are still unread in the kernel, so the next server reads
// the call as if it had accepted the connection itself, and nothing a client
// sent is lost to a handover (2026-11-21: an adoption call to the shared
// address hit a broken pipe, accepted by the departing daemon and closed
// before it was read).
//
// Two requests carry this, each one line naming its socket:
//
//   - "return NAME", with the connection's fd attached, answered StatusOK once
//     the holder has it (or StatusRefused and a reason).
//   - "accept NAME", which stays open: the holder sends StatusOK with one fd
//     attached for every connection handed back, queued ones first, until the
//     client half-closes (Returned.Stop), when the holder ends the stream. A
//     newer stream for NAME ends the older one.

const (
	verbReturn = "return "
	verbAccept = "accept "
)

// Return hands the connection FILE, accepted on the held socket NAME and
// never read, to the holder serving CONTROL, for the server accepting on NAME
// next. CTX bounds the exchange. The caller still closes FILE: the holder
// keeps a duplicate.
func Return(ctx context.Context, control, name string, file *os.File) error {
	if err := checkName(name); err != nil {
		return err
	}
	uc, err := dial(ctx, control)
	if err != nil {
		return err
	}
	defer uc.Close()
	if _, _, err := uc.WriteMsgUnix([]byte(verbReturn+name+"\n"), syscall.UnixRights(int(file.Fd())), nil); err != nil {
		return fmt.Errorf("sockethold: hand %s a connection of %q: %w", control, name, err)
	}
	reader := bufio.NewReader(uc)
	status, err := reader.ReadByte()
	if err != nil {
		return fmt.Errorf("sockethold: read %s's answer to a connection of %q: %w", control, name, err)
	}
	switch status {
	case StatusOK:
		return nil
	case StatusRefused:
		reason, _ := reader.ReadString('\n')
		return fmt.Errorf("%w: return %s: %s", ErrRefused, name, strings.TrimSpace(reason))
	}
	return fmt.Errorf("sockethold: %s answered a connection of %q with status %q", control, name, status)
}

// Returned is one server's accept stream of the connections handed back for
// a held socket.
type Returned struct {
	uc   *net.UnixConn
	name string
}

// Subscribe opens the accept stream of the held socket NAME at the holder
// serving CONTROL. CTX bounds opening it only; the stream lasts until Stop.
func Subscribe(ctx context.Context, control, name string) (*Returned, error) {
	if err := checkName(name); err != nil {
		return nil, err
	}
	uc, err := dial(ctx, control)
	if err != nil {
		return nil, err
	}
	if _, err := uc.Write([]byte(verbAccept + name + "\n")); err != nil {
		uc.Close()
		return nil, fmt.Errorf("sockethold: open %s's accept stream of %q: %w", control, name, err)
	}
	// THE STREAM OUTLIVES THE OPENING'S BOUND: only the request was bounded.
	if err := uc.SetDeadline(timeZero); err != nil {
		uc.Close()
		return nil, fmt.Errorf("sockethold: unbound %s's accept stream of %q: %w", control, name, err)
	}
	return &Returned{uc: uc, name: name}, nil
}

// Next answers the next connection handed back, blocking until the holder
// sends one. io.EOF is the stream's end: the holder ended it after Stop, or
// for a newer stream of the socket. A refusal (the holder holds no such
// socket) answers ErrRefused.
func (r *Returned) Next() (net.Conn, error) {
	buf := make([]byte, 1)
	oob := make([]byte, syscall.CmsgSpace(4))
	n, oobn, _, _, err := r.uc.ReadMsgUnix(buf, oob)
	if err != nil {
		return nil, fmt.Errorf("sockethold: read the accept stream of %q: %w", r.name, err)
	}
	if n == 0 {
		return nil, io.EOF
	}
	if buf[0] == StatusRefused {
		reason, _ := bufio.NewReader(r.uc).ReadString('\n')
		return nil, fmt.Errorf("%w: accept %s: %s", ErrRefused, r.name, strings.TrimSpace(reason))
	}
	if buf[0] != StatusOK {
		return nil, fmt.Errorf("sockethold: the accept stream of %q sent status %q", r.name, buf[0])
	}
	msgs, err := syscall.ParseSocketControlMessage(oob[:oobn])
	if err != nil || len(msgs) != 1 {
		return nil, fmt.Errorf("sockethold: the accept stream of %q sent no readable fd: %v", r.name, err)
	}
	fds, err := syscall.ParseUnixRights(&msgs[0])
	if err != nil || len(fds) != 1 {
		return nil, fmt.Errorf("sockethold: the accept stream of %q sent %d fds: %v", r.name, len(fds), err)
	}
	file := os.NewFile(uintptr(fds[0]), r.name)
	defer file.Close()
	conn, err := net.FileConn(file)
	if err != nil {
		return nil, fmt.Errorf("sockethold: the accept stream of %q sent a fd that is no connection: %w", r.name, err)
	}
	return conn, nil
}

// Stop asks the holder to end the stream: it sends nothing more, and Next
// answers what was already on its way, then io.EOF.
func (r *Returned) Stop() error {
	if err := r.uc.CloseWrite(); err != nil {
		return fmt.Errorf("sockethold: end the accept stream of %q: %w", r.name, err)
	}
	return nil
}

// Close releases the stream.
func (r *Returned) Close() error { return r.uc.Close() }

// checkName refuses a socket name no request can carry.
func checkName(name string) error {
	if strings.ContainsAny(name, "\n\x00") || name == "" {
		return fmt.Errorf("sockethold: socket name %q is empty or holds a line break", name)
	}
	return nil
}

// returns is the holder's state of the connections handed back: those
// waiting, per socket, and each socket's current accept stream.
type returns struct {
	mu      sync.Mutex
	waiting map[string][]*os.File
	streams map[string]*stream
}

// stream is one server's accept stream, which sends one connection at a time.
type stream struct {
	conn *net.UnixConn
	send sync.Mutex
	done bool
}

func newReturns() *returns {
	return &returns{waiting: map[string][]*os.File{}, streams: map[string]*stream{}}
}

// put keeps FILE for NAME: sent on its stream when one stands, else queued.
func (r *returns) put(name string, file *os.File) {
	r.mu.Lock()
	s := r.streams[name]
	if s == nil {
		r.waiting[name] = append(r.waiting[name], file)
		r.mu.Unlock()
		return
	}
	r.mu.Unlock()
	if !r.deliver(name, s, file) {
		r.mu.Lock()
		r.waiting[name] = append(r.waiting[name], file)
		r.mu.Unlock()
	}
}

// deliver sends FILE on S and closes the holder's copy, answering false when
// S has ended (the file stays the holder's).
func (r *returns) deliver(name string, s *stream, file *os.File) bool {
	s.send.Lock()
	defer s.send.Unlock()
	if s.done {
		return false
	}
	if _, _, err := s.conn.WriteMsgUnix([]byte{StatusOK}, syscall.UnixRights(int(file.Fd())), nil); err != nil {
		s.done = true
		return false
	}
	file.Close()
	return true
}

// subscribe makes CONN NAME's accept stream, ending the one before it, sends
// every connection waiting, and holds the stream until its client
// half-closes or the holder stops.
func (r *returns) subscribe(ctx context.Context, name string, conn *net.UnixConn) {
	s := &stream{conn: conn}
	r.mu.Lock()
	older := r.streams[name]
	r.streams[name] = s
	waiting := r.waiting[name]
	delete(r.waiting, name)
	r.mu.Unlock()
	if older != nil {
		r.end(older)
	}
	for i, file := range waiting {
		if !r.deliver(name, s, file) {
			r.mu.Lock()
			r.waiting[name] = append(waiting[i:], r.waiting[name]...)
			r.mu.Unlock()
			break
		}
	}
	// THE CLIENT'S HALF-CLOSE IS THE STREAM'S END: its read sees EOF.
	ended := make(chan struct{})
	stop := context.AfterFunc(ctx, func() { conn.Close() })
	defer stop()
	go func() {
		_, _ = io.Copy(io.Discard, conn)
		close(ended)
	}()
	<-ended
	r.mu.Lock()
	if r.streams[name] == s {
		delete(r.streams, name)
	}
	r.mu.Unlock()
	r.end(s)
}

// end ends S: nothing more is sent, and its client reads EOF.
func (r *returns) end(s *stream) {
	s.send.Lock()
	defer s.send.Unlock()
	if s.done {
		return
	}
	s.done = true
	_ = s.conn.CloseWrite()
}

// closeAll closes every connection still waiting, at the holder's exit.
func (r *returns) closeAll() {
	r.mu.Lock()
	defer r.mu.Unlock()
	for name, files := range r.waiting {
		for _, f := range files {
			f.Close()
		}
		delete(r.waiting, name)
	}
}

// timeZero clears a deadline.
var timeZero time.Time

// errNoFD is a return that carried no connection.
var errNoFD = errors.New("the request carried no connection")
