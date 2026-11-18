// Package sockethold hands a server the listening sockets a holder keeps open
// for it, so a socket outlives every restart of the server that accepts on it.
//
// THE SOCKET IS NEVER THE SERVER'S. A server that binds its own socket takes
// the path with it when it exits: at a store restart on 2026-11-18 store.sock
// did not exist for 10s, and every client failed with ENOENT. A holder (the
// sockethold program), whose service definition never changes, owns each
// socket for good; the server takes the listening fd from it at boot (Take)
// and accepts on it, and connects made while no server accepts queue in the
// kernel's backlog.
//
// The holder serves a control socket. A client writes the NAME of the socket
// it wants, one line, and reads one status byte: StatusOK with the socket's
// fd attached (SCM_RIGHTS), or StatusRefused followed by the reason, one
// line. One request per connection. An EMPTY line is a probe (Probe): the
// holder answers StatusOK with no fd, which proves it is loaded and serving.
package sockethold

import (
	"bufio"
	"context"
	"errors"
	"fmt"
	"net"
	"os"
	"strings"
	"syscall"
)

const (
	// StatusOK precedes a granted socket's fd.
	StatusOK byte = '1'
	// StatusRefused precedes the reason a request was refused.
	StatusRefused byte = '0'
)

// ErrRefused marks a request the holder answered with a refusal: it holds no
// socket of that name.
var ErrRefused = errors.New("sockethold: the holder refused the request")

// Take asks the holder serving CONTROL for the listening socket NAME and
// answers it as a listener. CTX bounds the whole exchange; the caller names
// the bound (a holder whose control socket launchd owns queues the connect
// until it accepts, so the wait is the holder's start).
func Take(ctx context.Context, control, name string) (net.Listener, error) {
	if strings.ContainsAny(name, "\n\x00") || name == "" {
		return nil, fmt.Errorf("sockethold: socket name %q is empty or holds a line break", name)
	}
	uc, err := dial(ctx, control)
	if err != nil {
		return nil, err
	}
	defer uc.Close()
	if _, err := uc.Write([]byte(name + "\n")); err != nil {
		return nil, fmt.Errorf("sockethold: ask %s for %q: %w", control, name, err)
	}
	buf := make([]byte, 1)
	oob := make([]byte, syscall.CmsgSpace(4))
	n, oobn, _, _, err := uc.ReadMsgUnix(buf, oob)
	if err != nil {
		return nil, fmt.Errorf("sockethold: read %s's answer for %q: %w", control, name, err)
	}
	if n != 1 {
		return nil, fmt.Errorf("sockethold: %s answered %q with no status", control, name)
	}
	if buf[0] == StatusRefused {
		reason, _ := bufio.NewReader(uc).ReadString('\n')
		return nil, fmt.Errorf("%w: %s: %s", ErrRefused, name, strings.TrimSpace(reason))
	}
	if buf[0] != StatusOK {
		return nil, fmt.Errorf("sockethold: %s answered %q with status %q", control, name, buf[0])
	}
	msgs, err := syscall.ParseSocketControlMessage(oob[:oobn])
	if err != nil || len(msgs) != 1 {
		return nil, fmt.Errorf("sockethold: %s granted %q with no readable fd: %v", control, name, err)
	}
	fds, err := syscall.ParseUnixRights(&msgs[0])
	if err != nil || len(fds) != 1 {
		return nil, fmt.Errorf("sockethold: %s granted %q with %d fds: %v", control, name, len(fds), err)
	}
	file := os.NewFile(uintptr(fds[0]), name)
	defer file.Close()
	ln, err := net.FileListener(file)
	if err != nil {
		return nil, fmt.Errorf("sockethold: %s granted %q a fd that is no listener: %w", control, name, err)
	}
	return ln, nil
}

// ErrNoHolder marks a control socket no holder serves: no socket file at
// its path, or one nothing listens on. That is a holder whose job is not
// loaded, which its installer brings up; any other failure is the holder's.
var ErrNoHolder = errors.New("sockethold: no holder serves the control socket")

// dial connects to the holder serving CONTROL, the exchange bounded by
// CTX's deadline. A control socket nothing serves answers ErrNoHolder.
func dial(ctx context.Context, control string) (*net.UnixConn, error) {
	var d net.Dialer
	conn, err := d.DialContext(ctx, "unix", control)
	if err != nil {
		if errors.Is(err, syscall.ENOENT) || errors.Is(err, syscall.ECONNREFUSED) {
			return nil, fmt.Errorf("%w: dial %s: %w", ErrNoHolder, control, err)
		}
		return nil, fmt.Errorf("sockethold: dial the holder's control socket %s: %w", control, err)
	}
	uc := conn.(*net.UnixConn)
	if deadline, ok := ctx.Deadline(); ok {
		if err := uc.SetDeadline(deadline); err != nil {
			uc.Close()
			return nil, fmt.Errorf("sockethold: bound the exchange with %s: %w", control, err)
		}
	}
	return uc, nil
}

// Probe proves a holder serves CONTROL: one empty request, answered StatusOK
// with no fd. It takes nothing, so any program may ask whether a holder it
// does not serve from is up. CTX bounds the whole exchange. A control socket
// nothing serves answers ErrNoHolder.
func Probe(ctx context.Context, control string) error {
	uc, err := dial(ctx, control)
	if err != nil {
		return err
	}
	defer uc.Close()
	if _, err := uc.Write([]byte("\n")); err != nil {
		return fmt.Errorf("sockethold: probe %s: %w", control, err)
	}
	buf := make([]byte, 1)
	n, err := uc.Read(buf)
	if err != nil {
		return fmt.Errorf("sockethold: read %s's answer to a probe: %w", control, err)
	}
	if n != 1 || buf[0] != StatusOK {
		return fmt.Errorf("sockethold: %s answered a probe with %q", control, buf[:n])
	}
	return nil
}

// Grant is one answered request, for the holder's record.
type Grant struct {
	// Name is the socket asked for, "" for a probe.
	Name string
	// Probe reports a probe (Probe), which asks for no socket.
	Probe bool
	// Err is why the request was not granted; nil when it was.
	Err error
}

// Serve answers every request on CONTROL with the socket of its name from
// SOCKETS, calling RECORD once per request, until CTX ends or CONTROL fails.
// The holder keeps every socket open: a grant duplicates the fd into the
// client, so a server's exit never closes it.
func Serve(ctx context.Context, control *net.UnixListener, sockets map[string]*os.File, record func(Grant)) error {
	if record == nil {
		panic("sockethold: Serve needs a record function")
	}
	stop := context.AfterFunc(ctx, func() { control.Close() })
	defer stop()
	for {
		conn, err := control.AcceptUnix()
		if err != nil {
			if ctx.Err() != nil {
				return nil
			}
			return fmt.Errorf("sockethold: accept on the control socket: %w", err)
		}
		record(answer(conn, sockets))
	}
}

// answer serves one request on CONN and closes it.
func answer(conn *net.UnixConn, sockets map[string]*os.File) Grant {
	defer conn.Close()
	line, err := bufio.NewReader(conn).ReadString('\n')
	if err != nil {
		return Grant{Err: fmt.Errorf("read the request: %w", err)}
	}
	name := strings.TrimSuffix(line, "\n")
	if name == "" {
		if _, err := conn.Write([]byte{StatusOK}); err != nil {
			return Grant{Probe: true, Err: fmt.Errorf("answer the probe: %w", err)}
		}
		return Grant{Probe: true}
	}
	file, ok := sockets[name]
	if !ok {
		reason := fmt.Sprintf("this holder holds no socket named %q", name)
		_, _ = conn.Write(append([]byte{StatusRefused}, []byte(reason+"\n")...))
		return Grant{Name: name, Err: errors.New(reason)}
	}
	if _, _, err := conn.WriteMsgUnix([]byte{StatusOK}, syscall.UnixRights(int(file.Fd())), nil); err != nil {
		return Grant{Name: name, Err: fmt.Errorf("send the socket: %w", err)}
	}
	return Grant{Name: name}
}
