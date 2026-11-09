package persistentwifi

import (
	"bufio"
	"context"
	"errors"
	"fmt"
	"net"
	"strings"
	"sync"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
)

// EnvLid replaces the operating system's lid with a fake one, in one of two
// forms: an absolute path is a Unix socket a test harness serves (SocketLid),
// and "open", "closed" or "none" is a lid fixed in that state (FixedLid), for
// a world whose daemon the harness cannot reach. It is the test knob that
// pairs with EnvToolsDir, so a daemon running against fake host tools never
// hears the real machine's lid (ConfigFromEnv refuses the one without the
// other).
const EnvLid = "AGENT_REPL_PERSISTENT_WIFI_LID"

// Lid is what the operating system says about the built-in display's lid.
type Lid int

// The lid states a LidWatcher reports.
const (
	// LidOpen is an open lid.
	LidOpen Lid = iota + 1
	// LidClosed is a closed lid.
	LidClosed
	// LidNone is a machine with no lid (a desktop Mac).
	LidNone
)

// String names the lid for the record and for a fake lid's lines.
func (l Lid) String() string {
	switch l {
	case LidOpen:
		return "open"
	case LidClosed:
		return "closed"
	case LidNone:
		return "none"
	}
	panic(fmt.Sprintf("persistentwifi: Lid %d has no name", int(l)))
}

// LidWatcher HEARS the lid: the operating system tells it of every change,
// so nothing polls.
type LidWatcher interface {
	// Watch subscribes to the lid's changes, THEN reads its state, and hands
	// heard that state followed by every change after it, in order and on one
	// goroutine, until ctx ends. Subscribing before reading is what makes a
	// change between the two impossible to miss: it is heard as a change
	// (possibly repeating the state read, which heard takes idempotently).
	// Watch answers nil once ctx ends, and an error when the subscription
	// could not be made or ended while ctx was live.
	Watch(ctx context.Context, heard func(Lid)) error
}

// lidKind is the lid as the controller holds it: what was heard, or why
// nothing is.
type lidKind int

const (
	// lidUnheard is the zero value: the watcher has not yet subscribed.
	lidUnheard lidKind = iota
	lidOpen
	lidClosed
	lidNone
	// lidUnwatched is a subscription that failed or ended; cause says why.
	lidUnwatched
)

// lidFact is the controller's lid.
type lidFact struct {
	kind  lidKind
	cause string
}

// heardLid is the fact a heard Lid states.
func heardLid(l Lid) lidFact {
	switch l {
	case LidOpen:
		return lidFact{kind: lidOpen}
	case LidClosed:
		return lidFact{kind: lidClosed}
	case LidNone:
		return lidFact{kind: lidNone}
	}
	panic(fmt.Sprintf("persistentwifi: a watcher heard an unnamed lid %d", int(l)))
}

// name is the fact for the record.
func (f lidFact) name() string {
	switch f.kind {
	case lidOpen:
		return "open"
	case lidClosed:
		return "closed"
	case lidNone:
		return "none"
	case lidUnwatched:
		return "unwatched"
	}
	return "unheard"
}

// stamp sets the standing's lid arm from the fact; an unheard lid leaves it
// unassigned.
func (f lidFact) stamp(s *agentreplv1.PersistentWifiState) {
	switch f.kind {
	case lidOpen:
		s.Lid = &agentreplv1.PersistentWifiState_LidOpen{LidOpen: &agentreplv1.PersistentWifiLidOpen{}}
	case lidClosed:
		s.Lid = &agentreplv1.PersistentWifiState_LidClosed{LidClosed: &agentreplv1.PersistentWifiLidClosed{}}
	case lidNone:
		s.Lid = &agentreplv1.PersistentWifiState_NoLid{NoLid: &agentreplv1.PersistentWifiNoLid{}}
	case lidUnwatched:
		s.Lid = &agentreplv1.PersistentWifiState_LidUnwatched{LidUnwatched: &agentreplv1.PersistentWifiLidUnwatched{Detail: f.cause}}
	default:
		s.Lid = nil
	}
}

// SocketLid is the LidWatcher over a Unix socket a harness serves: the
// server writes each connection the lid's state when it connects, then every
// change, one per line ("open", "closed" or "none"). EACH WATCH HAS A
// CONNECTION OF ITS OWN, so two daemons watching at once (a handover's
// predecessor and successor) never take each other's lines, and closing a
// watch's own connection is what wakes its read when ctx ends.
func SocketLid(path string) LidWatcher { return socketLid{path: path} }

type socketLid struct{ path string }

func (s socketLid) Watch(ctx context.Context, heard func(Lid)) error {
	var dialer net.Dialer
	conn, err := dialer.DialContext(ctx, "unix", s.path)
	if err != nil {
		if ctx.Err() != nil {
			return nil
		}
		return fmt.Errorf("connect to the lid socket: %w", err)
	}
	stopped := make(chan struct{})
	var closer sync.WaitGroup
	closer.Add(1)
	// inflight: joined: the function that starts it waits for it.
	go func() {
		defer closer.Done()
		select {
		case <-ctx.Done():
		case <-stopped:
		}
		_ = conn.Close()
	}()
	defer closer.Wait()
	defer close(stopped)
	sc := bufio.NewScanner(conn)
	for sc.Scan() {
		l, err := parseLidLine(sc.Text())
		if err != nil {
			return err
		}
		heard(l)
	}
	if ctx.Err() != nil {
		return nil
	}
	if err := sc.Err(); err != nil {
		return fmt.Errorf("read the lid socket: %w", err)
	}
	return errors.New("the lid socket ended")
}

// FixedLid is a lid heard once, in state l, that never changes.
func FixedLid(l Lid) LidWatcher { return fixedLid{lid: l} }

type fixedLid struct{ lid Lid }

func (f fixedLid) Watch(ctx context.Context, heard func(Lid)) error {
	heard(f.lid)
	<-ctx.Done()
	return nil
}

// parseLidLine reads one lid state's name.
func parseLidLine(line string) (Lid, error) {
	for _, l := range []Lid{LidOpen, LidClosed, LidNone} {
		if strings.TrimSpace(line) == l.String() {
			return l, nil
		}
	}
	return 0, fmt.Errorf("%q names no lid state", line)
}
