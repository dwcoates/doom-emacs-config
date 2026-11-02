package persistentwifi

import (
	"bufio"
	"context"
	"errors"
	"fmt"
	"os"
	"strings"
	"sync"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
)

// EnvLidPipe replaces the operating system's lid with a named pipe: each line
// written to it ("open", "closed" or "none") is one lid state, the first being
// the state read at subscription. It is the test knob that pairs with
// EnvToolsDir, so a daemon running against fake host tools never hears the
// real machine's lid (ConfigFromEnv refuses the one without the other).
const EnvLidPipe = "AGENT_REPL_PERSISTENT_WIFI_LID_PIPE"

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

// String names the lid for the record and for EnvLidPipe's lines.
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

// PipeLid is the LidWatcher over EnvLidPipe's named pipe.
func PipeLid(path string) LidWatcher { return pipeLid{path: path} }

type pipeLid struct{ path string }

// Watch opens the pipe read-write, so it never sees an end of file while it
// holds it, and reads one lid per line. THE END OF CTX IS WRITTEN INTO THE
// PIPE: the read blocks in the kernel (the runtime does not poll a pipe on
// macOS, so closing it would not wake the read), and the watcher's own
// descriptor writes the line that wakes it.
func (p pipeLid) Watch(ctx context.Context, heard func(Lid)) error {
	f, err := os.OpenFile(p.path, os.O_RDWR, 0)
	if err != nil {
		return fmt.Errorf("open the lid pipe: %w", err)
	}
	defer f.Close()
	stopped := make(chan struct{})
	var waker sync.WaitGroup
	waker.Add(1)
	go func() {
		defer waker.Done()
		select {
		case <-ctx.Done():
			_, _ = f.WriteString("\n")
		case <-stopped:
		}
	}()
	defer waker.Wait()
	defer close(stopped)
	sc := bufio.NewScanner(f)
	for sc.Scan() {
		if ctx.Err() != nil {
			return nil
		}
		l, err := parseLidLine(sc.Text())
		if err != nil {
			return err
		}
		heard(l)
	}
	if err := sc.Err(); err != nil {
		return fmt.Errorf("read the lid pipe: %w", err)
	}
	return errors.New("the lid pipe ended")
}

// parseLidLine reads one EnvLidPipe line.
func parseLidLine(line string) (Lid, error) {
	for _, l := range []Lid{LidOpen, LidClosed, LidNone} {
		if strings.TrimSpace(line) == l.String() {
			return l, nil
		}
	}
	return 0, fmt.Errorf("the lid pipe wrote %q, which names no lid state", line)
}
