package persistentwifi

import (
	"context"
	"fmt"

	"github.com/godbus/dbus/v5"

	"agent-repld/internal/inflight"
	"agent-repld/internal/kernelevent"
)

// ON LINUX THE MODE IS HEARD FROM logind (linuxHost): every inhibitor taken
// or released changes the manager's BlockInhibited property, and logind says
// so with PropertiesChanged on the system bus. A mode turned off outside
// agent-repl (its inhibitor killed) is heard as it happens, as macOS's power
// preference files are.

// logind's manager on the system bus.
const (
	logindPath      = dbus.ObjectPath("/org/freedesktop/login1")
	logindInterface = "org.freedesktop.login1.Manager"
	propsInterface  = "org.freedesktop.DBus.Properties"
	propsChanged    = "PropertiesChanged"
)

// inhibitorProperties are the manager's properties an inhibitor's change
// moves.
var inhibitorProperties = map[string]bool{"BlockInhibited": true, "DelayInhibited": true}

// logindPowerChanges is the Source of logind's inhibitors changing: the
// system bus's PropertiesChanged of logind's manager, filtered to the
// inhibitor properties (inhibitorChange).
func logindPowerChanges(ctx context.Context) (<-chan error, func()) {
	out := make(chan error, 1)
	conn, err := dbus.ConnectSystemBus()
	if err != nil {
		out <- fmt.Errorf("the system bus could not be reached to hear logind's inhibitors: %w", err)
		return out, func() {}
	}
	match := []dbus.MatchOption{
		dbus.WithMatchObjectPath(logindPath),
		dbus.WithMatchInterface(propsInterface),
		dbus.WithMatchMember(propsChanged),
		dbus.WithMatchArg(0, logindInterface),
	}
	if err := conn.AddMatchSignal(match...); err != nil {
		_ = conn.Close()
		out <- fmt.Errorf("logind's PropertiesChanged could not be subscribed to: %w", err)
		return out, func() {}
	}
	signals := make(chan *dbus.Signal, 16)
	conn.Signal(signals)
	ctx, cancel := context.WithCancel(ctx)
	done := make(chan struct{})
	inflight.Standing(func() {
		defer close(done)
		followInhibitorChanges(ctx, signals, out)
	})
	return out, func() {
		cancel()
		_ = conn.Close()
		<-done
	}
}

// followInhibitorChanges reports nil on out for each signal that is an
// inhibitor change, coalescing changes the reader has not yet taken, until ctx
// ends. A signals channel that closes (the bus connection lost) is reported
// once as an error, after which nothing more is sent.
func followInhibitorChanges(ctx context.Context, signals <-chan *dbus.Signal, out chan<- error) {
	for {
		select {
		case <-ctx.Done():
			return
		case sig, ok := <-signals:
			if !ok {
				select {
				case out <- fmt.Errorf("the system bus connection hearing logind's inhibitors closed"):
				case <-ctx.Done():
				}
				return
			}
			if inhibitorChange(sig) {
				notifyChange(out)
			}
		}
	}
}

// notifyChange reports one change on out without waiting: a change the reader
// has not taken yet already stands for this one.
func notifyChange(out chan<- error) {
	select {
	case out <- nil:
	default:
	}
}

// inhibitorChange reports whether sig is logind's manager saying an
// inhibitor property changed (PropertiesChanged's changed or invalidated
// names).
func inhibitorChange(sig *dbus.Signal) bool {
	if sig == nil || sig.Path != logindPath || sig.Name != propsInterface+"."+propsChanged || len(sig.Body) < 3 {
		return false
	}
	if iface, ok := sig.Body[0].(string); !ok || iface != logindInterface {
		return false
	}
	if changed, ok := sig.Body[1].(map[string]dbus.Variant); ok {
		for name := range changed {
			if inhibitorProperties[name] {
				return true
			}
		}
	}
	if invalidated, ok := sig.Body[2].([]string); ok {
		for _, name := range invalidated {
			if inhibitorProperties[name] {
				return true
			}
		}
	}
	return false
}

// a compile-time check that the Source has the shape the controller takes.
var _ kernelevent.Source = logindPowerChanges
