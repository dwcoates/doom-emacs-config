package main

import (
	"context"
	"fmt"
	"net"
	"os"
	"sync"
	"sync/atomic"

	"agentrepl/logging/sockethold"

	"agent-repld/internal/daemonaddr"
	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/server"
)

// holderDaemonSocket is the name the daemon's holder holds the shared
// listener under (requirements.json, socket-holder-daemon).
const holderDaemonSocket = "daemon"

// sharedStopper is the slice of server.SharedAccept beginShutdown needs.
type sharedStopper interface{ StopShared() error }

// sharedTake takes the daemon's shared listener from its holder once this
// daemon holds the boot claim, adds it to the accept loop, and names its
// address in the claim's advertisement. Every daemon has a holder (run
// refuses to start without one).
type sharedTake struct {
	holder string
	accept *server.SharedAccept
	claim  interface{ Advertise(string) }
	log    dlog.Logger
	once   sync.Once
	// acquired guards acquire; acquireErr is its answer, ln its listener.
	acquired   sync.Once
	acquireErr error
	ln         net.Listener
	// taken is the shared address once the take succeeded.
	taken atomic.Value
	// takeFn is sockethold.Take; a seam for the tests.
	takeFn sockethold.TakeFunc
	// subscribeFn opens the holder's stream of handed-back connections
	// (sockethold.Subscribe); a seam for the tests.
	subscribeFn func(ctx context.Context, control, name string) (server.Returned, error)
	// returnFn hands one unread connection back (sockethold.Return); a seam
	// for the tests.
	returnFn func(ctx context.Context, control, name string, file *os.File) error
	// takeErr is take's answer once it ran.
	takeErr error
	// ensure proves a holder serves the control socket, bringing a declared
	// one up when none does (holderensure.Ensurer.Ensure), before the take.
	// REQUIRED.
	ensure func(ctx context.Context, control string) error
}

// acquire runs once, at boot: it ensures the holder and takes the shared
// listener from it, so this daemon knows the shared address from the start
// (what it tells clients, even while a successor that does not yet accept on
// it), and accepts nothing on it yet. THE ACCEPT RIGHT IS THE BOOT CLAIM:
// take adds the listener to the accept loop only under it.
func (s *sharedTake) acquire() error {
	s.acquired.Do(func() {
		takeFn := s.takeFn
		if takeFn == nil {
			takeFn = sockethold.Take
		}
		// THE PROBE IS THE WHOLE COST on a machine that has its holder; only a
		// machine that predates it pays the install, once.
		if err := s.ensure(context.Background(), s.holder); err != nil {
			s.acquireErr = fmt.Errorf("ensure the daemon's socket holder at %s: %w", s.holder, err)
			return
		}
		ln, took, err := sockethold.TakeTimed(takeFn, s.holder, holderDaemonSocket)
		if err != nil {
			s.log.Error("daemon.cmd.holder", "the shared listener could not be taken from the daemon's socket holder", dlog.Context{
				"holder": s.holder, "bound_ms": sockethold.TakeBound.Milliseconds(), "measured_ms": sockethold.TakeMeasured.Milliseconds(), "took_ms": took.Milliseconds(), "error": err.Error()})
			s.acquireErr = fmt.Errorf("take the shared listener from %s: %w", s.holder, err)
			return
		}
		s.ln = ln
		s.taken.Store(ln.Addr().String())
		s.log.Info("daemon.cmd.holder", "took the shared listener from the daemon's socket holder", dlog.Context{
			"holder": s.holder, "address": ln.Addr().String(), "took_ms": took.Milliseconds()})
	})
	return s.acquireErr
}

// take runs once, at the first publication of this daemon's ownership, under
// the boot claim: it acquires the listener if the boot has not, then accepts
// on it and advertises its address.
func (s *sharedTake) take() error {
	if s == nil {
		return nil
	}
	if err := s.acquire(); err != nil {
		return err
	}
	s.once.Do(func() {
		subscribe := s.subscribeFn
		if subscribe == nil {
			subscribe = func(ctx context.Context, control, name string) (server.Returned, error) {
				return sockethold.Subscribe(ctx, control, name)
			}
		}
		ctx, cancel := context.WithTimeout(context.Background(), sockethold.TakeBound)
		returned, err := subscribe(ctx, s.holder, holderDaemonSocket)
		cancel()
		if err != nil {
			s.log.Error("daemon.cmd.holder", "the holder's stream of handed-back connections could not be opened", dlog.Context{
				"holder": s.holder, "bound_ms": sockethold.TakeBound.Milliseconds(), "error": err.Error()})
			s.takeErr = fmt.Errorf("open the holder's stream of handed-back connections at %s: %w", s.holder, err)
			return
		}
		s.accept.AddShared(s.ln, returned, s.log)
		s.claim.Advertise(s.ln.Addr().String())
	})
	return s.takeErr
}

// handBack gives the holder FILE, a connection this daemon accepted on the
// shared address and never read, for the daemon accepting next.
func (s *sharedTake) handBack(file *os.File) error {
	give := s.returnFn
	if give == nil {
		give = sockethold.Return
	}
	ctx, cancel := context.WithTimeout(context.Background(), sockethold.TakeBound)
	defer cancel()
	if err := give(ctx, s.holder, holderDaemonSocket, file); err != nil {
		return fmt.Errorf("hand an unread connection back to the holder at %s: %w", s.holder, err)
	}
	return nil
}

var _ interface{ Advertise(string) } = daemonaddr.Claim(nil)

// address answers the shared address, "" before the boot acquired it: what
// rollout and the server tell clients (Deps.SharedAddress), a successor that
// does not yet accept on it included.
func (s *sharedTake) address() string {
	if s == nil {
		return ""
	}
	if v, ok := s.taken.Load().(string); ok {
		return v
	}
	return ""
}

// yieldBootClaim stops this daemon's accept on the shared listener, THEN
// yields the boot claim to its successor: the accept right is the claim, so
// the daemon that takes the claim next is the only one accepting from the
// moment it does (docs/investigations/2026-11-18-stable-sockets-design.md).
func yieldBootClaim(claim interface{ Yield() error }, shared *sharedTake) error {
	if err := shared.accept.StopShared(); err != nil {
		shared.log.Error("daemon.cmd.holder", "the shared listener could not be stopped before the boot claim was yielded", dlog.Context{
			"error": err.Error()})
		return fmt.Errorf("stop the shared listener before yielding: %w", err)
	}
	return claim.Yield()
}

// instanceAddress answers the private address daemon INSTANCE recorded and
// whether that daemon runs, for a call that names a daemon (a page call's
// attachment, a feed token's minter). An instance that no longer runs
// answers alive false. One that runs and records no address is an invariant
// violation: every live daemon records its address at boot.
func instanceAddress(addrPath string, instance ids.InstanceID) (string, bool, error) {
	alive, err := daemonaddr.Alive(addrPath, string(instance))
	if err != nil {
		return "", false, fmt.Errorf("probe daemon %s: %w", instance, err)
	}
	if !alive {
		return "", false, nil
	}
	address, err := daemonaddr.InstanceAddress(addrPath, string(instance))
	if err != nil {
		return "", false, fmt.Errorf("daemon %s runs and records no address: %w", instance, err)
	}
	return address, true, nil
}

// servingRows is the slice of the state client servingElsewhere reads.
type servingRows interface {
	Serving(ctx context.Context, id ids.WorkspaceID) (*ids.InstanceID, error)
}

// servingElsewhere answers the private address of the other live daemon
// whose serving row holds WS, with ok true; ok false when the row names this
// daemon or stands released (docs/investigations/
// 2026-11-18-stable-sockets-design.md, D3). A row naming an instance that is
// not alive, or one alive with no recorded address, is an invariant
// violation: a serving row names a live daemon, and every live daemon
// records its address at boot.
func servingElsewhere(ctx context.Context, db servingRows, addrPath string, self ids.InstanceID, ws ids.WorkspaceID) (string, bool, error) {
	owner, err := db.Serving(ctx, ws)
	if err != nil {
		return "", false, fmt.Errorf("read %q's serving row: %w", ws, err)
	}
	if owner == nil || *owner == self {
		return "", false, nil
	}
	address, alive, err := instanceAddress(addrPath, *owner)
	if err != nil {
		return "", false, fmt.Errorf("%q's serving daemon %s: %w", ws, *owner, err)
	}
	if !alive {
		return "", false, fmt.Errorf("%q's serving row names %s, which is not running", ws, *owner)
	}
	return address, true, nil
}
