package main

import (
	"context"
	"fmt"
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
	err    error
	// taken is the shared address once the take succeeded.
	taken atomic.Value
	// takeFn is sockethold.Take; a seam for the tests.
	takeFn sockethold.TakeFunc
	// ensure proves a holder serves the control socket, bringing a declared
	// one up when none does (holderensure.Ensurer.Ensure), before the take.
	// REQUIRED.
	ensure func(ctx context.Context, control string) error
}

// take runs once, at the first publication of this daemon's ownership.
func (s *sharedTake) take() error {
	if s == nil {
		return nil
	}
	s.once.Do(func() {
		takeFn := s.takeFn
		if takeFn == nil {
			takeFn = sockethold.Take
		}
		// THE PROBE IS THE WHOLE COST on a machine that has its holder; only a
		// machine that predates it pays the install, once.
		if err := s.ensure(context.Background(), s.holder); err != nil {
			s.err = fmt.Errorf("ensure the daemon's socket holder at %s: %w", s.holder, err)
			return
		}
		ln, took, err := sockethold.TakeTimed(takeFn, s.holder, holderDaemonSocket)
		if err != nil {
			s.log.Error("daemon.cmd.holder", "the shared listener could not be taken from the daemon's socket holder", dlog.Context{
				"holder": s.holder, "bound_ms": sockethold.TakeBound.Milliseconds(), "measured_ms": sockethold.TakeMeasured.Milliseconds(), "took_ms": took.Milliseconds(), "error": err.Error()})
			s.err = fmt.Errorf("take the shared listener from %s: %w", s.holder, err)
			return
		}
		s.accept.AddShared(ln)
		s.claim.Advertise(ln.Addr().String())
		s.taken.Store(ln.Addr().String())
		s.log.Info("daemon.cmd.holder", "took the shared listener from the daemon's socket holder", dlog.Context{
			"holder": s.holder, "address": ln.Addr().String(), "took_ms": took.Milliseconds()})
	})
	return s.err
}

var _ interface{ Advertise(string) } = daemonaddr.Claim(nil)

// address answers the shared address this daemon advertises, "" before its
// take or with no holder: what rollout tells clients (Deps.SharedAddress).
// A successor that has not taken over yet answers "" and announces nothing.
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
	alive, err := daemonaddr.Alive(addrPath, string(*owner))
	if err != nil {
		return "", false, fmt.Errorf("probe %q's serving daemon %s: %w", ws, *owner, err)
	}
	if !alive {
		return "", false, fmt.Errorf("%q's serving row names %s, which is not running", ws, *owner)
	}
	address, err := daemonaddr.InstanceAddress(addrPath, string(*owner))
	if err != nil {
		return "", false, fmt.Errorf("%q's serving daemon %s: %w", ws, *owner, err)
	}
	return address, true, nil
}
