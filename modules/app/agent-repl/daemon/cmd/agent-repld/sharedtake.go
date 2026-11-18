package main

import (
	"context"
	"fmt"
	"net"
	"sync"
	"time"

	"agentrepl/logging/sockethold"

	"agent-repld/internal/daemonaddr"
	"agent-repld/internal/dlog"
	"agent-repld/internal/server"
)

// holderDaemonSocket is the name the daemon's holder holds the shared
// listener under (requirements.json, socket-holder-daemon).
const holderDaemonSocket = "daemon"

// holderTakeBound bounds taking the shared listener from the holder: the
// holder's start, when launchd is still spawning it (measured 10ms at worst
// over five starts, the timer's resolution, at load average 3.9-4.4 on
// 2026-11-18 11:25), and the exchange itself (under 1ms). Each take's time is
// recorded at INFO, so the bound is re-sized from real takes.
const holderTakeBound = 30 * time.Millisecond

// sharedStopper is the slice of server.SharedAccept beginShutdown needs.
type sharedStopper interface{ StopShared() error }

// sharedTake takes the daemon's shared listener from its holder once this
// daemon holds the boot claim, adds it to the accept loop, and names its
// address in the claim's advertisement. With no holder it does nothing, and
// the daemon advertises its own listener.
type sharedTake struct {
	holder string
	accept *server.SharedAccept
	claim  interface{ Advertise(string) }
	log    dlog.Logger
	once   sync.Once
	err    error
	// takeFn is sockethold.Take; a seam for the tests.
	takeFn func(ctx context.Context, control, name string) (net.Listener, error)
}

// take runs once, at the first publication of this daemon's ownership.
func (s *sharedTake) take() error {
	if s == nil || s.holder == "" {
		return nil
	}
	s.once.Do(func() {
		takeFn := s.takeFn
		if takeFn == nil {
			takeFn = sockethold.Take
		}
		ctx, cancel := context.WithTimeout(context.Background(), holderTakeBound)
		defer cancel()
		began := time.Now()
		ln, err := takeFn(ctx, s.holder, holderDaemonSocket)
		took := time.Since(began)
		if err != nil {
			s.log.Error("daemon.cmd.holder", "the shared listener could not be taken from the daemon's socket holder", dlog.Context{
				"holder": s.holder, "bound_ms": holderTakeBound.Milliseconds(), "measured_ms": 10, "took_ms": took.Milliseconds(), "error": err.Error()})
			s.err = fmt.Errorf("take the shared listener from %s: %w", s.holder, err)
			return
		}
		s.accept.AddShared(ln)
		s.claim.Advertise(ln.Addr().String())
		s.log.Info("daemon.cmd.holder", "took the shared listener from the daemon's socket holder", dlog.Context{
			"holder": s.holder, "address": ln.Addr().String(), "took_ms": took.Milliseconds()})
	})
	return s.err
}

var _ interface{ Advertise(string) } = daemonaddr.Claim(nil)
