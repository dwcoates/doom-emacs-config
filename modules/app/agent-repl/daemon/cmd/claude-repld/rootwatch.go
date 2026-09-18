package main

import (
	"context"
	"errors"
	"sync"
	"time"

	"claude-repld/internal/daemonaddr"
	"claude-repld/internal/dlog"
)

// stateRootCheckEvery is how often a serving daemon verifies that what it owns
// on disk -- the state root, daemon.lock, daemon.addr -- is still there.
//
// A DAEMON WHOSE STATE ROOT IS DELETED MUST NOTICE. Nothing a booted daemon
// runs looks at its root again, so one whose root was removed out from under
// it (the integration harness's worlds, when a run died and its daemons were
// orphaned) served, swept and retried against it until someone killed it --
// and, worse, a second daemon booting on a recreated root takes a daemon.lock
// inode this one's flock does not conflict with. A check is three stat(2)s
// and one small read, so once a second costs nothing measurable, and it bounds
// how long two daemons can believe they own one root.
const stateRootCheckEvery = time.Second

// stateRootCheckCeiling caps the backoff between checks that could not TELL.
// An undecided check is reported at ERROR and retried, doubling from
// stateRootCheckEvery, so a stat that keeps failing for a reason that is not
// a loss does not write an ERROR a second forever.
const stateRootCheckCeiling = time.Minute

// rootWatch stands the daemon down when what it owns on disk vanishes.
type rootWatch struct {
	// verify is daemonaddr.Claim.Verify.
	verify func() error
	// every is the cadence of a healthy watch; zero means stateRootCheckEvery.
	every time.Duration
	// after is the watch's clock; nil means time.After.
	after func(time.Duration) <-chan time.Time
	log   dlog.Logger
	// standDown ends the serving lifetime with the loss as its cause.
	standDown func(cause error)
}

// run checks on its cadence until ctx ends or a loss is found. A loss is
// recorded at ERROR naming what vanished, handed to standDown, and returned.
func (w rootWatch) run(ctx context.Context) error {
	every := w.every
	if every <= 0 {
		every = stateRootCheckEvery
	}
	after := w.after
	if after == nil {
		after = time.After
	}
	wait := every
	for {
		select {
		case <-ctx.Done():
			return nil
		case <-after(wait):
		}
		err := w.verify()
		switch {
		case err == nil:
			wait = every
		case errors.Is(err, daemonaddr.ErrVanished):
			w.log.Error("daemon.cmd.state_root", "what this daemon owns on disk vanished; standing down", dlog.Context{
				"cause": err.Error(),
			})
			w.standDown(err)
			return err
		default:
			w.log.Error("daemon.cmd.state_root", "could not tell whether the state root is intact; checking again", dlog.Context{
				"cause":   err.Error(),
				"next_in": min(2*wait, stateRootCheckCeiling).String(),
			})
			wait = min(2*wait, stateRootCheckCeiling)
		}
	}
}

// standDownCause records why the daemon stood down on its own, so the
// process's exit reports it rather than reading as an orderly one.
type standDownCause struct {
	mu    sync.Mutex
	cause error
}

func (s *standDownCause) set(err error) {
	s.mu.Lock()
	defer s.mu.Unlock()
	if s.cause == nil {
		s.cause = err
	}
}

func (s *standDownCause) get() error {
	s.mu.Lock()
	defer s.mu.Unlock()
	return s.cause
}
