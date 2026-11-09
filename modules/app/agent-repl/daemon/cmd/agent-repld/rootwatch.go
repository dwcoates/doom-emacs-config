package main

import (
	"context"
	"errors"
	"path/filepath"
	"sync"

	"agent-repld/internal/daemonaddr"
	"agent-repld/internal/dlog"
	"agent-repld/internal/kernelevent"
)

// A DAEMON WHOSE STATE ROOT IS DELETED MUST NOTICE. Nothing a booted daemon
// runs looks at its root again, so one whose root was removed out from under
// it (the integration harness's worlds, when a run died and its daemons were
// orphaned) served, swept and retried against it until someone killed it --
// and, worse, a second daemon booting on a recreated root takes a daemon.lock
// inode this one's flock does not conflict with.
//
// THE CHECK RUNS ON THE KERNEL'S REPORT OF A CHANGE, never on a cadence: the
// watch follows the root, its parent (the root renamed or removed), the lock
// and the address file, and every change it reports is verified at once. So
// a loss is seen the instant it happens, and a root nobody touches costs no
// check at all.

// rootWatch stands the daemon down when what it owns on disk vanishes.
type rootWatch struct {
	// verify is daemonaddr.Claim.Verify.
	verify func() error
	// changes follows what verify reads (rootWatchPaths).
	changes kernelevent.Source
	log     dlog.Logger
	// standDown ends the serving lifetime with the loss as its cause.
	standDown func(cause error)
}

// rootWatchPaths are what a serving daemon owns on disk and its root's
// parent, whose change is the root renamed or removed.
func rootWatchPaths(root, addrPath string) []string {
	return []string{filepath.Dir(root), root, daemonaddr.LockPath(addrPath), addrPath}
}

// run verifies once the watch is taken, then at every change it reports,
// until ctx ends or a loss is found. A loss is recorded at ERROR naming what
// vanished, handed to standDown, and returned. A check that cannot tell is
// an ERROR, and the next change checks again. A watch that fails is an ERROR
// once the root still verifies, and the root goes unwatched from then on.
func (w rootWatch) run(ctx context.Context) error {
	changes, stop := w.changes(ctx)
	defer stop()
	for {
		if lost := w.check(); lost != nil {
			return lost
		}
		select {
		case <-ctx.Done():
			return nil
		case err := <-changes:
			if err == nil {
				continue
			}
			if lost := w.check(); lost != nil {
				return lost
			}
			w.log.Error("daemon.cmd.state_root", "could not watch the state root; a loss is no longer noticed until the daemon restarts", dlog.Context{
				"cause": err.Error(),
			})
			<-ctx.Done()
			return nil
		}
	}
}

// check verifies once, answering the loss it stood down on.
func (w rootWatch) check() error {
	err := w.verify()
	switch {
	case err == nil:
		return nil
	case errors.Is(err, daemonaddr.ErrVanished):
		w.log.Error("daemon.cmd.state_root", "what this daemon owns on disk vanished; standing down", dlog.Context{
			"cause": err.Error(),
		})
		w.standDown(err)
		return err
	default:
		w.log.Error("daemon.cmd.state_root", "could not tell whether the state root is intact; checking again at its next change", dlog.Context{
			"cause": err.Error(),
		})
		return nil
	}
}

// standDownRecord records why the serving lifetime ended, so the process's
// exit reports the cause and runs only the teardown that reason owes. The
// FIRST record wins: a loss found while a handover is already leaving must
// not rewrite why the daemon left.
type standDownRecord struct {
	mu     sync.Mutex
	set    bool
	reason standDownReason
	cause  error
}

func (s *standDownRecord) record(reason standDownReason, cause error) {
	s.mu.Lock()
	defer s.mu.Unlock()
	if s.set {
		return
	}
	s.set, s.reason, s.cause = true, reason, cause
}

// get answers the recorded reason and cause; an unrecorded end -- the
// process's signal context -- is an orderly one with no cause.
func (s *standDownRecord) get() (standDownReason, error) {
	s.mu.Lock()
	defer s.mu.Unlock()
	return s.reason, s.cause
}
