package main

import (
	"context"
	"errors"
	"fmt"
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
// watch follows the root, its parent (the root renamed or removed) and the
// address file, and every change it reports is verified at once. So a loss
// is seen the instant it happens, and a root nobody touches costs no check
// at all.
//
// THE WATCH NEVER OPENS A LOCK FILE, AND REFUSES TO. Closing ANY descriptor
// of a file drops every record lock this process holds on it (recordlock's
// package comment), and the watch closes and reopens what it follows at every
// change and at its stop. Following daemon.lock dropped the boot claim while
// the daemon still ran: at a handover the successor took the claim and
// published before the incumbent exited, and the incumbent's exit then
// withdrew the successor's advertisement (2026-11-22). The root's own watch
// reports daemon.lock removed or replaced, a change of its entries, and
// Verify checks the lock's inode at that change.
//
// The refusal is the watch's own, not only its path list's: the root watch's
// change source is built by followRootPaths alone, which PANICS on any path
// that is one of this daemon's lock files (daemonaddr.LockFiles: the boot
// claim, the serving lock, its life lock and that lock's pending name, by
// cleaned path) or has a lock file's name (daemonaddr.LockKindOf), and
// TestOnlyFollowRootPathsBuildsTheRootWatchsSource keeps every other follow
// out of this package.

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

// rootWatchPaths are the root, its parent, whose change is the root renamed
// or removed, and the address file. No lock file is among them: its watch's
// descriptor closing would drop this daemon's record lock on it; the root's
// watch reports one removed or replaced.
func rootWatchPaths(root, addrPath string) []string {
	return []string{filepath.Dir(root), root, addrPath}
}

// followRootPaths is THE ONE constructor of the root watch's change source
// (kernelevent.FollowPaths over PATHS). It PANICS, an invariant violation,
// when any of PATHS is a lock file of daemon INSTANCE on the state root of
// ADDRPATH, or has a lock file's name: following one would drop this
// daemon's record lock on it the first time the watch's descriptor closed.
func followRootPaths(addrPath, instance string, paths ...string) kernelevent.Source {
	locks := daemonaddr.LockFiles(addrPath, instance)
	for _, path := range paths {
		clean := filepath.Clean(path)
		for _, lock := range locks {
			if clean == filepath.Clean(lock.Path) {
				panic(fmt.Sprintf("agent-repld: invariant violated: the state-root watch was asked to follow %q, this daemon's %s lock; closing the watch's descriptor would drop its record lock", path, lock.Kind))
			}
		}
		if kind, ok := daemonaddr.LockKindOf(clean); ok {
			panic(fmt.Sprintf("agent-repld: invariant violated: the state-root watch was asked to follow %q, named as a %s lock; closing the watch's descriptor would drop a record lock on it", path, kind))
		}
	}
	return kernelevent.FollowPaths(paths...)
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
