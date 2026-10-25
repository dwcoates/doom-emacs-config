// Package sweepguard is the exclusivity every background sweep of the daemon
// runs under: at most one sweep of a kind in this process at a time, and at
// most one across daemons (an incumbent and its handover successor), the
// latter arbitrated by a kernel record lock that dies with its holder.
package sweepguard

import (
	"errors"
	"fmt"
	"sync"

	"agent-repld/internal/dlog"
	"agent-repld/internal/recordlock"
)

// ErrRunning is Do's refusal while another sweep of the same guard is in
// flight in this process. A sweep never runs concurrently with itself.
var ErrRunning = errors.New("a sweep is already running")

// Guard serializes one kind of sweep.
type Guard struct {
	// running is held for one sweep's whole run; TryLock is the refusal.
	running sync.Mutex
	// pkg prefixes every error, naming the sweep's package.
	pkg string
	// lockPath is the kernel lock one sweep holds for its whole run.
	lockPath string
	// op is the operation every record of the guard carries.
	op string
}

// New builds the guard for one kind of sweep: pkg names the sweep's package
// in its errors, op is the operation its records carry.
func New(pkg, lockPath, op string) *Guard {
	return &Guard{pkg: pkg, lockPath: lockPath, op: op}
}

// Do runs fn while holding the guard, answering whether fn ran, and records
// each answer on log (the caller's, so a run's own fields ride along). It refuses
// with ErrRunning while this process is already sweeping, answers (false, nil)
// without running fn while another PROCESS holds the kernel lock, and answers
// the lock's error when the lock could not be told about, which is never read
// as free.
func (g *Guard) Do(log dlog.Logger, fn func()) (bool, error) {
	if !g.running.TryLock() {
		log.Info(g.op, "a sweep was asked for while one is running; it is not started", nil)
		return false, ErrRunning
	}
	defer g.running.Unlock()

	lock, held, err := acquire(g.pkg, g.lockPath)
	if err != nil {
		log.Error(g.op, "the sweep's kernel lock could not be told about; nothing is swept", dlog.Context{
			"lock": g.lockPath, "cause": err.Error(),
		})
		return false, err
	}
	if !held {
		log.Info(g.op, "another daemon is sweeping; this one does not", dlog.Context{"lock": g.lockPath})
		return false, nil
	}
	defer func() {
		if err := lock.Release(); err != nil {
			log.Error(g.op, "the sweep's kernel lock could not be released", dlog.Context{
				"lock": g.lockPath, "cause": err.Error(),
			})
		}
	}()
	fn()
	return true, nil
}

// acquire takes the cross-process sweep lock without blocking. The bool is
// false when another daemon holds it.
func acquire(pkg, path string) (*recordlock.Record, bool, error) {
	lock, ok, err := recordlock.Hold(path)
	if err != nil {
		return nil, false, fmt.Errorf("%s: %w", pkg, err)
	}
	return lock, ok, nil
}
