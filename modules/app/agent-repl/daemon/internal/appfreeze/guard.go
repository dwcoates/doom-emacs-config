package appfreeze

import (
	"errors"
	"fmt"
	"io"
	"os"
	"time"

	"golang.org/x/sys/unix"

	"agent-repld/internal/dlog"
)

// THE THAW GUARD: NO FROZEN PROCESS OUTLIVES EVERY DAEMON.
//
// The kept record makes a frozen process safe across any exit that has a
// successor (a restart, a handover): the next daemon adopts or thaws it. A
// daemon that exits with NO successor (the editor quit, a crash nobody
// restarts) would leave its frozen applications stopped until the next
// daemon happened to boot. The guard closes that, structurally:
//
//   - EVERY DAEMON HOLDS THE OWNERS FILE SHARED (flock) for its whole life,
//     taken at boot before it reads the record. The kernel releases it when
//     the daemon dies, however it dies.
//   - EVERY DAEMON STARTS ONE GUARD, in a session of its own so it outlives
//     the daemon, handed the read end of a pipe whose write end only the
//     daemon holds. The pipe's end of file is the daemon's death, however it
//     dies.
//   - At its daemon's death the guard asks for the owners file EXCLUSIVE,
//     without waiting. Granted, no daemon is alive to own the record, so the
//     guard thaws every process it names and clears it; refused, a living
//     daemon (a handover's successor) owns it, and the guard leaves it be.
//
// A daemon booting while a guard thaws waits for the shared hold
// (HoldOwnership), so it reads the record only once the guard has cleared it.

// HoldOwnership takes the owners file at path SHARED for the life of the
// process, and answers the open file, which the caller keeps open (closing it
// releases the hold). A guard mid-thaw holds it exclusive for as long as one
// thaw takes (guardThawBound), so the wait is bounded by that; a wait past
// the bound is a guard that is stuck, refused loudly.
func HoldOwnership(path string, bound time.Duration) (*os.File, error) {
	f, err := os.OpenFile(path, os.O_RDWR|os.O_CREATE, 0o600)
	if err != nil {
		return nil, fmt.Errorf("open the frozen set's owners file: %w", err)
	}
	done := make(chan error, 1)
	// inflight: bounded below: the flock call has no timeout of its own, so
	// the wait is bounded from outside.
	go func() { done <- unix.Flock(int(f.Fd()), unix.LOCK_SH) }()
	timer := time.NewTimer(bound)
	defer timer.Stop()
	select {
	case err := <-done:
		if err != nil {
			_ = f.Close()
			return nil, fmt.Errorf("hold the frozen set's owners file shared: %w", err)
		}
		return f, nil
	case <-timer.C:
		// A descriptor blocked in flock cannot be closed under it (the close
		// waits too), so an abandoned wait closes its file once flock
		// answers.
		// inflight: unjoined: it ends when the stuck flock answers.
		go func() {
			<-done
			_ = f.Close()
		}()
		return nil, fmt.Errorf("the frozen set's owners file was held exclusive past %s (a thaw takes about %s): a thaw guard is stuck", bound, guardThawExpected)
	}
}

// Expected time and bound of a booting daemon's wait for the shared hold
// behind a guard thawing under the exclusive one (a record read, one SIGCONT
// per frozen process, the record's removal). Measured 2026-11-11 on the
// owner's laptop on battery at load average 6 to 8, across processes: 100
// waits behind a 40-process thaw, median 0.2ms, p95 0.43ms, worst 25ms.
const (
	guardThawExpected = 25 * time.Millisecond
	GuardThawBound    = 60 * time.Millisecond
)

// GuardDeps are the guard's collaborators.
type GuardDeps struct {
	// DaemonGone blocks until the guarded daemon is gone (its pipe's end of
	// file). It is the guard's lifetime, not an operation with an expected
	// time, so it has no bound.
	DaemonGone func() error
	// TryOwn asks for the owners file exclusive without waiting, answering
	// whether it was granted and, granted, the release of the hold.
	TryOwn func() (release func(), owned bool, err error)
	Store  Store
	Signal Signaler
	Log    dlog.Logger
}

// RunGuard is the guard's whole life: wait for the daemon to be gone, then
// thaw the record if no daemon owns it.
func RunGuard(d GuardDeps) error {
	const op = "daemon.appfreeze.guard"
	if err := d.DaemonGone(); err != nil {
		d.Log.Error(op, "the thaw guard could not wait for its daemon; it thaws nothing", dlog.Context{"cause": err.Error()})
		return err
	}
	release, owned, err := d.TryOwn()
	if err != nil {
		d.Log.Error(op, "the thaw guard could not ask for the frozen set; it thaws nothing", dlog.Context{"cause": err.Error()})
		return err
	}
	if !owned {
		d.Log.Info(op, "the thaw guard's daemon is gone and another daemon owns the frozen set; nothing was thawed", nil)
		return nil
	}
	// THE EXCLUSIVE HOLD SPANS THE THAW ALONE, released before any record is
	// written, so a booting daemon waits for the thaw and nothing else.
	r, err := thawOwned(d)
	release()
	if err != nil {
		return err
	}
	if r == nil {
		d.Log.Info(op, "the thaw guard's daemon is gone and nothing was frozen", nil)
		return nil
	}
	d.Log.Info(op, "the last daemon is gone; the thaw guard thawed the frozen applications", dlog.Context{
		"thawed": describe(r.record), "signalled": r.signalled, "gone": r.gone, "failed": r.failed})
	return nil
}

// guardThaw is what one guard thaw did.
type guardThaw struct {
	record                  Record
	signalled, gone, failed int
}

// thawOwned thaws and clears the record under the exclusive hold, answering
// nil when nothing was frozen.
func thawOwned(d GuardDeps) (*guardThaw, error) {
	const op = "daemon.appfreeze.guard"
	r, ok, err := d.Store.Load()
	if err != nil {
		d.Log.Error(op, "the thaw guard could not read the frozen set; nothing was thawed", dlog.Context{"cause": err.Error()})
		return nil, err
	}
	if !ok || r.empty() {
		return nil, nil
	}
	signalled, goneCount, failed := thaw(r, d.Signal, d.Log, "thaw_guard")
	if err := d.Store.Clear(); err != nil {
		d.Log.Error(op, "the thaw guard thawed the frozen set but could not clear it", dlog.Context{"thawed": describe(r), "cause": err.Error()})
		return nil, err
	}
	return &guardThaw{record: r, signalled: signalled, gone: goneCount, failed: failed}, nil
}

// PipeGone is the production DaemonGone: read the pipe until end of file.
// The daemon never writes to it, so any byte read is a protocol breach.
func PipeGone(pipe io.Reader) func() error {
	return func() error {
		buf := make([]byte, 1)
		n, err := pipe.Read(buf)
		switch {
		case n > 0:
			return fmt.Errorf("the guard's pipe carried a byte; the daemon never writes to it")
		case errors.Is(err, io.EOF):
			return nil
		case err != nil:
			return fmt.Errorf("read the guard's pipe: %w", err)
		}
		return fmt.Errorf("the guard's pipe answered neither a byte nor an end")
	}
}

// FlockTryOwn is the production TryOwn: LOCK_EX|LOCK_NB on the owners file.
func FlockTryOwn(path string) func() (func(), bool, error) {
	return func() (func(), bool, error) {
		f, err := os.OpenFile(path, os.O_RDWR|os.O_CREATE, 0o600)
		if err != nil {
			return nil, false, fmt.Errorf("open the frozen set's owners file: %w", err)
		}
		err = unix.Flock(int(f.Fd()), unix.LOCK_EX|unix.LOCK_NB)
		if errors.Is(err, unix.EWOULDBLOCK) {
			_ = f.Close()
			return nil, false, nil
		}
		if err != nil {
			_ = f.Close()
			return nil, false, fmt.Errorf("ask for the frozen set's owners file exclusive: %w", err)
		}
		return func() { _ = f.Close() }, true, nil
	}
}
