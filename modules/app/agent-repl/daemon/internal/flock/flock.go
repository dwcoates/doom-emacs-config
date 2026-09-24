// Package flock is the daemon's one NON-BLOCKING EXCLUSIVE kernel lock that is
// HELD for the length of some work: the merge queue's per-repository lock and
// the landed-worktree reaper's sweep lock.
//
// The kernel arbitrates, so the guarantee survives a holder that died without
// cleaning up: an flock is released when the process holding it goes away,
// which no check-then-act on a state file can promise.
//
// It is not the shim-held lock PROBE (sessionlock: take and release at once,
// never hold) nor the boot claim (daemonaddr: a refusal is ErrClaimed, and a
// successor may block for it); those contracts differ from this one.
package flock

import (
	"fmt"
	"os"
	"path/filepath"
	"syscall"
)

// Lock is one held lock.
type Lock struct {
	path string
	file *os.File
}

// TryExclusive takes the lock at path without blocking, creating its directory
// and file as needed. The bool is false when another holder has it -- a legal
// answer the caller waits out or skips -- while an error means the lock could
// not be TOLD about, which is never read as free.
func TryExclusive(path string) (*Lock, bool, error) {
	if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
		return nil, false, fmt.Errorf("creating the lock directory %s: %w", filepath.Dir(path), err)
	}
	f, err := os.OpenFile(path, os.O_CREATE|os.O_RDWR, 0o644)
	if err != nil {
		return nil, false, fmt.Errorf("opening the lock %s: %w", path, err)
	}
	if err := syscall.Flock(int(f.Fd()), syscall.LOCK_EX|syscall.LOCK_NB); err != nil {
		f.Close()
		if err == syscall.EWOULDBLOCK {
			return nil, false, nil
		}
		return nil, false, fmt.Errorf("locking %s: %w", path, err)
	}
	return &Lock{path: path, file: f}, true, nil
}

// Path is the lock file, for the log record.
func (l *Lock) Path() string { return l.path }

// Release drops the lock. Releasing twice, or releasing a nil lock, is safe,
// because a teardown runs on both the ordinary and the failing path.
func (l *Lock) Release() error {
	if l == nil || l.file == nil {
		return nil
	}
	f := l.file
	l.file = nil
	if err := syscall.Flock(int(f.Fd()), syscall.LOCK_UN); err != nil {
		f.Close()
		return fmt.Errorf("unlocking %s: %w", l.path, err)
	}
	return f.Close()
}
