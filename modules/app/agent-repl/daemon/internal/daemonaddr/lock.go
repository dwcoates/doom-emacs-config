package daemonaddr

import (
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"syscall"
)

// LockName is the boot-exclusivity lock file beside daemon.addr.
const LockName = "daemon.lock"

// ErrClaimed reports that another daemon already holds the boot claim. An
// unflagged second daemon exits on this without touching the incumbent's
// listener or its daemon.addr; a successor is distinguishable because it was
// SPAWNED with the joining argument, not because it raced and lost.
var ErrClaimed = errors.New("another daemon holds the boot claim")

// LockPath is the boot lock's path for a given daemon.addr path. The lock
// lives beside the advertisement because they are the same claim: the file
// says who to talk to, the lock says who may say it.
func LockPath(addrPath string) string {
	return filepath.Join(filepath.Dir(addrPath), LockName)
}

// bootLock is an exclusive, non-blocking kernel lock on the boot lock file.
//
// The lock, not the bind, is the exclusivity: a port-0 bind hands every
// racing daemon a different free port and so arbitrates nothing. flock does
// arbitrate, and the kernel releases it on process death, so a daemon that
// was force-killed leaves the claim free rather than a stale file that has to
// be reasoned about.
type bootLock struct {
	f    *os.File
	path string
}

// acquireBootLock takes the claim, or returns ErrClaimed if a live daemon
// holds it. Any other error means the claim could not be decided and is never
// read as free.
func acquireBootLock(path string) (*bootLock, error) {
	f, err := os.OpenFile(path, os.O_CREATE|os.O_RDWR, 0o644)
	if err != nil {
		return nil, fmt.Errorf("open the boot lock %q: %w", path, err)
	}
	if err := syscall.Flock(int(f.Fd()), syscall.LOCK_EX|syscall.LOCK_NB); err != nil {
		f.Close()
		if errors.Is(err, syscall.EWOULDBLOCK) {
			return nil, fmt.Errorf("%w: %s", ErrClaimed, path)
		}
		return nil, fmt.Errorf("take the boot lock %q: %w", path, err)
	}
	return &bootLock{f: f, path: path}, nil
}

// release drops the claim. The lock file itself is left in place: removing it
// would let a racing daemon create a second inode and take a lock nobody else
// can see.
func (l *bootLock) release() error {
	if l.f == nil {
		return nil
	}
	f := l.f
	l.f = nil
	if err := syscall.Flock(int(f.Fd()), syscall.LOCK_UN); err != nil {
		f.Close()
		return fmt.Errorf("release the boot lock %q: %w", l.path, err)
	}
	if err := f.Close(); err != nil {
		return fmt.Errorf("close the boot lock %q: %w", l.path, err)
	}
	return nil
}
