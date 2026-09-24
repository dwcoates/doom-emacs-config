package worktreereap

import (
	"fmt"
	"os"
	"path/filepath"
	"syscall"
)

// sweepLock is the held cross-process sweep lock. The kernel arbitrates, so
// the guarantee survives a daemon that died holding it: an flock is released
// when its holder goes away.
type sweepLock struct {
	path string
	file *os.File
}

// acquireLock takes the sweep lock without blocking. The bool is false when
// another process holds it; an error means the lock could not be told about,
// which is never read as free.
func acquireLock(path string) (*sweepLock, bool, error) {
	if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
		return nil, false, fmt.Errorf("worktreereap: creating the lock directory %s: %w", filepath.Dir(path), err)
	}
	f, err := os.OpenFile(path, os.O_CREATE|os.O_RDWR, 0o644)
	if err != nil {
		return nil, false, fmt.Errorf("worktreereap: opening the sweep lock %s: %w", path, err)
	}
	if err := syscall.Flock(int(f.Fd()), syscall.LOCK_EX|syscall.LOCK_NB); err != nil {
		f.Close()
		if err == syscall.EWOULDBLOCK {
			return nil, false, nil
		}
		return nil, false, fmt.Errorf("worktreereap: locking %s: %w", path, err)
	}
	return &sweepLock{path: path, file: f}, true, nil
}

// release drops the lock.
func (l *sweepLock) release() error {
	if err := syscall.Flock(int(l.file.Fd()), syscall.LOCK_UN); err != nil {
		l.file.Close()
		return fmt.Errorf("worktreereap: unlocking %s: %w", l.path, err)
	}
	return l.file.Close()
}
