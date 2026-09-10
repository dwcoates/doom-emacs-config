package merge

import (
	"crypto/md5"
	"encoding/hex"
	"fmt"
	"os"
	"path/filepath"
	"syscall"
)

// This file holds the merge queue's REPO-SCOPED KERNEL LOCK.
//
// The durable queue keeps the order; the lock keeps two daemons from running
// one repository's queue at the same time. The kernel arbitrates, so the
// guarantee survives a daemon that died without cleaning up: an flock is
// released by the kernel when the holding process goes away, which no
// check-then-act on a state file can promise.

// repoLockName derives the lock file's name from a repository key. It hashes
// the key rather than embedding it because a common dir is a path, and a path
// is neither short enough nor safe enough to be a file name.
func repoLockName(repo string) string {
	sum := md5.Sum([]byte(filepath.Clean(repo)))
	return fmt.Sprintf("merge-%s.lock", hex.EncodeToString(sum[:])[:8])
}

// repoLock is one held repository lock.
type repoLock struct {
	// Path is the lock file, kept for the log record.
	Path string
	file *os.File
}

// Release drops the lock. Releasing twice is safe, because a merge's teardown
// runs on both the ordinary and the failing path.
func (l *repoLock) Release() error {
	if l == nil || l.file == nil {
		return nil
	}
	f := l.file
	l.file = nil
	if err := syscall.Flock(int(f.Fd()), syscall.LOCK_UN); err != nil {
		f.Close()
		return fmt.Errorf("merge: unlocking %s: %w", l.Path, err)
	}
	return f.Close()
}

// acquireRepoLock takes the repository's queue lock under dir, without
// blocking. The bool is false when another daemon holds it — a legal answer
// this daemon waits out — while an error means the lock could not be TOLD
// about, which is never read as free.
func acquireRepoLock(dir, repo string) (*repoLock, bool, error) {
	if err := os.MkdirAll(dir, 0o755); err != nil {
		return nil, false, fmt.Errorf("merge: creating the lock directory %s: %w", dir, err)
	}
	path := filepath.Join(dir, repoLockName(repo))
	f, err := os.OpenFile(path, os.O_CREATE|os.O_RDWR, 0o644)
	if err != nil {
		return nil, false, fmt.Errorf("merge: opening the repo lock %s: %w", path, err)
	}
	if err := syscall.Flock(int(f.Fd()), syscall.LOCK_EX|syscall.LOCK_NB); err != nil {
		f.Close()
		if err == syscall.EWOULDBLOCK {
			return nil, false, nil
		}
		return nil, false, fmt.Errorf("merge: locking %s: %w", path, err)
	}
	return &repoLock{Path: path, file: f}, true, nil
}
