package merge

import (
	"crypto/md5"
	"encoding/hex"
	"fmt"
	"path/filepath"

	"claude-repld/internal/flock"
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
type repoLock = flock.Lock

// acquireRepoLock takes the repository's queue lock under dir, without
// blocking. The bool is false when another daemon holds it — a legal answer
// this daemon waits out — while an error means the lock could not be TOLD
// about, which is never read as free.
func acquireRepoLock(dir, repo string) (*repoLock, bool, error) {
	lock, ok, err := flock.TryExclusive(filepath.Join(dir, repoLockName(repo)))
	if err != nil {
		return nil, false, fmt.Errorf("merge: %w", err)
	}
	return lock, ok, nil
}
