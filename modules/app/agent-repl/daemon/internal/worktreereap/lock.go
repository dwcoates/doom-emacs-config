package worktreereap

import (
	"fmt"

	"claude-repld/internal/flock"
)

// acquireLock takes the cross-process sweep lock without blocking. The bool
// is false when another daemon holds it; an error means the lock could not be
// told about, which is never read as free.
func acquireLock(path string) (*flock.Lock, bool, error) {
	lock, ok, err := flock.TryExclusive(path)
	if err != nil {
		return nil, false, fmt.Errorf("worktreereap: %w", err)
	}
	return lock, ok, nil
}
