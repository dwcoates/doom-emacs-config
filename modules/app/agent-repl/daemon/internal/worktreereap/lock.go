package worktreereap

import (
	"fmt"

	"agent-repld/internal/recordlock"
)

// acquireLock takes the cross-process sweep lock without blocking. The bool
// is false when another daemon holds it; an error means the lock could not be
// told about, which is never read as free.
func acquireLock(path string) (*recordlock.Record, bool, error) {
	lock, ok, err := recordlock.Hold(path)
	if err != nil {
		return nil, false, fmt.Errorf("worktreereap: %w", err)
	}
	return lock, ok, nil
}
