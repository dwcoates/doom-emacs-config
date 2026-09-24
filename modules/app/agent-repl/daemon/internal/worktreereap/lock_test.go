package worktreereap

import (
	"path/filepath"
	"testing"
)

func TestTheSweepLockIsExclusive(t *testing.T) {
	// Arrange.
	path := filepath.Join(t.TempDir(), "reap.lock")
	first, ok, err := acquireLock(path)
	if err != nil || !ok {
		t.Fatalf("the first acquire = (%v, %v), want the lock", ok, err)
	}
	t.Cleanup(func() { first.release() })

	// Act.
	_, again, err := acquireLock(path)

	// Assert.
	if err != nil || again {
		t.Fatalf("the second acquire = (%v, %v), want (false, nil) while it is held", again, err)
	}
}

func TestAReleasedSweepLockCanBeTakenAgain(t *testing.T) {
	// Arrange.
	path := filepath.Join(t.TempDir(), "reap.lock")
	first, _, err := acquireLock(path)
	if err != nil {
		t.Fatalf("acquire: %v", err)
	}
	if err := first.release(); err != nil {
		t.Fatalf("release: %v", err)
	}

	// Act.
	second, ok, err := acquireLock(path)

	// Assert.
	if err != nil || !ok {
		t.Fatalf("the re-acquire = (%v, %v), want the lock", ok, err)
	}
	second.release()
}

func TestTheSweepLocksDirectoryIsCreated(t *testing.T) {
	// Arrange.
	path := filepath.Join(t.TempDir(), "nested", "run", "reap.lock")

	// Act.
	lock, ok, err := acquireLock(path)

	// Assert.
	if err != nil || !ok {
		t.Fatalf("acquire under a missing directory = (%v, %v), want the lock", ok, err)
	}
	lock.release()
}
