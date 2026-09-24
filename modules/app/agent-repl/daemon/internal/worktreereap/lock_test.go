package worktreereap

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

func TestTheSweepLockIsExclusive(t *testing.T) {
	// Arrange.
	path := filepath.Join(t.TempDir(), "reap.lock")
	first, ok, err := acquireLock(path)
	if err != nil || !ok {
		t.Fatalf("the first acquire = (%v, %v), want the lock", ok, err)
	}
	t.Cleanup(func() { first.Release() })

	// Act.
	_, again, err := acquireLock(path)

	// Assert.
	if err != nil || again {
		t.Fatalf("the second acquire = (%v, %v), want (false, nil) while it is held", again, err)
	}
}

func TestASweepLockFailureNamesThePackage(t *testing.T) {
	// Arrange: the lock's directory is a regular file.
	notADir := filepath.Join(t.TempDir(), "file")
	if err := os.WriteFile(notADir, nil, 0o644); err != nil {
		t.Fatalf("write: %v", err)
	}

	// Act.
	_, _, err := acquireLock(filepath.Join(notADir, "reap.lock"))

	// Assert.
	if err == nil || !strings.HasPrefix(err.Error(), "worktreereap: ") {
		t.Fatalf("acquireLock = %v, want the failure prefixed with the package", err)
	}
}
