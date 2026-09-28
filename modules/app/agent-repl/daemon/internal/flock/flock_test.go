package flock

import (
	"errors"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

func TestTryExclusiveTakesAFreeLock(t *testing.T) {
	// Arrange.
	path := filepath.Join(t.TempDir(), "a.lock")

	// Act.
	lock, ok, err := TryExclusive(path)

	// Assert.
	if err != nil || !ok || lock.Path() != path {
		t.Fatalf("TryExclusive = (%v, %v, %v), want the lock at %s", lock, ok, err, path)
	}
	lock.Release()
}

func TestTryExclusiveAnswersFalseWhileHeld(t *testing.T) {
	// Arrange: a second open file description conflicts exactly as a second
	// process would.
	path := filepath.Join(t.TempDir(), "a.lock")
	held, _, err := TryExclusive(path)
	if err != nil {
		t.Fatalf("TryExclusive: %v", err)
	}
	t.Cleanup(func() { held.Release() })

	// Act.
	_, ok, err := TryExclusive(path)

	// Assert.
	if err != nil || ok {
		t.Fatalf("TryExclusive while held = (%v, %v), want (false, nil)", ok, err)
	}
}

func TestAReleasedLockIsFreeAgain(t *testing.T) {
	// Arrange.
	path := filepath.Join(t.TempDir(), "a.lock")
	held, _, err := TryExclusive(path)
	if err != nil {
		t.Fatalf("TryExclusive: %v", err)
	}
	if err := held.Release(); err != nil {
		t.Fatalf("Release: %v", err)
	}

	// Act.
	again, ok, err := TryExclusive(path)

	// Assert.
	if err != nil || !ok {
		t.Fatalf("TryExclusive after release = (%v, %v), want the lock", ok, err)
	}
	again.Release()
}

func TestTryExclusiveCreatesTheDirectory(t *testing.T) {
	// Arrange.
	path := filepath.Join(t.TempDir(), "nested", "run", "a.lock")

	// Act.
	lock, ok, err := TryExclusive(path)

	// Assert.
	if err != nil || !ok {
		t.Fatalf("TryExclusive under a missing directory = (%v, %v), want the lock", ok, err)
	}
	lock.Release()
}

func TestTryExclusiveInNeverCreatesADirectory(t *testing.T) {
	// Arrange
	missing := filepath.Join(t.TempDir(), "gone")

	// Act
	_, ok, err := TryExclusiveIn(filepath.Join(missing, "a.lock"))

	// Assert
	if !errors.Is(err, os.ErrNotExist) || ok {
		t.Fatalf("TryExclusiveIn under a missing directory = (%v, %v), want an os.ErrNotExist error", ok, err)
	}
	if _, statErr := os.Stat(missing); !errors.Is(statErr, os.ErrNotExist) {
		t.Fatalf("stat %s = %v, want the directory still absent", missing, statErr)
	}
}

func TestTryExclusiveInTakesAFreeLockInAnExistingDirectory(t *testing.T) {
	// Arrange
	path := filepath.Join(t.TempDir(), "a.lock")

	// Act
	lock, ok, err := TryExclusiveIn(path)

	// Assert
	if err != nil || !ok {
		t.Fatalf("TryExclusiveIn = (%v, %v), want the lock", ok, err)
	}
	lock.Release()
}

func TestTryExclusiveFailsWhenTheLockCannotBeToldAbout(t *testing.T) {
	// Arrange: the lock's directory is a regular file.
	root := t.TempDir()
	notADir := filepath.Join(root, "file")
	if err := os.WriteFile(notADir, nil, 0o644); err != nil {
		t.Fatalf("write: %v", err)
	}

	// Act.
	_, ok, err := TryExclusive(filepath.Join(notADir, "a.lock"))

	// Assert.
	if err == nil || ok {
		t.Fatalf("TryExclusive = (%v, %v), want an error, never a free lock", ok, err)
	}
}

func TestReleasingTwiceIsSafe(t *testing.T) {
	// Arrange.
	lock, _, err := TryExclusive(filepath.Join(t.TempDir(), "a.lock"))
	if err != nil {
		t.Fatalf("TryExclusive: %v", err)
	}
	if err := lock.Release(); err != nil {
		t.Fatalf("Release: %v", err)
	}

	// Act.
	err = lock.Release()

	// Assert.
	if err != nil {
		t.Fatalf("the second Release = %v, want nil", err)
	}
}

func TestReleasingANilLockIsSafe(t *testing.T) {
	// Arrange.
	var lock *Lock

	// Act.
	err := lock.Release()

	// Assert.
	if err != nil {
		t.Fatalf("Release on nil = %v, want nil", err)
	}
}

// TestEveryHeldNonBlockingLockGoesThroughThisPackage pins the call sites to
// the one shape: a production file that hand-rolls its own non-blocking
// exclusive flock fails here unless it is one of the two sites whose contract
// is different (see the package doc).
func TestEveryHeldNonBlockingLockGoesThroughThisPackage(t *testing.T) {
	// Arrange.
	allowed := map[string]bool{
		filepath.Join("flock", "flock.go"):     true,
		filepath.Join("sessionlock", "api.go"): true,
		filepath.Join("daemonaddr", "lock.go"): true,
	}
	var offenders []string

	// Act.
	err := filepath.WalkDir("..", func(path string, d os.DirEntry, err error) error {
		if err != nil || d.IsDir() || !strings.HasSuffix(path, ".go") || strings.HasSuffix(path, "_test.go") {
			return err
		}
		body, err := os.ReadFile(path)
		if err != nil {
			return err
		}
		rel, _ := filepath.Rel("..", path)
		if strings.Contains(string(body), "LOCK_EX|syscall.LOCK_NB") && !allowed[rel] {
			offenders = append(offenders, rel)
		}
		return nil
	})

	// Assert.
	if err != nil {
		t.Fatalf("walking the daemon's packages: %v", err)
	}
	if len(offenders) != 0 {
		t.Fatalf("hand-rolled non-blocking flocks in %v; take flock.TryExclusive", offenders)
	}
}
