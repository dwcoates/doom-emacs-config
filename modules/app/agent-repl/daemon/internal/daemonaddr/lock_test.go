package daemonaddr

import (
	"errors"
	"path/filepath"
	"testing"
)

func TestLockPathSitsBesideTheAdvertisement(t *testing.T) {
	// Arrange, Act.
	got := LockPath("/state/daemon.addr")

	// Assert: the file says who to talk to, the lock says who may say it.
	if got != filepath.Join("/state", LockName) {
		t.Fatalf("LockPath = %q, want %q", got, filepath.Join("/state", LockName))
	}
}

func TestAcquireBootLockTakesAFreeClaim(t *testing.T) {
	// Arrange.
	path := filepath.Join(t.TempDir(), LockName)

	// Act.
	lock, err := acquireBootLock(path)

	// Assert.
	if err != nil {
		t.Fatalf("acquireBootLock: %v", err)
	}
	defer lock.release()
}

func TestAcquireBootLockLosesToAHeldClaim(t *testing.T) {
	// Arrange.
	path := filepath.Join(t.TempDir(), LockName)
	incumbent, err := acquireBootLock(path)
	if err != nil {
		t.Fatalf("incumbent: %v", err)
	}
	defer incumbent.release()

	// Act.
	loser, err := acquireBootLock(path)

	// Assert.
	if err == nil {
		loser.release()
		t.Fatalf("a second daemon took the claim")
	}
	if !errors.Is(err, ErrClaimed) {
		t.Fatalf("error = %v, want ErrClaimed", err)
	}
}

func TestReleaseFreesTheClaimForASuccessor(t *testing.T) {
	// Arrange.
	path := filepath.Join(t.TempDir(), LockName)
	first, err := acquireBootLock(path)
	if err != nil {
		t.Fatalf("first: %v", err)
	}

	// Act.
	if err := first.release(); err != nil {
		t.Fatalf("release: %v", err)
	}
	second, err := acquireBootLock(path)

	// Assert.
	if err != nil {
		t.Fatalf("the claim was not freed: %v", err)
	}
	second.release()
}

func TestReleaseIsIdempotent(t *testing.T) {
	// Arrange.
	path := filepath.Join(t.TempDir(), LockName)
	lock, err := acquireBootLock(path)
	if err != nil {
		t.Fatalf("acquireBootLock: %v", err)
	}
	if err := lock.release(); err != nil {
		t.Fatalf("first release: %v", err)
	}

	// Act.
	err = lock.release()

	// Assert.
	if err != nil {
		t.Fatalf("second release = %v, want success", err)
	}
}
