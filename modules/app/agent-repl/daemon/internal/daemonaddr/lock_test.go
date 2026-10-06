package daemonaddr

import (
	"errors"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"
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

// TestAcquireBootLockWithinTakesAClaimReleasedInsideTheBound is the restart
// window itself: the outgoing daemon still holds the claim when the
// replacement checks, and releases it while the replacement is waiting.
func TestAcquireBootLockWithinTakesAClaimReleasedInsideTheBound(t *testing.T) {
	// Arrange: an incumbent holds the claim, and stands down at exactly the
	// moment the waiter has been refused once and is entering the wait.
	path := filepath.Join(t.TempDir(), LockName)
	incumbent, err := acquireBootLock(path)
	if err != nil {
		t.Fatalf("incumbent: %v", err)
	}
	standDown := func() {
		if err := incumbent.release(); err != nil {
			t.Errorf("the incumbent could not stand down: %v", err)
		}
	}

	// Act.
	lock, err := acquireBootLockWithin(path, ClaimWaitBound, standDown)

	// Assert: the replacement took the claim rather than exiting on the first
	// refusal, which is what destroyed the daemon on 2026-09-12.
	if err != nil {
		t.Fatalf("acquireBootLockWithin: %v", err)
	}
	defer lock.release()
}

// TestAcquireBootLockWithinLosesToAClaimHeldPastTheBound pins the other half:
// a claim still held when the bound runs out is a LIVE incumbent, and
// ErrClaimed remains the right answer for it.
func TestAcquireBootLockWithinLosesToAClaimHeldPastTheBound(t *testing.T) {
	// Arrange: the incumbent never stands down.
	path := filepath.Join(t.TempDir(), LockName)
	incumbent, err := acquireBootLock(path)
	if err != nil {
		t.Fatalf("incumbent: %v", err)
	}
	defer incumbent.release()

	// Act.
	loser, err := acquireBootLockWithin(path, time.Millisecond, nil)

	// Assert.
	if err == nil {
		loser.release()
		t.Fatalf("a second daemon took a claim that was never released")
	}
	if !errors.Is(err, ErrClaimed) {
		t.Fatalf("error = %v, want ErrClaimed", err)
	}
}

// TestAcquireBootLockWithinRefusesOnSightWithNoBound pins the zero-wait form
// that the suites and BindJoining rely on: no bound, no waiting.
func TestAcquireBootLockWithinRefusesOnSightWithNoBound(t *testing.T) {
	// Arrange.
	path := filepath.Join(t.TempDir(), LockName)
	incumbent, err := acquireBootLock(path)
	if err != nil {
		t.Fatalf("incumbent: %v", err)
	}
	defer incumbent.release()
	refusals := 0

	// Act.
	if _, err := acquireBootLockWithin(path, 0, func() { refusals++ }); !errors.Is(err, ErrClaimed) {
		t.Fatalf("error = %v, want ErrClaimed", err)
	}

	// Assert: the wait was never entered, so the refusal seam never fired.
	if refusals != 0 {
		t.Fatalf("refusal seam fired %d times, want 0 with no bound", refusals)
	}
}

// TestAcquireBootLockWithinReleasesAClaimItWonAfterTheBound pins the cleanup:
// the blocking attempt is still queued in the kernel when the bound runs out,
// and a claim it wins afterwards belongs to nobody and must not be leaked.
func TestAcquireBootLockWithinReleasesAClaimItWonAfterTheBound(t *testing.T) {
	// Arrange: a bound that expires while the incumbent still holds the claim.
	path := filepath.Join(t.TempDir(), LockName)
	incumbent, err := acquireBootLock(path)
	if err != nil {
		t.Fatalf("incumbent: %v", err)
	}
	if _, err := acquireBootLockWithin(path, time.Millisecond, nil); !errors.Is(err, ErrClaimed) {
		t.Fatalf("error = %v, want ErrClaimed", err)
	}

	// Act: the incumbent stands down, so the late attempt wins the claim.
	if err := incumbent.release(); err != nil {
		t.Fatalf("release: %v", err)
	}

	// Assert: a blocking attempt returns, which it can only do once the late
	// winner has handed the claim back. This waits in the kernel rather than
	// polling, so nothing here depends on timing.
	f, err := openBootLock(path)
	if err != nil {
		t.Fatalf("open: %v", err)
	}
	after, err := blockForBootLock(f, path)
	if err != nil {
		t.Fatalf("the claim was never handed back: %v", err)
	}
	after.release()
}

func TestProbeBootClaimReportsAFreeClaim(t *testing.T) {
	// Arrange.
	addrPath := filepath.Join(t.TempDir(), "daemon.addr")

	// Act, Assert.
	if err := ProbeBootClaim(addrPath); err != nil {
		t.Fatalf("ProbeBootClaim on a free claim = %v, want nil", err)
	}
}

func TestProbeBootClaimReportsAHeldClaim(t *testing.T) {
	// Arrange.
	addrPath := filepath.Join(t.TempDir(), "daemon.addr")
	incumbent, err := acquireBootLock(LockPath(addrPath))
	if err != nil {
		t.Fatalf("incumbent: %v", err)
	}
	defer incumbent.release()

	// Act.
	err = ProbeBootClaim(addrPath)

	// Assert.
	if !errors.Is(err, ErrClaimed) {
		t.Fatalf("ProbeBootClaim on a held claim = %v, want ErrClaimed", err)
	}
}

// TestProbeBootClaimLeavesTheClaimFree pins that the probe is a question and
// not a taking: a daemon booting right after one must still find the claim.
func TestProbeBootClaimLeavesTheClaimFree(t *testing.T) {
	// Arrange.
	addrPath := filepath.Join(t.TempDir(), "daemon.addr")
	if err := ProbeBootClaim(addrPath); err != nil {
		t.Fatalf("ProbeBootClaim: %v", err)
	}

	// Act.
	lock, err := acquireBootLock(LockPath(addrPath))

	// Assert.
	if err != nil {
		t.Fatalf("the probe kept the claim: %v", err)
	}
	lock.release()
}

// TestProbeBootClaimSurfacesAnUndecidableClaim pins the third answer: a claim
// that could not be looked at is reported as an error of its own, never as a
// free one.
func TestProbeBootClaimSurfacesAnUndecidableClaim(t *testing.T) {
	// Arrange: the lock's directory is a file, so the lock cannot be opened.
	blocked := filepath.Join(t.TempDir(), "not-a-directory")
	if err := os.WriteFile(blocked, []byte("x"), 0o644); err != nil {
		t.Fatalf("WriteFile: %v", err)
	}

	// Act.
	err := ProbeBootClaim(filepath.Join(blocked, "daemon.addr"))

	// Assert.
	if err == nil {
		t.Fatalf("ProbeBootClaim on an unreadable claim = nil, want an error")
	}
	if errors.Is(err, ErrClaimed) {
		t.Fatalf("an undecidable claim was reported as held: %v", err)
	}
}

func TestABoundedWaitOpensTheLockFileBeforeItAnswers(t *testing.T) {
	// Arrange: the incumbent holds the claim, and the lock file becomes
	// unopenable the moment the first attempt is refused.
	if os.Geteuid() == 0 {
		t.Fatal("this test needs a non-root user: root opens a mode-000 file")
	}
	path := filepath.Join(t.TempDir(), LockName)
	incumbent, err := acquireBootLock(path)
	if err != nil {
		t.Fatalf("incumbent: %v", err)
	}
	defer incumbent.release()
	unopenable := func() {
		if err := os.Chmod(path, 0); err != nil {
			t.Errorf("chmod the lock file: %v", err)
		}
	}

	// Act: the shortest bound there is, so an open left to a goroutine would
	// lose the race to the timer and be dropped.
	_, err = acquireBootLockWithin(path, time.Nanosecond, unopenable)

	// Assert: the wait's own open ran synchronously and its failure is the
	// answer, never a goroutine's error dropped behind a timeout.
	if err == nil || errors.Is(err, ErrClaimed) || !errors.Is(err, os.ErrPermission) {
		t.Fatalf("acquireBootLockWithin = %v, want the open's permission error", err)
	}
}

func TestBlockForBootLockRefusesADescriptorItCannotLock(t *testing.T) {
	// Arrange: a lock file whose descriptor is already closed.
	path := filepath.Join(t.TempDir(), LockName)
	f, err := openBootLock(path)
	if err != nil {
		t.Fatalf("openBootLock: %v", err)
	}
	f.Close()

	// Act.
	_, err = blockForBootLock(f, path)

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "wait for the boot lock") {
		t.Fatalf("blockForBootLock = %v, want the failed wait named", err)
	}
}
