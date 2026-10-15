package daemonaddr

import (
	"errors"
	"os"
	"path/filepath"
	"strings"
	"syscall"
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
	lock, _, err := acquireBootLockWithin(path, ClaimWaitBound, BootOrdinary, standDown)

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
	loser, _, err := acquireBootLockWithin(path, time.Millisecond, BootOrdinary, nil)

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
	if _, _, err := acquireBootLockWithin(path, 0, BootOrdinary, func() { refusals++ }); !errors.Is(err, ErrClaimed) {
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
	if _, _, err := acquireBootLockWithin(path, time.Millisecond, BootOrdinary, nil); !errors.Is(err, ErrClaimed) {
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
	_, _, err = acquireBootLockWithin(path, time.Nanosecond, BootOrdinary, unopenable)

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

// servingIncumbent holds the boot claim AND the serving lock, as a serving
// daemon does, and releases both at the end of the test.
func servingIncumbent(t *testing.T, path string) *bootLock {
	t.Helper()
	incumbent, err := acquireBootLock(path)
	if err != nil {
		t.Fatalf("incumbent: %v", err)
	}
	if err := incumbent.takeServing(); err != nil {
		t.Fatalf("incumbent takeServing: %v", err)
	}
	t.Cleanup(func() { incumbent.release() })
	return incumbent
}

// servingLockPath is the serving lock beside the boot lock at PATH.
func servingLockPath(path string) string {
	return filepath.Join(filepath.Dir(path), ServingName)
}

func TestServingPathSitsBesideTheAdvertisement(t *testing.T) {
	// Arrange, Act.
	got := ServingPath("/state/daemon.addr")

	// Assert.
	if got != filepath.Join("/state", ServingName) {
		t.Fatalf("ServingPath = %q, want %q", got, filepath.Join("/state", ServingName))
	}
}

func TestAFreeClaimIsTakenWithTheFreeVerdict(t *testing.T) {
	// Arrange.
	path := filepath.Join(t.TempDir(), LockName)

	// Act.
	lock, verdict, err := acquireBootLockWithin(path, ClaimWaitBound, BootOrdinary, nil)

	// Assert.
	if err != nil {
		t.Fatalf("acquireBootLockWithin: %v", err)
	}
	defer lock.release()
	if verdict != VerdictFree {
		t.Fatalf("verdict = %q, want %q", verdict, VerdictFree)
	}
}

func TestAHeldClaimWithNoWaitIsRefusedOnSight(t *testing.T) {
	// Arrange.
	path := filepath.Join(t.TempDir(), LockName)
	servingIncumbent(t, path)

	// Act.
	_, verdict, err := acquireBootLockWithin(path, 0, BootOrdinary, nil)

	// Assert.
	if !errors.Is(err, ErrClaimed) {
		t.Fatalf("error = %v, want ErrClaimed", err)
	}
	if verdict != VerdictRefusedOnSight {
		t.Fatalf("verdict = %q, want %q", verdict, VerdictRefusedOnSight)
	}
}

// TestAHeldClaimWhoseHolderServesIsRefusedAtOnce is the refusal this lock
// exists for: a serving incumbent is refused without the wait ever being
// entered, so the answer costs two flock calls instead of ClaimWaitBound.
func TestAHeldClaimWhoseHolderServesIsRefusedAtOnce(t *testing.T) {
	// Arrange.
	path := filepath.Join(t.TempDir(), LockName)
	servingIncumbent(t, path)
	waits := 0

	// Act: the production bound, which the refusal must not pay.
	_, verdict, err := acquireBootLockWithin(path, ClaimWaitBound, BootOrdinary, func() { waits++ })

	// Assert: refused, with the serving verdict, and the wait never entered.
	if !errors.Is(err, ErrClaimed) {
		t.Fatalf("error = %v, want ErrClaimed", err)
	}
	if verdict != VerdictIncumbentServing {
		t.Fatalf("verdict = %q, want %q", verdict, VerdictIncumbentServing)
	}
	if waits != 0 {
		t.Fatalf("the wait was entered %d times, want 0 for a serving holder", waits)
	}
}

// TestAHeldClaimWhoseHolderDepartedIsTakenInsideTheWait is the restart window
// with the serving lock in play: the incumbent has begun its shutdown
// (departed) and still holds the boot claim; the booting daemon waits and
// takes the claim when the incumbent's process ends.
func TestAHeldClaimWhoseHolderDepartedIsTakenInsideTheWait(t *testing.T) {
	// Arrange.
	path := filepath.Join(t.TempDir(), LockName)
	incumbent := servingIncumbent(t, path)
	if err := incumbent.depart(); err != nil {
		t.Fatalf("depart: %v", err)
	}
	exit := func() {
		if err := incumbent.release(); err != nil {
			t.Errorf("the incumbent could not exit: %v", err)
		}
	}

	// Act.
	lock, verdict, err := acquireBootLockWithin(path, ClaimWaitBound, BootOrdinary, exit)

	// Assert.
	if err != nil {
		t.Fatalf("acquireBootLockWithin: %v", err)
	}
	defer lock.release()
	if verdict != VerdictIncumbentDeparted {
		t.Fatalf("verdict = %q, want %q", verdict, VerdictIncumbentDeparted)
	}
}

// TestAHeldClaimWhoseHolderIsStillBootingIsWaitedFor: an incumbent between
// taking the boot claim and taking the serving lock reads as not serving, so
// the booting daemon waits -- and, the incumbent finishing its boot and
// serving on, is refused at the bound, which is the answer it got before the
// serving lock existed.
func TestAHeldClaimWhoseHolderIsStillBootingIsWaitedFor(t *testing.T) {
	// Arrange: the boot claim only, and the serving lock taken once the
	// booting daemon has entered the wait.
	path := filepath.Join(t.TempDir(), LockName)
	incumbent, err := acquireBootLock(path)
	if err != nil {
		t.Fatalf("incumbent: %v", err)
	}
	defer incumbent.release()
	waits := 0
	finishBoot := func() {
		waits++
		if err := incumbent.takeServing(); err != nil {
			t.Errorf("the incumbent could not finish its boot: %v", err)
		}
	}

	// Act.
	_, verdict, err := acquireBootLockWithin(path, time.Millisecond, BootOrdinary, finishBoot)

	// Assert.
	if !errors.Is(err, ErrClaimed) {
		t.Fatalf("error = %v, want ErrClaimed", err)
	}
	if verdict != VerdictHeldPastWait {
		t.Fatalf("verdict = %q, want %q", verdict, VerdictHeldPastWait)
	}
	if waits != 1 {
		t.Fatalf("the wait was entered %d times, want 1 for a holder still booting", waits)
	}
}

// TestAHolderThatNeverTakesTheServingLockIsWaitedFor is a mixed-version
// deploy: an incumbent built before the serving lock holds only the boot
// claim, reads as departing, and is waited for exactly as before.
func TestAHolderThatNeverTakesTheServingLockIsWaitedFor(t *testing.T) {
	// Arrange.
	path := filepath.Join(t.TempDir(), LockName)
	incumbent, err := acquireBootLock(path)
	if err != nil {
		t.Fatalf("incumbent: %v", err)
	}
	waits := 0
	exit := func() {
		waits++
		if err := incumbent.release(); err != nil {
			t.Errorf("the incumbent could not exit: %v", err)
		}
	}

	// Act.
	lock, verdict, err := acquireBootLockWithin(path, ClaimWaitBound, BootOrdinary, exit)

	// Assert.
	if err != nil {
		t.Fatalf("acquireBootLockWithin: %v", err)
	}
	defer lock.release()
	if verdict != VerdictIncumbentDeparted || waits != 1 {
		t.Fatalf("verdict = %q after %d waits, want %q after 1", verdict, waits, VerdictIncumbentDeparted)
	}
}

// TestAReplacingBootSkipsTheServingProbe: the replacement's incumbent spawned
// it while still serving, so a held serving lock must not refuse it.
func TestAReplacingBootSkipsTheServingProbe(t *testing.T) {
	// Arrange.
	path := filepath.Join(t.TempDir(), LockName)
	incumbent := servingIncumbent(t, path)
	exit := func() {
		if err := incumbent.release(); err != nil {
			t.Errorf("the incumbent could not exit: %v", err)
		}
	}

	// Act.
	lock, verdict, err := acquireBootLockWithin(path, ClaimWaitBound, BootReplacing, exit)

	// Assert.
	if err != nil {
		t.Fatalf("acquireBootLockWithin = %v, want the claim once the serving incumbent exits", err)
	}
	defer lock.release()
	if verdict != VerdictIncumbentDeparted {
		t.Fatalf("verdict = %q, want %q", verdict, VerdictIncumbentDeparted)
	}
}

func TestAnUndecidableServingProbeIsNotAFreeOne(t *testing.T) {
	// Arrange: the boot claim held, and the serving lock unopenable.
	if os.Geteuid() == 0 {
		t.Fatal("this test needs a non-root user: root opens a mode-000 file")
	}
	path := filepath.Join(t.TempDir(), LockName)
	incumbent, err := acquireBootLock(path)
	if err != nil {
		t.Fatalf("incumbent: %v", err)
	}
	defer incumbent.release()
	if err := os.WriteFile(servingLockPath(path), nil, 0); err != nil {
		t.Fatalf("write an unopenable serving lock: %v", err)
	}
	waits := 0

	// Act.
	_, verdict, err := acquireBootLockWithin(path, ClaimWaitBound, BootOrdinary, func() { waits++ })

	// Assert: the probe's failure is the answer; neither a refusal nor a wait.
	if err == nil || errors.Is(err, ErrClaimed) || !errors.Is(err, os.ErrPermission) {
		t.Fatalf("acquireBootLockWithin = %v, want the probe's permission error", err)
	}
	if verdict != VerdictUndecided || waits != 0 {
		t.Fatalf("verdict = %q after %d waits, want %q after none", verdict, waits, VerdictUndecided)
	}
}

// TestTwoServingProbesNeverRefuseEachOther: the probe is a shared lock, so a
// probe held at the same instant as another reads the lock free.
func TestTwoServingProbesNeverRefuseEachOther(t *testing.T) {
	// Arrange: another prober's shared lock, held.
	path := servingLockPath(filepath.Join(t.TempDir(), LockName))
	other, err := os.OpenFile(path, os.O_CREATE|os.O_RDWR, 0o644)
	if err != nil {
		t.Fatalf("open: %v", err)
	}
	defer other.Close()
	if err := syscall.Flock(int(other.Fd()), syscall.LOCK_SH|syscall.LOCK_NB); err != nil {
		t.Fatalf("the other probe's shared lock: %v", err)
	}

	// Act.
	held, err := servingHeld(path)

	// Assert.
	if err != nil || held {
		t.Fatalf("servingHeld = (%v, %v), want (false, nil) beside another probe", held, err)
	}
}

func TestDepartKeepsTheBootClaim(t *testing.T) {
	// Arrange.
	path := filepath.Join(t.TempDir(), LockName)
	incumbent := servingIncumbent(t, path)

	// Act.
	if err := incumbent.depart(); err != nil {
		t.Fatalf("depart: %v", err)
	}

	// Assert: the serving lock is free and the boot claim is still held.
	if held, err := servingHeld(servingLockPath(path)); err != nil || held {
		t.Fatalf("servingHeld after depart = (%v, %v), want (false, nil)", held, err)
	}
	if loser, err := acquireBootLock(path); !errors.Is(err, ErrClaimed) {
		if loser != nil {
			loser.release()
		}
		t.Fatalf("acquireBootLock after depart = %v, want ErrClaimed", err)
	}
}

func TestDepartIsIdempotent(t *testing.T) {
	// Arrange.
	path := filepath.Join(t.TempDir(), LockName)
	incumbent := servingIncumbent(t, path)
	if err := incumbent.depart(); err != nil {
		t.Fatalf("first depart: %v", err)
	}

	// Act.
	err := incumbent.depart()

	// Assert.
	if err != nil {
		t.Fatalf("second depart = %v, want success", err)
	}
}

func TestReleaseFreesTheServingLock(t *testing.T) {
	// Arrange.
	path := filepath.Join(t.TempDir(), LockName)
	incumbent := servingIncumbent(t, path)

	// Act.
	if err := incumbent.release(); err != nil {
		t.Fatalf("release: %v", err)
	}

	// Assert.
	if held, err := servingHeld(servingLockPath(path)); err != nil || held {
		t.Fatalf("servingHeld after release = (%v, %v), want (false, nil)", held, err)
	}
}

// TestTheServingLockFileOutlivesEveryRelease pins that the serving lock's
// inode is never unlinked or replaced: a second inode at the path would let
// two holders lock it at once.
func TestTheServingLockFileOutlivesEveryRelease(t *testing.T) {
	// Arrange.
	path := filepath.Join(t.TempDir(), LockName)
	incumbent := servingIncumbent(t, path)
	before, err := os.Stat(servingLockPath(path))
	if err != nil {
		t.Fatalf("stat the serving lock: %v", err)
	}

	// Act.
	if err := incumbent.depart(); err != nil {
		t.Fatalf("depart: %v", err)
	}
	if err := incumbent.release(); err != nil {
		t.Fatalf("release: %v", err)
	}

	// Assert.
	after, err := os.Stat(servingLockPath(path))
	if err != nil {
		t.Fatalf("stat the serving lock after release = %v, want it still there", err)
	}
	if !os.SameFile(before, after) {
		t.Fatalf("the serving lock's inode changed across depart and release")
	}
}
