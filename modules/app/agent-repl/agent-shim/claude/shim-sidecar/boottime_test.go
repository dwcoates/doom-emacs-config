package main

import (
	"errors"
	"testing"
	"time"
)

// TestPlatformBootTimeMillisAnswersAPlausibleBootInstant covers the ONE
// platform path this build has: each of boottime_darwin.go and
// boottime_linux.go is the sole definition on its own GOOS, so this single
// shared test exercises whichever one was compiled in.
func TestPlatformBootTimeMillisAnswersAPlausibleBootInstant(t *testing.T) {
	// Arrange: the machine running the test booted at some point in the past
	// and, on any machine that can run a Go test, after the unix epoch.
	now := time.Now().UnixMilli()

	// Act.
	got, err := platformBootTimeMillis()

	// Assert.
	if err != nil {
		t.Fatalf("platformBootTimeMillis() error = %v", err)
	}
	if got <= 0 {
		t.Fatalf("platformBootTimeMillis() = %d, want a positive unix millis timestamp", got)
	}
	if got > now {
		t.Fatalf("platformBootTimeMillis() = %d, want <= now (%d)", got, now)
	}
}

// withBootDerivation installs a boot-time derivation and clears the latch, so
// one test's latched answer can never leak into another's.
func withBootDerivation(t *testing.T, derive func() (int64, error)) {
	t.Helper()
	previous := derivePlatformBootTime
	derivePlatformBootTime = derive
	resetBootLatch()
	t.Cleanup(func() {
		derivePlatformBootTime = previous
		resetBootLatch()
	})
}

func resetBootLatch() {
	bootTime.mu.Lock()
	defer bootTime.mu.Unlock()
	bootTime.ms = 0
}

func TestBootTimeMillisHoldsOneAnswerAcrossADriftingDerivation(t *testing.T) {
	// Arrange: linux has no latched kern.boottime, so the instant is derived as
	// `time.Now() - sysinfo.Uptime` with Uptime in whole SECONDS. Two
	// derivations a moment apart therefore legitimately disagree, and a
	// wall-clock step moves the answer by the whole step. The sweep's
	// eligibility rule is `mtime < bootMs`, so a boot instant that walks
	// forward sweeps a live run whose file sits just past the old boundary.
	derived := int64(1_000)
	withBootDerivation(t, func() (int64, error) {
		derived += 1_000
		return derived, nil
	})

	// Act.
	first := bootTimeMillis()
	second := bootTimeMillis()

	// Assert: the machine's boot instant is a fixed fact, so every sweep reads
	// the same one.
	if first != second {
		t.Fatalf("bootTimeMillis() = %d then %d, want one latched answer", first, second)
	}
}

func TestBootTimeMillisDoesNotLatchAFailedDerivation(t *testing.T) {
	// Arrange: latching a failure would disable the boot sweep for the life of
	// the process off one bad syscall, and would silence BootSweep's own
	// "boot time unavailable" warning forever after.
	failing := true
	withBootDerivation(t, func() (int64, error) {
		if failing {
			return 0, errBootUnavailable
		}
		return 9_000, nil
	})

	// Act.
	unavailable := bootTimeMillis()
	failing = false
	recovered := bootTimeMillis()

	// Assert.
	if unavailable != 0 {
		t.Fatalf("bootTimeMillis() = %d on a failed derivation, want the documented 0", unavailable)
	}
	if recovered != 9_000 {
		t.Fatalf("bootTimeMillis() = %d after the derivation recovered, want 9000", recovered)
	}
}

func TestBootTimeMillisDoesNotLatchANegativeDerivation(t *testing.T) {
	// Arrange: a negative instant is not a boot time, and the "unavailable is
	// 0" contract already covers it. Latching it would pin every later sweep to
	// a nonsense boundary.
	negative := true
	withBootDerivation(t, func() (int64, error) {
		if negative {
			return -1, nil
		}
		return 9_000, nil
	})

	// Act.
	refused := bootTimeMillis()
	negative = false
	recovered := bootTimeMillis()

	// Assert.
	if refused != 0 {
		t.Fatalf("bootTimeMillis() = %d on a negative derivation, want 0", refused)
	}
	if recovered != 9_000 {
		t.Fatalf("bootTimeMillis() = %d after the derivation recovered, want 9000", recovered)
	}
}

var errBootUnavailable = errors.New("the kernel would not answer the boot instant")
