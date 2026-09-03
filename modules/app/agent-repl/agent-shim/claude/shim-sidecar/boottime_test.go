package main

import (
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
