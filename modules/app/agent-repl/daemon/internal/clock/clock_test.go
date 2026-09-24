package clock

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"
)

func TestSystemAfterFiresForAnElapsedDuration(t *testing.T) {
	// Arrange.
	clock := System{}

	// Act.
	fired := clock.After(0)

	// Assert: a zero duration has already passed, so the value is ready.
	if at := <-fired; at.IsZero() {
		t.Fatal("After(0) yielded the zero instant")
	}
}

func TestSystemNowIsTheWallClock(t *testing.T) {
	// Arrange.
	before := time.Now()

	// Act.
	got := System{}.Now()

	// Assert.
	if got.Before(before) {
		t.Fatalf("Now() = %v, before the instant %v taken just prior", got, before)
	}
}

// TestNoOtherPackageDeclaresItsOwnWaitingClock pins the call sites to the one
// shape: a package that hand-rolls its own Now/After clock fails here, and
// takes clock.Clock (or an alias of it) instead.
func TestNoOtherPackageDeclaresItsOwnWaitingClock(t *testing.T) {
	// Arrange.
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
		if strings.Contains(string(body), "After(d time.Duration) <-chan time.Time") && rel != filepath.Join("clock", "clock.go") {
			offenders = append(offenders, rel)
		}
		return nil
	})

	// Assert.
	if err != nil {
		t.Fatalf("walking the daemon's packages: %v", err)
	}
	if len(offenders) != 0 {
		t.Fatalf("hand-rolled clocks in %v; take clock.Clock", offenders)
	}
}
