package daemonrules

import (
	"os"
	"path/filepath"
	"strings"
	"testing"

	"agent-repld/reposcans/internal/sourcescan"
)

// internalClockDir is the package these rules hold, under the daemon module's root.
const internalClockDir = "internal/clock"

// TestNoOtherPackageDeclaresItsOwnWaitingClock pins the call sites to the one
// shape: a package that hand-rolls its own Now/After clock fails here, and
// takes clock.Clock (or an alias of it) instead.
func TestNoOtherPackageDeclaresItsOwnWaitingClock(t *testing.T) {
	sourcescan.InPackage(t, internalClockDir)
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
		// clocktest.Scripted is no rival clock: it IS a clock.Clock, the one
		// scripted implementation every schedule's tests drive.
		sanctioned := rel == filepath.Join("clock", "clock.go") || rel == filepath.Join("clock", "clocktest", "clocktest.go")
		if strings.Contains(string(body), "After(d time.Duration) <-chan time.Time") && !sanctioned {
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
