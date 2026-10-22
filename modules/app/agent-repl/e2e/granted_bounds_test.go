package e2e

import (
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"testing"
)

// TestNoE2EWaitIsBoundOnWallTime pins that every bound this layer keeps on
// its processes' progress is a grantedclock.WithBound context, never a wall
// timeout. A wall bound measures how busy the host is: on 2026-10-22 a gate
// run on a host carrying a second full run failed
// TestSubagentDetachedSurvivesAnInterjection and
// TestADetachedSubagentWhoseProcessDiedIsNotLiveAfterAColdBoot on
// context.WithTimeout waits for events that were merely late. The granted
// clock charges only the time this binary's processes were not starved,
// which is the harness's own rule (integration/harness/granted.go).
//
// realtest/ drives the owner's real editor on the host, where wall time is the
// subject, and is not this package.
func TestNoE2EWaitIsBoundOnWallTime(t *testing.T) {
	t.Parallel()
	// Arrange
	files, err := filepath.Glob("*.go")
	if err != nil {
		t.Fatalf("glob: %v", err)
	}

	// Act
	var offenders []string
	for _, f := range files {
		if f == "granted_bounds_test.go" {
			continue
		}
		b, err := os.ReadFile(f)
		if err != nil {
			t.Fatalf("read %s: %v", f, err)
		}
		for i, line := range strings.Split(string(b), "\n") {
			if strings.Contains(line, "context.WithTimeout(") || strings.Contains(line, "context.WithDeadline(") {
				offenders = append(offenders, f+":"+strconv.Itoa(i+1))
			}
		}
	}

	// Assert
	if len(offenders) != 0 {
		t.Fatalf("wall-clock waits at %v; bound them with grantedclock.WithBound", offenders)
	}
}
