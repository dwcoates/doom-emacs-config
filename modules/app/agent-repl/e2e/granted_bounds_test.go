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
// A bound on something OUTSIDE this binary's processes (an external engine
// answering) is the one exception, declared where it is taken by a comment
// block directly above it that opens `// wall-clock:` and says why.
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
		lines := strings.Split(string(b), "\n")
		for i, line := range lines {
			if !strings.Contains(line, "context.WithTimeout(") && !strings.Contains(line, "context.WithDeadline(") {
				continue
			}
			if wallClockDeclared(lines, i) {
				continue
			}
			offenders = append(offenders, f+":"+strconv.Itoa(i+1))
		}
	}

	// Assert
	if len(offenders) != 0 {
		t.Fatalf("wall-clock waits at %v; bound them with grantedclock.WithBound", offenders)
	}
}

// wallClockDeclared reports whether the comment block directly above line i
// opens with a `// wall-clock:` declaration of why the bound is wall time.
func wallClockDeclared(lines []string, i int) bool {
	for j := i - 1; j >= 0; j-- {
		trimmed := strings.TrimSpace(lines[j])
		if !strings.HasPrefix(trimmed, "//") {
			return false
		}
		if strings.HasPrefix(trimmed, "// wall-clock:") {
			return true
		}
	}
	return false
}

// TestWallClockDeclaredReadsOnlyTheCommentBlockAboveTheBound pins the one
// exception's shape: a declaration in the comment block directly above
// counts, and one separated from the bound by code does not.
func TestWallClockDeclaredReadsOnlyTheCommentBlockAboveTheBound(t *testing.T) {
	t.Parallel()
	tests := []struct {
		name  string
		lines []string
		want  bool
	}{
		{"declared directly above", []string{"\t// wall-clock: an external engine", "\t// answers in wall time", "\tctx, cancel := context.WithTimeout(p, b)"}, true},
		{"no declaration", []string{"\t// a wait", "\tctx, cancel := context.WithTimeout(p, b)"}, false},
		{"declaration above unrelated code", []string{"\t// wall-clock: elsewhere", "\tx := 1", "\tctx, cancel := context.WithTimeout(p, b)"}, false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := wallClockDeclared(tt.lines, len(tt.lines)-1)

			// Assert
			if got != tt.want {
				t.Fatalf("wallClockDeclared = %v, want %v", got, tt.want)
			}
		})
	}
}
