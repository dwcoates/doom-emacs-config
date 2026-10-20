package harness

import (
	"errors"
	"os"
	"path/filepath"
	"regexp"
	"strconv"
	"strings"
	"testing"
	"time"

	"agent-repld/integration/grantedclock"
)

// wallBound is a hand-rolled wall-clock bound, or a deadline read off a
// context, written out in the harness's source.
var wallBound = regexp.MustCompile(`context\.WithTimeout\(|context\.WithDeadline\(|time\.After\(|time\.NewTimer\(|time\.Now\(\)\.(Add|After|Before)\(|\.Deadline\(\)`)

// suiteWallBound is wallBound for the suite's tests, less time.Now().Add,
// which they use for timestamps they hand the daemon as data; a deadline built
// from one is still caught where it is checked.
var suiteWallBound = regexp.MustCompile(`context\.WithTimeout\(|context\.WithDeadline\(|time\.After\(|time\.NewTimer\(|time\.Now\(\)\.(After|Before)\(|\.Deadline\(\)`)

// wallBoundAllowed is every wall-clock bound the harness and the suite keep on
// purpose, by file and the line's text, with why the granted clock is not the
// right clock for it.
var wallBoundAllowed = map[string]map[string]string{
	"harness/daemon.go": {
		`AGENT_REPL_LOG_LEVEL_UNTIL=%d", time.Now().Add(5*time.Minute)`: "an instant handed to the daemon as configuration, not a bound on a wait",
		`deadline := time.After(DefaultTimeout)`:                        "the coverage-only stand-down of strays, best effort and reporting nothing; a stray launchd has adopted is outside this binary's process tree",
		`deadline := time.Now().Add(probe)`:                             "ExpectFileUnchanged's negative probe window: it is waited out, never a failure bound",
		`for time.Now().Before(deadline) {`:                             "ExpectFileUnchanged's negative probe window: it is waited out, never a failure bound",
	},
	"harness/procstate.go": {
		`deadline := time.NewTimer(bound)`:     "awaitFrozen confirms a stray's SIGSTOP; a stray launchd has adopted is outside this binary's process tree",
		`report := time.NewTimer(reportAfter)`: "stopFrozen's stall report on a stray; a stray launchd has adopted is outside this binary's process tree",
	},
	"harness/proctable_darwin.go": {
		`deadline := time.NewTimer(freezeBound)`: "procArgs reads the argv of any process this user owns, most of them outside this binary's process tree",
	},
	"footer_topbar_test.go": {
		`probe := time.NewTimer(harness.ProbeWindow)`: "a negative probe window: it is waited out, never a failure bound",
	},
	"support_session_test.go": {
		`for time.Now().Before(deadline) {`: "expectRPCCount's negative probe window: it is waited out, never a failure bound",
	},
	"harness/watch.go": {
		`timer := time.NewTimer(probe)`: "firstPushMatching's negative probe window: it is waited out, never a failure bound",
	},
}

// TestEveryWallBoundIsAllowedByName pins that every bound the harness and the
// integration suite put on a wait is kept on the granted clock
// (grantedclock.WithBound): a hand-rolled wall-clock bound in either fails
// here unless wallBoundAllowed names it with its reason, and an allowance whose
// line is gone fails too, so the list never outlives what it excuses.
func TestEveryWallBoundIsAllowedByName(t *testing.T) {
	// Arrange: the harness's own sources and the suite's tests. The harness's
	// tests exercise wall-clock waits directly and are not bounds on the
	// daemon.
	var files []string
	for _, glob := range []string{"*.go", "../*_test.go"} {
		matches, err := filepath.Glob(glob)
		if err != nil {
			t.Fatalf("glob %s: %v", glob, err)
		}
		files = append(files, matches...)
	}

	// Act
	found := map[string]map[string]bool{}
	var unallowed []string
	for _, path := range files {
		if filepath.Dir(path) == "." && strings.HasSuffix(path, "_test.go") {
			continue
		}
		body, err := os.ReadFile(path)
		if err != nil {
			t.Fatalf("read %s: %v", path, err)
		}
		key, pattern := filepath.ToSlash(filepath.Join("harness", path)), wallBound
		if strings.HasPrefix(path, "..") {
			key, pattern = filepath.Base(path), suiteWallBound
		}
		for i, line := range strings.Split(string(body), "\n") {
			if !pattern.MatchString(line) || strings.HasPrefix(strings.TrimSpace(line), "//") {
				continue
			}
			allowed := false
			for text := range wallBoundAllowed[key] {
				if strings.Contains(line, text) {
					allowed = true
					if found[key] == nil {
						found[key] = map[string]bool{}
					}
					found[key][text] = true
				}
			}
			if !allowed {
				unallowed = append(unallowed, key+":"+strconv.Itoa(i+1)+": "+strings.TrimSpace(line))
			}
		}
	}

	// Assert
	if len(unallowed) > 0 {
		t.Errorf("wall-clock bounds not kept on the granted clock (use grantedclock.WithBound, or allow one by name with its reason):\n%s", strings.Join(unallowed, "\n"))
	}
	for file, texts := range wallBoundAllowed {
		for text := range texts {
			if !found[file][text] {
				t.Errorf("wallBoundAllowed names %s in %s, which no longer holds it", text, file)
			}
		}
	}
}

func TestRequireGrantedClockRefusesARunThatCannotReadIt(t *testing.T) {
	readable := func() (time.Duration, error) { return 0, nil }
	tests := []struct {
		name     string
		source   func() (grantedclock.Stall, error)
		wantFail string
	}{
		{name: "a readable stall lets the run start",
			source: func() (grantedclock.Stall, error) { return readable, nil }},
		{name: "a source that cannot be opened refuses the run",
			source:   func() (grantedclock.Stall, error) { return nil, errors.New("no hw.tbfrequency") },
			wantFail: "the CPU stall cannot be read, and every harness bound is kept on it: no hw.tbfrequency"},
		{name: "a stall that cannot be read refuses the run",
			source: func() (grantedclock.Stall, error) {
				return func() (time.Duration, error) { return 0, errors.New("EPERM") }, nil
			},
			wantFail: "the CPU stall cannot be read, and every harness bound is kept on it: EPERM"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			rec := NewRecordingTB(t, "run")

			// Act
			completed := Within(func() { requireGrantedClockFrom(rec, tt.source) })

			// Assert
			if got := rec.Failures(); completed != (tt.wantFail == "") || !strings.Contains(got, tt.wantFail) || (tt.wantFail == "" && got != "") {
				t.Fatalf("requireGrantedClockFrom() completed %v with failures %q, want %q", completed, got, tt.wantFail)
			}
		})
	}
}
