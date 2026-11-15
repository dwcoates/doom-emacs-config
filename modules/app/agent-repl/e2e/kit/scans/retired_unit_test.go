package scans

import (
	"fmt"
	"os"
	"path/filepath"
	"reflect"
	"strings"
	"testing"
)

// retiredFixture is a module holding one Go integration test and one vitest
// file, the shapes a pin names.
func retiredFixture(t *testing.T) string {
	t.Helper()
	dir := t.TempDir()
	for path, body := range map[string]string{
		"daemon/integration/feed_test.go": "package integration\n\nimport \"testing\"\n\nfunc TestTheDaemonHalf(t *testing.T) {}\n\nfunc helper() {}\n",
		"agent-shim/claude/shim/test/integration/frames.test.ts": "describe(\"a query that dies\", () => {\n  test(\"denies the ask\", async () => {});\n});\n" +
			"describe(\"another block\", () => {\n  it(\"ends the turn\", async () => {});\n});\n// test(\"a commented-out test\")\n",
	} {
		full := filepath.Join(dir, path)
		if err := os.MkdirAll(filepath.Dir(full), 0o755); err != nil {
			t.Fatal(err)
		}
		if err := os.WriteFile(full, []byte(body), 0o644); err != nil {
			t.Fatal(err)
		}
	}
	return dir
}

const (
	pinDaemon = "`-go daemon/integration=TestTheDaemonHalf`"
	pinShim   = "`-vitest agent-shim/claude/shim/test/integration/frames.test.ts=a query that dies denies the ask`"
)

func TestParseRetiredReadsEveryRowOfItsSection(t *testing.T) {
	// Arrange
	lines := []string{
		"## (e) Before",
		"| `TestNot/ARetiredStep` | `!x` | " + pinDaemon + " |",
		retiredHeading,
		"| Retired step | Scenario | Replacement tests |",
		"|---|---|---|",
		"| `TestFaults/Gone` | `!query-eof-mid-ask` | " + pinShim + ", " + pinDaemon + " |",
		"## After",
		"| `TestNot/Either` | `!y` | " + pinDaemon + " |",
	}

	// Act
	got := parseRetired(t, lines)

	// Assert
	want := []retiredStep{{line: 6, step: "TestFaults/Gone", scenario: "query-eof-mid-ask", pins: []retiredPin{
		{flag: "-vitest", path: "agent-shim/claude/shim/test/integration/frames.test.ts", name: "a query that dies denies the ask"},
		{flag: "-go", path: "daemon/integration", name: "TestTheDaemonHalf"},
	}}}
	if !reflect.DeepEqual(got, want) {
		t.Fatalf("parseRetired = %+v, want %+v", got, want)
	}
}

// errorRecorder is an errorReporter that keeps what it was told.
type errorRecorder struct{ errs []string }

func (r *errorRecorder) Helper() {}
func (r *errorRecorder) Errorf(format string, args ...any) {
	r.errs = append(r.errs, fmt.Sprintf(format, args...))
}

func TestCheckRetiredCountsOnlyARowProvingBothHalves(t *testing.T) {
	dir := retiredFixture(t)
	shim := retiredPin{flag: "-vitest", path: "agent-shim/claude/shim/test/integration/frames.test.ts", name: "a query that dies denies the ask"}
	daemon := retiredPin{flag: "-go", path: "daemon/integration", name: "TestTheDaemonHalf"}
	tests := []struct {
		name      string
		step      retiredStep
		live      bool
		wantError string // "" for a row that counts
	}{
		{"both halves pinned and present", retiredStep{step: "TestFaults/Gone", scenario: "x", pins: []retiredPin{shim, daemon}}, false, ""},
		{"a describe block's other test", retiredStep{step: "TestFaults/Gone", scenario: "x", pins: []retiredPin{{flag: "-vitest", path: shim.path, name: "another block ends the turn"}, daemon}}, false, ""},
		{"a scenario the vendor does not register", retiredStep{step: "TestFaults/Gone", scenario: "nope", pins: []retiredPin{shim, daemon}}, false, "does not register"},
		{"a step still live", retiredStep{step: "TestFaults/Gone", scenario: "x", pins: []retiredPin{shim, daemon}}, true, "is still a step"},
		{"a Go test that does not exist", retiredStep{step: "TestFaults/Gone", scenario: "x", pins: []retiredPin{shim, {flag: "-go", path: daemon.path, name: "TestRenamed"}}}, false, "declares TestRenamed"},
		{"a Go name that is no test", retiredStep{step: "TestFaults/Gone", scenario: "x", pins: []retiredPin{shim, {flag: "-go", path: daemon.path, name: "helper"}}}, false, "declares helper"},
		{"a vitest test under the wrong describe", retiredStep{step: "TestFaults/Gone", scenario: "x", pins: []retiredPin{{flag: "-vitest", path: shim.path, name: "another block denies the ask"}, daemon}}, false, "names no test"},
		{"a commented-out vitest test", retiredStep{step: "TestFaults/Gone", scenario: "x", pins: []retiredPin{{flag: "-vitest", path: shim.path, name: "a commented-out test"}, daemon}}, false, "names no test"},
		{"no shim half", retiredStep{step: "TestFaults/Gone", scenario: "x", pins: []retiredPin{daemon}}, false, "in the shim's tier"},
		{"no daemon half", retiredStep{step: "TestFaults/Gone", scenario: "x", pins: []retiredPin{shim}}, false, "in the daemon's tier"},
		{"a pin in neither tier", retiredStep{step: "TestFaults/Gone", scenario: "x", pins: []retiredPin{shim, daemon, {flag: "-go", path: "e2e", name: "TestFaults"}}}, false, "in neither the shim's tier"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			r := &errorRecorder{}

			// Act
			got := checkRetired(r, []retiredStep{tt.step}, map[string]bool{"x": true}, func(string) bool { return tt.live }, dir)

			// Assert
			if tt.wantError == "" {
				if len(r.errs) != 0 || len(got["x"]) == 0 {
					t.Fatalf("errors = %q, counted = %v; want the row counted with no error", r.errs, got)
				}
				return
			}
			if len(got) != 0 {
				t.Errorf("counted %v, want the row not counted", got)
			}
			if !strings.Contains(strings.Join(r.errs, "\n"), tt.wantError) {
				t.Errorf("errors = %q, want one saying %q", r.errs, tt.wantError)
			}
		})
	}
}

func TestCheckOneTierPinsOnlyARowProvingOneHalf(t *testing.T) {
	dir := retiredFixture(t)
	shim := retiredPin{flag: "-vitest", path: "agent-shim/claude/shim/test/integration/frames.test.ts", name: "a query that dies denies the ask"}
	daemon := retiredPin{flag: "-go", path: "daemon/integration", name: "TestTheDaemonHalf"}
	tests := []struct {
		name      string
		step      retiredStep
		live      bool
		wantError string // "" for a row the section holds
	}{
		{"the shim's half alone", retiredStep{step: "TestFaults/Gone", scenario: "x", pins: []retiredPin{shim}}, false, ""},
		{"the daemon's half alone", retiredStep{step: "TestFaults/Gone", scenario: "x", pins: []retiredPin{daemon}}, false, ""},
		{"both halves belong in section (f)", retiredStep{step: "TestFaults/Gone", scenario: "x", pins: []retiredPin{shim, daemon}}, false, "move its row to section (f)"},
		{"no pin at all", retiredStep{step: "TestFaults/Gone", scenario: "x"}, false, "pins no replacement"},
		{"a pinned test that does not exist", retiredStep{step: "TestFaults/Gone", scenario: "x", pins: []retiredPin{{flag: "-go", path: daemon.path, name: "TestRenamed"}}}, false, "declares TestRenamed"},
		{"a step still live", retiredStep{step: "TestFaults/Gone", scenario: "x", pins: []retiredPin{daemon}}, true, "is still a step"},
		{"a scenario the vendor does not register", retiredStep{step: "TestFaults/Gone", scenario: "nope", pins: []retiredPin{daemon}}, false, "does not register"},
		{"a pin in neither tier", retiredStep{step: "TestFaults/Gone", scenario: "x", pins: []retiredPin{{flag: "-go", path: "e2e", name: "TestFaults"}}}, false, "in neither the shim's tier"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			r := &errorRecorder{}

			// Act
			checkOneTier(r, []retiredStep{tt.step}, map[string]bool{"x": true}, func(string) bool { return tt.live }, dir)

			// Assert
			if tt.wantError == "" {
				if len(r.errs) != 0 {
					t.Fatalf("errors = %q, want none", r.errs)
				}
				return
			}
			if !strings.Contains(strings.Join(r.errs, "\n"), tt.wantError) {
				t.Errorf("errors = %q, want one saying %q", r.errs, tt.wantError)
			}
		})
	}
}

func TestParseRetiredUnderReadsOnlyItsOwnSection(t *testing.T) {
	// Arrange
	lines := strings.Split(strings.Join([]string{
		retiredHeading,
		"| Retired step | Scenario | Replacement tests |",
		"|---|---|---|",
		"| `TestFaults/A` | `!x` | `-go daemon/integration=TestA` |",
		"",
		oneTierHeading,
		"| Retired step | Scenario | Replacement tests |",
		"|---|---|---|",
		"| `TestTurns/B` | `!y` | `-go daemon/integration=TestB` |",
		"",
		"## Another section",
		"| `TestTurns/C` | `!z` | `-go daemon/integration=TestC` |",
	}, "\n"), "\n")

	// Act
	f := parseRetiredUnder(t, lines, retiredHeading)
	g := parseRetiredUnder(t, lines, oneTierHeading)

	// Assert
	if len(f) != 1 || f[0].step != "TestFaults/A" || len(g) != 1 || g[0].step != "TestTurns/B" || g[0].scenario != "y" {
		t.Fatalf("section (f) = %+v, section (g) = %+v; want each its own one row", f, g)
	}
}
