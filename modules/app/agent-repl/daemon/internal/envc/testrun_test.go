package envc

import (
	"os"
	"path/filepath"
	"regexp"
	"strings"
	"testing"
)

func TestRequireTestRun(t *testing.T) {
	tests := []struct {
		name        string
		forbid      bool
		consequence string
		want        string
	}{
		{name: "a test run honors the seam", forbid: true, consequence: "a live daemon does not", want: ""},
		{name: "a live daemon refuses it, saying what it does instead", consequence: "a live daemon does not",
			want: "AGENT_REPL_X=1 is a test-run seam and is honored only with " + EnvForbidVendorCalls + " set; a live daemon does not"},
		{name: "a live daemon refuses it with no consequence named",
			want: "AGENT_REPL_X=1 is a test-run seam and is honored only with " + EnvForbidVendorCalls + " set"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			c := Contracts{forbidVendorCalls: tc.forbid}

			// Act
			err := c.RequireTestRun("AGENT_REPL_X=1", tc.consequence)

			// Assert
			got := ""
			if err != nil {
				got = err.Error()
			}
			if got != tc.want {
				t.Fatalf("RequireTestRun = %q, want %q", got, tc.want)
			}
		})
	}
}

// handRolledTestRunRefusal is a test-run seam's refusal spelled out by hand
// instead of through RequireTestRun.
var handRolledTestRunRefusal = regexp.MustCompile(`is a test-run seam and is honored only with`)

// TestEveryTestRunSeamRefusesThroughRequireTestRun keeps every seam's boot
// refusal on the one guard, so a hand-rolled one cannot drift from it.
func TestEveryTestRunSeamRefusesThroughRequireTestRun(t *testing.T) {
	// Arrange
	root := filepath.Join("..", "..")
	var offenders []string

	// Act
	err := filepath.WalkDir(root, func(path string, entry os.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if entry.IsDir() || filepath.Ext(path) != ".go" || filepath.Base(path) == "testrun.go" ||
			strings.HasSuffix(path, "_test.go") {
			return nil
		}
		src, err := os.ReadFile(path)
		if err != nil {
			return err
		}
		if handRolledTestRunRefusal.Match(src) {
			offenders = append(offenders, path)
		}
		return nil
	})

	// Assert
	if err != nil {
		t.Fatalf("walk the daemon's sources: %v", err)
	}
	if len(offenders) != 0 {
		t.Fatalf("%v hand-roll a test-run seam's refusal; call envc.Contracts.RequireTestRun", offenders)
	}
}

func TestRequireTestMetricsRoot(t *testing.T) {
	tests := []struct {
		name      string
		testRun   bool
		root      string
		wantError bool
	}{
		{name: "a live daemon keeps any root", root: "/Users/someone/.cache/agent-repl/metrics"},
		{name: "a test run with no root", testRun: true},
		{name: "a test run's root inside its temporary directory", testRun: true, root: "/tmp/run/T/metrics"},
		{name: "a test run's root outside it", testRun: true, root: "/Users/someone/.cache/agent-repl/metrics", wantError: true},
		{name: "a sibling that shares the prefix", testRun: true, root: "/tmp/run/Tother/metrics", wantError: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			c := Contracts{forbidVendorCalls: tt.testRun}

			// Act
			err := c.RequireTestMetricsRoot(tt.root, "/tmp/run/T")

			// Assert
			if (err != nil) != tt.wantError {
				t.Fatalf("RequireTestMetricsRoot = %v, want error %v", err, tt.wantError)
			}
			if err != nil && !strings.Contains(err.Error(), "never writes the live metrics store") {
				t.Fatalf("err = %v, want it to say why", err)
			}
		})
	}
}
