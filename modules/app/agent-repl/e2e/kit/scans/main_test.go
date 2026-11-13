package scans

import (
	"fmt"
	"os"
	"path/filepath"
	"testing"
)

// layout is where the scans find the trees they read.
type layout struct {
	e2eDir  string // .../modules/app/agent-repl/e2e
	repoDir string // .../modules/app/agent-repl
	shimDir string // .../modules/app/agent-repl/agent-shim/claude/shim
}

var repo layout

// TestMain runs every scan in the e2e package's directory, the one whose
// files they read.
func TestMain(m *testing.M) {
	l, err := resolveLayout()
	if err != nil {
		fmt.Fprintln(os.Stderr, "scans:", err)
		os.Exit(1)
	}
	if err := os.Chdir(l.e2eDir); err != nil {
		fmt.Fprintln(os.Stderr, "scans: enter the e2e package:", err)
		os.Exit(1)
	}
	repo = l
	os.Exit(m.Run())
}

// resolveLayout finds the e2e package two directories above this one, the
// directory a test runs in.
func resolveLayout() (layout, error) {
	here, err := os.Getwd()
	if err != nil {
		return layout{}, fmt.Errorf("the working directory, which is the scans package's: %w", err)
	}
	l := layout{e2eDir: filepath.Clean(filepath.Join(here, "..", ".."))}
	l.repoDir = filepath.Dir(l.e2eDir)
	l.shimDir = filepath.Join(l.repoDir, "agent-shim", "claude", "shim")
	for name, path := range map[string]string{
		"the e2e package's scenarios": filepath.Join(l.e2eDir, "scenarios_test.go"),
		"the shim source":             l.shimDir,
	} {
		if _, err := os.Stat(path); err != nil {
			return layout{}, fmt.Errorf("%s not found at %s: %w", name, path, err)
		}
	}
	return l, nil
}
