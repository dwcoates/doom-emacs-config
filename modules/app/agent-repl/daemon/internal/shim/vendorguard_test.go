package shim

import (
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	"claude-repld/internal/vendorguard"
)

// The whole test-mode guard rests on INHERITANCE: the Go harnesses set
// AGENT_REPL_FORBID_VENDOR_CALLS on the daemon test process, and the node shim
// (and the `claude` child it would drive) must see it too. Spawn leaves
// cmd.Env nil unless ExtraEnv is set, which is what makes that true — a future
// change that always populated cmd.Env would silently unplug the guard for
// every child, so it is pinned here.
func TestSpawnPassesTheForbidVariableToTheChild(t *testing.T) {
	tests := []struct {
		name     string
		extraEnv []string
	}{
		{name: "no extra env inherits", extraEnv: nil},
		{name: "extra env still inherits", extraEnv: []string{"AGENT_REPL_OWNED=1"}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: a child that echoes the variable back as a Layer-1
			// line is more machinery than the claim needs; reading it off a
			// file the child writes is the whole assertion.
			t.Setenv(vendorguard.EnvVar, "1")
			dir := t.TempDir()
			out := filepath.Join(dir, "seen")
			sh, err := exec.LookPath("sh")
			if err != nil {
				t.Skip("sh not found in PATH")
			}
			argv := []string{sh, "-c", "printf '%s' \"$" + vendorguard.EnvVar + "\" > " + out}

			// Act
			p, err := Spawn(Options{Argv: argv, Dir: dir, ExtraEnv: tc.extraEnv, Logger: &recordingLogger{}})
			if err != nil {
				t.Fatalf("Spawn: %v", err)
			}
			if err := p.Wait(); err != nil {
				t.Fatalf("Wait: %v", err)
			}

			// Assert
			got, err := os.ReadFile(out)
			if err != nil {
				t.Fatalf("read child's view of %s: %v", vendorguard.EnvVar, err)
			}
			if strings.TrimSpace(string(got)) != "1" {
				t.Fatalf("child saw %s=%q, want %q", vendorguard.EnvVar, got, "1")
			}
		})
	}
}
