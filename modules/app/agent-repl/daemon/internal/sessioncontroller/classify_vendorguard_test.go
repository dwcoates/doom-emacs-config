package sessioncontroller

import (
	"context"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"claude-repld/internal/vendorguard"
)

// fakeClaudeOnPath puts a `claude` executable on PATH that touches a sentinel
// file when run. The sentinel's existence is the evidence of an exec: the guard
// must return its error BEFORE the child is ever started, so a blocked run must
// leave no sentinel behind. cmd.Run is synchronous, so no waiting is involved.
func fakeClaudeOnPath(t *testing.T) (sentinel string) {
	t.Helper()
	dir := t.TempDir()
	sentinel = filepath.Join(dir, "exec-happened")
	// Shell builtins only: PATH is replaced below, so no external tool
	// (including `touch`) is reachable from inside the script.
	script := "#!/bin/sh\n: > " + sentinel + "\nprintf '%s' '" + tokenHold + "'\n"
	if err := os.WriteFile(filepath.Join(dir, "claude"), []byte(script), 0o755); err != nil {
		t.Fatalf("write fake claude: %v", err)
	}
	t.Setenv("PATH", dir)
	return sentinel
}

func TestSpawnClassifierBlocksTheExecWhenForbidden(t *testing.T) {
	// Arrange
	sentinel := fakeClaudeOnPath(t)
	t.Setenv(vendorguard.EnvVar, "1")
	// Act
	stdout, stderr, err := spawnClassifier(context.Background(), "haiku", "", "prompt")
	// Assert
	if err == nil {
		t.Fatalf("spawnClassifier() = (%q, %q, nil), want an error", stdout, stderr)
	}
	if _, statErr := os.Stat(sentinel); statErr == nil {
		t.Fatal("spawnClassifier execed the vendor CLI despite the guard")
	}
}

func TestSpawnClassifierErrorNamesTheVariable(t *testing.T) {
	// Arrange
	fakeClaudeOnPath(t)
	t.Setenv(vendorguard.EnvVar, "1")
	// Act
	_, _, err := spawnClassifier(context.Background(), "haiku", "", "prompt")
	// Assert
	if err == nil || !strings.Contains(err.Error(), vendorguard.EnvVar) {
		t.Fatalf("spawnClassifier() error = %v, want one naming %s", err, vendorguard.EnvVar)
	}
}

// The positive control: without the variable the very same call DOES exec.
// Without it, the test above could pass for the wrong reason (e.g. no `claude`
// on PATH at all).
func TestSpawnClassifierExecsWhenPermitted(t *testing.T) {
	// Arrange
	sentinel := fakeClaudeOnPath(t)
	t.Setenv(vendorguard.EnvVar, "")
	// Act
	stdout, _, err := spawnClassifier(context.Background(), "haiku", "", "prompt")
	// Assert
	if err != nil {
		t.Fatalf("spawnClassifier() = %v, want nil", err)
	}
	if _, statErr := os.Stat(sentinel); statErr != nil {
		t.Fatalf("expected the fake claude to have run: %v", statErr)
	}
	if string(stdout) != tokenHold {
		t.Fatalf("stdout = %q, want %q", stdout, tokenHold)
	}
}

// Classify is the exported surface; the guard must surface through it as an
// error rather than a verdict.
func TestClassifySurfacesTheGuardError(t *testing.T) {
	usePrompts(t)
	// Arrange
	fakeClaudeOnPath(t)
	t.Setenv(vendorguard.EnvVar, "1")
	c := NewCLIClassifier("haiku", t.Logf)
	// Act
	res, err := c.Classify(context.Background(), ClassifyRequest{QueuedPrompt: "hi"})
	// Assert
	if err == nil || !strings.Contains(err.Error(), vendorguard.EnvVar) {
		t.Fatalf("Classify() = (%v, %v), want an error naming %s", res, err, vendorguard.EnvVar)
	}
}
