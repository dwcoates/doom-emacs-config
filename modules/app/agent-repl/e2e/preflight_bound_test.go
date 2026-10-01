package e2e

import (
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
	"time"
)

// A PREFLIGHT EITHER ANSWERS OR IS REPORTED AS NOT ANSWERING, WITHIN A BOUND.
// It never blocks a test process. These cover that invariant at both layers:
// the Go probe (`runHostPreflight`) and the script it runs
// (`sandbox/bin/preflight.sh`).
//
// Observed 2026-09-12: a wedged Docker Desktop -- backend alive, socket never
// answering -- held `docker info` for 9m34s inside the probe's sync.Once, and
// the whole e2e package died on `go test`'s 10m timeout with no verdict.

// writeScript drops an executable shell script in a temp dir.
func writeScript(t *testing.T, dir, name, body string) string {
	t.Helper()
	path := filepath.Join(dir, name)
	if err := os.WriteFile(path, []byte("#!/usr/bin/env bash\n"+body), 0o755); err != nil {
		t.Fatalf("write %s: %v", path, err)
	}
	return path
}

func TestHostPreflightBoundReportsAScriptThatNeverAnswers(t *testing.T) {
	t.Parallel()
	// Arrange: a preflight that blocks far past the bound.
	script := writeScript(t, t.TempDir(), "preflight-hangs.sh", "exec sleep 120\n")
	const bound = 300 * time.Millisecond

	// Act.
	start := time.Now()
	reason := runHostPreflight(script, bound)
	elapsed := time.Since(start)

	// Assert: it answered, it said WHY, and it did so within the bound.
	if !strings.Contains(reason, "did not answer within") {
		t.Fatalf("reason = %q, want it to say the preflight did not answer within the bound", reason)
	}
	// 10x the bound: generous for process teardown on a loaded box, and still
	// orders of magnitude under the 9m34s hang this test exists to prevent.
	if elapsed > 10*bound {
		t.Fatalf("runHostPreflight took %s, want it bounded near %s", elapsed, bound)
	}
}

func TestHostPreflightBoundNamesTheWedgedEngine(t *testing.T) {
	t.Parallel()
	// Arrange: a preflight that prints, then blocks past the bound.
	script := writeScript(t, t.TempDir(), "preflight-partial.sh",
		"echo 'probing docker'\nexec sleep 120\n")

	// Act.
	reason := runHostPreflight(script, 300*time.Millisecond)

	// Assert: the partial output is carried, so the reader knows which
	// engine was being probed when the bound expired.
	if !strings.Contains(reason, "probing docker") {
		t.Fatalf("reason = %q, want it to quote the partial preflight output", reason)
	}
}

func TestHostPreflightQuotesANonZeroExitVerbatim(t *testing.T) {
	t.Parallel()
	// Arrange: a preflight that fails the way a missing image fails.
	const message = "agent-repl e2e sandbox UNAVAILABLE: image 'x:latest' is not built."
	script := writeScript(t, t.TempDir(), "preflight-fails.sh",
		"echo \""+message+"\"\nexit 12\n")

	// Act.
	reason := runHostPreflight(script, 30*time.Second)

	// Assert: the script's own actionable message survives unparaphrased.
	if !strings.Contains(reason, message) {
		t.Fatalf("reason = %q, want it to quote %q verbatim", reason, message)
	}
}

func TestHostPreflightIsSilentWhenTheSandboxIsUsable(t *testing.T) {
	t.Parallel()
	// Arrange: a preflight that reports READY.
	script := writeScript(t, t.TempDir(), "preflight-ready.sh",
		"echo 'agent-repl e2e sandbox READY'\nexit 0\n")

	// Act.
	reason := runHostPreflight(script, 30*time.Second)

	// Assert.
	if reason != "" {
		t.Fatalf("reason = %q, want empty for a usable sandbox", reason)
	}
}

func TestPreflightScriptExitsFourteenWhenTheRuntimeNeverAnswers(t *testing.T) {
	t.Parallel()
	// Arrange: a fake runtime binary that blocks on every call, standing in
	// for a Docker engine whose socket never answers. No real docker.
	dir := t.TempDir()
	runtime := writeScript(t, dir, "wedged-runtime", "exec sleep 120\n")
	script := filepath.Join(repoRoot(), sandboxScriptRel)
	script = filepath.Join(filepath.Dir(script), "preflight.sh")
	if _, err := os.Stat(script); err != nil {
		t.Fatalf("preflight.sh not found at %s: %v", script, err)
	}

	cmd := exec.Command(script)
	cmd.Env = append(os.Environ(),
		"AGENT_REPL_SANDBOX_RUNTIME="+runtime,
		"AGENT_REPL_SANDBOX_RUNTIME_TIMEOUT_SECONDS=1")

	// Act.
	out, err := cmd.CombinedOutput()

	// Assert.
	var code int
	if exitErr, ok := err.(*exec.ExitError); ok {
		code = exitErr.ExitCode()
	} else if err != nil {
		t.Fatalf("run preflight.sh: %v (output: %s)", err, out)
	}
	if code != 14 {
		t.Fatalf("preflight.sh exit = %d, want 14 (output: %s)", code, out)
	}
	if !strings.Contains(string(out), "did not answer within 1s") {
		t.Fatalf("preflight.sh output = %q, want it to name the expired bound", out)
	}
}

func TestPreflightScriptWedgeMessageSaysToRestartTheEngine(t *testing.T) {
	t.Parallel()
	// Arrange: the same wedged runtime, named `docker` so the docker-specific
	// remedy is the one printed.
	dir := t.TempDir()
	runtime := writeScript(t, dir, "docker", "exec sleep 120\n")
	script := filepath.Join(filepath.Dir(filepath.Join(repoRoot(), sandboxScriptRel)), "preflight.sh")

	cmd := exec.Command(script)
	cmd.Env = append(os.Environ(),
		"AGENT_REPL_SANDBOX_RUNTIME="+runtime,
		"AGENT_REPL_SANDBOX_RUNTIME_TIMEOUT_SECONDS=1")

	// Act.
	out, _ := cmd.CombinedOutput()

	// Assert: the message is actionable, not just a diagnosis.
	if !strings.Contains(string(out), "Restart Docker Desktop, then re-run.") {
		t.Fatalf("preflight.sh output = %q, want the restart instruction", out)
	}
}
