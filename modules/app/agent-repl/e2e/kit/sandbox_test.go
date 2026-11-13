package kit

import (
	"context"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"syscall"
	"testing"
	"time"
)

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
	reason := RunHostPreflight(script, bound)
	elapsed := time.Since(start)

	// Assert: it answered, it said WHY, and it did so within the bound.
	if !strings.Contains(reason, "did not answer within") {
		t.Fatalf("reason = %q, want it to say the preflight did not answer within the bound", reason)
	}
	// 10x the bound: generous for process teardown on a loaded box, and still
	// orders of magnitude under the 9m34s hang this test exists to prevent.
	if elapsed > 10*bound {
		t.Fatalf("RunHostPreflight took %s, want it bounded near %s", elapsed, bound)
	}
}

func TestHostPreflightBoundNamesTheWedgedEngine(t *testing.T) {
	t.Parallel()
	// Arrange: a preflight that prints, then blocks past the bound. The wait
	// ends only once the script has printed -- it says so through a FIFO the
	// test reads -- so the partial output is there to quote. Racing a 300ms
	// wall-clock bound instead failed whenever a loaded host had not yet run
	// the echo when the bound fired (2026-10-06, a full-suite run).
	dir := t.TempDir()
	printed := filepath.Join(dir, "printed")
	if err := syscall.Mkfifo(printed, 0o600); err != nil {
		t.Fatalf("mkfifo: %v", err)
	}
	script := writeScript(t, dir, "preflight-partial.sh",
		"echo 'probing docker'\necho printed > "+printed+"\nexec sleep 120\n")
	ctx, endTheWait := context.WithCancel(context.Background())
	defer endTheWait()
	go func() {
		// Opening the FIFO for reading blocks until the script opens it to
		// write, which is after its echo.
		_, _ = os.ReadFile(printed)
		endTheWait()
	}()

	// Act.
	reason := HostPreflightUntil(ctx, script, 300*time.Millisecond)

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
	reason := RunHostPreflight(script, 30*time.Second)

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
	reason := RunHostPreflight(script, 30*time.Second)

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
	script := sandboxPreflightScript(t)
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
	script := sandboxPreflightScript(t)

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

func TestSandboxRunCommandEntersTheE2EModule(t *testing.T) {
	// Arrange.
	const pattern = "TestEmacsExample"

	// Act.
	got := SandboxRunCommand(pattern)

	// Assert.
	want := SandboxScriptRel + " run --dir e2e go test ./ -run TestEmacsExample -v"
	if got != want {
		t.Fatalf("sandbox run command = %q, want %q", got, want)
	}
}

// sandboxPreflightScript is the sandbox's preflight.sh, beside its entry
// point (SandboxScriptRel) in the e2e package's sandbox/bin, one directory
// above this one, the directory a test runs in.
func sandboxPreflightScript(t *testing.T) string {
	t.Helper()
	script, err := filepath.Abs(filepath.Join("..", "sandbox", "bin", "preflight.sh"))
	if err != nil {
		t.Fatalf("resolve preflight.sh: %v", err)
	}
	return script
}
