package gitclient

import (
	"bytes"
	"errors"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
)

// runHelper runs this test binary as the fetch lock's helper for the lock at
// LOCK and the git at GIT, answering its exit status and stderr.
func runHelper(t *testing.T, lock, git string, args ...string) (int, string) {
	t.Helper()
	self, err := os.Executable()
	if err != nil {
		t.Fatalf("locating the test binary: %v", err)
	}
	cmd := exec.Command(self, args...)
	cmd.Env = append(os.Environ(), fetchLockEnv+"="+lock, fetchGitEnv+"="+git)
	var stderr bytes.Buffer
	cmd.Stderr = &stderr
	err = cmd.Run()
	var exit *exec.ExitError
	switch {
	case err == nil:
		return 0, stderr.String()
	case errors.As(err, &exit):
		return exit.ExitCode(), stderr.String()
	}
	t.Fatalf("running the helper: %v", err)
	return 0, ""
}

func TestTheFetchLockHelperExecsItsGitInItsPlace(t *testing.T) {
	// Arrange: a "git" that reports its arguments and environment.
	dir := t.TempDir()
	lock := filepath.Join(dir, fetchLockName)
	git := filepath.Join(dir, "git")
	script := "#!/bin/sh\necho \"args:$*\" >&2\necho \"helper-env:${" + fetchLockEnv + "}\" >&2\nexit 7\n"
	if err := os.WriteFile(git, []byte(script), 0o755); err != nil {
		t.Fatalf("writing the git: %v", err)
	}

	// Act.
	code, stderr := runHelper(t, lock, git, "-C", "/r", "fetch", "origin")

	// Assert: git ran in the helper's place with its arguments, its exit
	// status is the helper's, and the helper's variables never reached it.
	if code != 7 {
		t.Fatalf("exit = %d, stderr %q; want git's own exit 7", code, stderr)
	}
	if !strings.Contains(stderr, "args:-C /r fetch origin") || !strings.Contains(stderr, "helper-env:\n") {
		t.Fatalf("stderr = %q, want git's arguments and no helper variables", stderr)
	}
}

func TestTheFetchLockHelperThatCannotTakeTheLockSaysWhy(t *testing.T) {
	// Arrange: a lock file in a directory that does not exist.
	lock := filepath.Join(t.TempDir(), "gone", fetchLockName)

	// Act.
	code, stderr := runHelper(t, lock, "/bin/true")

	// Assert.
	if code != fetchLockHelperFailed || !strings.Contains(stderr, "agent-repl fetch lock: taking "+lock) {
		t.Fatalf("exit = %d, stderr %q; want the helper's failure naming the lock", code, stderr)
	}
}

func TestTheFetchLockHelperThatCannotFindGitSaysWhy(t *testing.T) {
	// Arrange.
	dir := t.TempDir()
	missing := filepath.Join(dir, "no-git-here")

	// Act.
	code, stderr := runHelper(t, filepath.Join(dir, fetchLockName), missing)

	// Assert.
	if code != fetchLockHelperFailed || !strings.Contains(stderr, "agent-repl fetch lock: finding git \""+missing+"\"") {
		t.Fatalf("exit = %d, stderr %q; want the helper's failure naming the git", code, stderr)
	}
}
