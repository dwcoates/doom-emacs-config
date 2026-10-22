package sandboxscripts

import (
	"bufio"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
)

// holdLock makes DIR a host lock this test process holds: the directory, with
// this process's pid recorded in it, exactly as claim_host_lock leaves one.
func holdLock(t *testing.T, dir string) {
	t.Helper()
	if err := os.MkdirAll(dir, 0o755); err != nil {
		t.Fatalf("mkdir lock: %v", err)
	}
	if err := os.WriteFile(filepath.Join(dir, "pid"), fmt.Appendf(nil, "%d\n", os.Getpid()), 0o644); err != nil {
		t.Fatalf("write pid: %v", err)
	}
}

// waitBehindLock starts CMD, which must block on the lock DIR this test holds.
// Once CMD's output carries ANNOUNCE it calls whileBlocked (where the caller
// asserts CMD has done nothing yet), releases DIR, waits for CMD and returns
// its error and whole output.
//
// THE WAIT HAS NO CLOCK. CMD either announces (it is blocked behind the lock)
// or exits without announcing (it did not wait, the failure); both are
// events, and the helper waits for whichever comes first. A bound on the
// announcement measured only how fast a loaded host reaches the lock: on
// 2026-10-22 a gate run on a contended host timed out two cases after 20s
// with no output at all, the script not yet at the lock. A command that does
// neither is a hang, and the test binary's own -timeout reports it.
func waitBehindLock(t *testing.T, cmd *exec.Cmd, dir, announce string, whileBlocked func()) (string, error) {
	t.Helper()
	out, err, blockErr := runBehindLock(cmd, dir, announce, whileBlocked)
	if blockErr != nil {
		t.Fatalf("%v", blockErr)
	}
	return out, err
}

// runBehindLock is waitBehindLock's body. BLOCKERR is set when CMD could not
// be started, exited before announcing, or the lock could not be released;
// otherwise OUT and ERR are CMD's whole output and its exit.
func runBehindLock(cmd *exec.Cmd, dir, announce string, whileBlocked func()) (string, error, error) {
	reader, writer, err := os.Pipe()
	if err != nil {
		return "", nil, fmt.Errorf("create output pipe: %w", err)
	}
	defer reader.Close()
	cmd.Stdout, cmd.Stderr = writer, writer
	announced := make(chan struct{}, 1)
	scanned := make(chan struct{})
	var output strings.Builder
	go func() {
		defer close(scanned)
		scanner := bufio.NewScanner(reader)
		for scanner.Scan() {
			line := scanner.Text()
			output.WriteString(line)
			output.WriteByte('\n')
			if strings.Contains(line, announce) {
				select {
				case announced <- struct{}{}:
				default:
				}
			}
		}
	}()

	if err := cmd.Start(); err != nil {
		_ = writer.Close()
		<-scanned
		return "", nil, fmt.Errorf("start: %w", err)
	}
	waitErr := make(chan error, 1)
	go func() {
		err := cmd.Wait()
		_ = writer.Close()
		waitErr <- err
	}()

	select {
	case <-announced:
	case err := <-waitErr:
		<-scanned
		return "", nil, fmt.Errorf("the command exited (%v) without announcing %q, so it never waited behind the lock:\n%s", err, announce, output.String())
	}
	whileBlocked()

	if err := os.RemoveAll(dir); err != nil {
		return "", nil, fmt.Errorf("release lock: %w", err)
	}
	err = <-waitErr
	<-scanned
	return output.String(), err, nil
}

// TestWaitBehindLockReleasesOnlyAfterTheAnnouncement drives the helper with a
// command that announces, then waits for the lock to vanish: whileBlocked runs
// while the lock still exists, and the command then finishes with its output.
func TestWaitBehindLockReleasesOnlyAfterTheAnnouncement(t *testing.T) {
	t.Parallel()
	// Arrange
	dir := filepath.Join(t.TempDir(), "held.lock")
	holdLock(t, dir)
	cmd := exec.Command("bash", "-c", `echo "WAITING here"; while [[ -d $1 ]]; do sleep 0.05; done; echo through`, "bash", dir)
	var heldDuringBlock bool

	// Act
	out, err := waitBehindLock(t, cmd, dir, "WAITING here", func() {
		_, statErr := os.Stat(dir)
		heldDuringBlock = statErr == nil
	})

	// Assert
	if err != nil {
		t.Fatalf("command = %v\n%s", err, out)
	}
	if !heldDuringBlock {
		t.Fatalf("whileBlocked ran after the lock was released")
	}
	if !strings.Contains(out, "through") {
		t.Fatalf("output lacks the command's finish:\n%s", out)
	}
}

// TestRunBehindLockFailsACommandThatExitsWithoutWaiting: a command that never
// blocks on the lock is reported by its exit, with its output, at once.
func TestRunBehindLockFailsACommandThatExitsWithoutWaiting(t *testing.T) {
	t.Parallel()
	// Arrange
	dir := filepath.Join(t.TempDir(), "held.lock")
	holdLock(t, dir)
	cmd := exec.Command("bash", "-c", `echo "went straight through"`)
	ran := false

	// Act
	_, _, blockErr := runBehindLock(cmd, dir, "WAITING here", func() { ran = true })

	// Assert
	if blockErr == nil || !strings.Contains(blockErr.Error(), "without announcing") || !strings.Contains(blockErr.Error(), "went straight through") {
		t.Fatalf("blockErr = %v, want the exit named with the command's output", blockErr)
	}
	if ran {
		t.Fatalf("whileBlocked ran for a command that never waited")
	}
	if _, err := os.Stat(dir); err != nil {
		t.Fatalf("the lock was released for a command that never waited: %v", err)
	}
}

// TestNoLockWaitTestHandRollsTheWait: every case that waits behind a held lock
// goes through waitBehindLock, so none can drift into a racy hand-rolled copy.
func TestNoLockWaitTestHandRollsTheWait(t *testing.T) {
	t.Parallel()
	// Arrange
	files, err := filepath.Glob("*_test.go")
	if err != nil {
		t.Fatalf("glob: %v", err)
	}

	// Act
	var offenders []string
	for _, f := range files {
		if f == "sandbox_lock_helpers_test.go" {
			continue
		}
		b, err := os.ReadFile(f)
		if err != nil {
			t.Fatalf("read %s: %v", f, err)
		}
		if strings.Contains(string(b), "announced := make(chan") {
			offenders = append(offenders, f)
		}
	}

	// Assert
	if len(offenders) != 0 {
		t.Fatalf("hand-rolled lock waits in %q; use waitBehindLock", offenders)
	}
}
