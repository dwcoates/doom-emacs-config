package sandboxscripts

import (
	"bufio"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
	"time"
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
// its error and whole output. The wait is observed through the output, never
// a timed sleep; the bound only turns a hang into a failure.
func waitBehindLock(t *testing.T, cmd *exec.Cmd, dir, announce string, whileBlocked func()) (string, error) {
	t.Helper()
	reader, writer, err := os.Pipe()
	if err != nil {
		t.Fatalf("create output pipe: %v", err)
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
		t.Fatalf("start: %v", err)
	}
	waitErr := make(chan error, 1)
	go func() {
		err := cmd.Wait()
		_ = writer.Close()
		waitErr <- err
	}()

	select {
	case <-announced:
	case <-time.After(20 * time.Second):
		_ = cmd.Process.Kill()
		<-waitErr
		<-scanned
		t.Fatalf("timed out waiting for %q:\n%s", announce, output.String())
	}
	whileBlocked()

	if err := os.RemoveAll(dir); err != nil {
		t.Fatalf("release lock: %v", err)
	}
	err = <-waitErr
	<-scanned
	return output.String(), err
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
