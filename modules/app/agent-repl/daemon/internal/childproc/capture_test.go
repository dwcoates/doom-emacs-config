package childproc

import (
	"os"
	"path/filepath"
	"strings"
	"syscall"
	"testing"
)

func TestCaptureAnswersEachStream(t *testing.T) {
	// Arrange
	cmd := Command(Stable, "/bin/sh", "-c", `printf out; printf err >&2`)
	capture, err := CaptureOutput(cmd)
	if err != nil {
		t.Fatalf("CaptureOutput: %v", err)
	}

	// Act
	runErr := cmd.Run()
	stdout, stderr, err := capture.Finish()

	// Assert
	if runErr != nil || err != nil || stdout != "out" || stderr != "err" {
		t.Fatalf("run %v, Finish = %q, %q, %v; want %q, %q and no errors", runErr, stdout, stderr, err, "out", "err")
	}
}

func TestCaptureHandsTheChildFilesNotPipes(t *testing.T) {
	// Arrange
	cmd := Command(Stable, "true")

	// Act
	capture, err := CaptureOutput(cmd)
	if err != nil {
		t.Fatalf("CaptureOutput: %v", err)
	}
	t.Cleanup(func() { _ = capture.Close() })

	// Assert: an *os.File is the child's own descriptor, which exec never
	// copies, so Wait cannot wait on whoever else holds it.
	if _, ok := cmd.Stdout.(*os.File); !ok {
		t.Fatalf("cmd.Stdout is %T, want *os.File", cmd.Stdout)
	}
	if _, ok := cmd.Stderr.(*os.File); !ok {
		t.Fatalf("cmd.Stderr is %T, want *os.File", cmd.Stderr)
	}
}

func TestCaptureLeavesNoFileOnDisk(t *testing.T) {
	// Arrange
	tmp := t.TempDir()
	t.Setenv("TMPDIR", tmp)
	cmd := Command(Stable, "true")

	// Act
	capture, err := CaptureOutput(cmd)
	if err != nil {
		t.Fatalf("CaptureOutput: %v", err)
	}
	t.Cleanup(func() { _ = capture.Close() })
	entries, readErr := os.ReadDir(tmp)

	// Assert
	if readErr != nil || len(entries) != 0 {
		t.Fatalf("TMPDIR holds %v (%v) while the capture is open; want nothing", entries, readErr)
	}
}

func TestCaptureRefusesWhenNoFileCanBeMade(t *testing.T) {
	// Arrange
	t.Setenv("TMPDIR", filepath.Join(t.TempDir(), "absent"))
	cmd := Command(Stable, "true")

	// Act
	capture, err := CaptureOutput(cmd)

	// Assert: the refusal names the stream, and cmd is left untouched.
	if err == nil || capture != nil || !strings.Contains(err.Error(), "make the child's stdout file") {
		t.Fatalf("CaptureOutput = %v, %v; want a refusal naming the stdout file", capture, err)
	}
	if cmd.Stdout != nil || cmd.Stderr != nil {
		t.Fatalf("cmd.Stdout = %v, cmd.Stderr = %v; want both untouched", cmd.Stdout, cmd.Stderr)
	}
}

func TestWaitReturnsWhileADescendantHoldsTheOutput(t *testing.T) {
	// Arrange: the child leaves a cat behind that inherits its output and
	// lives until this test opens and closes the pipe.
	fifo := filepath.Join(t.TempDir(), "holder")
	if err := syscall.Mkfifo(fifo, 0o600); err != nil {
		t.Fatalf("mkfifo: %v", err)
	}
	cmd := Command(Stable, "/bin/sh", "-c", `/bin/cat <"$0" >/dev/null & printf done`, fifo)
	capture, err := CaptureOutput(cmd)
	if err != nil {
		t.Fatalf("CaptureOutput: %v", err)
	}
	done := make(chan error, 1)

	// Act
	go func() { done <- cmd.Run() }()
	// The rendezvous: the open returns once the cat is live and reading.
	writer, err := os.OpenFile(fifo, os.O_WRONLY, 0)
	if err != nil {
		t.Fatalf("meet the holder: %v", err)
	}
	t.Cleanup(func() { writer.Close() })
	runErr := <-done
	stdout, _, finishErr := capture.Finish()

	// Assert
	if runErr != nil || finishErr != nil || stdout != "done" {
		t.Fatalf("run %v, Finish = %q, %v; want %q and no errors", runErr, stdout, finishErr, "done")
	}
}
