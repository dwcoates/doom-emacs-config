package harness

import (
	"errors"
	"os"
	"syscall"
	"testing"

	"golang.org/x/sys/unix"
)

// TestRunningThreadsCountsTheCallersOwnThread pins that runningThreads reads
// the kernel's thread table: the thread asking runs while it asks.
func TestRunningThreadsCountsTheCallersOwnThread(t *testing.T) {
	// Act
	running, err := runningThreads(os.Getpid())

	// Assert
	if err != nil || running < 1 {
		t.Fatalf("runningThreads(self) = %d, %v; want at least the calling thread", running, err)
	}
}

// TestAStoppedProcessSoonRunsNoThread pins the condition the stop-hold waits
// poll for: a stop the kernel has reported (WUNTRACED) may still have a thread
// running for an instant, and within freezeBound none runs.
func TestAStoppedProcessSoonRunsNoThread(t *testing.T) {
	// Arrange: a child stopped, as the kernel reports it to its parent.
	cmd := startIdle(t)
	if err := cmd.Process.Signal(syscall.SIGSTOP); err != nil {
		t.Fatalf("SIGSTOP: %v", err)
	}
	var ws syscall.WaitStatus
	if _, err := syscall.Wait4(cmd.Process.Pid, &ws, syscall.WUNTRACED, nil); err != nil {
		t.Fatalf("wait for the stop: %v", err)
	}

	// Act: the wait's own poll.
	err := awaitFrozen(cmd.Process.Pid, freezeBound)

	// Assert
	if err != nil {
		t.Fatalf("awaitFrozen = %v, want the stop to hold", err)
	}
	if running, err := runningThreads(cmd.Process.Pid); err != nil || running != 0 {
		t.Fatalf("runningThreads after the stop held = %d, %v; want none", running, err)
	}
}

// TestRunningThreadsOfAGoneProcessIsESRCH pins the answer readProcessState
// reads as gone.
func TestRunningThreadsOfAGoneProcessIsESRCH(t *testing.T) {
	// Arrange: a child that has exited and been reaped.
	cmd := startIdle(t)
	if err := cmd.Process.Kill(); err != nil {
		t.Fatalf("kill: %v", err)
	}
	_ = cmd.Wait()

	// Act
	_, err := runningThreads(cmd.Process.Pid)

	// Assert
	if !errors.Is(err, unix.ESRCH) {
		t.Fatalf("runningThreads(gone) = %v, want ESRCH", err)
	}
}
