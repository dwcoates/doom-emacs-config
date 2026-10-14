//go:build darwin

package shimclient

import (
	"os/exec"
	"syscall"
	"testing"

	"golang.org/x/sys/unix"
)

// awaitExitUnreaped blocks until pid has exited, WITHOUT reaping it, on the
// kernel's own exit notification.
func awaitExitUnreaped(t *testing.T, pid int) {
	t.Helper()
	kq, err := unix.Kqueue()
	if err != nil {
		t.Fatalf("kqueue: %v", err)
	}
	defer unix.Close(kq)
	change := unix.Kevent_t{Ident: uint64(pid), Filter: unix.EVFILT_PROC, Flags: unix.EV_ADD | unix.EV_ONESHOT, Fflags: unix.NOTE_EXIT}
	events := make([]unix.Kevent_t, 1)
	if _, err := unix.Kevent(kq, []unix.Kevent_t{change}, events, nil); err != nil {
		t.Fatalf("kevent NOTE_EXIT on %d: %v", pid, err)
	}
}

func TestProcExitingReadsARunningProcessAsNotExiting(t *testing.T) {
	// Arrange.
	cmd := exec.Command("sleep", "60")
	if err := cmd.Start(); err != nil {
		t.Fatalf("start: %v", err)
	}
	t.Cleanup(func() { _ = cmd.Process.Kill(); _ = cmd.Wait() })

	// Act.
	got, err := procExiting(cmd.Process.Pid)

	// Assert.
	if err != nil || got {
		t.Fatalf("procExiting(running) = %v, %v; want false", got, err)
	}
}

func TestProcExitingReadsAnUnreapedExitAsExiting(t *testing.T) {
	// Arrange: a child killed and exited, its reap not yet taken.
	cmd := exec.Command("sleep", "60")
	if err := cmd.Start(); err != nil {
		t.Fatalf("start: %v", err)
	}
	t.Cleanup(func() { _ = cmd.Wait() })
	pid := cmd.Process.Pid
	if err := syscall.Kill(pid, syscall.SIGKILL); err != nil {
		t.Fatalf("kill: %v", err)
	}
	awaitExitUnreaped(t, pid)

	// Act.
	got, err := procExiting(pid)

	// Assert.
	if err != nil || !got {
		t.Fatalf("procExiting(zombie) = %v, %v; want true", got, err)
	}
}

func TestProcExitingReadsAReapedProcessAsGone(t *testing.T) {
	// Arrange.
	cmd := exec.Command("true")
	if err := cmd.Run(); err != nil {
		t.Fatalf("run: %v", err)
	}

	// Act.
	got, err := procExiting(cmd.Process.Pid)

	// Assert.
	if err != nil || !got {
		t.Fatalf("procExiting(reaped) = %v, %v; want true", got, err)
	}
}
