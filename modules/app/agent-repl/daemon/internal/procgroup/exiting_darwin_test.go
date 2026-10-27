//go:build darwin

package procgroup

import (
	"os/exec"
	"syscall"
	"testing"

	"golang.org/x/sys/unix"
)

// watchExitUnreaped registers for pid's exit on the kernel's own exit
// notification and answers a wait that blocks until it has exited, WITHOUT
// reaping it.
//
// THE REGISTRATION COMES BEFORE THE KILL: kqueue refuses NOTE_EXIT on a
// process that has already exited (ESRCH), so registering after the kill
// failed whenever the child died first (2026-10-26 merge gate).
func watchExitUnreaped(t *testing.T, pid int) (wait func()) {
	t.Helper()
	kq, err := unix.Kqueue()
	if err != nil {
		t.Fatalf("kqueue: %v", err)
	}
	t.Cleanup(func() { unix.Close(kq) })
	change := unix.Kevent_t{Ident: uint64(pid), Filter: unix.EVFILT_PROC, Flags: unix.EV_ADD | unix.EV_ONESHOT, Fflags: unix.NOTE_EXIT}
	if _, err := unix.Kevent(kq, []unix.Kevent_t{change}, nil, nil); err != nil {
		t.Fatalf("kevent register NOTE_EXIT on %d: %v", pid, err)
	}
	return func() {
		t.Helper()
		// THE WAIT IS RESTARTED ON EINTR: the Go runtime's preemption
		// signal (SIGURG) can land on the blocked thread, and the
		// registration above stands, so waiting again loses nothing.
		events := make([]unix.Kevent_t, 1)
		for {
			_, err := unix.Kevent(kq, nil, events, nil)
			if err == nil {
				return
			}
			if err != unix.EINTR {
				t.Fatalf("kevent NOTE_EXIT on %d: %v", pid, err)
			}
		}
	}
}

func TestExitingReadsARunningProcessAsNotExiting(t *testing.T) {
	// Arrange.
	cmd := exec.Command("sleep", "60")
	if err := cmd.Start(); err != nil {
		t.Fatalf("start: %v", err)
	}
	t.Cleanup(func() { _ = cmd.Process.Kill(); _ = cmd.Wait() })

	// Act.
	got, err := Exiting(cmd.Process.Pid)

	// Assert.
	if err != nil || got {
		t.Fatalf("Exiting(running) = %v, %v; want false", got, err)
	}
}

func TestExitingReadsAnUnreapedExitAsExiting(t *testing.T) {
	// Arrange: a child killed and exited, its reap not yet taken.
	cmd := exec.Command("sleep", "60")
	if err := cmd.Start(); err != nil {
		t.Fatalf("start: %v", err)
	}
	t.Cleanup(func() { _ = cmd.Wait() })
	pid := cmd.Process.Pid
	exited := watchExitUnreaped(t, pid)
	if err := syscall.Kill(pid, syscall.SIGKILL); err != nil {
		t.Fatalf("kill: %v", err)
	}
	exited()

	// Act.
	got, err := Exiting(pid)

	// Assert.
	if err != nil || !got {
		t.Fatalf("Exiting(zombie) = %v, %v; want true", got, err)
	}
}

func TestExitingReadsAReapedProcessAsGone(t *testing.T) {
	// Arrange.
	cmd := exec.Command("true")
	if err := cmd.Run(); err != nil {
		t.Fatalf("run: %v", err)
	}

	// Act.
	got, err := Exiting(cmd.Process.Pid)

	// Assert.
	if err != nil || !got {
		t.Fatalf("Exiting(reaped) = %v, %v; want true", got, err)
	}
}
