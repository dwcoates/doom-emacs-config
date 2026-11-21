package ramdisk

import (
	"errors"
	"os/exec"
	"syscall"
	"testing"
)

// heldProcess starts cat on a stdin pipe the test holds: cat exits once the
// test closes it, an event the test makes, never a clock.
func heldProcess(t *testing.T) (*exec.Cmd, func()) {
	t.Helper()
	cmd := exec.Command("/bin/cat")
	stdin, err := cmd.StdinPipe()
	if err != nil {
		t.Fatal(err)
	}
	if err := cmd.Start(); err != nil {
		t.Fatal(err)
	}
	release := func() { stdin.Close() }
	t.Cleanup(func() { release(); _ = cmd.Wait() })
	return cmd, release
}

func TestWatchProcessExitClosesExitedWhenTheProcessExits(t *testing.T) {
	// Arrange.
	cmd, release := heldProcess(t)
	w, err := WatchProcessExit(cmd.Process.Pid)
	if err != nil {
		t.Fatalf("WatchProcessExit: %v", err)
	}

	// Act.
	release()

	// Assert.
	<-w.Exited()
	if err := w.Close(); err != nil {
		t.Fatalf("Close: %v", err)
	}
}

func TestWatchProcessExitClosedBeforeTheExitNeverReportsIt(t *testing.T) {
	// Arrange.
	cmd, _ := heldProcess(t)
	w, err := WatchProcessExit(cmd.Process.Pid)
	if err != nil {
		t.Fatalf("WatchProcessExit: %v", err)
	}

	// Act: Close returns once the wait has ended.
	err = w.Close()

	// Assert.
	if err != nil {
		t.Fatalf("Close: %v", err)
	}
	select {
	case <-w.Exited():
		t.Fatal("Exited closed for a process still running")
	default:
	}
	if err := w.Close(); err != nil {
		t.Fatalf("a second Close: %v", err)
	}
}

func TestWatchProcessExitOfAProcessAlreadyExited(t *testing.T) {
	// Arrange: the process is a zombie, unreaped, so its pid stays its own;
	// its exit is read on a kqueue of the test's.
	cmd, release := heldProcess(t)
	pid := cmd.Process.Pid
	kq, err := syscall.Kqueue()
	if err != nil {
		t.Fatal(err)
	}
	defer syscall.Close(kq)
	change := syscall.Kevent_t{Ident: uint64(pid), Filter: syscall.EVFILT_PROC, Flags: syscall.EV_ADD | syscall.EV_ONESHOT, Fflags: syscall.NOTE_EXIT}
	if _, err := syscall.Kevent(kq, []syscall.Kevent_t{change}, nil, nil); err != nil {
		t.Fatal(err)
	}
	release()
	events := make([]syscall.Kevent_t, 1)
	for {
		n, err := syscall.Kevent(kq, nil, events, nil)
		if errors.Is(err, syscall.EINTR) {
			continue
		}
		if err != nil || n != 1 {
			t.Fatalf("await the exit: %d events, %v", n, err)
		}
		break
	}

	// Act.
	w, err := WatchProcessExit(pid)

	// Assert.
	if !errors.Is(err, errExitedBeforeWatch) || w != nil {
		t.Fatalf("WatchProcessExit = %v, %v; want errExitedBeforeWatch", w, err)
	}
}
