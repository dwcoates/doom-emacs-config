package harness

import (
	"context"
	"errors"
	"fmt"
	"syscall"
	"unsafe"

	"golang.org/x/sys/unix"
)

// unregisteredExitSeen is what an exit event that could not be taken says of
// the exit (awaitExitEvent): pidfd_open(2) opens a zombie too, so its ESRCH is
// a process already reaped, whose files are released.
const unregisteredExitSeen = true

// exitEvent is one process's exit event on a pidfd, which turns readable at
// the exit, held from takeExitEvent until close.
//
// The pidfd needs no close-on-exec mark of its own: pidfd_open(2) always sets
// O_CLOEXEC on the descriptor it answers, so it never leaks into a child.
type exitEvent struct {
	fd int
}

// takeExitEvent opens pid's pidfd, answering nil (and no error) for a process
// already reaped.
func takeExitEvent(pid int) (*exitEvent, error) {
	fd, err := unix.PidfdOpen(pid, 0)
	if err != nil {
		if errors.Is(err, unix.ESRCH) {
			return nil, nil
		}
		return nil, fmt.Errorf("pidfd_open: %w", err)
	}
	return &exitEvent{fd: fd}, nil
}

// await waits for the exit, until ctx ends.
//
// THE WAIT ENDS WITH CTX, NOT AT A DEADLINE READ OFF IT. A context bounded on
// the granted clock (grantedclock.WithBound) has no wall deadline to hand the
// kernel, so the poll also watches an eventfd that ctx's end writes
// (awakenOnEnd), and either descriptor ends the wait.
func (e *exitEvent) await(ctx context.Context) error {
	ended, err := unix.Eventfd(0, unix.EFD_CLOEXEC)
	if err != nil {
		return fmt.Errorf("eventfd for the context's end: %w", err)
	}
	defer unix.Close(ended)
	release := awakenOnEnd(ctx, func() {
		_, _ = unix.Write(ended, []byte{1, 0, 0, 0, 0, 0, 0, 0})
	})
	defer release()
	fds := []unix.PollFd{{Fd: int32(e.fd), Events: unix.POLLIN}, {Fd: int32(ended), Events: unix.POLLIN}}
	for {
		_, err := unix.Poll(fds, -1)
		if errors.Is(err, unix.EINTR) {
			continue
		}
		if err != nil {
			return fmt.Errorf("wait for the exit: %w", err)
		}
		if fds[0].Revents == 0 {
			return ctx.Err()
		}
		return nil
	}
}

// close releases the pidfd.
// takeEndEvent is takeExitEvent for a child of this process; on linux every
// pidfd's process can be asked how it ended (endOf).
func takeEndEvent(pid int) (*exitEvent, error) { return takeExitEvent(pid) }

// endOf answers how the exited process of the event ended, read with waitid's
// WNOWAIT so the process stays unreaped for its owner's own reap.
func (e *exitEvent) endOf(pid int) (ProcessEnd, error) {
	var info unix.Siginfo
	if err := unix.Waitid(unix.P_PIDFD, e.fd, &info, unix.WEXITED|unix.WNOWAIT, nil); err != nil {
		return ProcessEnd{}, fmt.Errorf("waitid %d: %w", pid, err)
	}
	// si_status follows si_pid and si_uid in the SIGCHLD arm of the union,
	// 24 bytes into the struct on every 64-bit linux.
	status := int(*(*int32)(unsafe.Add(unsafe.Pointer(&info), 24)))
	switch info.Code {
	case cldExited:
		return ProcessEnd{Code: status}, nil
	case cldKilled, cldDumped:
		return ProcessEnd{Signal: syscall.Signal(status)}, nil
	}
	return ProcessEnd{}, fmt.Errorf("waitid %d answered si_code %d, which is no exit", pid, info.Code)
}

// The SIGCHLD si_code values (asm-generic/siginfo.h) waitid answers for an
// exited child.
const (
	cldExited = 1
	cldKilled = 2
	cldDumped = 3
)

func (e *exitEvent) close() {
	unix.Close(e.fd)
}
