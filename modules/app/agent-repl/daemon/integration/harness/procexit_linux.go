package harness

import (
	"context"
	"errors"
	"fmt"

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
func (e *exitEvent) close() {
	unix.Close(e.fd)
}
