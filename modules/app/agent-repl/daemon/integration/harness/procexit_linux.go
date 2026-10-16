package harness

import (
	"context"
	"errors"
	"fmt"
	"time"

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

// await waits for the exit, bounded by ctx's deadline.
func (e *exitEvent) await(ctx context.Context) error {
	timeout := -1
	if deadline, ok := ctx.Deadline(); ok {
		timeout = int(max(time.Until(deadline), 0).Milliseconds())
	}
	fds := []unix.PollFd{{Fd: int32(e.fd), Events: unix.POLLIN}}
	for {
		n, err := unix.Poll(fds, timeout)
		if errors.Is(err, unix.EINTR) {
			continue
		}
		if err != nil {
			return fmt.Errorf("wait for the exit: %w", err)
		}
		if n == 0 {
			return context.DeadlineExceeded
		}
		return nil
	}
}

// close releases the pidfd.
func (e *exitEvent) close() {
	unix.Close(e.fd)
}
