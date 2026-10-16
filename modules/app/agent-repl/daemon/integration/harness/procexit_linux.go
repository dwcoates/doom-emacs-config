package harness

import (
	"context"
	"errors"
	"fmt"
	"time"

	"golang.org/x/sys/unix"
)

// WaitProcessExit waits for pid's exit on a pidfd becoming readable, bounded
// by ctx's deadline. A pid already gone when the pidfd is opened (ESRCH) has
// exited.
//
// The pidfd needs no close-on-exec mark of its own: pidfd_open(2) always sets
// O_CLOEXEC on the descriptor it answers, so it never leaks into a child.
func WaitProcessExit(ctx context.Context, pid int) error {
	_, err := awaitExitEvent(ctx, pid)
	return err
}

// awaitExitEvent is WaitProcessExit, answering whether the exit was seen.
// pidfd_open(2) opens a zombie too, so its ESRCH is a process already reaped,
// whose files are released: seen.
func awaitExitEvent(ctx context.Context, pid int) (seen bool, err error) {
	fd, err := unix.PidfdOpen(pid, 0)
	if err != nil {
		if errors.Is(err, unix.ESRCH) {
			return true, nil
		}
		return false, fmt.Errorf("pidfd_open: %w", err)
	}
	defer unix.Close(fd)
	timeout := -1
	if deadline, ok := ctx.Deadline(); ok {
		timeout = int(max(time.Until(deadline), 0).Milliseconds())
	}
	fds := []unix.PollFd{{Fd: int32(fd), Events: unix.POLLIN}}
	for {
		n, err := unix.Poll(fds, timeout)
		if errors.Is(err, unix.EINTR) {
			continue
		}
		if err != nil {
			return false, fmt.Errorf("wait for the exit: %w", err)
		}
		if n == 0 {
			return false, context.DeadlineExceeded
		}
		return true, nil
	}
}
