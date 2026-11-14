//go:build linux

package reap

import (
	"errors"
	"fmt"
	"time"

	"golang.org/x/sys/unix"
)

// AwaitExit implements Table through pidfds: each pid's pidfd turns readable
// at its exit, the kernel's own word that the process has exited, waited on,
// never polled for.
func (OSTable) AwaitExit(pids []int, bound time.Duration) ([]int, error) {
	var fds []unix.PollFd
	waiting := map[int32]int{}
	defer func() {
		for fd := range waiting {
			_ = unix.Close(int(fd))
		}
	}()
	for _, pid := range pids {
		fd, err := unix.PidfdOpen(pid, 0)
		switch {
		case errors.Is(err, unix.ESRCH):
			// Already exited and reaped.
		case err != nil:
			return nil, fmt.Errorf("watch pid %d's exit (pidfd_open): %w", pid, err)
		default:
			waiting[int32(fd)] = pid
			fds = append(fds, unix.PollFd{Fd: int32(fd), Events: unix.POLLIN})
		}
	}
	deadline := time.Now().Add(bound)
	for len(fds) > 0 {
		left := time.Until(deadline)
		if left <= 0 {
			break
		}
		// Rounded up, so a wait shorter than a millisecond still waits.
		_, err := unix.Poll(fds, int((left+time.Millisecond-1)/time.Millisecond))
		if errors.Is(err, unix.EINTR) {
			continue
		}
		if err != nil {
			return nil, fmt.Errorf("wait for the killed processes' exits: %w", err)
		}
		still := fds[:0]
		for _, p := range fds {
			if p.Revents == 0 {
				still = append(still, p)
				continue
			}
			_ = unix.Close(int(p.Fd))
			delete(waiting, p.Fd)
		}
		fds = still
	}
	var left []int
	for _, p := range fds {
		left = append(left, waiting[p.Fd])
	}
	return left, nil
}
