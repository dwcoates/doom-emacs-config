package harness

import (
	"context"
	"errors"
	"fmt"
	"time"

	"golang.org/x/sys/unix"
)

// WaitProcessExit waits for pid's exit on a kqueue EVFILT_PROC/NOTE_EXIT
// event, bounded by ctx's deadline. A pid already gone when the event is
// registered (ESRCH) has exited.
func WaitProcessExit(ctx context.Context, pid int) error {
	kq, err := unix.Kqueue()
	if err != nil {
		return fmt.Errorf("kqueue: %w", err)
	}
	defer unix.Close(kq)
	change := unix.Kevent_t{Ident: uint64(pid), Filter: unix.EVFILT_PROC, Flags: unix.EV_ADD | unix.EV_ONESHOT, Fflags: unix.NOTE_EXIT}
	if _, err := unix.Kevent(kq, []unix.Kevent_t{change}, nil, nil); err != nil {
		if errors.Is(err, unix.ESRCH) {
			return nil
		}
		return fmt.Errorf("register the exit event: %w", err)
	}
	var timeout *unix.Timespec
	if deadline, ok := ctx.Deadline(); ok {
		ts := unix.NsecToTimespec(max(time.Until(deadline), 0).Nanoseconds())
		timeout = &ts
	}
	events := make([]unix.Kevent_t, 1)
	for {
		n, err := unix.Kevent(kq, nil, events, timeout)
		if errors.Is(err, unix.EINTR) {
			continue
		}
		if err != nil {
			return fmt.Errorf("wait for the exit event: %w", err)
		}
		if n == 0 {
			return context.DeadlineExceeded
		}
		return nil
	}
}
