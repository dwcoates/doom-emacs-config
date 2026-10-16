package harness

import (
	"context"
	"errors"
	"fmt"
	"syscall"
	"time"

	"golang.org/x/sys/unix"
)

// awaitExitEvent waits for pid's exit on a kqueue EVFILT_PROC/NOTE_EXIT
// event, bounded by ctx's deadline, answering whether it saw the event itself
// (seen) rather than a process it could not register on (ESRCH: one that has
// begun exiting, or is gone).
//
// ON DARWIN ESRCH DOES NOT MEAN THE FILES ARE RELEASED. A process SIGKILLed a
// moment before the registration can refuse it while its exit is still under
// way and it still holds its open files: measured 2026-10-15, 2 of 300 such
// registrations answered ESRCH with the process's FIFO write end still open,
// and none of the 298 that saw the event did. So an unseen exit is answered
// as such, for a caller that needs the files released to wait on something
// else.
func awaitExitEvent(ctx context.Context, pid int) (seen bool, err error) {
	kq, err := newExitQueue()
	if err != nil {
		return false, err
	}
	defer unix.Close(kq)
	change := unix.Kevent_t{Ident: uint64(pid), Filter: unix.EVFILT_PROC, Flags: unix.EV_ADD | unix.EV_ONESHOT, Fflags: unix.NOTE_EXIT}
	if _, err := unix.Kevent(kq, []unix.Kevent_t{change}, nil, nil); err != nil {
		if errors.Is(err, unix.ESRCH) {
			return false, nil
		}
		return false, fmt.Errorf("register the exit event: %w", err)
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
			return false, fmt.Errorf("wait for the exit event: %w", err)
		}
		if n == 0 {
			return false, context.DeadlineExceeded
		}
		return true, nil
	}
}

// newExitQueue answers a kqueue descriptor marked close-on-exec.
//
// NO DESCRIPTOR LEAKS INTO A CHILD. kqueue(2) takes no close-on-exec flag, so
// the mark is a second call; both run under syscall.ForkLock's read side, as
// the standard library's own non-atomic close-on-exec paths do, so a fork from
// a concurrent test cannot land between them and inherit the queue.
func newExitQueue() (int, error) {
	syscall.ForkLock.RLock()
	defer syscall.ForkLock.RUnlock()
	kq, err := unix.Kqueue()
	if err != nil {
		return -1, fmt.Errorf("kqueue: %w", err)
	}
	unix.CloseOnExec(kq)
	return kq, nil
}
