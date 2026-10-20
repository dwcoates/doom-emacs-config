package harness

import (
	"context"
	"errors"
	"fmt"
	"syscall"

	"golang.org/x/sys/unix"
)

// unregisteredExitSeen is what an exit event that could not be taken says of
// the exit (awaitExitEvent).
//
// ON DARWIN AN UNTAKEN EVENT DOES NOT MEAN THE FILES ARE RELEASED. A process
// SIGKILLed a moment before the registration refuses it (ESRCH: the kernel
// will not find a process whose exit is under way) while it still holds its
// open files: measured 2026-10-15, 2 of 300 such registrations answered ESRCH
// with the process's FIFO write end still open, and none of the 298 that saw
// the event did. The same lookup refuses it to the group listing, so a caller
// that needs the files released takes the event before the kill
// (holdGroupExits).
const unregisteredExitSeen = false

// exitEvent is one process's exit event on a kqueue EVFILT_PROC/NOTE_EXIT
// registration, held from takeExitEvent until close.
type exitEvent struct {
	kq int
}

// takeExitEvent registers for pid's exit, answering nil (and no error) for a
// process the kernel refuses the registration for (ESRCH: one that has begun
// exiting, or is gone).
func takeExitEvent(pid int) (*exitEvent, error) {
	kq, err := newExitQueue()
	if err != nil {
		return nil, err
	}
	change := unix.Kevent_t{Ident: uint64(pid), Filter: unix.EVFILT_PROC, Flags: unix.EV_ADD | unix.EV_ONESHOT, Fflags: unix.NOTE_EXIT}
	if _, err := unix.Kevent(kq, []unix.Kevent_t{change}, nil, nil); err != nil {
		unix.Close(kq)
		if errors.Is(err, unix.ESRCH) {
			return nil, nil
		}
		return nil, fmt.Errorf("register the exit event: %w", err)
	}
	return &exitEvent{kq: kq}, nil
}

// await waits for the exit, until ctx ends.
//
// THE WAIT ENDS WITH CTX, NOT AT A DEADLINE READ OFF IT. A context bounded on
// the granted clock (grantedclock.WithBound) has no wall deadline to hand the
// kernel, so the queue also carries a user event that ctx's end triggers
// (awakenOnEnd), and either event ends the wait.
func (e *exitEvent) await(ctx context.Context) error {
	ended := unix.Kevent_t{Ident: exitQueueEndedIdent, Filter: unix.EVFILT_USER, Flags: unix.EV_ADD | unix.EV_CLEAR}
	if _, err := unix.Kevent(e.kq, []unix.Kevent_t{ended}, nil, nil); err != nil {
		return fmt.Errorf("register the context's end on the exit queue: %w", err)
	}
	release := awakenOnEnd(ctx, func() {
		trigger := unix.Kevent_t{Ident: exitQueueEndedIdent, Filter: unix.EVFILT_USER, Fflags: unix.NOTE_TRIGGER}
		_, _ = unix.Kevent(e.kq, []unix.Kevent_t{trigger}, nil, nil)
	})
	defer release()
	events := make([]unix.Kevent_t, 1)
	for {
		_, err := unix.Kevent(e.kq, nil, events, nil)
		if errors.Is(err, unix.EINTR) {
			continue
		}
		if err != nil {
			return fmt.Errorf("wait for the exit event: %w", err)
		}
		if events[0].Filter == unix.EVFILT_USER {
			return ctx.Err()
		}
		return nil
	}
}

// exitQueueEndedIdent names the user event await's context end triggers. It
// shares the queue with the process filter, whose idents are pids, and a
// user event's ident is its own namespace.
const exitQueueEndedIdent = 0

// close releases the registration.
func (e *exitEvent) close() {
	unix.Close(e.kq)
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
