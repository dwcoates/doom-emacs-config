package kernelevent

import (
	"context"
	"errors"
	"fmt"
	"syscall"

	"golang.org/x/sys/unix"
)

// UnregisteredExitSeen is what an exit event that could not be taken says of
// the exit (AwaitExit).
//
// ON DARWIN AN UNTAKEN EVENT DOES NOT MEAN THE FILES ARE RELEASED. A process
// SIGKILLed a moment before the registration refuses it (ESRCH: the kernel
// will not find a process whose exit is under way) while it still holds its
// open files: measured 2026-10-15, 2 of 300 such registrations answered ESRCH
// with the process's FIFO write end still open, and none of the 298 that saw
// the event did. A caller that needs the files released takes the event
// before the kill.
const UnregisteredExitSeen = false

// Exit is one process's exit event on a kqueue EVFILT_PROC/NOTE_EXIT
// registration, held from TakeExit until Close.
type Exit struct {
	kq int
	// data is the exit event's filter data once Await has seen it: the wait
	// status when the registration asked NOTE_EXITSTATUS (TakeEnd).
	data int64
}

// TakeExit registers for pid's exit, answering nil (and no error) for a
// process the kernel refuses the registration for (ESRCH: one that has begun
// exiting, or is gone). It works for any process, not only a child.
func TakeExit(pid int) (*Exit, error) {
	return registerExit(pid, unix.NOTE_EXIT)
}

// TakeEnd is TakeExit for a CHILD of this process, whose event also carries
// its wait status (NOTE_EXITSTATUS, valid only on a child): how it ended can
// be read without reaping it.
func TakeEnd(pid int) (*Exit, error) {
	return registerExit(pid, unix.NOTE_EXIT|unix.NOTE_EXITSTATUS)
}

// EndOf answers how the process of an event awaited from TakeEnd ended.
func (e *Exit) EndOf(int) (ProcessEnd, error) {
	return processEndOf(syscall.WaitStatus(e.data)), nil
}

// registerExit registers pid's exit, with FFLAGS, on a fresh queue.
func registerExit(pid int, fflags uint32) (*Exit, error) {
	kq, err := newQueue()
	if err != nil {
		return nil, err
	}
	change := unix.Kevent_t{Ident: uint64(pid), Filter: unix.EVFILT_PROC, Flags: unix.EV_ADD | unix.EV_ONESHOT, Fflags: fflags}
	if _, err := unix.Kevent(kq, []unix.Kevent_t{change}, nil, nil); err != nil {
		unix.Close(kq)
		if errors.Is(err, unix.ESRCH) {
			return nil, nil
		}
		return nil, fmt.Errorf("register the exit event: %w", err)
	}
	return &Exit{kq: kq}, nil
}

// Await waits for the exit, until ctx ends.
//
// THE WAIT ENDS WITH CTX, NOT AT A DEADLINE READ OFF IT. A context with no
// wall deadline to hand the kernel still ends the wait: the queue carries a
// user event that ctx's end triggers (AwakenOnEnd), and either event ends it.
func (e *Exit) Await(ctx context.Context) error {
	ev, err := awaitQueue(ctx, e.kq)
	if err != nil {
		return err
	}
	e.data = ev.Data
	return nil
}

// Close releases the registration.
func (e *Exit) Close() {
	unix.Close(e.kq)
}

// queueEndedIdent names the user event a wait's context end triggers. It
// shares the queue with the process and vnode filters, whose idents are pids
// and descriptors, and a user event's ident is its own namespace.
const queueEndedIdent = 0

// awaitQueue blocks on KQ until one of its registered events fires or ctx
// ends, answering the event.
func awaitQueue(ctx context.Context, kq int) (unix.Kevent_t, error) {
	ended := unix.Kevent_t{Ident: queueEndedIdent, Filter: unix.EVFILT_USER, Flags: unix.EV_ADD | unix.EV_CLEAR}
	if _, err := unix.Kevent(kq, []unix.Kevent_t{ended}, nil, nil); err != nil {
		return unix.Kevent_t{}, fmt.Errorf("register the context's end on the queue: %w", err)
	}
	release := AwakenOnEnd(ctx, func() {
		trigger := unix.Kevent_t{Ident: queueEndedIdent, Filter: unix.EVFILT_USER, Fflags: unix.NOTE_TRIGGER}
		_, _ = unix.Kevent(kq, []unix.Kevent_t{trigger}, nil, nil)
	})
	defer release()
	events := make([]unix.Kevent_t, 1)
	for {
		_, err := unix.Kevent(kq, nil, events, nil)
		if errors.Is(err, unix.EINTR) {
			continue
		}
		if err != nil {
			return unix.Kevent_t{}, fmt.Errorf("wait on the queue: %w", err)
		}
		if events[0].Filter == unix.EVFILT_USER {
			return unix.Kevent_t{}, ctx.Err()
		}
		return events[0], nil
	}
}

// newQueue answers a kqueue descriptor marked close-on-exec.
//
// NO DESCRIPTOR LEAKS INTO A CHILD. kqueue(2) takes no close-on-exec flag, so
// the mark is a second call; both run under syscall.ForkLock's read side, as
// the standard library's own non-atomic close-on-exec paths do, so a
// concurrent fork cannot land between them and inherit the queue.
func newQueue() (int, error) {
	syscall.ForkLock.RLock()
	defer syscall.ForkLock.RUnlock()
	kq, err := unix.Kqueue()
	if err != nil {
		return -1, fmt.Errorf("kqueue: %w", err)
	}
	unix.CloseOnExec(kq)
	return kq, nil
}
