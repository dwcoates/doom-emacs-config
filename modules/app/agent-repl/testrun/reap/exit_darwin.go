package reap

import (
	"errors"
	"fmt"
	"syscall"
	"time"
)

// AwaitExit implements Table through kqueue: each pid's NOTE_EXIT is the
// kernel's own word that the process has exited, waited on, never polled.
func (OSTable) AwaitExit(pids []int, bound time.Duration) ([]int, error) {
	kq, err := syscall.Kqueue()
	if err != nil {
		return nil, fmt.Errorf("kqueue: %w", err)
	}
	defer syscall.Close(kq)
	waiting := map[int]bool{}
	for _, pid := range pids {
		change := syscall.Kevent_t{Ident: uint64(pid), Filter: syscall.EVFILT_PROC, Flags: syscall.EV_ADD | syscall.EV_ONESHOT, Fflags: syscall.NOTE_EXIT}
		_, err := syscall.Kevent(kq, []syscall.Kevent_t{change}, nil, nil)
		switch {
		case errors.Is(err, syscall.ESRCH):
			// Already exited (or a zombie the kernel no longer watches).
		case err != nil:
			return nil, fmt.Errorf("watch pid %d's exit: %w", pid, err)
		default:
			waiting[pid] = true
		}
	}
	deadline := time.Now().Add(bound)
	events := make([]syscall.Kevent_t, len(pids)+1)
	for len(waiting) > 0 {
		left := time.Until(deadline)
		if left <= 0 {
			break
		}
		timeout := syscall.NsecToTimespec(left.Nanoseconds())
		n, err := syscall.Kevent(kq, nil, events, &timeout)
		if errors.Is(err, syscall.EINTR) {
			continue
		}
		if err != nil {
			return nil, fmt.Errorf("wait for the killed processes' exits: %w", err)
		}
		for _, ev := range events[:n] {
			delete(waiting, int(ev.Ident))
		}
	}
	var left []int
	for _, pid := range pids {
		if waiting[pid] {
			left = append(left, pid)
		}
	}
	return left, nil
}
