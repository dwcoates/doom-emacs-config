package kernelevent

import (
	"context"
	"errors"
	"fmt"

	"golang.org/x/sys/unix"
)

// Spawns reports processes starting: a fork by a process it follows for
// forks, and an exec by one it follows for its next exec. It is a kqueue
// holding EVFILT_PROC registrations, and it names no pid: an event says only
// that something started, so its caller reads the process table again.
//
// LAUNCHD'S FORKS ARE REPORTED TO AN UNPRIVILEGED FOLLOWER. Registering
// NOTE_FORK on pid 1 needs no privilege and fires for each job launchd
// spawns (measured 2026-11-12, `launchctl submit` of one job: two events).
// A launchd-born service runs as xpcproxy until it execs its own binary, so
// a caller following launchd follows each new child's exec as well.
type Spawns struct {
	kq int
}

// NewSpawns answers a Spawns following nothing.
func NewSpawns() (*Spawns, error) {
	kq, err := newQueue()
	if err != nil {
		return nil, err
	}
	return &Spawns{kq: kq}, nil
}

// FollowForks reports each fork pid makes from now on. It answers false
// (and no error) for a process the kernel no longer finds.
func (s *Spawns) FollowForks(pid int) (bool, error) {
	return s.register(pid, unix.NOTE_FORK, 0)
}

// FollowExec reports pid's next exec, once. It answers false (and no error)
// for a process the kernel no longer finds.
func (s *Spawns) FollowExec(pid int) (bool, error) {
	return s.register(pid, unix.NOTE_EXEC, unix.EV_ONESHOT)
}

func (s *Spawns) register(pid int, fflags uint32, flags uint16) (bool, error) {
	change := unix.Kevent_t{Ident: uint64(pid), Filter: unix.EVFILT_PROC, Flags: unix.EV_ADD | unix.EV_CLEAR | flags, Fflags: fflags}
	if _, err := unix.Kevent(s.kq, []unix.Kevent_t{change}, nil, nil); err != nil {
		if errors.Is(err, unix.ESRCH) {
			return false, nil
		}
		return false, fmt.Errorf("register pid %d's process events: %w", pid, err)
	}
	return true, nil
}

// Wait blocks until a start has been reported since the last Wait, or ctx
// ends, then takes every report already pending.
func (s *Spawns) Wait(ctx context.Context) error {
	return awaitAndTakePending(ctx, s.kq)
}

// Close releases every registration.
func (s *Spawns) Close() {
	unix.Close(s.kq)
}
