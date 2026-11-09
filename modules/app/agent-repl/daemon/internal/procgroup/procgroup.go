// Package procgroup reads and signals process groups the way a process that
// is not their members' parent can: through the kernel's process table.
//
// ON DARWIN, killpg ANSWERS EPERM FOR A GROUP WITH NO LIVE MEMBER LEFT: it
// skips the group's zombies (an exited leader nobody has reaped yet among
// them) and reports EPERM when nothing was left to signal. So a group kill
// that meets EPERM lists the group's LIVE members and signals each by pid;
// with none left, the group is gone. The daemon integration harness's group
// kill and the daemon's stop of an adopted shim both go through Signal.
package procgroup

import (
	"context"
	"errors"
	"fmt"
	"syscall"

	"agent-repld/internal/kernelevent"
)

// State is the kernel's own account of whether a process can still run an
// instruction.
type State struct {
	// Frozen is true when the process is stopped, a zombie, or gone: in none of
	// those can it observe anything or write a record.
	Frozen bool
	// Exited is true when the process is a zombie or gone: it has ended, and
	// only its reaping (or nothing at all) is left. A stopped process is
	// frozen but has NOT exited.
	Exited bool
	// Name is the kernel's state, for a report.
	Name string
}

// Member is one process of a process group, as the kernel lists it.
type Member struct {
	Pid   int
	Comm  string
	State string
}

// Signal sends sig to process group pgid, answering nil once every live
// member was signalled or was already gone (SignalMembers).
func Signal(pgid int, sig syscall.Signal) error {
	return SignalMembers(pgid, sig, syscall.Kill, LiveMembers)
}

// Gone reports whether process group pgid holds no live process: the kernel
// has no such group, or every member left has exited.
func Gone(pgid int) (bool, error) {
	err := syscall.Kill(-pgid, 0)
	if errors.Is(err, syscall.ESRCH) {
		return true, nil
	}
	if err != nil && !errors.Is(err, syscall.EPERM) {
		return false, fmt.Errorf("probe process group %d: %w", pgid, err)
	}
	live, err := LiveMembers(pgid)
	if err != nil {
		return false, err
	}
	return len(live) == 0, nil
}

// AwaitGone waits until process group pgid holds no live process (Gone), or
// ctx ends. It waits on the kernel's own exit event for each live member,
// never on a timer: the daemon is not the members' parent and has no wait(2)
// to block in, but a process exit is reported to any process that registers
// for it (kernelevent). It works in rounds, because a member may fork another
// before it exits: each round lists the live members and waits for every
// one, and the group is gone once a round finds none.
func AwaitGone(ctx context.Context, pgid int) error {
	return awaitGone(ctx, pgid, Gone, LiveMembers, kernelevent.WaitExit)
}

// awaitGone is AwaitGone over its kernel readings, so a test can stand in for
// them.
func awaitGone(ctx context.Context, pgid int, gone func(int) (bool, error), live func(int) ([]Member, error),
	exit func(context.Context, int) error) error {
	for {
		done, err := gone(pgid)
		if err != nil {
			return err
		}
		if done {
			return nil
		}
		members, err := live(pgid)
		if err != nil {
			return err
		}
		for _, m := range members {
			if err := exit(ctx, m.Pid); err != nil {
				return fmt.Errorf("await member %d of process group %d: %w", m.Pid, pgid, err)
			}
		}
	}
}

// SignalMembers sends sig to process group pgid through kill, and on
// EPERM to each member live lists, by pid. It answers nil once every live
// member was signalled or was already gone.
func SignalMembers(pgid int, sig syscall.Signal, kill func(int, syscall.Signal) error,
	live func(int) ([]Member, error)) error {
	err := kill(-pgid, sig)
	if err == nil || errors.Is(err, syscall.ESRCH) {
		return nil
	}
	if !errors.Is(err, syscall.EPERM) {
		return fmt.Errorf("%v process group %d: %w", sig, pgid, err)
	}
	members, listErr := live(pgid)
	if listErr != nil {
		return fmt.Errorf("%v process group %d: %w, and its members could not be read: %v", sig, pgid, err, listErr)
	}
	var refused []error
	for _, m := range members {
		if kerr := kill(m.Pid, sig); kerr != nil && !errors.Is(kerr, syscall.ESRCH) {
			refused = append(refused, fmt.Errorf("member %d (%s, %s): %w", m.Pid, m.Comm, m.State, kerr))
		}
	}
	if len(refused) > 0 {
		return fmt.Errorf("%v process group %d: %w, and signalling its live members one by one failed: %w",
			sig, pgid, err, errors.Join(refused...))
	}
	return nil
}
