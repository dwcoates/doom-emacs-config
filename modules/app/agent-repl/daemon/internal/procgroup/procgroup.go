// Package procgroup is the daemon's reading and signalling of process
// groups. Every reading and signal is agentrepl/logging/childgroup's, shared
// with the test service; what stays here is AwaitGone, which waits on the
// daemon's kernel exit events (kernelevent).
package procgroup

import (
	"context"
	"fmt"

	"agent-repld/internal/kernelevent"

	"agentrepl/logging/childgroup"
)

// State is the kernel's own account of whether a process can still run an
// instruction (childgroup.State).
type State = childgroup.State

// Member is one process of a process group (childgroup.Member).
type Member = childgroup.Member

// The readings and signals, childgroup's.
var (
	Signal         = childgroup.Signal
	SignalMembers  = childgroup.SignalMembers
	Gone           = childgroup.Gone
	LiveMembers    = childgroup.LiveMembers
	UnendedMembers = childgroup.UnendedMembers
	ReadState      = childgroup.ReadState
	Exiting        = childgroup.Exiting
)

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
