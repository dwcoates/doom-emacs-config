package rollout

import (
	"context"
	"errors"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
)

const opRollOut = "daemon.rollout.roll_out"

// ErrJoining refuses a handover asked of a successor that is still joining: it
// serves nothing yet, so it has nothing to hand over.
var ErrJoining = errors.New("rollout: this daemon is a successor still joining; it serves nothing to roll out")

// HandOver implements Controller.
//
// THE BOUNDED HALF RUNS ON THE CALLER'S CALL, the unbounded half on the
// daemon's lifetime. The spawn, the announcement and the manifest are bounded
// by what they do; the transfers each wait on their own workspace's freeness
// for as long as that takes (unless forced), which is why the answer is an
// ACCEPTANCE and outlives the rpc that asked for it.
//
// AN UNFORCED HANDOVER ENDS NO TURN: every transfer is decided by the prompt
// queue's bounce registry, at the workspace's own freeness. A FORCED one
// transfers every workspace at once; the running shims are detached, never
// killed, and the successor adopts them with their turns still in flight.
func (c *controller) HandOver(ctx context.Context, force bool) (HandoverAcceptance, error) {
	fields := dlog.Context{"forced": force}
	if c.Joining() {
		c.log.Info(opRollOut, "refused a handover asked of a successor that is still joining", fields)
		return HandoverAcceptance{}, ErrJoining
	}
	// NOTHING IS PLANNED WHILE THIS DAEMON'S OWN TAKEOVER IS STILL BRINGING
	// WORKSPACES UP: the plan lists what this daemon serves, and a bring-up
	// still in flight would claim a workspace after the plan had moved it.
	if err := c.awaitTakeoverSettled(ctx, fields); err != nil {
		return HandoverAcceptance{}, err
	}
	// THE BOUNDED HALF SURVIVES THE CALLER. A caller that gives up between the
	// spawn and the manifest must not leave a successor standing with nothing
	// announced.
	plan, err := c.beginHandover(context.WithoutCancel(ctx), force)
	if err != nil {
		return HandoverAcceptance{}, err
	}
	accepted := HandoverAcceptance{
		Workspaces: len(plan.workspaces),
		Busy:       c.busy(workspaceIDs(plan)),
		Forced:     force,
	}
	c.log.Info(opRollOut, "accepted a handover: each workspace transfers through the bounce registry",
		merge(fields, dlog.Context{"successor": plan.successor, "workspaces": accepted.Workspaces, "busy": accepted.Busy}))
	c.completeHandover(c.lifetime(ctx), plan)
	return accepted, nil
}

// Joining implements Controller.
func (c *controller) Joining() bool {
	c.mu.Lock()
	defer c.mu.Unlock()
	return c.stillJoiningLocked()
}

// stillJoiningLocked reports whether this daemon is a successor that has NOT
// yet taken everything it was handed.
//
// `joiningMode` ALONE IS NOT THE ANSWER, and reading it as one refused every
// rollout a successor was ever asked for: it records how this process BOOTED,
// and stays true for its whole life. A successor stops joining when it has
// read the incumbent's manifest and owns every workspace that manifest named —
// the same condition under which it advertises daemon.addr and becomes the
// daemon every client dials. From then on it is simply the daemon, and the
// next deploy's handover is its to take. The caller holds c.mu.
func (c *controller) stillJoiningLocked() bool {
	if !c.joiningMode {
		return false
	}
	if !c.manifestSeen {
		return true
	}
	for ws := range c.joining {
		if !c.owned[ws] {
			return true
		}
	}
	return false
}

// lifetime is the context the unbounded half of a rollout runs on: the
// daemon's serving lifetime, or — where none is wired — the caller's context
// with its cancellation removed, so the work is bounded by the process alone.
// Never the rpc's own context: that is cancelled the moment the acceptance is
// answered, which is before the first workspace has been waited on.
func (c *controller) lifetime(ctx context.Context) context.Context {
	if c.deps.Lifetime != nil {
		return c.deps.Lifetime
	}
	return context.WithoutCancel(ctx)
}

// busy counts the workspaces that are not free right now.
func (c *controller) busy(workspaces []ids.WorkspaceID) int {
	n := 0
	for _, ws := range workspaces {
		if c.deps.Freeness != nil && !c.deps.Freeness.Free(ws) {
			n++
		}
	}
	return n
}

func workspaceIDs(plan *handoverPlan) []ids.WorkspaceID {
	out := make([]ids.WorkspaceID, 0, len(plan.workspaces))
	for _, ws := range plan.workspaces {
		out = append(out, ws.ID)
	}
	return out
}
