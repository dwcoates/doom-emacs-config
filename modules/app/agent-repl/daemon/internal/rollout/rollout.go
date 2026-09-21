package rollout

import (
	"context"
	"errors"
	"fmt"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
)

const opRollOut = "daemon.rollout.roll_out"

// Rebuilt is what the deploy chain says it rebuilt. The CALLER states it: a
// deploy has no landed range to classify — the chain fingerprints artifacts,
// so it sees a dirty tree and a forced rebuild that no commit range describes.
type Rebuilt struct {
	Daemon bool
	Shim   bool
	Webapp bool
}

// subsystems answers the rebuilt set in the classifier's own vocabulary, so
// the action below is chosen by the SAME rule a self-merge's is.
func (r Rebuilt) subsystems() []Subsystem {
	var out []Subsystem
	if r.Daemon {
		out = append(out, SubsystemDaemon)
	}
	if r.Shim {
		out = append(out, SubsystemShim)
	}
	if r.Webapp {
		out = append(out, SubsystemWebapp)
	}
	return out
}

// Action is which rollout a request was answered with.
type Action string

const (
	ActionHandover     Action = "handover"
	ActionShimRelaunch Action = "shim_relaunch"
	ActionWebappReload Action = "webapp_reload"
)

// Acceptance is a rollout that was ACCEPTED and is under way. It is not a
// completion: every rollout waits on freeness for as long as freeness takes.
type Acceptance struct {
	Action Action
	// Workspaces is how many workspaces the action covers; for a webapp reload
	// it is the open webviews that were told to reload.
	Workspaces int
	// Busy is how many of them were NOT free at acceptance — what the rollout
	// is waiting on. Always zero for a webapp reload, which waits on nothing.
	Busy int
}

// ErrJoining refuses a rollout asked of a successor that is still joining: it
// serves nothing yet, so it has nothing to roll out.
var ErrJoining = errors.New("rollout: this daemon is a successor still joining; it serves nothing to roll out")

// ErrNothingRebuilt refuses a rollout that names no rebuilt subsystem.
var ErrNothingRebuilt = errors.New("rollout: the request names nothing that was rebuilt")

// RollOut puts a build the deploy chain ALREADY PRODUCED into service.
//
// IT IS Trigger WITHOUT THE TWO THINGS ONLY A MERGE HAS: there is no landed
// range to classify (the caller states what moved), and no deploy chain to run
// (the caller IS the deploy chain, and running it again from here would be the
// script invoking itself). What is left is the per-subsystem action, chosen by
// the same precedence — daemon, else shim, else webapp — and taken the same
// way, so a deploy and a self-merge cannot roll out differently.
//
// NOTHING HERE ENDS A TURN. A handover transfers each workspace at its own
// freeness and a relaunch swaps each shim at its own freeness; both wait
// forever rather than interrupt. That is why the answer is an ACCEPTANCE: the
// bounded half runs on the caller's call, and the unbounded half runs on the
// daemon's lifetime, outliving the rpc that started it.
func (c *controller) RollOut(ctx context.Context, rebuilt Rebuilt) (Acceptance, error) {
	subsystems := rebuilt.subsystems()
	fields := dlog.Context{"rebuilt": names(subsystems)}
	if len(subsystems) == 0 {
		c.log.Error(opRollOut, "refused a rollout that names nothing rebuilt", fields)
		return Acceptance{}, ErrNothingRebuilt
	}
	c.mu.Lock()
	joining := c.stillJoiningLocked()
	c.mu.Unlock()
	if joining {
		c.log.Info(opRollOut, "refused a rollout asked of a successor that is still joining", fields)
		return Acceptance{}, ErrJoining
	}

	switch {
	case rebuilt.Daemon:
		return c.rollOutHandover(ctx, fields)
	case rebuilt.Shim:
		return c.rollOutRelaunch(ctx, fields)
	default:
		return c.rollOutReload(ctx, fields)
	}
}

// rollOutHandover announces a handover on the caller's call and completes it
// on the daemon's lifetime.
func (c *controller) rollOutHandover(ctx context.Context, fields dlog.Context) (Acceptance, error) {
	// THE BOUNDED HALF SURVIVES THE CALLER. A deploy script that gives up
	// between the spawn and the manifest must not leave a successor standing
	// with nothing announced, so the announcement runs detached from the rpc's
	// cancellation and is bounded by what it does, not by who asked.
	plan, err := c.beginHandover(context.WithoutCancel(ctx))
	if err != nil {
		return Acceptance{}, err
	}
	busy := c.busy(workspaceIDs(plan))
	accepted := Acceptance{Action: ActionHandover, Workspaces: len(plan.workspaces), Busy: busy}
	c.log.Info(opRollOut, "accepted a rollout: handing over at each workspace's freeness",
		merge(fields, dlog.Context{"successor": plan.successor, "workspaces": accepted.Workspaces, "busy": busy}))
	lifetime := c.lifetime(ctx)
	go func() {
		if err := c.completeHandover(lifetime, plan); err != nil {
			c.log.Error(opRollOut, "the accepted handover did not complete",
				withCause(merge(fields, dlog.Context{"successor": plan.successor}), err))
		}
	}()
	return accepted, nil
}

// rollOutRelaunch bounces every live shim, each at its own freeness.
func (c *controller) rollOutRelaunch(ctx context.Context, fields dlog.Context) (Acceptance, error) {
	workspaces, err := c.deps.DB.ListWorkspaces(ctx)
	if err != nil {
		c.log.Error(opRollOut, "could not list the workspaces to relaunch", withCause(fields, err))
		return Acceptance{}, fmt.Errorf("rollout: roll out: %w", err)
	}
	var live []ids.WorkspaceID
	for _, ws := range workspaces {
		if _, ok := c.deps.Shims.Client(ws.ID); ok {
			live = append(live, ws.ID)
		}
	}
	accepted := Acceptance{Action: ActionShimRelaunch, Workspaces: len(live), Busy: c.busy(live)}
	c.log.Info(opRollOut, "accepted a rollout: relaunching each live shim at its workspace's freeness",
		merge(fields, dlog.Context{"workspaces": accepted.Workspaces, "busy": accepted.Busy}))
	lifetime := c.lifetime(ctx)
	go func() {
		if err := c.relaunchFleet(lifetime, fields); err != nil {
			c.log.Error(opRollOut, "the accepted shim relaunch did not complete", withCause(fields, err))
		}
	}()
	return accepted, nil
}

// rollOutReload tells every open webview to reload. It waits on nothing, so it
// is done by the time it answers.
func (c *controller) rollOutReload(ctx context.Context, fields dlog.Context) (Acceptance, error) {
	workspaces, err := c.deps.DB.ListWorkspaces(ctx)
	if err != nil {
		c.log.Error(opRollOut, "could not list the workspaces to reload", withCause(fields, err))
		return Acceptance{}, fmt.Errorf("rollout: roll out: %w", err)
	}
	webviews := 0
	for _, ws := range workspaces {
		if c.deps.Participants.Participants(ws.ID).Web {
			webviews++
		}
	}
	if err := c.reloadFleet(ctx, fields); err != nil {
		return Acceptance{}, err
	}
	c.log.Info(opRollOut, "accepted a rollout: every open webview was told to reload",
		merge(fields, dlog.Context{"webviews": webviews}))
	return Acceptance{Action: ActionWebappReload, Workspaces: webviews}, nil
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
// next deploy's rollout is its to take. The caller holds c.mu.
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
		if !c.deps.Freeness.Free(ws) {
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
