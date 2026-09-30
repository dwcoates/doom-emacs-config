package rollout

import (
	"context"
	"errors"
	"fmt"
	"slices"
	"time"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
)

// A BUILD WHOSE STATE LAYOUT DIFFERS IS ROLLED OUT STOP-THEN-START, NEVER
// HANDED OVER.
//
// A blue-green successor opens the state READ-ONLY while the incumbent is
// still its sole writer, and a read-only handle cannot carry an older layout
// forward: on 2026-09-27 master landed layout 11, the deploy handed over, and
// the successor refused the layout-10 database and exited 3ms after binding.
// No arbitration lets the successor migrate while the incumbent writes -- the
// incumbent may keep serving a busy workspace for an hour -- so a layout
// change is rolled out the one way that makes the migration a SOLE WRITER's:
// the incumbent stands every workspace's serving down the moment no prompt is
// mid-delivery (exactly as a transfer does: the shims detached and left
// running mid-turn, the queue's memory sealed into each carry), spawns
// the fresh binary as an ordinary daemon that waits on the boot claim, and
// exits. The kernel releases the claim only when this process ends, so the
// replacement opens -- and migrates -- the state only once nothing else can
// write it; its boot then adopts the shims, restores the held prompts, takes
// each carry up (TakeUpRestartCarries) and releases nothing it does not own
// (this process's close released its holds).

// The replacement's command line and its claim wait.
const (
	// LayoutVersionFlagName asks a binary for the state layout it writes
	// (`claude-repld -layout-version`), and starts nothing.
	LayoutVersionFlagName = "layout-version"
	// ReplacingFlagName marks the daemon an incumbent restarting across a
	// layout change spawns: it waits ReplacementClaimWait for the claim.
	ReplacingFlagName = "replacing"
	// ReplacementClaimWait is how long a replacement waits for the boot claim.
	// The incumbent spawns it immediately before its orderly exit, whose
	// longest joins are bounded in seconds (the loop join, the queue drain,
	// the merge drain, the stand-down of what could not be transferred), so a
	// minute is headroom over a bounded exit, never a guess at a busy one:
	// nothing the incumbent waits on for freeness runs after the spawn.
	ReplacementClaimWait = time.Minute
)

// replacementArgv is the incumbent's CONFIGURATION argv with the replacing
// flag appended, for the reason successorArgv inherits it: the replacement is
// this daemon's configuration, not a curated copy of it, and never the flags
// that said how this process booted.
func replacementArgv(config []string) []string {
	return append(slices.Clone(config), "--"+ReplacingFlagName)
}

// Restart implements Controller.
func (c *controller) Restart(ctx context.Context, force bool) (HandoverAcceptance, error) {
	fields := dlog.Context{"forced": force, "restart": true}
	if c.Joining() {
		c.log.Info(opRollOut, "refused a restart asked of a successor that is still joining", fields)
		return HandoverAcceptance{}, ErrJoining
	}
	plan, err := c.beginRestart(context.WithoutCancel(ctx), force)
	if err != nil {
		return HandoverAcceptance{}, err
	}
	accepted := HandoverAcceptance{
		Workspaces: len(plan.workspaces),
		Busy:       c.busy(workspaceIDs(plan)),
		Forced:     force,
	}
	c.log.Info(opRollOut, "accepted a restart across a state layout change: each workspace's serving stands down through the bounce registry, then a replacement boots on the fresh build",
		merge(fields, dlog.Context{"workspaces": accepted.Workspaces, "busy": accepted.Busy}))
	c.completeHandover(c.lifetime(ctx), plan)
	return accepted, nil
}

// beginRestart is the restart's bounded half: claim the one rollout slot, list
// what is served, announce a PLAIN BOUNCE (no successor address: nothing is
// listening until this daemon has gone), and write the intent manifest the
// replacement's boot accounts its adopted shims against. It spawns nothing:
// the replacement is spawned at the very end, when every workspace stood
// down.
func (c *controller) beginRestart(ctx context.Context, force bool) (*handoverPlan, error) {
	slot, err := c.claimHandover()
	if err != nil {
		c.log.Info(opHandover, "refused a restart while a rollout is already in flight",
			withCause(dlog.Context{"self_address": c.deps.SelfAddress}, err))
		return nil, err
	}
	fields := dlog.Context{"self_address": c.deps.SelfAddress, "forced": force, "restart": true}
	if err := c.clearStaleManifest(fields); err != nil {
		c.abandonHandover(ctx, slot, fields, err)
		return nil, err
	}
	workspaces, untransferable, err := c.served(ctx)
	if err != nil {
		c.log.Error(opHandover, "could not list what this daemon serves; the restart is abandoned before anything was announced",
			withCause(fields, err))
		c.abandonHandover(ctx, slot, fields, err)
		return nil, err
	}
	snapshot := make(map[ids.WorkspaceID]Participants, len(workspaces))
	manifest := c.manifest(ctx, "", workspaces, snapshot)
	manifest.Forced = force
	manifest.Deploy = true
	if err := c.writeManifest(ctx, manifest); err != nil {
		c.log.Error(opHandover, "the intent manifest could not be written; the restart is abandoned before anything was announced",
			withCause(merge(fields, dlog.Context{"workspaces": len(workspaces)}), err))
		c.abandonHandover(ctx, slot, fields, err)
		return nil, err
	}
	c.deps.Announcer.ShutdownAnnounced(&agentreplv1.DaemonShutdownAnnounced{
		Cause: &agentreplv1.DaemonShutdownCause{
			Kind: &agentreplv1.DaemonShutdownCause_SelfMergeRollout{
				SelfMergeRollout: &agentreplv1.DaemonShutdownSelfMergeRollout{},
			},
		},
		ExpectedOutageMs: int64(c.deps.ExpectedOutage / time.Millisecond),
		MintedAtMs:       milliseconds(c.deps.Clock.Now()),
	})
	c.log.Info(opHandover, "announced a plain-bounce restart: no successor is listening until this daemon has gone",
		merge(fields, dlog.Context{"workspaces": len(workspaces)}))
	return &handoverPlan{
		workspaces:     workspaces,
		untransferable: untransferable,
		snapshot:       snapshot,
		forced:         force,
		fields:         fields,
		restart:        true,
		slot:           slot,
		moved:          map[ids.WorkspaceID]movedWorkspace{},
	}, nil
}

// movedWorkspace is what a restart's stand-down of one workspace took: the
// hold, whether its shim was detached, and the move it sealed and carried.
type movedWorkspace struct {
	lease    ids.LeaseID
	detached bool
	move     *sealedMove
}

// finishRestart is the restart's end once every stand-down has run: spawn the
// replacement and exit -- or, when anything failed, take EVERY workspace
// back and stay. A restart that cannot finish must not exit into nothing,
// and must not leave a workspace stood down with its hold standing.
func (c *controller) finishRestart(ctx context.Context, plan *handoverPlan, failed int) {
	fields := plan.fields
	if failed > 0 {
		c.abandonRestart(ctx, plan, fmt.Errorf("rollout: restart: %d workspace(s) could not stand down", failed))
		return
	}
	c.standDownTheUntransferred(ctx, plan.untransferable)
	pid, err := c.deps.Spawner.SpawnReplacement(ctx)
	if err != nil {
		c.abandonRestart(ctx, plan, fmt.Errorf("rollout: restart: spawn the replacement: %w", err))
		return
	}
	// THE CARRIED REPLACEMENTS ARE THE REPLACEMENT'S NOW: it runs them once
	// its boot has taken each carry up, so their requesters here are told they
	// were handed across rather than left waiting on a daemon that exits.
	plan.mu.Lock()
	for _, m := range plan.moved {
		handedAcross(m.move)
	}
	plan.mu.Unlock()
	c.log.Info(opHandover, "every workspace stood down and the replacement is waiting on the boot claim; exiting",
		merge(fields, dlog.Context{"replacement_pid": pid}))
	if err := c.deps.Exit(ctx); err != nil {
		c.log.Error(opHandover, "the orderly exit could not be started", withCause(fields, err))
	}
}

// abandonRestart takes back every workspace the restart stood down and frees
// the rollout slot: no process was spawned that could still be joining, so
// the next deploy may begin at once.
func (c *controller) abandonRestart(ctx context.Context, plan *handoverPlan, cause error) {
	fields := plan.fields
	plan.mu.Lock()
	moved := make(map[ids.WorkspaceID]movedWorkspace, len(plan.moved))
	for ws, m := range plan.moved {
		moved[ws] = m
	}
	plan.mu.Unlock()
	var failures []error
	for ws, m := range moved {
		wsFields := merge(fields, dlog.Context{"workspace": string(ws), "lease": string(m.lease)})
		if _, err := c.reclaim(ctx, ws, m.lease, m.detached, true, m.move, wsFields); err != nil {
			failures = append(failures, err)
		}
	}
	c.log.Error(opHandover, "the restart cannot finish; every workspace it stood down was taken back and this daemon keeps serving",
		withCause(merge(fields, dlog.Context{"taken_back": len(moved)}), errors.Join(append([]error{cause}, failures...)...)))
	c.clearDeployLine("the restart was abandoned", fields)
	c.abandonHandover(ctx, plan.slot, fields, cause)
}
