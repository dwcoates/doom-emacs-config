package main

import (
	"context"
	"fmt"
	"sync"

	"claude-repld/internal/bounce"
	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/rollout"
	"claude-repld/internal/wsm"
)

const shimLogRollOperation = "daemon.cmd.shim_log_roll"

type shimLogWorkspaceStore interface {
	WorkspaceByDir(context.Context, string) (wsm.Workspace, error)
}

type shimLogRelauncher interface {
	BounceShim(ctx context.Context, ws ids.WorkspaceID, reason rollout.RelaunchReason, force bool, done func(error)) (bounce.Decision, error)
}

// runShimLogRolls consumes hard-ceiling requests for the serving lifetime.
// Each workspace gets its own worker: one active turn may hold its roll at
// freeness indefinitely without preventing another workspace from rolling at
// its own boundary.
func runShimLogRolls(
	ctx context.Context,
	requests <-chan dlog.ShimRollRequest,
	db shimLogWorkspaceStore,
	relauncher shimLogRelauncher,
) error {
	var workers sync.WaitGroup
	defer workers.Wait()
	for {
		select {
		case <-ctx.Done():
			return nil
		case req, ok := <-requests:
			if !ok {
				return fmt.Errorf("the shim-log roll request channel closed before the serving lifetime ended")
			}
			workers.Add(1)
			go func(req dlog.ShimRollRequest) {
				defer workers.Done()
				forceShimLogRoll(ctx, req, db, relauncher)
			}(req)
		}
	}
}

func forceShimLogRoll(
	ctx context.Context,
	req dlog.ShimRollRequest,
	db shimLogWorkspaceStore,
	relauncher shimLogRelauncher,
) {
	fields := dlog.Context{
		"workspace_dir": req.Dir,
		"log_id":        req.LogID,
		"size_bytes":    req.SizeBytes,
		"hard_bytes":    req.HardBytes,
	}
	req.Log.Debug(shimLogRollOperation, "received a shim-log hard-ceiling roll request", fields)
	workspace, err := db.WorkspaceByDir(ctx, req.Dir)
	if err != nil {
		fields["cause"] = err.Error()
		req.Log.Error(shimLogRollOperation, "could not resolve the workspace whose shim log reached its hard ceiling", fields)
		return
	}
	fields["workspace"] = string(workspace.ID)
	req.Log.Info(shimLogRollOperation, "asking the bounce registry to roll the workspace's shim at freeness", fields)
	done := func(err error) {
		ended := copyFields(fields)
		switch bounce.OutcomeOf(err) {
		case bounce.OutcomeUnregistered:
			// The shim departed before the roll was taken, and nothing is left
			// to roll: the next shim spawned for the workspace starts its log
			// afresh.
			req.Log.Info(shimLogRollOperation, "the shim whose log reached its hard ceiling departed before the roll; nothing is left to roll", ended)
		case bounce.OutcomeHandedAcross:
			// A handover carried the roll to the daemon the workspace moved
			// to, which runs it after its adoption.
			req.Log.Info(shimLogRollOperation, "the shim-log roll was handed across: the daemon the workspace moved to runs it after its adoption", ended)
		case bounce.OutcomeFailed:
			ended["cause"] = err.Error()
			req.Log.Error(shimLogRollOperation, "could not roll the shim whose log reached its hard ceiling", ended)
		case bounce.OutcomeFinished:
			req.Log.Info(shimLogRollOperation, "rolled the shim whose log reached its hard ceiling", ended)
		}
	}
	decision, err := relauncher.BounceShim(ctx, workspace.ID, rollout.ReasonShimLogCeiling, false, done)
	if err != nil {
		fields["cause"] = err.Error()
		req.Log.Error(shimLogRollOperation, "the bounce registry refused the shim-log roll", fields)
		return
	}
	fields["bounced_now"] = decision.Now
	fields["turn_in_flight"] = decision.TurnInFlight
	fields["detached_work"] = decision.DetachedWork
	fields["already_pending"] = decision.AlreadyPending
	req.Log.Info(shimLogRollOperation, "the bounce registry took the shim-log roll", fields)
}

// copyFields copies a record's context, so a callback that runs later never
// shares a map with the caller that built it.
func copyFields(fields dlog.Context) dlog.Context {
	out := make(dlog.Context, len(fields)+1)
	for k, v := range fields {
		out[k] = v
	}
	return out
}
