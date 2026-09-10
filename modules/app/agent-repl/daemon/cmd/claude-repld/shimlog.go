package main

import (
	"context"
	"fmt"
	"sync"

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
	RelaunchShim(context.Context, ids.WorkspaceID, rollout.RelaunchReason) error
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
	req.Log.Info(shimLogRollOperation, "forcing the workspace's shim to roll at freeness", fields)
	if err := relauncher.RelaunchShim(ctx, workspace.ID, rollout.ReasonShimLogCeiling); err != nil {
		fields["cause"] = err.Error()
		req.Log.Error(shimLogRollOperation, "could not roll the shim whose log reached its hard ceiling", fields)
		return
	}
	req.Log.Info(shimLogRollOperation, "rolled the shim whose log reached its hard ceiling", fields)
}
