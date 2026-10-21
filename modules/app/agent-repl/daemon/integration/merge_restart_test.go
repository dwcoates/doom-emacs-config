//go:build integration

package integration

import (
	"strings"
	"testing"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/integration/harness"
)

// TestARestartOverAMergeWaitingForItsWorkspaceRestartsAndTheMergeLands
// reproduces 2026-10-08 18:55:03, workspace `doom`: the owner restarted a
// workspace whose merge was waiting for it to fall free, the relaunch could
// not take its hold over the merge's lease, and the restart did nothing. A
// merge only waiting must not block a restart (owner ruling, 2026-10-08): the
// restart answers success, the merge waits on the fresh session -- which is
// free, the stale live work gone with the old shim -- and lands.
func TestARestartOverAMergeWaitingForItsWorkspaceRestartsAndTheMergeLands(t *testing.T) {
	t.Parallel()
	// Arrange: a merge waiting on a live background subagent.
	f, d, _, script := mergeCleanRepo(t)
	script.SetExitCode(0)
	script.SetStdout("daemon: passed in 1s\n")
	d.ExpectWorkspaceRestartWarnings()
	workspaceLog := harness.WorkspaceLogPath(f.ws.GetDir(), "daemon")
	tail := f.watchRootFeed()
	f.submit("keep going", "k-restart-over-merge", origin)
	f.shim.ExpectStartTurn()
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, ftSubagentSpawn("toolu-1", "toolu-1", "sweep the tree")))
	f.shim.PushAgentFrame(mainAgent, detachedWorkFrame(mainAgent, movedSubagent("toolu-1")))
	d.AwaitLogRecord(workspaceLog, "the live subagent", func(r harness.LogRecord) bool {
		agents, ok := r.Context["agents"].(float64)
		return r.Operation == "daemon.sessionwatcher.live_work" && ok && agents == 1
	})
	harness.CommitWork(t, f.ws.GetDir())
	if _, err := d.Client().FinishWorkspace(d.Ctx(), connect.NewRequest(harness.FinishRequest(f.ws))); err != nil {
		t.Fatalf("MergeWorkspace = error %v, want the merge enqueued", err)
	}
	f.shim.ExpectKillTurn()
	d.AwaitLogRecord(workspaceLog, "the merge waiting for the workspace to fall free", func(r harness.LogRecord) bool {
		return r.Operation == "daemon.merge.await_free" && strings.HasPrefix(r.Message, "the merge waits")
	})

	// Act.
	resp, err := d.Client().RestartWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.RestartWorkspaceRequest{Workspace: f.ws}))

	// Assert: the restart happened, and the merge then started and landed.
	if err != nil || resp.Msg.GetSuccess() == nil {
		t.Fatalf("RestartWorkspace = (%v, %v), want a success", resp, err)
	}
	awaitRow(t, f, tail, "the merge bubble's landed head", func(r *frontendv1.FeedRow) bool {
		return isMergeHead(r) && r.GetActivity().GetMerge().GetSuccess() != nil
	})
}
