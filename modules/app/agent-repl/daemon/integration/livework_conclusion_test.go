//go:build integration

package integration

import (
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"claude-repld/integration/harness"

	"connectrpc.com/connect"
)

// TestAHungShellsConclusionFiresTheRegisteredBounce is the 2026-09-27 wedge
// (workspace 3e2d9cadc6794e13), end to end. A hand-backgrounded shell's
// WatchBash never got its first frame; the shim concluded the shell, but the
// daemon went on counting it live, so the bounce registered behind it waited on
// a freeness edge that never came and the workspace was never handed over.
//
// The daemon's live set now follows the shim's conclusion, not the watch: here
// the shim's re-announcement no longer names the shell, the daemon retires it
// (loudly: it had missed the conclusion), the workspace goes free, and the
// registered bounce runs.
func TestAHungShellsConclusionFiresTheRegisteredBounce(t *testing.T) {
	t.Parallel()

	// Arrange: a shell whose WatchBash the shim never answers.
	f := newOpened(t, harness.Opts{})
	// The sweep covers every test; the declared records are evidence of the
	// session stream the test severs (so the shim re-announces), the
	// conclusion the daemon missed and reconciles, and the graceful
	// stand-down the bounce drives (a KillSession the fake answers by exiting).
	f.d.ExpectWarnings("daemon.sessionwatcher.live_work_stale",
		"daemon.shimclient.redial", "daemon.sessionwatcher.reopen", "daemon.health.open_fault", "daemon.rollout.relaunch",
		"daemon.sessionwatcher.link_fault", "daemon.sessionwatcher.watch_agent",
		"daemon.sessionwatcher.watch_session", "daemon.shimclient.exit", "daemon.shimclient.kill_session")
	f.shim.ExpectStartSession()
	f.shim.SilenceBash("work-hung-1")
	pushDetachedShell(f.shim, "work-hung-1", "sleep 100")
	f.shim.ExpectWatchBash()

	// Arrange: a graceful bounce registers behind the live shell.
	resp, err := f.d.Client().RestartWorkspace(f.d.Ctx(), connect.NewRequest(&agentreplv1.RestartWorkspaceRequest{Workspace: f.ws, Force: false}))
	if err != nil || resp.Msg.GetSuccess() == nil {
		t.Fatalf("RestartWorkspace{force:false} = (%v, %v), want a success", resp, err)
	}
	daemonLog := harness.WorkspaceLogPath(f.repo.Dir, "daemon")
	f.d.AwaitLogRecord(daemonLog, "the bounce registered behind the live shell", func(r harness.LogRecord) bool {
		return r.Operation == "daemon.promptqueue.bounce" &&
			r.Message == "the workspace has work in flight; registered the bounce for when it ends" &&
			r.Context["detached_work"] == float64(1)
	})

	// Act: the shim concludes the shell -- its live membership no longer
	// names it -- and re-announces that membership on the daemon's next
	// session watch.
	f.shim.SetLiveWork()
	f.shim.DropStream(harness.StreamSession)

	// Assert: the daemon retires the shell it had missed the end of, and the
	// registered bounce runs: the resume reaches the relaunched shim.
	f.d.AwaitLogRecord(daemonLog, "the stale shell retired by reconciliation", func(r harness.LogRecord) bool {
		return r.Operation == "daemon.sessionwatcher.live_work_stale" && r.Context["work_id"] == "work-hung-1"
	})
	resume := f.d.ShimAt(prelaunchControlSocket(f.d, f.ws, 1)).ExpectStartSession()
	if resume.GetResume() == nil {
		t.Fatalf("StartSession on the relaunched shim = %v, want a resume source", resume)
	}
}
