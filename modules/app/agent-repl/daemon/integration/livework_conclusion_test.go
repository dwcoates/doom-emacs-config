//go:build integration

package integration

import (
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/integration/harness"
)

// TestAHungShellsConclusionFreesTheWorkspace is the 2026-09-27 wedge
// (workspace 3e2d9cadc6794e13), end to end. A hand-backgrounded shell's
// WatchBash never got its first frame; the shim concluded the shell, but the
// daemon went on counting it live, so a bounce registered behind it waited on
// a freeness edge that never came and the workspace was never handed over.
//
// The daemon's live set now follows the shim's conclusion, not the watch: here
// the shim's re-announcement no longer names the shell, the daemon retires it
// (loudly: it had missed the conclusion), and the workspace's freeness edge
// reaches the bounce registry -- the edge a registered bounce is taken on
// (internal/promptqueue's own tests take one there). The restart verb used to
// register that bounce here; a restart is immediate now
// (endpoint_restart_workspace.proto), so the edge itself is the assertion.
func TestAHungShellsConclusionFreesTheWorkspace(t *testing.T) {
	t.Parallel()

	// Arrange: a shell whose WatchBash the shim never answers.
	f := newOpened(t, harness.Opts{})
	// The sweep covers every test; the declared records are evidence of the
	// session stream the test severs (so the shim re-announces) and the
	// conclusion the daemon missed and reconciles.
	f.d.ExpectWarnings("daemon.sessionwatcher.live_work_stale",
		"daemon.shimclient.redial", "daemon.sessionwatcher.reopen", "daemon.health.open_fault",
		"daemon.sessionwatcher.link_fault", "daemon.sessionwatcher.watch_agent",
		"daemon.sessionwatcher.watch_session")
	f.shim.ExpectStartSession()
	f.shim.SilenceBash("work-hung-1")
	pushDetachedShell(f.shim, "work-hung-1", "sleep 100")
	f.shim.ExpectWatchBash()
	daemonLog := harness.WorkspaceLogPath(f.repo.Dir, "daemon")

	// Act: the shim concludes the shell -- its live membership no longer
	// names it -- and re-announces that membership on the daemon's next
	// session watch.
	f.shim.SetLiveWork()
	f.shim.DropStream(harness.StreamSession)

	// Assert: the daemon retires the shell it had missed the end of, and the
	// workspace falls free on the registry's freeness edge.
	f.d.AwaitLogRecord(daemonLog, "the stale shell retired by reconciliation", func(r harness.LogRecord) bool {
		return r.Operation == "daemon.sessionwatcher.live_work_stale" && r.Context["work_id"] == "work-hung-1"
	})
	f.d.AwaitLogRecord(daemonLog, "the freeness edge the registry takes a bounce on", func(r harness.LogRecord) bool {
		return r.Operation == "daemon.promptqueue.bounce" && r.Message == "the workspace fell free with no bounce to take"
	})
}

// TestAShellWhoseRowsAreAllTheSidecarsIsRetiredAtItsSpoolsTerminal is the
// 2026-09-28 phantom shells, end to end on the daemon's side. A subagent's
// shell the vendor's timeout moved to the background has no `start` the shim
// could write (it never saw the call), so its WatchBash waits for the rows the
// sidecar reads from its spool, instead of being refused. The daemon holds that
// watch, and the spool's terminal retires the shell from the live set.
func TestAShellWhoseRowsAreAllTheSidecarsIsRetiredAtItsSpoolsTerminal(t *testing.T) {
	t.Parallel()

	// Arrange: an announced shell whose watch opens with no `start`.
	f := newOpened(t, harness.Opts{})
	f.shim.ExpectStartSession()
	f.shim.AwaitBash("work-claimed-1")
	pushDetachedShell(f.shim, "work-claimed-1", "npm test")
	f.shim.ExpectWatchBash()

	// Act: the sidecar reads the spool's `[killed]` terminal.
	f.shim.PushBash("work-claimed-1", &conversationv1.AgentBash{Result: &conversationv1.AgentBash_Failure{
		Failure: &conversationv1.AgentBashFailure{},
	}})

	// Assert: the daemon retires the shell at its terminal.
	f.d.AwaitLogRecord(harness.WorkspaceLogPath(f.repo.Dir, "daemon"), "the shell retired at its terminal", func(r harness.LogRecord) bool {
		return r.Operation == "daemon.sessionwatcher.live_work_retired" &&
			r.Context["work_id"] == "work-claimed-1" && r.Context["conclusion"] == "bash_terminal"
	})
}
