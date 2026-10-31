//go:build integration

package integration

import (
	"syscall"
	"testing"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/integration/harness"
)

// cutAndReopen runs a turn, stands the daemon down under it, and opens the
// workspace on a successor daemon over the same state.
func cutAndReopen(t *testing.T) (*fixture, *harness.ShimControl) {
	t.Helper()
	f := newOpened(t, harness.Opts{DurableState: true})
	f.shim.ExpectStartSession()
	f.submit("do the long thing", "k-cut", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	f.shim.ExpectStartTurn()
	shutdownNow(t, f.d)
	f.d = harness.StartDaemon(t, harness.Opts{StateDir: f.d.StateDir})
	// The fake shim writes no vendor transcript, so the successor's resume
	// finds none and says so; a real session's transcript is on disk.
	f.d.ExpectWarnings("daemon.workspace.bring_up")
	resp, err := f.d.Client().OpenWorkspace(f.d.Ctx(), connect.NewRequest(&agentreplv1.OpenWorkspaceRequest{Workspace: f.ws}))
	if err != nil || resp.Msg.GetSuccess() == nil {
		t.Fatalf("OpenWorkspace on the successor = (%v, %v), want a success", resp, err)
	}
	shim := f.d.OpenedShim(f.ws)
	shim.ExpectStartSession()
	return f, shim
}

// cutByCrashAndReopen runs a turn, kills the daemon and its shim under it with
// no stand-down at all, and opens the workspace on a successor daemon over the
// same state.
func cutByCrashAndReopen(t *testing.T) (*fixture, *harness.ShimControl) {
	t.Helper()
	f := newOpened(t, harness.Opts{DurableState: true})
	f.shim.ExpectStartSession()
	f.submit("do the long thing", "k-crash-cut", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	f.shim.ExpectStartTurn()
	f.d.AwaitWorkspaceLogOperationCount(f.repo.Dir, harness.OpTurnOpened, 1)
	pid := f.shim.Info().PID
	f.d.Kill()
	if err := syscall.Kill(pid, syscall.SIGKILL); err != nil {
		t.Fatalf("kill the fake shim %d: %v", pid, err)
	}
	harness.AwaitProcessGone(t, f.d.Ctx(), pid)
	f.d = harness.StartDaemon(t, harness.Opts{StateDir: f.d.StateDir})
	f.d.ExpectWarnings("daemon.workspace.bring_up", "daemon.promptqueue.restore_holds")
	resp, err := f.d.Client().OpenWorkspace(f.d.Ctx(), connect.NewRequest(&agentreplv1.OpenWorkspaceRequest{Workspace: f.ws}))
	if err != nil || resp.Msg.GetSuccess() == nil {
		t.Fatalf("OpenWorkspace on the successor = (%v, %v), want a success", resp, err)
	}
	shim := f.d.OpenedShim(f.ws)
	shim.ExpectStartSession()
	return f, shim
}

func TestATurnACrashCutIsResumedOnTheNextSession(t *testing.T) {
	t.Parallel()
	// Arrange + Act
	_, shim := cutByCrashAndReopen(t)

	// Assert
	st := shim.ExpectStartTurn()
	if st.GetOrigin() != conversationv1.PromptOrigin_PROMPT_ORIGIN_RESUME_AFTER_RESTART {
		t.Fatalf("the successor's first turn has origin %v, want the resumption after restart", st.GetOrigin())
	}
}

func TestATurnTheImmediateShutdownCutIsResumedOnTheNextSession(t *testing.T) {
	t.Parallel()
	// Arrange + Act
	_, shim := cutAndReopen(t)

	// Assert
	st := shim.ExpectStartTurn()
	if st.GetOrigin() != conversationv1.PromptOrigin_PROMPT_ORIGIN_RESUME_AFTER_RESTART {
		t.Fatalf("the successor's first turn has origin %v, want the resumption after restart", st.GetOrigin())
	}
}

func TestTheResumptionIsSubmittedOnce(t *testing.T) {
	t.Parallel()
	// Arrange
	_, shim := cutAndReopen(t)
	shim.ExpectStartTurn()

	// Act: the resumed turn ends.
	shim.PushAgentFrame(mainAgent, successFrame(mainAgent, nil))

	// Assert
	if n := shim.Count(harness.RPCStartTurn); n != 1 {
		t.Fatalf("the successor's shim was asked to start %d turns, want the one resumption", n)
	}
}
