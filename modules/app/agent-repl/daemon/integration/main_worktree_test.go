//go:build integration

package integration

import (
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	"agentrepl/protohelpers/rosterwalk"

	"agent-repld/integration/harness"

	"connectrpc.com/connect"
)

// closedRowAt answers the roster's closed row for the workspace at dir, nil
// when the closed list carries none.
func closedRowAt(r *frontendv1.WorkspaceRoster, dir string) *frontendv1.RosterRow {
	for _, row := range rosterwalk.ClosedRows(r) {
		if row.GetWorkspace().GetWorkspace().GetDir() == dir {
			return row
		}
	}
	return nil
}

// TestRegisteringALinkedWorktreeRegistersItsMainWorktreeClosed drives the
// main-worktree guarantee through the daemon: announcing a LINKED worktree of
// a repository nothing has registered leaves the repository's main worktree a
// registered workspace too, closed rather than opened.
func TestRegisteringALinkedWorktreeRegistersItsMainWorktreeClosed(t *testing.T) {
	t.Parallel()
	// Arrange
	d := newDaemon(t, harness.Opts{})
	repo := harness.NewRepo(t)
	linked := repo.AddWorktree("feature")
	roster := d.WatchRoster()

	// Act
	harness.Register(t, d, linked)

	// Assert
	awaitRoster(t, d, roster, "a roster carrying the main worktree as a closed row",
		func(r *frontendv1.WorkspaceRoster) bool { return closedRowAt(r, repo.Dir) != nil })
}

// TestForgetRefusesTheMainWorktreeWhileALinkedWorktreeIsRegistered pins the
// other half: the main worktree's workspace stands while the repository has
// another workspace, and the refusal is the contract's main_worktree arm.
func TestForgetRefusesTheMainWorktreeWhileALinkedWorktreeIsRegistered(t *testing.T) {
	t.Parallel()
	// Arrange
	d := newDaemon(t, harness.Opts{})
	repo := harness.NewRepo(t)
	linked := repo.AddWorktree("feature")
	roster := d.WatchRoster()
	harness.Register(t, d, linked)
	main := closedRowAt(awaitRoster(t, d, roster, "a roster carrying the main worktree as a closed row",
		func(r *frontendv1.WorkspaceRoster) bool { return closedRowAt(r, repo.Dir) != nil }), repo.Dir)

	// Act
	resp, err := d.Client().ForgetWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.ForgetWorkspaceRequest{
		Workspace: main.GetWorkspace().GetWorkspace(),
	}))

	// Assert
	if err != nil {
		t.Fatalf("ForgetWorkspace = transport error %v, want an in-band answer", err)
	}
	if got := resp.Msg.GetError().GetMainWorktree(); got == nil || got.GetOtherWorkspaces() != 1 {
		t.Fatalf("ForgetWorkspace = %v, want main_worktree naming the one linked workspace", resp.Msg)
	}
}
