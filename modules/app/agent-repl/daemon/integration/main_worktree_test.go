//go:build integration

package integration

import (
	"testing"

	frontendv1 "agentrepl/proto/frontend/v1"
	"agentrepl/protohelpers/rosterwalk"

	"agent-repld/integration/harness"
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
