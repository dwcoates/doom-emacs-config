package e2e

import (
	"agentrepl/e2e/kit/stack"
	workspacev1 "agentrepl/proto/workspace/v1"

	"agent-repld/integration/harness"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

func testNewRepoWorkspaceRegistersTheRepositoryItMinted(t *testing.T) {
	// Arrange
	w := stack.SharedWorld(t)

	// Act
	repo, ws := w.NewRepoWorkspace(t)

	// Assert
	if ws.GetId() == "" {
		t.Errorf("workspace = %v, want a daemon-minted id", ws)
	}
	if _, err := os.Stat(filepath.Join(repo.Dir, ".git")); err != nil {
		t.Errorf("repository %s has no .git marker: %v", repo.Dir, err)
	}
	if got, _ := filepath.EvalSymlinks(ws.GetDir()); got != repo.Dir && ws.GetDir() != repo.Dir {
		t.Errorf("workspace dir = %s, want the minted repository %s", ws.GetDir(), repo.Dir)
	}
}

func testNewWorkspaceMintsAFreshWorkspaceEachTime(t *testing.T) {
	// Arrange
	w := stack.SharedWorld(t)

	// Act
	first, second := w.NewWorkspace(t), w.NewWorkspace(t)

	// Assert
	if first.GetId() == second.GetId() || first.GetDir() == second.GetDir() {
		t.Fatalf("workspaces %v and %v, want two distinct ones", first, second)
	}
}

func testRequireOpenOpensARegisteredWorkspace(t *testing.T) {
	// Arrange
	w := stack.SharedWorld(t)
	ws := w.NewWorkspace(t)

	// Act
	w.RequireOpen(t, ws)

	// Assert: the open spawned the shim, so a turn reaches its end.
	turn := stack.SubmitPrompt(t, w, ws, "Tell me something you find interesting.")
	stack.AwaitTurnEnded(t, w, ws, turn)
}

func testRequireOpenFailsTheTestForAWorkspaceTheDaemonDoesNotKnow(t *testing.T) {
	// Arrange
	w := stack.SharedWorld(t)
	r := harness.NewRecordingTB(t, "TestOpener")
	unknown := &workspacev1.WorkspaceRef{Id: "0000000000000000", Dir: t.TempDir()}

	// Act
	completed := harness.Within(func() { w.RequireOpen(r, unknown) })

	// Assert
	if completed || !strings.Contains(r.Failures(), "OpenWorkspace(0000000000000000)") {
		t.Fatalf("completed=%v failures=%q, want the open refused and the test failed", completed, r.Failures())
	}
}
