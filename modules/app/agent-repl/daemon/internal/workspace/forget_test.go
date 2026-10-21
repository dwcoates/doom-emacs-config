package workspace

import (
	"context"
	"errors"
	"fmt"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/ids"
	"agent-repld/internal/resolve/footer"
	"agent-repld/internal/sessionwatcher"
	"agent-repld/internal/wsm"
)

// closedWorkspace records a workspace the way the forget verb requires it:
// CLOSED, with no live session, which is the state Close leaves behind.
func closedWorkspace(t *testing.T, f *fixture, id ids.WorkspaceID) wsm.Workspace {
	t.Helper()
	ws := f.workspace(id, t.TempDir())
	ws.Closed = true
	f.db.with(ws)
	f.hasSession = false
	return ws
}

func TestForgetRemovesTheRegistryRecord(t *testing.T) {
	// Arrange
	f := newFixture(t)
	closedWorkspace(t, f, "w1")

	// Act
	if err := f.verbs.Forget(context.Background(), "w1"); err != nil {
		t.Fatalf("Forget: %v", err)
	}

	// Assert
	if len(f.db.forgotten) != 1 || f.db.forgotten[0] != "w1" {
		t.Fatalf("forgotten = %v, want exactly the forgotten workspace", f.db.forgotten)
	}
}

func TestForgetDestroysNoWorktree(t *testing.T) {
	// Arrange: forget is a registry act; nuke is the verb that deletes files.
	f := newFixture(t)
	closedWorkspace(t, f, "w1")

	// Act
	if err := f.verbs.Forget(context.Background(), "w1"); err != nil {
		t.Fatalf("Forget: %v", err)
	}

	// Assert
	if len(f.git.nuked) != 0 {
		t.Fatalf("git nukes = %+v, want none", f.git.nuked)
	}
}

func TestForgetRepublishesTheRoster(t *testing.T) {
	// Arrange
	f := newFixture(t)
	closedWorkspace(t, f, "w1")

	// Act
	if err := f.verbs.Forget(context.Background(), "w1"); err != nil {
		t.Fatalf("Forget: %v", err)
	}

	// Assert
	if len(f.sidebar.registries) != 1 {
		t.Fatalf("roster republishes = %d, want exactly one", len(f.sidebar.registries))
	}
}

func TestForgetEvictsTheWorkspaceLogSinks(t *testing.T) {
	// Arrange
	f := newFixture(t)
	ws := closedWorkspace(t, f, "w1")

	// Act
	if err := f.verbs.Forget(context.Background(), "w1"); err != nil {
		t.Fatalf("Forget: %v", err)
	}

	// Assert
	if len(f.log.evicted) != 1 || f.log.evicted[0] != ws.Dir {
		t.Fatalf("evicted sinks = %v, want the forgotten workspace's dir", f.log.evicted)
	}
}

func TestForgetSurvivesAFailedSinkEviction(t *testing.T) {
	// Arrange: a leaked sink is not a reason to fail a forget the registry
	// already carried out.
	f := newFixture(t)
	closedWorkspace(t, f, "w1")
	f.log.evictErr = errors.New("the sink would not release")

	// Act
	err := f.verbs.Forget(context.Background(), "w1")

	// Assert
	if err != nil {
		t.Fatalf("Forget: %v", err)
	}
}

func TestForgetSurfacesAFailedRegistryDelete(t *testing.T) {
	// Arrange: a half-forgotten registry is never reported as success.
	f := newFixture(t)
	closedWorkspace(t, f, "w1")
	f.db.forgetErr = errFake

	// Act
	err := f.verbs.Forget(context.Background(), "w1")

	// Assert
	if !errors.Is(err, errFake) {
		t.Fatalf("Forget = %v, want the store's failure", err)
	}
}

func TestForgetRefusesAnOpenWorkspace(t *testing.T) {
	// Arrange: the record still says open, so the close verb's gate was never
	// passed.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.hasSession = false

	// Act
	err := f.verbs.Forget(context.Background(), "w1")

	// Assert
	refusal, ok := AsRefusal(err)
	if !ok || refusal.Arm != ArmNotClosed {
		t.Fatalf("Forget = %v, want the not_closed refusal", err)
	}
	if len(f.db.forgotten) != 0 {
		t.Fatalf("forgotten = %v, want nothing forgotten", f.db.forgotten)
	}
}

func TestForgetRefusesAWorkspaceThatIsNotQuiet(t *testing.T) {
	tests := []struct {
		name string
		// arrange puts the one piece of live state this case is about in
		// place, on a workspace whose record already says closed.
		arrange func(f *fixture)
	}{
		{
			name: "a turn is still in flight",
			arrange: func(f *fixture) {
				turn := ids.TurnID("t1")
				f.hasSession = true
				f.running = Running{Turn: &turn}
			},
		},
		{
			name: "detached work is still live",
			arrange: func(f *fixture) {
				f.hasSession = true
				f.running = Running{LiveWork: sessionwatcher.LiveWorkSet{Shells: []*conversationv1.DetachedWorkId{{Value: "b1"}}}}
			},
		},
		{
			name: "held prompts have not been delivered",
			arrange: func(f *fixture) {
				f.db.held["w1"] = []wsm.HeldPrompt{{Turn: "t1", Workspace: "w1"}}
			},
		},
		{
			name: "a merge is still queued",
			arrange: func(f *fixture) {
				f.merge.facts["w1"] = footer.MergeFacts{State: "queued"}
			},
		},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			closedWorkspace(t, f, "w1")
			test.arrange(f)

			// Act
			err := f.verbs.Forget(context.Background(), "w1")

			// Assert
			refusal, ok := AsRefusal(err)
			if !ok || refusal.Arm != "blocked" {
				t.Fatalf("Forget = %v, want the blocked refusal", err)
			}
			if len(f.db.forgotten) != 0 {
				t.Fatalf("forgotten = %v, want nothing forgotten", f.db.forgotten)
			}
		})
	}
}

func TestForgetRefusesAWorkspaceOthersWereSpawnedFrom(t *testing.T) {
	// Arrange: the schema nulls a child's parent_id rather than refusing, so
	// the forget would silently flatten the fork's lineage.
	f := newFixture(t)
	parent := closedWorkspace(t, f, "w1")
	child := f.workspace("w2", t.TempDir())
	child.Parent = &parent.ID
	f.db.with(child)

	// Act
	err := f.verbs.Forget(context.Background(), "w1")

	// Assert
	refusal, ok := AsRefusal(err)
	if !ok || refusal.Arm != ArmHasChildren {
		t.Fatalf("Forget = %v, want the has_children refusal", err)
	}
	if len(f.db.forgotten) != 0 {
		t.Fatalf("forgotten = %v, want nothing forgotten", f.db.forgotten)
	}
}

func TestForgetNamesTheChildrenOnTheRefusal(t *testing.T) {
	// Arrange: a sentence is not a field — the arm carries the ids.
	f := newFixture(t)
	parent := closedWorkspace(t, f, "w1")
	child := f.workspace("w2", t.TempDir())
	child.Parent = &parent.ID
	f.db.with(child)

	// Act
	err := f.verbs.Forget(context.Background(), "w1")

	// Assert
	refusal, ok := AsRefusal(err)
	if !ok {
		t.Fatalf("Forget = %v, want a refusal", err)
	}
	children, ok := refusal.Fields["children"].([]string)
	if !ok || len(children) != 1 || children[0] != "w2" {
		t.Fatalf("refusal children = %v, want the one child", refusal.Fields["children"])
	}
}

func TestForgetRefusesAWorkspaceThisDaemonDoesNotServe(t *testing.T) {
	// Arrange
	f := newFixture(t)
	closedWorkspace(t, f, "w1")
	f.owner.standing = StandingTransferringAway

	// Act
	err := f.verbs.Forget(context.Background(), "w1")

	// Assert
	refusal, ok := AsRefusal(err)
	if !ok || refusal.Arm != ArmTransferringAway {
		t.Fatalf("Forget = %v, want the transferring_away refusal", err)
	}
}

func TestForgetRefusesAnUnknownWorkspace(t *testing.T) {
	// Arrange
	f := newFixture(t)

	// Act
	err := f.verbs.Forget(context.Background(), "absent")

	// Assert
	refusal, ok := AsRefusal(err)
	if !ok || refusal.Arm != ArmUnknownWorkspace {
		t.Fatalf("Forget = %v, want the unknown_workspace refusal", err)
	}
}

// TestForgetStandsDownTheWorkspacesLiveSession pins the fix for the phantom
// shim: Close is view-level and leaves the session alone, so a closed
// workspace routinely still has a live shim, and the forget is the LAST verb
// that can address it — the row it resolves through is about to go.
func TestForgetStandsDownTheWorkspacesLiveSession(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	closedWorkspace(t, f, "w1")
	f.fleet.live["w1"] = true

	// Act.
	if err := f.verbs.Forget(context.Background(), "w1"); err != nil {
		t.Fatalf("Forget: %v", err)
	}

	// Assert.
	if len(f.fleet.stopped) != 1 || f.fleet.stopped[0].WS != "w1" || !f.fleet.stopped[0].Force {
		t.Fatalf("session stops = %+v, want one forced stop of w1 before the record goes", f.fleet.stopped)
	}
}

// TestForgetStopsNoSessionWhenNoneIsLive pins the other edge: a workspace with
// no live session is forgotten without a stand-down.
func TestForgetStopsNoSessionWhenNoneIsLive(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	closedWorkspace(t, f, "w1")

	// Act.
	if err := f.verbs.Forget(context.Background(), "w1"); err != nil {
		t.Fatalf("Forget: %v", err)
	}

	// Assert.
	if len(f.fleet.stopped) != 0 {
		t.Fatalf("session stops = %+v, want none when no session is live", f.fleet.stopped)
	}
}

// TestForgetKeepsTheRecordWhenTheSessionWillNotStandDown pins that the
// stand-down's failure refuses the forget: forgetting the row anyway would
// orphan the very shim this step exists to reclaim.
func TestForgetKeepsTheRecordWhenTheSessionWillNotStandDown(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	closedWorkspace(t, f, "w1")
	f.fleet.live["w1"] = true
	f.fleet.stopErr = errors.New("the shim would not die")

	// Act.
	err := f.verbs.Forget(context.Background(), "w1")

	// Assert.
	if err == nil {
		t.Fatal("Forget = nil, want the stand-down's failure")
	}
	if len(f.db.forgotten) != 0 {
		t.Fatalf("forgotten = %v, want none: the shim is still running", f.db.forgotten)
	}
}

// closedMainWorktree records workspace id as the CLOSED main worktree of a
// repository of its own, with `linked' more workspaces registered under it.
func closedMainWorktree(t *testing.T, f *fixture, id ids.WorkspaceID, linked int) wsm.Workspace {
	t.Helper()
	dir := worktreeDir(t)
	repo := ids.RepoID("repo-" + string(id))
	f.db.repositories = append(f.db.repositories, wsm.Repository{ID: repo, Dir: dir})
	main := wsm.Workspace{ID: id, Dir: dir, Repo: repo, Closed: true}
	f.db.with(main)
	for i := 0; i < linked; i++ {
		f.db.with(wsm.Workspace{ID: ids.WorkspaceID(fmt.Sprintf("%s-linked-%d", id, i)), Dir: worktreeDir(t), Repo: repo})
	}
	f.hasSession = false
	return main
}

func TestForgetRefusesTheMainWorktreeWhileItsRepositoryHasOtherWorkspaces(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	closedMainWorktree(t, f, "main", 2)

	// Act.
	err := f.verbs.Forget(context.Background(), "main")

	// Assert.
	refusal := asRefusal(t, err, ArmMainWorktree)
	if refusal.Fields["other_workspaces"] != uint32(2) {
		t.Fatalf("refusal fields = %v, want other_workspaces = 2", refusal.Fields)
	}
	if len(f.db.forgotten) != 0 || len(f.sidebar.registries) != 0 {
		t.Fatalf("forgotten %v, publications %d; want nothing changed by a refused forget", f.db.forgotten, len(f.sidebar.registries))
	}
	if !hasRecord(f, "info", opRefusal) {
		t.Fatal("the refusal was not recorded at INFO")
	}
}

func TestForgetRefusesTheMainWorktreeBeforeStandingItsSessionDown(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	closedMainWorktree(t, f, "main", 1)
	f.fleet.live["main"] = true

	// Act.
	err := f.verbs.Forget(context.Background(), "main")

	// Assert.
	asRefusal(t, err, ArmMainWorktree)
	if len(f.fleet.stopped) != 0 {
		t.Fatalf("session stops = %+v, want none for a refused forget", f.fleet.stopped)
	}
}

func TestForgetForgetsTheMainWorktreeAsItsRepositorysLastWorkspace(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	closedMainWorktree(t, f, "main", 0)

	// Act.
	err := f.verbs.Forget(context.Background(), "main")

	// Assert.
	if err != nil {
		t.Fatalf("Forget: %v", err)
	}
	if len(f.db.forgotten) != 1 || f.db.forgotten[0] != "main" {
		t.Fatalf("forgotten = %v, want the main worktree", f.db.forgotten)
	}
}

func TestForgetAnswersTheRegistrysMainWorktreeRefusalByArm(t *testing.T) {
	// Arrange: a workspace registered under the repository between the
	// verb's check and the registry's deletion.
	f := newFixture(t)
	closedMainWorktree(t, f, "main", 0)
	f.db.forgetErr = fmt.Errorf("wsm: forget main: %w", wsm.ErrMainWorktreeInUse)

	// Act.
	err := f.verbs.Forget(context.Background(), "main")

	// Assert.
	asRefusal(t, err, ArmMainWorktree)
	if hasRecord(f, "error", opForget) {
		t.Fatal("the refusal was recorded at ERROR; a refusal is an answer")
	}
}

func TestForgetSurfacesAnUnreadableRegistryForTheMainWorktreeCheck(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	closedMainWorktree(t, f, "main", 0)
	f.db.listRepositoriesErr = errors.New("the state database is unreachable")

	// Act.
	err := f.verbs.Forget(context.Background(), "main")

	// Assert.
	if !errors.Is(err, f.db.listRepositoriesErr) {
		t.Fatalf("Forget = %v, want the registry's failure", err)
	}
	if !hasRecord(f, "error", opForget) || len(f.db.forgotten) != 0 {
		t.Fatalf("want the failure recorded at ERROR and nothing forgotten (forgotten %v)", f.db.forgotten)
	}
}
