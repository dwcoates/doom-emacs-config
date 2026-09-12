package workspace

import (
	"context"
	"errors"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/internal/ids"
	"claude-repld/internal/resolve/footer"
	"claude-repld/internal/sessionwatcher"
	"claude-repld/internal/wsm"
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
