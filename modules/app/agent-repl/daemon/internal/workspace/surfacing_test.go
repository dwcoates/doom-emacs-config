package workspace

import (
	"context"
	"errors"
	"slices"
	"testing"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// foldedFixture is a fixture whose workspace "inside" sits in the folded
// subsection "sub-1".
func foldedFixture(t *testing.T) *fixture {
	t.Helper()
	f := newFixture(t)
	sub := ids.SubsectionID("sub-1")
	f.db.subsections = append(f.db.subsections, wsm.Subsection{ID: sub, Repo: "repo-1", Name: "later", Place: 1, Folded: true})
	inside := f.workspace("inside", t.TempDir())
	inside.Subsection = &sub
	f.db.with(inside)
	f.workspace("outside", t.TempDir())
	return f
}

func TestSurfaceForWorkMarksTheWorkspaceAndLeavesTheFoldFolded(t *testing.T) {
	// Arrange
	f := foldedFixture(t)

	// Act
	err := f.verbs.SurfaceForWork(context.Background(), "inside")

	// Assert
	if err != nil {
		t.Fatalf("SurfaceForWork: %v", err)
	}
	if !f.db.workspaces["inside"].Surfaced {
		t.Error("the workspace is not marked surfaced")
	}
	if !f.db.subsections[0].Folded {
		t.Error("the turn's start unfolded the subsection")
	}
	if len(f.sidebar.registries) != 1 {
		t.Errorf("roster republications = %d, want 1", len(f.sidebar.registries))
	}
	if _, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: dlog.LevelInfo, Operation: opSurfaceForWork}); !ok {
		t.Error("the surfacing was not recorded at INFO")
	}
}

func TestSurfaceForWorkUnderAChildrenFold(t *testing.T) {
	// Arrange
	f := newFixture(t)
	parent := f.workspace("parent", t.TempDir())
	parent.ChildrenFolded = true
	f.db.with(parent)
	child := f.workspace("child", t.TempDir())
	child.Parent = &parent.ID
	f.db.with(child)

	// Act
	err := f.verbs.SurfaceForWork(context.Background(), "child")

	// Assert
	if err != nil {
		t.Fatalf("SurfaceForWork: %v", err)
	}
	if !f.db.workspaces["child"].Surfaced || !f.db.workspaces["parent"].ChildrenFolded {
		t.Errorf("child surfaced = %v, parent's children folded = %v; want true, true", f.db.workspaces["child"].Surfaced, f.db.workspaces["parent"].ChildrenFolded)
	}
}

func TestSurfaceForWorkLeavesOtherWorkspacesAlone(t *testing.T) {
	tests := []struct {
		name string
		ws   ids.WorkspaceID
	}{
		{name: "a workspace in no subsection", ws: "outside"},
		{name: "an unknown workspace", ws: "absent"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := foldedFixture(t)

			// Act
			err := f.verbs.SurfaceForWork(context.Background(), tt.ws)

			// Assert
			if err != nil {
				t.Fatalf("SurfaceForWork: %v", err)
			}
			if f.db.workspaces["outside"].Surfaced || len(f.sidebar.registries) != 0 {
				t.Errorf("surfaced = %v, republications = %d; want nothing marked and the roster untouched", f.db.workspaces["outside"].Surfaced, len(f.sidebar.registries))
			}
		})
	}
}

func TestEndSurfacingTakesTheMarkOffAndRepublishes(t *testing.T) {
	tests := []struct {
		name         string
		marked       bool
		wantRepublic int
	}{
		{name: "a surfaced workspace", marked: true, wantRepublic: 1},
		{name: "a workspace that was never surfaced"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := foldedFixture(t)
			if tt.marked {
				inside := f.db.workspaces["inside"]
				inside.Surfaced = true
				f.db.with(inside)
			}

			// Act
			err := f.verbs.EndSurfacing(context.Background(), "inside")

			// Assert
			if err != nil {
				t.Fatalf("EndSurfacing: %v", err)
			}
			if f.db.workspaces["inside"].Surfaced || len(f.sidebar.registries) != tt.wantRepublic {
				t.Errorf("surfaced = %v, republications = %d; want unmarked and %d", f.db.workspaces["inside"].Surfaced, len(f.sidebar.registries), tt.wantRepublic)
			}
		})
	}
}

func TestAFoldTheUserMakesLeavesTheSurfacingOfItsMembers(t *testing.T) {
	// Arrange
	f := foldedFixture(t)
	f.db.subsections[0].Folded = false
	for _, id := range []ids.WorkspaceID{"inside", "outside"} {
		ws := f.db.workspaces[id]
		ws.Surfaced = true
		f.db.with(ws)
	}

	// Act
	err := f.verbs.FoldSubsection(context.Background(), "sub-1", true, FoldRequester{RPC: "test"})

	// Assert
	if err != nil {
		t.Fatalf("FoldSubsection: %v", err)
	}
	if !f.db.workspaces["inside"].Surfaced || !f.db.workspaces["outside"].Surfaced {
		t.Errorf("inside surfaced = %v, outside surfaced = %v; want true, true", f.db.workspaces["inside"].Surfaced, f.db.workspaces["outside"].Surfaced)
	}
}

func TestAnExpandLeavesTheSurfacing(t *testing.T) {
	// Arrange
	f := foldedFixture(t)
	inside := f.db.workspaces["inside"]
	inside.Surfaced = true
	f.db.with(inside)

	// Act
	err := f.verbs.FoldSubsection(context.Background(), "sub-1", false, FoldRequester{RPC: "test"})

	// Assert
	if err != nil {
		t.Fatalf("FoldSubsection: %v", err)
	}
	if !f.db.workspaces["inside"].Surfaced {
		t.Errorf("inside surfaced = false, want true")
	}
}

func TestWorkSurfacingsOnlyATurnStartMovesThem(t *testing.T) {
	tests := []struct {
		name string
		edge func(*TurnEdgeRunner)
		want []ids.WorkspaceID
	}{
		{name: "a turn starting", edge: func(s *TurnEdgeRunner) { s.TurnStarted("w1") }, want: []ids.WorkspaceID{"w1"}},
		{name: "a turn ending", edge: func(s *TurnEdgeRunner) { s.TurnEnded("w1") }},
		{name: "a turn ending unobserved", edge: func(s *TurnEdgeRunner) { s.TurnEndedUnobserved("w1") }},
		{name: "an ask opening", edge: func(s *TurnEdgeRunner) { s.NeedsInput("w1") }},
		{name: "a landing", edge: func(s *TurnEdgeRunner) { s.Landed("w1") }},
		{name: "a close", edge: func(s *TurnEdgeRunner) { s.Closed(wsm.Workspace{ID: "w1"}, wsm.HowClosed) }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			var surfaced []ids.WorkspaceID
			s := NewWorkSurfacings(func(_ context.Context, ws ids.WorkspaceID) error { surfaced = append(surfaced, ws); return nil }, dlog.NewTestLogger())
			ctx, cancel := context.WithCancel(context.Background())
			cancel()

			// Act
			tt.edge(s)
			s.Run(ctx)

			// Assert
			if !slices.Equal(surfaced, tt.want) {
				t.Errorf("surfaced = %v, want %v", surfaced, tt.want)
			}
		})
	}
}

func TestWorkSurfacingEndsOnlyATurnEndMovesThem(t *testing.T) {
	tests := []struct {
		name string
		edge func(*TurnEdgeRunner)
		want []ids.WorkspaceID
	}{
		{name: "a turn starting", edge: func(s *TurnEdgeRunner) { s.TurnStarted("w1") }},
		{name: "a turn ending", edge: func(s *TurnEdgeRunner) { s.TurnEnded("w1") }, want: []ids.WorkspaceID{"w1"}},
		{name: "a turn ending unobserved", edge: func(s *TurnEdgeRunner) { s.TurnEndedUnobserved("w1") }, want: []ids.WorkspaceID{"w1"}},
		{name: "an ask opening", edge: func(s *TurnEdgeRunner) { s.NeedsInput("w1") }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			var ended []ids.WorkspaceID
			s := NewWorkSurfacingEnds(func(_ context.Context, ws ids.WorkspaceID) error { ended = append(ended, ws); return nil }, dlog.NewTestLogger())
			ctx, cancel := context.WithCancel(context.Background())
			cancel()

			// Act
			tt.edge(s)
			s.Run(ctx)

			// Assert
			if !slices.Equal(ended, tt.want) {
				t.Errorf("ended = %v, want %v", ended, tt.want)
			}
		})
	}
}

func TestSurfacingRunnersRecordAWriteTheyCannotCarryOut(t *testing.T) {
	tests := []struct {
		name    string
		build   func(func(context.Context, ids.WorkspaceID) error, dlog.Logger) *TurnEdgeRunner
		report  func(*TurnEdgeRunner)
		op      string
		message string
	}{
		{name: "the start", build: NewWorkSurfacings, report: func(s *TurnEdgeRunner) { s.TurnStarted("w1") }, op: opSurfaceForWork,
			message: "could not surface a workspace whose turn started under a fold"},
		{name: "the end", build: NewWorkSurfacingEnds, report: func(s *TurnEdgeRunner) { s.TurnEnded("w1") }, op: opEndSurfacing,
			message: "could not end the surfacing of a workspace whose turn ended"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			log := dlog.NewTestLogger()
			s := tt.build(func(context.Context, ids.WorkspaceID) error { return errors.New("db gone") }, log)
			ctx, cancel := context.WithCancel(context.Background())
			cancel()

			// Act
			tt.report(s)
			s.Run(ctx)

			// Assert
			if record, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: "error", Operation: tt.op, Message: tt.message}); !ok || record.Context["workspace"] != "w1" {
				t.Fatalf("no error record naming the workspace: %+v", log.Records())
			}
		})
	}
}
