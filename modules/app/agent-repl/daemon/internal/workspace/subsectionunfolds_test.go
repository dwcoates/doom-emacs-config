package workspace

import (
	"context"
	"errors"
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

func TestUnfoldSubsectionForWorkExpandsTheCollapsedSubsectionAndRepublishes(t *testing.T) {
	// Arrange
	f := foldedFixture(t)

	// Act
	err := f.verbs.UnfoldSubsectionForWork(context.Background(), "inside")

	// Assert
	if err != nil {
		t.Fatalf("UnfoldSubsectionForWork: %v", err)
	}
	if f.db.subsections[0].Folded {
		t.Error("the subsection is still collapsed")
	}
	if len(f.sidebar.registries) != 1 {
		t.Errorf("roster republications = %d, want 1", len(f.sidebar.registries))
	}
	if _, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: dlog.LevelInfo, Operation: opUnfoldForWork}); !ok {
		t.Error("the unfold was not recorded at INFO")
	}
}

func TestUnfoldSubsectionForWorkLeavesOtherWorkspacesAlone(t *testing.T) {
	tests := []struct {
		name string
		ws   ids.WorkspaceID
	}{
		{name: "a workspace in no subsection", ws: "outside"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := foldedFixture(t)

			// Act
			err := f.verbs.UnfoldSubsectionForWork(context.Background(), tt.ws)

			// Assert
			if err != nil {
				t.Fatalf("UnfoldSubsectionForWork: %v", err)
			}
			if !f.db.subsections[0].Folded || len(f.sidebar.registries) != 0 {
				t.Errorf("folded = %v, republications = %d; want the fold and the roster untouched", f.db.subsections[0].Folded, len(f.sidebar.registries))
			}
		})
	}
}

func TestSubsectionUnfoldsOnlyATurnStartMovesIt(t *testing.T) {
	tests := []struct {
		name string
		edge func(*SubsectionUnfolds)
		want []ids.WorkspaceID
	}{
		{name: "a turn starting", edge: func(s *SubsectionUnfolds) { s.TurnStarted("w1") }, want: []ids.WorkspaceID{"w1"}},
		{name: "a turn ending", edge: func(s *SubsectionUnfolds) { s.TurnEnded("w1") }},
		{name: "a turn ending unobserved", edge: func(s *SubsectionUnfolds) { s.TurnEndedUnobserved("w1") }},
		{name: "an ask opening", edge: func(s *SubsectionUnfolds) { s.NeedsInput("w1") }},
		{name: "a landing", edge: func(s *SubsectionUnfolds) { s.Landed("w1") }},
		{name: "a close", edge: func(s *SubsectionUnfolds) { s.Closed(wsm.Workspace{ID: "w1"}) }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			var expanded []ids.WorkspaceID
			s := NewSubsectionUnfolds(func(_ context.Context, ws ids.WorkspaceID) error { expanded = append(expanded, ws); return nil }, dlog.NewTestLogger())
			ctx, cancel := context.WithCancel(context.Background())
			cancel()

			// Act
			tt.edge(s)
			s.Run(ctx)

			// Assert
			if len(expanded) != len(tt.want) || (len(expanded) == 1 && expanded[0] != tt.want[0]) {
				t.Errorf("expanded = %v, want %v", expanded, tt.want)
			}
		})
	}
}

func TestSubsectionUnfoldsRecordsAnUnfoldItCannotCarryOut(t *testing.T) {
	// Arrange
	log := dlog.NewTestLogger()
	s := NewSubsectionUnfolds(func(context.Context, ids.WorkspaceID) error { return errors.New("db gone") }, log)
	ctx, cancel := context.WithCancel(context.Background())
	cancel()

	// Act
	s.TurnStarted("w1")
	s.Run(ctx)

	// Assert
	if record, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: "error", Operation: opUnfoldForWork,
		Message: "could not unfold the subsection of a workspace whose turn started"}); !ok || record.Context["workspace"] != "w1" {
		t.Fatalf("no error record naming the workspace: %+v", log.Records())
	}
}
