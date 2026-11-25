package workspace

import (
	"context"
	"errors"
	"testing"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

func TestATurnEndOfAWorkspaceThatIsNoIntakeDoesNothing(t *testing.T) {
	// Arrange: even a standing close request is not this edge's to judge.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	if err := f.db.RequestClose(context.Background(), "w1", wsm.CloseRequest{By: wsm.CloseByAgent}); err != nil {
		t.Fatalf("RequestClose: %v", err)
	}

	// Act
	err := f.verbs.RetireIntakeAtTurnEnd(context.Background(), "w1")

	// Assert
	if err != nil || f.db.closedFlags["w1"] {
		t.Fatalf("RetireIntakeAtTurnEnd = %v, closed %v; want nothing done", err, f.db.closedFlags["w1"])
	}
}

func TestIntakeRetirementsRetireAtEachTurnEnd(t *testing.T) {
	tests := []struct {
		name string
		edge func(s *TurnEdgeRunner)
		want []ids.WorkspaceID
	}{
		{name: "a turn ending", edge: func(s *TurnEdgeRunner) { s.TurnEnded("w1") }, want: []ids.WorkspaceID{"w1"}},
		{name: "a turn ending unobserved", edge: func(s *TurnEdgeRunner) { s.TurnEndedUnobserved("w1") }, want: []ids.WorkspaceID{"w1"}},
		{name: "a turn starting", edge: func(s *TurnEdgeRunner) { s.TurnStarted("w1") }},
		{name: "an ask opening", edge: func(s *TurnEdgeRunner) { s.NeedsInput("w1") }},
		{name: "a landing", edge: func(s *TurnEdgeRunner) { s.Landed("w1") }},
		{name: "a close", edge: func(s *TurnEdgeRunner) { s.Closed(wsm.Workspace{ID: "w1"}, wsm.HowClosed) }},
		{name: "a retarget", edge: func(s *TurnEdgeRunner) { s.Retargeted(wsm.ParentChange{Workspace: "w1"}) }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			var carried []ids.WorkspaceID
			s := NewIntakeRetirements(func(_ context.Context, ws ids.WorkspaceID) error { carried = append(carried, ws); return nil }, dlog.NewTestLogger())
			ctx, cancel := context.WithCancel(context.Background())
			cancel()

			// Act
			tt.edge(s)
			s.Run(ctx)

			// Assert
			if len(carried) != len(tt.want) || (len(carried) == 1 && carried[0] != tt.want[0]) {
				t.Errorf("carried = %v, want %v", carried, tt.want)
			}
		})
	}
}

func TestIntakeRetirementsRecordARetirementTheyCannotCarryOut(t *testing.T) {
	// Arrange
	log := dlog.NewTestLogger()
	s := NewIntakeRetirements(func(context.Context, ids.WorkspaceID) error { return errors.New("db gone") }, log)
	ctx, cancel := context.WithCancel(context.Background())
	cancel()

	// Act
	s.TurnEnded("w1")
	s.Run(ctx)

	// Assert
	if record, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: "error", Operation: opIntakeRetirement,
		Message: "could not retire an intake workspace at its turn's end"}); !ok || record.Context["workspace"] != "w1" {
		t.Fatalf("no error record naming the workspace: %+v", log.Records())
	}
}
