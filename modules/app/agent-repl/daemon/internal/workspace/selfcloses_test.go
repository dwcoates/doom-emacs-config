package workspace

import (
	"context"
	"errors"
	"testing"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// selfClosing is a fixture whose workspace w1 runs turn t1.
func selfClosing(t *testing.T) *fixture {
	t.Helper()
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	turn := wsm.TurnID("t1")
	f.running.Turn = &turn
	return f
}

func TestCloseSelfWithItsAskingTurnRunningClosesWhenTheTurnEnds(t *testing.T) {
	// Arrange
	f := selfClosing(t)

	// Act
	later, err := f.verbs.CloseSelf(context.Background(), "w1")

	// Assert
	if err != nil || !later {
		t.Fatalf("CloseSelf = %v, %v; want closes when the turn ends", later, err)
	}
	if f.db.closedFlags["w1"] || f.db.workspaces["w1"].CloseWhenTurnEnds == nil {
		t.Fatalf("closed %v, mark %v; want open and marked", f.db.closedFlags["w1"], f.db.workspaces["w1"].CloseWhenTurnEnds)
	}
}

func TestCloseSelfWithNoTurnRunningClosesNow(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act
	later, err := f.verbs.CloseSelf(context.Background(), "w1")

	// Assert
	if err != nil || later {
		t.Fatalf("CloseSelf = %v, %v; want closed now", later, err)
	}
	if closures := f.db.closures["w1"]; len(closures) != 1 || closures[0].By != wsm.CloseByAgent {
		t.Fatalf("closures = %+v, want one by the agent", closures)
	}
}

func TestCloseSelfIsBlockedByEverythingButItsOwnTurn(t *testing.T) {
	// Arrange
	f := selfClosing(t)
	f.db.held["w1"] = []wsm.HeldPrompt{{Turn: "held-1"}}

	// Act
	_, err := f.verbs.CloseSelf(context.Background(), "w1")

	// Assert
	refusal := asRefusal(t, err, "blocked")
	if refusal.Fields["turn_in_flight"] != false || refusal.Fields["held_prompts"] != uint32(1) {
		t.Errorf("fields = %+v, want the held prompt alone blocking", refusal.Fields)
	}
	if f.db.workspaces["w1"].CloseWhenTurnEnds != nil {
		t.Error("a blocked self-close left a mark")
	}
}

func TestATurnThatEndsAsTheMarkIsWrittenClosesOnce(t *testing.T) {
	// Arrange: the turn ends between the read and the mark.
	f := selfClosing(t)
	f.db.onMarkSelfClose = func() { f.running.Turn = nil }

	// Act
	later, err := f.verbs.CloseSelf(context.Background(), "w1")
	edgeErr := f.verbs.CarryOutSelfClose(context.Background(), "w1")

	// Assert
	if err != nil || later || edgeErr != nil {
		t.Fatalf("CloseSelf = %v, %v; edge = %v; want closed now", later, err, edgeErr)
	}
	if closures := f.db.closures["w1"]; len(closures) != 1 {
		t.Fatalf("closures = %+v, want exactly one", closures)
	}
}

func TestTheTurnEndCarriesTheMarkedCloseOut(t *testing.T) {
	tests := []struct {
		name       string
		arrange    func(f *fixture)
		wantClosed bool
	}{
		{name: "a quiet workspace closes", arrange: func(*fixture) {}, wantClosed: true},
		{name: "a close something else now blocks is abandoned", arrange: func(f *fixture) {
			f.db.held["w1"] = []wsm.HeldPrompt{{Turn: "held-1"}}
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := selfClosing(t)
			if _, err := f.verbs.CloseSelf(context.Background(), "w1"); err != nil {
				t.Fatalf("CloseSelf: %v", err)
			}
			f.running.Turn = nil
			tt.arrange(f)

			// Act
			err := f.verbs.CarryOutSelfClose(context.Background(), "w1")

			// Assert
			if err != nil {
				t.Fatalf("CarryOutSelfClose: %v", err)
			}
			if f.db.closedFlags["w1"] != tt.wantClosed || f.db.workspaces["w1"].CloseWhenTurnEnds != nil {
				t.Errorf("closed %v, mark %v; want closed %v and the mark taken", f.db.closedFlags["w1"], f.db.workspaces["w1"].CloseWhenTurnEnds, tt.wantClosed)
			}
		})
	}
}

func TestATurnEndWithNoMarkClosesNothing(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act
	err := f.verbs.CarryOutSelfClose(context.Background(), "w1")

	// Assert
	if err != nil || f.db.closedFlags["w1"] {
		t.Fatalf("CarryOutSelfClose = %v, closed %v; want nothing done", err, f.db.closedFlags["w1"])
	}
}

func TestCloseSelfFails(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(f *fixture)
		message string
	}{
		{name: "the mark cannot be written", arrange: func(f *fixture) { f.db.markSelfCloseErr = errors.New("db gone") },
			message: "could not mark the workspace to close when its asking turn ends"},
		{name: "the mark cannot be taken", arrange: func(f *fixture) {
			f.db.onMarkSelfClose = func() { f.running.Turn = nil }
			f.db.takeSelfCloseErr = errors.New("db gone")
		}, message: "could not take the workspace's self-close mark"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := selfClosing(t)
			tt.arrange(f)

			// Act
			_, err := f.verbs.CloseSelf(context.Background(), "w1")

			// Assert
			if _, refused := AsRefusal(err); err == nil || refused {
				t.Fatalf("CloseSelf = %v, want a hard failure", err)
			}
			if f.db.closedFlags["w1"] {
				t.Error("a failed self-close closed the workspace")
			}
			if _, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: "error", Operation: opClose, Message: tt.message}); !ok {
				t.Errorf("no error record %q: %+v", tt.message, f.log.logger.Records())
			}
		})
	}
}

func TestSelfClosesCarriesOutEachTurnEnd(t *testing.T) {
	tests := []struct {
		name string
		edge func(s *SelfCloses)
		want []ids.WorkspaceID
	}{
		{name: "a turn ending", edge: func(s *SelfCloses) { s.TurnEnded("w1") }, want: []ids.WorkspaceID{"w1"}},
		{name: "a turn ending unobserved", edge: func(s *SelfCloses) { s.TurnEndedUnobserved("w1") }, want: []ids.WorkspaceID{"w1"}},
		{name: "a turn starting", edge: func(s *SelfCloses) { s.TurnStarted("w1") }},
		{name: "an ask opening", edge: func(s *SelfCloses) { s.NeedsInput("w1") }},
		{name: "a landing", edge: func(s *SelfCloses) { s.Landed("w1") }},
		{name: "a close", edge: func(s *SelfCloses) { s.Closed(wsm.Workspace{ID: "w1"}) }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			var carried []ids.WorkspaceID
			s := NewSelfCloses(func(_ context.Context, ws ids.WorkspaceID) error { carried = append(carried, ws); return nil }, dlog.NewTestLogger())
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

func TestSelfClosesRecordsACloseItCannotCarryOut(t *testing.T) {
	// Arrange
	log := dlog.NewTestLogger()
	s := NewSelfCloses(func(context.Context, ids.WorkspaceID) error { return errors.New("db gone") }, log)
	ctx, cancel := context.WithCancel(context.Background())
	cancel()

	// Act
	s.TurnEnded("w1")
	s.Run(ctx)

	// Assert
	if record, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: "error", Operation: opSelfClose,
		Message: "could not carry out the close a workspace asked of itself at its turn's end"}); !ok || record.Context["workspace"] != "w1" {
		t.Fatalf("no error record naming the workspace: %+v", log.Records())
	}
}
