package workspace

import (
	"context"
	"errors"
	"slices"
	"testing"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/prompts"
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
		{name: "the open children cannot be read", arrange: func(f *fixture) { f.db.noOpenChildrenErr = errors.New("db gone") },
			message: "could not read whether the workspace has open children"},
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
		edge func(s *TurnEdgeRunner)
		want []ids.WorkspaceID
	}{
		{name: "a turn ending", edge: func(s *TurnEdgeRunner) { s.TurnEnded("w1") }, want: []ids.WorkspaceID{"w1"}},
		{name: "a turn ending unobserved", edge: func(s *TurnEdgeRunner) { s.TurnEndedUnobserved("w1") }, want: []ids.WorkspaceID{"w1"}},
		{name: "a turn starting", edge: func(s *TurnEdgeRunner) { s.TurnStarted("w1") }},
		{name: "an ask opening", edge: func(s *TurnEdgeRunner) { s.NeedsInput("w1") }},
		{name: "a landing", edge: func(s *TurnEdgeRunner) { s.Landed("w1") }},
		{name: "a close", edge: func(s *TurnEdgeRunner) { s.Closed(wsm.Workspace{ID: "w1"}, wsm.HowClosed) }},
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

func TestCloseSelfIsRefusedNowByAParentsOpenChildren(t *testing.T) {
	// Arrange
	f := selfClosing(t)
	f.db.noOpenChildrenErr = errOpenChildren

	// Act
	later, err := f.verbs.CloseSelf(context.Background(), "w1")

	// Assert: refused synchronously, as an immediate close is, with nothing
	// marked and nothing closed.
	requireFamilyAnswer(t, f, err, errOpenChildren, opClose)
	if later {
		t.Error("a refused self-close answered closes-when-the-turn-ends")
	}
	if f.db.workspaces["w1"].CloseWhenTurnEnds != nil || f.db.closedFlags["w1"] {
		t.Errorf("mark %v, closed %v; want neither", f.db.workspaces["w1"].CloseWhenTurnEnds, f.db.closedFlags["w1"])
	}
}

// selfCloseMarked is selfClosing with w1 marked to close and its turn ended,
// the self-close-refused brief shipped.
func selfCloseMarked(t *testing.T) *fixture {
	t.Helper()
	f := selfClosing(t)
	f.briefs[BriefSelfCloseRefused] = prompts.Prompt{
		Name: BriefSelfCloseRefused, Body: "refused: {{why}}", Placeholders: []string{"why"},
	}
	if _, err := f.verbs.CloseSelf(context.Background(), "w1"); err != nil {
		t.Fatalf("CloseSelf: %v", err)
	}
	f.running.Turn = nil
	return f
}

func TestTheTurnEndReportsASelfCloseThatIsNowRefused(t *testing.T) {
	tests := []struct {
		name       string
		arrange    func(f *fixture)
		wantNotice string
		wantFooter bool
	}{
		{name: "held prompts now block it", arrange: func(f *fixture) {
			f.db.held["w1"] = []wsm.HeldPrompt{{Turn: "held-1"}}
		}, wantNotice: "refused: 1 held prompts have not been delivered; release or drop them first", wantFooter: true},
		{name: "a child opened during the turn", arrange: func(f *fixture) {
			f.db.noOpenChildrenErr = errOpenChildren
		}, wantNotice: "refused: " + errOpenChildren.Error()},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := selfCloseMarked(t)
			tt.arrange(f)

			// Act
			err := f.verbs.CarryOutSelfClose(context.Background(), "w1")

			// Assert: the refusal is the workspace's to hear, not a failure
			// of the turn edge, and it stays open with the mark taken.
			if err != nil {
				t.Fatalf("CarryOutSelfClose: %v", err)
			}
			if f.db.closedFlags["w1"] || f.db.workspaces["w1"].CloseWhenTurnEnds != nil {
				t.Errorf("closed %v, mark %v; want open with the mark taken", f.db.closedFlags["w1"], f.db.workspaces["w1"].CloseWhenTurnEnds)
			}
			record, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: dlog.LevelInfo, Operation: opClose,
				Message: "the close the workspace asked for was refused when its turn ended; it stays open"})
			if !ok || record.Context["cause"] == nil {
				t.Errorf("no info record with the cause: %+v", f.log.logger.Records())
			}
			if told := toldRequester(f, "w1"); !slices.Equal(told, []string{tt.wantNotice}) {
				t.Errorf("notices = %q, want %q", told, tt.wantNotice)
			}
			if _, standing := f.footer.closing["w1"]; standing != tt.wantFooter {
				t.Errorf("footer close-blocked standing = %v, want %v", standing, tt.wantFooter)
			}
		})
	}
}

func TestARefusedSelfCloseWhoseNoticeFailsIsRecorded(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(f *fixture)
		message string
	}{
		{name: "the brief is missing", arrange: func(f *fixture) { delete(f.briefs, BriefSelfCloseRefused) },
			message: "could not read a daemon brief"},
		{name: "the queue does not accept it", arrange: func(f *fixture) { f.queue.submitErr = errors.New("queue gone") },
			message: "the workspace's queue did not accept the refused self-close's notice"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := selfCloseMarked(t)
			f.db.noOpenChildrenErr = errOpenChildren
			tt.arrange(f)

			// Act
			err := f.verbs.CarryOutSelfClose(context.Background(), "w1")

			// Assert
			if err != nil || f.db.closedFlags["w1"] {
				t.Fatalf("CarryOutSelfClose = %v, closed %v; want nil and open", err, f.db.closedFlags["w1"])
			}
			if _, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: dlog.LevelError, Operation: opClose, Message: tt.message}); !ok {
				t.Errorf("no error record %q: %+v", tt.message, f.log.logger.Records())
			}
		})
	}
}

func TestTheShippedSelfCloseRefusedBriefSplices(t *testing.T) {
	// Arrange
	brief := loadShippedBrief(t, BriefSelfCloseRefused)

	// Act
	_, err := brief.Splice(map[string]string{"why": "w"})

	// Assert
	if err != nil {
		t.Fatalf("splice: %v", err)
	}
}
