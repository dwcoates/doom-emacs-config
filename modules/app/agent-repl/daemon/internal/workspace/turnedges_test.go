package workspace

import (
	"context"
	"errors"
	"testing"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

func TestTurnEdgeRunnerCarriesOutOnlyItsOwnEdge(t *testing.T) {
	edges := map[string]func(r *TurnEdgeRunner){
		"a turn starting":          func(r *TurnEdgeRunner) { r.TurnStarted("w1") },
		"a turn ending":            func(r *TurnEdgeRunner) { r.TurnEnded("w1") },
		"a turn ending unobserved": func(r *TurnEdgeRunner) { r.TurnEndedUnobserved("w1") },
		"an ask opening":           func(r *TurnEdgeRunner) { r.NeedsInput("w1") },
		"a landing":                func(r *TurnEdgeRunner) { r.Landed("w1") },
		"a close":                  func(r *TurnEdgeRunner) { r.Closed(wsm.Workspace{ID: "w1"}, wsm.HowClosed) },
		"a pull request owned":     func(r *TurnEdgeRunner) { r.PullRequestOwned("w1", wsm.PullRequestKey{}) },
	}
	tests := []struct {
		name  string
		edge  turnEdge
		moves map[string]bool
	}{
		{name: "the start runner", edge: turnStarts, moves: map[string]bool{"a turn starting": true}},
		{name: "the end runner", edge: turnEnds, moves: map[string]bool{"a turn ending": true, "a turn ending unobserved": true}},
	}
	for _, tt := range tests {
		for edgeName, fire := range edges {
			t.Run(tt.name+"/"+edgeName, func(t *testing.T) {
				// Arrange
				var carried []ids.WorkspaceID
				r := newTurnEdgeRunner(tt.edge, "test.op", "untold", "failed", func(_ context.Context, ws ids.WorkspaceID) error {
					carried = append(carried, ws)
					return nil
				}, dlog.NewTestLogger())
				ctx, cancel := context.WithCancel(context.Background())
				cancel()

				// Act
				fire(r)
				r.Run(ctx)

				// Assert
				want := 0
				if tt.moves[edgeName] {
					want = 1
				}
				if len(carried) != want || (want == 1 && carried[0] != "w1") {
					t.Fatalf("carried = %v, want %d carry of w1", carried, want)
				}
			})
		}
	}
}

func TestTurnEdgeRunnerRecordsWorkItCannotCarryOut(t *testing.T) {
	// Arrange
	log := dlog.NewTestLogger()
	r := newTurnEdgeRunner(turnEnds, "test.op", "untold", "could not carry it out", func(context.Context, ids.WorkspaceID) error {
		return errors.New("db gone")
	}, log)
	ctx, cancel := context.WithCancel(context.Background())
	cancel()

	// Act
	r.TurnEnded("w1")
	r.Run(ctx)

	// Assert
	record, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: "error", Operation: "test.op", Message: "could not carry it out"})
	if !ok || record.Context["workspace"] != "w1" {
		t.Fatalf("no error record naming the workspace: %+v", log.Records())
	}
}

func TestTurnEdgeRunnerRecordsAnEdgeAfterItStopped(t *testing.T) {
	// Arrange
	log := dlog.NewTestLogger()
	r := newTurnEdgeRunner(turnEnds, "test.op", "reported after the runner stopped", "failed", func(context.Context, ids.WorkspaceID) error {
		t.Fatal("work carried out after the runner stopped")
		return nil
	}, log)
	ctx, cancel := context.WithCancel(context.Background())
	cancel()
	r.Run(ctx)

	// Act
	r.TurnEnded("w1")

	// Assert
	record, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: "info", Operation: "test.op", Message: "reported after the runner stopped"})
	if !ok || record.Context["workspace"] != "w1" {
		t.Fatalf("no info record naming the workspace: %+v", log.Records())
	}
}
