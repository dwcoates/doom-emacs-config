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

// judged records each judgment a StandingCloses carried out: a workspace's
// id, or "*" for every request.
type judged struct{ got []string }

func (j *judged) one(_ context.Context, ws ids.WorkspaceID) error {
	j.got = append(j.got, string(ws))
	return nil
}

func (j *judged) all(context.Context) error {
	j.got = append(j.got, "*")
	return nil
}

func TestStandingClosesJudgeOnEveryEdgeThatCanClearABlocker(t *testing.T) {
	tests := []struct {
		name string
		edge func(s *StandingCloses)
		want []string
	}{
		{name: "a request registered", edge: func(s *StandingCloses) { s.Reconsider("w1") }, want: []string{"w1"}},
		{name: "a turn ending", edge: func(s *StandingCloses) { s.TurnEnded("w1") }, want: []string{"w1"}},
		{name: "a turn ending unobserved", edge: func(s *StandingCloses) { s.TurnEndedUnobserved("w1") }, want: []string{"w1"}},
		{name: "the workspace falling free", edge: func(s *StandingCloses) { s.FellFree("w1") }, want: []string{"w1"}},
		{name: "its held prompts changing", edge: func(s *StandingCloses) { s.HoldsChanged("w1") }, want: []string{"w1"}},
		{name: "its merge landing", edge: func(s *StandingCloses) { s.Landed("w1") }, want: []string{"w1"}},
		{name: "its merge leaving the queue unlanded", edge: func(s *StandingCloses) { s.Evicted("w1", "dequeued") }, want: []string{"w1"}},
		{name: "any workspace closing", edge: func(s *StandingCloses) { s.Closed(wsm.Workspace{ID: "w2"}, wsm.HowClosed) }, want: []string{"*"}},
		{name: "any workspace killed", edge: func(s *StandingCloses) { s.Closed(wsm.Workspace{ID: "w2"}, wsm.HowKilled) }, want: []string{"*"}},
		{name: "any workspace nuked", edge: func(s *StandingCloses) { s.Closed(wsm.Workspace{ID: "w2"}, wsm.HowNuked) }, want: []string{"*"}},
		{name: "any workspace retargeted", edge: func(s *StandingCloses) { s.Retargeted(wsm.ParentChange{Workspace: "w2"}) }, want: []string{"*"}},
		{name: "every request owed at boot", edge: func(s *StandingCloses) { s.ReconsiderAll() }, want: []string{"*"}},
		{name: "a turn starting", edge: func(s *StandingCloses) { s.TurnStarted("w1") }},
		{name: "an ask opening", edge: func(s *StandingCloses) { s.NeedsInput("w1") }},
		{name: "a pull request owned", edge: func(s *StandingCloses) { s.PullRequestOwned("w1", wsm.PullRequestKey{}) }},
		{name: "a deploy", edge: func(s *StandingCloses) { s.Deployed("w1", "") }},
		{name: "a cold gate", edge: func(s *StandingCloses) { s.ColdGated("w1") }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			j := &judged{}
			s := NewStandingCloses(j.one, j.all, dlog.NewTestLogger())
			ctx, cancel := context.WithCancel(context.Background())
			cancel()

			// Act
			tt.edge(s)
			s.Run(ctx)

			// Assert
			if !slices.Equal(j.got, tt.want) {
				t.Fatalf("judged %v, want %v", j.got, tt.want)
			}
		})
	}
}

func TestStandingClosesRecordAJudgmentThatFailed(t *testing.T) {
	// Arrange
	log := dlog.NewTestLogger()
	fail := func(context.Context, ids.WorkspaceID) error { return errors.New("db gone") }
	s := NewStandingCloses(fail, func(context.Context) error { return nil }, log)
	ctx, cancel := context.WithCancel(context.Background())
	cancel()

	// Act
	s.Reconsider("w1")
	s.Run(ctx)

	// Assert
	record, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: "error", Operation: opStandingClose,
		Message: "could not judge a standing close request; it keeps standing for the next edge"})
	if !ok || record.Context["workspace"] != "w1" {
		t.Fatalf("no error record naming w1: %+v", log.Records())
	}
}

func TestStandingClosesRecordAnEdgeAfterTheyStopped(t *testing.T) {
	// Arrange
	log := dlog.NewTestLogger()
	j := &judged{}
	s := NewStandingCloses(j.one, j.all, log)
	ctx, cancel := context.WithCancel(context.Background())
	cancel()
	s.Run(ctx)

	// Act
	s.Reconsider("w1")

	// Assert
	if len(j.got) != 0 {
		t.Fatalf("judged %v after the loop stopped, want nothing", j.got)
	}
	if _, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: "info", Operation: opStandingClose,
		Message: "an edge arrived after the standing closes stopped, as the daemon stood down; the next daemon judges the request"}); !ok {
		t.Fatalf("no info record: %+v", log.Records())
	}
}
