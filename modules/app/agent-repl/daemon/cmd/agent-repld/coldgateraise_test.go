package main

import (
	"context"
	"errors"
	"slices"
	"testing"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/workspace"
)

// recordingQueue records each edge the merge queue is told.
type recordingQueue struct{ told []string }

func (q *recordingQueue) ColdGateRaised(ws ids.WorkspaceID) {
	q.told = append(q.told, "raised "+string(ws))
}

func (q *recordingQueue) ColdGateStoodAfterFailedAnswer(ws ids.WorkspaceID) {
	q.told = append(q.told, "stood after failed answer "+string(ws))
}

// raiseRecord is what coldGateRaiseSinks.raised told every sink.
type raiseRecord struct {
	cacheJudged []bool
	followers   []ids.WorkspaceID
	queue       *recordingQueue
	answered    []ids.WorkspaceID
	log         *dlog.TestLogger
}

// sinksFor answers sinks that record into a fresh raiseRecord, the deploy
// check's answer failing with ANSWERERR.
func sinksFor(answerErr error) (coldGateRaiseSinks, *raiseRecord) {
	rec := &raiseRecord{queue: &recordingQueue{}, log: dlog.NewTestLogger()}
	return coldGateRaiseSinks{
		cache:     func(_ ids.WorkspaceID, judged bool) { rec.cacheJudged = append(rec.cacheJudged, judged) },
		followers: func(ws ids.WorkspaceID) { rec.followers = append(rec.followers, ws) },
		queue:     func() coldGateQueue { return rec.queue },
		answerDeployCheck: func(_ context.Context, ws ids.WorkspaceID) error {
			rec.answered = append(rec.answered, ws)
			return answerErr
		},
		detach: func(fn func()) { fn() },
		log:    rec.log,
	}, rec
}

func TestColdGateRaiseSinksRaised(t *testing.T) {
	const ws = ids.WorkspaceID("ws-1")
	tests := []struct {
		name          string
		raise         workspace.ColdGateRaise
		wantJudged    bool
		wantFollowers bool
		wantQueue     []string
		wantAnswered  bool
	}{
		{name: "a judged gate tells the followers, the queue's raise and the deploy check's answer",
			raise: workspace.RaiseJudged, wantJudged: true, wantFollowers: true,
			wantQueue: []string{"raised ws-1"}, wantAnswered: true},
		{name: "a standing gate raised again is answered but is no news to the followers",
			raise: workspace.RaiseStanding, wantQueue: []string{"raised ws-1"}, wantAnswered: true},
		{name: "a gate stood again after a failed answer is answered by nobody automatically",
			raise: workspace.RaiseAfterFailedAnswer, wantQueue: []string{"stood after failed answer ws-1"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			sinks, rec := sinksFor(nil)

			// Act.
			sinks.raised(ws, tt.raise)

			// Assert.
			if !slices.Equal(rec.cacheJudged, []bool{tt.wantJudged}) {
				t.Fatalf("cache told judged = %v, want [%v]", rec.cacheJudged, tt.wantJudged)
			}
			if (len(rec.followers) == 1) != tt.wantFollowers || len(rec.followers) > 1 {
				t.Fatalf("followers told = %v, want told: %v", rec.followers, tt.wantFollowers)
			}
			if !slices.Equal(rec.queue.told, tt.wantQueue) {
				t.Fatalf("queue told = %v, want %v", rec.queue.told, tt.wantQueue)
			}
			if (len(rec.answered) == 1) != tt.wantAnswered || len(rec.answered) > 1 {
				t.Fatalf("deploy check answers = %v, want answered: %v", rec.answered, tt.wantAnswered)
			}
		})
	}
}

func TestColdGateRaiseSinksRaisedBeforeTheQueueIsWiredTellsTheRest(t *testing.T) {
	// Arrange.
	sinks, rec := sinksFor(nil)
	sinks.queue = func() coldGateQueue { return nil }

	// Act.
	sinks.raised("ws-1", workspace.RaiseJudged)

	// Assert.
	if len(rec.cacheJudged) != 1 || len(rec.answered) != 1 {
		t.Fatalf("cache told %v, answered %v; want each told once", rec.cacheJudged, rec.answered)
	}
}

func TestColdGateRaiseSinksRecordsAFailedDeployCheckAnswer(t *testing.T) {
	// Arrange.
	sinks, rec := sinksFor(errors.New("not owned"))

	// Act.
	sinks.raised("ws-1", workspace.RaiseJudged)

	// Assert.
	r, ok := dlog.FindRecord(rec.log.Records(), dlog.RecordQuery{
		Level: "error", Operation: "daemon.workspace.deploy_checks", Message: "the cold gate on a standing deploy check could not be answered"})
	if !ok || r.Context["workspace"] != "ws-1" || r.Context["cause"] != "not owned" {
		t.Fatalf("no error record naming the workspace and cause: %v", rec.log.Records())
	}
}
