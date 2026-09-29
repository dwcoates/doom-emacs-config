package main

import (
	"context"
	"errors"
	"testing"

	"claude-repld/internal/dlog"
	"claude-repld/internal/wsm"
)

// fakeResultWriter records SetResult calls and answers err.
type fakeResultWriter struct {
	calls []*wsm.TurnResult
	err   error
}

func (f *fakeResultWriter) SetResult(_ context.Context, _ wsm.WorkspaceID, result *wsm.TurnResult) error {
	f.calls = append(f.calls, result)
	return f.err
}

func TestRosterResultsWritesEachReportedResult(t *testing.T) {
	tests := []struct {
		name   string
		result *wsm.TurnResult
	}{
		{name: "a result", result: &wsm.TurnResult{End: wsm.TurnResultDone, Read: true}},
		{name: "a cleared result", result: nil},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			db := &fakeResultWriter{}
			log := dlog.NewTestLogger()

			// Act.
			rosterResults(db, log)("ws-1", tt.result)

			// Assert.
			if len(db.calls) != 1 || db.calls[0] != tt.result {
				t.Fatalf("SetResult calls = %v, want exactly the reported result", db.calls)
			}
			for _, r := range log.Records() {
				if r.Level == dlog.LevelError {
					t.Fatalf("records = %+v, want no error for a written result", log.Records())
				}
			}
		})
	}
}

func TestRosterResultsRecordsAFailedWriteAtError(t *testing.T) {
	// Arrange.
	db := &fakeResultWriter{err: errors.New("disk full")}
	log := dlog.NewTestLogger()

	// Act.
	rosterResults(db, log)("ws-1", &wsm.TurnResult{End: wsm.TurnResultInterrupted})

	// Assert.
	for _, r := range log.Records() {
		if r.Level == dlog.LevelError && r.Operation == "daemon.cmd.roster_results" {
			if r.Context["workspace"] != "ws-1" || r.Context["cause"] != "disk full" || r.Context["end"] != "interrupted" {
				t.Fatalf("record context = %v, want the workspace, the cause and the result", r.Context)
			}
			return
		}
	}
	t.Fatalf("records = %+v, want the failed write at ERROR", log.Records())
}
