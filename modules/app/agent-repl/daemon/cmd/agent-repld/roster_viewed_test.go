package main

import (
	"context"
	"errors"
	"slices"
	"testing"

	"agent-repld/internal/dlog"
	"agent-repld/internal/wsm"
)

// fakeViewedWriter records SetViewedStatus calls and answers err.
type fakeViewedWriter struct {
	calls []string
	err   error
}

func (f *fakeViewedWriter) SetViewedStatus(_ context.Context, _ wsm.WorkspaceID, arm string) error {
	f.calls = append(f.calls, arm)
	return f.err
}

func TestRosterViewedWritesEachReportedFact(t *testing.T) {
	tests := []struct {
		name string
		arm  string
	}{
		{name: "a viewed arm", arm: "thinking"},
		{name: "a cleared fact", arm: ""},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			db := &fakeViewedWriter{}
			log := dlog.NewTestLogger()

			// Act.
			rosterViewed(db, log)("ws-1", tt.arm)

			// Assert.
			if !slices.Equal(db.calls, []string{tt.arm}) {
				t.Fatalf("SetViewedStatus calls = %q, want exactly the reported fact", db.calls)
			}
			for _, r := range log.Records() {
				if r.Level == dlog.LevelError {
					t.Fatalf("records = %+v, want no error for a written fact", log.Records())
				}
			}
		})
	}
}

func TestRosterViewedRecordsAFailedWriteAtError(t *testing.T) {
	// Arrange.
	db := &fakeViewedWriter{err: errors.New("disk full")}
	log := dlog.NewTestLogger()

	// Act.
	rosterViewed(db, log)("ws-1", "done")

	// Assert.
	for _, r := range log.Records() {
		if r.Level == dlog.LevelError && r.Operation == "daemon.cmd.roster_viewed" {
			if r.Context["workspace"] != "ws-1" || r.Context["cause"] != "disk full" || r.Context["viewed_status"] != "done" {
				t.Fatalf("record context = %v, want the workspace, the cause and the fact", r.Context)
			}
			return
		}
	}
	t.Fatalf("records = %+v, want the failed write at ERROR", log.Records())
}
