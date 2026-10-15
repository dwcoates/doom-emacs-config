package main

import (
	"context"
	"errors"
	"testing"
	"time"

	"agent-repld/internal/dlog"
	"agent-repld/internal/wsm"
)

// fakeStatusChangeWriter records SetStatusChangedAt calls and answers err.
type fakeStatusChangeWriter struct {
	calls []time.Time
	err   error
}

func (f *fakeStatusChangeWriter) SetStatusChangedAt(_ context.Context, _ wsm.WorkspaceID, at time.Time) error {
	f.calls = append(f.calls, at)
	return f.err
}

var statusChangeInstant = time.Date(2026, 10, 14, 9, 0, 0, 0, time.UTC)

func TestRosterStatusChangesWritesEachReportedChange(t *testing.T) {
	// Arrange.
	db := &fakeStatusChangeWriter{}
	log := dlog.NewTestLogger()

	// Act.
	rosterStatusChanges(db, log)("ws-1", statusChangeInstant)

	// Assert.
	if len(db.calls) != 1 || !db.calls[0].Equal(statusChangeInstant) {
		t.Fatalf("SetStatusChangedAt calls = %v, want exactly the reported instant", db.calls)
	}
	for _, r := range log.Records() {
		if r.Level == dlog.LevelError {
			t.Fatalf("records = %+v, want no error for a written change", log.Records())
		}
	}
}

func TestRosterStatusChangesRecordsAFailedWriteAtError(t *testing.T) {
	// Arrange.
	db := &fakeStatusChangeWriter{err: errors.New("disk full")}
	log := dlog.NewTestLogger()

	// Act.
	rosterStatusChanges(db, log)("ws-1", statusChangeInstant)

	// Assert.
	record, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: dlog.LevelError, Operation: "daemon.cmd.roster_status_changes"})
	if !ok {
		t.Fatalf("records = %+v, want the failed write at ERROR", log.Records())
	}
	if record.Context["workspace"] != "ws-1" || record.Context["cause"] != "disk full" || record.Context["status_changed_at"] != statusChangeInstant {
		t.Fatalf("record context = %v, want the workspace, the instant and the cause", record.Context)
	}
}
