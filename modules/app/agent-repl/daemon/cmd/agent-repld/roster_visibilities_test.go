package main

import (
	"context"
	"errors"
	"reflect"
	"testing"

	"agent-repld/internal/dlog"
	"agent-repld/internal/wsm"
)

// fakeVisibilityWriter records SetVisibilities calls and answers err.
type fakeVisibilityWriter struct {
	calls []map[wsm.WorkspaceID]wsm.Visibility
	err   error
}

func (f *fakeVisibilityWriter) SetVisibilities(_ context.Context, changes map[wsm.WorkspaceID]wsm.Visibility) error {
	f.calls = append(f.calls, changes)
	return f.err
}

func TestRosterVisibilityWritesEachReportedChange(t *testing.T) {
	// Arrange.
	db := &fakeVisibilityWriter{}
	log := dlog.NewTestLogger()

	// Act.
	rosterVisibility(db, log)("ws-1", wsm.NotVisible)

	// Assert.
	want := []map[wsm.WorkspaceID]wsm.Visibility{{"ws-1": wsm.NotVisible}}
	if !reflect.DeepEqual(db.calls, want) {
		t.Fatalf("SetVisibilities calls = %v, want exactly the reported visibility", db.calls)
	}
	for _, r := range log.Records() {
		if r.Level == dlog.LevelError {
			t.Fatalf("records = %+v, want no error for a written change", log.Records())
		}
	}
}

func TestRosterVisibilityRecordsAFailedWriteAtError(t *testing.T) {
	// Arrange.
	db := &fakeVisibilityWriter{err: errors.New("disk full")}
	log := dlog.NewTestLogger()

	// Act.
	rosterVisibility(db, log)("ws-1", wsm.Selected)

	// Assert.
	record, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: dlog.LevelError, Operation: "daemon.cmd.roster_visibilities"})
	if !ok {
		t.Fatalf("records = %+v, want the failed write at ERROR", log.Records())
	}
	if record.Context["workspace"] != "ws-1" || record.Context["cause"] != "disk full" || record.Context["visibility"] != wsm.Selected.String() {
		t.Fatalf("record context = %v, want the workspace, the visibility and the cause", record.Context)
	}
}
