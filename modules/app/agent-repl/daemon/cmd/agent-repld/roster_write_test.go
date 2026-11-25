package main

import (
	"context"
	"errors"
	"fmt"
	"reflect"
	"testing"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
	"agentrepl/logging/logsite"
)

// recordedWrite is one call rosterWrite's write was given.
type recordedWrite struct {
	ws ids.WorkspaceID
	v  int
}

func TestRosterWriteRecordsAWriteForAForgottenWorkspaceAtInfo(t *testing.T) {
	// Arrange
	log := dlog.NewTestLogger()
	sink := rosterWrite(log, "daemon.cmd.test_write", logsite.Msg("could not keep the test value durable"),
		func(context.Context, ids.WorkspaceID, int) error {
			return fmt.Errorf("wsm: workspace ws-1: %w", wsm.ErrNotFound)
		},
		func(v int) dlog.Context { return dlog.Context{"value": v} })

	// Act
	sink("ws-1", 7)

	// Assert
	if _, found := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: dlog.LevelError}); found {
		t.Fatalf("records = %+v, want no ERROR for a forgotten workspace", log.Records())
	}
	record, found := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: dlog.LevelInfo, Operation: "daemon.cmd.test_write"})
	if !found || record.Context["workspace"] != "ws-1" || record.Context["value"] != 7 || record.Context["cause"] == nil {
		t.Fatalf("records = %+v, want an INFO record naming the workspace, the cause and the change", log.Records())
	}
}

func TestRosterWrite(t *testing.T) {
	tests := []struct {
		name      string
		err       error
		wantError bool
	}{
		{name: "a written change records nothing", err: nil, wantError: false},
		{name: "a failed write is recorded at ERROR", err: errors.New("disk full"), wantError: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			log := dlog.NewTestLogger()
			var writes []recordedWrite
			sink := rosterWrite(log, "daemon.cmd.test_write", logsite.Msg("could not keep the test value durable"),
				func(_ context.Context, ws ids.WorkspaceID, v int) error {
					writes = append(writes, recordedWrite{ws: ws, v: v})
					return tt.err
				},
				func(v int) dlog.Context { return dlog.Context{"value": v} })

			// Act
			sink("ws-1", 7)

			// Assert
			if !reflect.DeepEqual(writes, []recordedWrite{{ws: "ws-1", v: 7}}) {
				t.Fatalf("writes = %v, want exactly the change", writes)
			}
			record, found := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: dlog.LevelError, Operation: "daemon.cmd.test_write"})
			if found != tt.wantError {
				t.Fatalf("ERROR record found = %v, want %v (records %+v)", found, tt.wantError, log.Records())
			}
			if tt.wantError && (record.Message != "could not keep the test value durable" ||
				record.Context["workspace"] != "ws-1" || record.Context["cause"] != "disk full" || record.Context["value"] != 7) {
				t.Fatalf("record = %+v, want the message, the workspace, the cause and the change's fields", record)
			}
		})
	}
}
