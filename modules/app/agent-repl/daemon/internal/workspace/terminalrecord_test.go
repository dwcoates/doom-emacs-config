package workspace

import (
	"context"
	"errors"
	"fmt"
	"strings"
	"testing"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/shimlife"
	"agent-repld/internal/wsm"
)

// retireFixture is one fake store plus the logger the retirement records into.
func retireFixture(t *testing.T) (*fakeDB, dlog.Logger, *dlog.TestSurfaces) {
	t.Helper()
	surfaces := dlog.NewTestSurfaces()
	return newFakeDB(), surfaces.Global(), surfaces
}

func TestRetireTerminalRecordClearsTheCause(t *testing.T) {
	// Arrange.
	db, log, _ := retireFixture(t)
	db.sessions["w1"] = wsm.Session{Workspace: "w1", HostSessionID: "host-1"}
	if err := db.SetSessionTerminal(context.Background(), "w1", wsm.SessionTerminal{Kind: "killed", At: fixedNow}); err != nil {
		t.Fatalf("SetSessionTerminal: %v", err)
	}

	// Act.
	if err := retireTerminalRecord(context.Background(), log, db, opBringUp, "w1"); err != nil {
		t.Fatalf("retireTerminalRecord: %v", err)
	}

	// Assert.
	if terminal := db.sessions["w1"].Terminal; terminal != nil {
		t.Fatalf("terminal = %+v, want it retired", terminal)
	}
}

func TestRetireTerminalRecordAcceptsADeletedSession(t *testing.T) {
	// Arrange: a deleted session is not resurrected, and refusing to
	// resurrect it is an outcome rather than a failure — the live client is
	// not evidence against the deletion.
	db, log, _ := retireFixture(t)
	db.sessions["w1"] = wsm.Session{Workspace: "w1", HostSessionID: "host-1"}
	if err := db.SetSessionTerminal(context.Background(), "w1", wsm.SessionTerminal{Kind: "deleted", At: fixedNow}); err != nil {
		t.Fatalf("SetSessionTerminal: %v", err)
	}

	// Act.
	err := retireTerminalRecord(context.Background(), log, db, opBringUp, "w1")

	// Assert.
	if err != nil {
		t.Fatalf("retireTerminalRecord = %v, want the deletion accepted", err)
	}
}

func TestRetireTerminalRecordKeepsADeletedSessionsCause(t *testing.T) {
	// Arrange.
	db, log, _ := retireFixture(t)
	db.sessions["w1"] = wsm.Session{Workspace: "w1", HostSessionID: "host-1"}
	if err := db.SetSessionTerminal(context.Background(), "w1", wsm.SessionTerminal{Kind: "deleted", At: fixedNow}); err != nil {
		t.Fatalf("SetSessionTerminal: %v", err)
	}

	// Act.
	if err := retireTerminalRecord(context.Background(), log, db, opBringUp, "w1"); err != nil {
		t.Fatalf("retireTerminalRecord: %v", err)
	}

	// Assert.
	terminal := db.sessions["w1"].Terminal
	if terminal == nil || terminal.Kind != "deleted" {
		t.Fatalf("terminal = %+v, want the deletion kept", terminal)
	}
}

func TestRetireTerminalRecordSurfacesAStoreFailure(t *testing.T) {
	// Arrange.
	db, log, _ := retireFixture(t)
	db.clearTerminalErr = errors.New("the store is unreadable")

	// Act.
	err := retireTerminalRecord(context.Background(), log, db, opBringUp, ids.WorkspaceID("w1"))

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "the store is unreadable") {
		t.Fatalf("retireTerminalRecord = %v, want the store failure surfaced", err)
	}
}

func TestRetireTerminalRecordRecordsAStoreFailureAtError(t *testing.T) {
	// Arrange: a record that will go on calling a serving session dead is
	// never a silent outcome.
	db, log, surfaces := retireFixture(t)
	db.clearTerminalErr = errors.New("the store is unreadable")

	// Act.
	_ = retireTerminalRecord(context.Background(), log, db, opBringUp, "w1")

	// Assert.
	found := false
	for _, r := range surfaces.Records() {
		if r.Level == "error" && r.Operation == opBringUp {
			found = true
		}
	}
	if !found {
		t.Fatalf("records = %v, want the failure at error", surfaces.Records())
	}
}

func TestShimEndTerminalNamesWhatEachStopRecords(t *testing.T) {
	cases := []struct {
		reason shimlife.StandDownReason
		kind   string
		record bool
	}{
		{shimlife.StandDownShutdown, "", false},
		{shimlife.StandDownHandover, "", false},
		{shimlife.StandDownKill, "killed", true},
		{shimlife.StandDownKeepaliveCycle, wsm.TerminalParked, true},
		{shimlife.StandDownHibernate, wsm.TerminalParked, true},
		{shimlife.StandDownColdGate, wsm.TerminalParked, true},
		{shimlife.StandDownReplaced, wsm.TerminalParked, true},
		{shimlife.StandDownRebind, wsm.TerminalParked, true},
		{shimlife.StandDownUnstated, wsm.TerminalParked, true},
	}
	for _, tc := range cases {
		t.Run(string(tc.reason), func(t *testing.T) {
			// Act
			kind, record := shimEndTerminal(tc.reason)

			// Assert
			if kind != tc.kind || record != tc.record {
				t.Fatalf("shimEndTerminal(%s) = %q, %v; want %q, %v", tc.reason, kind, record, tc.kind, tc.record)
			}
		})
	}
}

func TestRecordShimEndedWritesTheTerminal(t *testing.T) {
	// Arrange
	db, log, surfaces := retireFixture(t)
	db.sessions["w1"] = wsm.Session{Workspace: "w1", VendorSessionID: "vendor-1"}

	// Act
	err := recordShimEnded(context.Background(), log, db, opBringUp, "w1", wsm.TerminalParked, "keepalive_cycle: stood down", fixedNow)

	// Assert
	if err != nil {
		t.Fatalf("recordShimEnded: %v", err)
	}
	terminal := db.sessions["w1"].Terminal
	if terminal == nil || terminal.Kind != wsm.TerminalParked || terminal.Detail != "keepalive_cycle: stood down" || !terminal.At.Equal(fixedNow) {
		t.Fatalf("terminal = %+v, want the park with its detail and instant", terminal)
	}
	rec, ok := dlog.FindRecord(surfaces.Records(), dlog.RecordQuery{Level: dlog.LevelInfo, Operation: opBringUp,
		Message: "recorded the end of the workspace's shim on its session record"})
	if !ok || rec.Context["terminal"] != wsm.TerminalParked || rec.Context["terminal_detail"] != "keepalive_cycle: stood down" {
		t.Fatalf("records = %v, want the end recorded at INFO with its terminal", surfaces.Records())
	}
}

func TestRecordShimEndedAcceptsAnOrdinaryAbsence(t *testing.T) {
	cases := []struct {
		name string
		err  error
	}{
		{name: "no session record", err: fmt.Errorf("session: %w", wsm.ErrNotFound)},
		{name: "a deleted session", err: fmt.Errorf("session: %w", wsm.ErrSessionDeleted)},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			db, log, surfaces := retireFixture(t)
			db.setTerminalErr = tc.err

			// Act
			err := recordShimEnded(context.Background(), log, db, opBringUp, "w1", wsm.TerminalParked, "detail", fixedNow)

			// Assert
			if err != nil {
				t.Fatalf("recordShimEnded = %v, want the absence accepted", err)
			}
			if errs := dlog.FilterRecords(surfaces.Records(), dlog.RecordQuery{Level: dlog.LevelError}); len(errs) != 0 {
				t.Fatalf("errors = %v, want none", errs)
			}
		})
	}
}

func TestRecordShimEndedSurfacesAStoreFailureAtError(t *testing.T) {
	// Arrange
	db, log, surfaces := retireFixture(t)
	db.setTerminalErr = errors.New("the store is unreadable")

	// Act
	err := recordShimEnded(context.Background(), log, db, opBringUp, "w1", wsm.TerminalParked, "detail", fixedNow)

	// Assert
	if err == nil || !strings.Contains(err.Error(), "the store is unreadable") {
		t.Fatalf("recordShimEnded = %v, want the store failure surfaced", err)
	}
	rec, ok := dlog.FindRecord(surfaces.Records(), dlog.RecordQuery{Level: dlog.LevelError, Operation: opBringUp,
		Message: "could not record the end of the workspace's shim"})
	if !ok || rec.Context["cause"] == nil || rec.Context["terminal"] != wsm.TerminalParked {
		t.Fatalf("records = %v, want the failure at ERROR with its cause and terminal", surfaces.Records())
	}
}
