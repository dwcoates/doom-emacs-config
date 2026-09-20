package workspace

import (
	"context"
	"errors"
	"strings"
	"testing"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/wsm"
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
