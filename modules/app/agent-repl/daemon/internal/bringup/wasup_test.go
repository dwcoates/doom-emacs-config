package bringup

import (
	"context"
	"errors"
	"slices"
	"testing"

	"agent-repld/internal/wsm"
)

type fakeSessionList struct {
	sessions []wsm.Session
	gated    []wsm.WorkspaceID
	err      error
	gateErr  error
}

func (f fakeSessionList) Sessions(context.Context) ([]wsm.Session, error) { return f.sessions, f.err }

func (f fakeSessionList) ColdGateWorkspaces(context.Context) ([]wsm.WorkspaceID, error) {
	return f.gated, f.gateErr
}

func TestOnlyWasUp(t *testing.T) {
	up := wsm.Session{Workspace: "ws-up", VendorSessionID: "vendor-up"}
	created := wsm.Session{Workspace: "ws-created"}
	parked := wsm.Session{Workspace: "ws-parked", Terminal: &wsm.SessionTerminal{Kind: wsm.TerminalParked}}
	hibernated := wsm.Session{Workspace: "ws-hibernated", Terminal: &wsm.SessionTerminal{Kind: wsm.TerminalParked}}
	killed := wsm.Session{Workspace: "ws-killed", Terminal: &wsm.SessionTerminal{Kind: "killed"}}
	pending := []wsm.Workspace{{ID: "ws-up"}, {ID: "ws-parked"}, {ID: "ws-hibernated"}, {ID: "ws-killed"}, {ID: "ws-created"}, {ID: "ws-never-started"}}

	// Act
	gotUp, gotLeft, err := OnlyWasUp(context.Background(), fakeSessionList{sessions: []wsm.Session{up, parked, hibernated, killed, created}}, pending)

	// Assert
	if err != nil {
		t.Fatalf("OnlyWasUp: %v", err)
	}
	if ids := IDs(gotUp); !slices.Equal(ids, []string{"ws-up"}) {
		t.Fatalf("up = %v, want only the session with no terminal", ids)
	}
	if ids := IDs(gotLeft); !slices.Equal(ids, []string{"ws-parked", "ws-hibernated", "ws-killed", "ws-created", "ws-never-started"}) {
		t.Fatalf("left = %v, want the parked, hibernated, killed, created-unstarted and never-started workspaces", ids)
	}
}

func TestOnlyWasUpSplitsNothingWhenTheSessionsCannotBeRead(t *testing.T) {
	// Act
	up, left, err := OnlyWasUp(context.Background(), fakeSessionList{err: errors.New("locked")}, []wsm.Workspace{{ID: "ws-1"}})

	// Assert
	if err == nil || up != nil || left != nil {
		t.Fatalf("OnlyWasUp = %v, %v, %v; want the failure and nothing split", up, left, err)
	}
}

func TestOnlyWasUpCountsAWorkspaceAtItsRecordedColdGateAsUp(t *testing.T) {
	// Arrange: a gate stands its shim down, so its session record reads parked or has none at all.
	parkedAtGate := wsm.Session{Workspace: "ws-gated-parked", Terminal: &wsm.SessionTerminal{Kind: wsm.TerminalParked}}
	pending := []wsm.Workspace{{ID: "ws-gated-parked"}, {ID: "ws-gated-no-session"}, {ID: "ws-parked"}}
	lister := fakeSessionList{
		sessions: []wsm.Session{parkedAtGate, {Workspace: "ws-parked", Terminal: &wsm.SessionTerminal{Kind: wsm.TerminalParked}}},
		gated:    []wsm.WorkspaceID{"ws-gated-parked", "ws-gated-no-session"},
	}

	// Act
	gotUp, gotLeft, err := OnlyWasUp(context.Background(), lister, pending)

	// Assert
	if err != nil {
		t.Fatalf("OnlyWasUp: %v", err)
	}
	if ids := IDs(gotUp); !slices.Equal(ids, []string{"ws-gated-parked", "ws-gated-no-session"}) {
		t.Fatalf("up = %v, want both workspaces at a recorded gate", ids)
	}
	if ids := IDs(gotLeft); !slices.Equal(ids, []string{"ws-parked"}) {
		t.Fatalf("left = %v, want only the parked workspace with no gate", ids)
	}
}

func TestOnlyWasUpSplitsNothingWhenTheColdGatesCannotBeRead(t *testing.T) {
	// Act
	up, left, err := OnlyWasUp(context.Background(), fakeSessionList{gateErr: errors.New("locked")}, []wsm.Workspace{{ID: "ws-1"}})

	// Assert
	if err == nil || up != nil || left != nil {
		t.Fatalf("OnlyWasUp = %v, %v, %v; want the failure and nothing split", up, left, err)
	}
}
