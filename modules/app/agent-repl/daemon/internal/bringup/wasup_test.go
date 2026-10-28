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
	err      error
}

func (f fakeSessionList) Sessions(context.Context) ([]wsm.Session, error) { return f.sessions, f.err }

func TestOnlyWasUp(t *testing.T) {
	up := wsm.Session{Workspace: "ws-up"}
	parked := wsm.Session{Workspace: "ws-parked", Terminal: &wsm.SessionTerminal{Kind: wsm.TerminalKeepaliveCycle}}
	hibernated := wsm.Session{Workspace: "ws-hibernated", Terminal: &wsm.SessionTerminal{Kind: wsm.TerminalHibernated}}
	killed := wsm.Session{Workspace: "ws-killed", Terminal: &wsm.SessionTerminal{Kind: "killed"}}
	pending := []wsm.Workspace{{ID: "ws-up"}, {ID: "ws-parked"}, {ID: "ws-hibernated"}, {ID: "ws-killed"}, {ID: "ws-never-started"}}

	// Act
	gotUp, gotLeft, err := OnlyWasUp(context.Background(), fakeSessionList{sessions: []wsm.Session{up, parked, hibernated, killed}}, pending)

	// Assert
	if err != nil {
		t.Fatalf("OnlyWasUp: %v", err)
	}
	if ids := IDs(gotUp); !slices.Equal(ids, []string{"ws-up"}) {
		t.Fatalf("up = %v, want only the session with no terminal", ids)
	}
	if ids := IDs(gotLeft); !slices.Equal(ids, []string{"ws-parked", "ws-hibernated", "ws-killed", "ws-never-started"}) {
		t.Fatalf("left = %v, want the parked, hibernated, killed and never-started workspaces", ids)
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
