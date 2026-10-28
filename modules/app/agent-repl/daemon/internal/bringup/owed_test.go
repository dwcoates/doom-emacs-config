package bringup

import (
	"context"
	"errors"
	"slices"
	"testing"

	"agent-repld/internal/restarttiming"
	"agent-repld/internal/shimlife"
	"agent-repld/internal/wsm"
)

type fakeOwed struct {
	owed map[wsm.WorkspaceID][]wsm.TurnID
	err  error
}

func (f fakeOwed) OwedTurns(_ context.Context, ws wsm.WorkspaceID) ([]wsm.TurnID, error) {
	return f.owed[ws], f.err
}

func TestOnlyOwed(t *testing.T) {
	// Arrange
	db := fakeOwed{owed: map[wsm.WorkspaceID][]wsm.TurnID{"ws-cut": {"turn-1"}}}
	left := []wsm.Workspace{{ID: "ws-quiet"}, {ID: "ws-cut"}}

	// Act
	owed, rest, err := OnlyOwed(context.Background(), db, left)

	// Assert
	if err != nil {
		t.Fatalf("OnlyOwed: %v", err)
	}
	if ids := IDs(owed); !slices.Equal(ids, []string{"ws-cut"}) {
		t.Fatalf("owed = %v, want the workspace owed a cut turn", ids)
	}
	if ids := IDs(rest); !slices.Equal(ids, []string{"ws-quiet"}) {
		t.Fatalf("rest = %v, want the workspace owed nothing", ids)
	}
}

func TestOnlyOwedSplitsNothingWhenATurnReadFails(t *testing.T) {
	// Act
	owed, rest, err := OnlyOwed(context.Background(), fakeOwed{err: errors.New("locked")}, []wsm.Workspace{{ID: "ws-1"}})

	// Assert
	if err == nil || owed != nil || rest != nil {
		t.Fatalf("OnlyOwed = %v, %v, %v; want the failure and nothing split", owed, rest, err)
	}
}

func TestStartCauseNamesTheBringUp(t *testing.T) {
	tests := []struct {
		name string
		via  restarttiming.StartVia
		want shimlife.StartCause
	}{
		{name: "a boot's", via: restarttiming.StartBoot, want: shimlife.StartBoot},
		{name: "a takeover's", via: restarttiming.StartTakeover, want: shimlife.StartTakeover},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := startCause(tt.via)

			// Assert
			if got != tt.want {
				t.Fatalf("startCause = %q, want %q", got, tt.want)
			}
		})
	}
}
