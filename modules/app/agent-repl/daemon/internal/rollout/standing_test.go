package rollout

import (
	"testing"

	"claude-repld/internal/ids"
)

// TestStandingIsOwnedBeforeAnyHandover covers the ordinary case: a daemon that
// transferred nothing and is not joining serves every workspace it knows, and
// refusing on ignorance would refuse every ordinary rpc.
func TestStandingIsOwnedBeforeAnyHandover(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)

	// Act
	standing := h.c.Standing(ws)

	// Assert
	if standing != StandingOwned {
		t.Fatalf("Standing = %v before any handover, want owned", standing)
	}
}

// TestStandingIsTransferringAwayAfterTheTransferPush covers the outgoing
// half: from the transfer notice on, this daemon serves nothing for the
// workspace and the rpcs must say where it went.
func TestStandingIsTransferringAwayAfterTheTransferPush(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)

	// Act
	if err := runHandover(t, h, 1); err != nil {
		t.Fatalf("Handover: %v", err)
	}

	// Assert
	if standing := h.c.Standing(ws); standing != StandingTransferringAway {
		t.Fatalf("Standing = %v after the transfer, want transferring_away", standing)
	}
}

// TestSuccessorAddressIsTheSpawnedDaemonsAddress covers the one field the
// transferring_away arm carries.
func TestSuccessorAddressIsTheSpawnedDaemonsAddress(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.workspace(t)

	// Act
	if err := runHandover(t, h, 1); err != nil {
		t.Fatalf("Handover: %v", err)
	}

	// Assert
	if got := h.c.SuccessorAddress(); got != "127.0.0.1:7788" {
		t.Fatalf("SuccessorAddress = %q, want the spawned successor's address", got)
	}
}

// TestSuccessorAddressIsEmptyWithNoHandoverInFlight covers the arm's absent
// case: nothing moved, so there is no address to name.
func TestSuccessorAddressIsEmptyWithNoHandoverInFlight(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	got := h.c.SuccessorAddress()

	// Assert
	if got != "" {
		t.Fatalf("SuccessorAddress = %q with no handover in flight, want empty", got)
	}
}

// TestStandingIsNotYetAdoptedForAJoiningDaemonsManifestedWorkspace covers the
// incoming half: a successor knows the workspace from the intent manifest but
// must refuse for it until it has actually adopted it.
func TestStandingIsNotYetAdoptedForAJoiningDaemonsManifestedWorkspace(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws := ids.WorkspaceID("ws-manifested")
	h.c.mu.Lock()
	h.c.joining = map[ids.WorkspaceID]bool{ws: true}
	h.c.mu.Unlock()

	// Act
	standing := h.c.Standing(ws)

	// Assert
	if standing != StandingNotYetAdopted {
		t.Fatalf("Standing = %v for a manifested but unadopted workspace, want not_yet_adopted", standing)
	}
}

// TestStandingIsOwnedOnceAJoiningDaemonHasAdopted covers the standing's end:
// adoption is what makes the successor the server.
func TestStandingIsOwnedOnceAJoiningDaemonHasAdopted(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws := ids.WorkspaceID("ws-manifested")
	h.c.mu.Lock()
	h.c.joining = map[ids.WorkspaceID]bool{ws: true}
	h.c.owned = map[ids.WorkspaceID]bool{ws: true}
	h.c.mu.Unlock()

	// Act
	standing := h.c.Standing(ws)

	// Assert
	if standing != StandingOwned {
		t.Fatalf("Standing = %v after adoption, want owned", standing)
	}
}

func TestServesIntakeOnlyWhileNeitherJoiningNorHandingOver(t *testing.T) {
	ws := ids.WorkspaceID("ws-handed-over")
	tests := []struct {
		name        string
		joiningMode bool
		owned       bool
		handingOver bool
		want        bool
	}{
		{name: "an incumbent with no handover in flight serves", want: true},
		{name: "an incumbent whose handover has begun does not", handingOver: true},
		{name: "a successor still joining does not", joiningMode: true},
		{name: "a successor that owns everything it was handed serves", joiningMode: true, owned: true, want: true},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			h.c.mu.Lock()
			h.c.joiningMode, h.c.manifestSeen = tc.joiningMode, tc.joiningMode
			if tc.joiningMode {
				h.c.joining = map[ids.WorkspaceID]bool{ws: true}
				h.c.owned = map[ids.WorkspaceID]bool{ws: tc.owned}
			}
			h.c.mu.Unlock()
			if tc.handingOver {
				if _, err := h.c.claimHandover(); err != nil {
					t.Fatalf("claimHandover: %v", err)
				}
			}

			// Act
			got := h.c.ServesIntake()

			// Assert
			if got != tc.want {
				t.Fatalf("ServesIntake = %v, want %v", got, tc.want)
			}
		})
	}
}
