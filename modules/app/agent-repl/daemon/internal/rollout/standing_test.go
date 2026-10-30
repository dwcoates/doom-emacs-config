package rollout

import (
	"testing"

	"agent-repld/internal/ids"
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

// TestIncumbentAddressNamesTheDaemonServingWhatIsNotAdopted covers the one
// field the not_yet_adopted arm carries: the incumbent's address while this
// daemon joins, and none once it has taken over or never joined.
func TestIncumbentAddressNamesTheDaemonServingWhatIsNotAdopted(t *testing.T) {
	tests := []struct {
		name        string
		incumbent   string
		joiningMode bool
		want        string
	}{
		{name: "a joining daemon names its incumbent", incumbent: "127.0.0.1:6666", joiningMode: true, want: "127.0.0.1:6666"},
		{name: "a daemon whose incumbent exited names none", incumbent: "127.0.0.1:6666", joiningMode: false, want: ""},
		{name: "a daemon that never joined names none", incumbent: "", joiningMode: false, want: ""},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t, func(d *Deps) { d.IncumbentAddress = tc.incumbent })
			h.c.mu.Lock()
			h.c.joiningMode = tc.joiningMode
			h.c.mu.Unlock()

			// Act
			got := h.c.IncumbentAddress()

			// Assert
			if got != tc.want {
				t.Fatalf("IncumbentAddress = %q, want %q", got, tc.want)
			}
		})
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

// TestStandingOnAJoiningSuccessorDerivesFromTheManifestAndItsOwnCreates
// covers the 2026-10-30 17:41-17:54 stranding: a successor answered
// not_yet_adopted for workspaces it had created itself. Each case is one way
// a joining daemon comes to know a workspace.
func TestStandingOnAJoiningSuccessorDerivesFromTheManifestAndItsOwnCreates(t *testing.T) {
	manifested := ids.WorkspaceID("ws-manifested")
	created := ids.WorkspaceID("ws-created-here")
	unknown := ids.WorkspaceID("ws-unknown")
	tests := []struct {
		name         string
		manifestSeen bool
		ws           ids.WorkspaceID
		createIt     bool
		adoptIt      bool
		want         Standing
	}{
		{name: "a workspace it created while joining is owned", manifestSeen: true, ws: created, createIt: true, want: StandingOwned},
		{name: "a workspace it created before the manifest was read is owned", ws: created, createIt: true, want: StandingOwned},
		{name: "a manifested workspace is not yet adopted until it is adopted", manifestSeen: true, ws: manifested, want: StandingNotYetAdopted},
		{name: "a manifested workspace is owned once adopted", manifestSeen: true, ws: manifested, adoptIt: true, want: StandingOwned},
		{name: "a workspace the read manifest does not name is not yet adopted while joining", manifestSeen: true, ws: unknown, want: StandingNotYetAdopted},
		{name: "an unknown workspace before the manifest is read is not yet adopted", ws: unknown, want: StandingNotYetAdopted},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			h.c.mu.Lock()
			h.c.enterJoiningLocked()
			h.c.manifestSeen = tc.manifestSeen
			h.c.joining = map[ids.WorkspaceID]bool{manifested: true}
			h.c.owned = map[ids.WorkspaceID]bool{manifested: tc.adoptIt}
			h.c.mu.Unlock()
			if tc.createIt {
				h.c.Created(tc.ws)
			}

			// Act
			standing := h.c.Standing(tc.ws)

			// Assert
			if standing != tc.want {
				t.Fatalf("Standing(%s) = %v, want %v", tc.ws, standing, tc.want)
			}
		})
	}
}

// TestCreatedRecordsTheOwnedTransitionOnce covers the canonical record a
// creation leaves: one serving_standing transition, not one per call.
func TestCreatedRecordsTheOwnedTransitionOnce(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws := ids.WorkspaceID("ws-created-here")

	// Act
	h.c.Created(ws)
	h.c.Created(ws)

	// Assert
	var transitions int
	for _, rec := range records(h.log, opJoin) {
		if rec.Context["workspace"] == string(ws) && rec.Context["created_here"] == true {
			transitions++
		}
	}
	if transitions != 1 {
		t.Fatalf("created_here transitions = %d, want 1", transitions)
	}
}

// TestStandingOnAnOutgoingDaemonPointsTheSuccessorsCreatesToTheSuccessor
// covers the outgoing half of the stranding: once a handover fixed its
// served set, a workspace outside it (the successor created it) is the
// successor's, and every rpc here is pointed there.
func TestStandingOnAnOutgoingDaemonPointsTheSuccessorsCreatesToTheSuccessor(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.workspace(t)
	if err := runHandover(t, h, 1); err != nil {
		t.Fatalf("Handover: %v", err)
	}
	minted := ids.WorkspaceID("ws-the-successor-created")

	// Act
	standing := h.c.Standing(minted)

	// Assert
	if standing != StandingTransferringAway {
		t.Fatalf("Standing = %v for a workspace outside the handover's served set, want transferring_away", standing)
	}
	if got := h.c.SuccessorAddress(); got != "127.0.0.1:7788" {
		t.Fatalf("SuccessorAddress = %q, want the successor's address", got)
	}
}

// TestFixServedRecordsTheServedSet covers the handover's canonical record of
// the set it fixed.
func TestFixServedRecordsTheServedSet(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.workspace(t)

	// Act
	if err := runHandover(t, h, 1); err != nil {
		t.Fatalf("Handover: %v", err)
	}

	// Assert
	var found bool
	for _, rec := range levelRecords(records(h.log, opHandover), "info") {
		if rec.Message == "fixed the handover's served set; any other workspace is the successor's" {
			found = true
			if rec.Context["served"] != 1 {
				t.Fatalf("served = %v, want 1", rec.Context["served"])
			}
		}
	}
	if !found {
		t.Fatal("no record of the fixed served set")
	}
}
