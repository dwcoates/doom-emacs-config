package workspace

import (
	"context"
	"errors"
	"testing"

	"claude-repld/internal/sessionlock"
	"claude-repld/internal/shimsocket"
)

// TestStartAdoptsASurvivorTheSocketReachesWhenTheLockReadsFree pins the run-9
// defect at the bring-up: a surviving shim was still bound to the workspace
// socket while the lock read free, the fleet spawned anyway, the newcomer
// could not bind and died, and the dial reached the SURVIVOR — which answered
// StartSession `already_started`.
func TestStartAdoptsASurvivorTheSocketReachesWhenTheLockReadsFree(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.probeState = sessionlock.StateFree
	f.socketState = shimsocket.StateLive

	// Act.
	if err := f.fleet.Start(context.Background(), ws.ID); err != nil {
		t.Fatalf("Start: %v", err)
	}

	// Assert.
	if len(f.supervisor.adopts) != 1 || len(f.supervisor.spawns) != 0 {
		t.Fatalf("bring-ups = %d spawns, %d adopts; want one adopt: a reachable survivor is never spawned over",
			len(f.supervisor.spawns), len(f.supervisor.adopts))
	}
}

// TestStartRefusesWhenTheSocketProbeCouldNotTell pins the refusal: an
// undetermined socket does not answer "is anybody there", and that is never
// grounds to spawn.
func TestStartRefusesWhenTheSocketProbeCouldNotTell(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.probeState = sessionlock.StateFree
	f.socketState, f.socketErr = shimsocket.StateUndetermined, errors.New("permission denied")

	// Act.
	err := f.fleet.Start(context.Background(), ws.ID)

	// Assert.
	if err == nil {
		t.Fatalf("Start = nil, want a refusal when the shim socket could not be probed")
	}
}

// TestStartSpawnsWhenTheSocketProbeCouldNotTellButTheLockIsHeld pins that the
// socket cross-check only ever ADDS a reason not to spawn: a held lock is
// still adopted, whatever the socket said.
func TestStartAdoptsWhenTheLockIsHeldEvenIfTheSocketProbeCouldNotTell(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.probeState = sessionlock.StateHeld
	f.socketState, f.socketErr = shimsocket.StateUndetermined, errors.New("permission denied")

	// Act.
	if err := f.fleet.Start(context.Background(), ws.ID); err != nil {
		t.Fatalf("Start: %v", err)
	}

	// Assert.
	if len(f.supervisor.adopts) != 1 || len(f.supervisor.spawns) != 0 {
		t.Fatalf("bring-ups = %d spawns, %d adopts; want one adopt",
			len(f.supervisor.spawns), len(f.supervisor.adopts))
	}
}

// TestStartSpawnsWhenNothingHoldsAndNothingListens pins the ordinary path: a
// free lock and an absent socket is a workspace with no shim at all.
func TestStartSpawnsWhenNothingHoldsAndNothingListens(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.probeState = sessionlock.StateFree
	f.socketState = shimsocket.StateAbsent

	// Act.
	if err := f.fleet.Start(context.Background(), ws.ID); err != nil {
		t.Fatalf("Start: %v", err)
	}

	// Assert.
	if len(f.supervisor.spawns) != 1 || len(f.supervisor.adopts) != 0 {
		t.Fatalf("bring-ups = %d spawns, %d adopts; want one spawn",
			len(f.supervisor.spawns), len(f.supervisor.adopts))
	}
}
