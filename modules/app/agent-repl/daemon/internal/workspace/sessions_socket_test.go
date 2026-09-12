package workspace

import (
	"context"
	"errors"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"claude-repld/internal/health"
	"claude-repld/internal/sessionlock"
	"claude-repld/internal/shimsocket"
	"claude-repld/internal/wsm"
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

// TestStartStartsASessionOnTheInertSurvivorWhoseLockReadsFree pins the run-12
// defect: a StartSession that REFUSED (vendor_start_failed) rolls the shim's
// two conversation locks back but leaves the process serving, so the recovery
// bring-up finds a live listener over a free lock. Per shim.md that is an
// INERT shim — spawned, serving, no session — and attaching to it without
// starting one leaves the workspace sessionless, which is what answered the
// recovery prompt `no_session`.
func TestStartStartsASessionOnTheInertSurvivorWhoseLockReadsFree(t *testing.T) {
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
	if len(f.client.requests) != 1 {
		t.Fatalf("StartSession calls = %d, want exactly one: an inert survivor holds no lock and has no session",
			len(f.client.requests))
	}
}

// TestStartDoesNotStartASecondSessionOnAShimHoldingTheLock pins the other side:
// a HELD lock is the shim saying the conversation is already its own, and a
// second StartSession would answer `already_started`.
func TestStartDoesNotStartASecondSessionOnAShimHoldingTheLock(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.probeState = sessionlock.StateHeld
	f.socketState = shimsocket.StateLive

	// Act.
	if err := f.fleet.Start(context.Background(), ws.ID); err != nil {
		t.Fatalf("Start: %v", err)
	}

	// Assert.
	if len(f.client.requests) != 0 {
		t.Fatalf("StartSession calls = %d, want none: an adopted shim is attached to, never started",
			len(f.client.requests))
	}
}

// TestStartRefusesAHeldLockWithNoListener pins the phantom shim's fix. The
// workspace lock is keyed by DIRECTORY and the shim socket by workspace ID, so
// a shim left running for a registry row that has since been forgotten holds
// the directory's lock while the new row's socket was never bound by anybody.
// The bring-up used to choose adopt on the lock alone and spend the whole
// adoption bound dialing that path.
func TestStartRefusesAHeldLockWithNoListener(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.probeState = sessionlock.StateHeld
	f.socketState = shimsocket.StateAbsent

	// Act.
	err := f.fleet.Start(context.Background(), ws.ID)

	// Assert.
	if err == nil {
		t.Fatal("Start = nil, want a refusal: the lock's owner is unreachable")
	}
	if len(f.supervisor.adopts) != 0 {
		t.Fatalf("adoptions = %d, want none: there is nothing listening to adopt", len(f.supervisor.adopts))
	}
}

// TestStartRefusesAHeldLockWithOnlyAStaleSocket is the same refusal for the
// other unreachable shape: a socket FILE outlived the listener that bound it.
func TestStartRefusesAHeldLockWithOnlyAStaleSocket(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.probeState = sessionlock.StateHeld
	f.socketState = shimsocket.StateStale

	// Act.
	err := f.fleet.Start(context.Background(), ws.ID)

	// Assert.
	if err == nil {
		t.Fatal("Start = nil, want a refusal: a stale socket has no listener to adopt")
	}
}

// TestStartSpawnsNoSecondShimOverAHeldLockWithNoListener pins that the refusal
// is a refusal and never a spawn: the lock is exactly what forbids a second
// shim on one conversation.
func TestStartSpawnsNoSecondShimOverAHeldLockWithNoListener(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.probeState = sessionlock.StateHeld
	f.socketState = shimsocket.StateAbsent

	// Act.
	_ = f.fleet.Start(context.Background(), ws.ID)

	// Assert.
	if len(f.supervisor.spawns) != 0 {
		t.Fatalf("spawns = %d, want none over a held lock", len(f.supervisor.spawns))
	}
}

// TestStartRaisesAFaultWhenTheLocksOwnerIsUnreachable pins the user-visible
// half: a bring-up that cannot reach the lock's owner opens the same workspace
// fault a failed spawn does, so the footer says the shim would not come up
// instead of showing nothing at all.
func TestStartRaisesAFaultWhenTheLocksOwnerIsUnreachable(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.probeState = sessionlock.StateHeld
	f.socketState = shimsocket.StateAbsent

	// Act.
	_ = f.fleet.Start(context.Background(), ws.ID)

	// Assert.
	if got := faultKinds(f.db.dbFaults); len(got) != 1 || got[0] != health.KindShimStartFailed {
		t.Fatalf("faults = %v, want exactly one %s", got, health.KindShimStartFailed)
	}
}

// TestStartRaisesAFaultWhenAnAdoptionFails pins the same announcement for the
// other adoption death: an adoption that spends its whole bound used to open
// no fault and state no dead link, so the queue dropped the prompt waiting on
// it while every surface still read idle.
func TestStartRaisesAFaultWhenAnAdoptionFails(t *testing.T) {
	// Arrange.
	f := newFleetFixtureBoundedAt(t, 10*time.Millisecond)
	ws := f.workspace("w1")
	f.probeState = sessionlock.StateHeld
	f.socketState = shimsocket.StateLive
	f.supervisor.adoptBlocks = true

	// Act.
	_ = f.fleet.Start(context.Background(), ws.ID)

	// Assert.
	if got := faultKinds(f.db.dbFaults); len(got) != 1 || got[0] != health.KindShimStartFailed {
		t.Fatalf("faults = %v, want exactly one %s", got, health.KindShimStartFailed)
	}
}

// TestAnUnreachableLockOwnerStandsTheFootersBringUpFailureLine pins the
// owner's ruling of 2026-09-12: a bring-up failure writes no feed row, so the
// footer's own line is the whole account of it and it has to name the cause.
func TestAnUnreachableLockOwnerStandsTheFootersBringUpFailureLine(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.probeState = sessionlock.StateHeld
	f.socketState = shimsocket.StateAbsent

	// Act.
	_ = f.fleet.Start(context.Background(), ws.ID)

	// Assert.
	failure := f.footer.startFailed[ws.ID]
	if failure == nil || !strings.Contains(failure.Detail, "unreachable") {
		t.Fatalf("the footer's bring-up failure = %+v, want the refusal's own words", failure)
	}
}

// TestAFailedAdoptionStandsTheFootersBringUpFailureLine pins the same line for
// the other adoption death.
func TestAFailedAdoptionStandsTheFootersBringUpFailureLine(t *testing.T) {
	// Arrange.
	f := newFleetFixtureBoundedAt(t, 10*time.Millisecond)
	ws := f.workspace("w1")
	f.probeState = sessionlock.StateHeld
	f.socketState = shimsocket.StateLive
	f.supervisor.adoptBlocks = true

	// Act.
	_ = f.fleet.Start(context.Background(), ws.ID)

	// Assert.
	if failure := f.footer.startFailed[ws.ID]; failure == nil || failure.Detail == "" {
		t.Fatalf("the footer's bring-up failure = %+v, want the adoption's own words", failure)
	}
}

// TestStartAdoptsTheNewestLiveSocketGeneration pins the second phantom path: a
// relaunch moves the shim onto `<base>.nN.sock` and the counter that minted N
// lives in the fleet's memory, so a bring-up after a restart dialed the base
// path a survivor has not held since. Boot already resolved this with
// shimsocket.NewestLive; the fleet did not.
func TestStartAdoptsTheNewestLiveSocketGeneration(t *testing.T) {
	// Arrange.
	dir := t.TempDir()
	f := newFleetFixture(t)
	f.socketDir = dir
	ws := f.workspace("w1")
	base := filepath.Join(dir, "w1.sock")
	generation := base[:len(base)-len(".sock")] + ".n1.sock"
	if err := os.WriteFile(generation, nil, 0o600); err != nil {
		t.Fatalf("writing the generation's socket path: %v", err)
	}
	f.probeState = sessionlock.StateHeld
	f.socketState = shimsocket.StateAbsent
	f.socketStates = map[string]shimsocket.State{generation: shimsocket.StateLive}

	// Act.
	if err := f.fleet.Start(context.Background(), ws.ID); err != nil {
		t.Fatalf("Start: %v", err)
	}

	// Assert.
	if len(f.supervisor.adopts) != 1 || f.supervisor.adopts[0] != generation {
		t.Fatalf("adoptions = %+v, want exactly one of %q", f.supervisor.adopts, generation)
	}
}

// faultKinds names the fault kinds a fake registry recorded.
func faultKinds(faults []wsm.Fault) []string {
	out := make([]string, 0, len(faults))
	for _, f := range faults {
		out = append(out, f.Kind)
	}
	return out
}
