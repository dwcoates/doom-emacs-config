package workspace

import (
	"context"
	"errors"
	"testing"
	"time"

	shimv1 "agentrepl/proto/shim/v1"

	"claude-repld/internal/ids"
	"claude-repld/internal/sessionlock"
	"claude-repld/internal/shimsocket"
)

// vendorStartFailed is the shim's own refusal of the start, which is the shape
// the 2026-09-13 duplicate-client defect was measured on: the vendor never
// reached `init`, the shim rolled its conversation locks back, and the process
// kept serving the workspace socket.
func vendorStartFailed() *shimv1.StartSessionResponse {
	return &shimv1.StartSessionResponse{
		Result: &shimv1.StartSessionResponse_Failure{Failure: &shimv1.StartSessionFailure{
			Cause:  &shimv1.StartSessionFailure_VendorStartFailed{VendorStartFailed: rejectedVendorStart()},
			Detail: "the vendor did not answer inside the init bound",
		}},
	}
}

// arrangeFailedStart is a fleet whose bring-up SPAWNS -- free lock, nothing
// listening -- and whose StartSession then fails. The socket follows the
// process: the spawn binds it and the stop lets it go, which is what the next
// bring-up's probe reads.
func arrangeFailedStart(t *testing.T) *fleetFixture {
	t.Helper()
	f := newFleetFixture(t)
	f.probeState = sessionlock.StateFree
	f.socketState = shimsocket.StateAbsent
	f.supervisor.onSpawn = func() { f.socketState = shimsocket.StateLive }
	f.client.onKill = func() { f.socketState = shimsocket.StateAbsent }
	return f
}

// TestAFailedStartLeavesNoShimOfItsOwnServing is the invariant. `Fleet.Start`
// returned a refused StartSession BEFORE `f.remember`, so nothing held the
// client -- while the shim kept serving the workspace socket for the next
// bring-up to adopt (2026-09-13T18:17:56, shim pid 48170).
func TestAFailedStartLeavesNoShimOfItsOwnServing(t *testing.T) {
	tests := []struct {
		name     string
		response *shimv1.StartSessionResponse
		startErr error
	}{
		{name: "the shim refuses the start", response: vendorStartFailed()},
		{name: "the shim never answers the start", startErr: errors.New("unavailable: unexpected EOF")},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			f := arrangeFailedStart(t)
			ws := f.workspace("w1")
			f.client.response, f.client.startErr = tt.response, tt.startErr

			// Act.
			err := f.fleet.Start(context.Background(), ws.ID)

			// Assert.
			if err == nil {
				t.Fatal("Start = nil, want the start's own failure")
			}
			if len(f.client.kills) != 1 {
				t.Fatalf("process stops = %d, want exactly one: a failed start stops the shim it spawned", len(f.client.kills))
			}
			if f.socketState != shimsocket.StateAbsent {
				t.Fatalf("socket state = %s, want %s: the stopped shim's socket is gone", f.socketState, shimsocket.StateAbsent)
			}
			if f.fleet.Live(ws.ID) {
				t.Fatal("the fleet remembers a session for a start that failed")
			}
		})
	}
}

// TestAFailedStartReleasesItsSpawnRecord pins the other half of "no shim of its
// own": the supervisor's registry is the ONLY witness of a spawn that never
// reached the session map, and a registry that still named the process would
// refuse the next bring-up's spawn as a duplicate of a shim that is gone.
func TestAFailedStartReleasesItsSpawnRecord(t *testing.T) {
	// Arrange.
	f := arrangeFailedStart(t)
	ws := f.workspace("w1")
	f.client.response = vendorStartFailed()

	// Act.
	_ = f.fleet.Start(context.Background(), ws.ID)

	// Assert.
	if pid, held := f.supervisor.SpawnedFor(ws.ID); held {
		t.Fatalf("the supervisor still holds pid %d for %q after the failed start's stop", pid, ws.ID)
	}
}

// TestTheBringUpAfterAFailedStartSpawnsFresh is the defect's second half: the
// NEXT bring-up found "lock free, socket live" and adopted the very shim this
// daemon had spawned and still supervised (18:18:41). With the failed start's
// shim stopped there is nothing to adopt, and the workspace comes up on a new
// process.
func TestTheBringUpAfterAFailedStartSpawnsFresh(t *testing.T) {
	// Arrange.
	f := arrangeFailedStart(t)
	ws := f.workspace("w1")
	f.client.response = vendorStartFailed()
	if err := f.fleet.Start(context.Background(), ws.ID); err == nil {
		t.Fatal("the arranged start did not fail")
	}
	f.client.response = startedResponse("vendor-1")

	// Act.
	if err := f.fleet.Start(context.Background(), ws.ID); err != nil {
		t.Fatalf("the bring-up after the failed start: %v", err)
	}

	// Assert.
	if len(f.supervisor.spawns) != 2 || len(f.supervisor.adopts) != 0 {
		t.Fatalf("bring-ups = %d spawns, %d adopts; want two spawns and no adoption of our own shim",
			len(f.supervisor.spawns), len(f.supervisor.adopts))
	}
}

// TestAForeignSurvivorIsStillAdopted is the case the guard must not touch: a
// shim this daemon never spawned -- a previous daemon's survivor -- is exactly
// what the adopt path exists for.
func TestAForeignSurvivorIsStillAdopted(t *testing.T) {
	tests := []struct {
		name string
		lock sessionlock.State
	}{
		{name: "a survivor holding the workspace lock", lock: sessionlock.StateHeld},
		{name: "an inert survivor holding no lock", lock: sessionlock.StateFree},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			f := newFleetFixture(t)
			ws := f.workspace("w1")
			f.probeState, f.socketState = tt.lock, shimsocket.StateLive

			// Act.
			err := f.fleet.Start(context.Background(), ws.ID)

			// Assert.
			if err != nil {
				t.Fatalf("Start: %v", err)
			}
			if len(f.supervisor.adopts) != 1 {
				t.Fatalf("adoptions = %d, want one: a foreign survivor is adopted", len(f.supervisor.adopts))
			}
		})
	}
}

// TestTheBringUpRefusesToAdoptAShimItSpawnedItself pins the backstop. One
// process with two clients is a bug in this daemon -- the two disagree about
// the exit, the redial and the lock -- so the bring-up refuses by name rather
// than proceeding.
func TestTheBringUpRefusesToAdoptAShimItSpawnedItself(t *testing.T) {
	tests := []struct {
		name string
		lock sessionlock.State
	}{
		{name: "our own shim holding the workspace lock", lock: sessionlock.StateHeld},
		{name: "our own inert shim holding no lock", lock: sessionlock.StateFree},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange. The supervisor still holds a spawn for this workspace,
			// which is the whole window between cmd.Start and the session map.
			f := newFleetFixture(t)
			ws := f.workspace("w1")
			f.probeState, f.socketState = tt.lock, shimsocket.StateLive
			f.supervisor.spawned = map[ids.WorkspaceID]int{ws.ID: 4242}

			// Act.
			err := f.fleet.Start(context.Background(), ws.ID)

			// Assert.
			if !errors.Is(err, ErrAdoptOwnSpawn) {
				t.Fatalf("Start = %v, want %v", err, ErrAdoptOwnSpawn)
			}
			if len(f.supervisor.adopts) != 0 {
				t.Fatalf("adoptions = %d, want none: this daemon already holds that process", len(f.supervisor.adopts))
			}
		})
	}
}

// TestTheFailedStartsStopArmsTheStandDownBeforeItEndsAnything is the ordered
// path. The latch is what the exit watcher, the redialer and the adopted-death
// witness read: armed after the fact, this daemon's own teardown lands as
// `daemon.shimclient.exit` ERROR "shim died" plus its redial WARNs.
func TestTheFailedStartsStopArmsTheStandDownBeforeItEndsAnything(t *testing.T) {
	tests := []struct {
		name string
		read func(*fakeClient) bool
	}{
		{name: "before the session ask", read: func(c *fakeClient) bool { return c.standDownBeforeKill }},
		{name: "before the process stop", read: func(c *fakeClient) bool { return c.standDownBeforeStop }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			f := arrangeFailedStart(t)
			ws := f.workspace("w1")
			f.client.response = vendorStartFailed()

			// Act.
			_ = f.fleet.Start(context.Background(), ws.ID)

			// Assert.
			if !tt.read(f.client) {
				t.Fatal("the stand-down latch was not armed before the failed start's shim was ended")
			}
		})
	}
}

// TestTheFailedStartsStopIsNotItselfLoud pins the LEVEL. The start's own
// refusal is already recorded and filed as a fault; the stop that follows it is
// the mechanism working, and a `no_session` from a shim whose start just failed
// is the ORDINARY answer rather than a second defect.
func TestTheFailedStartsStopIsNotItselfLoud(t *testing.T) {
	// Arrange.
	f := arrangeFailedStart(t)
	ws := f.workspace("w1")
	f.client.response = vendorStartFailed()
	f.client.killSessionErr = errors.New("no_session: this shim never started one")

	// Act.
	_ = f.fleet.Start(context.Background(), ws.ID)

	// Assert.
	for _, r := range f.log.logger.Records() {
		if r.Level == "warn" && r.Message == "the failed start's KillSession did not answer" {
			t.Fatal("the refused KillSession of a stand-down this daemon ordered was recorded at WARN")
		}
	}
}

// TestTheFailedStartsStopIsRecordedWithItsPidAndItsCause is the record a reader
// needs to correlate the stop against the start it followed -- the pair the
// 18:17:56/18:18:41 shape could only be read from once both pids were named.
func TestTheFailedStartsStopIsRecordedWithItsPidAndItsCause(t *testing.T) {
	// Arrange.
	f := arrangeFailedStart(t)
	ws := f.workspace("w1")
	f.client.response = vendorStartFailed()

	// Act.
	_ = f.fleet.Start(context.Background(), ws.ID)

	// Assert.
	var stop map[string]any
	for _, r := range f.log.logger.Records() {
		if r.Message == "stopped the shim of a failed start" {
			if r.Level != "info" {
				t.Fatalf("the stop was recorded at %q, want info", r.Level)
			}
			stop = r.Context
		}
	}
	if stop == nil {
		t.Fatal("no record says the failed start's shim was stopped")
	}
	if stop["shim_pid"] != f.client.pid {
		t.Fatalf("the stop's shim_pid = %v, want %d", stop["shim_pid"], f.client.pid)
	}
	if cause, _ := stop["cause"].(string); cause == "" {
		t.Fatalf("the stop's cause = %v, want the failure it followed", stop["cause"])
	}
}

// TestTheFailedStartsStopWaitsForTheSocketToGo pins the wait. The socket is the
// fact the NEXT bring-up branches on, so a stop that returned while the path
// was still bound would leave the very adoption this prevents one probe away.
func TestTheFailedStartsStopWaitsForTheSocketToGo(t *testing.T) {
	// Arrange. The shim's socket outlives the kill by one probe.
	f := arrangeFailedStart(t)
	ws := f.workspace("w1")
	f.client.response = vendorStartFailed()
	f.client.onKill = nil
	probes := 0
	f.onSocketProbe = func(string) {
		if f.socketState != shimsocket.StateLive {
			return
		}
		if probes++; probes > 1 {
			f.socketState = shimsocket.StateAbsent
		}
	}

	// Act.
	_ = f.fleet.Start(context.Background(), ws.ID)

	// Assert.
	for _, r := range f.log.logger.Records() {
		if r.Message == "stopped the shim of a failed start" {
			if r.Context["socket_state"] != shimsocket.StateAbsent.String() {
				t.Fatalf("the stop concluded with socket_state = %v, want %s",
					r.Context["socket_state"], shimsocket.StateAbsent)
			}
			return
		}
	}
	t.Fatal("no record says the failed start's shim was stopped once its socket had gone")
}

// TestAFailedStartsShimThatKeepsServingIsLoud is the give-up. A shim still
// bound to the workspace socket after the stop is the invariant BROKEN, and the
// record is the only thing that will ever say so.
func TestAFailedStartsShimThatKeepsServingIsLoud(t *testing.T) {
	// Arrange.
	f := arrangeFailedStart(t)
	ws := f.workspace("w1")
	f.client.response = vendorStartFailed()
	f.client.onKill = nil
	f.fleet.socketGoneBound = 30 * time.Millisecond

	// Act.
	_ = f.fleet.Start(context.Background(), ws.ID)

	// Assert.
	for _, r := range f.log.logger.Records() {
		if r.Message == "the failed start's shim is still reachable on the workspace socket after the stop" {
			if r.Level != "error" {
				t.Fatalf("the leaked shim was recorded at %q, want error", r.Level)
			}
			return
		}
	}
	t.Fatal("a shim still serving after its stop was never recorded")
}
