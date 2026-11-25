package workspace

import (
	"context"
	"errors"
	"fmt"
	"testing"

	shimv1 "agentrepl/proto/shim/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/sessionlock"
	"agent-repld/internal/shimclient"
	"agent-repld/internal/shimsocket"
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

// A FAILED START KEEPS ITS SHIM HELD, with no session on it, exactly as a cold
// gate keeps its own: the shim serves the workspace's book and the next start
// starts the session on it. Nothing is stopped.
func TestAFailedStartKeepsItsShimHeldWithNoSession(t *testing.T) {
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
			if len(f.client.kills) != 0 {
				t.Fatalf("process stops = %d, want none: a failed start keeps its shim", len(f.client.kills))
			}
			if !f.fleet.Held(ws.ID) || f.fleet.Live(ws.ID) {
				t.Fatalf("held = %t, live = %t; want the shim held with no session", f.fleet.Held(ws.ID), f.fleet.Live(ws.ID))
			}
		})
	}
}

// THE NEXT START REUSES THE HELD SHIM: it is started on, never spawned over and
// never adopted a second time (the one-shim-two-clients regression).
func TestTheStartAfterAFailedStartReusesItsShim(t *testing.T) {
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
		t.Fatalf("the start after the failed start: %v", err)
	}

	// Assert.
	if len(f.supervisor.spawns) != 1 || len(f.supervisor.adopts) != 0 || len(f.client.requests) != 2 {
		t.Fatalf("spawns = %d, adopts = %d, StartSession asks = %d; want one spawn, no adoption, two asks of the one shim",
			len(f.supervisor.spawns), len(f.supervisor.adopts), len(f.client.requests))
	}
	if !f.fleet.Live(ws.ID) {
		t.Fatal("the session the reused shim started is not live")
	}
}

// A HELD SHIM WITH NO SESSION IS NO SESSION to the surfaces that need one: the
// idle sweep does not direct it and the queue does not deliver to it.
func TestAHeldShimWithNoSessionIsNoSession(t *testing.T) {
	tests := []struct {
		name string
		got  func(f *fleetFixture, ws ids.WorkspaceID) bool
	}{
		{"the idle sweep does not serve it", func(f *fleetFixture, ws ids.WorkspaceID) bool { return f.fleet.Serving(ws) }},
		{"the queue delivers nothing to it", func(f *fleetFixture, ws ids.WorkspaceID) bool {
			_, ok := f.fleet.Sender(ws)
			return ok
		}},
		{"a start has work to do on it", func(f *fleetFixture, ws ids.WorkspaceID) bool { return f.fleet.Live(ws) }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			f := arrangeFailedStart(t)
			ws := f.workspace("w1")
			f.client.response = vendorStartFailed()
			_ = f.fleet.Start(context.Background(), ws.ID)

			// Act / Assert.
			if tt.got(f, ws.ID) {
				t.Fatal("a held shim with no session was read as a session")
			}
		})
	}
}

// A TEARDOWN STANDS A HELD SHIM WITH NO SESSION DOWN like any other.
func TestStopStopsAHeldShimWithNoSession(t *testing.T) {
	// Arrange.
	f := arrangeFailedStart(t)
	ws := f.workspace("w1")
	f.client.response = vendorStartFailed()
	_ = f.fleet.Start(context.Background(), ws.ID)

	// Act.
	if err := f.fleet.Stop(context.Background(), ws.ID, true); err != nil {
		t.Fatalf("Stop: %v", err)
	}

	// Assert.
	if len(f.client.kills) != 1 || f.fleet.Held(ws.ID) {
		t.Fatalf("stops = %d, held = %t; want the held shim stopped and gone", len(f.client.kills), f.fleet.Held(ws.ID))
	}
}

// A SHIM IS HELD FROM ITS SPAWN, before StartSession answers: a handover or a
// teardown that lands while the start runs sees it.
func TestAShimIsHeldBeforeItsStartAnswers(t *testing.T) {
	// Arrange.
	f := arrangeFailedStart(t)
	ws := f.workspace("w1")
	held := false
	f.client.onStart = func() { held = f.fleet.Held(ws.ID) }

	// Act.
	if err := f.fleet.Start(context.Background(), ws.ID); err != nil {
		t.Fatalf("Start: %v", err)
	}

	// Assert.
	if !held {
		t.Fatal("the shim was not held while its StartSession ran")
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

// A FAILED StartSession IS RECORDED BY ITS CAUSE: a stand-down this daemon
// ordered and a bring-up whose caller went away are stated at INFO, and only a
// start that failed on its own is an ERROR. The shim's own cancellation, with
// the bring-up's context still live, is a failure of the start.
func TestAFailedStartSessionIsRecordedByItsCause(t *testing.T) {
	const (
		standDownMessage = "the StartSession call ended in a stand-down this daemon ordered"
		abandonedMessage = "the StartSession call was abandoned: the bring-up's caller went away before the shim answered"
		failedMessage    = "the StartSession call failed"
	)
	tests := []struct {
		name        string
		startErr    error
		cancelCall  bool
		wantLevel   string
		wantMessage string
		wantIs      error
	}{
		{
			name:        "a stand-down this daemon ordered",
			startErr:    fmt.Errorf("%w: unavailable: unexpected EOF", shimclient.ErrStandDownOrdered),
			wantLevel:   "info",
			wantMessage: standDownMessage,
			wantIs:      shimclient.ErrStandDownOrdered,
		},
		{
			name:        "the bring-up's caller went away before the shim answered",
			startErr:    fmt.Errorf("canceled: %w", context.Canceled),
			cancelCall:  true,
			wantLevel:   "info",
			wantMessage: abandonedMessage,
			wantIs:      context.Canceled,
		},
		{
			name:        "the shim's own cancellation with the bring-up's context live",
			startErr:    fmt.Errorf("canceled: %w", context.Canceled),
			wantLevel:   "error",
			wantMessage: failedMessage,
			wantIs:      context.Canceled,
		},
		{
			name:        "an unrelated error",
			startErr:    errors.New("unavailable: the shim hung up"),
			wantLevel:   "error",
			wantMessage: failedMessage,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			f := arrangeFailedStart(t)
			ws := f.workspace("w1")
			ctx, cancel := context.WithCancel(context.Background())
			defer cancel()
			f.client.startErr = tt.startErr
			if tt.cancelCall {
				f.client.onStart = cancel
			}

			// Act.
			err := f.fleet.Start(ctx, ws.ID)

			// Assert.
			if err == nil {
				t.Fatal("Start = nil, want the start's own failure")
			}
			if tt.wantIs != nil && !errors.Is(err, tt.wantIs) {
				t.Fatalf("Start = %v, want an error wrapping %v", err, tt.wantIs)
			}
			rec, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: tt.wantLevel, Operation: opBringUp, Message: tt.wantMessage})
			if !ok {
				t.Fatalf("no %s record %q; records: %v", tt.wantLevel, tt.wantMessage, f.log.logger.Records())
			}
			if rec.Context["cause"] != tt.startErr.Error() {
				t.Fatalf("cause = %v, want %q", rec.Context["cause"], tt.startErr.Error())
			}
			for _, other := range []string{standDownMessage, abandonedMessage, failedMessage} {
				if other != tt.wantMessage && recordedAt(f, "info", opBringUp, other) || other != tt.wantMessage && recordedAt(f, "error", opBringUp, other) {
					t.Fatalf("a record %q was written beside the expected %q", other, tt.wantMessage)
				}
			}
		})
	}
}
