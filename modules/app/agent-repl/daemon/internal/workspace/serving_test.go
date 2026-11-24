package workspace

import (
	"context"
	"errors"
	"fmt"
	"testing"

	"agent-repld/internal/ids"
	"agent-repld/internal/sessionlock"
	"agent-repld/internal/shimsocket"
	"agent-repld/internal/wsm"
)

// THE SERVING CLAIM IS MADE WHERE THE FLEET BEGINS HOLDING A CLIENT, so every
// path that brings a session up in this daemon leaves the serving row naming
// it. The defect these pin: a cold-started daemon served three live sessions
// under a dead instance's row, and its handover skipped every one of them.

func TestEveryBringUpClaimsServingForThisDaemon(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(f *fleetFixture)
		act     func(f *fleetFixture, ws ids.WorkspaceID) error
	}{
		{
			name:    "a cold-start bring-up that spawns a shim",
			arrange: func(*fleetFixture) {},
			act: func(f *fleetFixture, ws ids.WorkspaceID) error {
				return f.fleet.Start(context.Background(), ws)
			},
		},
		{
			name: "a bring-up that attaches to a surviving shim",
			arrange: func(f *fleetFixture) {
				f.probeState = sessionlock.StateHeld
				f.socketState = shimsocket.StateLive
			},
			act: func(f *fleetFixture, ws ids.WorkspaceID) error {
				return f.fleet.Start(context.Background(), ws)
			},
		},
		{
			name:    "an install of an adopted client (the boot's and the handover's adoption)",
			arrange: func(*fleetFixture) {},
			act: func(f *fleetFixture, ws ids.WorkspaceID) error {
				return f.fleet.Install(context.Background(), ws, f.client)
			},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			f := newFleetFixture(t)
			ws := f.workspace("w1")
			tt.arrange(f)

			// Act.
			if err := tt.act(f, ws.ID); err != nil {
				t.Fatalf("bring-up: %v", err)
			}

			// Assert.
			if len(f.db.claims) == 0 {
				t.Fatal("no serving claim was made; a handover would skip this live session")
			}
			for _, c := range f.db.claims {
				if c.ws != ws.ID || c.instance != fixtureInstance {
					t.Fatalf("claims = %+v, want every claim for %q by %q", f.db.claims, ws.ID, fixtureInstance)
				}
			}
		})
	}
}

func TestARestatedSessionIsNotClaimedTwice(t *testing.T) {
	// Arrange: sessionUp states the same client twice (before and after the
	// watcher opens); the second statement is not a new arrival.
	f := newFleetFixture(t)
	ws := f.workspace("w1")

	// Act.
	if err := f.fleet.Start(context.Background(), ws.ID); err != nil {
		t.Fatalf("Start: %v", err)
	}

	// Assert.
	if len(f.db.claims) != 1 {
		t.Fatalf("claims = %+v, want exactly one for one bring-up", f.db.claims)
	}
}

func TestAFailedServingClaimFailsTheBringUpLoudly(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.db.claimErr = errFake

	// Act.
	err := f.fleet.Start(context.Background(), ws.ID)

	// Assert.
	if err == nil {
		t.Fatal("Start() = nil error, want the failed claim to fail the bring-up")
	}
	errors := 0
	for _, rec := range f.log.logger.Records() {
		if rec.Level == "error" && rec.Operation == opServing {
			errors++
		}
	}
	if errors != 1 {
		t.Fatalf("serving ERROR records = %d, want exactly one", errors)
	}
}

func TestAServingClaimOfAForgottenWorkspaceEndsTheBringUpAtInfo(t *testing.T) {
	// Arrange: the workspace was forgotten under its bring-up.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.db.claimErr = fmt.Errorf("wsm: workspace %s: %w", ws.ID, wsm.ErrNotFound)

	// Act.
	err := f.fleet.Start(context.Background(), ws.ID)

	// Assert.
	if !errors.Is(err, wsm.ErrNotFound) {
		t.Fatalf("Start() = %v, want the bring-up ended on wsm.ErrNotFound", err)
	}
	var infos int
	for _, rec := range f.log.logger.Records() {
		if rec.Operation != opServing {
			continue
		}
		if rec.Level == "error" {
			t.Fatalf("serving record %+v at ERROR, want a forgotten workspace recorded at INFO", rec)
		}
		if rec.Level == "info" && rec.Context["workspace"] == string(ws.ID) && rec.Context["instance"] == string(fixtureInstance) {
			infos++
		}
	}
	if infos != 1 {
		t.Fatalf("serving INFO records naming %s = %d, want exactly one", ws.ID, infos)
	}
}

func TestAFailedServingClaimLeavesTheShimHeld(t *testing.T) {
	// Arrange: the claim fails AFTER the client is recorded, so the spawned
	// shim is never left running with nothing holding it.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.db.claimErr = errFake

	// Act.
	_ = f.fleet.Start(context.Background(), ws.ID)

	// Assert.
	if _, ok := f.fleet.Client(ws.ID); !ok {
		t.Fatal("the fleet holds no client after a failed claim; the shim would be orphaned")
	}
}

func TestAServingClaimTheForgetsFenceEndedIsRecordedAtInfo(t *testing.T) {
	// Arrange: the forget's fence cancelled the start, and with it its claim.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.db.claimErr = context.Canceled
	ctx, cancel := context.WithCancelCause(context.Background())
	cancel(ErrWorkspaceForgotten)

	// Act.
	err := f.fleet.claimServing(ctx, f.log.logger, ws.ID)

	// Assert.
	if !errors.Is(err, ErrWorkspaceForgotten) {
		t.Fatalf("claimServing() = %v, want the claim ended on ErrWorkspaceForgotten", err)
	}
	var infos int
	for _, rec := range f.log.logger.Records() {
		if rec.Operation != opServing {
			continue
		}
		if rec.Level == "error" {
			t.Fatalf("serving record %+v at ERROR, want a claim the fence ended recorded at INFO", rec)
		}
		if rec.Level == "info" && rec.Context["workspace"] == string(ws.ID) {
			infos++
		}
	}
	if infos != 1 {
		t.Fatalf("serving INFO records naming %s = %d, want exactly one", ws.ID, infos)
	}
}

func TestAServingClaimCancelledForAnotherCauseStaysAnError(t *testing.T) {
	// Arrange: the claim's context ended for a reason that is not the forget.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.db.claimErr = context.Canceled
	ctx, cancel := context.WithCancel(context.Background())
	cancel()

	// Act.
	err := f.fleet.claimServing(ctx, f.log.logger, ws.ID)

	// Assert.
	if err == nil || errors.Is(err, ErrWorkspaceForgotten) {
		t.Fatalf("claimServing() = %v, want a failed claim not read as the forget's", err)
	}
	var errs int
	for _, rec := range f.log.logger.Records() {
		if rec.Operation == opServing && rec.Level == "error" {
			errs++
		}
	}
	if errs != 1 {
		t.Fatalf("serving ERROR records = %d, want exactly one", errs)
	}
}
