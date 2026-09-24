package workspace

import (
	"context"
	"testing"

	"claude-repld/internal/ids"
	"claude-repld/internal/sessionlock"
	"claude-repld/internal/shimsocket"
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
