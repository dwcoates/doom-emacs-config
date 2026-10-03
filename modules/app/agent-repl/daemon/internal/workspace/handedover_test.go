package workspace

import (
	"context"
	"errors"
	"testing"

	"claude-repld/internal/bringup"
)

// A START IN FLIGHT WHEN ITS WORKSPACE IS HANDED OVER serves nothing: the
// handover lands while the bring-up probes the socket (the start's own first
// step), exactly as a deploy's transfer lands under a selection-triggered
// start (e2e TestEmacsHandoverTransfersAtFreeness, 2026-10-03).
func TestAStartAHandoverLandsUnderLeavesItsShimToTheSuccessor(t *testing.T) {
	tests := []struct {
		name  string
		check func(t *testing.T, f *fleetFixture, err error)
	}{
		{"the start answers that it was handed over", func(t *testing.T, _ *fleetFixture, err error) {
			if !errors.Is(err, ErrShimTaken) || !errors.Is(err, bringup.ErrNotServed) {
				t.Fatalf("Start = %v, want ErrShimTaken (and so bringup.ErrNotServed)", err)
			}
		}},
		{"its shim is detached, never stopped", func(t *testing.T, f *fleetFixture, _ error) {
			if f.client.detached != 1 || len(f.client.kills) != 0 {
				t.Fatalf("detached = %d, killed = %t; want the shim detached once and left running", f.client.detached, len(f.client.kills) != 0)
			}
		}},
		{"the serving row is not claimed back", func(t *testing.T, f *fleetFixture, _ error) {
			if len(f.db.claims) != 0 {
				t.Fatalf("serving claims = %v, want none after the handover", f.db.claims)
			}
		}},
		{"no session is held", func(t *testing.T, f *fleetFixture, _ error) {
			if f.fleet.Live("w1") {
				t.Fatal("the fleet holds a session for a workspace it handed over")
			}
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			f := newFleetFixture(t)
			ws := f.workspace("w1")
			f.onSocketProbe = func(string) {
				if _, err := f.fleet.HandOver(ws.ID); err != nil {
					t.Fatalf("HandOver: %v", err)
				}
			}

			// Act.
			err := f.fleet.Start(context.Background(), ws.ID)

			// Assert.
			tt.check(t, f, err)
		})
	}
}

// A HANDOVER THAT LANDS WHILE StartSession RUNS takes the held shim: it is
// detached for the successor, and the start serves nothing.
func TestAHandoverLandingWhileTheStartRunsTakesItsHeldShim(t *testing.T) {
	tests := []struct {
		name  string
		check func(t *testing.T, f *fleetFixture, err error)
	}{
		{"the start answers that its shim was taken", func(t *testing.T, _ *fleetFixture, err error) {
			if !errors.Is(err, ErrShimTaken) || !errors.Is(err, bringup.ErrNotServed) {
				t.Fatalf("Start = %v, want ErrShimTaken (and so bringup.ErrNotServed)", err)
			}
		}},
		{"its shim is detached once, never stopped", func(t *testing.T, f *fleetFixture, _ error) {
			if f.client.detached != 1 || len(f.client.kills) != 0 {
				t.Fatalf("detached = %d, stops = %d; want one detach and no stop", f.client.detached, len(f.client.kills))
			}
		}},
		{"the serving row is claimed only at the spawn", func(t *testing.T, f *fleetFixture, _ error) {
			if len(f.db.claims) != 1 {
				t.Fatalf("serving claims = %v, want the spawn's alone", f.db.claims)
			}
		}},
		{"nothing is held", func(t *testing.T, f *fleetFixture, _ error) {
			if f.fleet.Held("w1") {
				t.Fatal("the fleet holds a shim for a workspace it handed over")
			}
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			f := newFleetFixture(t)
			ws := f.workspace("w1")
			f.client.onStart = func() {
				if _, err := f.fleet.HandOver(ws.ID); err != nil {
					t.Fatalf("HandOver: %v", err)
				}
			}

			// Act.
			err := f.fleet.Start(context.Background(), ws.ID)

			// Assert.
			tt.check(t, f, err)
		})
	}
}

// A RECLAIMED WORKSPACE IS SERVED AGAIN: a transfer that failed after its
// HandOver gives the workspace back, and a start then serves it here.
func TestAReclaimedWorkspaceIsServedByItsNextStart(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	if _, err := f.fleet.HandOver(ws.ID); err != nil {
		t.Fatalf("HandOver: %v", err)
	}
	f.fleet.Reclaimed(ws.ID)

	// Act.
	err := f.fleet.Start(context.Background(), ws.ID)

	// Assert.
	if err != nil || !f.fleet.Live(ws.ID) || len(f.db.claims) != 1 {
		t.Fatalf("Start = %v, live = %t, claims = %v; want the reclaimed workspace served", err, f.fleet.Live(ws.ID), f.db.claims)
	}
}

// A HANDOVER OF ONE WORKSPACE LEAVES EVERY OTHER WORKSPACE'S START ALONE.
func TestAHandoverOfOneWorkspaceLeavesAnothersStartServing(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	f.workspace("w1")
	other := f.workspace("w2")
	if _, err := f.fleet.HandOver("w1"); err != nil {
		t.Fatalf("HandOver: %v", err)
	}

	// Act.
	err := f.fleet.Start(context.Background(), other.ID)

	// Assert.
	if err != nil || !f.fleet.Live(other.ID) {
		t.Fatalf("Start(w2) = %v, live = %t; want it served", err, f.fleet.Live(other.ID))
	}
}
