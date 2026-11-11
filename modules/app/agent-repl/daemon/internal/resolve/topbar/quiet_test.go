package topbar

import (
	"testing"

	"agent-repld/internal/viewquiet"
)

func TestAQuietTopbarTakesTheFactAndPublishesNothing(t *testing.T) {
	// Arrange
	gate := viewquiet.New(nil)
	h := newHarness(t, WithQuiet(gate))
	h.ready(t)
	before := h.view(t).GetTitle().GetText()
	gate.Set(true)

	// Act
	h.r.OnSessionUpdate(testWS, sessionTitle("Retitled while the lid is closed"))

	// Assert
	if got := h.view(t).GetTitle().GetText(); got != before {
		t.Fatalf("published title = %q, want the pre-quiet %q", got, before)
	}
	if !h.state(t).quietStale {
		t.Fatal("the strip was not marked stale for the resync")
	}
}

func TestLeavingTheQuietStatePublishesEachStaleStripAtItsCurrentState(t *testing.T) {
	// Arrange
	gate := viewquiet.New(nil)
	h := newHarness(t, WithQuiet(gate))
	h.ready(t)
	gate.Set(true)
	h.r.OnSessionUpdate(testWS, sessionTitle("Retitled while the lid is closed"))

	// Act
	gate.Set(false)

	// Assert
	if got, want := h.view(t).GetTitle().GetText(), "Retitled while the lid is closed"; got != want {
		t.Fatalf("published title = %q, want %q", got, want)
	}
	if h.state(t).quietStale {
		t.Fatal("the strip is still marked stale after the resync")
	}
}
