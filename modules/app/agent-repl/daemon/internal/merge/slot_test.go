package merge

import (
	"testing"
)

// The owner's ruling (2026-09-28): a parked merge does NOT block its
// repository's queue. It yields its slot; the merges behind it proceed; once
// resumed it waits for the slot and makes its merge afresh on the new tip.

// TestAKickToARunningPumpIsRemembered pins the lost-wakeup fix: a merge
// enqueued (or a parked run asking for its slot back) in the instant the pump
// is finishing is still admitted, because the pump reads the mark before it
// goes idle.
func TestAKickToARunningPumpIsRemembered(t *testing.T) {
	// Arrange: an asynchronous orchestrator whose pump is running.
	h := newHarness(t)
	h.o.async = true
	h.o.mu.Lock()
	h.o.pumping[h.repoKey()] = true
	h.o.mu.Unlock()

	// Act.
	h.o.kick(h.repoKey())

	// Assert.
	h.o.mu.Lock()
	defer h.o.mu.Unlock()
	if !h.o.kicked[h.repoKey()] {
		t.Fatal("a kick to a running pump was dropped")
	}
}
