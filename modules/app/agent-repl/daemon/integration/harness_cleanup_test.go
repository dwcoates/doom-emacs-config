//go:build integration

package integration

import (
	"testing"
	"time"

	"claude-repld/integration/harness"
)

// TestAKilledDaemonLeavesNoProcessNamingItsStateDir proves the harness bounds
// its own process tree. The daemon runs in its own process group and dying
// takes that group with it, but every shim it spawned is in a group of ITS own,
// so nothing but an explicit sweep keyed on the run's state directory stops a
// crashed test from leaking a shim — and a leaked daemon keeps prelaunching
// more of them for as long as the machine is up.
func TestAKilledDaemonLeavesNoProcessNamingItsStateDir(t *testing.T) {
	// Arrange: a daemon with a live shim under it.
	f := newOpened(t, harness.Opts{})
	if len(f.d.StrayPIDs()) == 0 {
		t.Fatal("StrayPIDs() is empty with a shim running, want the shim listed")
	}

	// Act: the daemon dies the way a killed test kills it, then the sweep runs.
	f.d.Kill()
	f.d.ReapStrays()

	// Assert: nothing naming this run's state dir survives.
	deadline := time.Now().Add(5 * time.Second)
	for {
		stray := f.d.StrayPIDs()
		if len(stray) == 0 {
			return
		}
		if time.Now().After(deadline) {
			t.Fatalf("StrayPIDs() = %v after the sweep, want none: a killed test left processes behind", stray)
		}
	}
}
