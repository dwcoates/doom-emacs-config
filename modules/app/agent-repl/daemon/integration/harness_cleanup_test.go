//go:build integration

package integration

import (
	"os"
	"path/filepath"
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

// TestAKilledDaemonLeavesNoLogTargetOutsideItsStateDir proves the harness bounds
// its own FILES the way the test above bounds its processes. The per-workspace
// durable sinks are symlinks whose targets the daemon mints itself, and a target
// minted outside the run's state directory survives every cleanup the harness
// has: a killed test would leave one behind per sink, per run, forever.
func TestAKilledDaemonLeavesNoLogTargetOutsideItsStateDir(t *testing.T) {
	// Arrange: an opened workspace, so both the daemon and the shim sinks exist.
	f := newOpened(t, harness.Opts{})
	logsDir := filepath.Join(f.d.StateDir, "logs")

	// Act: the daemon dies the way a killed test kills it.
	f.d.Kill()

	// Assert: every canonical link points inside this run's own state dir, so
	// removing the state dir removes the targets with it.
	for _, name := range []string{"daemon", "shim"} {
		link := harness.WorkspaceLogPath(f.ws.GetDir(), name)
		target, err := os.Readlink(link)
		if err != nil {
			t.Fatalf("readlink %s: %v", link, err)
		}
		if filepath.Dir(target) != logsDir {
			t.Fatalf("%s.log target = %q, want it under this run's %q: a killed test would leak it", name, target, logsDir)
		}
	}
}
