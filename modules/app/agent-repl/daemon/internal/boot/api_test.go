package boot

import (
	"testing"

	"claude-repld/internal/dlog"
	"claude-repld/internal/sessionlock"
)

// TestProbeWorkspaceLockRecordsAnOrdinaryProbe pins that the production probe
// lands a debug record for a lock it could read, so a boot's probe results are
// reconstructable from the log.
func TestProbeWorkspaceLockRecordsAnOrdinaryProbe(t *testing.T) {
	// Arrange.
	log := dlog.NewTestLogger()
	runDir := t.TempDir()

	// Act.
	if _, err := probeWorkspaceLock(log)(runDir, t.TempDir()); err != nil {
		t.Fatalf("probe() error = %v", err)
	}

	// Assert.
	records := log.Records()
	if len(records) != 1 || records[0].Level != "debug" ||
		records[0].Operation != "daemon.sessionlock.probe" {
		t.Fatalf("records = %+v, want one debug daemon.sessionlock.probe record", records)
	}
}

// TestProbeWorkspaceLockRecordsAnUnderivablePath pins that a lock path that
// cannot be derived is recorded rather than returned silently.
func TestProbeWorkspaceLockRecordsAnUnderivablePath(t *testing.T) {
	// Arrange.
	log := dlog.NewTestLogger()

	// Act.
	state, err := probeWorkspaceLock(log)("", "")

	// Assert.
	if err == nil || state != sessionlock.StateUnknown {
		t.Fatalf("probe() = %v, %v, want StateUnknown and an error", state, err)
	}
	records := log.Records()
	if len(records) != 1 || records[0].Level != "error" ||
		records[0].Operation != "daemon.boot.probe_workspace_lock" {
		t.Fatalf("records = %+v, want one error daemon.boot.probe_workspace_lock record", records)
	}
}
