package boot

import (
	"os"
	"path/filepath"
	"testing"

	"claude-repld/internal/dlog"
	"claude-repld/internal/sessionlock"
	"claude-repld/internal/shimsocket"
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

// TestProbeShimSocketRecordsAnAbsentPath pins the production socket probe's
// ordinary answer: nothing ever bound the path, and the probe says so in the
// log a boot report is reconstructed from.
func TestProbeShimSocketRecordsAnAbsentPath(t *testing.T) {
	// Arrange.
	log := dlog.NewTestLogger()
	path := filepath.Join(t.TempDir(), "nobody.sock")

	// Act.
	state, err := probeShimSocket(log)(path)

	// Assert.
	if err != nil || state != shimsocket.StateAbsent {
		t.Fatalf("probe() = (%v, %v), want StateAbsent and no error", state, err)
	}
	records := log.Records()
	if len(records) != 1 || records[0].Level != "debug" ||
		records[0].Operation != "daemon.shimsocket.probe" {
		t.Fatalf("records = %+v, want one debug daemon.shimsocket.probe record", records)
	}
}

// TestProbeShimSocketRecordsAPathThatIsNotASocket pins the refusal: a path
// that exists and is not a socket is never read as free to spawn onto, and
// the reason is recorded.
func TestProbeShimSocketRecordsAPathThatIsNotASocket(t *testing.T) {
	// Arrange.
	log := dlog.NewTestLogger()
	path := filepath.Join(t.TempDir(), "not-a-socket")
	if err := os.WriteFile(path, []byte("x"), 0o600); err != nil {
		t.Fatalf("WriteFile: %v", err)
	}

	// Act.
	state, err := probeShimSocket(log)(path)

	// Assert.
	if err == nil || state != shimsocket.StateUndetermined {
		t.Fatalf("probe() = (%v, %v), want StateUndetermined and an error", state, err)
	}
	records := log.Records()
	if len(records) != 1 || records[0].Level != "error" ||
		records[0].Operation != "daemon.shimsocket.probe" {
		t.Fatalf("records = %+v, want one error daemon.shimsocket.probe record", records)
	}
}

// TestNewDefaultsTheLockProbe pins that a Deps naming no lock probe gets the
// production one rather than a nil call at the first workspace.
func TestNewDefaultsTheLockProbe(t *testing.T) {
	// Arrange.
	deps := minimalDeps(t)
	deps.Probe = nil

	// Act.
	seq, err := New(deps)

	// Assert.
	if err != nil {
		t.Fatalf("New: %v", err)
	}
	if seq.(*sequence).probe == nil {
		t.Fatal("probe = nil, want the production lock probe")
	}
}

// TestNewDefaultsTheSocketProbe pins the same for the SECOND kernel fact: a
// boot with no socket probe would spawn over a survivor it never looked for.
func TestNewDefaultsTheSocketProbe(t *testing.T) {
	// Arrange.
	deps := minimalDeps(t)
	deps.SocketProbe = nil

	// Act.
	seq, err := New(deps)

	// Assert.
	if err != nil {
		t.Fatalf("New: %v", err)
	}
	if seq.(*sequence).socketProbe == nil {
		t.Fatal("socketProbe = nil, want the production socket probe")
	}
}

// TestNewDefaultsTheClock pins that an orphan close is stamped with the wall
// clock when the caller supplies none.
func TestNewDefaultsTheClock(t *testing.T) {
	// Arrange.
	deps := minimalDeps(t)
	deps.Now = nil

	// Act.
	seq, err := New(deps)

	// Assert.
	if err != nil {
		t.Fatalf("New: %v", err)
	}
	now := seq.(*sequence).now
	if now == nil {
		t.Fatal("now = nil, want time.Now")
	}
	if now().IsZero() {
		t.Fatal("now() = the zero time, want the wall clock")
	}
}
