package sessionlock

import (
	"crypto/md5"
	"encoding/hex"
	"os"
	"path/filepath"
	"strings"
	"syscall"
	"testing"

	"claude-repld/internal/dlog"
)

// TestResolveRunDirHonorsEnvOverride asserts the test override wins over the
// home-relative default.
func TestResolveRunDirHonorsEnvOverride(t *testing.T) {
	// Arrange.
	dir := t.TempDir()
	t.Setenv(RunDirEnv, dir)

	// Act.
	got, err := ResolveRunDir()

	// Assert.
	if err != nil {
		t.Fatalf("ResolveRunDir() error = %v", err)
	}
	if got != dir {
		t.Fatalf("ResolveRunDir() = %q, want %q", got, dir)
	}
}

// TestResolveRunDirDefaultsUnderHome asserts the tilde in RunDir is the home
// directory and nothing else.
func TestResolveRunDirDefaultsUnderHome(t *testing.T) {
	// Arrange.
	t.Setenv(RunDirEnv, "")
	home, err := os.UserHomeDir()
	if err != nil {
		t.Skipf("no home directory: %v", err)
	}

	// Act.
	got, err := ResolveRunDir()

	// Assert.
	if err != nil {
		t.Fatalf("ResolveRunDir() error = %v", err)
	}
	if want := filepath.Join(home, ".cache", "agent-repl", "run"); got != want {
		t.Fatalf("ResolveRunDir() = %q, want %q", got, want)
	}
}

// TestWorkspaceLockPathSpelling asserts the derived name is exactly
// workspace-<md5hex(clean abs dir)[:8]>.lock.
func TestWorkspaceLockPathSpelling(t *testing.T) {
	// Arrange.
	runDir := t.TempDir()
	wsDir := "/tmp/some/workspace"
	sum := md5.Sum([]byte(wsDir))
	want := filepath.Join(runDir, "workspace-"+hex.EncodeToString(sum[:])[:8]+".lock")

	// Act.
	got, err := WorkspaceLockPath(runDir, wsDir)

	// Assert.
	if err != nil {
		t.Fatalf("WorkspaceLockPath() error = %v", err)
	}
	if got != want {
		t.Fatalf("WorkspaceLockPath() = %q, want %q", got, want)
	}
}

// TestWorkspaceLockPathCleansBeforeHashing asserts an uncleaned directory
// hashes to the same lock as its cleaned form.
func TestWorkspaceLockPathCleansBeforeHashing(t *testing.T) {
	// Arrange.
	runDir := t.TempDir()
	clean, err := WorkspaceLockPath(runDir, "/tmp/some/workspace")
	if err != nil {
		t.Fatalf("WorkspaceLockPath(clean) error = %v", err)
	}

	// Act.
	got, err := WorkspaceLockPath(runDir, "/tmp/some/other/../workspace/")

	// Assert.
	if err != nil {
		t.Fatalf("WorkspaceLockPath() error = %v", err)
	}
	if got != clean {
		t.Fatalf("WorkspaceLockPath() = %q, want %q", got, clean)
	}
}

// TestWorkspaceLockPathRefusesEmptyDir asserts an empty workspace dir is an
// error rather than a path under the run directory.
func TestWorkspaceLockPathRefusesEmptyDir(t *testing.T) {
	// Arrange.
	runDir := t.TempDir()

	// Act.
	_, err := WorkspaceLockPath(runDir, "  ")

	// Assert.
	if err == nil {
		t.Fatal("WorkspaceLockPath() error = nil, want an error")
	}
}

// TestSessionLockPathSpelling asserts the session lock's name.
func TestSessionLockPathSpelling(t *testing.T) {
	// Arrange.
	runDir := t.TempDir()

	// Act.
	got, err := SessionLockPath(runDir, "vendor-abc")

	// Assert.
	if err != nil {
		t.Fatalf("SessionLockPath() error = %v", err)
	}
	if want := filepath.Join(runDir, "session-vendor-abc.lock"); got != want {
		t.Fatalf("SessionLockPath() = %q, want %q", got, want)
	}
}

// TestSessionLockPathRefusesSeparator asserts a vendor id carrying a path
// separator cannot escape the run directory.
func TestSessionLockPathRefusesSeparator(t *testing.T) {
	// Arrange.
	runDir := t.TempDir()

	// Act.
	_, err := SessionLockPath(runDir, "../escape")

	// Assert.
	if err == nil {
		t.Fatal("SessionLockPath() error = nil, want an error")
	}
}

// TestProbeFree asserts an unheld lock probes free.
func TestProbeFree(t *testing.T) {
	// Arrange.
	path := filepath.Join(t.TempDir(), "workspace-deadbeef.lock")

	// Act.
	state, err := Probe(path)

	// Assert.
	if err != nil {
		t.Fatalf("Probe() error = %v", err)
	}
	if state != StateFree {
		t.Fatalf("Probe() = %v, want StateFree", state)
	}
}

// TestProbeHeld asserts a lock another open file description holds probes
// held, not free.
func TestProbeHeld(t *testing.T) {
	// Arrange.
	path := filepath.Join(t.TempDir(), "workspace-deadbeef.lock")
	holder, err := os.OpenFile(path, os.O_RDWR|os.O_CREATE, 0o600)
	if err != nil {
		t.Fatalf("open holder: %v", err)
	}
	t.Cleanup(func() { holder.Close() })
	if err := syscall.Flock(int(holder.Fd()), syscall.LOCK_EX|syscall.LOCK_NB); err != nil {
		t.Fatalf("holder flock: %v", err)
	}

	// Act.
	state, err := Probe(path)

	// Assert.
	if err != nil {
		t.Fatalf("Probe() error = %v", err)
	}
	if state != StateHeld {
		t.Fatalf("Probe() = %v, want StateHeld", state)
	}
}

// TestProbeUnknownOnUnopenableLock asserts a lock that cannot even be opened
// is unknown WITH the error, never free.
func TestProbeUnknownOnUnopenableLock(t *testing.T) {
	// Arrange: a REGULAR FILE standing where the run directory would go, so
	// neither the directory nor the lock file can be made.
	root := t.TempDir()
	blocker := filepath.Join(root, "run")
	if err := os.WriteFile(blocker, []byte("not a directory"), 0o600); err != nil {
		t.Fatalf("writing the blocker: %v", err)
	}
	path := filepath.Join(blocker, "workspace-deadbeef.lock")

	// Act.
	state, err := Probe(path)

	// Assert.
	if state != StateUnknown {
		t.Fatalf("Probe() = %v, want StateUnknown", state)
	}
	if err == nil {
		t.Fatal("Probe() error = nil, want the unopenable-lock error")
	}
}

// TestProbeCreatesTheRunDirectoryAndAnswersFree is the BOOTSTRAP: on a machine
// that has never started a session the run directory does not exist, and the
// daemon will not spawn the shim that would create it until a probe says free.
// A missing directory answered StateUnknown, so nothing could ever start.
func TestProbeCreatesTheRunDirectoryAndAnswersFree(t *testing.T) {
	// Arrange: a run directory that has never existed.
	runDir := filepath.Join(t.TempDir(), "run")
	path := filepath.Join(runDir, "workspace-deadbeef.lock")

	// Act.
	state, err := Probe(path)

	// Assert: nobody can hold a lock in a directory that does not exist, so the
	// honest answer is free — and the directory now exists for the shim.
	if err != nil {
		t.Fatalf("Probe() error = %v", err)
	}
	if state != StateFree {
		t.Fatalf("Probe() = %v, want StateFree", state)
	}
	info, statErr := os.Stat(runDir)
	if statErr != nil {
		t.Fatalf("the run directory was not created: %v", statErr)
	}
	if !info.IsDir() {
		t.Fatalf("the run directory is not a directory: %v", info.Mode())
	}
}

// TestProbeRefusesEmptyPath asserts an empty path is an error, not a free
// answer.
func TestProbeRefusesEmptyPath(t *testing.T) {
	// Arrange, Act.
	state, err := Probe("")

	// Assert.
	if err == nil {
		t.Fatal("Probe() error = nil, want an error")
	}
	if state != StateUnknown {
		t.Fatalf("Probe() = %v, want StateUnknown", state)
	}
}

// TestProbeWithLogRecordsTheOrdinaryBranch asserts the free branch logs at
// debug under the canonical operation name.
func TestProbeWithLogRecordsTheOrdinaryBranch(t *testing.T) {
	// Arrange.
	log := dlog.NewTestLogger()
	path := filepath.Join(t.TempDir(), "workspace-deadbeef.lock")

	// Act.
	if _, err := ProbeWithLog(log, path); err != nil {
		t.Fatalf("ProbeWithLog() error = %v", err)
	}

	// Assert.
	records := log.Records()
	if len(records) != 1 {
		t.Fatalf("records = %d, want 1", len(records))
	}
	if records[0].Level != "debug" || records[0].Operation != "daemon.sessionlock.probe" {
		t.Fatalf("record = %+v, want a debug daemon.sessionlock.probe record", records[0])
	}
	if got := records[0].Context["state"]; got != "free" {
		t.Fatalf("context state = %v, want \"free\"", got)
	}
}

// TestProbeWithLogRecordsTheFailureBranch asserts a probe that could not tell
// logs at error with its evidence.
func TestProbeWithLogRecordsTheFailureBranch(t *testing.T) {
	// Arrange: a regular file where the run directory would go. A merely
	// MISSING directory is no longer a failure -- the probe creates it, because
	// nobody can hold a lock in a directory that does not exist.
	log := dlog.NewTestLogger()
	blocker := filepath.Join(t.TempDir(), "run")
	if err := os.WriteFile(blocker, []byte("not a directory"), 0o600); err != nil {
		t.Fatalf("writing the blocker: %v", err)
	}
	path := filepath.Join(blocker, "workspace-deadbeef.lock")

	// Act.
	if _, err := ProbeWithLog(log, path); err == nil {
		t.Fatal("ProbeWithLog() error = nil, want an error")
	}

	// Assert.
	records := log.Records()
	if len(records) != 1 {
		t.Fatalf("records = %d, want 1", len(records))
	}
	if records[0].Level != "error" {
		t.Fatalf("level = %q, want \"error\"", records[0].Level)
	}
	if msg, _ := records[0].Context["error"].(string); !strings.Contains(msg, "sessionlock: create the run directory") {
		t.Fatalf("context error = %q, want the run-directory failure", msg)
	}
}
