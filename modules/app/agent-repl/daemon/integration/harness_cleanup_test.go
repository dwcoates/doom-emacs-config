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
	t.Parallel()
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
	t.Parallel()
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

// TestAWarningLoggedAfterTheWorktreeIsGoneIsStillSwept proves the warning sweep
// survives the one event that used to blind it: the worktree's removal.
//
// A landed merge takes the source worktree with `git worktree remove`, and
// `<workspace>/.claude/emacs/daemon.log` — the sweep's ONLY reader for that
// workspace — is a symlink inside it. Reading through the link answered "no
// records" from then on, so every warning a merged workspace produced went
// unswept, in exactly the tests whose subject is the merge. The sweep reads the
// state root's own target instead, which the removal cannot reach.
func TestAWarningLoggedAfterTheWorktreeIsGoneIsStillSwept(t *testing.T) {
	t.Parallel()
	// Arrange: a workspace on a worktree, so its removal is a real merge's
	// removal and not the repository's.
	f := newOpenedWorktree(t, harness.Opts{}, "merged")
	link := harness.WorkspaceLogPath(f.ws.GetDir(), "daemon")
	if err := os.RemoveAll(f.ws.GetDir()); err != nil {
		t.Fatalf("removing the worktree: %v", err)
	}
	if _, err := os.Lstat(link); !os.IsNotExist(err) {
		t.Fatalf("Lstat %s after the removal = %v, want it gone; the arrangement would prove nothing", link, err)
	}

	// Act: the shim dies, which the daemon records as WARNINGS on that
	// workspace's own sink — the sink whose canonical link no longer exists.
	f.shim.Exit(1, "simulated crash")
	record := f.d.AwaitWorkspaceLogRecordInState("the exit the daemon recorded for a workspace whose worktree is gone",
		func(r harness.LogRecord) bool { return r.Operation == "daemon.shimclient.exit" })

	// Assert: the sweep's own material carries it.
	if record.Level != "warn" && record.Level != "error" {
		t.Fatalf("the shim's exit was recorded at %q, want a warning the sweep would catch", record.Level)
	}
	// THE OLD READER IS BLIND, and saying so is what makes this a regression
	// test rather than a restatement: reading the same sink through the
	// workspace's canonical link answers nothing at all now.
	if through := f.d.WorkspaceLog(f.ws.GetDir(), "daemon"); len(through) != 0 {
		t.Fatalf("reading %s answered %d records, want none; the removal did not take the link", link, len(through))
	}
	if !holdsOperation(f.d.UnexpectedWarnings(), "daemon.shimclient.exit") {
		t.Fatalf("UnexpectedWarnings() = %v, want the post-removal shim exit among them", f.d.UnexpectedWarnings())
	}
	// Declared LAST, so the assertion above reads the undeclared sweep and the
	// cleanup sweep reads the declared one. The death's one ERROR is awaited
	// above, so it is required; the health fault it opens is written on its
	// own goroutine and may land after the test body ends, so it is only
	// allowed.
	f.d.RequireWarnings("daemon.shimclient.exit")
	f.d.ExpectWarnings("daemon.health.open_fault")
}

// holdsOperation reports whether a swept record set names that operation.
func holdsOperation(records []harness.LogRecord, operation string) bool {
	for _, r := range records {
		if r.Operation == operation {
			return true
		}
	}
	return false
}

// TestARequiredWarningCountsAsStaleUntilItIsProduced pins RequireWarnings'
// half of the sweep: a declaration nothing has produced is named as stale, and
// stops being named the moment its record is written -- which is what fails a
// test whose list outlived the records it licensed.
func TestARequiredWarningCountsAsStaleUntilItIsProduced(t *testing.T) {
	t.Parallel()
	// Arrange.
	f := newOpened(t, harness.Opts{})
	f.shim.ExpectStartSession()
	f.d.RequireWarnings("daemon.shimclient.exit")
	f.d.ExpectWarnings("daemon.health.open_fault")
	before := f.d.UnproducedRequiredWarnings()

	// Act: the shim dies, which writes the death's one ERROR.
	f.shim.Exit(1, "simulated crash")
	f.d.AwaitWorkspaceLogRecord(f.repo.Dir, "the death's error", func(r harness.LogRecord) bool {
		return r.Operation == "daemon.shimclient.exit" && r.Level == "error"
	})

	// Assert.
	if len(before) != 1 || before[0] != "daemon.shimclient.exit" {
		t.Fatalf("UnproducedRequiredWarnings() before the death = %v, want the exit named", before)
	}
	if after := f.d.UnproducedRequiredWarnings(); len(after) != 0 {
		t.Fatalf("UnproducedRequiredWarnings() after the death = %v, want none", after)
	}
}
