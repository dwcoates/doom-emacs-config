package harness

import (
	"fmt"
	"os"
	"path/filepath"
)

// A PROFILING RUN (testrun.v1.TestRunRequest.profile) PROFILES EVERY DAEMON A
// TEST STARTS. StartDaemon tells the daemon to profile its whole life into the
// test's profiles directory (testenv.ExitProfileEnvFor), and every shim the
// daemon spawns inherits the binding (shimclient.spawnEnv copies the daemon's
// environment forward), so the test's directory holds daemon-<pid>.cpu.pprof
// and one shim-<pid>.cpuprofile per shim.
//
// A SHARED DAEMON SPLITS BY TEST. The daemon labels its per-workspace work
// with the workspace's id (daemon/AGENTS.md "Profiler labels"), and each test
// registers its own workspaces, so ProfiledWorkspacesFile maps each label to
// the test that owns it.

// ProfiledWorkspacesFile, in a profiled daemon's profiles directory, holds one
// "<workspace id>\t<test>" line per workspace a test registered with it.
const ProfiledWorkspacesFile = "workspaces.tsv"

// recordProfiledWorkspace appends the line naming workspace ID as TEST's to
// DIR's ProfiledWorkspacesFile. Each line is one append-mode write, so the
// tests sharing a daemon never interleave theirs.
func recordProfiledWorkspace(dir, id, test string) error {
	f, err := os.OpenFile(filepath.Join(dir, ProfiledWorkspacesFile), os.O_APPEND|os.O_CREATE|os.O_WRONLY, 0o644)
	if err != nil {
		return fmt.Errorf("harness: open the profiled workspaces of %s: %w", dir, err)
	}
	if _, err := fmt.Fprintf(f, "%s\t%s\n", id, test); err != nil {
		_ = f.Close()
		return fmt.Errorf("harness: record workspace %s as %s's: %w", id, test, err)
	}
	if err := f.Close(); err != nil {
		return fmt.Errorf("harness: close the profiled workspaces of %s: %w", dir, err)
	}
	return nil
}
