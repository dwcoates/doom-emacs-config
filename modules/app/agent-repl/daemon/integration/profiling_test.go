//go:build integration

package integration

import (
	"fmt"
	"os"
	"path/filepath"
	"testing"

	"agent-repld/integration/harness"

	"agentrepl/testrun/testenv"
)

// A PROFILING TEST RUN PROFILES THE DAEMON A TEST STARTS (harness/profile.go):
// the daemon profiles its whole life into the test's own directory and writes
// the profile when it stops, and every workspace the test registers is named
// as the test's, so a shared daemon's labeled profile splits by test.
//
// Not parallel: the profiling run's directory is the process's environment,
// which t.Setenv sets for this test alone.
func TestAProfilingRunProfilesTheDaemonAndNamesItsWorkspacesTest(t *testing.T) {
	// Arrange
	root := t.TempDir()
	t.Setenv(testenv.ProfileDir, root)
	d := harness.StartDaemon(t, harness.Opts{})
	ws := harness.Register(t, d, harness.NewRepo(t).Dir)

	// Act
	d.Stop()

	// Assert
	dir := filepath.Join(root, testenv.ProfileName(t.Name()))
	profile := filepath.Join(dir, fmt.Sprintf("daemon-%d.cpu.pprof", d.PID()))
	if info, err := os.Stat(profile); err != nil || info.Size() == 0 {
		t.Fatalf("the daemon's whole-life profile %s: %v, want a written profile", profile, err)
	}
	mapping, err := os.ReadFile(filepath.Join(dir, harness.ProfiledWorkspacesFile))
	if err != nil {
		t.Fatal(err)
	}
	if want := ws.GetId() + "\t" + t.Name() + "\n"; string(mapping) != want {
		t.Fatalf("%s = %q, want %q", harness.ProfiledWorkspacesFile, mapping, want)
	}
	stopped := d.AwaitRunLogOperation("daemon.profiling.exit_stopped")
	if files, _ := stopped.Context["files"].([]any); len(files) != 1 || files[0] != profile {
		t.Fatalf("daemon.profiling.exit_stopped names %v, want the profile %s", stopped.Context["files"], profile)
	}
}
