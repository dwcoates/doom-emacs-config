package e2e

// Coverage collection for the systems this suite SPAWNS.
//
// The suite's whole subject is four separate processes, so `go test -cover`
// on this package measures nothing worth having. Coverage therefore comes
// from instrumented BUILDS (`go build -cover`, main_test.go's goBuildOnce)
// whose processes write counters into a per-binary GOCOVERDIR, plus
// NODE_V8_COVERAGE for the shim (set on the daemon by harness.StartDaemon and
// inherited by every shim it spawns). The one knob is
// AGENT_REPL_E2E_COVERAGE; unset, every helper here answers nil and the run
// is byte-for-byte the ordinary one.
//
// COUNTERS ONLY LAND ON A GRACEFUL EXIT: the store and the sidecar are both
// stopped with SIGTERM (Store.Stop, Sidecar.Stop -> stopProcess) and both
// leave through their own main, and harness.StartDaemon's cleanup asks the
// daemon for the same before it kills it. A process this suite deliberately
// SIGKILLs (a crash-simulation cold gate) contributes nothing, by design.
//
// `make coverage` in this directory runs the suite with the knob set and
// merges what lands; see Makefile.

import (
	"testing"

	"claude-repld/integration/harness"
)

// coverageEnv answers the GOCOVERDIR assignment one spawned Go binary must
// carry, or nil when this run is not collecting coverage. A directory that
// cannot be created fails the test rather than silently costing the run its
// numbers.
func coverageEnv(t *testing.T, binary string) []string {
	t.Helper()
	env, err := harness.CoverageEnv(harness.CoverageRoot(), binary)
	if err != nil {
		t.Fatalf("e2e: %v", err)
	}
	return env
}

func TestCoverageEnvFollowsTheRun(t *testing.T) {
	tests := []struct {
		name    string
		root    string
		binary  string
		wantSet bool
	}{
		{name: "off, no assignment", root: "", binary: "shim-store"},
		{name: "on, one assignment", root: t.TempDir(), binary: "shim-store", wantSet: true},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			if tc.root == "" {
				t.Setenv(harness.CoverageEnvVar, "")
			} else {
				t.Setenv(harness.CoverageEnvVar, tc.root)
			}

			// Act.
			env := coverageEnv(t, tc.binary)

			// Assert.
			if tc.wantSet {
				want := "GOCOVERDIR=" + tc.root + "/" + tc.binary
				if len(env) != 1 || env[0] != want {
					t.Fatalf("coverageEnv = %v, want [%s]", env, want)
				}
				return
			}
			if env != nil {
				t.Fatalf("coverageEnv = %v, want nil", env)
			}
		})
	}
}
