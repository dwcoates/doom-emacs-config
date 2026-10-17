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
	"fmt"
	"os"
	"path/filepath"
	"testing"

	"agent-repld/integration/harness"
)

// WebappLayerCoverageEnv names the directory the webapp layer's vitest driver
// writes its istanbul reports into (webapp/test/webapp-layer/layer-server.mjs).
const WebappLayerCoverageEnv = "AGENT_REPL_WEBAPP_LAYER_COVERAGE"

// webappLayerCoverageEnv answers the assignment handing one layer driver a
// directory of its own under root's webapp-layer directory (testrun's
// baseline merges every one of them into the webapp's coverage), or nil when
// root is empty and the run collects no coverage.
func webappLayerCoverageEnv(root string) ([]string, error) {
	if root == "" {
		return nil, nil
	}
	parent := filepath.Join(root, "webapp-layer")
	if err := os.MkdirAll(parent, 0o755); err != nil {
		return nil, fmt.Errorf("make the webapp layer's coverage directory: %w", err)
	}
	dir, err := os.MkdirTemp(parent, "driver-")
	if err != nil {
		return nil, fmt.Errorf("make the webapp layer driver's coverage directory: %w", err)
	}
	return []string{WebappLayerCoverageEnv + "=" + dir}, nil
}

// coverageEnv answers the GOCOVERDIR assignment one spawned Go binary must
// carry, or nil when this run is not collecting coverage. A directory that
// cannot be created fails the test rather than silently costing the run its
// numbers.
func coverageEnv(t testing.TB, binary string) []string {
	t.Helper()
	env, err := harness.CoverageEnv(harness.CoverageRoot(), binary)
	if err != nil {
		t.Fatalf("e2e: %v", err)
	}
	return env
}
