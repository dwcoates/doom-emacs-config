package e2e

import (
	"os"
	"testing"

	"claude-repld/internal/vendorguard"
)

// TestMain forbids real Claude/Anthropic calls for EVERY test in this package,
// so no individual test — and no shim it spawns — can forget.
//
// It lands on the CHILDREN too: shim.Spawn leaves cmd.Env nil unless ExtraEnv
// is set (and otherwise builds on os.Environ()), so the node shim and anything
// it spawns inherit this process's environment, where the variable now lives.
//
// os.Setenv rather than t.Setenv: t.Setenv is per-test and refuses to coexist
// with t.Parallel, and this posture must hold for the whole binary.
//
// It also captures the operator's REAL home before the first test runs, which
// is the only moment at which that value is knowable: once any harness has
// called t.Setenv("HOME", ...), os.UserHomeDir reports the isolated directory
// and the question "is this path the live daemon's?" can no longer be asked.
// requireIsolatedHome answers it from this captured value.
func TestMain(m *testing.M) {
	if err := os.Setenv(vendorguard.EnvVar, "1"); err != nil {
		panic("e2e: set " + vendorguard.EnvVar + ": " + err.Error())
	}
	home, err := os.UserHomeDir()
	if err != nil {
		panic("e2e: resolving the real home dir: " + err.Error())
	}
	realHome = home
	// Teardown that outlives t.Cleanup. See childreaper_test.go: a binary killed
	// by a signal never unwinds its cleanups, and this package spawns processes
	// that hold sockets and database handles when they are stranded.
	watchForTerminationSignals()
	code := m.Run()
	// The backstop for the paths that reach here with children still
	// registered: a test that returned without its cleanup, or a panic that
	// unwound past the framework.
	reapChildren()
	os.Exit(code)
}
