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
func TestMain(m *testing.M) {
	if err := os.Setenv(vendorguard.EnvVar, "1"); err != nil {
		panic("e2e: set " + vendorguard.EnvVar + ": " + err.Error())
	}
	os.Exit(m.Run())
}
