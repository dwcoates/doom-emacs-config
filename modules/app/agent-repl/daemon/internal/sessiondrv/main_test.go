package sessiondrv

import (
	"os"
	"testing"

	"claude-repld/internal/vendorguard"
)

// TestMain forbids real Claude/Anthropic calls for EVERY test in this package,
// so no individual test can forget. The classifier is the one code path here
// that execs the real `claude` CLI; with this set it returns a loud error
// instead (see spawnClassifier and vendorguard.Check).
//
// os.Setenv rather than t.Setenv: t.Setenv is per-test and refuses to coexist
// with t.Parallel, and this posture must hold for the whole binary.
func TestMain(m *testing.M) {
	if err := os.Setenv(vendorguard.EnvVar, "1"); err != nil {
		panic("sessiondrv tests: set " + vendorguard.EnvVar + ": " + err.Error())
	}
	os.Exit(m.Run())
}
