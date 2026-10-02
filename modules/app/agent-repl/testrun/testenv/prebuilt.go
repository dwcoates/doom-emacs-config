// Package testenv owns the environment contract between the test runner and
// the integration harnesses it starts: the variable names, and how a test
// process reads them.
//
// BUILD ONCE, RUN IN MANY PROCESSES. The test runner splits a suite's tests
// across several test processes, one per core. Each process's TestMain would
// otherwise build every binary again -- the daemon, the fakes, and in the e2e
// suite the real store, sidecar, lock and shim bundle -- so N chunks would pay
// N identical builds. Instead ONE process builds them all into a directory and
// exits (Prebuild), and every chunk reads them from it (Prebuilt).
//
// The two are mutually exclusive, and neither may be combined with coverage:
// a prebuilt binary is uninstrumented, so a coverage run that read one would
// silently measure nothing.
package testenv

import (
	"fmt"
	"os"
	"path/filepath"
)

const (
	// Prebuild names a directory one test process fills with shared binaries.
	Prebuild = "AGENT_REPL_TEST_PREBUILD"
	// Prebuilt names the directory later test processes read shared binaries from.
	Prebuilt = "AGENT_REPL_TEST_PREBUILT"
	// Coverage names the directory spawned Go systems write coverage counters into.
	Coverage = "AGENT_REPL_E2E_COVERAGE"
)

// BuildMode is how a test process gets the binaries it starts.
type BuildMode int

const (
	// BuildHere builds binaries for this test process alone.
	BuildHere BuildMode = iota
	// BuildInto fills a shared directory and runs no tests.
	BuildInto
	// UsePrebuilt reads binaries from a shared directory.
	UsePrebuilt
)

// BinaryMode resolves the mutually exclusive shared-prebuild environment.
func BinaryMode(getenv func(string) string) (BuildMode, string, error) {
	into, from := getenv(Prebuild), getenv(Prebuilt)
	switch {
	case into != "" && from != "":
		return 0, "", fmt.Errorf("testenv: %s and %s are both set; a process either builds shared binaries or reads them", Prebuild, Prebuilt)
	case (into != "" || from != "") && getenv(Coverage) != "":
		return 0, "", fmt.Errorf("testenv: shared prebuilt binaries are uninstrumented, so they cannot serve a coverage run (%s is set)", Coverage)
	case into != "":
		return BuildInto, into, nil
	case from != "":
		return UsePrebuilt, from, nil
	default:
		return BuildHere, "", nil
	}
}

// SharedBinary resolves one required file from a shared prebuilt directory,
// failing when it is missing: a chunk told to read a binary that was never
// built is a broken run, never a reason to build one quietly.
func SharedBinary(dir, sub, name string) (string, error) {
	p := filepath.Join(dir, sub, name)
	info, err := os.Stat(p)
	if err != nil {
		return "", fmt.Errorf("testenv: %s names %s, which holds no %s: %w", Prebuilt, dir, filepath.Join(sub, name), err)
	}
	if info.IsDir() {
		return "", fmt.Errorf("testenv: the prebuilt %s is a directory", p)
	}
	return filepath.EvalSymlinks(p)
}

// PrebuildDir creates and answers a prebuild process's own subdirectory of
// the shared directory. Each harness fills only its sub, so the binaries of
// suites that share one prebuilt directory never collide.
func PrebuildDir(shared, sub string) (string, error) {
	dir := filepath.Join(shared, sub)
	if err := os.MkdirAll(dir, 0o755); err != nil {
		return "", fmt.Errorf("testenv: create the prebuild directory %s: %w", dir, err)
	}
	return dir, nil
}
