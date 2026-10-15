// Package testenv owns the environment contract between the test runner and
// the integration harnesses it starts: the variable names, and how a test
// process reads them.
//
// BUILD ONCE, RUN IN MANY PROCESSES. The test runner splits a suite's tests
// across several test processes, one per core, and several suites run the
// same programs. So:
//
//   - every Go program a test runs (the daemon, its fakes, the store, the
//     sidecar, the lock helper) is built ONCE per run, by the runner, into
//     its programs directory (Programs), and every test process of every
//     suite reads it from there (Program);
//   - what one suite alone builds beyond those (the e2e suite's shim bundle)
//     is built by ONE of its processes into a directory, which then exits
//     (Prebuild), and every chunk reads it from there (Prebuilt).
//
// Neither may be combined with coverage unless the run marks them
// instrumented (ProgramsInstrumented): an ordinary prebuilt binary is
// uninstrumented, so a coverage run that read one would silently measure
// nothing.
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
	case (into != "" || from != "") && getenv(Coverage) != "" && getenv(ProgramsInstrumented) == "":
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

// Programs names the directory a test run builds every shared program into,
// ONCE for the whole run: the daemon, its fakes, the store, the sidecar and
// the lock helper, each by its roster name (roster.Programs). Every test
// process that runs one of them reads it from here instead of building it
// again, whichever suite it belongs to.
const Programs = "AGENT_REPL_TEST_PROGRAMS"

// ProgramsInstrumented says the programs directory and the shared prebuilt
// binaries were cover-built, so they may serve a coverage run. Only a run
// that built them so sets it: `testrun run --coverage`, and `testrun
// redundancy` for the programs directory it builds itself. An ordinary run's
// programs are never instrumented.
const ProgramsInstrumented = "AGENT_REPL_TEST_PROGRAMS_INSTRUMENTED"

// Program answers the run's built program name: its path and true when the
// run named a programs directory, or false when the process runs outside such
// a run and builds the program itself. A run that names the directory without
// the program in it is a defect in the run, answered as an error.
func Program(getenv func(string) string, name string) (string, bool, error) {
	dir := getenv(Programs)
	if dir == "" {
		return "", false, nil
	}
	if getenv(Coverage) != "" && getenv(ProgramsInstrumented) == "" {
		return "", false, fmt.Errorf("testenv: a run's programs are uninstrumented, so they cannot serve a coverage run (%s is set)", Coverage)
	}
	p := filepath.Join(dir, name)
	info, err := os.Stat(p)
	if err != nil {
		return "", false, fmt.Errorf("testenv: %s names %s, which holds no program %s: %w", Programs, dir, name, err)
	}
	if info.IsDir() {
		return "", false, fmt.Errorf("testenv: the program %s is a directory", p)
	}
	real, err := filepath.EvalSymlinks(p)
	if err != nil {
		return "", false, fmt.Errorf("testenv: resolve the program %s: %w", p, err)
	}
	return real, true, nil
}

// ProgramOr answers the run's built program name (Program) when this process
// runs inside a test run that built it, and otherwise whatever build answers:
// the one way a suite comes by a program another suite runs too. build is
// called only outside such a run, and a run that names its programs directory
// without the program is refused before it is.
func ProgramOr(getenv func(string) string, name string, build func() (string, error)) (string, error) {
	path, fromRun, err := Program(getenv, name)
	if err != nil || fromRun {
		return path, err
	}
	return build()
}
