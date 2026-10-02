package harness

import (
	"fmt"
	"os"
	"path/filepath"

	"agentrepl/testrun/testenv"
)

// BUILD ONCE, RUN IN MANY PROCESSES.
//
// The test runner (modules/app/agent-repl/testrun) splits a suite's tests
// across several `go test` processes, one per core. Each process's TestMain
// would otherwise build every binary again -- the daemon, the fakes, and in
// the e2e suite the real store, sidecar, lock and shim bundle -- so N chunks
// would pay N identical builds. Instead ONE process builds them all into a
// directory and exits (PrebuildEnv), and every chunk reads them from it
// (PrebuiltEnv).
//
// The two are mutually exclusive, and neither may be combined with coverage:
// a prebuilt binary is uninstrumented, so a coverage run that read one would
// silently measure nothing.

// PrebuildEnv names a directory to build every binary into; the process then
// exits without running a test.
const PrebuildEnv = testenv.Prebuild

// PrebuiltEnv names a directory a PrebuildEnv process filled; every binary is
// read from it and none is built.
const PrebuiltEnv = testenv.Prebuilt

// BuildMode is how a test process gets its binaries.
type BuildMode int

const (
	// BuildHere builds every binary into this run's own root.
	BuildHere BuildMode = iota
	// BuildInto builds every binary into a shared directory and runs nothing.
	BuildInto
	// UsePrebuilt reads every binary from a shared directory.
	UsePrebuilt
)

// BinaryMode answers how this process gets its binaries, and the shared
// directory for BuildInto and UsePrebuilt.
func BinaryMode(getenv func(string) string) (BuildMode, string, error) {
	into, from := getenv(PrebuildEnv), getenv(PrebuiltEnv)
	switch {
	case into != "" && from != "":
		return 0, "", fmt.Errorf("harness: %s and %s are both set; a process either builds the shared binaries or reads them", PrebuildEnv, PrebuiltEnv)
	case (into != "" || from != "") && getenv(CoverageEnvVar) != "":
		return 0, "", fmt.Errorf("harness: shared prebuilt binaries are uninstrumented, so they cannot serve a coverage run (%s is set)", CoverageEnvVar)
	case into != "":
		return BuildInto, into, nil
	case from != "":
		return UsePrebuilt, from, nil
	}
	return BuildHere, "", nil
}

// SharedBinary answers a binary in the shared prebuilt directory's sub
// directory, failing when it is missing: a chunk told to read a binary that
// was never built is a broken run, never a reason to build one quietly.
func SharedBinary(dir, sub, name string) (string, error) {
	p := filepath.Join(dir, sub, name)
	info, err := os.Stat(p)
	if err != nil {
		return "", fmt.Errorf("harness: %s names %s, which holds no %s: %w", PrebuiltEnv, dir, filepath.Join(sub, name), err)
	}
	if info.IsDir() {
		return "", fmt.Errorf("harness: the prebuilt %s is a directory", p)
	}
	return filepath.EvalSymlinks(p)
}
