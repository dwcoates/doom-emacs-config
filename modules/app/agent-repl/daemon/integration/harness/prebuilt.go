package harness

import (
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
type BuildMode = testenv.BuildMode

const (
	// BuildHere builds every binary into this run's own root.
	BuildHere = testenv.BuildHere
	// BuildInto builds every binary into a shared directory and runs nothing.
	BuildInto = testenv.BuildInto
	// UsePrebuilt reads every binary from a shared directory.
	UsePrebuilt = testenv.UsePrebuilt
)

// BinaryMode answers how this process gets its binaries, and the shared
// directory for BuildInto and UsePrebuilt.
func BinaryMode(getenv func(string) string) (BuildMode, string, error) {
	return testenv.BinaryMode(getenv)
}

// SharedBinary answers a binary in the shared prebuilt directory's sub
// directory, failing when it is missing: a chunk told to read a binary that
// was never built is a broken run, never a reason to build one quietly.
func SharedBinary(dir, sub, name string) (string, error) {
	return testenv.SharedBinary(dir, sub, name)
}
