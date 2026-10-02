package harness

// Coverage plumbing for the SPAWNED systems.
//
// A `go test -cover` run instruments only the test binary's own packages,
// which for this harness is nothing that matters: every system under test is
// a SEPARATE PROCESS the harness builds and starts. Go's answer to that is
// `go build -cover` plus GOCOVERDIR — the binary is instrumented at build
// time and writes its counter files into that directory as it exits — and
// Node's is NODE_V8_COVERAGE, which every `node` process honours by writing a
// v8 coverage JSON on exit.
//
// Everything here is INERT unless AGENT_REPL_E2E_COVERAGE names a directory:
// CoverageRoot answers "" and every helper answers nil, so an ordinary run
// builds and starts exactly the processes it always did.
//
// COUNTERS ONLY LAND ON A GRACEFUL EXIT. The Go runtime writes a binary's
// counters when main returns or os.Exit is called; a SIGKILLed process writes
// nothing at all. claude-repld, shim-store and shim-claude-sidecar each
// install a SIGTERM handler and leave through their own main, so the bounded
// SIGTERM-first Stop paths (Daemon.Stop, the e2e Store/Sidecar stops) are the
// ones that produce data. Daemon.Kill deliberately does not, which is why
// StartDaemon's cleanup asks for a graceful exit FIRST whenever coverage is
// on — see gracefulStopForCoverage.

import (
	"fmt"
	"os"
	"path/filepath"

	"agentrepl/testrun/testenv"
)

// CoverageEnvVar names the directory every spawned system writes its coverage
// under. Set it to turn coverage collection on for a whole `go test` run.
const CoverageEnvVar = testenv.Coverage

// NodeCoverageDirName is the subdirectory the shim's v8 coverage lands in.
const NodeCoverageDirName = "shim"

// CoverageRoot answers the coverage root for this run, or "" when coverage
// collection is off.
func CoverageRoot() string { return os.Getenv(CoverageEnvVar) }

// CoverageEnabled reports whether this run collects coverage.
func CoverageEnabled() bool { return CoverageRoot() != "" }

// CoverageDir answers the per-binary coverage directory under root, creating
// it. It answers "" (and no error) when root is empty, so a caller never
// needs to branch on whether coverage is on.
func CoverageDir(root, binary string) (string, error) {
	if root == "" {
		return "", nil
	}
	if binary == "" {
		return "", fmt.Errorf("harness: a coverage directory needs a binary name")
	}
	dir := filepath.Join(root, binary)
	if err := os.MkdirAll(dir, 0o755); err != nil {
		return "", fmt.Errorf("harness: make the coverage directory %s: %w", dir, err)
	}
	return dir, nil
}

// CoverageEnv answers the GOCOVERDIR assignment one spawned Go binary must
// carry, or nil when coverage is off. Append it to the child's environment.
func CoverageEnv(root, binary string) ([]string, error) {
	dir, err := CoverageDir(root, binary)
	if err != nil || dir == "" {
		return nil, err
	}
	return []string{"GOCOVERDIR=" + dir}, nil
}

// NodeCoverageEnv answers the NODE_V8_COVERAGE assignment that makes every
// `node` process started beneath the holder write v8 coverage, or nil when
// coverage is off. It belongs in the DAEMON's environment: the daemon copies
// its own environment forward to each shim it spawns verbatim except for a
// fixed override set (shimclient.spawnEnv), so one assignment covers every
// shim the run ever starts.
func NodeCoverageEnv(root string) ([]string, error) {
	dir, err := CoverageDir(root, NodeCoverageDirName)
	if err != nil || dir == "" {
		return nil, err
	}
	return []string{"NODE_V8_COVERAGE=" + dir}, nil
}

// CoverageBuildArgs answers the extra `go build` arguments an instrumented
// build needs, or nil when coverage is off. `-cover` instruments the main
// module's own packages, which is exactly the code this suite exercises.
func CoverageBuildArgs(root string) []string {
	if root == "" {
		return nil
	}
	return []string{"-cover"}
}
