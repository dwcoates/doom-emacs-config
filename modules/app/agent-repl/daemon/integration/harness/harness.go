// Package harness runs a REAL claude-repld process against FAKES of every
// neighbor: a fake shim.v1 server, fake git repositories, a temp state root, a
// fake store socket path nothing listens on, and a fake webapp dist. It never
// runs the real shim, the real store, Emacs or the webapp, and never calls the
// vendor.
//
// # The flags and environment the harness relies on
//
// These are the daemon's contracted names (ARCHITECTURE.md, the kickoff
// rulings) plus the four the harness had to choose for the fakes. The
// production agents must honor every one of them.
//
// All of these are the BINDING spellings from daemon/AGENTS.md. The harness
// invents nothing.
//
// Flags:
//
//	--state-dir <dir>                the state root (a fresh temp dir per test)
//	--fake                           the shim's offline scripted SDK and the -fake classifier
//	--node <bin>                     the interpreter the shim is spawned with (the fakeshim binary here)
//	--shim-main <path>               the shim entry point (a placeholder file here)
//	--webapp-dist <dir>              the webapp assets served at /
//	--store-socket <path>            the store socket passed explicitly to every shim
//	--prompts-dir <dir>              the prompts directory the briefs are read from
//	--default-config-dir <dir>       the default account root
//	--multi-repo-config-dir <dir>    the account root for workspaces under MULTI_REPO_ROOT
//	--joining <addr>                 blue-green successor of the incumbent at the address
//	--idle-cutoff <duration>         the hibernation idle cutoff
//	--pprof <addr>                   the opt-in local-only profiling listener
//
// Environment:
//
//	AGENT_REPL_STATE_DIR                  the state root
//	AGENT_REPL_FORBID_VENDOR_CALLS=1      set in every spawned process
//	AGENT_REPL_STORE_SOCKET               the store socket a flag beats
//	MULTI_REPO_ROOT                       the tree whose workspaces use the multi-repo account
//	AGENT_REPL_LOCK_DIR                   redirects ~/.cache/agent-repl/run for the kernel locks
//	AGENT_REPL_SELF_REPO_DIR              the daemon's own-checkout identity for the merge split
//	AGENT_REPL_HIBERNATE_IDLE_CUTOFF_MS   compresses the idle cutoff
//	AGENT_REPL_BROWSER_CMD                the external browser launcher for OpenExternal
//	AGENT_REPL_CLAUDE_BIN                 the claude binary for the login pty and the classifier
//	AGENT_REPL_DEPLOY_SCRIPT              overrides bin/deploy-all.sh for the self-reload trigger
//
// Fake-shim-only (read by the fake, never by the daemon; they ride the
// daemon's own environment into the spawned shim):
//
//	FAKESHIM_PROFILE_DIR   per-workspace startup profiles
//	FAKESHIM_BUILD_SHA     the runtime shim_build_sha the fake reports
//
// # Discipline
//
// Nothing in the harness sleeps to synchronize. Every wait is a channel
// receive or a file poll bounded by the test's context, and every spawned
// process is killed from a t.Cleanup so a failing test leaks nothing.
package harness

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"testing"
)

// Binaries built once per `go test` run by Main.
var (
	daemonBinary   string
	fakeshimBinary string
	gitBinary      string
	buildErr       error
)

// Main builds the daemon and the fake shim, runs the suite, and reports the
// status the caller should exit with. A suite's TestMain is exactly:
//
//	func TestMain(m *testing.M) { os.Exit(harness.Main(m)) }
func Main(m *testing.M) int {
	dir, err := os.MkdirTemp("", "agent-repl-integration-bin-")
	if err != nil {
		fmt.Fprintln(os.Stderr, "harness: temp dir:", err)
		return 1
	}
	defer os.RemoveAll(dir)

	module, err := moduleRoot()
	if err != nil {
		fmt.Fprintln(os.Stderr, "harness:", err)
		return 1
	}
	daemonBinary = filepath.Join(dir, "claude-repld")
	fakeshimBinary = filepath.Join(dir, "fakeshim")
	// The scripted `git`. It is built under its real name because it is placed
	// first on the daemon's PATH: nothing in a test ever reaches the real
	// binary.
	gitBinary = filepath.Join(dir, "git")
	for _, b := range []struct{ out, pkg string }{
		{daemonBinary, "./cmd/claude-repld"},
		{fakeshimBinary, "./integration/fakeshim"},
		{gitBinary, "./integration/fakegit/git"},
	} {
		cmd := exec.Command("go", "build", "-o", b.out, b.pkg)
		cmd.Dir = module
		cmd.Stderr = os.Stderr
		cmd.Stdout = os.Stderr
		if err := cmd.Run(); err != nil {
			// Recorded rather than fatal: a suite that only exercises the fake
			// still reports a real failure through DaemonBinary below.
			buildErr = fmt.Errorf("harness: go build %s: %w", b.pkg, err)
			fmt.Fprintln(os.Stderr, buildErr)
			return 1
		}
	}
	return m.Run()
}

// DaemonBinary is the built daemon, or a fatal failure if Main did not build.
func DaemonBinary(t *testing.T) string {
	t.Helper()
	if buildErr != nil {
		t.Fatalf("%v", buildErr)
	}
	if daemonBinary == "" {
		t.Fatal("harness: the suite's TestMain must call harness.Main")
	}
	return daemonBinary
}

// FakeShimBinary is the built fake shim.
func FakeShimBinary(t *testing.T) string {
	t.Helper()
	if fakeshimBinary == "" {
		t.Fatal("harness: the suite's TestMain must call harness.Main")
	}
	return fakeshimBinary
}

// FakeGitBinary is the built scripted `git`.
func FakeGitBinary(t *testing.T) string {
	t.Helper()
	if gitBinary == "" {
		t.Fatal("harness: the suite's TestMain must call harness.Main")
	}
	return gitBinary
}

// moduleRoot walks up from the working directory to the daemon module root
// (the directory holding go.mod).
func moduleRoot() (string, error) {
	dir, err := os.Getwd()
	if err != nil {
		return "", fmt.Errorf("working directory: %w", err)
	}
	for {
		if _, err := os.Stat(filepath.Join(dir, "go.mod")); err == nil {
			return dir, nil
		}
		parent := filepath.Dir(dir)
		if parent == dir {
			return "", fmt.Errorf("no go.mod above %s", dir)
		}
		dir = parent
	}
}

// RepoRoot is the agent-repl module directory (the daemon module's parent),
// from which prompts/ and the other shipped assets are copied.
func RepoRoot(t *testing.T) string {
	t.Helper()
	module, err := moduleRoot()
	if err != nil {
		t.Fatalf("harness: %v", err)
	}
	return filepath.Dir(module)
}
