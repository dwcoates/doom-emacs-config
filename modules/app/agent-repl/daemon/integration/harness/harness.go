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
// Contracted, from ARCHITECTURE.md and the kickoff rulings:
//
//	AGENT_REPL_STATE_DIR            the state root (a fresh temp dir per test)
//	AGENT_REPL_FORBID_VENDOR_CALLS  set to 1 in every spawned process
//	MULTI_REPO_ROOT                 the tree whose workspaces use the multi-repo config root
//	AGENT_REPL_STORE_SOCKET         the store socket the -store-socket flag beats
//	-fake                           the daemon's fake mode (the keyword classifier heuristic)
//	-node <path>                    the interpreter the shim is spawned with (the fakeshim binary here)
//	-shim <path>                    the shim module path (a placeholder file here)
//	-webapp <dir>                   the webapp dist served at the asset origin
//	-store-socket <path>            the store socket passed explicitly to every shim
//	-prompts-dir <dir>              the prompts/ directory the briefs are read from
//	-default-config-dir <dir>       the default account root
//	-multi-repo-config-dir <dir>    the account root for workspaces under MULTI_REPO_ROOT
//	-joining <addr>                 joining mode, naming the incumbent's address
//	-pprof <addr>                   the opt-in local-only profiling listener
//	-idle-cutoff <duration>         the hibernation idle cutoff
//
// CHOSEN BY THE HARNESS (relay these to the production agents):
//
//	AGENT_REPL_LOCK_DIR             redirects ~/.cache/agent-repl/run so a test
//	                                never touches the real kernel-lock directory.
//	                                Both the daemon's probe and the shim's own
//	                                acquisition must honor it.
//	-self-repo <dir>                the daemon's own checkout, for deciding
//	                                whether a merge target is the emacs repo.
//	                                Env fallback AGENT_REPL_SELF_REPO.
//	-browser <path>                 the external browser launcher OpenExternal
//	                                invokes, as `<path> <url>`.
//	                                Env fallback AGENT_REPL_BROWSER.
//	-deploy-script <path>           the rollout's deploy script, invoked as
//	                                `<path> --no-bounce`.
//	                                Env fallback AGENT_REPL_DEPLOY_SCRIPT.
//
// Fake-shim-only (read by the fake, never by the daemon; they ride the
// daemon's own environment into the spawned shim):
//
//	FAKESHIM_PROFILE_DIR            per-workspace startup profiles
//	FAKESHIM_BUILD_SHA              the runtime shim_build_sha the fake reports
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
	for _, b := range []struct{ out, pkg string }{
		{daemonBinary, "./cmd/claude-repld"},
		{fakeshimBinary, "./integration/fakeshim"},
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
