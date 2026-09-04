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
	"errors"
	"fmt"
	"io/fs"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
)

// Binaries built once per `go test` run by Main.
var (
	daemonBinary   string
	fakeshimBinary string
	gitBinary      string
	buildErr       error
	pinnedCheckout string
)

// Main builds the daemon and the fake shim, runs the suite, and reports the
// status the caller should exit with. A suite's TestMain is exactly:
//
//	func TestMain(m *testing.M) { os.Exit(harness.Main(m)) }
func Main(m *testing.M) int {
	module, err := moduleRoot()
	if err != nil {
		fmt.Fprintln(os.Stderr, "harness:", err)
		return 1
	}
	return MainAt(m, module)
}

// MainAt is Main, except the daemon module root is given explicitly instead
// of discovered by walking up from the working directory. A suite outside the
// claude-repld module (agentrepl/e2e) cannot use the cwd-walk — its own
// go.mod would be found first, since the walk stops at the nearest one — so
// it resolves its own relative path to daemon/ and calls this instead of
// Main.
func MainAt(m *testing.M, module string) int {
	dir, err := os.MkdirTemp("", "agent-repl-integration-bin-")
	if err != nil {
		fmt.Fprintln(os.Stderr, "harness: temp dir:", err)
		return 1
	}
	defer os.RemoveAll(dir)

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
		// An instrumented build when coverage is on; the plain build
		// otherwise. `-cover` changes only what the binary WRITES, never
		// what it does.
		buildArgs := append([]string{"build"}, CoverageBuildArgs(CoverageRoot())...)
		buildArgs = append(buildArgs, "-o", b.out, b.pkg)
		cmd := exec.Command("go", buildArgs...)
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
	repo := filepath.Dir(module)
	pinnedCheckout, err = newPinnedCheckout(filepath.Join(dir, "checkout"), repo)
	if err != nil {
		fmt.Fprintln(os.Stderr, "harness:", err)
		return 1
	}
	// The self-check runs BEFORE any test, because a build identity that
	// disagrees fails as a stale-shim relaunch on every spawn rather than as
	// anything a test can read.
	if err := checkBuildIdentityAgrees(pinnedCheckout); err != nil {
		fmt.Fprintln(os.Stderr, "harness:", err)
		return 1
	}
	return m.Run()
}

// newPinnedCheckout lays out the checkout root every daemon in this suite
// resolves its build stamps from: a directory that carries NEITHER
// agent-shim/claude/shim/dist/.built-sha nor daemon/bin/.built-sha, so the
// harness's SHIM_BUILD_SHA and AGENT_REPL_DEPLOY_STAMP are the only answers
// and no host build can be read as this suite's deployed build. The shared
// render vocabulary is the one asset beneath the checkout that has no flag
// override, so the real tree's proto/ is linked in.
func newPinnedCheckout(dir, repo string) (string, error) {
	if err := os.MkdirAll(dir, 0o755); err != nil {
		return "", fmt.Errorf("mkdir the pinned checkout %s: %w", dir, err)
	}
	link := filepath.Join(dir, "proto")
	if err := os.Symlink(filepath.Join(repo, "proto"), link); err != nil && !errors.Is(err, fs.ErrExist) {
		return "", fmt.Errorf("link the render vocabulary into %s: %w", dir, err)
	}
	return dir, nil
}

// PinnedCheckout is the harness-owned checkout root, or a fatal failure if
// Main did not lay it out.
func PinnedCheckout(t *testing.T) string {
	t.Helper()
	if pinnedCheckout == "" {
		t.Fatal("harness: the suite's TestMain must call harness.Main")
	}
	return pinnedCheckout
}

// checkBuildIdentityAgrees is this harness's self-check for the invariant
// BuildIdentityEnv exists to hold: the build the daemon EXPORTS to each shim,
// the build the fake shim REPORTS, and the build the daemon reads as its
// DEPLOYED one are one string, and no stamp file under the pinned checkout
// can answer ahead of them.
func checkBuildIdentityAgrees(checkout string) error {
	env := BuildIdentityEnv(checkout)
	reported, deployed := valueOf(env, "SHIM_BUILD_SHA"), valueOf(env, "AGENT_REPL_DEPLOY_STAMP")
	if reported == "" || deployed == "" {
		return fmt.Errorf("build identity is unset: SHIM_BUILD_SHA=%q AGENT_REPL_DEPLOY_STAMP=%q", reported, deployed)
	}
	if reported != deployed {
		return fmt.Errorf("build identity disagrees: the daemon would export SHIM_BUILD_SHA=%q "+
			"while reading AGENT_REPL_DEPLOY_STAMP=%q as its deployed build; every fake shim would be judged stale and relaunched",
			reported, deployed)
	}
	if reported != FakeShimDefaultBuildSHA {
		return fmt.Errorf("build identity is %q, but the fake shim reports %q; every fake shim would be judged stale and relaunched",
			reported, FakeShimDefaultBuildSHA)
	}
	if root := valueOf(env, "AGENT_REPL_CHECKOUT"); root != checkout {
		return fmt.Errorf("the pinned checkout is %q, but the daemon would resolve %q", checkout, root)
	}
	for _, stamp := range []string{
		filepath.Join(checkout, "agent-shim", "claude", "shim", "dist", ".built-sha"),
		filepath.Join(checkout, "daemon", "bin", ".built-sha"),
	} {
		if _, err := os.Stat(stamp); err == nil {
			return fmt.Errorf("the pinned checkout carries the build stamp %s, which answers ahead of the harness's own identity", stamp)
		} else if !errors.Is(err, fs.ErrNotExist) {
			return fmt.Errorf("stat the build stamp %s: %w", stamp, err)
		}
	}
	return nil
}

// valueOf answers a KEY=VALUE list's entry for key, or "" when it carries none.
func valueOf(env []string, key string) string {
	for _, kv := range env {
		if name, value, ok := strings.Cut(kv, "="); ok && name == key {
			return value
		}
	}
	return ""
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
