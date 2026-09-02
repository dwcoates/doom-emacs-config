// Package e2e is the cross-system end-to-end suite: a real claude-repld, a
// real shim-store, a real shim-claude-sidecar, and the real TypeScript shim
// (built from source, running its `--fake` offline scripted vendor),
// composed exactly as production wires them. Every area test file imports
// the surface this file and world_test.go export; see SPEC.md (read-only —
// never edited by an area writer) for the full design and the per-area test
// list.
//
// Nothing here reaches inside any of the four systems. Every assertion an
// area test makes is on what crossed a real wire: the daemon's Connect API
// (agentreplv1connect.AgentReplClient) and its watch streams, or a
// structured-log record from one of the four processes.
//
// SYNCHRONIZATION IS ALWAYS A REAL SIGNAL — a watch-stream frame, a
// structured-log record, or a bounded poll of a store read verb. There is no
// time.Sleep anywhere in this package.
package e2e

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"regexp"
	"runtime"
	"sync"
	"testing"

	"claude-repld/integration/harness"
)

// TestMain runs the grep gate, then hands off to harness.MainAt (seam 1),
// which builds claude-repld (and the fake shim / fake git this package never
// uses — they are harmless byproducts of a build routine this suite does not
// own) before running the suite.
func TestMain(m *testing.M) {
	os.Exit(runSuite(m))
}

func runSuite(m *testing.M) int {
	l, err := resolveLayout()
	if err != nil {
		fmt.Fprintln(os.Stderr, "e2e:", err)
		return 1
	}
	repo = l

	if err := runGrepGate(l.e2eDir); err != nil {
		fmt.Fprintln(os.Stderr, "e2e: grep gate:", err)
		return 1
	}

	binDir, err := os.MkdirTemp("", "agentrepl-e2e-bin-")
	if err != nil {
		fmt.Fprintln(os.Stderr, "e2e: temp bin dir:", err)
		return 1
	}
	defer os.RemoveAll(binDir)
	e2eBinDir = binDir

	return harness.MainAt(m, l.daemonDir)
}

// ---------------------------------------------------------------------------
// Repository layout, resolved from this file's own location — correct
// wherever the worktree lives and whatever the test's cwd is.
// ---------------------------------------------------------------------------

type layout struct {
	e2eDir     string // .../modules/app/agent-repl/e2e
	repoDir    string // .../modules/app/agent-repl
	daemonDir  string // .../modules/app/agent-repl/daemon
	shimDir    string // .../modules/app/agent-repl/agent-shim/claude/shim
	storeDir   string // .../modules/app/agent-repl/agent-shim/shim-store
	sidecarDir string // .../modules/app/agent-repl/agent-shim/claude/shim-sidecar
}

var repo layout

func resolveLayout() (layout, error) {
	_, self, _, ok := runtime.Caller(0)
	if !ok {
		return layout{}, fmt.Errorf("runtime.Caller could not locate main_test.go")
	}
	l := layout{e2eDir: filepath.Dir(self)}
	l.repoDir = filepath.Dir(l.e2eDir)
	l.daemonDir = filepath.Join(l.repoDir, "daemon")
	l.shimDir = filepath.Join(l.repoDir, "agent-shim", "claude", "shim")
	l.storeDir = filepath.Join(l.repoDir, "agent-shim", "shim-store")
	l.sidecarDir = filepath.Join(l.repoDir, "agent-shim", "claude", "shim-sidecar")
	for name, dir := range map[string]string{
		"daemon module":  l.daemonDir,
		"shim source":    l.shimDir,
		"store module":   l.storeDir,
		"sidecar module": l.sidecarDir,
	} {
		if _, err := os.Stat(dir); err != nil {
			return layout{}, fmt.Errorf("%s not found at %s: %w", name, dir, err)
		}
	}
	return l, nil
}

// e2eBinDir holds every binary this package builds itself (the real shim
// bundle, the real store, the real sidecar), one per `go test` run, shared
// across every test. claude-repld and the (unused) fake shim/git live in
// harness's OWN temp dir, built by MainAt above.
var e2eBinDir string

// ---------------------------------------------------------------------------
// Loud, lazy, per-precondition builds. Each is built ONCE (sync.Once) on the
// first test that needs it, and SKIPS the calling test (never fails the
// whole run) when its precondition is absent. The harness never installs
// anything — a missing node_modules names the exact `npm ci` command and
// directory instead of running it.
// ---------------------------------------------------------------------------

var (
	nodeOnce sync.Once
	nodeBin  string
	nodeErr  error
)

// requireNode answers the `node` binary on PATH, skipping the test loudly if
// none is found.
func requireNode(t *testing.T) string {
	t.Helper()
	nodeOnce.Do(func() {
		nodeBin, nodeErr = exec.LookPath("node")
	})
	if nodeErr != nil {
		t.Skip("e2e: node not found on PATH")
	}
	return nodeBin
}

var (
	gitOnce sync.Once
	gitBin  string
	gitErr  error
)

// requireGit answers the real `git` binary on PATH, skipping the test loudly
// if none is found. This suite uses REAL git (ruling 4) — see world_test.go's
// "Real git" section.
func requireGit(t *testing.T) string {
	t.Helper()
	gitOnce.Do(func() {
		gitBin, gitErr = exec.LookPath("git")
	})
	if gitErr != nil {
		t.Skip("e2e: git not found on PATH")
	}
	return gitBin
}

// shimBuildSHA is the fixed build identity every test's real shim bundle and
// daemon agree on. src/main.ts refuses to start without SHIM_BUILD_SHA in its
// spawn environment, and the daemon's stale-shim rollout check compares that
// runtime value against what it itself expects — a fixed constant on both
// sides means no test ever races that check.
const shimBuildSHA = "e2e-fixed-build-sha"

var (
	shimOnce sync.Once
	shimPath string
	shimErr  error
)

// requireShimBundle builds the real TypeScript shim from source into
// e2eBinDir, once per run, and answers its entry point (dist/main.js under a
// staged package.json, exactly as the daemon expects to spawn it). It skips
// loudly, naming the exact `npm ci` command, when the shim's dependencies are
// not installed; it never installs them itself. A genuine build failure
// (deps present, esbuild fails) is NOT a skip — it fails every test that
// needs the real shim, which is correct: the shim not compiling is a real
// defect this suite exists to catch.
func requireShimBundle(t *testing.T) string {
	t.Helper()
	node := requireNode(t)
	if _, err := os.Stat(filepath.Join(repo.shimDir, "node_modules")); err != nil {
		t.Skipf("e2e: shim deps not installed (%s/node_modules missing): run `npm ci` in %s",
			repo.shimDir, repo.shimDir)
	}
	shimOnce.Do(func() {
		shimPath, shimErr = buildShimBundle(node)
	})
	if shimErr != nil {
		t.Fatalf("e2e: build the real shim bundle: %v", shimErr)
	}
	return shimPath
}

// buildShimBundle bundles the TS shim FROM SOURCE, the way the deleted
// daemon/e2e's buildShim did: never from a stale, gitignored dist/, so this
// suite can never silently stop covering the source it exists to cover.
func buildShimBundle(node string) (string, error) {
	outDir := filepath.Join(e2eBinDir, "shim-dist")
	if err := os.MkdirAll(outDir, 0o755); err != nil {
		return "", fmt.Errorf("make shim build dir: %w", err)
	}
	pkg, err := os.ReadFile(filepath.Join(repo.shimDir, "package.json"))
	if err != nil {
		return "", fmt.Errorf("read shim package.json: %w", err)
	}
	// The bundle resolves its own version via a require of "../package.json"
	// relative to itself, so it is staged one directory up from the bundle.
	if err := os.WriteFile(filepath.Join(outDir, "..", "package.json"), pkg, 0o644); err != nil {
		return "", fmt.Errorf("stage shim package.json: %w", err)
	}
	out := filepath.Join(outDir, "main.js")
	cmd := exec.Command(node, "build.mjs")
	cmd.Dir = repo.shimDir
	cmd.Env = append(os.Environ(), "SHIM_BUILD_OUTFILE="+out)
	if combined, err := cmd.CombinedOutput(); err != nil {
		return "", fmt.Errorf("build shim bundle: %w\n%s", err, combined)
	}
	// Resolve symlinks: the daemon spawns the bundle by an exact path, and
	// Node resolves a module's own URL to the REAL path; a path through a
	// symlinked temp root (macOS: /var/folders/... -> /private/var/folders/...)
	// would make the bundle's argv[1] self-check compare unequal.
	real, err := filepath.EvalSymlinks(out)
	if err != nil {
		return "", fmt.Errorf("resolve shim bundle path: %w", err)
	}
	return real, nil
}

var (
	storeOnce sync.Once
	storePath string
	storeErr  error
)

// requireStoreBinary builds the real shim-store, once per run.
func requireStoreBinary(t *testing.T) string {
	t.Helper()
	storeOnce.Do(func() {
		storePath, storeErr = goBuildOnce(repo.storeDir, filepath.Join(e2eBinDir, "shim-store"))
	})
	if storeErr != nil {
		t.Fatalf("e2e: this suite runs against the REAL store, which does not build: %v", storeErr)
	}
	return storePath
}

var (
	sidecarOnce sync.Once
	sidecarPath string
	sidecarErr  error
)

// requireSidecarBinary builds the real shim-claude-sidecar, once per run.
func requireSidecarBinary(t *testing.T) string {
	t.Helper()
	sidecarOnce.Do(func() {
		sidecarPath, sidecarErr = goBuildOnce(repo.sidecarDir, filepath.Join(e2eBinDir, "shim-claude-sidecar"))
	})
	if sidecarErr != nil {
		t.Fatalf("e2e: this suite runs against the REAL sidecar, which does not build: %v", sidecarErr)
	}
	return sidecarPath
}

func goBuildOnce(moduleDir, out string) (string, error) {
	cmd := exec.Command("go", "build", "-o", out, ".")
	cmd.Dir = moduleDir
	if combined, err := cmd.CombinedOutput(); err != nil {
		return "", fmt.Errorf("go build in %s: %w\n%s", moduleDir, err, combined)
	}
	return out, nil
}

// ---------------------------------------------------------------------------
// The grep gate (SPEC.md section B, "Grep gate"). Runs once, before any
// test, over every *_test.go file this package's own directory holds
// (harness files included — they hold none of the forbidden shapes either).
//
// Two forbidden shapes:
//
//  1. A direct call to the store's one write verb, WriteBatch. Of
//     storev1connect.ShimStoreClient's eight methods, WriteBatch is the ONLY
//     one that is not Open-/Watch-/Get-prefixed (confirmed by inspection of
//     proto/gen/go/store/v1/storev1connect: OpenAgentSession,
//     WatchAgentSession, ReadAgentPage, WatchBashRun, GetWorkflow,
//     GetSidecarCursors, GetLiveWork, WriteBatch) — so a literal source scan
//     for a method call naming it is exact, not a heuristic. (This doc
//     comment spells the verb without its call syntax on purpose: written
//     out as a call it would trip the gate's own self-scan below.)
//  2. A literal path write (os.MkdirAll/os.WriteFile/os.Create) whose path
//     argument's own source line mentions a vendor-transcript location this
//     suite must never hand-author: "CLAUDE_CONFIG_DIR", "/projects/", or
//     this package's own config-root/spool-root field names.
//
// This is a small regex scan run from TestMain, not a build-tag trick, so it
// fails loudly and specifically, naming the offending file:line.
// ---------------------------------------------------------------------------

var (
	writeBatchCall  = regexp.MustCompile(`\.WriteBatch\s*\(`)
	pathWriteCall   = regexp.MustCompile(`os\.(MkdirAll|WriteFile|Create)\s*\(`)
	forbiddenPathIn = []string{
		"CLAUDE_CONFIG_DIR",
		"/projects/",
		"DefaultConfigDir",
		"MultiRepoConfigDir",
		"SpoolRoot",
	}
)

func runGrepGate(dir string) error {
	entries, err := os.ReadDir(dir)
	if err != nil {
		return fmt.Errorf("read %s: %w", dir, err)
	}
	var violations []string
	for _, entry := range entries {
		name := entry.Name()
		if entry.IsDir() || filepath.Ext(name) != ".go" || !bytes.HasSuffix([]byte(name), []byte("_test.go")) {
			continue
		}
		path := filepath.Join(dir, name)
		src, err := os.ReadFile(path)
		if err != nil {
			return fmt.Errorf("read %s: %w", path, err)
		}
		for i, line := range bytes.Split(src, []byte("\n")) {
			lineNo := i + 1
			if writeBatchCall.Match(line) {
				violations = append(violations, fmt.Sprintf("%s:%d: calls a store write verb (WriteBatch) directly — drive the shim/sidecar instead", path, lineNo))
			}
			if pathWriteCall.Match(line) {
				for _, forbidden := range forbiddenPathIn {
					if bytes.Contains(line, []byte(forbidden)) {
						violations = append(violations, fmt.Sprintf("%s:%d: hand-authors a path under %q — vendor files must come from the real shim's --fake writer, never a test", path, lineNo, forbidden))
						break
					}
				}
			}
		}
	}
	if len(violations) == 0 {
		return nil
	}
	msg := "the grep gate found forbidden shapes in this package's own test files:"
	for _, v := range violations {
		msg += "\n  " + v
	}
	return fmt.Errorf("%s", msg)
}
