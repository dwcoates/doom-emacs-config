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
	"strings"
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

	sha, err := resolveBuildIdentity(l.repoDir)
	if err != nil {
		fmt.Fprintln(os.Stderr, "e2e: resolve the shim build identity:", err)
		return 1
	}
	buildIdentity = sha
	if err := checkBuildIdentityAgrees(); err != nil {
		fmt.Fprintln(os.Stderr, "e2e:", err)
		return 1
	}

	binDir, err := os.MkdirTemp("", "agentrepl-e2e-bin-")
	if err != nil {
		fmt.Fprintln(os.Stderr, "e2e: temp bin dir:", err)
		return 1
	}
	defer os.RemoveAll(binDir)
	e2eBinDir = binDir

	code := harness.MainAt(m, l.daemonDir)
	// The perf phase's final summary block (PERF-SPEC.md §D4), after every
	// test has reported. A no-op in the default build, where the perf files
	// are not compiled at all.
	perfReportSummary()
	return code
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
	lockDir    string // .../modules/app/agent-repl/agent-shim/shim-lock
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
	l.lockDir = filepath.Join(l.repoDir, "agent-shim", "shim-lock")
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

// shimBuildSHA is the build identity this suite WANTS on both sides:
// src/main.ts refuses to start without SHIM_BUILD_SHA in its spawn
// environment, and a fixed constant means no test races the daemon's
// stale-shim rollout check.
//
// It is only the FALLBACK, never the whole answer — see
// resolveBuildIdentity: the daemon prefers the shim bundle's own build stamp
// in the checkout over this variable, so a checkout that has ever run
// bin/build-frontend.sh makes the daemon export a real git sha instead and
// this constant would be a lie.
const shimBuildSHA = "e2e-fixed-build-sha"

// buildIdentity is the ONE sha every daemon in this run is started with, in
// BOTH roles: the identity it stamps onto every shim spawn (SHIM_BUILD_SHA)
// and the deployed build it compares that runtime identity against
// (AGENT_REPL_DEPLOY_STAMP). Resolved once by runSuite; read by
// buildIdentityEnv.
var buildIdentity string

// resolveBuildIdentity answers what the daemon will report as the shim's
// build identity when its checkout is repoDir, replicating the daemon's own
// resolution (claude-repld's resolveShimBuildSHA): the shim bundle's build
// stamp at agent-shim/claude/shim/dist/.built-sha wins, because that is the
// bundle a deployed daemon launches, and SHIM_BUILD_SHA answers only when
// that stamp is absent.
//
// The harness must resolve it the daemon's way rather than assert its own
// constant, because the two are DIFFERENT SOURCES otherwise: the deploy
// stamp the daemon reads (daemon/bin/.built-sha) and the shim stamp it
// exports are two real, independently written git shas, and a stale-shim
// bounce on every OpenWorkspace is what their disagreement looks like.
func resolveBuildIdentity(repoDir string) (string, error) {
	stamp := filepath.Join(repoDir, "agent-shim", "claude", "shim", "dist", ".built-sha")
	raw, err := os.ReadFile(stamp)
	switch {
	case err == nil:
		sha := strings.TrimSpace(string(raw))
		if sha == "" {
			return "", fmt.Errorf("the shim build stamp %s is empty: the daemon refuses to boot on it", stamp)
		}
		return sha, nil
	case os.IsNotExist(err):
		return shimBuildSHA, nil
	default:
		return "", fmt.Errorf("read the shim build stamp %s: %w", stamp, err)
	}
}

// buildIdentityEnv is the build-identity environment EVERY daemon this suite
// starts must carry, and the only place the three variables are named:
//
//   - AGENT_REPL_CHECKOUT pins the checkout the daemon resolves its stamps
//     from to this worktree, so the sha the harness read is the sha the
//     daemon reads (unpinned, a binary built into a temp dir can resolve a
//     DIFFERENT worktree's module root and its stamps).
//   - SHIM_BUILD_SHA is the identity every shim spawn reports, honored by the
//     shim's build-identity.ts (a live read of its spawn env, never baked
//     into the bundle) and by the daemon when no shim build stamp exists.
//   - AGENT_REPL_DEPLOY_STAMP is the deployed build the daemon's rollout
//     staleness check compares that report against. Setting it to the SAME
//     string is the whole point: any other value makes the daemon judge every
//     freshly spawned shim stale, bounce it, fail the bounce's resume ("no
//     transcript exists for vendor session") and answer every prompt
//     no_session.
func buildIdentityEnv() []string {
	return []string{
		checkoutEnv + "=" + repo.repoDir,
		"SHIM_BUILD_SHA=" + buildIdentity,
		"AGENT_REPL_DEPLOY_STAMP=" + buildIdentity,
	}
}

// checkoutEnv is claude-repld's checkout-root override (checkout.Env).
const checkoutEnv = "AGENT_REPL_CHECKOUT"

// checkBuildIdentityAgrees is this harness's self-check for the invariant
// buildIdentityEnv exists to hold: the sha the daemon will EXPORT to each
// shim and the sha it will read as its DEPLOYED build are one string. It runs
// before any test, because their disagreement does not fail visibly — it
// fails as a stale-shim bounce on every OpenWorkspace and a whole suite of
// no_session prompts.
func checkBuildIdentityAgrees() error {
	env := buildIdentityEnv()
	reported, deployed := valueOf(env, "SHIM_BUILD_SHA"), valueOf(env, "AGENT_REPL_DEPLOY_STAMP")
	if reported == "" || deployed == "" {
		return fmt.Errorf("build identity is unset: SHIM_BUILD_SHA=%q AGENT_REPL_DEPLOY_STAMP=%q", reported, deployed)
	}
	if reported != deployed {
		return fmt.Errorf("build identity disagrees: the shim will report SHIM_BUILD_SHA=%q "+
			"while the daemon reads AGENT_REPL_DEPLOY_STAMP=%q as its deployed build; "+
			"the daemon would judge every shim stale and bounce it on every OpenWorkspace",
			reported, deployed)
	}
	return nil
}

// valueOf answers the value of name in a KEY=VALUE list, or "" when absent.
func valueOf(env []string, name string) string {
	for _, kv := range env {
		if v, ok := strings.CutPrefix(kv, name+"="); ok {
			return v
		}
	}
	return ""
}

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
	// THE BUNDLE MUST OUTLIVE THE RUN ON A COVERAGE RUN. The v8 profiles name
	// the bundle by path and attribute back to src/**/*.ts only through the
	// map beside it, and both are read by the REPORTER, after this process
	// (and with it e2eBinDir) is gone. Under coverage the bundle is therefore
	// staged beneath the coverage root, which the reporter owns.
	root := e2eBinDir
	if covRoot := harness.CoverageRoot(); covRoot != "" {
		root = filepath.Join(covRoot, "shim-bundle")
	}
	outDir := filepath.Join(root, "shim-dist")
	if err := os.MkdirAll(outDir, 0o755); err != nil {
		return "", fmt.Errorf("make shim build dir: %w", err)
	}
	if err := stageShimSiblings(outDir); err != nil {
		return "", err
	}
	out := filepath.Join(outDir, "main.js")
	cmd := exec.Command(node, "build.mjs")
	cmd.Dir = repo.shimDir
	cmd.Env = append(os.Environ(), "SHIM_BUILD_OUTFILE="+out)
	// A COVERAGE RUN NEEDS THE BUNDLE'S SOURCE MAP. The shim runs as one
	// esbuild bundle, so v8 reports coverage against dist/main.js; only the
	// map attributes those ranges back to src/**/*.ts. The flag is read by
	// build.mjs and is OFF for every other build, so the production bundle's
	// bytes — and with them its build identity — are untouched.
	if harness.CoverageEnabled() {
		cmd.Env = append(cmd.Env, "SHIM_BUILD_SOURCEMAP=1")
	}
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
	if err := checkShimBundleLayout(outDir); err != nil {
		return "", err
	}
	return real, nil
}

// sdkProbe is the file whose presence beside the staged bundle proves the
// SDK is resolvable from it. The shim keeps @anthropic-ai/claude-agent-sdk
// EXTERNAL (agent-shim/claude/shim/build.mjs), so the bundle resolves it at
// runtime by walking up from its own URL; build-identity.ts's sdkPackageDir()
// does exactly that, on the `main` path, before anything else runs. Without
// this file the shim dies at startup with "Cannot find module".
const sdkProbe = "@anthropic-ai/claude-agent-sdk/package.json"

// stageShimSiblings reproduces, beside the staged bundle, the sibling layout
// bin/build-frontend.sh gives the production shim: a node_modules the bundle's
// runtime `require.resolve` walks into, and the package.json it reads its own
// version from. Both are staged at BOTH levels the bundle can reach —
// alongside main.js (Node's resolver walks up from the bundle's own
// directory) and one directory up (src/main.ts reads its version through a
// literal require of "../package.json") — so neither lookup depends on how
// deep in e2eBinDir the bundle happens to sit.
//
// Idempotent: TestMain re-entry re-stages over whatever a previous pass left.
func stageShimSiblings(outDir string) error {
	pkg, err := os.ReadFile(filepath.Join(repo.shimDir, "package.json"))
	if err != nil {
		return fmt.Errorf("read shim package.json: %w", err)
	}
	mods := filepath.Join(repo.shimDir, "node_modules")
	for _, dir := range []string{outDir, filepath.Dir(outDir)} {
		if err := os.WriteFile(filepath.Join(dir, "package.json"), pkg, 0o644); err != nil {
			return fmt.Errorf("stage shim package.json in %s: %w", dir, err)
		}
		link := filepath.Join(dir, "node_modules")
		if err := os.Remove(link); err != nil && !os.IsNotExist(err) {
			return fmt.Errorf("clear stale %s: %w", link, err)
		}
		if err := os.Symlink(mods, link); err != nil {
			return fmt.Errorf("link shim node_modules into %s: %w", dir, err)
		}
	}
	return nil
}

// checkShimBundleLayout is this harness's self-test for the invariant
// stageShimSiblings exists to hold: the vendor SDK the bundle deliberately
// does NOT contain must be resolvable from beside the bundle. It runs on
// every build, because a bundle that cannot find the SDK does not fail
// visibly here — it fails as ninety-odd unrelated tests timing out on a shim
// that died in its first millisecond.
func checkShimBundleLayout(outDir string) error {
	probe := filepath.Join(outDir, "node_modules", sdkProbe)
	if _, err := os.Stat(probe); err != nil {
		return fmt.Errorf("staged shim bundle cannot resolve the vendor SDK: %s is not readable (%w)\n"+
			"the shim keeps @anthropic-ai/claude-agent-sdk external and resolves it from beside the bundle; "+
			"without it every shim dies at startup with `Cannot find module`. "+
			"Run `npm ci` in %s", probe, err, repo.shimDir)
	}
	return nil
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
		storePath, storeErr = goBuildCovered(repo.storeDir, filepath.Join(e2eBinDir, "shim-store"))
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
		sidecarPath, sidecarErr = goBuildCovered(repo.sidecarDir, filepath.Join(e2eBinDir, "shim-claude-sidecar"))
	})
	if sidecarErr != nil {
		t.Fatalf("e2e: this suite runs against the REAL sidecar, which does not build: %v", sidecarErr)
	}
	return sidecarPath
}

var (
	lockOnce sync.Once
	lockPath string
	lockErr  error
)

// requireLockBinary builds the real shim-lock, once per run.
//
// EVERY SHIM THIS SUITE SPAWNS NEEDS IT. Node cannot take a flock, so the
// shim's session and workspace claims are shim-lock CHILD PROCESSES; a shim
// that cannot find the binary refuses every StartSession. The path travels to
// the shim as AGENT_REPL_SHIM_LOCK_BIN, so what is claimed here is the binary
// THIS checkout built, never whatever happens to be deployed on the machine.
func requireLockBinary(t *testing.T) string {
	t.Helper()
	lockOnce.Do(func() {
		lockPath, lockErr = goBuildOnce(repo.lockDir, filepath.Join(e2eBinDir, "shim-lock"))
	})
	if lockErr != nil {
		t.Fatalf("e2e: this suite runs against the REAL shim, whose lock holder does not build: %v", lockErr)
	}
	return lockPath
}

// goBuildOnce builds one module's binary into out, uninstrumented.
func goBuildOnce(moduleDir, out string) (string, error) {
	return goBuild(moduleDir, out, false)
}

// goBuildCovered builds one module's binary into out, INSTRUMENTED whenever
// the run collects coverage: `go build -cover` covers the built module's own
// packages, and this suite's spawn sites hand the resulting process its
// GOCOVERDIR.
//
// It is deliberately NOT used for shim-lock. That binary is spawned by the
// SHIM, not by this suite, so nothing can give it a GOCOVERDIR — and an
// instrumented Go binary started without one writes a warning to its stderr,
// which the shim reads. An unmeasured lock is better than a suite-wide
// spurious warning; see SPEC.md's Coverage section.
func goBuildCovered(moduleDir, out string) (string, error) {
	return goBuild(moduleDir, out, harness.CoverageEnabled())
}

func goBuild(moduleDir, out string, instrumented bool) (string, error) {
	args := []string{"build"}
	if instrumented {
		args = append(args, harness.CoverageBuildArgs(harness.CoverageRoot())...)
	}
	args = append(args, "-o", out, ".")
	cmd := exec.Command("go", args...)
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
