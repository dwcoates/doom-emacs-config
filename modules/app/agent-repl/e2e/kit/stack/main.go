// Package stack is the e2e suite's world: a real agent-repld, a real
// shim-store, a real shim-claude-sidecar, and the real TypeScript shim (built
// from source, running its `--fake` offline scripted vendor), composed
// exactly as production wires them, and the Emacs sandbox and the webapp
// layer's driver that stand a client in front of it. The e2e scenarios
// (agentrepl/e2e) build their worlds from it; its own tests run here, in the
// e2e-kit suite, which is reused, rather than in the e2e suite, which never
// is. See e2e/SPEC.md for the full design.
//
// Nothing here reaches inside any of the four systems. Every assertion an
// area test makes is on what crossed a real wire: the daemon's Connect API
// (agentreplv1connect.AgentReplClient) and its watch streams, or a
// structured-log record from one of the four processes.
//
// SYNCHRONIZATION IS ALWAYS A REAL SIGNAL — a watch-stream frame, a
// structured-log record, or a bounded poll of a store read verb. There is no
// time.Sleep anywhere in this package.
package stack

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"sync"
	"testing"

	"agent-repld/integration/harness"
	"agentrepl/testrun/testenv"
)

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
	zettelDir  string // .../modules/app/agent-repl/zettel
	// FakeDaemonDir is the Doom tier's fake daemon module.
	FakeDaemonDir string // .../modules/app/agent-repl/lisp/testsupport/fakedaemon
}

var Repo layout

// resolveLayout finds every tree the suite reads from the e2e package's
// directory, E2EDIR.
func resolveLayout(e2eDir string) (layout, error) {
	l := layout{e2eDir: e2eDir}
	l.repoDir = filepath.Dir(l.e2eDir)
	l.daemonDir = filepath.Join(l.repoDir, "daemon")
	l.shimDir = filepath.Join(l.repoDir, "agent-shim", "claude", "shim")
	l.storeDir = filepath.Join(l.repoDir, "agent-shim", "shim-store")
	l.lockDir = filepath.Join(l.repoDir, "agent-shim", "shim-lock")
	l.sidecarDir = filepath.Join(l.repoDir, "agent-shim", "claude", "shim-sidecar")
	l.zettelDir = filepath.Join(l.repoDir, "zettel")
	l.FakeDaemonDir = filepath.Join(l.repoDir, "lisp", "testsupport", "fakedaemon")
	for name, dir := range map[string]string{
		"daemon module":  l.daemonDir,
		"shim source":    l.shimDir,
		"store module":   l.storeDir,
		"sidecar module": l.sidecarDir,
		"fake daemon":    l.FakeDaemonDir,
	} {
		if _, err := os.Stat(dir); err != nil {
			return layout{}, fmt.Errorf("%s not found at %s: %w", name, dir, err)
		}
	}
	return l, nil
}

// e2eBinDir holds every binary this package builds itself (the real shim
// bundle, the real store, the real sidecar), one per `go test` run, shared
// across every test. agent-repld and the (unused) fake shim/git live in
// harness's OWN temp dir, built by MainAt above.
//
// IT LIVES UNDER THE HARNESS'S OWNER-LOCKED RUN ROOT, which MainAt lays out
// before any test runs and removes after the last, so a run that dies
// abnormally (a -timeout panic, a SIGKILL) leaves nothing the next run does
// not reclaim. It used to be its own os.MkdirTemp removed by a defer, which a
// dead run never reached.
func E2eBinDir() string {
	e2eBinOnce.Do(func() {
		// A PREBUILD process fills the shared directory every chunk reads.
		if mode, shared, err := testenv.BinaryMode(os.Getenv); err != nil || mode == testenv.BuildInto {
			if err != nil {
				e2eBinErr = err
				return
			}
			e2eBinPath, e2eBinErr = testenv.PrebuildDir(shared, e2eSharedSub)
			return
		}
		root := harness.RunRoot()
		if root == "" {
			e2eBinErr = fmt.Errorf("e2e: the harness run root is not laid out; TestMain must run the suite through harness.MainAt")
			return
		}
		e2eBinPath = filepath.Join(root, "e2e-bin")
		e2eBinErr = os.MkdirAll(e2eBinPath, 0o755)
	})
	if e2eBinErr != nil {
		panic(e2eBinErr)
	}
	return e2eBinPath
}

var (
	e2eBinOnce sync.Once
	e2eBinPath string
	e2eBinErr  error
)

// ---------------------------------------------------------------------------
// Loud, lazy, per-precondition builds. Each is built ONCE (sync.Once) on the
// first test that needs it. The harness never installs anything — a missing
// node_modules names the exact `npm ci` command and directory instead of
// running it — but naming it is not the same as tolerating it: an absent
// dependency FAILS the calling test through requireDependency, because a
// suite that covered nothing must never read as a pass. See
// precondition.go, including the AGENT_REPL_E2E_ALLOW_MISSING_DEPS
// opt-out that turns those failures back into (loudly summarized) skips.
// ---------------------------------------------------------------------------

var (
	nodeOnce sync.Once
	nodeBin  string
	nodeErr  error
)

// requireNode answers the `node` binary on PATH, failing the test loudly if
// none is found (a skip only under the opt-out; see precondition.go).
func requireNode(t testing.TB) string {
	t.Helper()
	nodeOnce.Do(func() {
		nodeBin, nodeErr = exec.LookPath("node")
	})
	if nodeErr != nil {
		requireDependency(t, "e2e: node not found on PATH: install Node.js (the shim bundle is built with it)")
	}
	return nodeBin
}

var (
	shimOnce sync.Once
	shimPath string
	shimErr  error
)

// requireShimBundle builds the real TypeScript shim from source into
// e2eBinDir, once per run, and answers its entry point (dist/main.js under a
// staged package.json, exactly as the daemon expects to spawn it). It fails
// loudly, naming the exact `npm ci` command, when the shim's dependencies are
// not installed; it never installs them itself. A genuine build failure
// (deps present, esbuild fails) is NOT a skip — it fails every test that
// needs the real shim, which is correct: the shim not compiling is a real
// defect this suite exists to catch.
func requireShimBundle(t testing.TB) string {
	t.Helper()
	node := requireNode(t)
	if _, err := os.Stat(filepath.Join(Repo.shimDir, "node_modules")); err != nil {
		requireDependency(t, "e2e: shim deps not installed (%s/node_modules missing): run `npm ci` in %s",
			Repo.shimDir, Repo.shimDir)
	}
	shimOnce.Do(func() {
		shimPath, shimErr = sharedOr(shimBundleName, func() (string, error) { return buildShimBundle(node) })
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
	// A shared prebuild's bundle stays in the shared directory every chunk
	// reads, coverage or not: that directory lives as long as the run, and a
	// chunk looks for the bundle nowhere else (sharedOr).
	root := E2eBinDir()
	if mode, _, err := testenv.BinaryMode(os.Getenv); err != nil {
		return "", err
	} else if covRoot := harness.CoverageRoot(); covRoot != "" && mode == testenv.BuildHere {
		root = filepath.Join(covRoot, "shim-bundle")
	}
	out := filepath.Join(root, shimBundleName)
	outDir := filepath.Dir(out)
	if err := os.MkdirAll(outDir, 0o755); err != nil {
		return "", fmt.Errorf("make shim build dir: %w", err)
	}
	if err := stageShimSiblings(outDir); err != nil {
		return "", err
	}
	cmd := exec.Command(node, "build.mjs")
	cmd.Dir = Repo.shimDir
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
	pkg, err := os.ReadFile(filepath.Join(Repo.shimDir, "package.json"))
	if err != nil {
		return fmt.Errorf("read shim package.json: %w", err)
	}
	mods := filepath.Join(Repo.shimDir, "node_modules")
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
			"Run `npm ci` in %s", probe, err, Repo.shimDir)
	}
	return nil
}

// programOnce is one Go program this suite builds at most once per run: the
// path the testenv prebuild names for NAME when the run handed one over
// (testenv.ProgramOr), else BUILD's. A failure is the run's, every caller's.
type ProgramOnce struct {
	Name  string
	Build func() (string, error)
	once  sync.Once
	path  string
	err   error
}

// fatalHelper is what programOnce.require needs of a test.
type fatalHelper interface {
	Helper()
	Fatalf(format string, args ...any)
}

// Require answers the program's path, built on the first call; a program
// that does not build fails T with WHY and the build's error.
func (p *ProgramOnce) Require(t fatalHelper, why string) string {
	t.Helper()
	p.once.Do(func() {
		p.path, p.err = testenv.ProgramOr(os.Getenv, p.Name, p.Build)
	})
	if p.err != nil {
		t.Fatalf("e2e: %s: %v", why, p.err)
	}
	return p.path
}

var storeProgram = &ProgramOnce{Name: "shim-store", Build: func() (string, error) {
	return goBuildCovered(Repo.storeDir, filepath.Join(E2eBinDir(), "shim-store"))
}}

// requireStoreBinary builds the real shim-store, once per run.
func requireStoreBinary(t testing.TB) string {
	t.Helper()
	return storeProgram.Require(t, "this suite runs against the REAL store, which does not build")
}

var sidecarProgram = &ProgramOnce{Name: "shim-claude-sidecar", Build: func() (string, error) {
	return goBuildCovered(Repo.sidecarDir, filepath.Join(E2eBinDir(), "shim-claude-sidecar"))
}}

// requireSidecarBinary builds the real shim-claude-sidecar, once per run.
func requireSidecarBinary(t testing.TB) string {
	t.Helper()
	return sidecarProgram.Require(t, "this suite runs against the REAL sidecar, which does not build")
}

var lockProgram = &ProgramOnce{Name: "shim-lock", Build: func() (string, error) {
	return GoBuildOnce(Repo.lockDir, filepath.Join(E2eBinDir(), "shim-lock"))
}}

// requireLockBinary builds the real shim-lock, once per run.
//
// EVERY SHIM THIS SUITE SPAWNS NEEDS IT. Node cannot take a flock, so the
// shim's session and workspace claims are shim-lock CHILD PROCESSES; a shim
// that cannot find the binary refuses every StartSession. The path travels to
// the shim as AGENT_REPL_SHIM_LOCK_BIN, so what is claimed here is the binary
// THIS checkout built, never whatever happens to be deployed on the machine.
func requireLockBinary(t testing.TB) string {
	t.Helper()
	return lockProgram.Require(t, "this suite runs against the REAL shim, whose lock holder does not build")
}

// e2eSharedSub is this suite's subdirectory of a shared prebuilt directory
// (testenv.Prebuild / testenv.Prebuilt); the harness keeps its own
// binaries beside it.
const e2eSharedSub = "e2e-bin"

// shimBundleName is the staged shim bundle's path under e2eBinDir.
const shimBundleName = "shim-dist/main.js"

// sharedOr answers a binary from the shared prebuilt directory when this
// process was told to read one, and builds it otherwise.
func sharedOr(name string, build func() (string, error)) (string, error) {
	mode, shared, err := testenv.BinaryMode(os.Getenv)
	if err != nil {
		return "", err
	}
	if mode == testenv.UsePrebuilt {
		return testenv.SharedBinary(shared, e2eSharedSub, name)
	}
	return build()
}

// prebuildE2E builds every binary this suite's tests would build lazily, into
// the shared directory, for a PREBUILD process. Each is built exactly as the
// lazy path builds it, by the same functions.
func prebuildE2E() error {
	node, err := exec.LookPath("node")
	if err != nil {
		return fmt.Errorf("e2e: prebuild: node not found on PATH: %w", err)
	}
	if _, err := os.Stat(filepath.Join(Repo.shimDir, "node_modules")); err != nil {
		return fmt.Errorf("e2e: prebuild: shim deps not installed (%s/node_modules missing): run `npm ci` in %s", Repo.shimDir, Repo.shimDir)
	}
	if _, err := harness.TimedStep(os.Stderr, "e2e", "prebuild the shim bundle", func() (string, error) { return buildShimBundle(node) }); err != nil {
		return fmt.Errorf("e2e: prebuild the shim bundle: %w", err)
	}
	return nil
}

// goBuildOnce builds one module's binary into out, uninstrumented.
func GoBuildOnce(moduleDir, out string) (string, error) {
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
	// -buildvcs=false: a test build never asks git to stamp the binary (no
	// test runs real git, owner rule).
	args := []string{"build", "-buildvcs=false"}
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

// RunSuite runs m as an e2e suite: it resolves the layout from e2eDir (the
// e2e package's directory), runs gate when there is one, builds the suite's
// shared binaries and returns when the run asks this process for a prebuild,
// and otherwise runs the tests under harness.MainAt (seam 1), which builds
// agent-repld (and the fake shim / fake git this package never uses — they
// are harmless byproducts of a build routine this suite does not own). After
// the tests it stops the warm webapp-layer driver, runs atExit when there is
// one, and reprints every unmet precondition.
func RunSuite(m *testing.M, e2eDir string, gate func(e2eDir string) error, atExit func()) int {
	l, err := resolveLayout(e2eDir)
	if err != nil {
		fmt.Fprintln(os.Stderr, "e2e:", err)
		return 1
	}
	Repo = l

	if gate != nil {
		if _, err := harness.TimedStep(os.Stderr, "e2e", "the grep gate", func() (struct{}, error) { return struct{}{}, gate(l.e2eDir) }); err != nil {
			fmt.Fprintln(os.Stderr, "e2e: grep gate:", err)
			return 1
		}
	}

	mode, _, err := testenv.BinaryMode(os.Getenv)
	if err != nil {
		fmt.Fprintln(os.Stderr, "e2e:", err)
		return 1
	}
	// A PREBUILD process runs no test. Build this suite's shared binaries
	// before MainAt enters its disposable run root: MainAt removes that root
	// when it returns, so no later Go build may inherit its TMPDIR.
	if mode == testenv.BuildInto {
		if err := prebuildE2E(); err != nil {
			fmt.Fprintln(os.Stderr, err)
			return 1
		}
		return 0
	}

	// The warm webapp-layer driver keeps its temp files OUTSIDE the run root
	// MainAt makes and removes, because the driver outlives MainAt's tests by
	// the moment it takes to stop (webapplayer_server.go).
	wlOuterTempDir = os.TempDir()
	code := harness.MainAt(m, l.daemonDir)
	// The warm webapp-layer driver, if an area started one, ends with the
	// tests that used it (webapplayer_server.go).
	wlStopLayerServer()
	if atExit != nil {
		atExit()
	}
	// A SKIP IS NOT A PASS: every unmet precondition this run hit, reprinted
	// where the reader of the summary line cannot miss it (precondition.go).
	reportSkipSummary()
	return code
}
