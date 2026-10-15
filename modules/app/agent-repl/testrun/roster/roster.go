// Package roster is THE list of agent-repl test suites: the names
// bin/test-all.sh's --suites accepts and the merge gate selects from. It is
// the one place the list lives; the runner (agentrepl/testrun) and the merge
// gate (agent-repld's internal/merge) both import it, so the two can no
// longer drift apart the way a hand-copied list did.
package roster

// Kind is how a suite is turned into units.
type Kind int

const (
	// Script is one shell entry point run whole, as one unit.
	Script Kind = iota
	// SplitScript is a shell harness implementing --list and --only.
	SplitScript
	// ERT is the Emacs suite, split by test file.
	ERT
	// GoModule is a Go module's tests, split by top-level test.
	GoModule
	// Vitest is a TypeScript package: a typecheck and split test files.
	Vitest
	// E2E is the cross-system Go suite: one build, its top-level tests split
	// into chunks.
	E2E
)

// Suite is one roster entry.
type Suite struct {
	Name string
	Kind Kind
	// Path is relative to the agent-repl module root: the script for a Script
	// suite (or, when it starts with "/", relative to the repository root),
	// the module or package directory otherwise.
	Path string
	// Args are a Script suite's arguments.
	Args []string
	// MayDecline marks the suites allowed to exit 77 ("precondition unmet").
	MayDecline bool
	// Harness marks a suite that tests the module's own shell scripts in bin/,
	// which is the blast radius the merge gate gives a change under bin/.
	Harness bool
	// IntegrationTags, with IntegrationPackages, adds a GoModule suite's
	// build-tagged integration package to the suite: its tests compile only
	// under these tags, so the module's ordinary pass never sees them.
	IntegrationTags string
	// VetTags names build tags whose files no unit of the suite compiles
	// otherwise (a perf phase, the realtests); each gets its own vet unit, so a
	// tagged file that stopped compiling fails the run instead of rotting.
	VetTags []string
	// IntegrationPackages is the `go list` pattern (relative to Path) the
	// integration pass covers.
	IntegrationPackages string
	// IntegrationConfig adds a Vitest suite's integration config (relative to
	// Path) to the suite: its files are a second split run under that config.
	IntegrationConfig string
	// IntegrationBuild is the command (run in Path) that builds what the
	// integration files spawn, run once before any of them. Empty means none.
	IntegrationBuild []string
	// Slots is a Script suite's width: the core slots its one unit holds.
	// Zero is one slot. A wider suite is told its width in
	// AGENT_REPL_UNIT_SLOTS and must cap itself to exactly that many cores.
	Slots int
	// TestParallel names the Go packages (by path relative to Path, "." for
	// the root) whose chunks overlap that many of their top-level tests
	// (-test.parallel). A package not named runs its tests one at a time.
	//
	// ONLY WAIT-BOUND PACKAGES ARE NAMED: tests that spend most of their wall
	// waiting on the processes they boot (a daemon, a store, a shim). MEASURED
	// on a full run (2026-10-11): those chunks held their slot at ~35% CPU one
	// test at a time; four at a time cut their wall 1.5-2.6x at ~0.9 cores
	// each, and eight at a time (2026-10-12) cut the full run's slot-seconds a
	// further 13% (2,056 -> 1,798) with every suite green.
	TestParallel map[string]int
	// Programs names, per Go package (by path relative to Path, "." for the
	// root), the shared programs (by Program.Name) that package's tests run.
	// The run builds each program ONCE (Programs) and every chunk of a package
	// named here waits for it and reads it from AGENT_REPL_TEST_PROGRAMS.
	Programs map[string][]string
	// Reuse marks a suite a run may skip when every input it reads is
	// byte-identical to inputs an earlier run passed whole
	// (agentrepl/testrun/internal/reuse). Nil means the suite ALWAYS runs: the
	// end-to-end suites run every system for real, and a shell harness's
	// inputs are a call graph no tool derives.
	Reuse *Reuse
}

// Reuse is what a keyed suite reads beyond its build graph. Its units run
// under a profile that refuses every read in the checkout outside the input
// set, so an input missing here fails the suite; it never lets a stale pass
// through.
//
// Paths are spelled as Suite.Path is: relative to the module root, or to the
// repository root when they start with "/". A path ending in "/" is a whole
// subtree; any other path is one file.
type Reuse struct {
	// Inputs are the files and subtrees the suite's tests read as data: the
	// prompts, the vocabulary, fixtures, and the trees a source scan walks.
	Inputs []string
	// Generated are subtrees the suite's own run builds or installs before it
	// reads them (a bundle the integration build writes, an npm install). They
	// are readable and never hashed, so each one must be produced by the run
	// itself from hashed inputs, never left over from an earlier one.
	Generated []string
}

// Program is one binary a run builds ONCE, into its programs directory, for
// every test process of every suite that runs it.
type Program struct {
	// Name is the binary's file name in the programs directory, and the name
	// a test asks testenv.Program for.
	Name string
	// Owner is the suite whose system the program is: the run reports the
	// program's build under it when it is selected, and otherwise under the
	// first selected suite that runs the program.
	Owner string
	// Module is the Go module directory, relative to the agent-repl module
	// root; Package is the package built, relative to Module.
	Module, Package string
	// Links are the -X bindings the build sets, each pointing a variable at
	// another program of the same run.
	Links []ProgramLink
}

// ProgramLink binds the package variable Var, at link time, to the path of the
// run's program Program.
type ProgramLink struct {
	Var     string
	Program string
}

// Programs is every program a run may build. A test build of the daemon runs
// the mocked git by construction (its gitclient seam, the daemon's integration
// harness's MockedGitLink).
var Programs = []Program{
	{Name: "agent-repld", Owner: "daemon", Module: "daemon", Package: "./cmd/agent-repld",
		Links: []ProgramLink{{Var: "agent-repld/internal/gitclient.gitBinary", Program: "git"}}},
	{Name: "fakeshim", Owner: "daemon", Module: "daemon", Package: "./integration/fakeshim"},
	{Name: "git", Owner: "daemon", Module: "daemon", Package: "./integration/fakegit/git"},
	{Name: "shim-store", Owner: "store", Module: "agent-shim/shim-store", Package: "."},
	{Name: "shim-claude-sidecar", Owner: "sidecar", Module: "agent-shim/claude/shim-sidecar", Package: "."},
	{Name: "shim-lock", Owner: "lock", Module: "agent-shim/shim-lock", Package: "."},
}

// LookupProgram answers the program named name.
func LookupProgram(name string) (Program, bool) {
	for _, p := range Programs {
		if p.Name == name {
			return p, true
		}
	}
	return Program{}, false
}

// Suites is the roster. Order is report order only; the scheduler decides run
// order.
var Suites = []Suite{
	{Name: "orchestrator-harness", Kind: Script, Path: "bin/test-test-all.sh", Harness: true},
	{Name: "coverage-harness", Kind: Script, Path: "bin/test-report-nonlisp-coverage.sh", Harness: true},
	{Name: "logging-density-harness", Kind: Script, Path: "bin/test-report-logging-density.sh", Harness: true},
	{Name: "build-frontend-harness", Kind: SplitScript, Path: "bin/test-build-frontend.sh", Harness: true},
	{Name: "background-harness", Kind: Script, Path: "bin/test-background.sh", Harness: true},
	{Name: "cpu-load-harness", Kind: Script, Path: "bin/test-with-cpu-load.sh", Harness: true},
	{Name: "store-reset-harness", Kind: Script, Path: "bin/test-store-reset.sh", Harness: true},
	{Name: "requirements-harness", Kind: Script, Path: "bin/test-requirements.sh", Harness: true},
	{Name: "worktrees-lib-harness", Kind: Script, Path: "bin/test-lib-worktrees.sh", Harness: true},
	{Name: "fake-git-fixture-lib-harness", Kind: Script, Path: "bin/test-lib-fake-git-fixture.sh", Harness: true},
	{Name: "emit-commands-harness", Kind: Script, Path: "bin/test-emit-workspace-commands.sh", Harness: true},
	{Name: "runtime-harness", Kind: Script, Path: "bin/test-agent-repl-runtime.sh", Harness: true},
	{Name: "contract-check-harness", Kind: Script, Path: "bin/test-daemon-contract-current.sh", Harness: true},
	{Name: "readiness-harness", Kind: SplitScript, Path: "bin/test-readiness-report.sh", Harness: true},
	{Name: "test-split-harness", Kind: Script, Path: "bin/test-lib-test-split.sh", Harness: true},
	{Name: "logs-harness", Kind: Script, Path: "bin/test-logs.sh", Harness: true},
	{Name: "profile-agent-repl-harness", Kind: Script, Path: "bin/test-profile-agent-repl.sh", Harness: true},
	{Name: "go-deps-harness", Kind: Script, Path: "bin/test-check-go-deps.sh", Harness: true},
	{Name: "personal-values-harness", Kind: Script, Path: "bin/test-check-personal-values.sh", Harness: true},
	{Name: "realtest-harness", Kind: Script, Path: "bin/test-realtest.sh", Harness: true},
	{Name: "e2e-coverage-harness", Kind: Script, Path: "bin/test-e2e-coverage.sh", Harness: true},
	{Name: "e2e-repeat-harness", Kind: Script, Path: "bin/test-e2e-repeat.sh", Harness: true},
	{Name: "doctor-harness", Kind: Script, Path: "scripts/test-agent-shim-doctor.sh"},
	{Name: "bounce-harness", Kind: Script, Path: "scripts/test-bounce-agent-repl-forcefully.sh"},
	{Name: "log-discovery-harness", Kind: Script, Path: "scripts/test-agent-repl-log-discovery.sh"},
	{Name: "precommit-harness", Kind: Script, Path: "/.githooks/test-pre-commit.sh"},
	{Name: "safe-test-run-harness", Kind: Script, Path: "/.claude/test-safe-test-run.sh"},
	{Name: "merge-queue-hook-harness", Kind: Script, Path: "/.githooks/test-reference-transaction.sh"},
	{Name: "merge-queue-skill-harness", Kind: Script, Path: "/.claude/skills/merge-queue/test-run.sh"},
	{Name: "install-harness", Kind: Script, Path: "/.claude/test-install.sh"},
	{Name: "workspace-cli-harness", Kind: Script, Path: "/bin/test-agent_repl_workspace.sh"},
	{Name: "workspace-open-cli-harness", Kind: Script, Path: "/bin/test-agent_repl_workspace_open.sh"},
	{Name: "ert", Kind: ERT, Path: "lisp"},
	{Name: "testrun", Kind: GoModule, Path: "testrun"},
	// THE INTEGRATION SUITES RIDE THEIR SYSTEM'S SUITE, so a full run and
	// `--suites daemon` (or webapp, shim) run unit and integration together,
	// as the owner's test policy asks of one complete change set (AGENTS.md,
	// "When to run which tests"). They used to be outside the roster entirely
	// (`make integration`, `npm run test:integration`), and so outside the
	// host slot too.
	{Name: "daemon", Kind: GoModule, Path: "daemon",
		IntegrationTags: "integration", IntegrationPackages: "./integration",
		TestParallel: map[string]int{"integration": 8},
		Programs:     map[string][]string{"integration": {"agent-repld", "fakeshim", "git"}},
		Reuse: &Reuse{Inputs: []string{
			// Source scans walk the daemon's own tree; the briefs and the
			// vocabulary are read as data.
			"daemon/", "prompts/", "proto/vocab/",
			// The harness's session-serving scan walks the e2e tree.
			"e2e/",
			// The command-file tests run the emitter script.
			"skills/emit-workspace-commands.sh",
			// gitclient pins the markers the merge queue's hook spells.
			"/.githooks/reference-transaction",
		}}},
	{Name: "sidecar", Kind: GoModule, Path: "agent-shim/claude/shim-sidecar", TestParallel: map[string]int{"integration": 8},
		Programs: map[string][]string{"integration": {"shim-claude-sidecar", "shim-lock"}},
		Reuse: &Reuse{Inputs: []string{
			"proto/vocab/agent-repl-tools.json", "testdata/corpus/", "testdata/projects/", "logging-contract.md",
			"agent-shim/claude/shim-sidecar/package.json",
			// The mocked vendor IS the shim's fake mode, bundled from the
			// shim's tree after bin/ensure-deps.sh installs its packages.
			"agent-shim/claude/shim/", "bin/ensure-deps.sh", "bin/lib-node-store.sh",
			// What the bundle imports from outside the shim.
			"proto/gen/ts/", "proto/ts/", "agent-shim/logging/ts/",
		}, Generated: []string{"agent-shim/claude/shim/node_modules/"}}},
	{Name: "store", Kind: GoModule, Path: "agent-shim/shim-store", TestParallel: map[string]int{"integration": 8},
		Programs: map[string][]string{"integration": {"shim-store"}},
		// internal/testclose's scan walks the store's own tree.
		Reuse: &Reuse{Inputs: []string{"agent-shim/shim-store/"}}},
	{Name: "lock", Kind: GoModule, Path: "agent-shim/shim-lock", Reuse: &Reuse{}},
	{Name: "logging", Kind: GoModule, Path: "agent-shim/logging/go", Reuse: &Reuse{Inputs: []string{"proto/vocab/"}}},
	{Name: "protohelpers", Kind: GoModule, Path: "proto/helpers/go", Reuse: &Reuse{}},
	// The tests that read the whole module rather than one system's tree:
	// the merge gate's selection checked against every suite's real
	// dependencies, the mock-rule source scan, and the scans pinning that
	// every Go reader goes through the proto helpers. Their own module, so no
	// system's reuse key covers the other systems' trees; they always run.
	{Name: "repo-scans", Kind: GoModule, Path: "daemon/reposcans"},
	{Name: "webapp", Kind: Vitest, Path: "webapp", IntegrationConfig: "vitest.integration.config.ts"},
	// The shim's integration files spawn its bundle and its lock helper
	// (dist/main.js, dist/shim-lock): `pretest:integration` builds both, as
	// `npm run test:integration` always has.
	{Name: "shim", Kind: Vitest, Path: "agent-shim/claude/shim", IntegrationConfig: "vitest.integration.config.ts",
		IntegrationBuild: []string{"npm", "run", "pretest:integration"}},
	{Name: "proto", Kind: Script, Path: "bin/report-nonlisp-coverage.sh", Args: []string{"proto"}},
	{Name: "logging-density", Kind: Script, Path: "bin/report-logging-density.sh"},
	{Name: "e2e", Kind: E2E, Path: "e2e", TestParallel: map[string]int{".": 8}, VetTags: []string{"perf", "realtest"},
		Programs: map[string][]string{".": {"agent-repld", "fakeshim", "git", "shim-store", "shim-claude-sidecar", "shim-lock"}}},
	// Two slots: the sandbox container is capped to the width it is given
	// (docker --cpus, GOMAXPROCS), and the layer runs its two Emacsen
	// (e2e/EMACS-LAYER-SPEC.md, "The parallelism bound, measured") on two
	// CPUs nearly as fast as on four -- 82s against 66s alone, worst Doom
	// boot 0.88s against its 3.5s bound (2026-10-12) -- while holding half
	// the run's slots: 264 -> 164 slot-seconds, a fifth of a full run's.
	{Name: "e2e-emacs", Kind: Script, Path: "bin/test-e2e-emacs.sh", MayDecline: true, Slots: 2},
}

// AlwaysRunRegions are the parts of the repository that only suites which
// ALWAYS run read: the end-to-end suites' tree and the scripts the harnesses
// test. Spelled as Suite.Path is; a "/" suffix is a subtree. Every file of the
// repository is placed somewhere (a keyed suite's input set, a region here, or
// ReadByNoSuite), or the run fails its placement check: a new directory is
// unplaced until someone decides who tests it.
var AlwaysRunRegions = []string{
	// The module's scripts and what their harnesses and the e2e suites read.
	"bin/", "scripts/", "skills/", "hooks/", "launchd/", "images/", "testdata/",
	// The proto suite (bin/report-nonlisp-coverage.sh proto) reads the
	// contract's sources; the keyed suites read what they import of
	// proto/gen through their build graph, and proto/vocab as declared data.
	"proto/",
	// The logging module's TS half is the Vitest suites' import, and they
	// always run.
	"agent-shim/logging/",
	// The Doom module files the Emacs layer loads, the metaprompt the shim
	// suite reads, the logging contract ERT reads, the requirement manifest
	// and timing history the harnesses read.
	"config.el", "doctor.el", "packages.el", "metaprompt.md", "logging-contract.md",
	"requirements.json", "test_time.csv",
	// Outside the module: the hooks, the .claude scripts and the repository's
	// bin/ each have their harness; the sandbox image reads .agents-sandbox.
	"/.githooks/", "/.claude/", "/bin/", "/.agents-sandbox/",
}

// ReadByNoSuite are the files and subtrees no suite reads: prose, and the
// owner's own Doom configuration. Each entry is reviewed: none may be broad
// enough to take in code a suite should test.
var ReadByNoSuite = []string{
	// The module's prose.
	"docs/", "plans/", "AGENTS.md", "agent-shim/AGENTS.md", ".gitattributes", ".gitignore",
	"DESIGN-protobuf-surfaces.md", "design-agent-shim-architecture.md", "design-async-response-streaming.md",
	"design-chessboard-widget.md", "design-progress-footer-samples.html", "design-progress-footer.md",
	"design-shim-transport-inversion.md", "design-watcher-bubble-expansion.md", "design-workspace-sidebar-mock.html",
	"output-navigation-assessment.md",
	"test-coverage-autosave.md", "test-coverage-commands.md", "test-coverage-core.md", "test-coverage-history.md",
	"test-coverage-input.md", "test-coverage-keybindings.md", "test-coverage-notifications.md",
	"test-coverage-overlay.md", "test-coverage-panels.md", "test-coverage-sentinel.md", "test-coverage-session.md",
	"test-coverage-status.md", "test-coverage-worktree.md",
	// The owner's own Doom configuration, which no agent-repl suite loads.
	"/config.el", "/init.el", "/packages.el", "/snippets/", "/modules/lang/", "/modules/config/", "/modules/app/chess/",
	"/clean-emacs.sh", "/.claude-repl-history", "/.gitignore",
	// The repository's prose.
	"/docs/", "/AGENTS.md", "/CLAUDE.md", "/README.md", "/SIDECAR-AUDIT-1.md", "/STORE-AUDIT-1.md",
}

// NotSuites is every test-named script that is deliberately NOT a suite, with
// why. A harness file the roster neither runs nor lists here fails
// TestEveryHarnessScriptIsRosteredOrExcluded: test-realtest.sh and
// test-e2e-coverage.sh ran nowhere in the full run until 2026-10-06, and
// nothing said so. Paths are spelled as Suite.Path is.
var NotSuites = map[string]string{
	// The scheduler itself: it runs the roster, it is not in it.
	"bin/test-all.sh": "the runner every suite runs under",
	// A container-free entry to the e2e Go suite, which the roster runs as
	// the "e2e" suite directly (Kind E2E); rostering this would run it twice.
	"bin/test-e2e.sh": "a manual entry to the e2e suite the roster already runs",
}

// Names is the roster's names, in roster order.
func Names() []string {
	out := make([]string, len(Suites))
	for i, s := range Suites {
		out[i] = s.Name
	}
	return out
}

// Harnesses is the names of the suites that test the module's bin/ scripts.
func Harnesses() []string {
	var out []string
	for _, s := range Suites {
		if s.Harness {
			out = append(out, s.Name)
		}
	}
	return out
}

// Lookup finds a suite by name.
func Lookup(name string) (Suite, bool) {
	for _, s := range Suites {
		if s.Name == name {
			return s, true
		}
	}
	return Suite{}, false
}
