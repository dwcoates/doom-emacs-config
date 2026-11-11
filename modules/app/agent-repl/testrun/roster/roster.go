// Package roster is THE list of agent-repl test suites: the names
// bin/test-all.sh's --suites accepts and the merge gate selects from. It is
// the one place the list lives; the runner (agentrepl/testrun) and the merge
// gate (agent-repld's internal/merge) both import it, so the two can no
// longer drift apart the way a hand-copied list did.
package roster

import (
	"errors"
	"fmt"
	"path/filepath"
	"sort"
	"strings"
)

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

// RunCondition is when a suite runs.
type RunCondition int

const (
	// RunsAlways: every run that selects the suite runs it, a full run
	// included.
	RunsAlways RunCondition = iota
	// RunsInCoverage: only a coverage run, since the suite checks the
	// coverage numbers themselves; a full coverage run includes it, and a run
	// without coverage that names it is refused.
	RunsInCoverage
	// RunsWhenNamed: only a run that names it; no full run includes it. For a
	// measurement that needs its own run (the e2e perf phase).
	RunsWhenNamed
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
	// Runs is when the suite runs; the zero value is in every run that
	// selects it.
	Runs RunCondition
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
	// NeedsWebappDist marks a suite that reads the checkout's built webapp
	// (webapp/dist). The run builds it ONCE, before any unit starts
	// (cli.Deps.PrepareWebappDist), and the suite never builds it itself
	// (suites.NoWebappBuildEnv): a suite that rebuilt it mid-run rewrote files
	// another suite's watch-mode driver was reading, which restarted the
	// driver and failed tests that had nothing to do with the rebuild.
	NeedsWebappDist bool
	// TestParallel names the Go packages (by path relative to Path, "." for
	// the root) whose chunks overlap Tests of their top-level tests
	// (-test.parallel) on Slots cores. A package not named runs its tests one
	// at a time on one core.
	//
	// ONLY WAIT-BOUND PACKAGES ARE NAMED: tests that spend most of their wall
	// waiting on the processes they boot (a daemon, a store, a shim). MEASURED
	// on a full run (2026-10-11): those chunks held their slot at ~35% CPU one
	// test at a time; four at a time cut their wall 1.5-2.6x at ~0.9 cores
	// each, and eight at a time (2026-10-12) cut the full run's slot-seconds a
	// further 13% (2,056 -> 1,798) with every suite green.
	//
	// A CHUNK HOLDS THE CORES ITS OVERLAPPED TESTS USE TOGETHER. Every run on
	// the host shares one core budget (package cores) counted in widths, so a
	// chunk one slot wide that uses four spends three cores the budget never
	// gave it. MEASURED over the 100 runs before 2026-10-20 (each chunk's
	// reaped CPU over its wall): the chunks the run summary named as "not
	// pinned to its width" averaged 2.5 cores for daemon integration, 1.9
	// for e2e and 1.9 for sidecar integration, and daemon integration's
	// long chunks 1.0; store integration was never named. A chunk's short
	// bursts past its width are named by the summary still, and a package
	// whose chunks are named run after run needs its width raised.
	TestParallel map[string]Parallel
	// Programs names, per Go package (by path relative to Path, "." for the
	// root), the shared programs (by Program.Name) that package's tests run.
	// The run builds each program ONCE (Programs) and every chunk of a package
	// named here waits for it and reads it from AGENT_REPL_TEST_PROGRAMS.
	Programs map[string][]string
	// GoTests marks a Script suite whose script runs the Go tests of one
	// package where the runner cannot run them itself (inside the e2e
	// sandbox), and is the one home of which tests those are. The script
	// takes the -test.run pattern as its one argument: a whole run hands it
	// GoTests.Pattern, unchanged, and a run that names some of its tests
	// (`-harness SCRIPT=TestA,TestB`) hands it exactly those, so one failing
	// test reruns without the rest of the suite.
	GoTests *GoTests
	// Reuse declares what the suite's units read beyond what the runner
	// derives (agentrepl/testrun/internal/reuse): a run skips every unit whose
	// inputs are byte-identical to inputs an earlier run passed that unit on.
	// EVERY SUITE DECLARES IT, or names why it never can in NeverReused; Check
	// refuses a suite with neither, or with both.
	Reuse *Reuse
	// NeverReused is why the suite can never be reused: it runs whole in
	// every run that selects it, unconfined, and is never recorded. Only a
	// suite whose reads no profile can confine (a container's daemon reads
	// the checkout outside it) or whose verdict is not a function of its
	// inputs (a latency measurement) is named here.
	NeverReused string
}

// Reuse is what a keyed suite's units read beyond what the runner derives for
// each of them (a Go package's build graph from `go list -deps -test`, a
// vitest file's import graph, an ERT file's loaded sources, a script suite's
// own script). Every unit runs under a profile refusing every read in the
// checkout outside ITS input set, so an input missing here fails the unit; it
// never lets a stale pass through.
//
// THE UNIT is what the runner records one pass for: a Go package (unit or
// integration, named by its path relative to Suite.Path, "." the root, and
// "[tags]" after a tagged pass's: "integration[integration]"), a vitest test
// file or an ERT test file (named by its path relative to Suite.Path), a
// suite-wide check ("vet", "vet[perf]", "typecheck"), or, for a script or e2e
// suite, the whole suite (named "").
//
// Paths are spelled as Suite.Path is: relative to the module root, or to the
// repository root when they start with "/". A path ending in "/" is a whole
// subtree; any other path is one file.
type Reuse struct {
	// Inputs are the files and subtrees EVERY unit of the suite reads as data:
	// for a script or e2e suite, everything it reads beyond its script.
	Inputs []string
	// Generated are subtrees the suite's own run builds or installs before it
	// reads them (a bundle the integration build writes, an npm install). They
	// are readable and never hashed, so each one must be produced by the run
	// itself from hashed inputs, never left over from an earlier one.
	Generated []string
	// Units are what one unit reads beyond Inputs and its derived graph, by
	// unit name: the prompts, the vocabulary, the goldens, a tree a source
	// scan walks.
	Units map[string][]string
	// Never names each unit whose reads cannot be enumerated, with why: it
	// runs whenever its suite runs, under the suite's own envelope, and is
	// never recorded.
	Never map[string]string
}

// GoTests is the Go tests a Script suite's script runs.
type GoTests struct {
	// Dir is the package's directory, relative to the agent-repl module root.
	Dir string
	// Pattern is the -test.run pattern a whole run runs: every top-level test
	// of the package it matches.
	Pattern string
}

// Parallel is how a wait-bound Go package's chunks overlap their tests.
type Parallel struct {
	// Tests is how many top-level tests a chunk overlaps (-test.parallel).
	Tests int
	// Slots is the core slots a chunk holds: the cores its overlapped tests
	// use together on average, as measured. At least one.
	Slots int
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
	// Fake marks harness scaffolding rather than a system under test. A
	// coverage run never instruments one: a fake inherits the environment of
	// the system that spawns it, so its counters would land in that system's
	// coverage directory and read as covered production code.
	Fake bool
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
	{Name: "fakeshim", Owner: "daemon", Module: "daemon", Package: "./integration/fakeshim", Fake: true},
	{Name: "git", Owner: "daemon", Module: "daemon", Package: "./integration/fakegit/git", Fake: true},
	{Name: "shim-store", Owner: "store", Module: "agent-shim/shim-store", Package: "."},
	{Name: "shim-claude-sidecar", Owner: "sidecar", Module: "agent-shim/claude/shim-sidecar", Package: "."},
	{Name: "shim-lock", Owner: "lock", Module: "agent-shim/shim-lock", Package: "."},
	{Name: "zettel", Owner: "zettel", Module: "zettel", Package: "."},
}

// CoverageSystems are the suites whose code is a system the coverage
// baseline measures: each one's statement coverage, every tier together, is
// the reference a test removal is held to (testrun/coverage-baseline.json).
// The generated protobufs, the test tooling and the e2e harness are not.
var CoverageSystems = []string{"daemon", "sidecar", "store", "lock", "logging", "protohelpers", "webapp", "shim"}

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
	{Name: "orchestrator-harness", Kind: Script, Path: "bin/test-test-all.sh", Harness: true, Reuse: harness("bin/", "lisp/test-agent-repl.el", "test_time.csv", "testrun/", "/.claude/safe-test-run.sh")},
	{Name: "logging-density-harness", Kind: Script, Path: "bin/test-report-logging-density.sh", Harness: true, Reuse: harness("bin/")},
	{Name: "build-frontend-harness", Kind: SplitScript, Path: "bin/test-build-frontend.sh", Harness: true,
		Reuse: harness("bin/", "daemon/internal/checkout/checkout.go")},
	{Name: "background-harness", Kind: Script, Path: "bin/test-background.sh", Harness: true, Reuse: harness("bin/", "lisp/test-helpers.el", "e2e/sandbox/bin/entrypoint.sh", "/.githooks/pre-commit")},
	{Name: "e2e-perf-harness", Kind: Script, Path: "bin/test-e2e-perf.sh", Harness: true, Reuse: harness("bin/")},
	{Name: "cpu-load-harness", Kind: Script, Path: "bin/test-with-cpu-load.sh", Harness: true, Reuse: harness("bin/")},
	{Name: "store-reset-harness", Kind: Script, Path: "bin/test-store-reset.sh", Harness: true, Reuse: harness("bin/")},
	{Name: "requirements-harness", Kind: Script, Path: "bin/test-requirements.sh", Harness: true, NeverReused: "it walks the whole module for every requirements.json, so no input set short of the checkout covers it"},
	{Name: "push-release-harness", Kind: Script, Path: "bin/test-push-release.sh", Harness: true, Reuse: harness("bin/")},
	{Name: "slack-merged-reaction-harness", Kind: Script, Path: "bin/test-slack-merged-reaction.sh", Harness: true, Reuse: harness("bin/")},
	{Name: "worktrees-lib-harness", Kind: Script, Path: "bin/test-lib-worktrees.sh", Harness: true, Reuse: harness("bin/", "scripts/")},
	{Name: "fake-git-fixture-lib-harness", Kind: Script, Path: "bin/test-lib-fake-git-fixture.sh", Harness: true, Reuse: harness("bin/", "scripts/", "/.githooks/")},
	{Name: "harness-report-lib-harness", Kind: Script, Path: "bin/test-lib-harness-report.sh", Harness: true, Reuse: harness("bin/")},
	{Name: "runtime-harness", Kind: Script, Path: "bin/test-agent-repl-runtime.sh", Harness: true, Reuse: harness("bin/", "lisp/elisp-build.el")},
	{Name: "contract-check-harness", Kind: Script, Path: "bin/test-daemon-contract-current.sh", Harness: true, Reuse: harness("bin/")},
	{Name: "readiness-harness", Kind: SplitScript, Path: "bin/test-readiness-report.sh", Harness: true, Reuse: harness("bin/")},
	{Name: "test-split-harness", Kind: Script, Path: "bin/test-lib-test-split.sh", Harness: true, Reuse: harness("bin/")},
	{Name: "logs-harness", Kind: Script, Path: "bin/test-logs.sh", Harness: true, Reuse: harness("bin/")},
	{Name: "deploy-hook-probe-harness", Kind: Script, Path: "bin/test-deploy-hook-probe.sh", Harness: true, Reuse: harness("bin/")},
	{Name: "profile-agent-repl-harness", Kind: Script, Path: "bin/test-profile-agent-repl.sh", Harness: true, Reuse: harness("bin/", "proto/vocab/latencies.json")},
	{Name: "metrics-skill-harness", Kind: Script, Path: "skills/agent-repl-metrics/test-run.sh", Reuse: harness("skills/agent-repl-metrics/", "bin/")},
	{Name: "gns-auth-renew-skill-harness", Kind: Script, Path: "skills/gns-auth-renew/test-run.sh", Reuse: harness("skills/gns-auth-renew/", "bin/")},
	{Name: "go-deps-harness", Kind: Script, Path: "bin/test-check-go-deps.sh", Harness: true, Reuse: harness("bin/")},
	{Name: "personal-values-harness", Kind: Script, Path: "bin/test-check-personal-values.sh", Harness: true, Reuse: harness("bin/")},
	{Name: "realtest-harness", Kind: Script, Path: "bin/test-realtest.sh", Harness: true, Reuse: harness("bin/", "e2e/realtest/")},
	{Name: "doctor-harness", Kind: Script, Path: "scripts/test-agent-shim-doctor.sh", Reuse: harness("scripts/", "bin/")},
	{Name: "bounce-harness", Kind: Script, Path: "scripts/test-bounce-agent-repl-forcefully.sh", Reuse: harness("scripts/", "bin/")},
	{Name: "log-discovery-harness", Kind: Script, Path: "scripts/test-agent-repl-log-discovery.sh", Reuse: harness("scripts/", "bin/")},
	{Name: "precommit-harness", Kind: Script, Path: "/.githooks/test-pre-commit.sh", Reuse: harness("/.githooks/", "/.claude/", "bin/")},
	{Name: "safe-test-run-harness", Kind: Script, Path: "/.claude/test-safe-test-run.sh", Reuse: harness("/.claude/", "bin/", "lisp/test-agent-repl.el")},
	{Name: "merge-queue-hook-harness", Kind: Script, Path: "/.githooks/test-reference-transaction.sh", Reuse: harness("/.githooks/", "bin/")},
	{Name: "release-tag-hook-harness", Kind: Script, Path: "/.githooks/test-pre-push.sh", Reuse: harness("/.githooks/", "bin/")},
	{Name: "install-harness", Kind: Script, Path: "/.claude/test-install.sh", Reuse: harness("/.claude/", "/.githooks/", "bin/")},
	{Name: "workspace-cli-harness", Kind: Script, Path: "/bin/test-agent_repl_workspace.sh", Reuse: harness("/bin/", "bin/")},
	{Name: "workspace-open-cli-harness", Kind: Script, Path: "/bin/test-agent_repl_workspace_open.sh", Reuse: harness("/bin/", "bin/")},
	{Name: "ert", Kind: ERT, Path: "lisp", Reuse: &Reuse{Inputs: []string{
		// The Doom module files and the data the Emacs layer reads.
		"config.el", "doctor.el", "packages.el", "proto/vocab/", "images/", "skills/", "hooks/",
		"logging-contract.md", "metaprompt.md",
	}}},
	{Name: "testrun", Kind: GoModule, Path: "testrun", Reuse: &Reuse{
		// The canonical timing history, which the runner's own test parses.
		Units: map[string][]string{"internal/cli": {"test_time.csv"}},
		Never: map[string]string{
			"internal/reuse":  "its tests apply seatbelt profiles themselves, and the kernel refuses a sandbox inside another",
			"roster":          "its scans walk the whole repository for test scripts and every path the roster names",
			"internal/suites": "its tests plan every roster suite against the real tree, running the go command and Emacs over it",
		}}},
	// THE INTEGRATION SUITES RIDE THEIR SYSTEM'S SUITE, so a full run and
	// `--suites daemon` (or webapp, shim) run unit and integration together,
	// as the owner's test policy asks of one complete change set (AGENTS.md,
	// "When to run which tests"). They used to be outside the roster entirely
	// (`make integration`, `npm run test:integration`), and so outside the
	// host slot too.
	{Name: "daemon", Kind: GoModule, Path: "daemon",
		IntegrationTags: "integration", IntegrationPackages: "./integration",
		TestParallel: map[string]Parallel{"integration": {Tests: 8, Slots: 2}},
		Programs:     map[string][]string{"integration": {"agent-repld", "fakeshim", "git"}},
		// EACH PACKAGE IS REUSED ON ITS OWN, keyed on the build graph `go list
		// -deps -test` names for it; what its tests read beyond that graph is
		// declared per package, and every other read in the checkout is
		// refused while it runs.
		Reuse: &Reuse{Units: daemonUnitInputs}},
	{Name: "sidecar", Kind: GoModule, Path: "agent-shim/claude/shim-sidecar", TestParallel: map[string]Parallel{"integration": {Tests: 8, Slots: 2}},
		Programs: map[string][]string{"integration": {"shim-claude-sidecar"}},
		Reuse: &Reuse{Inputs: []string{
			"proto/vocab/agent-repl-tools.json", "testdata/corpus/", "testdata/projects/", "logging-contract.md",
			// The mocked vendor's scenarios, as the shim's suite generates and
			// keeps them fresh: this suite reads their files and never boots
			// the shim.
			"testdata/scenario-goldens/",
			// internal/recorded holds its home-token rule to the shim's capture
			// tooling's own vectors and script.
			"agent-shim/claude/shim/scripts/capture/",
		}}},
	{Name: "store", Kind: GoModule, Path: "agent-shim/shim-store", TestParallel: map[string]Parallel{"integration": {Tests: 8, Slots: 1}},
		Programs: map[string][]string{"integration": {"shim-store"}},
		// internal/testclose's scan walks the store's own tree, and
		// internal/history renders the stored-line cases the shim's reader
		// renders too.
		Reuse: &Reuse{Inputs: []string{"agent-shim/shim-store/", "testdata/history-golden/"}}},
	{Name: "lock", Kind: GoModule, Path: "agent-shim/shim-lock", Reuse: &Reuse{}},
	{Name: "zettel", Kind: GoModule, Path: "zettel", Programs: map[string][]string{"integration": {"zettel"}},
		Reuse: &Reuse{Inputs: []string{"zettel/"}}},
	{Name: "logging", Kind: GoModule, Path: "agent-shim/logging/go", Reuse: &Reuse{Inputs: []string{"proto/vocab/"}}},
	{Name: "protohelpers", Kind: GoModule, Path: "proto/helpers/go", Reuse: &Reuse{}},
	// The tests that read the whole module rather than one system's tree:
	// the merge gate's selection checked against every suite's real
	// dependencies, the mock-rule source scan, and the scans pinning that
	// every Go reader goes through the proto helpers. Their own module, so no
	// system's reuse key covers the other systems' trees.
	{Name: "repo-scans", Kind: GoModule, Path: "daemon/reposcans",
		NeverReused: "its scans read the whole module, every untracked build output in it included, so no input set short of the checkout covers them"},
	{Name: "webapp", Kind: Vitest, Path: "webapp", IntegrationConfig: "vitest.integration.config.ts",
		Reuse: &Reuse{Inputs: []string{
			// The vocabulary and the glyph pictures its tests read as data, and
			// the scripts its npm hooks run. What it imports, from inside its
			// package or out, is each file's import graph.
			"proto/vocab/", "images/",
			"bin/ensure-deps.sh", "bin/lib-node-store.sh", "bin/background.sh", "bin/require-background.mjs",
		},
			// THE BUILT DIST IS AN OUTPUT, built from the hashed sources by the
			// run itself (cli.Deps.PrepareWebappDist) and read by no webapp
			// test. Hashed, a run that rebuilt a stale dist after keying the
			// suite refused to record its green pass ("its inputs changed while
			// it ran", 2026-10-16).
			Generated: []string{"webapp/node_modules/", "webapp/.vite-cache/", "webapp/dist/"}}},
	// The shim's integration files spawn its bundle and its lock helper
	// (dist/main.js, dist/shim-lock): `pretest:integration` builds both, as
	// `npm run test:integration` always has.
	{Name: "shim", Kind: Vitest, Path: "agent-shim/claude/shim", IntegrationConfig: "vitest.integration.config.ts",
		IntegrationBuild: []string{"npm", "run", "pretest:integration"},
		Reuse: &Reuse{Inputs: []string{
			// The vocabulary, the lock helper its integration build compiles
			// with the logging module it imports,
			// the metaprompt its suite reads back, and the scripts its npm
			// hooks run. What it imports is each file's import graph.
			"proto/vocab/", "agent-shim/shim-lock/", "agent-shim/logging/go/", "metaprompt.md", "metaprompt-merge-queue.md", "metaprompt-queue-bypass.md", "testdata/",
			"bin/ensure-deps.sh", "bin/lib-node-store.sh", "bin/background.sh", "bin/require-background.mjs",
			// The capture script's tests run the fake git, which builds its model.
			"bin/fake-git.sh", "bin/fake-git.go",
		}, Generated: []string{"agent-shim/claude/shim/node_modules/", "agent-shim/claude/shim/dist/"}}},
	{Name: "proto", Kind: Script, Path: "proto/scripts/test-validate.sh", Reuse: harness("proto/", "bin/")},
	// The istanbul honesty probes (bin/coverage-honesty.mjs): whether each
	// package's per-file coverage numbers are still a measurement. They run
	// only in a coverage run, the one place those numbers are produced.
	{Name: "webapp-coverage-honesty", Kind: Script, Path: "bin/coverage-honesty.sh", Args: []string{"webapp"}, Runs: RunsInCoverage, NeverReused: onlyInCoverage},
	{Name: "shim-coverage-honesty", Kind: Script, Path: "bin/coverage-honesty.sh", Args: []string{"agent-shim/claude/shim"}, Runs: RunsInCoverage, NeverReused: onlyInCoverage},
	// The e2e perf phase (e2e/PERF-SPEC.md section D4): a latency
	// measurement that needs its own run, so each mode runs only when named.
	{Name: "e2e-perf", Kind: Script, Path: "bin/e2e-perf.sh", Args: []string{"report"}, Runs: RunsWhenNamed, NeverReused: perfMeasurement},
	{Name: "e2e-perf-enforce", Kind: Script, Path: "bin/e2e-perf.sh", Args: []string{"enforce"}, Runs: RunsWhenNamed, NeverReused: perfMeasurement},
	{Name: "e2e-perf-baseline", Kind: Script, Path: "bin/e2e-perf.sh", Args: []string{"baseline"}, Runs: RunsWhenNamed, NeverReused: perfMeasurement},
	// The TLDR-tree grammar: its corpus, and the committed parser checked
	// against what grammar.js generates.
	{Name: "tree-sitter-tldr", Kind: Script, Path: "tree-sitter-tldr/test-tree-sitter-tldr.sh",
		Reuse: &Reuse{Inputs: []string{"tree-sitter-tldr/", "bin/"}, Generated: []string{"tree-sitter-tldr/node_modules/"}}},
	// The density report scans every system's logging calls.
	{Name: "logging-density", Kind: Script, Path: "bin/report-logging-density.sh", Reuse: harness("bin/", "daemon/",
		"agent-shim/claude/shim-sidecar/", "agent-shim/shim-store/", "agent-shim/logging/go/", "agent-shim/claude/shim/src/",
		"webapp/src/", "lisp/")},
	{Name: "e2e", Kind: E2E, Path: "e2e", TestParallel: map[string]Parallel{".": {Tests: 8, Slots: 2}}, VetTags: []string{"perf", "realtest"},
		Programs:    map[string][]string{".": {"agent-repld", "fakeshim", "git", "shim-store", "shim-claude-sidecar", "shim-lock", "zettel"}},
		NeverReused: "its sandbox staging walks the whole module, every untracked build output in it included, so no input set short of the checkout covers it"},
	// Two slots: the sandbox container is capped to the width it is given
	// (docker --cpus, GOMAXPROCS), and the layer runs its two Emacsen
	// (e2e/EMACS-LAYER-SPEC.md, "The parallelism bound, measured") on two
	// CPUs nearly as fast as on four -- 82s against 66s alone, worst Doom
	// boot 0.88s against its 3.5s bound (2026-10-12) -- while holding half
	// the run's slots: 264 -> 164 slot-seconds, a fifth of a full run's.
	//
	// THE DOOM TIER RUNS HERE TOO (TestDoom*, e2e/doomtier_test.go): it boots
	// the same Doom in the same image, so it shares this suite's container
	// rather than paying a second container start for tests of about three
	// seconds each.
	{Name: "e2e-emacs", Kind: Script, Path: "bin/test-e2e-emacs.sh", Slots: 2, NeedsWebappDist: true,
		GoTests:     &GoTests{Dir: "e2e", Pattern: "TestEmacs|TestDoom"},
		NeverReused: "its tests run inside a docker container, whose daemon reads the checkout outside any profile the runner applies"},
}

// harness is a script suite's declaration: its one unit, the whole suite,
// reads its script and paths.
func harness(paths ...string) *Reuse { return &Reuse{Inputs: paths} }

// The reasons shared by several suites that are never reused.
const (
	onlyInCoverage  = "it runs only in a coverage run, which reuses nothing"
	perfMeasurement = "its verdict is a latency measurement of this run, not a function of its inputs"
)

// daemonUnitInputs is what each daemon package reads beyond its build graph,
// found by running every package confined to its build graph alone
// (2026-11-10, the integration pass 2026-11-11) and declaring each read it was
// refused.
var daemonUnitInputs = map[string][]string{
	// The structural source scans: each walks the daemon's tree, or the part
	// of it named, so any change there reruns it.
	".":                                   {"daemon/"},
	"internal/account":                    {"daemon/"},
	"internal/bounce":                     {"daemon/"},
	"internal/childproc":                  {"daemon/"},
	"internal/clock":                      {"daemon/internal/"},
	"internal/commandcatalog/catalogtest": {"daemon/internal/"},
	"internal/creategate":                 {"daemon/"},
	"internal/ctxend":                     {"daemon/"},
	"internal/dirpath":                    {"daemon/internal/"},
	"internal/effortlevel":                {"daemon/internal/"},
	"internal/envc":                       {"daemon/"},
	"internal/freshinput":                 {"daemon/internal/"},
	"internal/h2cclient":                  {"daemon/"},
	"internal/health":                     {"daemon/"},
	"internal/ids":                        {"daemon/internal/"},
	"internal/inflight":                   {"daemon/"},
	"internal/latestmodel":                {"daemon/internal/resolve/"},
	"internal/outputtext":                 {"daemon/"},
	"internal/procgroup":                  {"daemon/"},
	"internal/promptorigin":               {"daemon/"},
	"internal/promptqueue":                {"daemon/"},
	"internal/protoarm":                   {"daemon/"},
	"internal/ratelimittest":              {"daemon/"},
	"internal/recordlock":                 {"daemon/"},
	"internal/runecap":                    {"daemon/internal/"},
	"internal/shellword":                  {"daemon/"},
	"internal/sourcescan":                 {"daemon/"},
	"internal/sweepguard":                 {"daemon/internal/"},
	"internal/textline":                   {"daemon/internal/"},
	"internal/clock/clocktest": {"daemon/internal/clock/", "daemon/internal/newsdigest/", "daemon/internal/repodivergence/",
		"daemon/internal/worktreereap/"},
	// The scans pinning named call sites in other packages.
	"internal/atomicfile": {"daemon/internal/classifierupdate/update.go", "daemon/internal/daemonaddr/claim.go",
		"daemon/internal/rollout/carry.go", "daemon/internal/rollout/manifest.go", "daemon/internal/rollout/spawn.go"},
	"internal/briefprompt": {"daemon/internal/mainsync/mainsync.go", "daemon/internal/mergenotice/mergenotice.go",
		"daemon/internal/prwatch/poller.go", "daemon/internal/workspacenotice/workspacenotice.go",
		"daemon/internal/workspacesubs/workspacesubs.go"},
	"internal/standdown": {"daemon/internal/desktopnotify/notifier.go", "daemon/internal/desktopnotify/summary.go",
		"daemon/internal/mergeworkspace/mergeworkspace.go"},
	"internal/streamslot":    {"daemon/internal/desktopnotify/focus.go", "daemon/internal/procmetrics/emacs.go"},
	"internal/usersetup":     {"daemon/internal/externalbrowser/slack.go", "daemon/internal/pullrefs/pullrefs.go"},
	"internal/workspacesubs": {"daemon/internal/mergenotice/mergenotice.go"},
	// The briefs, read as data.
	"internal/classifierupdate": {"prompts/"},
	"internal/desktopnotify":    {"prompts/"},
	"internal/merge":            {"prompts/"},
	"internal/mergenotice":      {"prompts/"},
	"internal/namingdigest":     {"prompts/"},
	"internal/newsdigest":       {"prompts/"},
	"internal/prompts":          {"prompts/"},
	"internal/retarget":         {"prompts/"},
	"internal/revision":         {"prompts/"},
	"internal/wssummary":        {"prompts/"},
	"internal/resolve/feed":     {"prompts/", "daemon/internal/promptqueue/deliver.go"},
	"internal/headless":         {"daemon/"},
	"internal/workspace":        {"daemon/", "prompts/"},
	"internal/wsm":              {"daemon/"},
	// The vocabulary, read as data.
	"internal/buildid":         {"proto/vocab/"},
	"internal/dlog":            {"proto/vocab/"},
	"internal/handover":        {"proto/vocab/"},
	"internal/paint":           {"proto/vocab/"},
	"internal/resolve/footer":  {"proto/vocab/"},
	"internal/resolve/sidebar": {"proto/vocab/"},
	"internal/runtimemetrics":  {"proto/vocab/"},
	"internal/sessionwatcher":  {"proto/vocab/"},
	"internal/vocab":           {"daemon/", "proto/vocab/"},
	// The agent CLI's tests check every command a skill or a daemon brief
	// names, and that each API skill links the front door.
	"cmd/agent-repld": {"skills/", "prompts/", "bin/agent-repld"},
	// gitclient pins the markers the merge queue's hook spells.
	"internal/gitclient": {"/.githooks/reference-transaction"},
	// lifecycleconfig pins the repository's own lifecycle config to the
	// pipeline its queue runs, and to the scripts it names.
	"internal/lifecycleconfig": {"daemon/", "/.agent-repl/", "bin/test-all.sh", "bin/deploy-landing.sh", "bin/push-release.sh", "bin/hooks/"},
	// The harness's deploy configuration reads the briefs and the landing
	// script, and its scans read the integration suite's own sources.
	"integration/harness": {"prompts/", "bin/deploy-landing.sh", "daemon/integration/"},
	// prose holds the daemon's parse to the old webapp parser's golden
	// answers, and to the webapp's parsed-prose fixture.
	"internal/prose": {"tree-sitter-tldr/tldr/test/golden/golden.json", "webapp/test/fixtures/"},
	// The integration pass boots the daemon on the briefs, the vocabulary and
	// the repository's lifecycle config, and the daemon half of every
	// mocked-vendor scenario (integration/scenariogoldens_test.go) reads the
	// shim's goldens and keeps its own feed.json beside them.
	"integration[integration]": {"daemon/", "prompts/", "proto/vocab/", "skills/", "bin/agent-repld",
		"/.githooks/reference-transaction", "/.agent-repl/", "bin/test-all.sh", "bin/deploy-landing.sh",
		"bin/push-release.sh", "bin/hooks/", "testdata/scenario-goldens/"},
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
	// The metrics store's manifest and scrape config, which the requirements
	// harness reads and installs, as it does launchd/'s plists.
	"metrics/",
	// The proto suite (proto/scripts/test-validate.sh) reads the
	// contract's sources; the keyed suites read what they import of
	// proto/gen through their build graph, and proto/vocab as declared data.
	"proto/",
	// The tree-sitter-tldr suite (its own script, always run) reads both
	// grammars, their generated parsers and their corpora; the daemon reads
	// tldr's binding through its build graph and the golden answers as
	// declared data.
	"tree-sitter-tldr/",
	// The logging module's TS half is the Vitest suites' import, and they
	// always run.
	"agent-shim/logging/",
	// The Doom module files the Emacs layer loads, the metaprompt the shim
	// suite reads, the logging contract ERT reads, the requirement manifest
	// and timing history the harnesses read.
	"config.el", "doctor.el", "packages.el", "metaprompt.md", "metaprompt-merge-queue.md", "metaprompt-queue-bypass.md", "logging-contract.md",
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
	"docs/", "plans/", "org/", "AGENTS.md", "agent-shim/AGENTS.md", ".gitattributes", ".gitignore",
	"DESIGN-protobuf-surfaces.md", "design-agent-shim-architecture.md", "design-async-response-streaming.md",
	"design-chessboard-widget.md", "design-compact-orange.html", "design-dark-accent-tuner.html", "design-dark-palette-tuner.html", "design-green-lighter.html", "design-red-blue-darker.html", "design-selection-less-intense.html", "design-sidebar-selection-options.html", "design-progress-footer-samples.html", "design-progress-footer.md",
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

// Check refuses a roster whose reuse declarations cannot be what they say:
// a suite that declares neither its inputs (Reuse) nor why it never can be
// reused (NeverReused), or both, and a suite whose declaration both lists a
// unit's inputs and names that unit never reused. Every run starts with it,
// so an undeclared suite fails loudly instead of silently always running.
func Check(suites []Suite) error {
	var errs []error
	for _, s := range suites {
		never := strings.TrimSpace(s.NeverReused) != ""
		switch {
		case s.Reuse == nil && !never:
			errs = append(errs, fmt.Errorf("roster: %s declares no Reuse and no NeverReused reason: declare its inputs, or write why it can never be reused", s.Name))
		case s.Reuse != nil && s.NeverReused != "":
			errs = append(errs, fmt.Errorf("roster: %s declares both Reuse and NeverReused", s.Name))
		}
		if s.Reuse == nil {
			continue
		}
		var both []string
		for unit, why := range s.Reuse.Never {
			if strings.TrimSpace(why) == "" {
				errs = append(errs, fmt.Errorf("roster: %s names its unit %q never reused without a reason", s.Name, unit))
			}
			if _, ok := s.Reuse.Units[unit]; ok {
				both = append(both, unit)
			}
		}
		sort.Strings(both)
		for _, unit := range both {
			errs = append(errs, fmt.Errorf("roster: %s both declares its unit %q's inputs and names it never reused", s.Name, unit))
		}
	}
	return errors.Join(errs...)
}

// Resolve turns a roster path into an absolute one: a path starting with "/"
// is relative to the repository root REPO, any other to the agent-repl module
// root MODULE. It is the one reading of the convention Suite.Path, Reuse's
// paths and the placement lists are spelled in.
func Resolve(repo, module, p string) string {
	if strings.HasPrefix(p, "/") {
		return filepath.Join(repo, filepath.FromSlash(p[1:]))
	}
	return filepath.Join(module, filepath.FromSlash(p))
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
