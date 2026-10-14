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
	// PrebuildPackage names the Go package whose TestMain fills and consumes
	// shared binaries before its test chunks run. Empty means no prebuild.
	PrebuildPackage string
	// Harness marks a suite that tests the module's own shell scripts in bin/,
	// which is the blast radius the merge gate gives a change under bin/.
	Harness bool
	// IntegrationTags, with IntegrationPackages, adds a GoModule suite's
	// build-tagged integration package to the suite: its tests compile only
	// under these tags, so the module's ordinary pass never sees them.
	IntegrationTags string
	// IntegrationPackages is the `go list` pattern (relative to Path) the
	// integration pass covers.
	IntegrationPackages string
	// IntegrationPrebuildPackage is the integration package whose TestMain
	// fills and consumes shared binaries, as PrebuildPackage is for the
	// ordinary pass. Empty means no prebuild.
	IntegrationPrebuildPackage string
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
		IntegrationTags: "integration", IntegrationPackages: "./integration", IntegrationPrebuildPackage: "integration",
		TestParallel: map[string]int{"integration": 8}},
	{Name: "sidecar", Kind: GoModule, Path: "agent-shim/claude/shim-sidecar", PrebuildPackage: "integration", TestParallel: map[string]int{"integration": 8}},
	{Name: "store", Kind: GoModule, Path: "agent-shim/shim-store", TestParallel: map[string]int{"integration": 8}},
	{Name: "lock", Kind: GoModule, Path: "agent-shim/shim-lock"},
	{Name: "logging", Kind: GoModule, Path: "agent-shim/logging/go"},
	{Name: "protohelpers", Kind: GoModule, Path: "proto/helpers/go"},
	{Name: "webapp", Kind: Vitest, Path: "webapp", IntegrationConfig: "vitest.integration.config.ts"},
	// The shim's integration files spawn its bundle and its lock helper
	// (dist/main.js, dist/shim-lock): `pretest:integration` builds both, as
	// `npm run test:integration` always has.
	{Name: "shim", Kind: Vitest, Path: "agent-shim/claude/shim", IntegrationConfig: "vitest.integration.config.ts",
		IntegrationBuild: []string{"npm", "run", "pretest:integration"}},
	{Name: "proto", Kind: Script, Path: "bin/report-nonlisp-coverage.sh", Args: []string{"proto"}},
	{Name: "logging-density", Kind: Script, Path: "bin/report-logging-density.sh"},
	{Name: "e2e", Kind: E2E, Path: "e2e", TestParallel: map[string]int{".": 8}},
	// Two slots: the sandbox container is capped to the width it is given
	// (docker --cpus, GOMAXPROCS), and the layer runs its two Emacsen
	// (e2e/EMACS-LAYER-SPEC.md, "The parallelism bound, measured") on two
	// CPUs nearly as fast as on four -- 82s against 66s alone, worst Doom
	// boot 0.88s against its 3.5s bound (2026-10-12) -- while holding half
	// the run's slots: 264 -> 164 slot-seconds, a fifth of a full run's.
	{Name: "e2e-emacs", Kind: Script, Path: "bin/test-e2e-emacs.sh", MayDecline: true, Slots: 2},
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
