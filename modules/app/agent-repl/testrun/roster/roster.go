// Package roster is THE list of agent-repl test suites: the names
// bin/test-all.sh's --suites accepts and the merge gate selects from. It is
// the one place the list lives; the runner (agentrepl/testrun) and the merge
// gate (claude-repld's internal/merge) both import it, so the two can no
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
}

// Suites is the roster. Order is report order only; the scheduler decides run
// order.
var Suites = []Suite{
	{Name: "orchestrator-harness", Kind: Script, Path: "bin/test-test-all.sh", Harness: true},
	{Name: "coverage-harness", Kind: Script, Path: "bin/test-report-nonlisp-coverage.sh", Harness: true},
	{Name: "logging-density-harness", Kind: Script, Path: "bin/test-report-logging-density.sh", Harness: true},
	{Name: "build-frontend-harness", Kind: SplitScript, Path: "bin/test-build-frontend.sh", Harness: true},
	{Name: "suite-slot-harness", Kind: Script, Path: "bin/test-suite-slot.sh", Harness: true},
	{Name: "background-harness", Kind: Script, Path: "bin/test-background.sh", Harness: true},
	{Name: "cpu-load-harness", Kind: Script, Path: "bin/test-with-cpu-load.sh", Harness: true},
	{Name: "store-reset-harness", Kind: Script, Path: "bin/test-store-reset.sh", Harness: true},
	{Name: "readiness-harness", Kind: SplitScript, Path: "bin/test-readiness-report.sh", Harness: true},
	{Name: "test-split-harness", Kind: Script, Path: "bin/test-lib-test-split.sh", Harness: true},
	{Name: "logs-harness", Kind: Script, Path: "bin/test-logs.sh", Harness: true},
	{Name: "go-deps-harness", Kind: Script, Path: "bin/test-check-go-deps.sh", Harness: true},
	{Name: "doctor-harness", Kind: Script, Path: "scripts/test-agent-shim-doctor.sh"},
	{Name: "precommit-harness", Kind: Script, Path: "/.githooks/test-pre-commit.sh"},
	{Name: "merge-queue-hook-harness", Kind: Script, Path: "/.githooks/test-reference-transaction.sh"},
	{Name: "merge-queue-skill-harness", Kind: Script, Path: "/.claude/skills/merge-queue/test-run.sh"},
	{Name: "ert", Kind: ERT, Path: "lisp"},
	{Name: "testrun", Kind: GoModule, Path: "testrun"},
	{Name: "daemon", Kind: GoModule, Path: "daemon"},
	{Name: "sidecar", Kind: GoModule, Path: "agent-shim/claude/shim-sidecar", PrebuildPackage: "integration"},
	{Name: "store", Kind: GoModule, Path: "agent-shim/shim-store"},
	{Name: "lock", Kind: GoModule, Path: "agent-shim/shim-lock"},
	{Name: "logging", Kind: GoModule, Path: "agent-shim/logging/go"},
	{Name: "webapp", Kind: Vitest, Path: "webapp"},
	{Name: "shim", Kind: Vitest, Path: "agent-shim/claude/shim"},
	{Name: "proto", Kind: Script, Path: "bin/report-nonlisp-coverage.sh", Args: []string{"proto"}},
	{Name: "logging-density", Kind: Script, Path: "bin/report-logging-density.sh"},
	{Name: "e2e", Kind: E2E, Path: "e2e"},
	{Name: "e2e-emacs", Kind: Script, Path: "bin/test-e2e-emacs.sh", MayDecline: true},
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
