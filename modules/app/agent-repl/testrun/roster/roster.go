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
	// ERT is the Emacs suite, split by test file.
	ERT
	// GoModule is a Go module's tests with coverage, one unit per package.
	GoModule
	// Vitest is a TypeScript package: a typecheck, its test files split into
	// chunks, and a coverage merge.
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
}

// Suites is the roster. Order is report order only; the scheduler decides run
// order.
var Suites = []Suite{
	{Name: "orchestrator-harness", Kind: Script, Path: "bin/test-test-all.sh"},
	{Name: "coverage-harness", Kind: Script, Path: "bin/test-report-nonlisp-coverage.sh"},
	{Name: "logging-density-harness", Kind: Script, Path: "bin/test-report-logging-density.sh"},
	{Name: "build-frontend-harness", Kind: Script, Path: "bin/test-build-frontend.sh"},
	{Name: "suite-slot-harness", Kind: Script, Path: "bin/test-suite-slot.sh"},
	{Name: "background-harness", Kind: Script, Path: "bin/test-background.sh"},
	{Name: "cpu-load-harness", Kind: Script, Path: "bin/test-with-cpu-load.sh"},
	{Name: "store-reset-harness", Kind: Script, Path: "bin/test-store-reset.sh"},
	{Name: "readiness-harness", Kind: Script, Path: "bin/test-readiness-report.sh"},
	{Name: "logs-harness", Kind: Script, Path: "bin/test-logs.sh"},
	{Name: "go-deps-harness", Kind: Script, Path: "bin/test-check-go-deps.sh"},
	{Name: "doctor-harness", Kind: Script, Path: "scripts/test-agent-shim-doctor.sh"},
	{Name: "precommit-harness", Kind: Script, Path: "/.githooks/test-pre-commit.sh"},
	{Name: "merge-queue-hook-harness", Kind: Script, Path: "/.githooks/test-reference-transaction.sh"},
	{Name: "merge-queue-skill-harness", Kind: Script, Path: "/.claude/skills/merge-queue/test-run.sh"},
	{Name: "ert", Kind: ERT, Path: "lisp"},
	{Name: "testrun", Kind: GoModule, Path: "testrun"},
	{Name: "daemon", Kind: GoModule, Path: "daemon"},
	{Name: "sidecar", Kind: GoModule, Path: "agent-shim/claude/shim-sidecar"},
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

// Lookup finds a suite by name.
func Lookup(name string) (Suite, bool) {
	for _, s := range Suites {
		if s.Name == name {
			return s, true
		}
	}
	return Suite{}, false
}
