package merge

import (
	"fmt"
	"sort"
	"strings"
)

// This file decides WHICH of the target repository's suites the merge gate runs
// for a given merge.
//
// WHY THE GATE STOPPED RUNNING EVERYTHING. The gate used to hand the repository's
// entrypoint no arguments, so every merge ran all eighteen suites. That is a
// defensible default and an indefensible steady state: a four-file webapp-only
// merge was denied by a shell-harness suite and by a Go suite that shared not one
// line of code with the change, and neither verdict said anything about the work
// being merged. A suite that cannot be affected by a change cannot testify about
// it — it can only add a way for the merge to fail.
//
// UNKNOWN BEATS WRONG. The mapping below is deliberately incomplete: it names the
// roots whose blast radius is known, and every path that matches none of them
// selects the FULL set. Adding a directory to the repository therefore makes the
// gate more conservative, never less, and forgetting to map it costs time rather
// than correctness.

// SuiteSelection is the gate's decision for one merge: which suites run, and the
// human-readable reason it reached that answer.
type SuiteSelection struct {
	// Suites are the suite names to run, in the repository's own declaration
	// order. It is EMPTY when Full is true — an empty list is what the runner
	// passes through as "run everything", which is the entrypoint's own default.
	Suites []string
	// Full reports that the conservative complete set was chosen, either because
	// a touched path maps to no known root or because no paths could be read.
	Full bool
	// Reason names the paths that drove the decision. It travels onto the merge's
	// workspace record, so it must read as an explanation and not as a dump.
	Reason string
}

// matchKind is how one selection rule matches a repository-relative path.
type matchKind int

const (
	// matchSubtree matches every path under Path, which ends in "/".
	matchSubtree matchKind = iota
	// matchDirFile matches files sitting DIRECTLY in Path (which ends in "/")
	// whose name ends in Suffix. It exists for the module's top-level elisp,
	// which is a flat set of files beside directories that map elsewhere.
	matchDirFile
	// matchExact matches one repository-relative path exactly.
	matchExact
)

// suiteRule maps a region of the repository to the suites a change there can
// affect. Suites nil means the conservative full set.
type suiteRule struct {
	Kind   matchKind
	Path   string
	Suffix string
	Suites []string
}

// allSuites is the target repository's complete roster, in the order
// bin/test-all.sh runs them. It is the ordering every selection is reported in,
// so a selection reads the same way the run does.
//
// IT MUST TRACK bin/test-all.sh. A name here that the entrypoint does not know
// would be passed to `--suites` and rejected, which is why validateSuiteNames
// refuses an unknown name at selection time rather than at run time.
var allSuites = []string{
	"orchestrator-harness",
	"coverage-harness",
	"logging-density-harness",
	"build-frontend-harness",
	"deploy-harness",
	"readiness-harness",
	"doctor-harness",
	"precommit-harness",
	"ert",
	"daemon",
	"sidecar",
	"store",
	"wire",
	"logging",
	"webapp",
	"shim",
	"proto",
	"logging-density",
}

// scriptHarnessSuites are the suites that test the module's own shell scripts.
var scriptHarnessSuites = []string{
	"orchestrator-harness",
	"coverage-harness",
	"logging-density-harness",
	"build-frontend-harness",
	"deploy-harness",
	"readiness-harness",
}

// moduleRoot is where the agent-repl module sits in the repository. Every rule
// below is spelled relative to the repository root because that is what
// `git log --name-only` prints.
const moduleRoot = "modules/app/agent-repl/"

// suiteRules is the mapping, most specific first: the FIRST rule that matches a
// path decides it.
//
// Each entry answers one question — "a change here can break what?" — and the
// answer is the blast radius, not the component's own name. The Go services all
// compile against wire, the shared logging module and the generated protobuf
// bindings, so a change to any of those selects every consumer.
var suiteRules = []suiteRule{
	// The three scripts that ARE the runner for every other suite. A change to
	// one of them can invalidate any suite in the roster, so they take the full
	// set rather than the harness subset their directory otherwise maps to.
	{Kind: matchExact, Path: moduleRoot + "bin/test-all.sh"},
	{Kind: matchExact, Path: moduleRoot + "bin/report-nonlisp-coverage.sh"},
	{Kind: matchExact, Path: moduleRoot + "bin/report-logging-density.sh"},
	{Kind: matchSubtree, Path: moduleRoot + "bin/", Suites: scriptHarnessSuites},

	{Kind: matchSubtree, Path: moduleRoot + "webapp/", Suites: []string{"webapp", "build-frontend-harness"}},
	// daemon/e2e lives inside the daemon module, so the daemon suite carries the
	// end-to-end tests with it.
	{Kind: matchSubtree, Path: moduleRoot + "daemon/", Suites: []string{"daemon"}},

	{Kind: matchSubtree, Path: moduleRoot + "agent-shim/claude/shim/", Suites: []string{"shim"}},
	{Kind: matchSubtree, Path: moduleRoot + "agent-shim/claude/shim-sidecar/", Suites: []string{"sidecar"}},
	{Kind: matchSubtree, Path: moduleRoot + "agent-shim/shim-store/", Suites: []string{"store"}},
	{Kind: matchSubtree, Path: moduleRoot + "agent-shim/wire/", Suites: []string{"wire", "daemon", "store", "sidecar"}},
	{Kind: matchSubtree, Path: moduleRoot + "agent-shim/logging/", Suites: []string{"logging", "logging-density", "daemon", "store", "sidecar"}},

	// The wire contract every producer and consumer is generated from.
	{Kind: matchSubtree, Path: moduleRoot + "proto/", Suites: []string{"proto", "daemon", "shim", "webapp"}},

	// The module's elisp: every source and suite lives in lisp/, while the
	// three files Doom's module loader resolves by exact path (config.el,
	// packages.el, doctor.el) stay directly at the module root.
	{Kind: matchDirFile, Path: moduleRoot, Suffix: ".el", Suites: []string{"ert"}},
	{Kind: matchSubtree, Path: moduleRoot + "lisp/", Suites: []string{"ert"}},
}

// maxReasonPaths is how many touched paths the recorded reason names before it
// summarizes the rest. The reason lands on the merge's workspace record, which a
// frontend renders, so it is bounded rather than complete — the daemon log
// carries the full list.
const maxReasonPaths = 6

// SelectSuites decides which suites the gate runs for a change touching paths.
//
// It is a pure function of the path set so the decision is reproducible from the
// merge's own record: the same paths always select the same suites, in the same
// order.
//
// AN EMPTY PATH SET SELECTS THE FULL SET. "This merge touches nothing" is not a
// fact any real merge produces, so it means the paths could not be read, and a
// gate that narrowed itself on an unread change would be narrowing on nothing.
func SelectSuites(paths []string) SuiteSelection {
	if len(paths) == 0 {
		return SuiteSelection{Full: true, Reason: "no changed paths could be read — running every suite"}
	}
	chosen := map[string]bool{}
	var unmapped []string
	for _, p := range paths {
		p = strings.TrimSpace(p)
		if p == "" {
			continue
		}
		rule, ok := matchRule(p)
		if !ok {
			unmapped = append(unmapped, p)
			continue
		}
		for _, s := range rule.Suites {
			chosen[s] = true
		}
		if len(rule.Suites) == 0 {
			// A rule that maps to the full set on purpose (the runner scripts).
			unmapped = append(unmapped, p)
		}
	}
	if len(unmapped) > 0 {
		return SuiteSelection{
			Full: true,
			Reason: fmt.Sprintf("%d of %d changed paths map to no known suite root (%s) — running every suite",
				len(unmapped), len(paths), summarizePaths(unmapped)),
		}
	}
	if len(chosen) == 0 {
		return SuiteSelection{Full: true, Reason: "every changed path was blank — running every suite"}
	}
	var suites []string
	for _, s := range allSuites {
		if chosen[s] {
			suites = append(suites, s)
		}
	}
	return SuiteSelection{
		Suites: suites,
		Reason: fmt.Sprintf("%d changed paths (%s) select %s",
			len(paths), summarizePaths(paths), strings.Join(suites, ",")),
	}
}

// matchRule returns the first rule matching path.
func matchRule(path string) (suiteRule, bool) {
	for _, r := range suiteRules {
		switch r.Kind {
		case matchExact:
			if path == r.Path {
				return r, true
			}
		case matchSubtree:
			if strings.HasPrefix(path, r.Path) {
				return r, true
			}
		case matchDirFile:
			if !strings.HasPrefix(path, r.Path) {
				continue
			}
			rest := path[len(r.Path):]
			if !strings.Contains(rest, "/") && strings.HasSuffix(rest, r.Suffix) {
				return r, true
			}
		}
	}
	return suiteRule{}, false
}

// summarizePaths renders a bounded, sorted sample of paths for a decision
// reason. Sorted because the reason must not change when git happens to list the
// same change in another order.
func summarizePaths(paths []string) string {
	sorted := append([]string(nil), paths...)
	sort.Strings(sorted)
	if len(sorted) <= maxReasonPaths {
		return strings.Join(sorted, ", ")
	}
	return fmt.Sprintf("%s and %d more", strings.Join(sorted[:maxReasonPaths], ", "), len(sorted)-maxReasonPaths)
}

// validateSuiteNames refuses a selection naming a suite the roster does not
// carry. A bad name would reach the entrypoint's `--suites` argument, which
// rejects it — turning a mapping typo into an unrunnable gate rather than a
// loud construction error here.
func validateSuiteNames(suites []string) error {
	known := map[string]bool{}
	for _, s := range allSuites {
		known[s] = true
	}
	for _, s := range suites {
		if !known[s] {
			return fmt.Errorf("merge: suite selection names %q, which bin/test-all.sh does not declare", s)
		}
	}
	return nil
}
