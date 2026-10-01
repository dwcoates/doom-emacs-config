package merge

import (
	"fmt"
	"sort"
	"strings"

	"agentrepl/testrun/roster"
)

// This file decides WHICH of the target repository's suites the test gate runs
// for one merge.
//
// A suite that cannot be affected by a change cannot testify about it — it can
// only add a way for the merge to fail. So the gate narrows by BLAST RADIUS:
// each region of the repository maps to the suites a change there can break,
// which is not the same as the suites that share its name (every Go service
// compiles against the generated bindings and the shared logging module, so a
// change to either selects all of their consumers).
//
// UNKNOWN BEATS WRONG. The table is deliberately incomplete: a path matching no
// rule selects EVERY suite. Adding a directory to the repository therefore
// makes the gate more conservative, never less, and forgetting to map one costs
// time rather than correctness.

// AllSuites is the test runner's roster (agentrepl/testrun/roster), in roster
// order. A selection is reported in this order, so it reads the way the run
// reports. It IS the runner's list, imported rather than copied: a copy
// drifted seven suites behind it.
var AllSuites = roster.Names()

// scriptHarnessSuites are the suites that test the module's own bin/ scripts.
var scriptHarnessSuites = roster.Harnesses()

// moduleRoot is where the agent-repl module sits in the repository. Every rule
// is spelled relative to the repository root, because that is what a changed
// path is.
const moduleRoot = "modules/app/agent-repl/"

// matchKind is how one rule matches a repository-relative path.
type matchKind int

const (
	// matchSubtree matches every path under Path, which ends in "/".
	matchSubtree matchKind = iota
	// matchDirFile matches files sitting DIRECTLY in Path whose name ends in
	// Suffix. It exists for the module's three root-level elisp files, which sit
	// beside directories that map elsewhere.
	matchDirFile
	// matchExact matches one repository-relative path exactly.
	matchExact
)

// suiteRule maps a region of the repository to the suites a change there can
// affect. A nil Suites means the conservative full set.
type suiteRule struct {
	Kind   matchKind
	Path   string
	Suffix string
	Suites []string
}

// suiteRules is the mapping, most specific first: the FIRST rule that matches a
// path decides it. Each entry answers "a change here can break what?", and the
// answer is the blast radius rather than the component's own name.
var suiteRules = []suiteRule{
	// The runner itself and the scripts it delegates to. A change to any of them
	// can invalidate any suite in the roster, so they take the full set rather
	// than the harness subset their directory otherwise maps to.
	{Kind: matchSubtree, Path: moduleRoot + "testrun/"},
	{Kind: matchExact, Path: moduleRoot + "bin/test-all.sh"},
	{Kind: matchExact, Path: moduleRoot + "bin/report-nonlisp-coverage.sh"},
	{Kind: matchExact, Path: moduleRoot + "bin/report-logging-density.sh"},
	{Kind: matchSubtree, Path: moduleRoot + "bin/", Suites: scriptHarnessSuites},

	// The sandbox image and its runner are the Emacs client layer's
	// precondition and nothing else's: the containerless cross-system suite
	// never touches them.
	{Kind: matchSubtree, Path: moduleRoot + "e2e/sandbox/", Suites: []string{"e2e-emacs"}},
	// Both cross-system suites live in e2e/ and share its harness.
	{Kind: matchSubtree, Path: moduleRoot + "e2e/", Suites: []string{"e2e", "e2e-emacs"}},

	{Kind: matchSubtree, Path: moduleRoot + "webapp/", Suites: []string{"webapp", "build-frontend-harness"}},
	// daemon/integration lives inside the daemon module, so the daemon suite
	// carries the end-to-end tests with it. The cross-system suites RUN a real
	// claude-repld, so a daemon change can break them too — blast radius, not
	// name. `e2e-emacs` rides along because the daemon is what Emacs's own
	// launcher spawns, which is the seam only that suite covers.
	{Kind: matchSubtree, Path: moduleRoot + "daemon/", Suites: []string{"daemon", "e2e", "e2e-emacs"}},

	// Each of these three runs for real inside the cross-system suites.
	{Kind: matchSubtree, Path: moduleRoot + "agent-shim/claude/shim/", Suites: []string{"shim", "e2e"}},
	{Kind: matchSubtree, Path: moduleRoot + "agent-shim/claude/shim-sidecar/", Suites: []string{"sidecar", "e2e"}},
	{Kind: matchSubtree, Path: moduleRoot + "agent-shim/shim-store/", Suites: []string{"store", "e2e"}},
	{Kind: matchSubtree, Path: moduleRoot + "agent-shim/shim-lock/", Suites: []string{"lock", "e2e"}},
	{Kind: matchSubtree, Path: moduleRoot + "agent-shim/logging/", Suites: []string{"logging", "logging-density", "daemon", "store", "sidecar", "e2e"}},

	// The wire contract every producer and consumer is generated from.
	{Kind: matchSubtree, Path: moduleRoot + "proto/", Suites: []string{"proto", "daemon", "shim", "webapp", "e2e"}},

	// The module's elisp: every source and suite lives in lisp/, while the three
	// files Doom's module loader resolves by exact path (config.el, packages.el,
	// doctor.el) stay directly at the module root.
	// `e2e-emacs` joins `ert` here because it is the ONLY suite that drives
	// this elisp as a client against a real daemon: `ert` mocks Emacs's one
	// neighbour, so it cannot testify about the launcher's argv or a sentinel
	// recursion on close.
	{Kind: matchDirFile, Path: moduleRoot, Suffix: ".el", Suites: []string{"ert", "e2e-emacs"}},
	{Kind: matchSubtree, Path: moduleRoot + "lisp/", Suites: []string{"ert", "e2e-emacs"}},
}

// maxReasonPaths bounds how many touched paths the recorded reason names before
// it summarizes the rest. The reason is drawn, so it is bounded rather than
// complete; the daemon log carries the full list.
const maxReasonPaths = 6

// SuiteSelection is the gate's decision for one merge.
type SuiteSelection struct {
	// Suites are the selected suite names, in AllSuites order. It holds the FULL
	// roster when Full is true, so a caller never has to special-case the
	// conservative answer into an empty argument list.
	Suites []string
	// Full reports that the complete set was chosen, because a touched path
	// matched no rule or because no paths could be read.
	Full bool
	// Reason names the paths that drove the decision. It reads as an
	// explanation rather than a dump.
	Reason string
}

// matches reports whether one repository-relative path falls under this rule.
func (r suiteRule) matches(path string) bool {
	switch r.Kind {
	case matchExact:
		return path == r.Path
	case matchSubtree:
		return strings.HasPrefix(path, r.Path)
	case matchDirFile:
		rest, under := strings.CutPrefix(path, r.Path)
		return under && !strings.Contains(rest, "/") && strings.HasSuffix(rest, r.Suffix)
	}
	return false
}

// SelectSuites decides which suites the gate runs for a change touching paths.
//
// It is a pure function of the path set, so the decision is reproducible from
// the merge's own record: the same paths always select the same suites, in the
// same order.
//
// AN EMPTY PATH SET SELECTS THE FULL SET. "This merge touches nothing" is not a
// fact a real merge produces, so it means the paths could not be read — and a
// gate that narrowed itself on an unread change would be narrowing on nothing.
func SelectSuites(paths []string) SuiteSelection {
	if len(paths) == 0 {
		return SuiteSelection{Suites: append([]string(nil), AllSuites...), Full: true,
			Reason: "no changed paths were readable, so every suite runs"}
	}
	selected := map[string]bool{}
	var unmapped []string
	for _, path := range paths {
		rule, ok := ruleFor(path)
		if !ok {
			unmapped = append(unmapped, path)
			continue
		}
		if rule.Suites == nil {
			return SuiteSelection{Suites: append([]string(nil), AllSuites...), Full: true,
				Reason: fmt.Sprintf("%s can invalidate any suite, so every suite runs", path)}
		}
		for _, suite := range rule.Suites {
			selected[suite] = true
		}
	}
	if len(unmapped) > 0 {
		return SuiteSelection{Suites: append([]string(nil), AllSuites...), Full: true,
			Reason: fmt.Sprintf("%s map to no known blast radius, so every suite runs", namePaths(unmapped))}
	}
	return SuiteSelection{Suites: inRosterOrder(selected),
		Reason: fmt.Sprintf("%s select %s", namePaths(paths), strings.Join(inRosterOrder(selected), ", "))}
}

// ruleFor finds the first rule matching a path.
func ruleFor(path string) (suiteRule, bool) {
	for _, rule := range suiteRules {
		if rule.matches(path) {
			return rule, true
		}
	}
	return suiteRule{}, false
}

// inRosterOrder renders a selected set in roster order.
func inRosterOrder(selected map[string]bool) []string {
	var out []string
	for _, suite := range AllSuites {
		if selected[suite] {
			out = append(out, suite)
		}
	}
	return out
}

// namePaths renders a bounded, stable list of paths for a reason sentence.
func namePaths(paths []string) string {
	unique := map[string]bool{}
	for _, p := range paths {
		unique[p] = true
	}
	named := make([]string, 0, len(unique))
	for p := range unique {
		named = append(named, p)
	}
	sort.Strings(named)
	if len(named) <= maxReasonPaths {
		return strings.Join(named, ", ")
	}
	return fmt.Sprintf("%s and %d more", strings.Join(named[:maxReasonPaths], ", "), len(named)-maxReasonPaths)
}

// validateSuites refuses a name the roster does not declare, at selection
// time. The script would refuse it at run time, and a gate that discovers its
// own roster drift by failing a merge has told the user nothing useful.
func validateSuites(suites []string) error {
	known := map[string]bool{}
	for _, suite := range AllSuites {
		known[suite] = true
	}
	for _, suite := range suites {
		if !known[suite] {
			return fmt.Errorf("merge: %q is not a suite the test runner's roster declares", suite)
		}
	}
	return nil
}
