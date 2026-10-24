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
// THE INTENDED DESIGN, OWNER-RULED (2026-10-12, amended 2026-10-14):
// modules/app/agent-repl/AGENTS.md, "The merge queue's test gate and
// remediation contract". Do not change what a region selects, or what an empty
// selection does, without a conversation with the owner first.
//
// ONLY RELEVANT SYSTEMS ARE TESTED. A change to one system runs that system's
// own suites (its unit and integration tests) and the end-to-end suites that
// run that system for real: a system's own suites run every OTHER system as a
// mock (AGENTS.md, "A system's integration tests run that system alone"), so
// they never exercise another system, and only the end-to-end suites test the
// systems together. The wire contract selects every suite. A change to
// something several systems share -- the shared logging module, the
// cross-system harness itself -- fans out to its consumers plus the
// end-to-end suites.
//
// DOCUMENTATION SELECTS NOTHING, and neither does anything outside the module
// that no suite reads: the owner's own Doom configuration is exercised by no
// agent-repl suite. The repository paths a roster harness DOES read (the git
// hooks, the .claude scripts, the repository's bin/) select exactly the suites
// that read them. A selection that comes out EMPTY runs nothing at all
// (gate.go).
//
// UNKNOWN BEATS WRONG, INSIDE THE MODULE. The table is deliberately incomplete
// there: a module path matching no rule selects EVERY suite, so adding a
// directory to the module makes the gate more conservative, never less, and
// forgetting to map one costs time rather than correctness.

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
	// Suffix. It exists for the module's root-level files, which sit beside
	// directories that map elsewhere.
	matchDirFile
	// matchExact matches one repository-relative path exactly.
	matchExact
	// matchSubtreeFile matches files anywhere under Path whose name ends in
	// Suffix.
	matchSubtreeFile
)

// suiteRule maps a region of the repository to the suites a change there can
// affect. Full selects the conservative full set; otherwise Suites is the
// selection, and an EMPTY Suites selects nothing.
type suiteRule struct {
	Kind   matchKind
	Path   string
	Suffix string
	Full   bool
	Suites []string
}

// The suites that read the repository's own paths outside the module.
const (
	// backgroundHarness scans every test entry point of the repository: the
	// repository's bin/test-*.sh, the hooks and the .claude scripts.
	backgroundHarness = "background-harness"
	// testrunSuite's roster-coverage test globs the same test scripts.
	testrunSuite = "testrun"
	// worktreesLibHarness greps every .claude shell script.
	worktreesLibHarness = "worktrees-lib-harness"
	// repoScans reads every system's go.mod, Vitest imports, integration
	// sources and Go sources, and checks this selection and the proto helpers'
	// call sites against them (daemon/reposcans): a change to any system can
	// move its verdict.
	repoScans = "repo-scans"
)

// suiteRules is the mapping, most specific first: the FIRST rule that matches a
// path decides it. Each entry answers "a change here can break what?".
var suiteRules = []suiteRule{
	// The runner itself and the scripts it delegates to. A change to any of them
	// can invalidate any suite in the roster, so they take the full set rather
	// than the harness subset their directory otherwise maps to.
	{Kind: matchSubtree, Path: moduleRoot + "testrun/", Full: true},
	{Kind: matchExact, Path: moduleRoot + "bin/test-all.sh", Full: true},
	{Kind: matchExact, Path: moduleRoot + "bin/report-logging-density.sh", Full: true},
	// The sandboxed suites' scripts and the runner they share are those
	// suites' own, and the e2e module's sandboxscripts package tests them.
	{Kind: matchExact, Path: moduleRoot + "bin/lib-sandbox-suite.sh", Suites: []string{"e2e", "e2e-emacs"}},
	{Kind: matchExact, Path: moduleRoot + "bin/test-e2e-emacs.sh", Suites: []string{"e2e", "e2e-emacs"}},
	// repo-scans reads the Go sources here too.
	{Kind: matchSubtree, Path: moduleRoot + "bin/", Suites: append(append([]string(nil), scriptHarnessSuites...), repoScans)},

	// The sandbox image and its runner are the sandboxed suite's precondition
	// -- the Emacs client layer's and the Doom tier's, which runs in it -- and
	// nothing else's: the containerless cross-system suite never touches them.
	{Kind: matchSubtree, Path: moduleRoot + "e2e/sandbox/", Suites: []string{"e2e-emacs"}},
	// Both cross-system suites live in e2e/ and share its harness.
	{Kind: matchSubtree, Path: moduleRoot + "e2e/", Suites: []string{"e2e", "e2e-emacs", repoScans}},

	// ONE SYSTEM, ITS OWN SUITES, PLUS THE END-TO-END SUITES THAT RUN IT.
	// Every world of both end-to-end suites runs the store, the sidecar, the
	// daemon and its shims for real, and the Emacs layer draws the built
	// webapp, so each system selects both. The daemon suite carries the
	// daemon's integration tests (daemon/integration) with it.
	// THE WEBAPP'S PARSED-PROSE FIXTURE IS THE DAEMON'S ANSWER: the daemon's
	// prose suite writes it from its inputs and fails while it is stale, and
	// the webapp's tests draw from it.
	{Kind: matchSubtree, Path: moduleRoot + "webapp/test/fixtures/", Suites: []string{"daemon", "webapp", repoScans}},
	{Kind: matchSubtree, Path: moduleRoot + "webapp/", Suites: []string{"webapp", "build-frontend-harness", "e2e", "e2e-emacs", repoScans}},
	// The whole-module scans are their own suite; nothing else reads them.
	{Kind: matchSubtree, Path: moduleRoot + "daemon/reposcans/", Suites: []string{repoScans}},
	{Kind: matchSubtree, Path: moduleRoot + "daemon/", Suites: []string{"daemon", "e2e", "e2e-emacs", repoScans}},
	// The sidecar's mocked vendor IS the shim's fake mode, built from this tree.
	{Kind: matchSubtree, Path: moduleRoot + "agent-shim/claude/shim/", Suites: []string{"shim", "sidecar", "e2e", "e2e-emacs", repoScans}},
	{Kind: matchSubtree, Path: moduleRoot + "agent-shim/claude/shim-sidecar/", Suites: []string{"sidecar", "e2e", "e2e-emacs", repoScans}},
	{Kind: matchSubtree, Path: moduleRoot + "agent-shim/shim-store/", Suites: []string{"store", "e2e", "e2e-emacs", repoScans}},
	// The shim's integration files, and the sidecar's mocked vendor, spawn the
	// real lock helper.
	{Kind: matchSubtree, Path: moduleRoot + "agent-shim/shim-lock/", Suites: []string{"lock", "shim", "sidecar", "e2e", "e2e-emacs", repoScans}},

	// SHARED BY SEVERAL SYSTEMS: their consumers, plus the end-to-end suites.
	// The logging module's Go half is replaced into the daemon, the store, the
	// sidecar, the lock helper and the test runner; its TS half is imported by the shim and the
	// webapp (whose build harness builds the webapp).
	{Kind: matchSubtree, Path: moduleRoot + "agent-shim/logging/", Suites: []string{
		"logging", "logging-density", "daemon", "store", "sidecar", "lock", "shim", "webapp", "build-frontend-harness", testrunSuite, "e2e", "e2e-emacs", repoScans}},
	// The TLDR-tree grammars: their consumer, the daemon (which parses every
	// drawn text with tldr through cgo, internal/prose, and holds its reading
	// to tldr's golden answers), the end-to-end suites whose real daemon serves
	// that parse, and the scans that read the Go binding's go.mod. Each later
	// consumer joins this list as it starts parsing with them.
	{Kind: matchSubtree, Path: moduleRoot + "tree-sitter-tldr/", Suites: []string{"tree-sitter-tldr", "daemon", "e2e", "e2e-emacs", repoScans}},

	// The wire contract every producer and consumer is generated from, and the
	// Go helpers beside it: every system and suite reads it, so every suite
	// runs.
	{Kind: matchSubtree, Path: moduleRoot + "proto/", Full: true},

	// THE BRIEFS ARE THE DAEMON'S INPUT, not prose: the daemon composes every
	// merge and repair prompt from prompts/, its suite pins each brief's
	// placeholders (internal/prompts), and its integration harness copies them
	// into every test daemon. A broken placeholder would land untested and then
	// fail every merge's brief, so prompts/ selects the daemon suite.
	{Kind: matchSubtree, Path: moduleRoot + "prompts/", Suites: []string{"daemon", "e2e", "e2e-emacs"}},

	// DOCUMENTATION SELECTS NOTHING. The module's docs/, plans/ and org/
	// (the todo table of noticed deficiencies) hold prose, and so does every Markdown file at
	// the module root -- except metaprompt.md, whose committed text the shim's
	// own suite reads back (test/metaprompt.test.ts). Markdown deeper in the
	// tree belongs to its system's region above: a suite may read it (the
	// shim's AGENTS.md, a testdata MANIFEST.md, a skill's SKILL.md).
	{Kind: matchSubtree, Path: moduleRoot + "docs/", Suites: []string{}},
	{Kind: matchSubtree, Path: moduleRoot + "plans/", Suites: []string{}},
	{Kind: matchSubtree, Path: moduleRoot + "org/", Suites: []string{}},
	{Kind: matchExact, Path: moduleRoot + "metaprompt.md", Suites: []string{"shim", "sidecar", "e2e", "e2e-emacs"}},
	{Kind: matchDirFile, Path: moduleRoot, Suffix: ".md", Suites: []string{}},

	// The module's elisp: every source and suite lives in lisp/, while the three
	// files Doom's module loader resolves by exact path (config.el, packages.el,
	// doctor.el) stay directly at the module root. `ert` carries Emacs's unit
	// and integration tests both.
	{Kind: matchDirFile, Path: moduleRoot, Suffix: ".el", Suites: []string{"ert", "e2e-emacs"}},
	// e2e-emacs carries the Doom tier too, which boots this elisp in a real
	// Doom against the fake daemon under lisp/testsupport/.
	{Kind: matchSubtree, Path: moduleRoot + "lisp/", Suites: []string{"ert", "e2e-emacs"}},

	// OUTSIDE THE MODULE, the paths a roster harness reads select exactly the
	// suites that read them (roster.Suites spells them with a leading "/").
	// The hooks: each its own harness; the installer copies the pre-commit
	// hook; the background harness scans every hook; testrun's roster-coverage
	// test globs the hooks' test scripts.
	{Kind: matchExact, Path: ".githooks/pre-commit", Suites: []string{"precommit-harness", "install-harness", backgroundHarness}},
	{Kind: matchExact, Path: ".githooks/test-pre-commit.sh", Suites: []string{"precommit-harness", backgroundHarness, testrunSuite}},
	{Kind: matchExact, Path: ".githooks/reference-transaction", Suites: []string{"merge-queue-hook-harness", backgroundHarness}},
	{Kind: matchExact, Path: ".githooks/test-reference-transaction.sh", Suites: []string{"merge-queue-hook-harness", backgroundHarness, testrunSuite}},
	// push-release.sh sources the pre-push hook for its version lookup.
	{Kind: matchExact, Path: ".githooks/pre-push", Suites: []string{"release-tag-hook-harness", "push-release-harness", backgroundHarness}},
	{Kind: matchExact, Path: ".githooks/test-pre-push.sh", Suites: []string{"release-tag-hook-harness", backgroundHarness, testrunSuite}},
	{Kind: matchSubtree, Path: ".githooks/", Suites: []string{backgroundHarness, testrunSuite}},
	// The .claude scripts: the safe test runner and the installer each have
	// their harness; the background harness scans the top-level scripts, the
	// worktrees library's harness greps every one, and testrun's
	// roster-coverage test walks .claude for test scripts.
	{Kind: matchExact, Path: ".claude/safe-test-run.sh", Suites: []string{"safe-test-run-harness", backgroundHarness, worktreesLibHarness}},
	{Kind: matchExact, Path: ".claude/test-safe-test-run.sh", Suites: []string{"safe-test-run-harness", backgroundHarness, worktreesLibHarness, testrunSuite}},
	{Kind: matchExact, Path: ".claude/install.sh", Suites: []string{"install-harness", backgroundHarness, worktreesLibHarness}},
	{Kind: matchExact, Path: ".claude/test-install.sh", Suites: []string{"install-harness", backgroundHarness, worktreesLibHarness, testrunSuite}},
	{Kind: matchDirFile, Path: ".claude/", Suffix: ".sh", Suites: []string{backgroundHarness, worktreesLibHarness, testrunSuite}},
	{Kind: matchSubtreeFile, Path: ".claude/", Suffix: ".sh", Suites: []string{worktreesLibHarness, testrunSuite}},
	// The repository's bin/: the two workspace commands each have their
	// harness; the background harness and testrun's roster-coverage test read
	// its test scripts.
	{Kind: matchExact, Path: "bin/agent_repl_workspace", Suites: []string{"workspace-cli-harness"}},
	{Kind: matchExact, Path: "bin/test-agent_repl_workspace.sh", Suites: []string{"workspace-cli-harness", backgroundHarness, testrunSuite}},
	{Kind: matchExact, Path: "bin/agent_repl_workspace_open", Suites: []string{"workspace-open-cli-harness"}},
	{Kind: matchExact, Path: "bin/test-agent_repl_workspace_open.sh", Suites: []string{"workspace-open-cli-harness", backgroundHarness, testrunSuite}},
	{Kind: matchSubtree, Path: "bin/", Suites: []string{backgroundHarness, testrunSuite}},
}

// maxReasonPaths bounds how many touched paths the recorded reason names before
// it summarizes the rest. The reason is drawn, so it is bounded rather than
// complete; the daemon log carries the full list.
const maxReasonPaths = 6

// SuiteSelection is the gate's decision for one merge.
type SuiteSelection struct {
	// Suites are the selected suite names, in AllSuites order. It holds the FULL
	// roster when Full is true, so a caller never has to special-case the
	// conservative answer into an empty argument list. It is EMPTY when nothing
	// the merge changed is something a suite tests: the gate then runs nothing.
	Suites []string
	// Full reports that the complete set was chosen, because a touched module
	// path matched no rule, a path can invalidate any suite, or no paths could
	// be read.
	Full bool
	// Reason names the paths that drove the decision. It reads as an
	// explanation rather than a dump.
	Reason string
}

// Empty reports that the selection runs no suite at all.
func (s SuiteSelection) Empty() bool { return !s.Full && len(s.Suites) == 0 }

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
	case matchSubtreeFile:
		return strings.HasPrefix(path, r.Path) && strings.HasSuffix(path, r.Suffix)
	}
	return false
}

// FullSelection is the conservative answer for a change whose paths could not
// be read: a gate that narrowed itself on an unread change would be narrowing
// on nothing.
func FullSelection(reason string) SuiteSelection {
	return SuiteSelection{Suites: append([]string(nil), AllSuites...), Full: true, Reason: reason}
}

// SelectSuites decides which suites the gate runs for a change touching paths.
//
// It is a pure function of the path set, so the decision is reproducible from
// the merge's own record: the same paths always select the same suites, in the
// same order.
//
// AN EMPTY PATH SET SELECTS NOTHING: the branch changes nothing against the
// tip it stands on, so there is nothing for a suite to testify about. A path
// set that could not be READ is not empty; the caller answers FullSelection
// for it.
func SelectSuites(paths []string) SuiteSelection {
	if len(paths) == 0 {
		return SuiteSelection{Reason: "the branch changes no path against the tip it stands on, so no suite runs"}
	}
	selected := map[string]bool{}
	var unmapped []string
	for _, path := range paths {
		rule, ok := ruleFor(path)
		if !ok {
			// OUTSIDE THE MODULE, AN UNMAPPED PATH IS THE OWNER'S OWN
			// CONFIGURATION, which no suite exercises: it selects nothing.
			// Inside the module, unknown beats wrong.
			if strings.HasPrefix(path, moduleRoot) {
				unmapped = append(unmapped, path)
			}
			continue
		}
		if rule.Full {
			return FullSelection(fmt.Sprintf("%s can invalidate any suite, so every suite runs", path))
		}
		for _, suite := range rule.Suites {
			selected[suite] = true
		}
	}
	if len(unmapped) > 0 {
		return FullSelection(fmt.Sprintf("%s map to no known blast radius, so every suite runs", namePaths(unmapped)))
	}
	suites := inRosterOrder(selected)
	if len(suites) == 0 {
		return SuiteSelection{Reason: fmt.Sprintf("%s select no suite: documentation, or paths outside agent-repl that no suite reads", namePaths(paths))}
	}
	return SuiteSelection{Suites: suites,
		Reason: fmt.Sprintf("%s select %s", namePaths(paths), strings.Join(suites, ", "))}
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
