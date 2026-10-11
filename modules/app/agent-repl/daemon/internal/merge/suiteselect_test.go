package merge

import (
	"agentrepl/testrun/roster"
	"strings"
	"testing"
)

// TestSelectSuitesNarrowsByBlastRadius covers the ordinary case: paths whose
// blast radius is known select only the suites that can testify about them.
//
// A change to any system also selects repo-scans, which checks this selection
// against every system's go.mod, Vitest imports and integration sources.
func TestSelectSuitesNarrowsByBlastRadius(t *testing.T) {
	tests := []struct {
		name  string
		paths []string
		want  []string
	}{
		{
			name:  "a webapp change selects the webapp suite, the build harness and the end-to-end suites",
			paths: []string{"modules/app/agent-repl/webapp/src/App.tsx"},
			want:  []string{"build-frontend-harness", "webapp", "e2e", "e2e-emacs", "repo-scans"},
		},
		{
			name:  "a brief change selects the daemon suite that composes and pins it, and the end-to-end suites",
			paths: []string{"modules/app/agent-repl/prompts/merge-test-failure-resolve.md"},
			want:  []string{"daemon", "e2e", "e2e-emacs"},
		},
		{
			name:  "a daemon change selects the daemon suite and the end-to-end suites",
			paths: []string{"modules/app/agent-repl/daemon/internal/merge/run.go"},
			want:  []string{"daemon", "e2e", "e2e-emacs", "repo-scans"},
		},
		{
			name:  "the shared logging module selects its consumers and the cross-system suite",
			paths: []string{"modules/app/agent-repl/agent-shim/logging/go/log.go"},
			want:  []string{"daemon", "sidecar", "store", "lock", "shim", "webapp", "build-frontend-harness", "testrun", "logging", "logging-density", "e2e", "e2e-emacs", "repo-scans"},
		},
		{
			name:  "an ordinary bin script selects the script harnesses",
			paths: []string{"modules/app/agent-repl/bin/build-frontend.sh"},
			want:  append(append([]string(nil), scriptHarnessSuites...), "repo-scans"),
		},
		{
			name:  "module-root elisp selects ert, the Emacs layer, which carries the Doom tier",
			paths: []string{"modules/app/agent-repl/config.el"},
			want:  []string{"ert", "e2e-emacs"},
		},
		{
			name:  "lisp/ selects ert, the Emacs layer, which carries the Doom tier",
			paths: []string{"modules/app/agent-repl/lisp/core.el"},
			want:  []string{"ert", "e2e-emacs"},
		},
		{
			name:  "several regions union their selections",
			paths: []string{"modules/app/agent-repl/webapp/src/App.tsx", "modules/app/agent-repl/daemon/main.go"},
			want:  []string{"build-frontend-harness", "daemon", "webapp", "e2e", "e2e-emacs", "repo-scans"},
		},
		{
			name:  "the sandbox image selects only the sandboxed suite",
			paths: []string{"modules/app/agent-repl/e2e/sandbox/Dockerfile"},
			want:  []string{"e2e-emacs"},
		},
		{
			name:  "the e2e harness selects both cross-system suites",
			paths: []string{"modules/app/agent-repl/e2e/world_test.go"},
			want:  []string{"e2e", "e2e-emacs", "repo-scans"},
		},
		{
			name:  "the shared sandboxed-suite runner selects the suite it runs, and the tests of it",
			paths: []string{"modules/app/agent-repl/bin/lib-sandbox-suite.sh"},
			want:  []string{"e2e", "e2e-emacs"},
		},
		{
			name:  "a shim change selects the shim suite, the sidecar's mocked vendor and the end-to-end suites",
			paths: []string{"modules/app/agent-repl/agent-shim/claude/shim/src/main.ts"},
			want:  []string{"shim", "sidecar", "e2e", "e2e-emacs", "repo-scans"},
		},
		{
			name:  "a sidecar change selects the sidecar suite and the end-to-end suites",
			paths: []string{"modules/app/agent-repl/agent-shim/claude/shim-sidecar/main.go"},
			want:  []string{"sidecar", "e2e", "e2e-emacs", "repo-scans"},
		},
		{
			name:  "a store change selects the store suite and the end-to-end suites",
			paths: []string{"modules/app/agent-repl/agent-shim/shim-store/main.go"},
			want:  []string{"store", "e2e", "e2e-emacs", "repo-scans"},
		},
		{
			name:  "the committed metaprompt selects the shim suite that reads it back, and the end-to-end suites",
			paths: []string{"modules/app/agent-repl/metaprompt.md"},
			want:  []string{"shim", "sidecar", "e2e", "e2e-emacs"},
		},
		{
			name:  "documentation beside a daemon change adds nothing to the daemon's selection",
			paths: []string{"modules/app/agent-repl/docs/USER-GUIDE.md", "modules/app/agent-repl/daemon/main.go"},
			want:  []string{"daemon", "e2e", "e2e-emacs", "repo-scans"},
		},
		{
			name:  "the owner's configuration beside a daemon change adds nothing to the daemon's selection",
			paths: []string{"config.el", "modules/app/agent-repl/daemon/main.go"},
			want:  []string{"daemon", "e2e", "e2e-emacs", "repo-scans"},
		},
		{
			name:  "the pre-commit hook selects its harness, the installer's and the background scan",
			paths: []string{".githooks/pre-commit"},
			want:  []string{"precommit-harness", "install-harness", "background-harness"},
		},
		{
			name:  "the pre-commit hook's own test selects its harness, the background scan and testrun",
			paths: []string{".githooks/test-pre-commit.sh"},
			want:  []string{"precommit-harness", "background-harness", "testrun"},
		},
		{
			name:  "the reference-transaction hook selects its harness and the background scan",
			paths: []string{".githooks/reference-transaction"},
			want:  []string{"merge-queue-hook-harness", "background-harness"},
		},
		{
			name:  "the pre-push hook selects its harness, push-release's and the background scan",
			paths: []string{".githooks/pre-push"},
			want:  []string{"release-tag-hook-harness", "push-release-harness", "background-harness"},
		},
		{
			name:  "the pre-push hook's own test selects its harness, the background scan and testrun",
			paths: []string{".githooks/test-pre-push.sh"},
			want:  []string{"release-tag-hook-harness", "background-harness", "testrun"},
		},
		{
			name:  "another hook selects the background scan and testrun",
			paths: []string{".githooks/post-checkout"},
			want:  []string{"background-harness", "testrun"},
		},
		{
			name:  "the safe test runner selects its harness, the background scan and the worktrees library's",
			paths: []string{".claude/safe-test-run.sh"},
			want:  []string{"safe-test-run-harness", "background-harness", "worktrees-lib-harness"},
		},
		{
			name:  "the installer selects its harness, the background scan and the worktrees library's",
			paths: []string{".claude/install.sh"},
			want:  []string{"install-harness", "background-harness", "worktrees-lib-harness"},
		},
		{
			name:  "another top-level .claude script selects the scans that read it",
			paths: []string{".claude/check-external-boundaries.sh"},
			want:  []string{"background-harness", "worktrees-lib-harness", "testrun"},
		},
		{
			name:  "a deeper .claude script selects the worktrees library's harness and testrun",
			paths: []string{".claude/skills/other/run.sh"},
			want:  []string{"worktrees-lib-harness", "testrun"},
		},
		{
			name:  "the workspace command selects its harness",
			paths: []string{"bin/agent_repl_workspace"},
			want:  []string{"workspace-cli-harness"},
		},
		{
			name:  "the workspace-open command selects its harness",
			paths: []string{"bin/agent_repl_workspace_open"},
			want:  []string{"workspace-open-cli-harness"},
		},
		{
			name:  "the workspace command's test selects its harness, the background scan and testrun",
			paths: []string{"bin/test-agent_repl_workspace.sh"},
			want:  []string{"workspace-cli-harness", "background-harness", "testrun"},
		},
		{
			name:  "another repository bin script selects the background scan and testrun",
			paths: []string{"bin/new-tool"},
			want:  []string{"background-harness", "testrun"},
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: the changed paths are the whole input; selection is pure.
			want := inRosterOrder(setOf(tc.want))

			// Act.
			got := SelectSuites(tc.paths)

			// Assert.
			if got.Full {
				t.Fatalf("selection went full for %v, reason %q", tc.paths, got.Reason)
			}
			if strings.Join(got.Suites, ",") != strings.Join(want, ",") {
				t.Fatalf("selected %v, want %v", got.Suites, want)
			}
		})
	}
}

// TestSelectSuitesUnknownPathSelectsEverything covers the unknown-beats-wrong
// rule: one unmapped path widens the whole selection.
func TestSelectSuitesUnknownPathSelectsEverything(t *testing.T) {
	// Arrange: a mapped path alongside one that matches no rule.
	paths := []string{"modules/app/agent-repl/webapp/src/App.tsx", "modules/app/agent-repl/newthing/main.go"}

	// Act.
	got := SelectSuites(paths)

	// Assert.
	if !got.Full {
		t.Fatalf("an unmapped path did not select the full set: %v", got.Suites)
	}
	if len(got.Suites) != len(AllSuites) {
		t.Fatalf("full selection has %d suites, want the whole roster of %d", len(got.Suites), len(AllSuites))
	}
	if !strings.Contains(got.Reason, "modules/app/agent-repl/newthing/main.go") {
		t.Fatalf("the reason does not name the unmapped path: %q", got.Reason)
	}
}

// TestSelectSuitesRunnerScriptSelectsEverything covers the three scripts that
// ARE the runner: a change to one can invalidate any suite.
func TestSelectSuitesRunnerScriptSelectsEverything(t *testing.T) {
	tests := []string{
		"modules/app/agent-repl/bin/test-all.sh",
		"modules/app/agent-repl/testrun/internal/sched/plan.go",
		"modules/app/agent-repl/testrun/roster/roster.go",
		"modules/app/agent-repl/bin/report-nonlisp-coverage.sh",
		"modules/app/agent-repl/bin/report-logging-density.sh",
		"modules/app/agent-repl/proto/src/frontend/v1/feed.proto",
		"modules/app/agent-repl/proto/helpers/go/rosterwalk/rosterwalk.go",
	}
	for _, path := range tests {
		t.Run(path, func(t *testing.T) {
			// Arrange: the runner script alone.
			paths := []string{path}

			// Act.
			got := SelectSuites(paths)

			// Assert.
			if !got.Full {
				t.Fatalf("%s did not select the full set: %v", path, got.Suites)
			}
		})
	}
}

// TestSelectSuitesMapsShimLockToItsSuiteAndTheShims covers the shim-lock
// module: its own suite, plus the shim's, whose integration files spawn the
// real lock helper, and the end-to-end suites.
func TestSelectSuitesMapsShimLockToItsSuiteAndTheShims(t *testing.T) {
	// Act.
	got := SelectSuites([]string{"modules/app/agent-repl/agent-shim/shim-lock/main.go"})

	// Assert.
	want := strings.Join(inRosterOrder(setOf([]string{"lock", "shim", "sidecar", "e2e", "e2e-emacs", "repo-scans"})), ",")
	if got.Full || strings.Join(got.Suites, ",") != want {
		t.Fatalf("shim-lock selected %v (full=%v), want %s", got.Suites, got.Full, want)
	}
}

// TestSelectSuitesMapsTheGrammarToItsOwnSuite covers tree-sitter-tldr/: no
// program loads the grammar yet, so its own suite is the whole blast radius,
// with the scans that read its Go binding's go.mod.
func TestSelectSuitesMapsTheGrammarToItsOwnSuite(t *testing.T) {
	// Act.
	got := SelectSuites([]string{"modules/app/agent-repl/tree-sitter-tldr/grammar.js"})

	// Assert.
	if got.Full || strings.Join(got.Suites, ",") != "repo-scans,tree-sitter-tldr" {
		t.Fatalf("the grammar selected %v (full=%v), want repo-scans and tree-sitter-tldr", got.Suites, got.Full)
	}
}

// TestSelectSuitesABinScriptSelectsEveryHarness covers bin/: a change there
// can break any of the harnesses that test the module's scripts, and the
// repo-scans suite reads the Go sources there.
func TestSelectSuitesABinScriptSelectsEveryHarness(t *testing.T) {
	// Act.
	got := SelectSuites([]string{"modules/app/agent-repl/bin/background.sh"})

	// Assert.
	want := strings.Join(inRosterOrder(setOf(append(roster.Harnesses(), "repo-scans"))), ",")
	if got.Full || strings.Join(got.Suites, ",") != want {
		t.Fatalf("a bin/ change selected %v, want every bin/ harness and repo-scans: %s", got.Suites, want)
	}
}

// TestAllSuitesIsTheRunnersRoster covers the drift this list once had: it is
// the runner's own roster, in its order.
func TestAllSuitesIsTheRunnersRoster(t *testing.T) {
	if strings.Join(AllSuites, ",") != strings.Join(roster.Names(), ",") {
		t.Fatalf("AllSuites = %v, want the roster %v", AllSuites, roster.Names())
	}
}

// TestSelectSuitesSelectsNothing covers every change that runs no suite.
func TestSelectSuitesSelectsNothing(t *testing.T) {
	tests := []struct {
		name  string
		paths []string
	}{
		{name: "an empty diff", paths: nil},
		{name: "the module's docs", paths: []string{"modules/app/agent-repl/docs/USER-GUIDE.md"}},
		{name: "the module's plans", paths: []string{"modules/app/agent-repl/plans/lazy-tail-first-replay-plan.md"}},
		{name: "Markdown at the module root", paths: []string{"modules/app/agent-repl/AGENTS.md"}},
		{name: "the owner's Doom configuration", paths: []string{"config.el"}},
		{name: "another module of the configuration", paths: []string{"modules/lang/personal-cc/config.el"}},
		{name: ".claude prose", paths: []string{".claude/agents/opus-medium.md"}},
		{name: "the repository's settings", paths: []string{".claude/settings.json"}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			got := SelectSuites(tc.paths)

			// Assert.
			if !got.Empty() {
				t.Fatalf("%v selected %v (full=%v), want no suite", tc.paths, got.Suites, got.Full)
			}
		})
	}
}

// TestSelectSuitesNamesWhyNothingRuns covers the empty selection's reason: it
// names the paths that select nothing.
func TestSelectSuitesNamesWhyNothingRuns(t *testing.T) {
	// Act.
	got := SelectSuites([]string{"modules/app/agent-repl/docs/USER-GUIDE.md"})

	// Assert.
	if !strings.Contains(got.Reason, "modules/app/agent-repl/docs/USER-GUIDE.md") || !strings.Contains(got.Reason, "select no suite") {
		t.Fatalf("reason = %q, want the path and why it selects nothing", got.Reason)
	}
}

// TestFullSelectionIsTheWholeRoster covers the unread change's answer.
func TestFullSelectionIsTheWholeRoster(t *testing.T) {
	// Act.
	got := FullSelection("the paths could not be read")

	// Assert.
	if !got.Full || strings.Join(got.Suites, ",") != strings.Join(AllSuites, ",") || got.Empty() {
		t.Fatalf("FullSelection = %+v, want the whole roster", got)
	}
}

// TestSelectSuitesReportsInRosterOrder covers the ordering contract: a
// selection reads the way bin/test-all.sh runs.
func TestSelectSuitesReportsInRosterOrder(t *testing.T) {
	// Arrange: paths given in the reverse of the roster's order.
	paths := []string{
		"modules/app/agent-repl/webapp/src/App.tsx",
		"modules/app/agent-repl/lisp/core.el",
	}

	// Act.
	got := SelectSuites(paths)

	// Assert.
	if strings.Join(got.Suites, ",") != "build-frontend-harness,ert,repo-scans,webapp,e2e,e2e-emacs" {
		t.Fatalf("selection order is %v, want the roster's own order", got.Suites)
	}
}

// TestSelectSuitesBoundsTheReason covers the reason's bound: it explains rather
// than dumping every path.
func TestSelectSuitesBoundsTheReason(t *testing.T) {
	// Arrange: more unmapped paths than the reason names.
	paths := []string{"a/1", "a/2", "a/3", "a/4", "a/5", "a/6", "a/7", "a/8"}

	// Act.
	got := SelectSuites(paths)

	// Assert.
	if !strings.Contains(got.Reason, "and 2 more") {
		t.Fatalf("the reason did not summarize the overflow: %q", got.Reason)
	}
}

// TestValidateSuitesRefusesAnUnknownName covers the roster-drift guard: a name
// bin/test-all.sh does not declare is refused at selection time.
func TestValidateSuitesRefusesAnUnknownName(t *testing.T) {
	// Arrange: one real suite and one that never existed.
	suites := []string{"daemon", "not-a-suite"}

	// Act.
	err := validateSuites(suites)

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "not-a-suite") {
		t.Fatalf("validateSuites accepted an unknown name, err = %v", err)
	}
}

// TestSelectedSuitesAreAllDeclared covers the table's own integrity: every
// suite a rule can select is one the roster declares.
func TestSelectedSuitesAreAllDeclared(t *testing.T) {
	for _, rule := range suiteRules {
		t.Run(rule.Path, func(t *testing.T) {
			// Arrange: the rule's own suite list.
			suites := rule.Suites

			// Act.
			err := validateSuites(suites)

			// Assert.
			if err != nil {
				t.Fatalf("rule %s names a suite the roster does not declare: %v", rule.Path, err)
			}
		})
	}
}

// setOf turns a suite list into the set inRosterOrder consumes.
func setOf(suites []string) map[string]bool {
	out := map[string]bool{}
	for _, s := range suites {
		out[s] = true
	}
	return out
}
