package merge

import (
	"strings"
	"testing"
)

// TestSelectSuitesNarrowsByBlastRadius covers the ordinary case: paths whose
// blast radius is known select only the suites that can testify about them.
func TestSelectSuitesNarrowsByBlastRadius(t *testing.T) {
	tests := []struct {
		name  string
		paths []string
		want  []string
	}{
		{
			name:  "a webapp change selects the webapp suite and the build harness",
			paths: []string{"modules/app/agent-repl/webapp/src/App.tsx"},
			want:  []string{"build-frontend-harness", "webapp"},
		},
		{
			name:  "a daemon change also selects the cross-system suites",
			paths: []string{"modules/app/agent-repl/daemon/internal/merge/run.go"},
			want:  []string{"daemon", "e2e", "e2e-emacs"},
		},
		{
			name:  "a proto change selects every generated consumer",
			paths: []string{"modules/app/agent-repl/proto/src/frontend/v1/feed.proto"},
			want:  []string{"daemon", "webapp", "shim", "proto", "e2e"},
		},
		{
			name:  "the shared logging module selects its Go consumers",
			paths: []string{"modules/app/agent-repl/agent-shim/logging/go/log.go"},
			want:  []string{"daemon", "sidecar", "store", "logging", "logging-density", "e2e"},
		},
		{
			name:  "an ordinary bin script selects the script harnesses",
			paths: []string{"modules/app/agent-repl/bin/build-frontend.sh"},
			want:  scriptHarnessSuites,
		},
		{
			name:  "module-root elisp selects the ert and Emacs-client suites",
			paths: []string{"modules/app/agent-repl/config.el"},
			want:  []string{"ert", "e2e-emacs"},
		},
		{
			name:  "lisp/ selects the ert and Emacs-client suites",
			paths: []string{"modules/app/agent-repl/lisp/core.el"},
			want:  []string{"ert", "e2e-emacs"},
		},
		{
			name:  "several regions union their radii",
			paths: []string{"modules/app/agent-repl/webapp/src/App.tsx", "modules/app/agent-repl/daemon/main.go"},
			want:  []string{"build-frontend-harness", "daemon", "webapp", "e2e", "e2e-emacs"},
		},
		{
			// The sandbox image is the Emacs client layer's precondition and
			// nothing else's, so it is the one region that selects e2e-emacs
			// WITHOUT the containerless cross-system suite.
			name:  "the sandbox image selects only the Emacs client layer",
			paths: []string{"modules/app/agent-repl/e2e/sandbox/Dockerfile"},
			want:  []string{"e2e-emacs"},
		},
		{
			name:  "the e2e harness selects both cross-system suites",
			paths: []string{"modules/app/agent-repl/e2e/world_test.go"},
			want:  []string{"e2e", "e2e-emacs"},
		},
		{
			// Blast radius, not name: the cross-system suites RUN a real
			// claude-repld, so a daemon change can break them.
			name:  "a daemon change reaches the cross-system suites",
			paths: []string{"modules/app/agent-repl/daemon/main.go"},
			want:  []string{"daemon", "e2e", "e2e-emacs"},
		},
		{
			// The shim runs for real in the containerless suite, but the Emacs
			// client layer's own claim is the elisp seam, so it stays out.
			name:  "a shim change reaches the containerless cross-system suite",
			paths: []string{"modules/app/agent-repl/agent-shim/claude/shim/src/main.ts"},
			want:  []string{"shim", "e2e"},
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
	paths := []string{"modules/app/agent-repl/webapp/src/App.tsx", "tools/newthing/main.go"}

	// Act.
	got := SelectSuites(paths)

	// Assert.
	if !got.Full {
		t.Fatalf("an unmapped path did not select the full set: %v", got.Suites)
	}
	if len(got.Suites) != len(AllSuites) {
		t.Fatalf("full selection has %d suites, want the whole roster of %d", len(got.Suites), len(AllSuites))
	}
	if !strings.Contains(got.Reason, "tools/newthing/main.go") {
		t.Fatalf("the reason does not name the unmapped path: %q", got.Reason)
	}
}

// TestSelectSuitesRunnerScriptSelectsEverything covers the three scripts that
// ARE the runner: a change to one can invalidate any suite.
func TestSelectSuitesRunnerScriptSelectsEverything(t *testing.T) {
	tests := []string{
		"modules/app/agent-repl/bin/test-all.sh",
		"modules/app/agent-repl/bin/report-nonlisp-coverage.sh",
		"modules/app/agent-repl/bin/report-logging-density.sh",
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

// TestSelectSuitesEmptyPathsSelectsEverything covers the unread change: no
// paths means the paths could not be read, never that nothing changed.
func TestSelectSuitesEmptyPathsSelectsEverything(t *testing.T) {
	// Arrange: nothing to go on.
	var paths []string

	// Act.
	got := SelectSuites(paths)

	// Assert.
	if !got.Full || len(got.Suites) != len(AllSuites) {
		t.Fatalf("an empty path set selected %v (full=%v), want the whole roster", got.Suites, got.Full)
	}
}

// TestSelectSuitesReportsInRosterOrder covers the ordering contract: a
// selection reads the way bin/test-all.sh runs.
func TestSelectSuitesReportsInRosterOrder(t *testing.T) {
	// Arrange: paths given in the reverse of the roster's order.
	paths := []string{
		"modules/app/agent-repl/proto/src/frontend/v1/feed.proto",
		"modules/app/agent-repl/lisp/core.el",
	}

	// Act.
	got := SelectSuites(paths)

	// Assert.
	if strings.Join(got.Suites, ",") != "ert,daemon,webapp,shim,proto,e2e,e2e-emacs" {
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
