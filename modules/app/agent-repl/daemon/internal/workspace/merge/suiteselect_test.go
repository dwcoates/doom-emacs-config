package merge

import (
	"os"
	"reflect"
	"regexp"
	"strings"
	"testing"
)

// The mapping is DATA, so it is tested as data: one table, path set in, suite
// set out, including every way a path can fall off the mapped roots.
func TestSelectSuitesMapsPathsToSuites(t *testing.T) {
	tests := []struct {
		name     string
		paths    []string
		want     []string
		wantFull bool
	}{
		{
			name:  "a webapp-only change takes the webapp suites",
			paths: []string{"modules/app/agent-repl/webapp/src/App.tsx"},
			want:  []string{"build-frontend-harness", "webapp"},
		},
		{
			name:  "a daemon change takes the daemon suite, which carries e2e",
			paths: []string{"modules/app/agent-repl/daemon/internal/workspace/merge/merge.go"},
			want:  []string{"daemon"},
		},
		{
			name:  "a daemon e2e change is still the daemon suite",
			paths: []string{"modules/app/agent-repl/daemon/e2e/mergelease_e2e_test.go"},
			want:  []string{"daemon"},
		},
		{
			name:  "a shim change takes the shim suite",
			paths: []string{"modules/app/agent-repl/agent-shim/claude/shim/src/main.ts"},
			want:  []string{"shim"},
		},
		{
			name:  "the sidecar is not the shim, despite the shared prefix",
			paths: []string{"modules/app/agent-repl/agent-shim/claude/shim-sidecar/main.go"},
			want:  []string{"sidecar"},
		},
		{
			name:  "a store change takes the store suite",
			paths: []string{"modules/app/agent-repl/agent-shim/shim-store/main.go"},
			want:  []string{"store"},
		},
		{
			name:  "a wire change takes every Go consumer with it",
			paths: []string{"modules/app/agent-repl/agent-shim/wire/frame.go"},
			want:  []string{"daemon", "sidecar", "store", "wire"},
		},
		{
			name:  "a shared logging change takes its consumers and the density report",
			paths: []string{"modules/app/agent-repl/agent-shim/logging/go/timestamp.go"},
			want:  []string{"daemon", "sidecar", "store", "logging", "logging-density"},
		},
		{
			name:  "a proto change takes every generated consumer",
			paths: []string{"modules/app/agent-repl/proto/frontend/v1/frontend.proto"},
			want:  []string{"daemon", "webapp", "shim", "proto"},
		},
		{
			name:  "top-level elisp takes the ert suites",
			paths: []string{"modules/app/agent-repl/lisp/status.el", "modules/app/agent-repl/lisp/test-status.el"},
			want:  []string{"ert"},
		},
		{
			name:  "a script harness change takes the script harnesses",
			paths: []string{"modules/app/agent-repl/bin/test-deploy-all.sh"},
			want: []string{
				"orchestrator-harness", "coverage-harness", "logging-density-harness",
				"build-frontend-harness", "deploy-harness", "readiness-harness",
			},
		},
		{
			name:  "a change spanning two roots takes the union",
			paths: []string{"modules/app/agent-repl/webapp/src/App.tsx", "modules/app/agent-repl/daemon/main.go"},
			want:  []string{"build-frontend-harness", "daemon", "webapp"},
		},
		{
			name:     "the suite runner itself takes the full set",
			paths:    []string{"modules/app/agent-repl/bin/test-all.sh"},
			wantFull: true,
		},
		{
			name:     "the coverage driver takes the full set",
			paths:    []string{"modules/app/agent-repl/bin/report-nonlisp-coverage.sh"},
			wantFull: true,
		},
		{
			name:     "an unmapped repository path takes the full set",
			paths:    []string{"config.el"},
			wantFull: true,
		},
		{
			name:     "an unmapped path anywhere in the change takes the full set",
			paths:    []string{"modules/app/agent-repl/webapp/src/App.tsx", ".githooks/pre-commit"},
			wantFull: true,
		},
		{
			name:     "an unmapped agent-repl subdirectory takes the full set",
			paths:    []string{"modules/app/agent-repl/scripts/test-agent-shim-doctor.sh"},
			wantFull: true,
		},
		{
			name:     "a non-elisp file at the module root takes the full set",
			paths:    []string{"modules/app/agent-repl/AGENTS.md"},
			wantFull: true,
		},
		{
			name:     "an unreadable change (no paths) takes the full set",
			paths:    nil,
			wantFull: true,
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			got := SelectSuites(tc.paths)

			// Assert.
			if got.Full != tc.wantFull {
				t.Fatalf("SelectSuites(%v).Full = %t, want %t (reason: %s)", tc.paths, got.Full, tc.wantFull, got.Reason)
			}
			if tc.wantFull {
				if len(got.Suites) != 0 {
					t.Fatalf("SelectSuites(%v).Suites = %v, want empty for a full-set decision", tc.paths, got.Suites)
				}
				return
			}
			if !sameSet(got.Suites, tc.want) {
				t.Fatalf("SelectSuites(%v).Suites = %v, want %v", tc.paths, got.Suites, tc.want)
			}
			if got.Reason == "" {
				t.Fatalf("SelectSuites(%v).Reason is empty; the decision must record why", tc.paths)
			}
		})
	}
}

// The selection is what the gate is narrowed by, so the same paths must always
// produce the same list in the same order — including when git lists them in
// another order.
func TestSelectSuitesIsOrderIndependent(t *testing.T) {
	// Arrange.
	forward := []string{
		"modules/app/agent-repl/webapp/src/App.tsx",
		"modules/app/agent-repl/daemon/main.go",
		"modules/app/agent-repl/proto/x.proto",
	}
	reversed := []string{forward[2], forward[1], forward[0]}

	// Act.
	a := SelectSuites(forward)
	b := SelectSuites(reversed)

	// Assert.
	if !reflect.DeepEqual(a.Suites, b.Suites) {
		t.Fatalf("SelectSuites order-dependent: %v vs %v", a.Suites, b.Suites)
	}
	if a.Reason != b.Reason {
		t.Fatalf("SelectSuites reason order-dependent:\n%s\n%s", a.Reason, b.Reason)
	}
}

// The chosen suites are reported in the entrypoint's own run order, so a
// selection reads the way the run does.
func TestSelectSuitesReportsSuitesInRunOrder(t *testing.T) {
	// Arrange — a proto change, whose rule lists its suites out of run order.
	paths := []string{"modules/app/agent-repl/proto/frontend/v1/frontend.proto"}

	// Act.
	got := SelectSuites(paths).Suites

	// Assert.
	if !inRosterOrder(got) {
		t.Fatalf("SelectSuites().Suites = %v, want them in bin/test-all.sh's run order %v", got, allSuites)
	}
}

// Every rule must name suites the roster carries: a typo would reach the
// entrypoint's --suites argument, which rejects it, turning a mapping mistake
// into an unrunnable gate.
func TestSuiteRulesOnlyNameKnownSuites(t *testing.T) {
	for _, r := range suiteRules {
		// Act + Assert.
		if err := validateSuiteNames(r.Suites); err != nil {
			t.Errorf("rule %+v: %v", r, err)
		}
	}
}

// The Go roster and bin/test-all.sh's are two lists that can disagree, and the
// gate passes one to the other. A name the script does not declare is rejected
// at run time, which would deny every merge the mapping selected it for.
func TestRosterMatchesTheEntrypointScript(t *testing.T) {
	// Arrange.
	script, err := os.ReadFile("../../../../bin/test-all.sh")
	if err != nil {
		t.Fatalf("read bin/test-all.sh: %v", err)
	}
	block := regexp.MustCompile(`(?s)\nALL_SUITES=\(\n(.*?)\n\)\n`).FindSubmatch(script)
	if block == nil {
		t.Fatalf("bin/test-all.sh has no ALL_SUITES=( ... ) block to compare against")
	}
	var declared []string
	for _, line := range strings.Split(string(block[1]), "\n") {
		if name := strings.TrimSpace(line); name != "" {
			declared = append(declared, name)
		}
	}

	// Act + Assert.
	if !reflect.DeepEqual(declared, allSuites) {
		t.Fatalf("bin/test-all.sh ALL_SUITES = %v, merge.allSuites = %v; they must be identical and in the same order",
			declared, allSuites)
	}
}

func TestValidateSuiteNamesRefusesAnUnknownName(t *testing.T) {
	// Act.
	err := validateSuiteNames([]string{"webapp", "not-a-suite"})

	// Assert.
	if err == nil {
		t.Fatal("validateSuiteNames() err = nil, want a refusal for an unknown suite")
	}
	if !strings.Contains(err.Error(), "not-a-suite") {
		t.Fatalf("validateSuiteNames() err = %v, want it to name the offending suite", err)
	}
}

func sameSet(got, want []string) bool {
	if len(got) != len(want) {
		return false
	}
	have := map[string]bool{}
	for _, g := range got {
		have[g] = true
	}
	for _, w := range want {
		if !have[w] {
			return false
		}
	}
	return true
}

func inRosterOrder(suites []string) bool {
	last := -1
	for _, s := range suites {
		idx := -1
		for i, r := range allSuites {
			if r == s {
				idx = i
				break
			}
		}
		if idx <= last {
			return false
		}
		last = idx
	}
	return true
}
