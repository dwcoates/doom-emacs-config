package cli

import (
	"bytes"
	"errors"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"agentrepl/testrun/internal/impact"
	"agentrepl/testrun/internal/run"
	"agentrepl/testrun/internal/suites"
	"agentrepl/testrun/internal/testtimes"
	"agentrepl/testrun/roster"
	"agentrepl/testrun/testenv"
)

func TestImpactChunkKeepsTopLevelTestsAndFoldsSubtests(t *testing.T) {
	cases := []struct {
		name         string
		r            run.Result
		wantTests    []string
		wantUnnamed  bool
		wantOutcomes []string
	}{
		{
			name: "subtests fold into their top-level test",
			r: run.Result{Outcome: run.Passed, Spec: run.Spec{Impact: &run.ImpactChunk{Package: "integration", Windows: "/w"}}, Tests: []testtimes.Test{
				{Name: "integration.TestA", Outcome: testtimes.Pass, CPU: 2, HasCPU: true},
				{Name: "integration.TestA/sub", Outcome: testtimes.Pass},
				{Name: "integration.TestB", Outcome: testtimes.Skip},
				{Name: "other.TestC", Outcome: testtimes.Pass},
			}},
			wantTests: []string{"TestA", "TestB"}, wantOutcomes: []string{"pass", "skip"},
		},
		{
			name: "a failed chunk that names a failing test is no unnamed failure",
			r: run.Result{Outcome: run.Failed, Spec: run.Spec{Impact: &run.ImpactChunk{Package: "integration"}}, Tests: []testtimes.Test{
				{Name: "integration.TestA", Outcome: testtimes.Fail},
			}},
			wantTests: []string{"TestA"}, wantOutcomes: []string{"fail"},
		},
		{
			name: "a failed chunk that names none is a package-level failure",
			r: run.Result{Outcome: run.Failed, Spec: run.Spec{Impact: &run.ImpactChunk{Package: "integration"}}, Tests: []testtimes.Test{
				{Name: "integration.TestA", Outcome: testtimes.Pass},
			}},
			wantTests: []string{"TestA"}, wantOutcomes: []string{"pass"}, wantUnnamed: true,
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			c := impactChunk(tc.r)
			// Assert
			var names, outcomes []string
			for _, x := range c.Tests {
				names = append(names, x.Name)
				outcomes = append(outcomes, x.Outcome)
			}
			if strings.Join(names, ",") != strings.Join(tc.wantTests, ",") || strings.Join(outcomes, ",") != strings.Join(tc.wantOutcomes, ",") || c.UnnamedFailure != tc.wantUnnamed {
				t.Fatalf("impactChunk = %+v, want tests %v outcomes %v unnamed %v", c, tc.wantTests, tc.wantOutcomes, tc.wantUnnamed)
			}
		})
	}
}

func TestImpactDataTakesTheDeclaredReadsButNotTheModulesCodeOrDocs(t *testing.T) {
	// Arrange
	layout := suites.Layout{Repo: "/repo", Module: "/repo/mod"}
	s := roster.Suite{Name: "daemon", Reuse: &roster.Reuse{
		Units:  map[string][]string{"integration[integration]": {"daemon/", "prompts/", "bin/agent-repld", "/.githooks/hook"}},
		Inputs: []string{"requirements.json"},
	}}
	files := []string{
		"mod/daemon/internal/x/x.go", "mod/daemon/internal/x/testdata/golden.json", "mod/daemon/AGENTS.md", "mod/daemon/scripts/run.sh",
		"mod/prompts/a.md", "mod/bin/agent-repld", "mod/bin/other", ".githooks/hook", "mod/requirements.json", "mod/unrelated.txt",
	}

	// Act
	got, err := impactData(layout, s, "/repo/mod/daemon", "integration", "integration", files)

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	want := ".githooks/hook,mod/bin/agent-repld,mod/daemon/internal/x/testdata/golden.json,mod/daemon/scripts/run.sh,mod/prompts/a.md,mod/requirements.json"
	if strings.Join(got, ",") != want {
		t.Fatalf("impactData = %v, want %s", got, want)
	}
}

func TestImpactDataRefusesASuiteThatDeclaresNoInputs(t *testing.T) {
	// Act
	_, err := impactData(suites.Layout{}, roster.Suite{Name: "daemon"}, "/m", "integration", "integration", nil)
	// Assert
	if err == nil || !strings.Contains(err.Error(), "declares no Reuse inputs") {
		t.Fatalf("impactData = %v", err)
	}
}

func TestImpactMetricsCarryTheReportAndSortTheBreadth(t *testing.T) {
	// Arrange
	rep := impact.Report{
		WouldSkip: 3, WouldSkipCPU: 9.5, Misses: []impact.Miss{{}}, RecordsWritten: 5, OverheadCPU: 1.25,
		Breadth: map[impact.TestKey]float64{
			{Package: "integration", Test: "TestB"}: 0.5,
			{Package: "integration", Test: "TestA"}: 0.25,
		},
	}
	// Act
	m := impactMetrics(impact.PackagePass{Suite: "daemon"}, rep)
	// Assert
	if m.Suite != "daemon" || m.WouldSkip != 3 || m.WouldSkipCPU != 9.5 || m.Misses != 1 || m.RecordsWritten != 5 || m.OverheadCPU != 1.25 {
		t.Fatalf("metrics = %+v", m)
	}
	if len(m.Breadth) != 2 || m.Breadth[0].Test != "integration.TestA" || m.Breadth[1].Share != 0.5 {
		t.Fatalf("breadth = %+v, want sorted by test", m.Breadth)
	}
}

// impactFixture is a checkout and module with go.mod files for planning, and
// a module "fx" for the recording.
type impactFixture struct {
	repo, module string
	deps         Deps
	out          *bytes.Buffer
	errOut       *bytes.Buffer
	layout       suites.Layout
}

func newImpactFixture(t *testing.T) *impactFixture {
	t.Helper()
	repo, err := filepath.EvalSymlinks(t.TempDir())
	if err != nil {
		t.Fatal(err)
	}
	module := filepath.Join(repo, "mod")
	for path, content := range map[string]string{
		"mod/daemon/go.mod":                         "module fx\n\ngo 1.24\n",
		"mod/daemon/cmd/agent-repld/main.go":        "package main\n\nimport \"fx/internal/a\"\n\nfunc main() { _ = a.A() }\n",
		"mod/daemon/internal/a/a.go":                "package a\n\nfunc A() int { return 1 }\n",
		"mod/daemon/integration/harness/h.go":       "package harness\n\nfunc Start() {}\n",
		"mod/daemon/integration/one_test.go":        "package integration\n\nimport (\n\t\"testing\"\n\n\t\"fx/integration/harness\"\n)\n\nfunc TestOne(t *testing.T) { harness.Start() }\n",
		"mod/prompts/p.txt":                         "p\n",
		"mod/agent-shim/logging/go/go.mod":          "module agentrepl/logging\n",
		"mod/agent-shim/claude/shim-sidecar/go.mod": "module agentrepl/shim-claude-sidecar\n",
		"mod/agent-shim/shim-store/go.mod":          "module agentrepl/shim-store\n",
		"mod/agent-shim/shim-lock/go.mod":           "module agentrepl/shim-lock\n",
		"mod/proto/helpers/go/go.mod":               "module agentrepl/protohelpers\n",
	} {
		p := filepath.Join(repo, filepath.FromSlash(path))
		if err := os.MkdirAll(filepath.Dir(p), 0o755); err != nil {
			t.Fatal(err)
		}
		if err := os.WriteFile(p, []byte(content), 0o644); err != nil {
			t.Fatal(err)
		}
	}
	f := &impactFixture{repo: repo, module: module, out: &bytes.Buffer{}, errOut: &bytes.Buffer{}}
	f.layout = suites.Layout{Repo: repo, Module: module, Work: t.TempDir(), Impact: true}
	f.deps = Deps{
		Log: &run.Log{Out: f.out, Err: f.errOut},
		Roster: func(name string) (roster.Suite, bool) {
			return roster.Suite{Name: name, Path: "daemon", IntegrationTags: "integration",
				Programs: map[string][]string{"integration": {"agent-repld", "fakeshim"}},
				Reuse:    &roster.Reuse{Units: map[string][]string{"integration[integration]": {"prompts/"}}}}, name == "daemon"
		},
		Impact: &ImpactDeps{
			Store: impact.Store{Dir: filepath.Join(t.TempDir(), "impact")},
			List:  impact.ExecLister,
			Toolchain: func() (map[string]string, error) {
				return map[string]string{"go": "go1.24"}, nil
			},
			RepoFiles: func() ([]string, error) { return []string{"mod/prompts/p.txt", "mod/daemon/internal/a/a.go"}, nil },
			Retention: impact.DefaultRetention,
		},
	}
	return f
}

// result is a recorded chunk's result whose harness wrote windows.
func (f *impactFixture) result(t *testing.T, outcome run.Outcome, tests []testtimes.Test, windows ...testenv.ImpactWindow) run.Result {
	t.Helper()
	path := filepath.Join(t.TempDir(), "w.jsonl")
	env := func(k string) string {
		if k == testenv.ImpactOut {
			return path
		}
		return ""
	}
	for _, w := range windows {
		if err := testenv.AppendImpactWindow(env, w); err != nil {
			t.Fatal(err)
		}
	}
	return run.Result{Outcome: outcome, Tests: tests, Spec: run.Spec{Impact: &run.ImpactChunk{Suite: "daemon", Package: "integration", Tags: "integration", Windows: path}}}
}

func TestRecordImpactRecordsDecidesAndLogsEveryDecision(t *testing.T) {
	// Arrange
	f := newImpactFixture(t)
	tests := []testtimes.Test{{Name: "integration.TestOne", Outcome: testtimes.Pass, CPU: 3, HasCPU: true}}
	window := testenv.ImpactWindow{Test: "TestOne", Executed: []string{"fx/internal/a/a.go"}, CPUSeconds: 0.5}
	now := time.Date(2026, 11, 21, 0, 0, 0, 0, time.UTC)

	// Act
	first := recordImpact(f.deps, f.layout, []run.Result{f.result(t, run.Passed, tests, window)}, now)
	second := recordImpact(f.deps, f.layout, []run.Result{f.result(t, run.Passed, tests, window)}, now.Add(time.Hour))

	// Assert
	if len(first) != 1 || first[0].WouldSkip != 0 || first[0].RecordsWritten != 1 || first[0].Errors != 0 {
		t.Fatalf("first = %+v, want one record written and nothing to skip", first)
	}
	if len(second) != 1 || second[0].WouldSkip != 1 || second[0].WouldSkipCPU != 3 || second[0].Misses != 0 {
		t.Fatalf("second = %+v, want the test would-skipped with its CPU", second)
	}
	log := f.out.String()
	for _, want := range []string{"impact: daemon:integration[integration].TestOne: would run: no record", "impact: daemon:integration[integration].TestOne: would skip: passed at", "1 would be skipped (3.0 CPU-s)"} {
		if !strings.Contains(log, want) {
			t.Fatalf("log = %s, want %q", log, want)
		}
	}
	if f.errOut.Len() != 0 {
		t.Fatalf("error log = %s, want none", f.errOut.String())
	}
}

func TestRecordImpactFailuresAreLoggedCountedAndNeverFailTheRun(t *testing.T) {
	cases := []struct {
		name    string
		mutate  func(f *impactFixture)
		results func(f *impactFixture, t *testing.T) []run.Result
		wantLog string
	}{
		{"a suite the roster does not hold", func(f *impactFixture) {
			f.deps.Roster = func(string) (roster.Suite, bool) { return roster.Suite{}, false }
		}, func(f *impactFixture, t *testing.T) []run.Result {
			return []run.Result{f.result(t, run.Passed, nil)}
		}, "the roster holds no suite daemon"},
		{"a toolchain that cannot be read", func(f *impactFixture) {
			f.deps.Impact.Toolchain = func() (map[string]string, error) { return nil, errors.New("no go") }
		}, func(f *impactFixture, t *testing.T) []run.Result {
			return []run.Result{f.result(t, run.Passed, nil)}
		}, "no go"},
		{"a checkout whose files cannot be listed", func(f *impactFixture) {
			f.deps.Impact.RepoFiles = func() ([]string, error) { return nil, errors.New("git failed") }
		}, func(f *impactFixture, t *testing.T) []run.Result {
			return []run.Result{f.result(t, run.Passed, nil)}
		}, "list the checkout's files: git failed"},
		{"a window naming code no package holds", nil, func(f *impactFixture, t *testing.T) []run.Result {
			return []run.Result{f.result(t, run.Passed, []testtimes.Test{{Name: "integration.TestOne", Outcome: testtimes.Pass}},
				testenv.ImpactWindow{Test: "TestOne", Executed: []string{"fx/nowhere/x.go"}})}
		}, "recording daemon:integration failed, so none of its tests was recorded"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			f := newImpactFixture(t)
			if tc.mutate != nil {
				tc.mutate(f)
			}
			// Act
			got := recordImpact(f.deps, f.layout, tc.results(f, t), time.Now())
			// Assert
			if len(got) != 1 || got[0].Errors != 1 {
				t.Fatalf("metrics = %+v, want one entry counting the error", got)
			}
			if !strings.Contains(f.errOut.String(), tc.wantLog) {
				t.Fatalf("error log = %s, want %q", f.errOut.String(), tc.wantLog)
			}
			if !strings.Contains(f.errOut.String(), "impact: ") {
				t.Fatalf("error log = %s, want the impact record", f.errOut.String())
			}
		})
	}
}

func TestRecordImpactSaysSoWhenNothingRecordedRan(t *testing.T) {
	cases := []struct {
		name    string
		deps    func(f *impactFixture)
		results []run.Result
		wantLog string
		wantNil bool
	}{
		{"a run with no recorded chunk says its units were reused", nil, []run.Result{{Outcome: run.Passed}}, "no recorded chunk ran in this run", true},
		{"a cancelled recorded chunk is not a run of it", nil, []run.Result{{Outcome: run.Cancelled, Spec: run.Spec{Impact: &run.ImpactChunk{Suite: "daemon", Package: "integration"}}}}, "no recorded chunk ran in this run", true},
		{"a host with no impact seams records nothing and says nothing", func(f *impactFixture) { f.deps.Impact = nil }, []run.Result{{Outcome: run.Passed}}, "", true},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			f := newImpactFixture(t)
			if tc.deps != nil {
				tc.deps(f)
			}
			// Act
			got := recordImpact(f.deps, f.layout, tc.results, time.Now())
			// Assert
			if (got == nil) != tc.wantNil {
				t.Fatalf("metrics = %+v", got)
			}
			if tc.wantLog != "" && !strings.Contains(f.out.String(), tc.wantLog) {
				t.Fatalf("log = %s, want %q", f.out.String(), tc.wantLog)
			}
			if tc.wantLog == "" && f.out.Len() != 0 {
				t.Fatalf("log = %s, want nothing", f.out.String())
			}
		})
	}
}

func TestPlanImpactPassRefusesAPackageThatRunsNoImpactProgram(t *testing.T) {
	// Arrange
	f := newImpactFixture(t)
	f.deps.Roster = func(name string) (roster.Suite, bool) {
		return roster.Suite{Name: name, Path: "daemon", Programs: map[string][]string{"integration": {"fakeshim"}}, Reuse: &roster.Reuse{}}, true
	}
	// Act
	_, err := planImpactPass(f.deps, f.layout, "daemon", "integration", "integration")
	// Assert
	if err == nil || !strings.Contains(err.Error(), "runs no program with an impact build") {
		t.Fatalf("planImpactPass = %v", err)
	}
}

func TestRunRecordsImpactOnlyForARunThatHasTheSeamsAndIsNoCoverageRun(t *testing.T) {
	cases := []struct {
		name     string
		impact   bool
		coverage bool
		want     bool
	}{
		{"an ordinary run with the seams records", true, false, true},
		{"a coverage run builds its own programs", true, true, false},
		{"a host with no seams records nothing", false, false, false},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := impactLayoutOn(Deps{Impact: map[bool]*ImpactDeps{true: {}, false: nil}[tc.impact]}, Args{Coverage: tc.coverage})
			// Assert
			if got != tc.want {
				t.Fatalf("Layout.Impact = %v, want %v", got, tc.want)
			}
		})
	}
}
