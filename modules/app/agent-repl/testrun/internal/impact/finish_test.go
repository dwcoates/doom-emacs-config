package impact

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"agentrepl/testrun/testenv"
)

// recordingLog keeps the run's canonical log lines.
type recordingLog struct{ infos, errors []string }

func (l *recordingLog) Infof(f string, a ...any)  { l.infos = append(l.infos, fmt.Sprintf(f, a...)) }
func (l *recordingLog) Errorf(f string, a ...any) { l.errors = append(l.errors, fmt.Sprintf(f, a...)) }

// world is a checkout holding the module "fx": a program (cmd/prog) over
// internal/a, internal/b and a generated package gen, a harness, and an
// integration package of two tests.
type world struct {
	t     *testing.T
	repo  string
	store Store
	now   time.Time
}

func newWorld(t *testing.T) *world {
	t.Helper()
	repo, err := filepath.EvalSymlinks(t.TempDir())
	if err != nil {
		t.Fatal(err)
	}
	w := &world{t: t, repo: repo, store: Store{Dir: filepath.Join(t.TempDir(), "impact")}, now: time.Date(2026, 11, 21, 12, 0, 0, 0, time.UTC)}
	for path, content := range map[string]string{
		"mod/go.mod":                     "module fx\n\ngo 1.24\n",
		"mod/cmd/prog/main.go":           "package main\n\nimport (\n\t\"fx/gen\"\n\t\"fx/internal/a\"\n\t\"fx/internal/c\"\n)\n\nfunc main() { _ = a.A() + c.C() + gen.G }\n",
		"mod/internal/a/a.go":            "package a\n\nimport \"fx/internal/b\"\n\nfunc A() int { return K + b.B() }\n",
		"mod/internal/a/consts.go":       "package a\n\nconst K = 1\n",
		"mod/internal/b/b.go":            "package b\n\nfunc B() int { return 2 }\n",
		"mod/internal/c/c.go":            "package c\n\nfunc C() int { return 3 }\n",
		"mod/gen/gen.go":                 "package gen\n\nconst G = 4\n",
		"mod/integration/harness/h.go":   "package harness\n\nimport \"fx/internal/b\"\n\nfunc Start() int { return b.B() }\n",
		"mod/integration/one_test.go":    "package integration\n\nimport (\n\t\"testing\"\n\n\t\"fx/integration/harness\"\n)\n\nfunc TestOne(t *testing.T) { _ = harness.Start(); _ = helper() }\n",
		"mod/integration/two_test.go":    "package integration\n\nimport \"testing\"\n\nfunc TestTwo(t *testing.T) {}\n",
		"mod/integration/helper_test.go": "package integration\n\nfunc helper() int { return 5 }\n",
		"data/prompts.md":                "prompt\n",
	} {
		w.write(path, content)
	}
	return w
}

func (w *world) write(rel, content string) {
	w.t.Helper()
	p := filepath.Join(w.repo, filepath.FromSlash(rel))
	if err := os.MkdirAll(filepath.Dir(p), 0o755); err != nil {
		w.t.Fatal(err)
	}
	if err := os.WriteFile(p, []byte(content), 0o644); err != nil {
		w.t.Fatal(err)
	}
}

// result is what a test did in a recorded run.
type result struct {
	outcome    string
	executed   []string
	cpu        float64
	incomplete string
}

func (w *world) pass(t *testing.T, results map[string]result) PackagePass {
	t.Helper()
	windows := filepath.Join(t.TempDir(), "windows.jsonl")
	env := func(k string) string {
		if k == testenv.ImpactOut {
			return windows
		}
		return ""
	}
	var tests []Test
	for name, r := range results {
		tests = append(tests, Test{Name: name, Outcome: r.outcome, CPU: 2, HasCPU: true})
		if err := testenv.AppendImpactWindow(env, testenv.ImpactWindow{Test: name, Executed: r.executed, CPUSeconds: r.cpu, Incomplete: r.incomplete}); err != nil {
			t.Fatal(err)
		}
	}
	if err := testenv.AppendImpactWindow(env, testenv.ImpactWindow{CPUSeconds: 1.5}); err != nil {
		t.Fatal(err)
	}
	return PackagePass{
		Suite: "daemon", Package: "integration", Tags: "",
		ModuleDir: filepath.Join(w.repo, "mod"), TestPackage: "./integration", ProgramPackages: []string{"./cmd/prog"},
		Instrumented: func(p string) bool {
			return strings.HasPrefix(p, "fx/internal/") || strings.HasPrefix(p, "fx/cmd/") || strings.HasPrefix(p, "fx/integration/")
		},
		HarnessPrefix: "fx/integration/",
		Data:          []string{"data/prompts.md"},
		Facts:         map[string]string{"go": "go1.24"},
		Chunks:        []Chunk{{Windows: windows, Tests: tests}},
	}
}

func (w *world) finish(t *testing.T, pass PackagePass) (Report, *recordingLog, error) {
	t.Helper()
	log := &recordingLog{}
	w.now = w.now.Add(time.Hour)
	rep, err := Finisher{Repo: w.repo, Store: w.store, List: ExecLister, Now: w.now, Log: log, Retention: DefaultRetention}.Finish([]PackagePass{pass})
	return rep, log, err
}

func decisionOf(rep Report, test string) Decision {
	for _, d := range rep.Decisions {
		if d.Key.Test == test {
			return d
		}
	}
	return Decision{}
}

var okResults = func() map[string]result {
	return map[string]result{
		"TestOne": {outcome: Pass, executed: []string{"fx/internal/a/a.go", "fx/internal/b/b.go"}},
		"TestTwo": {outcome: Pass, executed: []string{"fx/internal/c/c.go"}},
	}
}

func TestFinishRecordsThenWouldSkipAnUnchangedTree(t *testing.T) {
	// Arrange
	w := newWorld(t)

	// Act
	first, firstLog, err := w.finish(t, w.pass(t, okResults()))
	if err != nil {
		t.Fatalf("first Finish: %v", err)
	}
	second, secondLog, err := w.finish(t, w.pass(t, okResults()))
	if err != nil {
		t.Fatalf("second Finish: %v", err)
	}

	// Assert
	if first.WouldSkip != 0 || first.RecordsWritten != 2 || !strings.Contains(decisionOf(first, "TestOne").Reason, "no record") {
		t.Fatalf("first run = %+v, want two records written and nothing to skip yet", first)
	}
	if second.WouldSkip != 2 || second.WouldSkipCPU != 4 || second.WouldSkipCPUKnown != 2 || len(second.Misses) != 0 {
		t.Fatalf("second run = %+v, want both would-skip with their CPU", second)
	}
	if second.OverheadCPU != 1.5 {
		t.Fatalf("overhead = %v, want the boot's cost", second.OverheadCPU)
	}
	if second.ProgramFiles != 5 || second.Breadth[TestKey{Suite: "daemon", Package: "integration", Test: "TestOne"}] != 2.0/5 {
		t.Fatalf("breadth = %v of %d files, want TestOne's two of the program's five", second.Breadth, second.ProgramFiles)
	}
	for _, log := range []*recordingLog{firstLog, secondLog} {
		joined := strings.Join(log.infos, "\n")
		if !strings.Contains(joined, "daemon:integration.TestOne: ") || !strings.Contains(joined, "daemon:integration.TestTwo: ") {
			t.Fatalf("log = %s, want every decision logged by test", joined)
		}
	}
	if !strings.Contains(strings.Join(secondLog.infos, "\n"), "daemon:integration.TestOne: would skip: passed at ") {
		t.Fatalf("log = %v, want the would-skip said explicitly", secondLog.infos)
	}
}

func TestFinishDecidesAgainstWhatChangedSinceTheRecord(t *testing.T) {
	cases := []struct {
		name         string
		change       func(w *world)
		wantRunOne   string
		wantRunTwo   string
		wantSkipOnly string
	}{
		{"a constant a test's executed code uses", func(w *world) { w.write("mod/internal/a/consts.go", "package a\n\nconst K = 99\n") }, "consts.go", "", "TestTwo"},
		{"a file a test executed", func(w *world) { w.write("mod/internal/c/c.go", "package c\n\nfunc C() int { return 30 }\n") }, "", "c.go", "TestOne"},
		{"a file a test never touched", func(w *world) { w.write("mod/internal/b/unused.go", "package b\n") }, "", "", "both"},
		{"the harness", func(w *world) {
			w.write("mod/integration/harness/h.go", "package harness\n\nimport \"fx/internal/b\"\n\nfunc Start() int { return b.B() + 1 }\n")
		}, "h.go", "h.go", ""},
		{"a data file the processes read", func(w *world) { w.write("data/prompts.md", "changed\n") }, "prompts.md", "prompts.md", ""},
		{"a test's own file", func(w *world) {
			w.write("mod/integration/two_test.go", "package integration\n\nimport \"testing\"\n\nfunc TestTwo(t *testing.T) { _ = 1 }\n")
		}, "", "two_test.go", "TestOne"},
		{"a helper only one test uses", func(w *world) {
			w.write("mod/integration/helper_test.go", "package integration\n\nfunc helper() int { return 6 }\n")
		}, "helper_test.go", "", "TestTwo"},
		{"go.mod", func(w *world) { w.write("mod/go.mod", "module fx\n\ngo 1.24\n\n// changed\n") }, "go.mod", "go.mod", ""},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			w := newWorld(t)
			if _, _, err := w.finish(t, w.pass(t, okResults())); err != nil {
				t.Fatal(err)
			}
			tc.change(w)
			// Act
			rep, _, err := w.finish(t, w.pass(t, okResults()))
			// Assert
			if err != nil {
				t.Fatalf("Finish: %v", err)
			}
			one, two := decisionOf(rep, "TestOne"), decisionOf(rep, "TestTwo")
			check := func(d Decision, want string) {
				t.Helper()
				switch {
				case want == "" && !d.WouldSkip:
					t.Fatalf("%s: %+v, want it skipped", d.Key.Test, d)
				case want != "" && (d.WouldSkip || !strings.Contains(d.Reason, want)):
					t.Fatalf("%s: %+v, want it to run for %s", d.Key.Test, d, want)
				}
			}
			check(one, tc.wantRunOne)
			check(two, tc.wantRunTwo)
		})
	}
}

func TestFinishRecordsAFailureAndNeverSkipsOnIt(t *testing.T) {
	// Arrange
	w := newWorld(t)
	failing := okResults()
	failing["TestTwo"] = result{outcome: Fail, executed: []string{"fx/internal/c/c.go"}}
	if _, _, err := w.finish(t, w.pass(t, failing)); err != nil {
		t.Fatal(err)
	}
	// Act
	rep, _, err := w.finish(t, w.pass(t, okResults()))
	// Assert
	if err != nil {
		t.Fatal(err)
	}
	if d := decisionOf(rep, "TestTwo"); d.WouldSkip || !strings.Contains(d.Reason, "last failed") {
		t.Fatalf("TestTwo = %+v, want a would-run after a failure", d)
	}
}

func TestFinishReportsAMissAndLogsItAsAnError(t *testing.T) {
	// Arrange
	w := newWorld(t)
	if _, _, err := w.finish(t, w.pass(t, okResults())); err != nil {
		t.Fatal(err)
	}
	failing := okResults()
	failing["TestOne"] = result{outcome: Fail, executed: failing["TestOne"].executed}
	// Act: the same tree, and TestOne fails.
	rep, log, err := w.finish(t, w.pass(t, failing))
	// Assert
	if err != nil {
		t.Fatal(err)
	}
	if len(rep.Misses) != 1 || rep.Misses[0].Key.Test != "TestOne" {
		t.Fatalf("misses = %+v, want TestOne", rep.Misses)
	}
	if len(log.errors) != 1 || !strings.Contains(log.errors[0], "MISS: daemon:integration.TestOne failed although selection would have skipped it") {
		t.Fatalf("error log = %v, want the miss named", log.errors)
	}
}

func TestFinishNeverRecordsAnIncompleteWindowOrASkippedTest(t *testing.T) {
	// Arrange
	w := newWorld(t)
	results := okResults()
	results["TestOne"] = result{outcome: Pass, executed: results["TestOne"].executed, incomplete: "a daemon was killed"}
	results["TestTwo"] = result{outcome: "skip"}
	// Act
	first, log, err := w.finish(t, w.pass(t, results))
	second, _, err2 := w.finish(t, w.pass(t, results))
	// Assert
	if err != nil || err2 != nil {
		t.Fatalf("Finish: %v, %v", err, err2)
	}
	if first.RecordsWritten != 0 || first.Incomplete != 1 || second.WouldSkip != 0 {
		t.Fatalf("first = %+v, second = %+v, want nothing recorded or skipped", first, second)
	}
	if !strings.Contains(strings.Join(log.infos, "\n"), "daemon:integration.TestOne: would run: its recording is incomplete: a daemon was killed") {
		t.Fatalf("log = %v, want the incomplete recording named", log.infos)
	}
	if len(log.errors) != 1 || !strings.Contains(log.errors[0], "impact: INCOMPLETE: daemon:integration.TestOne was not recorded: a daemon was killed") {
		t.Fatalf("error log = %v, want the incomplete recording logged as an error", log.errors)
	}
}

func TestFinishCountsAnUnnamedChunkFailureAndSkipsNothingOnIt(t *testing.T) {
	// Arrange
	w := newWorld(t)
	pass := w.pass(t, okResults())
	pass.Chunks[0].UnnamedFailure = true
	// Act
	rep, log, err := w.finish(t, pass)
	// Assert
	if err != nil || rep.UnnamedFailures != 1 {
		t.Fatalf("Finish = (%+v, %v), want the unnamed failure counted", rep, err)
	}
	if !strings.Contains(strings.Join(log.infos, "\n"), "failed with no failing test named") {
		t.Fatalf("log = %v, want the unnamed failure said", log.infos)
	}
}

func TestFinishRefusesWhatItCannotPlace(t *testing.T) {
	cases := []struct {
		name    string
		mutate  func(p *PackagePass, w *world)
		results map[string]result
		wantErr string
	}{
		{"a window naming code no package holds", nil,
			map[string]result{"TestOne": {outcome: Pass, executed: []string{"fx/nowhere/x.go"}}}, "which the listing does not hold"},
		{"a pass with no package under test", func(p *PackagePass, w *world) { p.TestPackage = "./cmd/prog" }, okResults(), "no test variant"},
		{"a module that is not there", func(p *PackagePass, w *world) { p.ModuleDir = filepath.Join(w.repo, "absent") }, okResults(), "impact: go list"},
		{"an unreadable windows file", func(p *PackagePass, w *world) {
			bad := filepath.Join(w.repo, "bad.jsonl")
			_ = os.WriteFile(bad, []byte("nope\n"), 0o644)
			p.Chunks[0].Windows = bad
		}, okResults(), "is not an impact window"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			w := newWorld(t)
			pass := w.pass(t, tc.results)
			if tc.mutate != nil {
				tc.mutate(&pass, w)
			}
			// Act
			_, _, err := w.finish(t, pass)
			// Assert
			if err == nil || !strings.Contains(err.Error(), tc.wantErr) {
				t.Fatalf("Finish = %v, want an error containing %q", err, tc.wantErr)
			}
			if _, statErr := os.Stat(filepath.Join(w.store.Dir, "tests")); statErr == nil {
				t.Fatal("a refused pass left records behind")
			}
		})
	}
}

func TestFinishPrunesTheStoreItWrites(t *testing.T) {
	// Arrange
	w := newWorld(t)
	if _, _, err := w.finish(t, w.pass(t, okResults())); err != nil {
		t.Fatal(err)
	}
	w.write("mod/internal/c/c.go", "package c\n\nfunc C() int { return 31 }\n")
	w.write("mod/internal/b/b.go", "package b\n\nfunc B() int { return 21 }\n")
	log := &recordingLog{}
	// Act: a run 40 days on prunes the first run's records away.
	rep, err := Finisher{Repo: w.repo, Store: w.store, List: ExecLister, Now: w.now.Add(40 * 24 * time.Hour), Log: log, Retention: DefaultRetention}.Finish([]PackagePass{w.pass(t, okResults())})
	// Assert
	if err != nil || rep.Pruned.Records != 2 {
		t.Fatalf("Finish = (%+v, %v), want the two 40-day-old records pruned", rep.Pruned, err)
	}
}

func TestExecToolchainNamesTheVersionAndPlatform(t *testing.T) {
	// Act
	facts, err := ExecToolchain()
	// Assert
	if err != nil || !strings.HasPrefix(facts["go"], "go1.") || facts["goos"] == "" || facts["goarch"] == "" {
		t.Fatalf("ExecToolchain = (%v, %v)", facts, err)
	}
}
