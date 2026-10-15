package gomeasure

import (
	"context"
	"os"
	"path/filepath"
	"reflect"
	"strings"
	"testing"

	"agentrepl/testrun/internal/redundancy"
	"agentrepl/testrun/testenv"
)

// verifyConfig is fakeConfig's unit suite plus a kept integration suite
// (TestI) that spawns shim-store; every case reaches F's two mutants.
func verifyConfig(t *testing.T, del ...string) (Config, *fakeGo) {
	t.Helper()
	cfg, dir := fakeConfig(t)
	if err := os.MkdirAll(filepath.Join(dir, "integ"), 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(dir, "integ", "i_test.go"), []byte("package integ\n\nimport \"testing\"\n\nfunc TestI(t *testing.T) {}\n"), 0o644); err != nil {
		t.Fatal(err)
	}
	cfg.Suites = append(cfg.Suites, Suite{Package: "./integ", Tags: "integration", Tests: []string{"i_test.go"}, Programs: []string{"shim-store"}})
	cfg.Root = rootTree(t)
	cfg.Delete = del
	f := &fakeGo{dir: dir, reaches: map[string]bool{"TestA": true, "TestB/x": true, "TestI": true}}
	return cfg, f
}

// script answers each mutant's runs: kills[mutant] are the leaves that fail
// under it; every other leaf passes.
func script(f *fakeGo, kills map[string][]string) {
	f.mutantCmd = map[string]func(Cmd) Result{}
	for _, id := range []string{"1", "2"} {
		failing := kills[id]
		f.mutantCmd[id] = func(c Cmd) Result {
			p := argAfter(c.Args, "-test.run")
			var lns []string
			exit := 0
			for _, l := range withParents(leavesOf(p)) {
				lns = append(lns, "=== RUN   "+l)
			}
			for _, l := range withParents(leavesOf(p)) {
				outcome := "PASS"
				for _, k := range failing {
					if l == k || strings.HasPrefix(k, l+"/") {
						outcome, exit = "FAIL", 1
					}
				}
				lns = append(lns, "--- "+outcome+": "+l+" (0.00s)")
			}
			return Result{Exit: exit, Lines: lines(lns...)}
		}
	}
}

func verified(t *testing.T, cfg Config, f *fakeGo) *redundancy.Verification {
	t.Helper()
	log, _ := quietLog()
	m, err := Measure(context.Background(), cfg, f.exec, log)
	if err != nil {
		t.Fatal(err)
	}
	if m.Verification == nil || len(m.Mutants) != 0 {
		t.Fatalf("a verification answered %+v with %d mutants", m.Verification, len(m.Mutants))
	}
	return m.Verification
}

func TestVerifySettlesWhatAKeptCaseKills(t *testing.T) {
	// Arrange: TestB/x (unit) kills mutant 1; only TestA (deleted) and TestI
	// (kept integration) kill mutant 2.
	cfg, f := verifyConfig(t, ".:TestA")
	script(f, map[string][]string{"1": {"TestA", "TestB/x"}, "2": {"TestA", "TestI"}})

	// Act
	v := verified(t, cfg, f)

	// Assert
	if v.Reached != 2 || v.SettledByUnit != 1 || v.Residual != 1 || v.SettledByTier != 1 || len(v.PutBack) != 0 {
		t.Fatalf("verification = %+v", v)
	}
	if !reflect.DeepEqual(v.Proven, []string{".:TestA"}) {
		t.Fatalf("proven = %v, want TestA", v.Proven)
	}
}

func TestVerifyPutsBackTheCaseThatAloneKillsAMutant(t *testing.T) {
	// Arrange: only TestA kills mutant 2.
	cfg, f := verifyConfig(t, ".:TestA")
	script(f, map[string][]string{"1": {"TestB/x"}, "2": {"TestA"}})

	// Act
	v := verified(t, cfg, f)

	// Assert
	want := []redundancy.PutBack{{Case: ".:TestA", Mutant: "p.go:3:29:< -> >="}}
	if !reflect.DeepEqual(v.PutBack, want) || len(v.Proven) != 0 {
		t.Fatalf("put back %v, proven %v; want TestA put back for mutant 2", v.PutBack, v.Proven)
	}
}

func TestVerifyLosesNothingToAMutantNoDeletedCaseKills(t *testing.T) {
	// Arrange: mutant 2 survives everything.
	cfg, f := verifyConfig(t, ".:TestA")
	script(f, map[string][]string{"1": {"TestB/x"}})

	// Act
	v := verified(t, cfg, f)

	// Assert
	if v.SurvivedDeleted != 1 || len(v.PutBack) != 0 || !reflect.DeepEqual(v.Proven, []string{".:TestA"}) {
		t.Fatalf("verification = %+v", v)
	}
}

func TestVerifyEstimateRunsNoDeletedOrIntegrationCaseUnderAMutant(t *testing.T) {
	// Arrange
	cfg, f := verifyConfig(t, ".:TestA")
	cfg.Estimate = true
	script(f, map[string][]string{"2": {"TestA"}})

	// Act
	v := verified(t, cfg, f)

	// Assert
	if !v.Estimate || v.Residual != 2 || v.ResidualCostHigh < v.ResidualCostLow || len(v.Proven) != 0 {
		t.Fatalf("estimate = %+v", v)
	}
	for _, call := range f.calls {
		underMutant := strings.Contains(call, "[mutant ")
		if underMutant && (strings.HasPrefix(call, "mutants-01") || strings.Contains(call, RunPattern("TestA")+" ")) {
			t.Fatalf("an estimate ran %q", call)
		}
	}
}

func TestVerifyPutsBackAPinnedCaseFirst(t *testing.T) {
	// Arrange: TestLog asserts a log record.
	cfg, f := verifyConfig(t, ".:TestLog")
	f.reaches["TestLog"] = true
	script(f, map[string][]string{"1": {"TestB/x"}, "2": {"TestB/x"}})

	// Act
	v := verified(t, cfg, f)

	// Assert
	if len(v.PutBack) != 1 || v.PutBack[0].Case != ".:TestLog" || !strings.HasPrefix(v.PutBack[0].Mutant, "(pinned: asserts a log record") {
		t.Fatalf("put back %v, want TestLog as pinned", v.PutBack)
	}
}

func TestVerifyMeasuresAKeptCaseOnceAndADeletedOneEveryRun(t *testing.T) {
	// Arrange: two coverage runs.
	cfg, f := verifyConfig(t, ".:TestA")
	script(f, map[string][]string{"1": {"TestB/x"}, "2": {"TestB/x"}})

	// Act
	verified(t, cfg, f)

	// Assert
	runs := map[string]int{}
	for _, call := range f.calls {
		if strings.HasPrefix(call, "cover-") && strings.Contains(call, "/cov/") {
			for _, l := range []string{"TestA", "TestB/x"} {
				if strings.Contains(call, RunPattern(l)+" ") {
					runs[l]++
				}
			}
		}
	}
	if runs["TestA"] != 2 || runs["TestB/x"] != 1 {
		t.Fatalf("coverage runs = %v, want TestA twice and TestB/x once", runs)
	}
}

func TestVerifyRefusesADeletionSetNamingAnUnknownCase(t *testing.T) {
	// Arrange
	cfg, f := verifyConfig(t, ".:TestGone")
	log, _ := quietLog()

	// Act
	_, err := Measure(context.Background(), cfg, f.exec, log)

	// Assert
	if err == nil || !strings.Contains(err.Error(), "names cases the measurement holds none of: .:TestGone") {
		t.Fatalf("err = %v", err)
	}
}

func TestVerifyNamesAWholeTopLevelTest(t *testing.T) {
	// Arrange: deleting TestB deletes both its leaves.
	cfg, f := verifyConfig(t, ".:TestB")
	script(f, map[string][]string{"1": {"TestA"}, "2": {"TestA"}})

	// Act
	v := verified(t, cfg, f)

	// Assert
	if !reflect.DeepEqual(v.Deleted, []string{".:TestB/x", ".:TestB/y"}) {
		t.Fatalf("deleted = %v", v.Deleted)
	}
}

func TestMeasurePrebuildsAnE2ESuiteOnceAndEveryCaseReadsIt(t *testing.T) {
	// Arrange: the integration suite lives in a module of its own under the
	// root, spawns shim-store, and fills a shared prebuild first.
	cfg, f := verifyConfig(t)
	root := cfg.Root
	e2eDir := filepath.Join(root, "e2e", "integ")
	if err := os.MkdirAll(e2eDir, 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(e2eDir, "i_test.go"), []byte("package integ\n\nimport \"testing\"\n\nfunc TestI(t *testing.T) {}\n"), 0o644); err != nil {
		t.Fatal(err)
	}
	cfg.Suites[1].Module = "e2e"
	cfg.Suites[1].Prebuild = true
	script(f, map[string][]string{"1": {"TestA"}, "2": {"TestA"}})
	log, _ := quietLog()

	// Act
	m, err := Measure(context.Background(), cfg, f.exec, log)

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	if len(m.Cases) != 5 {
		t.Fatalf("%d cases, want the e2e module's TestI among 5", len(m.Cases))
	}
	prebuilds := 0
	for _, call := range f.calls {
		if strings.HasPrefix(call, "cover-01.test -test.run ^$") {
			prebuilds++
		}
	}
	if prebuilds != 1 {
		t.Fatalf("%d prebuilds, want 1; calls:\n%s", prebuilds, strings.Join(f.calls, "\n"))
	}
	for _, env := range f.mutantEnvs {
		if envOf(env, testenv.Programs) != "" && !strings.HasSuffix(envOf(env, testenv.Prebuilt), "prebuilt-01") {
			t.Fatalf("an e2e mutant run read no shared prebuild: %v", env)
		}
	}
}

func TestVerifyRunsDeletedUnitCasesTogether(t *testing.T) {
	// Arrange: TestA and TestB/x are deleted unit cases; only TestI kills.
	cfg, f := verifyConfig(t, ".:TestA", ".:TestB/x")
	script(f, map[string][]string{"1": {"TestI"}, "2": {"TestI"}})

	// Act
	v := verified(t, cfg, f)

	// Assert: each mutant ran the two deleted unit cases in one process.
	if v.SurvivedDeleted != 2 {
		t.Fatalf("verification = %+v", v)
	}
	lone := 0
	for _, call := range f.calls {
		if strings.Contains(call, "[mutant ") && strings.HasPrefix(call, "mutants-00") && (strings.Contains(call, RunPattern("TestA")+" ") || strings.Contains(call, RunPattern("TestB/x")+" ")) {
			lone++
		}
	}
	if lone != 0 {
		t.Fatalf("%d lone runs of a deleted unit case; calls:\n%s", lone, strings.Join(f.calls, "\n"))
	}
}
