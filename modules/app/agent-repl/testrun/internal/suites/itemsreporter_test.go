package suites

import (
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	"agentrepl/testrun/internal/testtimes"
)

// testrun/vitest/items-reporter.mjs is driven here by node, its own runtime,
// with vitest's onFinished argument shaped by hand: no vitest runs.

func runReporter(t *testing.T, files, target string, setTarget bool) (string, error) {
	t.Helper()
	return runReporterTo(t, files, target, setTarget, filepath.Join(t.TempDir(), "tests.json"))
}

// runReporterTo runs the reporter with TESTS as its tests file ("" for none).
func runReporterTo(t *testing.T, files, target string, setTarget bool, tests string) (string, error) {
	t.Helper()
	reporter, err := filepath.Abs(filepath.Join("..", "..", "vitest", "items-reporter.mjs"))
	if err != nil {
		t.Fatal(err)
	}
	script := "const { default: R } = await import(process.argv[1]); new R().onFinished(" + files + ");"
	cmd := exec.Command("node", "--input-type=module", "-e", script, reporter)
	cmd.Env = os.Environ()
	if setTarget {
		cmd.Env = append(cmd.Env, VitestItemsEnv+"="+target)
	} else {
		cmd.Env = append(cmd.Env, VitestItemsEnv+"=")
	}
	cmd.Env = append(cmd.Env, testtimes.VitestTestsEnv+"="+tests)
	out, err := cmd.CombinedOutput()
	return string(out), err
}

func TestItemsReporterWritesEachFilesWholeCost(t *testing.T) {
	// Arrange: every phase vitest times, and a file missing the optional ones.
	target := filepath.Join(t.TempDir(), "items.json")
	files := `[
	  {filepath: "/p/a.test.ts", prepareDuration: 100, environmentLoad: 200, setupDuration: 300, collectDuration: 400, result: {duration: 500}},
	  {filepath: "/p/b.test.ts", collectDuration: 250}
	]`

	// Act
	out, err := runReporter(t, files, target, true)

	// Assert: the file the runner reads, read the way the runner reads it.
	if err != nil {
		t.Fatalf("reporter: %v\n%s", err, out)
	}
	got, err := ParseVitestItems(target, "/p", []string{"a.test.ts", "b.test.ts"})
	if err != nil || got["a.test.ts"] != 1.5 || got["b.test.ts"] != 0.25 {
		t.Fatalf("ParseVitestItems = %v, %v", got, err)
	}
}

func TestItemsReporterRefusesAMissingTarget(t *testing.T) {
	// Act
	out, err := runReporter(t, "[]", "", false)

	// Assert
	if err == nil || !strings.Contains(out, "AGENT_REPL_TESTRUN_ITEMS names no output file") {
		t.Fatalf("err = %v, out = %s", err, out)
	}
}

func TestItemsReporterWritesEveryTestsTime(t *testing.T) {
	// Arrange: a test at the top, one in nested suites, a failed one and a
	// skipped one vitest never ran.
	target := filepath.Join(t.TempDir(), "items.json")
	tests := filepath.Join(t.TempDir(), "tests.json")
	files := `[{filepath: "/p/a.test.ts", tasks: [
	  {type: "test", name: "top", result: {state: "pass", duration: 12}},
	  {type: "suite", name: "outer", tasks: [{type: "suite", name: "inner", tasks: [
	    {type: "test", name: "deep", result: {state: "fail", duration: 1500}}]}]},
	  {type: "test", name: "later", mode: "skip"}
	]}]`

	// Act
	out, err := runReporterTo(t, files, target, true, tests)

	// Assert: the file the runner reads, read the way the runner reads it.
	if err != nil {
		t.Fatalf("reporter: %v\n%s", err, out)
	}
	got, err := testtimes.ParseVitest("webapp", tests, "/p")
	want := []testtimes.Test{
		{Suite: "webapp", Name: "a.test.ts > top", Outcome: testtimes.Pass, Seconds: 0.012},
		{Suite: "webapp", Name: "a.test.ts > outer > inner > deep", Outcome: testtimes.Fail, Seconds: 1.5},
		{Suite: "webapp", Name: "a.test.ts > later", Outcome: testtimes.Skip, Seconds: 0},
	}
	if err != nil || len(got) != len(want) {
		t.Fatalf("ParseVitest = %+v, %v", got, err)
	}
	for i := range want {
		if got[i] != want[i] {
			t.Fatalf("test %d = %+v, want %+v", i, got[i], want[i])
		}
	}
}

func TestItemsReporterRefusesAMissingTestsTarget(t *testing.T) {
	// Act
	out, err := runReporterTo(t, "[]", filepath.Join(t.TempDir(), "items.json"), true, "")

	// Assert
	if err == nil || !strings.Contains(out, "AGENT_REPL_TESTRUN_TESTS names no output file") {
		t.Fatalf("err = %v, out = %s", err, out)
	}
}
