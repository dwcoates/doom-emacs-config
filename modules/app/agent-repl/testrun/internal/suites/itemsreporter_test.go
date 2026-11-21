package suites

import (
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"agentrepl/testrun/internal/testtimes"
)

// testrun/vitest/items-reporter.mjs is driven here by node, its own runtime,
// with vitest's onTestRunEnd argument shaped by hand: no vitest runs. Each
// module is written as plain data ({moduleId, diagnostic, tests}) and lifted
// into the shape vitest hands a reporter (a TestModule's diagnostic() and
// children.allTests(), a TestCase's fullName, options, result() and
// diagnostic()) by liftModules.

// liftModules is the node prelude that shapes plain module data the way
// vitest's reporter API presents it.
const liftModules = `const lift = (ms) => ms.map((m) => ({
  moduleId: m.moduleId,
  diagnostic: () => ({prepareDuration: 0, environmentSetupDuration: 0, setupDuration: 0, collectDuration: 0, duration: 0, ...m.diagnostic}),
  children: {*allTests() {
    for (const t of m.tests ?? []) yield {
      fullName: t.fullName,
      options: {mode: t.mode ?? "run"},
      result: () => ({state: t.state}),
      diagnostic: () => (t.duration === undefined ? undefined : {duration: t.duration, startTime: t.startTime}),
    };
  }},
}));`

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
	script := liftModules + "const { default: R } = await import(process.argv[1]); new R().onTestRunEnd(lift(" + files + "), [], \"passed\");"
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
	  {moduleId: "/p/a.test.ts", diagnostic: {prepareDuration: 100, environmentSetupDuration: 200, setupDuration: 300, collectDuration: 400, duration: 500}},
	  {moduleId: "/p/b.test.ts", diagnostic: {collectDuration: 250}}
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
	// Arrange: a test at the top, one in nested suites, a failed one, a
	// skipped one vitest never ran and a todo.
	target := filepath.Join(t.TempDir(), "items.json")
	tests := filepath.Join(t.TempDir(), "tests.json")
	files := `[{moduleId: "/p/a.test.ts", tests: [
	  {fullName: "top", state: "passed", duration: 12, startTime: 1800000000250},
	  {fullName: "outer > inner > deep", state: "failed", duration: 1500, startTime: 1800000001000},
	  {fullName: "later", state: "skipped", mode: "skip"},
	  {fullName: "someday", state: "skipped", mode: "todo"}
	]}]`

	// Act
	out, err := runReporterTo(t, files, target, true, tests)

	// Assert: the file the runner reads, read the way the runner reads it.
	if err != nil {
		t.Fatalf("reporter: %v\n%s", err, out)
	}
	got, err := testtimes.ParseVitest("webapp", tests, "/p")
	want := []testtimes.Test{
		{Suite: "webapp", Name: "a.test.ts > top", Outcome: testtimes.Pass, Seconds: 0.012, Start: time.Unix(1_800_000_000, 250_000_000)},
		{Suite: "webapp", Name: "a.test.ts > outer > inner > deep", Outcome: testtimes.Fail, Seconds: 1.5, Start: time.Unix(1_800_000_001, 0)},
		{Suite: "webapp", Name: "a.test.ts > later", Outcome: testtimes.Skip, Seconds: 0},
		{Suite: "webapp", Name: "a.test.ts > someday", Outcome: testtimes.Skip, Seconds: 0},
	}
	if err != nil || len(got) != len(want) {
		t.Fatalf("ParseVitest = %+v, %v", got, err)
	}
	for i := range want {
		if got[i].Name != want[i].Name || got[i].Outcome != want[i].Outcome || got[i].Seconds != want[i].Seconds || !got[i].Start.Equal(want[i].Start) {
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

func TestItemsReporterRefusesAnUnknownTestState(t *testing.T) {
	// Arrange
	files := `[{moduleId: "/p/a.test.ts", tests: [{fullName: "x", state: "exploded"}]}]`

	// Act
	out, err := runReporter(t, files, filepath.Join(t.TempDir(), "items.json"), true)

	// Assert
	if err == nil || !strings.Contains(out, "ended in the unknown state exploded") {
		t.Fatalf("err = %v, out = %s", err, out)
	}
}
