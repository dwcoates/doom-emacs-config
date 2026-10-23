// Package testtimes is every individual test's time in a run, read off the
// output of the runner that ran it, and pushed to the always-on metrics store
// when the run ends.
//
// Each test is as fine as its runner reports out of the box: every `go test`
// test and subtest (its -v result line), every ERT test (its batch result
// line), every vitest test (testrun/vitest/items-reporter.mjs's tests file),
// and every harness group (its TESTRUN-ITEM line). A test's outcome is its
// own where the runner says it (pass, fail, skip), and its unit's otherwise.
//
// THE PUSH IS THE MERGE QUEUE'S. A run pushes only when the environment names
// a metrics root and a trigger (EnvRoot, EnvTrigger), which only the merge
// queue's gate sets (daemon/internal/merge gateArgv). The runner takes both
// out of its own environment before any unit starts, so a test that runs the
// runner again never pushes into the owner's store.
package testtimes

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"net/http"
	"os"
	"regexp"
	"sort"
	"strconv"
	"strings"
	"time"

	"agentrepl/logging/metrics/storeimport"
	"agentrepl/testrun/internal/gotest"
)

// The environment the merge queue's gate names the push with.
const (
	EnvRoot    = "AGENT_REPL_TEST_METRICS_ROOT"
	EnvTrigger = "AGENT_REPL_TEST_METRICS_TRIGGER"
)

// SeriesName is the gauge every test's time lands in, one sample per run.
const SeriesName = "agentrepl_test_seconds"

// SuiteSeriesName is the gauge every suite's slot time lands in, one sample
// per run.
const SuiteSeriesName = "agentrepl_test_suite_seconds"

// The outcomes a test is labeled with.
const (
	Pass = "pass"
	Fail = "fail"
	Skip = "skip"
)

// Test is one test's time in a run.
type Test struct {
	Suite   string
	Name    string
	Outcome string
	Seconds float64
}

// Suite is one suite's time in a run.
type Suite struct {
	Name    string
	Outcome string
	Seconds float64
}

// Target is where a run pushes and what ran it.
type Target struct {
	Root    string
	Trigger string
}

// TakeTarget reads the push's target from the environment and removes both
// variables from it, so no unit inherits them. Neither set is no push (a
// zero Target); one without the other is a gate that broke its contract.
func TakeTarget() (Target, error) {
	root, hasRoot := os.LookupEnv(EnvRoot)
	trigger, hasTrigger := os.LookupEnv(EnvTrigger)
	for _, name := range []string{EnvRoot, EnvTrigger} {
		if err := os.Unsetenv(name); err != nil {
			return Target{}, fmt.Errorf("testtimes: unset %s: %w", name, err)
		}
	}
	switch {
	case !hasRoot && !hasTrigger:
		return Target{}, nil
	case root == "" || trigger == "":
		return Target{}, fmt.Errorf("testtimes: %s=%q and %s=%q: a push names both or neither", EnvRoot, root, EnvTrigger, trigger)
	}
	return Target{Root: root, Trigger: trigger}, nil
}

// Enabled reports whether the run pushes.
func (t Target) Enabled() bool { return t.Root != "" }

// goOutcomes maps a go result word to its outcome.
var goOutcomes = map[string]string{"PASS": Pass, "FAIL": Fail, "SKIP": Skip}

// ParseGo reads every test and subtest result line of a `go test -v` run of
// package PKG, each named "<PKG>.<TestName>[/<subtest>...]".
func ParseGo(suite, pkg string, out []byte) ([]Test, error) {
	var tests []Test
	for line := range strings.SplitSeq(string(out), "\n") {
		m := gotest.ResultLine.FindStringSubmatch(line)
		if m == nil {
			continue
		}
		secs, err := strconv.ParseFloat(m[3], 64)
		if err != nil {
			return nil, fmt.Errorf("testtimes: unreadable seconds in %q: %w", line, err)
		}
		tests = append(tests, Test{Suite: suite, Name: pkg + "." + m[2], Outcome: goOutcomes[m[1]], Seconds: secs})
	}
	return tests, nil
}

// ertResultLine is an ERT batch result line: its outcome word, name and
// seconds ("   passed  3/13  agent-repl-test-x (0.000231 sec)").
var ertResultLine = regexp.MustCompile(`^\s*(passed|FAILED|skipped)\s+\d+/\d+\s+(\S+)\s+\(([0-9.]+) sec\)`)

// ertOutcomes maps an ERT result word to its outcome.
var ertOutcomes = map[string]string{"passed": Pass, "FAILED": Fail, "skipped": Skip}

// ParseERT reads every test result line of an ERT batch run.
func ParseERT(suite string, out []byte) ([]Test, error) {
	var tests []Test
	for line := range strings.SplitSeq(string(out), "\n") {
		m := ertResultLine.FindStringSubmatch(line)
		if m == nil {
			continue
		}
		secs, err := strconv.ParseFloat(m[3], 64)
		if err != nil {
			return nil, fmt.Errorf("testtimes: unreadable seconds in %q: %w", line, err)
		}
		tests = append(tests, Test{Suite: suite, Name: m[2], Outcome: ertOutcomes[m[1]], Seconds: secs})
	}
	return tests, nil
}

// vitestTest is one entry of items-reporter.mjs's tests file.
type vitestTest struct {
	File    string  `json:"file"`
	Name    string  `json:"name"`
	State   string  `json:"state"`
	Seconds float64 `json:"seconds"`
}

// vitestOutcomes maps a vitest task state to its outcome.
var vitestOutcomes = map[string]string{"pass": Pass, "fail": Fail, "skip": Skip, "todo": Skip}

// VitestTestsEnv names the tests file items-reporter.mjs writes.
const VitestTestsEnv = "AGENT_REPL_TESTRUN_TESTS"

// ParseVitest reads items-reporter.mjs's tests file at PATH, each test named
// "<file relative to DIR> > <its suites and name>".
func ParseVitest(suite, path, dir string) ([]Test, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, fmt.Errorf("testtimes: read the chunk's tests file: %w", err)
	}
	var raw []vitestTest
	if err := json.Unmarshal(data, &raw); err != nil {
		return nil, fmt.Errorf("testtimes: %s is not an items-reporter tests file: %w", path, err)
	}
	tests := make([]Test, 0, len(raw))
	for _, r := range raw {
		outcome, ok := vitestOutcomes[r.State]
		if !ok || r.Seconds < 0 {
			return nil, fmt.Errorf("testtimes: %s holds %q in state %q taking %v seconds", path, r.Name, r.State, r.Seconds)
		}
		file := strings.TrimPrefix(strings.TrimPrefix(r.File, dir), "/")
		tests = append(tests, Test{Suite: suite, Name: file + " > " + r.Name, Outcome: outcome, Seconds: r.Seconds})
	}
	return tests, nil
}

// OfItems turns a unit's item timings (a harness group's) into tests, each
// with the unit's OUTCOME.
func OfItems(suite string, items map[string]float64, outcome string) []Test {
	tests := make([]Test, 0, len(items))
	for name, secs := range items {
		tests = append(tests, Test{Suite: suite, Name: name, Outcome: outcome, Seconds: secs})
	}
	sort.Slice(tests, func(i, j int) bool { return tests[i].Name < tests[j].Name })
	return tests
}

// labelValue escapes V for a Prometheus text exposition label value.
func labelValue(v string) string {
	r := strings.NewReplacer(`\`, `\\`, `"`, `\"`, "\n", `\n`)
	return r.Replace(v)
}

// Encode writes TESTS and SUITES as Prometheus text exposition, every sample
// labeled with TRIGGER and stamped AT.
func Encode(tests []Test, suites []Suite, trigger string, at time.Time) []byte {
	var b bytes.Buffer
	ms := at.UnixMilli()
	for _, t := range tests {
		fmt.Fprintf(&b, "%s{suite=\"%s\",test=\"%s\",outcome=\"%s\",trigger=\"%s\"} %s %d\n",
			SeriesName, labelValue(t.Suite), labelValue(t.Name), t.Outcome, labelValue(trigger),
			strconv.FormatFloat(t.Seconds, 'g', -1, 64), ms)
	}
	for _, s := range suites {
		fmt.Fprintf(&b, "%s{suite=\"%s\",outcome=\"%s\",trigger=\"%s\"} %s %d\n",
			SuiteSeriesName, labelValue(s.Name), s.Outcome, labelValue(trigger),
			strconv.FormatFloat(s.Seconds, 'g', -1, 64), ms)
	}
	return b.Bytes()
}

// pushBound bounds the push, so a store that does not answer cannot hold the
// run's exit.
const pushBound = 10 * time.Second

// Push sends TESTS and SUITES, stamped AT, to the store under the target's
// root, labeled as the test runner's.
func Push(ctx context.Context, client *http.Client, target Target, tests []Test, suites []Suite, at time.Time) error {
	ctx, cancel := context.WithTimeout(ctx, pushBound)
	defer cancel()
	body := Encode(tests, suites, target.Trigger, at)
	if err := storeimport.Post(ctx, client, target.Root, body, "instance=testrun"); err != nil {
		return fmt.Errorf("testtimes: %d test and %d suite times were not pushed: %w", len(tests), len(suites), err)
	}
	return nil
}
