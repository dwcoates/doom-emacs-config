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
	"math"
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

// CPUSeriesName is the gauge every test's CPU lands in, one sample per run,
// labeled as SeriesName (AttributeCPU says how a test's share is measured).
const CPUSeriesName = "agentrepl_test_cpu_seconds"

// ProcessesSeriesName is the gauge every test's share of the processes its
// unit started lands in, beside its CPU (AttributeCPU).
const ProcessesSeriesName = "agentrepl_test_processes_started"

// LogRecordsSeriesName is the gauge every test's counted log records land in,
// for the tests whose harness counts them (Test.LogRecords).
const LogRecordsSeriesName = "agentrepl_test_log_records"

// SuiteSeriesName is the gauge every suite's slot time lands in, one sample
// per run.
const SuiteSeriesName = "agentrepl_test_suite_seconds"

// The gauges every unit's resource use lands in, one sample per run each: a
// unit is one process (a chunk of tests, a build, a harness group), the
// finest grain the kernel accounts resources at.
const (
	UnitWallSeriesName        = "agentrepl_test_unit_wall_seconds"
	UnitCPUSeriesName         = "agentrepl_test_unit_cpu_seconds"
	UnitMaxRSSSeriesName      = "agentrepl_test_unit_max_rss_bytes"
	UnitBlockReadsSeriesName  = "agentrepl_test_unit_block_reads"
	UnitBlockWritesSeriesName = "agentrepl_test_unit_block_writes"
)

// RunSeriesName is the gauge a whole run's wall time lands in, from its
// first unit's start to its last unit's end, one sample per run;
// RunPredictedSeriesName the plan's predicted makespan beside it.
const (
	RunSeriesName          = "agentrepl_test_run_seconds"
	RunPredictedSeriesName = "agentrepl_test_run_predicted_seconds"
)

// SuiteUnitsSeriesName is the gauge every suite's unit count lands in, one
// sample per run for each state: "run" (units that ran, each a process) and
// "reused" (units a pass record let the run skip). Batching trades these:
// larger batches start fewer processes and reuse less after a change.
const SuiteUnitsSeriesName = "agentrepl_test_suite_units"

// The unit states SuiteUnitsSeriesName is labeled with.
const (
	UnitsRun    = "run"
	UnitsReused = "reused"
)

// The gauges the shadow-mode impact recording lands in (internal/impact), one
// sample per run each, labeled with the suite and the trigger.
const (
	// ImpactWouldSkipSeriesName counts the tests selection WOULD have skipped
	// (a passing record matched every dependency), and
	// ImpactWouldSkipCPUSeriesName is the CPU this run spent on them.
	ImpactWouldSkipSeriesName    = "agentrepl_test_impact_would_skip"
	ImpactWouldSkipCPUSeriesName = "agentrepl_test_impact_would_skip_cpu_seconds"
	// ImpactMissesSeriesName counts the tests that failed although selection
	// would have skipped them: zero, over a stretch of merges, is what
	// enforcement waits for.
	ImpactMissesSeriesName = "agentrepl_test_impact_misses"
	// ImpactRecordsSeriesName counts the records the run wrote.
	ImpactRecordsSeriesName = "agentrepl_test_impact_records_written"
	// ImpactOverheadSeriesName is the CPU cutting and reducing the coverage
	// windows cost the harnesses.
	ImpactOverheadSeriesName = "agentrepl_test_impact_overhead_cpu_seconds"
	// ImpactBreadthSeriesName is, per test, the share of the daemon's files
	// its record covers.
	ImpactBreadthSeriesName = "agentrepl_test_impact_record_breadth"
	// ImpactIncompleteSeriesName counts the tests whose recording was
	// incomplete (a window the harness could not cut, a daemon killed before
	// it wrote its counters): never recorded, always would run.
	ImpactIncompleteSeriesName = "agentrepl_test_impact_incomplete"
	// ImpactErrorsSeriesName is 1 when recording or deciding failed in this
	// run, and 0 otherwise.
	ImpactErrorsSeriesName = "agentrepl_test_impact_errors"
)

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
	// Start is when the test's own clock started, where its runner says
	// (zero where it does not); it ran from Start for Seconds.
	Start time.Time
	// CPU is the CPU seconds AttributeCPU gave the test, when HasCPU.
	CPU    float64
	HasCPU bool
	// Processes is the test's share of the processes its unit's tree started
	// (AttributeCPU), when HasCPU.
	Processes float64
	// LogRecords is the log records the test's harness counted its
	// production processes writing (testenv.CountLogRecords), when
	// HasLogRecords.
	LogRecords    int
	HasLogRecords bool
}

// Suite is one suite's time in a run.
type Suite struct {
	Name    string
	Outcome string
	Seconds float64
	// Batching names the suite's own file batching (roster.Batching.Label),
	// or Unbatched.
	Batching string
}

// Unbatched is the batching label of a suite whose files are not batched.
const Unbatched = "unbatched"

// Unit is one unit's resource use in a run.
type Unit struct {
	Suite   string
	ID      string
	Outcome string
	// Wall and CPU are seconds.
	Wall, CPU float64
	// MaxRSSBytes is the largest resident set of the unit's process or any
	// one descendant it reaped.
	MaxRSSBytes int64
	// BlockReads and BlockWrites are block I/O operations to storage.
	BlockReads, BlockWrites int64
}

// Run is a whole run's time: its wall time, its plan's prediction, and
// whether every suite passed.
type Run struct {
	Seconds, PredictedSeconds float64
	Outcome                   string
}

// SuiteUnits is how many of a suite's units a run ran and reused.
type SuiteUnits struct {
	Suite       string
	Run, Reused int
	// Batching is the suite's own, as Suite.Batching.
	Batching string
}

// Report is everything one run pushes. Batching names the file batching the
// run's suites were cut under (suites.BatchingLabel); it labels the run, suite
// and unit-count series so runs under different batch sizes compare.
type Report struct {
	Tests      []Test
	Suites     []Suite
	Units      []Unit
	Run        *Run
	SuiteUnits []SuiteUnits
	Batching   string
	// Impact is the run's shadow-mode impact recording, one per suite that
	// recorded.
	Impact []Impact
}

// Impact is one suite's shadow-mode impact recording in a run.
type Impact struct {
	Suite          string
	WouldSkip      int
	WouldSkipCPU   float64
	Misses         int
	RecordsWritten int
	OverheadCPU    float64
	// Incomplete counts tests whose recording was incomplete.
	Incomplete int
	// Errors is 1 when recording or deciding failed.
	Errors int
	// Breadth is each recorded test's share of the daemon's files.
	Breadth []ImpactBreadth
}

// ImpactBreadth is how much of the program one test's record covers.
type ImpactBreadth struct {
	Test  string
	Share float64
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

// WithLogRecords gives each of a Go package PKG's tests the log records its
// harness counted (testenv.ReadLogRecords, by test name as `go test` names
// it). A count naming no test the unit reported is an error: the harness and
// the runner disagree about what ran.
func WithLogRecords(tests []Test, pkg string, counts map[string]int) error {
	byName := make(map[string]int, len(tests))
	for i, t := range tests {
		byName[t.Name] = i
	}
	var unknown []string
	for name, n := range counts {
		i, ok := byName[pkg+"."+name]
		if !ok {
			unknown = append(unknown, name)
			continue
		}
		tests[i].LogRecords, tests[i].HasLogRecords = n, true
	}
	if len(unknown) > 0 {
		sort.Strings(unknown)
		return fmt.Errorf("testtimes: log records were counted for tests the unit did not report: %s", strings.Join(unknown, ", "))
	}
	return nil
}

// goOutcomes maps a go result word to its outcome.
var goOutcomes = map[string]string{"PASS": Pass, "FAIL": Fail, "SKIP": Skip}

// StartMarker opens the line a unit's runner writes as each test's clock
// starts: "TESTRUN-TEST-START <unix seconds> <test name>". `testrun
// exec-test` writes one at each `=== RUN` and `=== CONT` of a Go test binary
// (gobuild.ExecTest), and the ERT driver one as each test starts
// (testrun/ert/driver.el); a test's latest one before its result is its Start.
const StartMarker = "TESTRUN-TEST-START"

// startLine is a StartMarker line: its unix seconds and test name.
var startLine = regexp.MustCompile(`^` + StartMarker + ` ([0-9]+(?:\.[0-9]+)?) (\S+)$`)

// StartLine is the StartMarker line saying NAME's clock started AT.
func StartLine(at time.Time, name string) string {
	return fmt.Sprintf("%s %d.%09d %s", StartMarker, at.Unix(), at.Nanosecond(), name)
}

// startOf reads LINE as a StartMarker line: its test's name and start.
func startOf(line string) (string, time.Time, bool, error) {
	m := startLine.FindStringSubmatch(strings.TrimRight(line, "\r"))
	if m == nil {
		return "", time.Time{}, false, nil
	}
	whole, frac, _ := strings.Cut(m[1], ".")
	sec, err := strconv.ParseInt(whole, 10, 64)
	if err != nil {
		return "", time.Time{}, false, fmt.Errorf("testtimes: unreadable start in %q: %w", line, err)
	}
	nanos := int64(0)
	if frac != "" {
		frac = (frac + "000000000")[:9]
		if nanos, err = strconv.ParseInt(frac, 10, 64); err != nil {
			return "", time.Time{}, false, fmt.Errorf("testtimes: unreadable start in %q: %w", line, err)
		}
	}
	return m[2], time.Unix(sec, nanos), true, nil
}

// ParseGo reads every test and subtest result line of a `go test -v` run of
// package PKG, each named "<PKG>.<TestName>[/<subtest>...]", and its Start
// from the StartMarker lines `testrun exec-test` wrote.
func ParseGo(suite, pkg string, out []byte) ([]Test, error) {
	var tests []Test
	starts := map[string]time.Time{}
	for line := range strings.SplitSeq(string(out), "\n") {
		name, at, ok, err := startOf(line)
		if err != nil {
			return nil, err
		}
		if ok {
			starts[name] = at
			continue
		}
		m := gotest.ResultLine.FindStringSubmatch(line)
		if m == nil {
			continue
		}
		secs, err := strconv.ParseFloat(m[3], 64)
		if err != nil {
			return nil, fmt.Errorf("testtimes: unreadable seconds in %q: %w", line, err)
		}
		tests = append(tests, Test{Suite: suite, Name: pkg + "." + m[2], Outcome: goOutcomes[m[1]], Seconds: secs, Start: starts[m[2]]})
	}
	return tests, nil
}

// ertResultLine is an ERT batch result line: its outcome word, name and
// seconds ("   passed  3/13  agent-repl-test-x (0.000231 sec)").
var ertResultLine = regexp.MustCompile(`^\s*(passed|FAILED|skipped)\s+\d+/\d+\s+(\S+)\s+\(([0-9.]+) sec\)`)

// ertOutcomes maps an ERT result word to its outcome.
var ertOutcomes = map[string]string{"passed": Pass, "FAILED": Fail, "skipped": Skip}

// ParseERT reads every test result line of an ERT batch run, and its Start
// from the StartMarker lines the driver wrote.
func ParseERT(suite string, out []byte) ([]Test, error) {
	var tests []Test
	starts := map[string]time.Time{}
	for line := range strings.SplitSeq(string(out), "\n") {
		name, at, ok, err := startOf(line)
		if err != nil {
			return nil, err
		}
		if ok {
			starts[name] = at
			continue
		}
		m := ertResultLine.FindStringSubmatch(line)
		if m == nil {
			continue
		}
		secs, err := strconv.ParseFloat(m[3], 64)
		if err != nil {
			return nil, fmt.Errorf("testtimes: unreadable seconds in %q: %w", line, err)
		}
		tests = append(tests, Test{Suite: suite, Name: m[2], Outcome: ertOutcomes[m[1]], Seconds: secs, Start: starts[m[2]]})
	}
	return tests, nil
}

// vitestTest is one entry of items-reporter.mjs's tests file.
type vitestTest struct {
	File    string  `json:"file"`
	Name    string  `json:"name"`
	State   string  `json:"state"`
	Seconds float64 `json:"seconds"`
	// Start is the unix seconds the test started at; absent for a test that
	// never ran (skipped, todo).
	Start float64 `json:"start"`
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
		var start time.Time
		if r.Start > 0 {
			// To the microsecond: vitest's own start is in milliseconds, and a
			// float's nanoseconds of a unix instant are not exact.
			start = time.UnixMicro(int64(math.Round(r.Start * 1e6)))
		}
		tests = append(tests, Test{Suite: suite, Name: file + " > " + r.Name, Outcome: outcome, Seconds: r.Seconds, Start: start})
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

// Encode writes REPORT as Prometheus text exposition, every sample labeled
// with TRIGGER and stamped AT.
func Encode(report Report, trigger string, at time.Time) []byte {
	var b bytes.Buffer
	ms := at.UnixMilli()
	num := func(v float64) string { return strconv.FormatFloat(v, 'g', -1, 64) }
	for _, t := range report.Tests {
		labels := fmt.Sprintf("suite=\"%s\",test=\"%s\",outcome=\"%s\",trigger=\"%s\"",
			labelValue(t.Suite), labelValue(t.Name), t.Outcome, labelValue(trigger))
		fmt.Fprintf(&b, "%s{%s} %s %d\n", SeriesName, labels, num(t.Seconds), ms)
		if t.HasCPU {
			fmt.Fprintf(&b, "%s{%s} %s %d\n", CPUSeriesName, labels, num(t.CPU), ms)
			fmt.Fprintf(&b, "%s{%s} %s %d\n", ProcessesSeriesName, labels, num(t.Processes), ms)
		}
		if t.HasLogRecords {
			fmt.Fprintf(&b, "%s{%s} %d %d\n", LogRecordsSeriesName, labels, t.LogRecords, ms)
		}
	}
	batching := labelValue(report.Batching)
	for _, s := range report.Suites {
		fmt.Fprintf(&b, "%s{suite=\"%s\",outcome=\"%s\",trigger=\"%s\",batching=\"%s\"} %s %d\n",
			SuiteSeriesName, labelValue(s.Name), s.Outcome, labelValue(trigger), labelValue(s.Batching), num(s.Seconds), ms)
	}
	if r := report.Run; r != nil {
		for _, v := range []struct {
			name  string
			value float64
		}{{RunSeriesName, r.Seconds}, {RunPredictedSeriesName, r.PredictedSeconds}} {
			fmt.Fprintf(&b, "%s{outcome=\"%s\",trigger=\"%s\",batching=\"%s\"} %s %d\n",
				v.name, r.Outcome, labelValue(trigger), batching, num(v.value), ms)
		}
	}
	for _, su := range report.SuiteUnits {
		for _, v := range []struct {
			state string
			n     int
		}{{UnitsRun, su.Run}, {UnitsReused, su.Reused}} {
			fmt.Fprintf(&b, "%s{suite=\"%s\",state=\"%s\",trigger=\"%s\",batching=\"%s\"} %d %d\n",
				SuiteUnitsSeriesName, labelValue(su.Suite), v.state, labelValue(trigger), labelValue(su.Batching), v.n, ms)
		}
	}
	for _, im := range report.Impact {
		labels := fmt.Sprintf("suite=\"%s\",trigger=\"%s\"", labelValue(im.Suite), labelValue(trigger))
		for _, v := range []struct {
			name  string
			value string
		}{
			{ImpactWouldSkipSeriesName, strconv.Itoa(im.WouldSkip)},
			{ImpactWouldSkipCPUSeriesName, num(im.WouldSkipCPU)},
			{ImpactMissesSeriesName, strconv.Itoa(im.Misses)},
			{ImpactRecordsSeriesName, strconv.Itoa(im.RecordsWritten)},
			{ImpactOverheadSeriesName, num(im.OverheadCPU)},
			{ImpactIncompleteSeriesName, strconv.Itoa(im.Incomplete)},
			{ImpactErrorsSeriesName, strconv.Itoa(im.Errors)},
		} {
			fmt.Fprintf(&b, "%s{%s} %s %d\n", v.name, labels, v.value, ms)
		}
		for _, br := range im.Breadth {
			fmt.Fprintf(&b, "%s{suite=\"%s\",test=\"%s\",trigger=\"%s\"} %s %d\n",
				ImpactBreadthSeriesName, labelValue(im.Suite), labelValue(br.Test), labelValue(trigger), num(br.Share), ms)
		}
	}
	for _, u := range report.Units {
		labels := fmt.Sprintf("suite=\"%s\",unit=\"%s\",outcome=\"%s\",trigger=\"%s\"",
			labelValue(u.Suite), labelValue(u.ID), u.Outcome, labelValue(trigger))
		for _, v := range []struct {
			name  string
			value string
		}{
			{UnitWallSeriesName, num(u.Wall)},
			{UnitCPUSeriesName, num(u.CPU)},
			{UnitMaxRSSSeriesName, strconv.FormatInt(u.MaxRSSBytes, 10)},
			{UnitBlockReadsSeriesName, strconv.FormatInt(u.BlockReads, 10)},
			{UnitBlockWritesSeriesName, strconv.FormatInt(u.BlockWrites, 10)},
		} {
			fmt.Fprintf(&b, "%s{%s} %s %d\n", v.name, labels, v.value, ms)
		}
	}
	return b.Bytes()
}

// pushBound bounds the push, so a store that does not answer cannot hold the
// run's exit.
const pushBound = 10 * time.Second

// Push sends REPORT, stamped AT, to the store under the target's root,
// labeled as the test runner's.
func Push(ctx context.Context, client *http.Client, target Target, report Report, at time.Time) error {
	ctx, cancel := context.WithTimeout(ctx, pushBound)
	defer cancel()
	body := Encode(report, target.Trigger, at)
	if err := storeimport.Post(ctx, client, target.Root, body, "instance=testrun"); err != nil {
		return fmt.Errorf("testtimes: %d test, %d suite and %d unit records were not pushed: %w",
			len(report.Tests), len(report.Suites), len(report.Units), err)
	}
	return nil
}
