// Package cli is `testrun run`: plan every selected suite onto the host's core
// slots, run it, and report it in bin/test-all.sh's established shape.
package cli

import (
	"context"
	"errors"
	"fmt"
	"io"
	"path/filepath"
	"sort"
	"strings"
	"sync"
	"time"

	testrunv1 "agentrepl/proto/testrun/v1"

	"agentrepl/testrun/internal/events"
	"agentrepl/testrun/internal/history"
	"agentrepl/testrun/internal/plan"
	"agentrepl/testrun/internal/protofile"
	"agentrepl/testrun/internal/ramdisk"
	"agentrepl/testrun/internal/reuse"
	"agentrepl/testrun/internal/run"
	"agentrepl/testrun/internal/selection"
	"agentrepl/testrun/internal/suites"
	"agentrepl/testrun/internal/testtimes"
	"agentrepl/testrun/roster"
)

// Deps are the run's seams to the outside.
type Deps struct {
	Log *run.Log
	// Events is where the run reports its plan, each unit and its end
	// (--events-fd); nil reports nowhere.
	Events *events.Emitter
	Exec   run.Executor
	Clock  run.Clock
	// At is what each unit's time limit waits on (run.Runner.At).
	At func(deadline time.Time) <-chan time.Time
	// GoCache keeps the Go build cache's trim out of every unit's time limit
	// (run.Runner.GoCache).
	GoCache run.GoCache
	// Slots is how many units run at once: the host's core budget.
	Slots int
	// Cores is the host's core pool every run on the host shares.
	Cores run.Cores
	// HistoryPath is the host's timing history.
	HistoryPath string
	// Build turns a suite into units.
	Build func(suites.Layout, roster.Suite) (suites.Units, error)
	// Git answers the checkout's branch and HEAD, for --record.
	Git func(repo string) (branch, commit string, err error)
	// Self is the testrun binary.
	Self string
	// Work is the run's scratch directory, which the caller removes.
	Work string
	// Pid names the run in the CSV.
	Pid int
	// Reuse cuts each selected suite into its units, decides which an
	// earlier run's pass covers, confines the rest, and records their
	// outcomes (agentrepl/testrun/internal/reuse).
	Reuse Reuser
	// RunLog is the run's own log file; a coverage run writes its merged
	// baseline beside it. "" when testrun was started directly.
	RunLog string
	// Metrics is where the run pushes every test's time when it ends, and
	// what ran it; the zero Target pushes nothing (testtimes.TakeTarget).
	Metrics testtimes.Target
	// PushTimes sends the run's report, stamped AT, to TARGET (testtimes.Push).
	PushTimes func(ctx context.Context, target testtimes.Target, report testtimes.Report, at time.Time) error
}

// Reuser is the run's pass-record seam (reuse.Runner).
type Reuser interface {
	// Probe answers whether the host can confine a unit, once per run.
	Probe() error
	// Decide cuts suite s, built as u, into its units and decides which
	// this run reuses.
	Decide(s roster.Suite, u suites.Units, mode reuse.Mode, sandboxErr error) (reuse.SuiteVerdict, error)
	Unplaced(verdicts []reuse.SuiteVerdict) ([]string, error)
	Confine(v reuse.SuiteVerdict, u suites.Units) suites.Units
	Record(mode reuse.Mode, verdicts []reuse.SuiteVerdict, results []run.Result) error
}

// RunConditions names what a run's units measure under, for the timing
// history they plan from (history.DefaultPath): the RAM disk at
// ramdisk.UnitIOTier when the run has its unit prefix, else the disk at the
// tier the run inherited.
func RunConditions(unitPrefix []string) string {
	if unitPrefix == nil {
		return "disk-io-inherited"
	}
	return "ramdisk-io-" + ramdisk.UnitIOTier
}

// asked names what a run was asked to run, for its abort: the selection
// file's targets once read, else the suites its arguments select.
func asked(a Args, req *testrunv1.TestRunRequest, selected []roster.Suite) string {
	switch {
	case req != nil:
		return selection.Describe(req)
	case a.Selection != "":
		return "the selection in " + a.Selection
	case len(selected) > 0:
		names := make([]string, len(selected))
		for i, s := range selected {
			names[i] = s.Name
		}
		return "suites " + strings.Join(names, ", ")
	}
	return "the suites its arguments select"
}

// Run executes `testrun run` and returns the process exit status.
func Run(ctx context.Context, d Deps, a Args) int {
	log := d.Log
	ev := d.Events
	if ev == nil {
		ev = events.New(io.Discard, log)
	}
	var req *testrunv1.TestRunRequest
	var selected []roster.Suite
	// abort ends a run that stops before it plans: the failure is logged and
	// stated as the run's one event (testrun.v1.TestRunAborted), so no run
	// leaves its caller without its end.
	abort := func(format string, args ...any) int {
		detail := fmt.Sprintf(format, args...)
		log.Errorf("%s", detail)
		ev.Aborted(asked(a, req, selected), detail)
		return 1
	}
	// EVERY SUITE DECLARES ITS INPUTS, or why it never can: a roster that
	// does not is refused before anything runs (roster.Check).
	if err := roster.Check(roster.Suites); err != nil {
		return abort("the roster is refused: %v", err)
	}
	moduleRoot, err := filepath.Abs(a.Module)
	if err != nil {
		return abort("resolve the module root %s: %v", a.Module, err)
	}
	// A SELECTION FILE IS READ FIRST: a request of whole suites lowers onto
	// the ordinary suite run (its coverage and record included) before
	// anything is planned from the arguments.
	if a.Selection != "" {
		req = &testrunv1.TestRunRequest{}
		err := protofile.Read(a.Selection, req)
		if err != nil {
			err = fmt.Errorf("read the selection: %w", err)
		}
		if err == nil {
			a, err = a.WithRequest(req)
		}
		if err != nil {
			log.Errorf("the selection cannot be run: %v", err)
			ev.Refused(err.Error())
			return 1
		}
	}
	layout := suites.Layout{
		Repo:     repoRoot(moduleRoot),
		Module:   moduleRoot,
		Work:     d.Work,
		Self:     d.Self,
		Coverage: a.Coverage,
	}
	csvPath := filepath.Join(moduleRoot, "test_time.csv")
	if err := ValidateCSV(csvPath); err != nil {
		return abort("%v", err)
	}
	var startBranch, startCommit string
	if a.Record {
		startBranch, startCommit, err = d.Git(layout.Repo)
		if err != nil {
			return abort("could not resolve the initial git branch and commit: %v", err)
		}
	}
	hist, err := history.Load(d.HistoryPath)
	if err != nil {
		return abort("%v", err)
	}

	var resolved selection.Resolved
	if a.Selection != "" {
		resolved, err = selection.Resolve(req, layout)
		if err != nil {
			log.Errorf("the selection cannot be run: %v", err)
			ev.Refused(err.Error())
			return 1
		}
		selected = resolved.Suites
		for _, s := range selected {
			log.Infof("%s: selected by --selection", s.Name)
		}
	} else {
		for _, s := range roster.Suites {
			if a.Selects(s.Name) {
				selected = append(selected, s)
			} else {
				log.Infof("%s: %s, skipping", s.Name, a.skipReason(s))
			}
		}
	}
	mode := a.ReuseMode()
	sandboxErr := d.Reuse.Probe()
	if sandboxErr != nil {
		log.Infof("reuse is off for this run, every unit runs and none is recorded: %v", sandboxErr)
	}

	// Building units can mean `go list`, `vitest list` and npm dependency
	// checks, and deciding them hashes their inputs, so every suite builds
	// and decides at once.
	built := make([]suites.Units, len(selected))
	buildErrs := make([]error, len(selected))
	narrowErrs := make([]error, len(selected))
	decided := make([]reuse.SuiteVerdict, len(selected))
	decideErrs := make([]error, len(selected))
	var wg sync.WaitGroup
	for i, s := range selected {
		wg.Add(1)
		go func() {
			defer wg.Done()
			built[i], buildErrs[i] = d.Build(layout, s)
			// NARROWED BEFORE IT IS CONFINED: confinement wraps each chunk's
			// command, and a chunk narrowed after it would escape the wrap.
			if buildErrs[i] == nil && a.Selection != "" {
				built[i], narrowErrs[i] = resolved.Narrow(s, built[i])
			}
			if buildErrs[i] != nil || narrowErrs[i] != nil {
				return
			}
			// Reuse after the narrowing (a targeted run reuses nothing) and
			// before confinement, which wraps each unit in its own profile.
			decided[i], decideErrs[i] = d.Reuse.Decide(s, built[i], mode, sandboxErr)
			if decideErrs[i] != nil || decided[i].AllReused() {
				return
			}
			built[i] = d.Reuse.Confine(decided[i], reuse.SkipReused(decided[i], built[i]))
		}()
	}
	wg.Wait()
	if err := errors.Join(narrowErrs...); err != nil {
		log.Errorf("the selection cannot be run: %v", err)
		ev.Refused(err.Error())
		return 1
	}
	if err := errors.Join(decideErrs...); err != nil {
		return abort("deciding which units an earlier run's pass covers failed: %v", err)
	}
	// A suite every unit of which is reused does not run at all; one that
	// could not be built has no verdict, and fails below.
	var verdicts []reuse.SuiteVerdict
	var running []roster.Suite
	var runningBuilt []suites.Units
	var runningErrs []error
	for i, s := range selected {
		if buildErrs[i] == nil {
			reuse.Report(log, decided[i])
			verdicts = append(verdicts, decided[i])
			if decided[i].AllReused() {
				continue
			}
		}
		running = append(running, s)
		runningBuilt = append(runningBuilt, built[i])
		runningErrs = append(runningErrs, buildErrs[i])
	}
	selected, built, buildErrs = running, runningBuilt, runningErrs
	unplaced, err := d.Reuse.Unplaced(verdicts)
	if err != nil {
		return abort("the placement check could not run: %v", err)
	}
	placed := reuse.ReportPlacement(log, unplaced)
	var planned []suites.Units
	var unbuilt []run.SuiteResult
	for i, s := range selected {
		if buildErrs[i] != nil {
			now := d.Clock.Now()
			log.Infof("%s: starting", s.Name)
			log.Errorf("%s could not be planned: %v", s.Name, buildErrs[i])
			log.Errorf("%s failed after 0.000s with exit code 1", s.Name)
			unbuilt = append(unbuilt, run.SuiteResult{Name: s.Name, Outcome: run.Failed, Exit: 1, Start: now, End: now})
			continue
		}
		planned = append(planned, built[i])
	}
	programs, err := programUnits(layout, selected, built, buildErrs)
	if err != nil {
		return abort("planning the run's shared programs failed: %v", err)
	}
	if len(programs.Atomic) > 0 {
		planned = append(planned, programs)
	}
	if a.Coverage {
		baseline, ok, err := baselineUnit(layout, d, planned)
		if err != nil {
			return abort("planning the coverage baseline failed: %v", err)
		}
		if ok {
			planned = append(planned, baseline)
		} else {
			log.Infof("coverage: no run log to keep a baseline beside (testrun started directly); the all-tier baseline is not merged")
		}
	}

	p, err := plan.Build(hist, planned, d.Slots, a.Remeasure)
	if err != nil {
		return abort("planning the run failed: %v", err)
	}
	var chunkNote []string
	for group, n := range p.Chunks {
		chunkNote = append(chunkNote, fmt.Sprintf("%s=%d", group, n))
	}
	sort.Strings(chunkNote)
	log.Infof("plan: %d units on %d core slots, predicted %.1fs; chunks: %s",
		len(p.Specs), d.Slots, p.Makespan, strings.Join(chunkNote, " "))

	ev.Planned(p.Specs, d.RunLog)
	if a.PlanOnly {
		// A PLAN-ONLY RUN ENDS AT ITS PLAN (testrun.v1.TestRunRequest.plan_only):
		// no unit runs, so nothing is reused, recorded or timed, and a suite
		// that could not be planned fails it as it fails a real run.
		log.Infof("plan only: %d units planned, none run", len(p.Specs))
		if len(unbuilt) > 0 {
			ev.Ended(nil, len(unbuilt), nil)
			return 1
		}
		ev.PlannedOnly(len(p.Specs))
		return 0
	}
	runner := &run.Runner{Slots: d.Slots, Cores: d.Cores, Exec: d.Exec, Clock: d.Clock, At: d.At, GoCache: d.GoCache, Log: log, Ended: ev.UnitEnded}
	start := d.Clock.Now()
	results, suiteResults, runErr := runner.Run(ctx, p.Specs)
	end := d.Clock.Now()
	if runErr != nil {
		log.Errorf("the run was interrupted: %v", runErr)
		// The units it never ran are failures of the run, so it ended failed.
		ev.Ended(results, len(unbuilt)+1, nil)
		return 130
	}
	// THE RUN'S END IS STATED ONLY ONCE IT IS DECIDED. Every check after the
	// units (the history, the metrics store, the reuse record, the placement
	// check, the staged timing record) can still fail the run, and a run that
	// exits 1 must never have said it passed: on 2026-11-14 every merge gate
	// failed its placement check while `agent-repld test run -every-suite`
	// reported ended passed.
	code, runFailure := settleRun(ctx, d, a, settleInputs{
		results: results, suiteResults: inRosterOrder(append(unbuilt, suiteResults...)),
		verdicts: verdicts, mode: mode, hist: hist,
		times:  runTimes{start: start, end: end, predicted: p.Makespan, verdicts: verdicts},
		placed: placed, unplaced: unplaced,
		repo: layout.Repo, startBranch: startBranch, startCommit: startCommit,
	})
	var runFailures []string
	if runFailure != "" {
		runFailures = []string{runFailure}
	}
	ev.Ended(results, len(unbuilt), runFailures)
	return code
}

// settleInputs are what a run's checks after its units read.
type settleInputs struct {
	results      []run.Result
	suiteResults []run.SuiteResult
	verdicts     []reuse.SuiteVerdict
	mode         reuse.Mode
	hist         *history.Store
	times        runTimes
	placed       bool
	unplaced     []string
	repo         string
	startBranch  string
	startCommit  string
}

// settleRun records a run whose units have ended, judges it, and answers its
// exit status with the run's own failure, if any, for a person: a failure
// apart from any unit's, which the ended event carries (run_failures). A
// failed unit is no run failure: the ended event counts it.
func settleRun(ctx context.Context, d Deps, a Args, in settleInputs) (int, string) {
	log := d.Log
	fail := func(format string, args ...any) (int, string) {
		detail := fmt.Sprintf(format, args...)
		log.Errorf("%s", detail)
		return 1, detail
	}
	if err := in.hist.Record(plan.Measurements(in.results)); err != nil {
		return fail("recording this run's timings in the host history failed: %v", err)
	}
	if err := pushTimes(ctx, d, in.results, in.suiteResults, in.times); err != nil {
		return fail("recording this run's test times in the metrics store failed: %v", err)
	}
	if err := d.Reuse.Record(in.mode, in.verdicts, in.results); err != nil {
		return fail("recording this run's passes failed: %v", err)
	}
	reuse.Summarize(log, in.verdicts)

	printUnitSummary(log, in.results, d.Slots, in.times.start, in.times.end, in.times.predicted)
	passed, failed := partition(in.suiteResults)
	printSuiteSummaries(log, passed, failed)
	if len(failed) > 0 {
		return 1, ""
	}
	if !in.placed && !a.Targeted {
		// A targeted run ran only what it named: the whole checkout's
		// placement is the full runs' to enforce.
		return fail("placement: %d file(s) are read by no suite and listed nowhere (%s); place each in testrun/roster",
			len(in.unplaced), strings.Join(in.unplaced, ", "))
	}

	if a.Record {
		// `run` only STAGES --record: it never writes test_time.csv itself.
		// bin/test-all.sh runs the whole suite schedule inside
		// .claude/safe-test-run.sh's git-state net, and a run that appended
		// its own timing row to a tracked file from inside that net left the
		// append looking exactly like drift the run caused — DRIFT DETECTED,
		// a preserved checkpoint tag, and a --record run that could never
		// exit 0. The actual CSV write is `testrun finish-record`'s alone,
		// and test-all.sh runs it only after the net has already taken its
		// clean post-run snapshot, so the write never lands inside the
		// window the net inspects.
		if err := stageRecord(d, in.repo, a.RecordOut, in.startBranch, in.startCommit, passed); err != nil {
			return fail("staging this run's timing record failed: %v", err)
		}
	} else {
		log.Infof("timings were not recorded, pass --record only for a canonical history run")
	}
	printClosing(log, a)
	return 0, ""
}

// repoRoot is the checkout root a module root sits three directories under
// (modules/app/agent-repl).
func repoRoot(moduleRoot string) string {
	return filepath.Clean(filepath.Join(moduleRoot, "..", "..", ".."))
}

// inRosterOrder sorts verdicts the way the roster lists their suites, so
// what a run records and reports never depends on which suite finished first.
func inRosterOrder(rs []run.SuiteResult) []run.SuiteResult {
	index := map[string]int{}
	for i, s := range roster.Suites {
		index[s.Name] = i
	}
	out := append([]run.SuiteResult(nil), rs...)
	sort.SliceStable(out, func(i, j int) bool { return index[out[i].Name] < index[out[j].Name] })
	return out
}

// suiteOutcomes is the outcome a suite's time is labeled with.
var suiteOutcomes = map[run.Outcome]string{
	run.Passed:    testtimes.Pass,
	run.Failed:    testtimes.Fail,
	run.Cancelled: testtimes.Fail,
}

// unitOutcomes is the outcome a unit's resource use is labeled with.
var unitOutcomes = suiteOutcomes

// pushTimes sends every test's and every suite's time, and every unit's
// resource use, to the metrics store when the run was told to
// (Deps.Metrics), stamped with the run's end. A suite's time is its slot time
// (run.SuiteResult.UnitSeconds). A cancelled unit never ran, so it used
// nothing and is left out.
// runTimes is what pushTimes reports of the whole run: when its units ran,
// what its plan predicted, and how many of each suite's units it reused.
type runTimes struct {
	start, end time.Time
	predicted  float64
	verdicts   []reuse.SuiteVerdict
}

func pushTimes(ctx context.Context, d Deps, results []run.Result, suiteResults []run.SuiteResult, rt runTimes) error {
	if !d.Metrics.Enabled() {
		return nil
	}
	end := rt.end
	report := testtimes.Report{Batching: suites.BatchingLabel()}
	outcome := testtimes.Pass
	for _, s := range suiteResults {
		if s.Outcome != run.Passed {
			outcome = testtimes.Fail
		}
	}
	report.Run = &testtimes.Run{Seconds: rt.end.Sub(rt.start).Seconds(), PredictedSeconds: rt.predicted, Outcome: outcome}
	for _, v := range rt.verdicts {
		report.SuiteUnits = append(report.SuiteUnits, testtimes.SuiteUnits{Suite: v.Suite.Name, Run: len(v.Units) - v.Reused(), Reused: v.Reused()})
	}
	for _, r := range results {
		report.Tests = append(report.Tests, r.Tests...)
		if r.Outcome == run.Cancelled {
			continue
		}
		report.Units = append(report.Units, testtimes.Unit{
			Suite: r.Spec.Suite, ID: r.Spec.ID, Outcome: unitOutcomes[r.Outcome],
			Wall: r.Wall(), CPU: r.Usage.CPU, MaxRSSBytes: r.Usage.MaxRSSBytes,
			BlockReads: r.Usage.BlockReads, BlockWrites: r.Usage.BlockWrites,
		})
	}
	for _, s := range suiteResults {
		report.Suites = append(report.Suites, testtimes.Suite{Name: s.Name, Outcome: suiteOutcomes[s.Outcome], Seconds: s.UnitSeconds})
	}
	if err := d.PushTimes(ctx, d.Metrics, report, end); err != nil {
		return err
	}
	d.Log.Infof("metrics: pushed %d test, %d suite and %d unit records, the run's %.1fs and %d suites' unit counts (trigger %s, batching %s)",
		len(report.Tests), len(report.Suites), len(report.Units), report.Run.Seconds, len(report.SuiteUnits), d.Metrics.Trigger, report.Batching)
	return nil
}

func partition(rs []run.SuiteResult) (passed, failed []run.SuiteResult) {
	for _, r := range rs {
		switch r.Outcome {
		case run.Passed:
			passed = append(passed, r)
		default:
			failed = append(failed, r)
		}
	}
	return
}

// printUnitSummary is the distribution report: how well the run used its
// slots, and the units that set its length.
func printUnitSummary(log *run.Log, results []run.Result, slots int, start, end time.Time, predicted float64) {
	wall := end.Sub(start).Seconds()
	busy, cpu := 0.0, 0.0
	for _, r := range results {
		busy += r.Wall()
		cpu += r.Usage.CPU
	}
	util := 0.0
	if wall > 0 {
		util = busy / (float64(slots) * wall) * 100
	}
	log.Infof("distribution: %.1fs wall (predicted %.1fs), %d units, %.1fs unit-seconds on %d slots = %.0f%% slot use, %.1fs cpu",
		wall, predicted, len(results), busy, slots, util, cpu)
	log.Infof("slowest units:")
	for i, r := range run.SortedBy(results, run.Result.Wall) {
		if i == 10 {
			break
		}
		log.Infof("  %8.3fs  %s [%s] (estimated %.1fs, cpu %.1fs)", r.Wall(), r.Spec.ID, r.Spec.Suite, r.Spec.Est, r.Usage.CPU)
	}
	for _, r := range results {
		if line, over := overCores(r); over {
			log.Infof("%s", line)
		}
	}
}

// overCores reports a unit whose reaped processes used more cores, on
// average, than the slots it held: half a core past its width, over a run
// long enough (above one second) for the average to mean something. Only
// CPU the unit's own process tree reaped is visible here; work done in a VM
// or by an unreaped descendant is not, which is why such a unit must be
// capped to its width by its own command line.
func overCores(r run.Result) (string, bool) {
	w := r.Wall()
	width := r.Spec.Width()
	if w <= 1 || r.Usage.CPU <= (float64(width)+0.5)*w {
		return "", false
	}
	return fmt.Sprintf("unit %s used %.1f cores on average (%.1fs cpu in %.1fs) but holds %d core slot(s): it is not pinned to its width",
		r.Spec.ID, r.Usage.CPU/w, r.Usage.CPU, w, width), true
}

func printSuiteSummaries(log *run.Log, passed, failed []run.SuiteResult) {
	if len(passed) > 0 {
		log.Infof("timing summary, most unit time first (%s is what --record keeps; the span depends on the interleaving)", RecordedMeasure)
		sorted := append([]run.SuiteResult(nil), passed...)
		sort.SliceStable(sorted, func(i, j int) bool { return sorted[i].UnitSeconds > sorted[j].UnitSeconds })
		for _, s := range sorted {
			log.Infof("timing: %s %.3fs of units over a %.3fs span", s.Name, s.UnitSeconds, s.Seconds())
		}
	}
	if len(failed) > 0 {
		log.Errorf("failure summary, %d of %d suites failed", len(failed), len(failed)+len(passed))
		for _, s := range failed {
			log.Errorf("failed: %s exit code %d after %.3fs", s.Name, s.Exit, s.Seconds())
		}
	}
}

// stageRecord re-verifies the branch and commit held steady across the
// suite run, then stages this run's timing rows at recordOut for a later
// `testrun finish-record` to commit. It never touches test_time.csv: see
// Run's "testrun run only STAGES --record" above.
func stageRecord(d Deps, repo, recordOut, startBranch, startCommit string, passed []run.SuiteResult) error {
	log := d.Log
	endBranch, endCommit, err := d.Git(repo)
	if err != nil {
		return fmt.Errorf("could not resolve the final git branch and commit: %w", err)
	}
	if endBranch != startBranch {
		return fmt.Errorf("git branch changed during tests: %s -> %s", startBranch, endBranch)
	}
	if endCommit != startCommit {
		return fmt.Errorf("git commit changed during tests: %s -> %s", startCommit, endCommit)
	}
	now := d.Clock.Now()
	runID := RunID(now, startCommit, d.Pid)
	var rows []TimingRow
	for _, s := range passed {
		rows = append(rows, TimingRow{
			RunID: runID, RecordedAt: now.UTC().Format("2006-01-02T15:04:05Z"),
			Commit: startCommit, Branch: startBranch, Suite: s.Name,
			Seconds: s.UnitSeconds, Measure: RecordedMeasure,
		})
	}
	if err := WritePendingRecord(recordOut, PendingRecord{RunID: runID, Branch: startBranch, Commit: startCommit, Rows: rows}); err != nil {
		return err
	}
	log.Infof("staged %d suite timings at %s for `testrun finish-record` to commit", len(rows), recordOut)
	return nil
}

// FinishRecord is the second half of a --record run: it commits a prior
// `testrun run --record`'s staged rows to test_time.csv and prints the
// regression report, from OUTSIDE the git-state net the suite run itself
// ran inside. It re-checks the branch and commit against what was staged,
// so a commit that moved in the (very short) window between the net's
// clean exit and this call still records nothing rather than mis-attributing
// a row.
func FinishRecord(d Deps, moduleRoot, recordOut string) int {
	log := d.Log
	p, err := ReadPendingRecord(recordOut)
	if err != nil {
		log.Errorf("%v", err)
		return 1
	}
	branch, commit, err := d.Git(repoRoot(moduleRoot))
	if err != nil {
		log.Errorf("could not resolve the current git branch and commit: %v", err)
		return 1
	}
	if branch != p.Branch || commit != p.Commit {
		log.Errorf("git branch/commit moved since the staged run finished: %s@%s -> %s@%s; recording nothing", p.Branch, p.Commit, branch, commit)
		return 1
	}
	csvPath := filepath.Join(moduleRoot, "test_time.csv")
	if err := ValidateCSV(csvPath); err != nil {
		log.Errorf("%v", err)
		return 1
	}
	if err := AppendTimings(csvPath, p.Rows); err != nil {
		log.Errorf("%v", err)
		return 1
	}
	log.Infof("recorded %d suite timings in %s", len(p.Rows), csvPath)
	lines, err := Regressions(csvPath, p.RunID, p.Branch)
	if err != nil {
		log.Errorf("%v", err)
		return 1
	}
	for _, l := range lines {
		log.Infof("%s", l)
	}
	return 0
}

// printClosing names the suites that passed and, for a selected run, the
// ones it did not select.
func printClosing(log *run.Log, a Args) {
	if len(a.Selected) == 0 {
		log.Infof("all agent-repl test suites passed")
		return
	}
	log.Infof("selected agent-repl suites passed: %s", strings.Join(a.Selected, " "))
	var skipped []string
	for _, s := range roster.Suites {
		if !a.Selects(s.Name) {
			skipped = append(skipped, s.Name)
		}
	}
	if len(skipped) == 0 {
		skipped = []string{"none"}
	}
	log.Infof("not selected, NOT run: %s", strings.Join(skipped, " "))
}

// BaselineUnitID is a coverage run's last unit, which merges every counter
// the run wrote into each coverage system's all-tier statement coverage.
const BaselineUnitID = "coverage:baseline"

// BaselineFile is where a coverage run writes its merged baseline: beside
// its own log, so it outlives the run's scratch directory.
func BaselineFile(runLog string) string {
	return filepath.Join(filepath.Dir(runLog), "coverage-baseline.json")
}

// baselineUnit is the coverage run's baseline merge, after every chunk and
// every coverage report of the run. false when the run has no log to keep it
// beside.
func baselineUnit(layout suites.Layout, d Deps, planned []suites.Units) (suites.Units, bool, error) {
	if d.RunLog == "" {
		return suites.Units{}, false, nil
	}
	_, commit, err := d.Git(layout.Repo)
	if err != nil {
		return suites.Units{}, false, fmt.Errorf("resolve the commit the coverage run measures: %w", err)
	}
	var deps []string
	for _, u := range planned {
		for _, sp := range u.Splits {
			deps = append(deps, sp.Group)
		}
		for _, at := range u.Atomic {
			if strings.HasSuffix(at.ID, ":coverage") {
				deps = append(deps, at.ID)
			}
		}
	}
	sp := run.Spec{
		Argv: []string{d.Self, "coverage-baseline", "-module", layout.Module, "-work", layout.Work, "-commit", commit, "-out", BaselineFile(d.RunLog)},
		Dir:  layout.Module,
		Env:  suites.PinnedEnv(),
	}
	sp.ID, sp.Suite, sp.Deps = BaselineUnitID, "coverage", deps
	return suites.Units{Atomic: []run.Spec{sp}}, true, nil
}

// programUnits is the run's one build of every shared program a planned suite
// runs (roster.Programs). A program is reported under its own system's suite
// when that suite runs, and otherwise under the first running suite, in
// roster order, that runs it: a suite the run reused or never selected must
// not gain a verdict from a build it did not ask for.
func programUnits(layout suites.Layout, selected []roster.Suite, built []suites.Units, buildErrs []error) (suites.Units, error) {
	running := map[string]bool{}
	for _, s := range selected {
		running[s.Name] = true
	}
	var needed []string
	firstConsumer := map[string]string{}
	for i, s := range selected {
		if buildErrs[i] != nil {
			continue
		}
		for _, name := range built[i].Programs {
			needed = append(needed, name)
			if _, ok := firstConsumer[name]; !ok {
				firstConsumer[name] = s.Name
			}
		}
	}
	specs, err := suites.ProgramUnits(layout, needed, func(p roster.Program) string {
		if running[p.Owner] {
			return p.Owner
		}
		return firstConsumer[p.Name]
	})
	if err != nil {
		return suites.Units{}, err
	}
	return suites.Units{Atomic: specs}, nil
}
