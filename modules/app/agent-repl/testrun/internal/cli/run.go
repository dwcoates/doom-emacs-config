// Package cli is `testrun run`: plan every selected suite onto the host's core
// slots, run it, and report it in bin/test-all.sh's established shape.
package cli

import (
	"context"
	"fmt"
	"path/filepath"
	"sort"
	"strings"
	"sync"
	"time"

	"agentrepl/testrun/internal/history"
	"agentrepl/testrun/internal/plan"
	"agentrepl/testrun/internal/ramdisk"
	"agentrepl/testrun/internal/reuse"
	"agentrepl/testrun/internal/run"
	"agentrepl/testrun/internal/suites"
	"agentrepl/testrun/roster"
)

// Deps are the run's seams to the outside.
type Deps struct {
	Log   *run.Log
	Exec  run.Executor
	Clock run.Clock
	// Slots is how many units run at once.
	Slots int
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
	// Reuse decides which selected suites an earlier run's pass covers,
	// confines the keyed ones, and records their passes
	// (agentrepl/testrun/internal/reuse).
	Reuse Reuser
	// RunLog is the run's own log file; a coverage run writes its merged
	// baseline beside it. "" when testrun was started directly.
	RunLog string
	// PrepareWebappDist builds the checkout's webapp dist when it is stale. A
	// run calls it ONCE, before any unit starts, when a suite it runs needs
	// the dist (roster.Suite.NeedsWebappDist): the run is then the dist's one
	// writer, and no unit sees it change under it.
	PrepareWebappDist func(moduleRoot string) error
}

// Reuser is the run's pass-record seam (reuse.Runner).
type Reuser interface {
	Decide(selected []roster.Suite, mode reuse.Mode) ([]reuse.Verdict, error)
	Unplaced(verdicts []reuse.Verdict) ([]string, error)
	Confine(verdicts []reuse.Verdict, name string, u suites.Units) suites.Units
	Record(mode reuse.Mode, verdicts []reuse.Verdict, results []run.SuiteResult, units map[string]int) error
}

// HostReserve is how many of the host's cores a run leaves unclaimed for the
// owner's live runtime, input handling and apps (owner ruling, 2026-10-14).
// The run itself is at nice 19 (bin/background.sh), so it always yields the
// CPU to them as well.
const HostReserve = 3

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

// SlotsForHost is the core budget: every core but HostReserve. A host with no
// core left over is answered with a non-positive count, which Slots refuses.
func SlotsForHost(numCPU int) int {
	return numCPU - HostReserve
}

// Slots is the run's core count, SlotsForHost. NO PROCESSOR IS HANDED OUT
// BETWEEN RUNS (owner ruling, 2026-10-14): a run sizes itself to the host,
// overlapping runs contend, nice orders them (the merge queue's head first),
// and the one thing rationed between them is the memory their RAM disks wire
// (ramdisk seats). A host too small to leave HostReserve cores free is
// refused, never adjusted.
func Slots(numCPU int) (int, error) {
	n := SlotsForHost(numCPU)
	if n < 1 {
		return 0, fmt.Errorf("this host has %d cores; a run reserves %d for the live runtime and needs at least one more", numCPU, HostReserve)
	}
	return n, nil
}

// Run executes `testrun run` and returns the process exit status.
func Run(ctx context.Context, d Deps, a Args) int {
	log := d.Log
	moduleRoot, err := filepath.Abs(a.Module)
	if err != nil {
		log.Errorf("resolve the module root %s: %v", a.Module, err)
		return 1
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
		log.Errorf("%v", err)
		return 1
	}
	var startBranch, startCommit string
	if a.Record {
		startBranch, startCommit, err = d.Git(layout.Repo)
		if err != nil {
			log.Errorf("could not resolve the initial git branch and commit: %v", err)
			return 1
		}
	}
	hist, err := history.Load(d.HistoryPath)
	if err != nil {
		log.Errorf("%v", err)
		return 1
	}

	var selected []roster.Suite
	for _, s := range roster.Suites {
		if a.Selects(s.Name) {
			selected = append(selected, s)
		} else {
			log.Infof("%s: not selected by --suites, skipping", s.Name)
		}
	}
	mode := a.ReuseMode()
	verdicts, err := d.Reuse.Decide(selected, mode)
	if err != nil {
		log.Errorf("deciding which suites an earlier run's pass covers failed: %v", err)
		return 1
	}
	reuse.Report(log, verdicts)
	unplaced, err := d.Reuse.Unplaced(verdicts)
	if err != nil {
		log.Errorf("the placement check could not run: %v", err)
		return 1
	}
	placed := reuse.ReportPlacement(log, unplaced)
	selected = reuse.Remaining(verdicts)

	// Building units can mean `go list`, `vitest list` and npm dependency
	// checks, so every suite builds at once.
	built := make([]suites.Units, len(selected))
	buildErrs := make([]error, len(selected))
	var wg sync.WaitGroup
	for i, s := range selected {
		wg.Add(1)
		go func() {
			defer wg.Done()
			built[i], buildErrs[i] = d.Build(layout, s)
			if buildErrs[i] == nil {
				built[i] = d.Reuse.Confine(verdicts, s.Name, built[i])
			}
		}()
	}
	wg.Wait()
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
		log.Errorf("planning the run's shared programs failed: %v", err)
		return 1
	}
	if len(programs.Atomic) > 0 {
		planned = append(planned, programs)
	}
	if a.Coverage {
		baseline, ok, err := baselineUnit(layout, d, planned)
		if err != nil {
			log.Errorf("planning the coverage baseline failed: %v", err)
			return 1
		}
		if ok {
			planned = append(planned, baseline)
		} else {
			log.Infof("coverage: no run log to keep a baseline beside (testrun started directly); the all-tier baseline is not merged")
		}
	}

	p, err := plan.Build(hist, planned, d.Slots)
	if err != nil {
		log.Errorf("planning the run failed: %v", err)
		return 1
	}
	var chunkNote []string
	for group, n := range p.Chunks {
		chunkNote = append(chunkNote, fmt.Sprintf("%s=%d", group, n))
	}
	sort.Strings(chunkNote)
	log.Infof("plan: %d units on %d core slots, predicted %.1fs; chunks: %s",
		len(p.Specs), d.Slots, p.Makespan, strings.Join(chunkNote, " "))

	if needsWebappDist(selected) {
		log.Infof("webapp dist: building it once, before any unit starts, if it is stale")
		if err := d.PrepareWebappDist(moduleRoot); err != nil {
			log.Errorf("webapp dist: building it before the run failed, so no suite that reads it can run: %v", err)
			return 1
		}
	}

	runner := &run.Runner{Slots: d.Slots, Exec: d.Exec, Clock: d.Clock, Log: log}
	start := d.Clock.Now()
	results, suiteResults, runErr := runner.Run(ctx, p.Specs)
	end := d.Clock.Now()
	if runErr != nil {
		log.Errorf("the run was interrupted: %v", runErr)
		return 130
	}
	suiteResults = inRosterOrder(append(unbuilt, suiteResults...))

	if err := hist.Record(plan.Measurements(results)); err != nil {
		log.Errorf("recording this run's timings in the host history failed: %v", err)
		return 1
	}
	unitsPerSuite := map[string]int{}
	for _, r := range results {
		unitsPerSuite[r.Spec.Suite]++
	}
	if err := d.Reuse.Record(mode, verdicts, suiteResults, unitsPerSuite); err != nil {
		log.Errorf("recording this run's passes failed: %v", err)
		return 1
	}

	printUnitSummary(log, results, d.Slots, start, end, p.Makespan)
	passed, declined, failed := partition(suiteResults)
	printSuiteSummaries(log, passed, declined, failed)
	if len(failed) > 0 {
		return 1
	}
	if !placed {
		log.Errorf("placement: %d file(s) are read by no suite and listed nowhere; place each in testrun/roster", len(unplaced))
		return 1
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
		if code := stageRecord(d, layout.Repo, a.RecordOut, startBranch, startCommit, passed); code != 0 {
			return code
		}
	} else {
		log.Infof("timings were not recorded, pass --record only for a canonical history run")
	}
	printClosing(log, a, passed, declined)
	if names := reuse.ReusedNames(verdicts); len(names) > 0 {
		log.Infof("reused an earlier green run of their exact inputs, NOT run: %s (--no-reuse runs them)", strings.Join(names, " "))
	}
	return 0
}

// needsWebappDist reports whether a suite the run will run reads the
// checkout's webapp dist.
func needsWebappDist(selected []roster.Suite) bool {
	for _, s := range selected {
		if s.NeedsWebappDist {
			return true
		}
	}
	return false
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

func partition(rs []run.SuiteResult) (passed, declined, failed []run.SuiteResult) {
	for _, r := range rs {
		switch r.Outcome {
		case run.Passed:
			passed = append(passed, r)
		case run.Declined:
			declined = append(declined, r)
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
		cpu += r.CPU
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
		log.Infof("  %8.3fs  %s [%s] (estimated %.1fs, cpu %.1fs)", r.Wall(), r.Spec.ID, r.Spec.Suite, r.Spec.Est, r.CPU)
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
	if w <= 1 || r.CPU <= (float64(width)+0.5)*w {
		return "", false
	}
	return fmt.Sprintf("unit %s used %.1f cores on average (%.1fs cpu in %.1fs) but holds %d core slot(s): it is not pinned to its width",
		r.Spec.ID, r.CPU/w, r.CPU, w, width), true
}

func printSuiteSummaries(log *run.Log, passed, declined, failed []run.SuiteResult) {
	if len(passed) > 0 {
		log.Infof("timing summary, most unit time first (%s is what --record keeps; the span depends on the interleaving)", RecordedMeasure)
		sorted := append([]run.SuiteResult(nil), passed...)
		sort.SliceStable(sorted, func(i, j int) bool { return sorted[i].UnitSeconds > sorted[j].UnitSeconds })
		for _, s := range sorted {
			log.Infof("timing: %s %.3fs of units over a %.3fs span", s.Name, s.UnitSeconds, s.Seconds())
		}
	}
	if len(declined) > 0 {
		log.Infof("declined summary, %d suite(s) could not run", len(declined))
		for _, s := range declined {
			log.Infof("declined: %s did not run (precondition unmet) after %.3fs", s.Name, s.Seconds())
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
func stageRecord(d Deps, repo, recordOut, startBranch, startCommit string, passed []run.SuiteResult) int {
	log := d.Log
	endBranch, endCommit, err := d.Git(repo)
	if err != nil {
		log.Errorf("could not resolve the final git branch and commit: %v", err)
		return 1
	}
	if endBranch != startBranch {
		log.Errorf("git branch changed during tests: %s -> %s", startBranch, endBranch)
		return 1
	}
	if endCommit != startCommit {
		log.Errorf("git commit changed during tests: %s -> %s", startCommit, endCommit)
		return 1
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
		log.Errorf("%v", err)
		return 1
	}
	log.Infof("staged %d suite timings at %s for `testrun finish-record` to commit", len(rows), recordOut)
	return 0
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

func names(rs []run.SuiteResult) string {
	var n []string
	for _, r := range rs {
		n = append(n, r.Name)
	}
	if len(n) == 0 {
		return "none"
	}
	return strings.Join(n, " ")
}

// printClosing names what ran and what declined separately: a DECLINED suite
// did not pass, it did not run.
func printClosing(log *run.Log, a Args, passed, declined []run.SuiteResult) {
	if len(a.Selected) == 0 {
		if len(declined) == 0 {
			log.Infof("all agent-repl test suites passed")
		} else {
			log.Infof("every agent-repl suite that could run passed; DECLINED: %s", names(declined))
		}
		return
	}
	if len(declined) == 0 {
		log.Infof("selected agent-repl suites passed: %s", strings.Join(a.Selected, " "))
	} else {
		log.Infof("selected agent-repl suites that ran passed: %s", names(passed))
		log.Infof("selected but DECLINED, did NOT run: %s", names(declined))
	}
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
