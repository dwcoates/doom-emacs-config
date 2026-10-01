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
}

// SlotsForHost is the core budget: every core but two, which stay free for
// the owner's live runtime and the machine itself. Never fewer than one.
func SlotsForHost(numCPU int) int {
	return max(1, numCPU-2)
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
		Repo:   filepath.Clean(filepath.Join(moduleRoot, "..", "..", "..")),
		Module: moduleRoot,
		Work:   d.Work,
		Self:   d.Self,
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

	printUnitSummary(log, results, d.Slots, start, end, p.Makespan)
	passed, declined, failed := partition(suiteResults)
	printSuiteSummaries(log, passed, declined, failed)
	if len(failed) > 0 {
		return 1
	}

	if a.Record {
		if code := record(d, layout.Repo, csvPath, startBranch, startCommit, passed); code != 0 {
			return code
		}
	} else {
		log.Infof("timings were not recorded, pass --record only for a canonical history run")
	}
	printClosing(log, a, passed, declined)
	return 0
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
		if w := r.Wall(); w > 1 && r.CPU > 1.5*w {
			log.Infof("unit %s used %.1f cores on average (%.1fs cpu in %.1fs): it is not pinned to one core", r.Spec.ID, r.CPU/w, r.CPU, w)
		}
	}
}

func printSuiteSummaries(log *run.Log, passed, declined, failed []run.SuiteResult) {
	if len(passed) > 0 {
		log.Infof("timing summary, slowest suite first")
		sorted := append([]run.SuiteResult(nil), passed...)
		sort.SliceStable(sorted, func(i, j int) bool { return sorted[i].Seconds() > sorted[j].Seconds() })
		for _, s := range sorted {
			log.Infof("timing: %s %.3fs", s.Name, s.Seconds())
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

func record(d Deps, repo, csvPath, startBranch, startCommit string, passed []run.SuiteResult) int {
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
			Commit: startCommit, Branch: startBranch, Suite: s.Name, Seconds: s.Seconds(),
		})
	}
	if err := AppendTimings(csvPath, rows); err != nil {
		log.Errorf("%v", err)
		return 1
	}
	log.Infof("recorded %d suite timings in %s", len(rows), csvPath)
	lines, err := Regressions(csvPath, runID, startBranch)
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
			log.Infof("all agent-repl tests and coverage suites passed")
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
