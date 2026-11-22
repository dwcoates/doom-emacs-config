// Package run executes a planned set of units on a fixed number of core slots
// and reports them in the shape bin/test-all.sh always has: one "starting"
// line per suite, followed by its "N units planned" line, one verdict line per
// suite, then the summaries.
package run

import (
	"agentrepl/testrun/internal/cores"
	"bytes"
	"context"
	"fmt"
	"maps"
	"slices"
	"sort"
	"strings"
	"time"

	"agentrepl/testrun/internal/sched"
	"agentrepl/testrun/internal/testtimes"
)

// Spec is one unit and the process that runs it.
type Spec struct {
	sched.Unit
	Argv []string
	Dir  string
	// Env is added to the runner's own environment.
	Env []string
	// Bound is the unit's time limit: a unit still running at it is killed
	// and fails. REQUIRED, positive: every unit is bounded (plan.Build sizes
	// it from the unit's expected time), and a run refuses a unit without
	// one.
	Bound time.Duration
	// Expected is the expected time Bound was sized from, which a unit that
	// ran past its bound is reported against.
	Expected time.Duration
	// Cores is how many cores the unit is expected to use while it runs
	// (cores.ExpectedCores: its own measured history, or a default a slot),
	// which the host's CPU governor admits it by. REQUIRED, positive.
	Cores float64
	// Items parses the unit's output into seconds per item, for the history.
	// Nil for a unit that is not a chunk.
	Items func(output []byte) (map[string]float64, error)
	// Tests parses the unit's output into every individual test's time, for
	// the metrics store (testtimes), given whether the unit passed. Nil for a
	// unit that runs no tests of its own.
	Tests func(output []byte, passed bool) ([]testtimes.Test, error)
	// Display, when set, is what of the unit's output the run log shows,
	// given whether the unit passed. Nil shows it whole.
	Display func(output []byte, passed bool) []byte
	// NamesTests marks a chunk a selection narrowed to named tests: one that
	// runs none of them fails, never passes on nothing (2026-11-15: a
	// multi-file ERT selection handed the names to the wrong file and passed
	// on zero tests).
	NamesTests bool
	// Profiler is how a profiling run profiles the unit's own process
	// (internal/profile). NoProfiler profiles only the production processes
	// its tests start, which every unit of a profiling run is told the
	// directory of.
	Profiler Profiler
	// Impact marks a chunk run recorded (testrun/internal/impact): what the
	// runner needs to turn the chunk's results and its harness's windows into
	// records. Nil for every other unit.
	Impact *ImpactChunk
}

// ImpactChunk names one recorded Go chunk: the package pass its tests belong
// to and the file its harness appends its windows to (testenv.ImpactOut).
type ImpactChunk struct {
	Suite, Package, Tags string
	// Windows is the file the chunk's harness appends each window to.
	Windows string
}

// Profiler is how a unit's own process writes a CPU profile.
type Profiler int

const (
	// NoProfiler is a unit whose own process is not profiled.
	NoProfiler Profiler = iota
	// GoTestProfiler is a Go test binary: `-test.cpuprofile`.
	GoTestProfiler
	// NodeProfiler is node and every node process it starts (vitest's
	// workers): `--cpu-prof` through NODE_OPTIONS.
	NodeProfiler
)

// Usage is what a unit's process and its reaped descendants used, as the
// kernel's wait accounting reports it.
type Usage struct {
	// CPU is user plus system seconds.
	CPU float64
	// MaxRSSBytes is the largest resident set of the process or any one
	// descendant it reaped.
	MaxRSSBytes int64
	// BlockReads and BlockWrites are the block input and output operations
	// that went to storage (the kernel's ru_inblock and ru_oublock).
	BlockReads, BlockWrites int64
}

// Process is one started unit.
type Process interface {
	// Wait blocks until the process exits and returns its exit status and
	// what it and its reaped descendants used.
	Wait() (exit int, usage Usage, err error)
	// Kill stops the process and everything it started.
	Kill()
}

// Executor starts a unit's process with its output going to out.
type Executor interface {
	Start(spec Spec, out *bytes.Buffer) (Process, error)
}

// Clock is the runner's time source.
type Clock interface{ Now() time.Time }

// Outcome is how a unit ended.
type Outcome int

const (
	Passed Outcome = iota
	Failed
	Cancelled
)

// Result is one unit's record.
type Result struct {
	Spec       Spec
	Outcome    Outcome
	Exit       int
	Start, End time.Time
	// Usage is what the unit's process used.
	Usage Usage
	// Items are the per-item seconds the unit's output reported.
	Items map[string]float64
	// Tests are the individual tests' times the unit's output reported.
	Tests []testtimes.Test
	// CancelledBy names the failed dependency of a cancelled unit.
	CancelledBy string
	// TimedOut is a unit killed because it ran past its Bound.
	TimedOut bool
}

// Wall is the unit's wall time in seconds.
func (r Result) Wall() float64 { return r.End.Sub(r.Start).Seconds() }

// SuiteResult is one suite's verdict.
type SuiteResult struct {
	Name       string
	Outcome    Outcome
	Exit       int
	Start, End time.Time
	// UnitSeconds is the sum of the suite's own units' wall times: the slot
	// time the suite occupied. Unlike the span, it does not grow while the
	// suite's next unit waits for a slot another suite holds.
	UnitSeconds float64
}

// Seconds is the suite's wall span, first unit start to last unit end. It
// depends on how the scheduler interleaved every suite in the run.
func (s SuiteResult) Seconds() float64 { return s.End.Sub(s.Start).Seconds() }

// Cores hands out the host's core seats, shared by every run on the host
// (cores.Host), to a unit of width expected to use cores. Hold blocks while
// the host's budget is spent or its CPU governor holds the unit back.
type Cores interface {
	Hold(id string, width int, cores float64) (release func() error, err error)
}

// Runner runs units.
type Runner struct {
	Slots int
	// Cores is the host's core budget: a unit starts only once it holds as
	// many of the host's core seats as it is wide, and gives them back when
	// it exits.
	Cores Cores
	Exec  Executor
	Clock Clock
	// At answers a channel that receives once deadline has come: what a
	// unit's time limit waits on (AtDeadline in production).
	At func(deadline time.Time) <-chan time.Time
	// GoCache fixes each unit's deadline once the host's Go build cache
	// cannot fall due for its trim before it (gocache.Guard).
	GoCache GoCache
	Log     *Log
	// Ended, when set, is told each unit's result as its verdict is
	// reported, with the unit's output as the run log shows it (Spec.Display;
	// nil for a unit that never started): a caller streaming the run's
	// progress (events.Emitter).
	Ended func(res Result, output []byte)
	// CPU, when set, answers the CPU each unit's tree used over time, which
	// each of the unit's tests is given its share of
	// (testtimes.AttributeCPU). Nil measures no test's CPU.
	CPU UnitCPU
	// Pid is this run's process, whose units' trees the runner tags
	// (cores.UnitEnvEntry): EVERY UNIT IS TAGGED, measured or not, since the
	// CPU governor tells a just-started unit's use apart by its tag.
	Pid int
}

// UnitCPU answers what each unit's tagged processes used over time
// (cputime.Sampler).
type UnitCPU interface {
	// Take answers unit ID's CPU intervals and forgets them.
	Take(id string) []testtimes.CPUSlice
}

// GoCache keeps cmd/go's trim of the build cache every unit's go commands
// share out of their time limits (gocache.Guard).
type GoCache interface {
	// Deadline answers when a unit bounded by bound is killed, once the
	// cache's trim cannot fall due before then, trimming it first when it
	// would.
	Deadline(unit string, bound time.Duration) (time.Time, error)
}

// AtDeadline is the real deadline timer.
func AtDeadline(deadline time.Time) <-chan time.Time { return time.After(time.Until(deadline)) }

// grant is a unit's answer from the host's core pool and its Go cache.
type grant struct {
	id      string
	release func() error
	// deadline is when the unit is killed (GoCache.Deadline).
	deadline time.Time
	err      error
}

type completion struct {
	id     string
	exit   int
	usage  Usage
	err    error
	output *bytes.Buffer
	// timedOut is a unit killed at its time limit.
	timedOut bool
}

// Run executes every spec and returns per-unit results in completion order
// and per-suite verdicts in the order suites first started. A unit that
// cannot be started at all is a failed unit, reported like any other.
//
// Cancelling ctx kills every running unit and returns ctx's error once they
// have all exited.
func (r *Runner) Run(ctx context.Context, specs []Spec) ([]Result, []SuiteResult, error) {
	if r.Slots < 1 {
		return nil, nil, fmt.Errorf("run: need at least one slot, got %d", r.Slots)
	}
	if r.Cores == nil {
		return nil, nil, fmt.Errorf("run: no host core pool to hold the units' cores")
	}
	if r.At == nil {
		return nil, nil, fmt.Errorf("run: no timer to hold the units to their time limits")
	}
	if r.GoCache == nil {
		return nil, nil, fmt.Errorf("run: no Go cache guard to keep its trim out of the units' time limits")
	}
	for _, s := range specs {
		if s.Bound <= 0 {
			return nil, nil, fmt.Errorf("run: unit %s (%s) has no time limit (bound %v); every unit is bounded", s.ID, s.Suite, s.Bound)
		}
		if s.Cores <= 0 {
			return nil, nil, fmt.Errorf("run: unit %s (%s) has no expected cores (%v); the CPU governor admits every unit by them", s.ID, s.Suite, s.Cores)
		}
		for _, kv := range s.Env {
			if strings.HasPrefix(kv, "GOCACHE=") {
				return nil, nil, fmt.Errorf("run: unit %s (%s) sets its own %s, whose trim the run's Go cache guard does not keep out of its time limit", s.ID, s.Suite, kv)
			}
		}
	}
	units := make([]sched.Unit, len(specs))
	byID := map[string]Spec{}
	suiteUnits := map[string]int{}
	for i, s := range specs {
		units[i] = s.Unit
		byID[s.ID] = s
		suiteUnits[s.Suite]++
	}
	if err := sched.CheckWidths(units, r.Slots); err != nil {
		return nil, nil, fmt.Errorf("run: %w", err)
	}
	q, err := sched.NewQueue(units)
	if err != nil {
		return nil, nil, err
	}

	var (
		results   []Result
		suites    = map[string]*SuiteResult{}
		order     []string
		remaining = maps.Clone(suiteUnits)
		running   = map[string]Process{}
		starts    = map[string]time.Time{}
		releases  = map[string]func() error{}
		done      = make(chan completion)
		// granted is buffered so the one hold in flight can always deliver
		// its answer, even after a cancelled run stopped reading.
		granted = make(chan grant, 1)
	)

	suiteOf := func(name string, at time.Time) *SuiteResult {
		sr, ok := suites[name]
		if !ok {
			sr = &SuiteResult{Name: name, Outcome: Passed, Start: at}
			suites[name] = sr
			order = append(order, name)
			r.Log.Infof("%s: starting", name)
			// How many units the suite runs, so a reader of the run (the
			// merge gate's tests tab) can count its unit verdicts against it.
			r.Log.Infof("%s: %d units planned", name, suiteUnits[name])
		}
		return sr
	}
	finishUnit := func(res Result) {
		results = append(results, res)
		sr := suiteOf(res.Spec.Suite, res.Start)
		if res.End.After(sr.End) {
			sr.End = res.End
		}
		sr.UnitSeconds += res.Wall()
		switch {
		case res.Outcome == Failed && sr.Outcome != Failed:
			sr.Outcome, sr.Exit = Failed, res.Exit
		case res.Outcome == Cancelled && sr.Outcome != Failed:
			sr.Outcome, sr.Exit = Failed, 1
		}
		remaining[res.Spec.Suite]--
		if remaining[res.Spec.Suite] == 0 {
			r.reportSuite(*sr)
		}
	}

	// unstarted ends a unit that never ran, as a failed unit reported like
	// any other.
	unstarted := func(s Spec, err error, out *bytes.Buffer) {
		r.Log.Errorf("unit %s (%s) could not start: %v", s.ID, s.Suite, err)
		go func() { done <- completion{id: s.ID, exit: -1, err: err, output: out} }()
	}
	begin := func(s Spec) {
		now := r.Clock.Now()
		suiteOf(s.Suite, now)
		starts[s.ID] = now
	}
	start := func(s Spec, deadline time.Time) {
		begin(s)
		s.Env = append(append([]string(nil), s.Env...), cores.UnitEnvEntry(r.Pid, s.ID))
		out := &bytes.Buffer{}
		p, err := r.Exec.Start(s, out)
		if err != nil {
			unstarted(s, err, out)
			return
		}
		running[s.ID] = p
		go r.await(s, deadline, p, out, done)
	}

	// ONE HOLD AT A TIME, IN THE QUEUE'S ORDER. The next unit the queue
	// picks waits for its host cores while the run keeps reaping the units
	// already going; nothing behind it is asked for cores until it has them,
	// so the run's own priority order survives the wait.
	inFlight, used, holding := 0, 0, false
	for {
		for ctx.Err() == nil && !holding {
			u, ok := q.Next(r.Slots - used)
			if !ok {
				break
			}
			inFlight++
			used += u.Width()
			holding = true
			bound := byID[u.ID].Bound
			go func() {
				release, err := r.Cores.Hold(u.ID, u.Width(), byID[u.ID].Cores)
				if err != nil {
					granted <- grant{id: u.ID, err: err}
					return
				}
				// THE DEADLINE IS FIXED HERE, after the unit holds its cores
				// and before it starts, so the Go cache's trim cannot fall
				// due while the unit may still be running.
				deadline, err := r.GoCache.Deadline(u.ID, bound)
				if err != nil {
					r.releaseCores(u.ID, release)
					granted <- grant{id: u.ID, err: err}
					return
				}
				granted <- grant{id: u.ID, release: release, deadline: deadline}
			}()
		}
		if inFlight == 0 {
			break
		}
		var c completion
		select {
		case g := <-granted:
			holding = false
			if g.err != nil {
				begin(byID[g.id])
				unstarted(byID[g.id], g.err, &bytes.Buffer{})
				continue
			}
			releases[g.id] = g.release
			start(byID[g.id], g.deadline)
			continue
		case c = <-done:
		case <-ctx.Done():
			for _, p := range running {
				p.Kill()
			}
			// A unit still waiting for host cores never started: it is not
			// waited for, and whatever seats its hold later takes go back
			// when this process exits, the kernel dropping their locks.
			if holding {
				inFlight--
			}
			for inFlight > 0 {
				<-done
				inFlight--
			}
			for id, release := range releases {
				r.releaseCores(id, release)
			}
			return results, r.suiteList(suites, order), ctx.Err()
		}
		inFlight--
		delete(running, c.id)
		if release, ok := releases[c.id]; ok {
			delete(releases, c.id)
			r.releaseCores(c.id, release)
		}
		spec := byID[c.id]
		used -= spec.Width()
		res := Result{Spec: spec, Exit: c.exit, Start: starts[c.id], End: r.Clock.Now(), Usage: c.usage, TimedOut: c.timedOut}
		switch {
		case c.timedOut:
			res.Outcome = Failed
		case c.err != nil:
			r.Log.Errorf("unit %s (%s) could not be waited on: %v", c.id, spec.Suite, c.err)
			res.Outcome, res.Exit = Failed, -1
		case c.exit == 0:
			res.Outcome = Passed
		default:
			res.Outcome = Failed
		}
		if res.Outcome == Passed && spec.Items != nil {
			items, err := spec.Items(c.output.Bytes())
			if err != nil {
				// The unit passed, but what it printed is not what its suite
				// promises: the timing contract is broken, which is a failure.
				r.Log.Errorf("unit %s (%s) passed but its item timings are unreadable: %v", c.id, spec.Suite, err)
				res.Outcome, res.Exit = Failed, 1
			}
			res.Items = items
		}
		if spec.Tests != nil && (res.Outcome == Passed || res.Outcome == Failed) {
			tests, err := spec.Tests(c.output.Bytes(), res.Outcome == Passed)
			switch {
			case err != nil && res.Outcome == Passed:
				// As for the item timings: a passing unit that printed what its
				// suite does not promise broke its timing contract.
				r.Log.Errorf("unit %s (%s) passed but its test timings are unreadable: %v", c.id, spec.Suite, err)
				res.Outcome, res.Exit = Failed, 1
			case err != nil:
				// A unit that failed may have stopped before it reported its
				// tests; its failure is the error, recorded with its verdict.
				r.Log.Infof("unit %s (%s) failed, and its test timings are unreadable: %v", c.id, spec.Suite, err)
			}
			res.Tests = tests
		}
		if r.CPU != nil {
			intervals := r.CPU.Take(c.id)
			if err := testtimes.AttributeCPU(res.Tests, intervals, res.Usage.CPU, res.Start, res.End); err != nil {
				r.Log.Errorf("unit %s (%s) reported tests whose CPU cannot be told apart (%d tests, %d CPU intervals, %.3fs CPU): %v", c.id, spec.Suite, len(res.Tests), len(intervals), res.Usage.CPU, err)
				if res.Outcome == Passed {
					// As for the test timings: a passing unit that broke its
					// timing contract fails.
					res.Outcome, res.Exit = Failed, 1
				}
			}
		}
		// A TEST THE RUNNER SKIPPED NEVER RAN: a vitest name that matched
		// nothing reports every test of its file skipped (2026-11-21: a
		// named run of test/metaprompt.test.ts skipped all 49 and passed).
		if res.Outcome == Passed && spec.NamesTests && !slices.ContainsFunc(res.Tests, func(t testtimes.Test) bool { return t.Outcome != testtimes.Skip }) {
			r.Log.Errorf("unit %s (%s) was narrowed to named tests and ran none of them; a selection that runs nothing fails", c.id, spec.Suite)
			fmt.Fprintf(c.output, "\n%s unit %s ran NONE of the tests it was narrowed to: a selection that runs nothing fails.\n", Prefix, c.id)
			res.Outcome, res.Exit = Failed, 1
		}
		// The block is bracketed by a line naming its unit and suite and by
		// the unit's verdict line, so a reader of the interleaved run (the
		// merge gate's tests tab) attributes every line to its suite.
		r.Log.Infof("unit %s [%s] output:", c.id, spec.Suite)
		shown := c.output.Bytes()
		if spec.Display != nil {
			shown = spec.Display(shown, res.Outcome == Passed)
		}
		r.Log.Block(shown)
		r.reportUnit(res)
		r.tellEnded(res, shown)
		cancelled := q.Done(c.id, res.Outcome == Passed)
		finishUnit(res)
		for _, u := range cancelled {
			now := r.Clock.Now()
			cres := Result{Spec: byID[u.ID], Outcome: Cancelled, Exit: 1, Start: now, End: now, CancelledBy: c.id}
			r.reportUnit(cres)
			r.tellEnded(cres, nil)
			finishUnit(cres)
		}
	}
	if err := ctx.Err(); err != nil {
		// Cancelled while nothing was running: what never started never will.
		return results, r.suiteList(suites, order), err
	}
	if p := q.Pending(); p != 0 {
		panic(fmt.Sprintf("run: %d units never became ready, which NewQueue's validation rules out", p))
	}
	return results, r.suiteList(suites, order), nil
}

// await waits for a started unit to exit and reports it on done. A unit still
// running at its deadline is killed, and reported as timed out.
//
// THE TIME LIMIT IS THE UNIT'S OWN. Its deadline is fixed once the unit holds
// its cores (GoCache.Deadline), so a wait for CPU or seats, or for the Go
// cache's trim, never counts against it.
func (r *Runner) await(s Spec, deadline time.Time, p Process, out *bytes.Buffer, done chan<- completion) {
	waited := make(chan completion, 1)
	go func() {
		exit, usage, err := p.Wait()
		waited <- completion{id: s.ID, exit: exit, usage: usage, err: err, output: out}
	}()
	select {
	case c := <-waited:
		done <- c
	case <-r.At(deadline):
		r.Log.Errorf("unit %s (%s) ran past its time limit of %s (%d times its expected %s) and is killed", s.ID, s.Suite, s.Bound, BoundFactor, s.Expected)
		p.Kill()
		c := <-waited
		c.timedOut = true
		fmt.Fprintf(c.output, "\n%s unit %s was KILLED: it ran past its time limit of %s (%d times its expected %s). A unit that legitimately got slower is re-measured with -remeasure.\n", Prefix, s.ID, s.Bound, BoundFactor, s.Expected)
		done <- c
	}
}

// BoundFactor is how many times its expected time a unit may run before it
// is killed. The expected time is the unit's peak passing time (plan.Build).
// It was 3 (owner ruling, 2026-11-09: every timeout is 2-3 times the expected
// time of what it bounds; over 4,659 recorded runs of 260 units, 3 times a
// decaying peak was passed only twice). The owner raised it to 5 (2026-11-13)
// while tests ran at macOS utility QoS, whose first governed run killed eight
// short healthy units at 3 times peaks recorded at full priority, and kept it
// at 5 once that clamp was dropped the same day.
const BoundFactor = 5

// tellEnded tells the run's observer, if it has one, that a unit ended.
func (r *Runner) tellEnded(res Result, output []byte) {
	if r.Ended != nil {
		r.Ended(res, output)
	}
}

// releaseCores gives unit id's host core seats back. Closing a seat's file
// drops its lock whatever close answers, so a failure is logged and the seat
// is free regardless.
func (r *Runner) releaseCores(id string, release func() error) {
	if err := release(); err != nil {
		r.Log.Errorf("unit %s gave back its host cores, but closing a seat failed: %v", id, err)
	}
}

func (r *Runner) suiteList(suites map[string]*SuiteResult, order []string) []SuiteResult {
	out := make([]SuiteResult, 0, len(order))
	for _, name := range order {
		out = append(out, *suites[name])
	}
	return out
}

// reportUnit prints one unit's line. Deliberately NOT the suite verdict's
// "<name>: passed in" shape, which the merge gate parses as a suite.
func (r *Runner) reportUnit(res Result) {
	id, suite := res.Spec.ID, res.Spec.Suite
	switch res.Outcome {
	case Passed:
		r.Log.Infof("unit %s [%s] ok, %.3fs wall, %.3fs cpu", id, suite, res.Wall(), res.Usage.CPU)
	case Failed:
		r.Log.Errorf("unit %s [%s] FAILED with exit code %d after %.3fs", id, suite, res.Exit, res.Wall())
	case Cancelled:
		r.Log.Errorf("unit %s [%s] NOT RUN: its dependency %s did not pass", id, suite, res.CancelledBy)
	}
}

// reportSuite prints the suite verdict line the merge gate parses.
func (r *Runner) reportSuite(sr SuiteResult) {
	switch sr.Outcome {
	case Passed:
		r.Log.Infof("%s: passed in %.3fs", sr.Name, sr.Seconds())
	default:
		r.Log.Errorf("%s failed after %.3fs with exit code %d", sr.Name, sr.Seconds(), sr.Exit)
	}
}

// SortedBy returns results ordered by a key, largest first.
func SortedBy(results []Result, key func(Result) float64) []Result {
	out := append([]Result(nil), results...)
	sort.SliceStable(out, func(i, j int) bool { return key(out[i]) > key(out[j]) })
	return out
}
