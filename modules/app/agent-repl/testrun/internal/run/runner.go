// Package run executes a planned set of units on a fixed number of core slots
// and reports them in the shape bin/test-all.sh always has: one "starting"
// line per suite, one verdict line per suite, then the summaries.
package run

import (
	"bytes"
	"context"
	"fmt"
	"maps"
	"sort"
	"time"

	"agentrepl/testrun/internal/sched"
)

// ExitDeclined is the exit status a suite uses to say "my precondition is not
// met, I did not run" (autotools' "skipped"). It is never a pass.
const ExitDeclined = 77

// Spec is one unit and the process that runs it.
type Spec struct {
	sched.Unit
	Argv []string
	Dir  string
	// Env is added to the runner's own environment.
	Env []string
	// MayDecline lets exit 77 mean "declined" rather than "failed".
	MayDecline bool
	// Items parses the unit's output into seconds per item, for the history.
	// Nil for a unit that is not a chunk.
	Items func(output []byte) (map[string]float64, error)
}

// Process is one started unit.
type Process interface {
	// Wait blocks until the process exits and returns its exit status and the
	// CPU seconds it and its reaped descendants used.
	Wait() (exit int, cpu float64, err error)
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
	Declined
	Cancelled
)

// Result is one unit's record.
type Result struct {
	Spec       Spec
	Outcome    Outcome
	Exit       int
	Start, End time.Time
	CPU        float64
	// Items are the per-item seconds the unit's output reported.
	Items map[string]float64
	// CancelledBy names the failed dependency of a cancelled unit.
	CancelledBy string
}

// Wall is the unit's wall time in seconds.
func (r Result) Wall() float64 { return r.End.Sub(r.Start).Seconds() }

// SuiteResult is one suite's verdict.
type SuiteResult struct {
	Name       string
	Outcome    Outcome
	Exit       int
	Start, End time.Time
}

// Seconds is the suite's wall span, first unit start to last unit end.
func (s SuiteResult) Seconds() float64 { return s.End.Sub(s.Start).Seconds() }

// Runner runs units.
type Runner struct {
	Slots int
	Exec  Executor
	Clock Clock
	Log   *Log
}

type completion struct {
	id     string
	exit   int
	cpu    float64
	err    error
	output *bytes.Buffer
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
	units := make([]sched.Unit, len(specs))
	byID := map[string]Spec{}
	suiteUnits := map[string]int{}
	for i, s := range specs {
		units[i] = s.Unit
		byID[s.ID] = s
		suiteUnits[s.Suite]++
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
		done      = make(chan completion)
	)

	suiteOf := func(name string, at time.Time) *SuiteResult {
		sr, ok := suites[name]
		if !ok {
			sr = &SuiteResult{Name: name, Outcome: Passed, Start: at}
			suites[name] = sr
			order = append(order, name)
			r.Log.Infof("%s: starting", name)
		}
		return sr
	}
	finishUnit := func(res Result) {
		results = append(results, res)
		sr := suiteOf(res.Spec.Suite, res.Start)
		if res.End.After(sr.End) {
			sr.End = res.End
		}
		switch {
		case res.Outcome == Failed && sr.Outcome != Failed:
			sr.Outcome, sr.Exit = Failed, res.Exit
		case res.Outcome == Cancelled && sr.Outcome != Failed:
			sr.Outcome, sr.Exit = Failed, 1
		case res.Outcome == Declined && sr.Outcome == Passed:
			sr.Outcome, sr.Exit = Declined, ExitDeclined
		}
		remaining[res.Spec.Suite]--
		if remaining[res.Spec.Suite] == 0 {
			r.reportSuite(*sr)
		}
	}

	start := func(s Spec) {
		now := r.Clock.Now()
		suiteOf(s.Suite, now)
		starts[s.ID] = now
		out := &bytes.Buffer{}
		p, err := r.Exec.Start(s, out)
		if err != nil {
			r.Log.Errorf("unit %s (%s) could not start: %v", s.ID, s.Suite, err)
			go func() { done <- completion{id: s.ID, exit: -1, err: err, output: out} }()
			return
		}
		running[s.ID] = p
		go func() {
			exit, cpu, err := p.Wait()
			done <- completion{id: s.ID, exit: exit, cpu: cpu, err: err, output: out}
		}()
	}

	inFlight := 0
	for {
		for inFlight < r.Slots && ctx.Err() == nil {
			u, ok := q.Next()
			if !ok {
				break
			}
			start(byID[u.ID])
			inFlight++
		}
		if inFlight == 0 {
			break
		}
		var c completion
		select {
		case c = <-done:
		case <-ctx.Done():
			for _, p := range running {
				p.Kill()
			}
			for inFlight > 0 {
				<-done
				inFlight--
			}
			return results, r.suiteList(suites, order), ctx.Err()
		}
		inFlight--
		delete(running, c.id)
		spec := byID[c.id]
		res := Result{Spec: spec, Exit: c.exit, Start: starts[c.id], End: r.Clock.Now(), CPU: c.cpu}
		switch {
		case c.err != nil:
			r.Log.Errorf("unit %s (%s) could not be waited on: %v", c.id, spec.Suite, c.err)
			res.Outcome, res.Exit = Failed, -1
		case c.exit == 0:
			res.Outcome = Passed
		case c.exit == ExitDeclined && spec.MayDecline:
			res.Outcome = Declined
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
		r.Log.Block(c.output.Bytes())
		r.reportUnit(res)
		cancelled := q.Done(c.id, res.Outcome == Passed)
		finishUnit(res)
		for _, u := range cancelled {
			now := r.Clock.Now()
			cres := Result{Spec: byID[u.ID], Outcome: Cancelled, Exit: 1, Start: now, End: now, CancelledBy: c.id}
			r.reportUnit(cres)
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
		r.Log.Infof("unit %s [%s] ok, %.3fs wall, %.3fs cpu", id, suite, res.Wall(), res.CPU)
	case Declined:
		r.Log.Infof("unit %s [%s] declined (exit %d) after %.3fs", id, suite, ExitDeclined, res.Wall())
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
	case Declined:
		r.Log.Infof("%s: DECLINED after %.3fs — its precondition is not met (exit %d); see its message above", sr.Name, sr.Seconds(), ExitDeclined)
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
