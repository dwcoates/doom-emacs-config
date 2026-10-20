// Package run executes a planned set of units on a fixed number of core slots
// and reports them in the shape bin/test-all.sh always has: one "starting"
// line per suite, followed by its "N units planned" line, one verdict line per
// suite, then the summaries.
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
	// Display, when set, is what of the unit's output the run log shows,
	// given whether the unit passed. Nil shows it whole.
	Display func(output []byte, passed bool) []byte
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
	// UnitSeconds is the sum of the suite's own units' wall times: the slot
	// time the suite occupied. Unlike the span, it does not grow while the
	// suite's next unit waits for a slot another suite holds.
	UnitSeconds float64
}

// Seconds is the suite's wall span, first unit start to last unit end. It
// depends on how the scheduler interleaved every suite in the run.
func (s SuiteResult) Seconds() float64 { return s.End.Sub(s.Start).Seconds() }

// Cores hands out the host's core seats, shared by every run on the host
// (cores.Host). Hold blocks while the host's budget is spent.
type Cores interface {
	Hold(id string, width int) (release func() error, err error)
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
	Log   *Log
}

// grant is a unit's answer from the host's core pool.
type grant struct {
	id      string
	release func() error
	err     error
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
	if r.Cores == nil {
		return nil, nil, fmt.Errorf("run: no host core pool to hold the units' cores")
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
		case res.Outcome == Declined && sr.Outcome == Passed:
			sr.Outcome, sr.Exit = Declined, ExitDeclined
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
	start := func(s Spec) {
		begin(s)
		out := &bytes.Buffer{}
		p, err := r.Exec.Start(s, out)
		if err != nil {
			unstarted(s, err, out)
			return
		}
		running[s.ID] = p
		go func() {
			exit, cpu, err := p.Wait()
			done <- completion{id: s.ID, exit: exit, cpu: cpu, err: err, output: out}
		}()
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
			go func() {
				release, err := r.Cores.Hold(u.ID, u.Width())
				granted <- grant{id: u.ID, release: release, err: err}
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
			start(byID[g.id])
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
