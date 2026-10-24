package run

import (
	"bytes"
	"context"
	"errors"
	"strings"
	"sync"
	"testing"
	"time"

	"agentrepl/testrun/internal/sched"
	"agentrepl/testrun/internal/testtimes"
)

// script is what a fake unit does: print, then exit.
type script struct {
	output   string
	exit     int
	usage    Usage
	startErr error
	waitErr  error
	// hold, when set, blocks the unit until it is closed.
	hold chan struct{}
}

type fakeExec struct {
	mu      sync.Mutex
	scripts map[string]script
	started []string
	running int
	peak    int
	// width and peakWidth count the slots the running units hold.
	width     int
	peakWidth int
	widths    map[string]int
	killed    []string
	onStart   func(id string)
}

type fakeProc struct {
	e    *fakeExec
	id   string
	s    script
	out  *bytes.Buffer
	kill chan struct{}
	once sync.Once
}

func (e *fakeExec) Start(spec Spec, out *bytes.Buffer) (Process, error) {
	s := e.scripts[spec.ID]
	if s.startErr != nil {
		return nil, s.startErr
	}
	e.mu.Lock()
	e.started = append(e.started, spec.ID)
	e.running++
	if e.running > e.peak {
		e.peak = e.running
	}
	if e.widths == nil {
		e.widths = map[string]int{}
	}
	e.widths[spec.ID] = spec.Width()
	e.width += spec.Width()
	e.peakWidth = max(e.peakWidth, e.width)
	e.mu.Unlock()
	if e.onStart != nil {
		e.onStart(spec.ID)
	}
	return &fakeProc{e: e, id: spec.ID, s: s, out: out, kill: make(chan struct{})}, nil
}

func (p *fakeProc) Wait() (int, Usage, error) {
	exit := p.s.exit
	if p.s.hold != nil {
		select {
		case <-p.s.hold:
		case <-p.kill:
			exit = 143
		}
	}
	p.out.WriteString(p.s.output)
	p.e.mu.Lock()
	p.e.running--
	p.e.width -= p.e.widths[p.id]
	p.e.mu.Unlock()
	return exit, p.s.usage, p.s.waitErr
}

func (p *fakeProc) Kill() {
	p.once.Do(func() {
		p.e.mu.Lock()
		p.e.killed = append(p.e.killed, p.id)
		p.e.mu.Unlock()
		close(p.kill)
	})
}

// tickClock advances one second every time it is read, so every duration in
// a test is a whole, predictable number of seconds.
type tickClock struct {
	mu sync.Mutex
	t  time.Time
}

func (c *tickClock) Now() time.Time {
	c.mu.Lock()
	defer c.mu.Unlock()
	c.t = c.t.Add(time.Second)
	return c.t
}

// fakeCores is the host's core pool: free seats shared by every hold, a
// hold blocking until enough are free.
type fakeCores struct {
	mu   sync.Mutex
	cond *sync.Cond
	free int
	// holdErr fails a unit's hold; releaseErr fails every release.
	holdErr    map[string]error
	releaseErr error
	held       int
	peak       int
	released   []string
	// waiting, when set, receives a unit's ID when its hold must wait.
	waiting chan string
}

func newCores(free int) *fakeCores {
	c := &fakeCores{free: free}
	c.cond = sync.NewCond(&c.mu)
	return c
}

func (c *fakeCores) Hold(id string, width int) (func() error, error) {
	if err := c.holdErr[id]; err != nil {
		return nil, err
	}
	c.mu.Lock()
	defer c.mu.Unlock()
	if c.free < width && c.waiting != nil {
		c.waiting <- id
	}
	for c.free < width {
		c.cond.Wait()
	}
	c.free -= width
	c.held += width
	c.peak = max(c.peak, c.held)
	return func() error {
		c.mu.Lock()
		defer c.mu.Unlock()
		c.free += width
		c.held -= width
		c.released = append(c.released, id)
		c.cond.Broadcast()
		return c.releaseErr
	}, nil
}

// give frees n more of the host's seats, as another run's units exiting would.
func (c *fakeCores) give(n int) {
	c.mu.Lock()
	defer c.mu.Unlock()
	c.free += n
	c.cond.Broadcast()
}

func newRunner(slots int, e *fakeExec) (*Runner, *bytes.Buffer, *bytes.Buffer) {
	return newRunnerOn(slots, newCores(1<<20), e)
}

func newRunnerOn(slots int, c *fakeCores, e *fakeExec) (*Runner, *bytes.Buffer, *bytes.Buffer) {
	out, errOut := &bytes.Buffer{}, &bytes.Buffer{}
	return &Runner{Slots: slots, Cores: c, Exec: e, Clock: &tickClock{}, Log: &Log{Out: out, Err: errOut}}, out, errOut
}

func spec(id, suite string, deps ...string) Spec {
	return Spec{Unit: sched.Unit{ID: id, Suite: suite, Deps: deps, Est: 1}, Argv: []string{"x"}}
}

func suiteByName(suites []SuiteResult, name string) SuiteResult {
	for _, s := range suites {
		if s.Name == name {
			return s
		}
	}
	return SuiteResult{Name: "<missing " + name + ">", Outcome: -1}
}

func TestRunReportsEachSuiteVerdict(t *testing.T) {
	tests := []struct {
		name        string
		specs       []Spec
		scripts     map[string]script
		suite       string
		wantOutcome Outcome
		wantExit    int
		wantLine    string
		wantErrLine string
	}{
		{
			name:        "every unit passing passes the suite",
			specs:       []Spec{spec("a#00", "a"), spec("a#01", "a")},
			scripts:     map[string]script{},
			suite:       "a",
			wantOutcome: Passed,
			wantLine:    "[agent-repl-tests] a: passed in ",
		},
		{
			name:        "one failing unit fails the suite with its exit code",
			specs:       []Spec{spec("a#00", "a"), spec("a#01", "a")},
			scripts:     map[string]script{"a#01": {exit: 3}},
			suite:       "a",
			wantOutcome: Failed,
			wantExit:    3,
			wantErrLine: "[agent-repl-tests] ERROR: a failed after ",
		},
		{
			name:        "exit 77 from a unit that may decline declines the suite",
			specs:       []Spec{{Unit: sched.Unit{ID: "e", Suite: "e"}, Argv: []string{"x"}, MayDecline: true}},
			scripts:     map[string]script{"e": {exit: ExitDeclined}},
			suite:       "e",
			wantOutcome: Declined,
			wantExit:    ExitDeclined,
			wantLine:    "[agent-repl-tests] e: DECLINED after ",
		},
		{
			name:        "exit 77 from a unit that may not decline is a failure",
			specs:       []Spec{spec("e", "e")},
			scripts:     map[string]script{"e": {exit: ExitDeclined}},
			suite:       "e",
			wantOutcome: Failed,
			wantExit:    ExitDeclined,
			wantErrLine: "e failed after ",
		},
		{
			name:        "a unit that cannot start fails its suite",
			specs:       []Spec{spec("s", "s")},
			scripts:     map[string]script{"s": {startErr: errors.New("no such file")}},
			suite:       "s",
			wantOutcome: Failed,
			wantExit:    -1,
			wantErrLine: "unit s (s) could not start: no such file",
		},
		{
			name:        "a unit that cannot be waited on fails its suite",
			specs:       []Spec{spec("w", "w")},
			scripts:     map[string]script{"w": {waitErr: errors.New("wait4: boom")}},
			suite:       "w",
			wantOutcome: Failed,
			wantExit:    -1,
			wantErrLine: "unit w (w) could not be waited on: wait4: boom",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			r, out, errOut := newRunner(2, &fakeExec{scripts: tt.scripts})

			// Act
			_, suites, err := r.Run(context.Background(), tt.specs)

			// Assert
			if err != nil {
				t.Fatal(err)
			}
			got := suiteByName(suites, tt.suite)
			if got.Outcome != tt.wantOutcome || got.Exit != tt.wantExit {
				t.Fatalf("suite = %+v, want outcome %v exit %d", got, tt.wantOutcome, tt.wantExit)
			}
			if !strings.Contains(out.String(), "[agent-repl-tests] "+tt.suite+": starting") {
				t.Fatalf("no starting line in:\n%s", out)
			}
			if tt.wantLine != "" && !strings.Contains(out.String(), tt.wantLine) {
				t.Fatalf("stdout lacks %q:\n%s", tt.wantLine, out)
			}
			if tt.wantErrLine != "" && !strings.Contains(errOut.String(), tt.wantErrLine) {
				t.Fatalf("stderr lacks %q:\n%s", tt.wantErrLine, errOut)
			}
		})
	}
}

func TestRunSaysHowManyUnitsEachSuiteRunsAsItStarts(t *testing.T) {
	// Arrange: a suite of three units and a suite of one.
	r, out, _ := newRunner(2, &fakeExec{scripts: map[string]script{}})
	specs := []Spec{spec("a1", "a"), spec("a2", "a"), spec("a3", "a"), spec("b1", "b")}

	// Act
	if _, _, err := r.Run(context.Background(), specs); err != nil {
		t.Fatal(err)
	}

	// Assert
	for _, want := range []string{"[agent-repl-tests] a: 3 units planned", "[agent-repl-tests] b: 1 units planned"} {
		if !strings.Contains(out.String(), want) {
			t.Fatalf("stdout lacks %q:\n%s", want, out)
		}
	}
}

func TestRunNeverExceedsItsSlots(t *testing.T) {
	// Arrange: six units that each hold until released, two slots.
	release := make(chan struct{})
	scripts := map[string]script{}
	var specs []Spec
	for _, id := range []string{"a", "b", "c", "d", "e", "f"} {
		scripts[id] = script{hold: release}
		specs = append(specs, spec(id, "s"))
	}
	e := &fakeExec{scripts: scripts}
	starts := make(chan string, len(specs))
	e.onStart = func(id string) { starts <- id }
	r, _, _ := newRunner(2, e)

	// Act
	result := make(chan error)
	go func() {
		_, _, err := r.Run(context.Background(), specs)
		result <- err
	}()
	<-starts
	<-starts
	close(release)
	err := <-result

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	if e.peak != 2 {
		t.Fatalf("peak concurrency = %d, want 2", e.peak)
	}
	if len(e.started) != 6 {
		t.Fatalf("started %d units, want 6", len(e.started))
	}
}

func TestRunCancelsTheDependentsOfAFailedUnit(t *testing.T) {
	// Arrange
	specs := []Spec{spec("build", "e2e"), spec("e2e#00", "e2e", "build"), spec("other", "o")}
	e := &fakeExec{scripts: map[string]script{"build": {exit: 2}}}
	r, _, errOut := newRunner(1, e)

	// Act
	results, suites, err := r.Run(context.Background(), specs)

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	for _, id := range e.started {
		if id == "e2e#00" {
			t.Fatal("a unit whose dependency failed was started")
		}
	}
	var cancelled *Result
	for i := range results {
		if results[i].Spec.ID == "e2e#00" {
			cancelled = &results[i]
		}
	}
	if cancelled == nil || cancelled.Outcome != Cancelled || cancelled.CancelledBy != "build" {
		t.Fatalf("e2e#00 result = %+v, want cancelled by build", cancelled)
	}
	if !strings.Contains(errOut.String(), "unit e2e#00 [e2e] NOT RUN: its dependency build did not pass") {
		t.Fatalf("no cancellation line in:\n%s", errOut)
	}
	if s := suiteByName(suites, "e2e"); s.Outcome != Failed || s.Exit != 2 {
		t.Fatalf("e2e suite = %+v, want failed with the build's exit 2", s)
	}
	if s := suiteByName(suites, "o"); s.Outcome != Passed {
		t.Fatalf("unrelated suite = %+v, want passed", s)
	}
}

func TestRunSumsEachSuitesOwnUnitTime(t *testing.T) {
	tests := []struct {
		name      string
		specs     []Spec
		scripts   map[string]script
		suite     string
		wantUnits float64
		wantSpan  float64
	}{
		{
			// The dependencies force a's units either side of b's on one slot.
			// Every unit takes one tick, so a holds a slot for 2s of a 5s span.
			name:      "another suite's unit between two of a suite's units is not the suite's time",
			specs:     []Spec{spec("a#00", "a"), spec("b", "b", "a#00"), spec("a#01", "a", "b")},
			scripts:   map[string]script{},
			suite:     "a",
			wantUnits: 2,
			wantSpan:  5,
		},
		{
			name:      "a cancelled unit adds no time",
			specs:     []Spec{spec("build", "e"), spec("e#00", "e", "build")},
			scripts:   map[string]script{"build": {exit: 2}},
			suite:     "e",
			wantUnits: 1,
			wantSpan:  2,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			r, _, _ := newRunner(1, &fakeExec{scripts: tt.scripts})

			// Act
			_, suites, err := r.Run(context.Background(), tt.specs)

			// Assert
			if err != nil {
				t.Fatal(err)
			}
			got := suiteByName(suites, tt.suite)
			if got.UnitSeconds != tt.wantUnits || got.Seconds() != tt.wantSpan {
				t.Fatalf("suite %s: %.0fs of units over a %.0fs span, want %.0fs over %.0fs",
					tt.suite, got.UnitSeconds, got.Seconds(), tt.wantUnits, tt.wantSpan)
			}
		})
	}
}

func TestRunPrintsEachUnitsOutputInOnePiece(t *testing.T) {
	// Arrange: two units run side by side.
	release := make(chan struct{})
	e := &fakeExec{scripts: map[string]script{
		"a": {output: "a1\na2\n", hold: release},
		"b": {output: "b1\nb2", hold: release},
	}}
	starts := make(chan string, 2)
	e.onStart = func(id string) { starts <- id }
	r, out, _ := newRunner(2, e)

	// Act
	result := make(chan error)
	go func() {
		_, _, err := r.Run(context.Background(), []Spec{spec("a", "s"), spec("b", "s")})
		result <- err
	}()
	<-starts
	<-starts
	close(release)
	if err := <-result; err != nil {
		t.Fatal(err)
	}

	// Assert
	text := out.String()
	if !strings.Contains(text, "a1\na2\n") || !strings.Contains(text, "b1\nb2\n") {
		t.Fatalf("a unit's output was split or unterminated:\n%s", text)
	}
}

func TestRunRecordsItemTimings(t *testing.T) {
	// Arrange
	s := spec("ert#00", "ert")
	s.Items = func(out []byte) (map[string]float64, error) {
		return map[string]float64{string(bytes.TrimSpace(out)): 1.5}, nil
	}
	r, _, _ := newRunner(1, &fakeExec{scripts: map[string]script{"ert#00": {output: "test-core.el\n", usage: Usage{CPU: 0.75, MaxRSSBytes: 4096, BlockReads: 2, BlockWrites: 3}}}})

	// Act
	results, _, err := r.Run(context.Background(), []Spec{s})

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	if got := results[0].Items["test-core.el"]; got != 1.5 {
		t.Fatalf("item seconds = %v, want 1.5", got)
	}
	if want := (Usage{CPU: 0.75, MaxRSSBytes: 4096, BlockReads: 2, BlockWrites: 3}); results[0].Usage != want {
		t.Fatalf("usage = %+v, want %+v", results[0].Usage, want)
	}
}

func TestRunFailsAPassingUnitWhoseTimingsAreUnreadable(t *testing.T) {
	// Arrange
	s := spec("ert#00", "ert")
	s.Items = func([]byte) (map[string]float64, error) { return nil, errors.New("no item lines") }
	r, _, errOut := newRunner(1, &fakeExec{scripts: map[string]script{}})

	// Act
	_, suites, err := r.Run(context.Background(), []Spec{s})

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	if got := suiteByName(suites, "ert"); got.Outcome != Failed {
		t.Fatalf("suite = %+v, want failed", got)
	}
	if !strings.Contains(errOut.String(), "unit ert#00 (ert) passed but its item timings are unreadable: no item lines") {
		t.Fatalf("no timing error in:\n%s", errOut)
	}
}

func TestRunRecordsTheTestTimesOfAPassedAndAFailedUnit(t *testing.T) {
	tests := []struct {
		name   string
		exit   int
		passed bool
	}{
		{name: "a passed unit", exit: 0, passed: true},
		{name: "a failed unit", exit: 1, passed: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s := spec("go#00", "daemon")
			var told []bool
			s.Tests = func(out []byte, passed bool) ([]testtimes.Test, error) {
				told = append(told, passed)
				return []testtimes.Test{{Suite: "daemon", Name: string(bytes.TrimSpace(out)), Seconds: 2}}, nil
			}
			r, _, _ := newRunner(1, &fakeExec{scripts: map[string]script{"go#00": {output: "x.TestA\n", exit: tt.exit}}})

			// Act
			results, _, err := r.Run(context.Background(), []Spec{s})

			// Assert
			if err != nil {
				t.Fatal(err)
			}
			if len(told) != 1 || told[0] != tt.passed {
				t.Fatalf("Tests was told passed = %v, want [%v]", told, tt.passed)
			}
			if len(results[0].Tests) != 1 || results[0].Tests[0].Name != "x.TestA" {
				t.Fatalf("tests = %+v, want x.TestA", results[0].Tests)
			}
		})
	}
}

func TestRunFailsAPassingUnitWhoseTestTimesAreUnreadable(t *testing.T) {
	// Arrange
	s := spec("go#00", "daemon")
	s.Tests = func([]byte, bool) ([]testtimes.Test, error) { return nil, errors.New("no tests file") }
	r, _, errOut := newRunner(1, &fakeExec{scripts: map[string]script{}})

	// Act
	_, suites, err := r.Run(context.Background(), []Spec{s})

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	if got := suiteByName(suites, "daemon"); got.Outcome != Failed {
		t.Fatalf("suite = %+v, want failed", got)
	}
	if !strings.Contains(errOut.String(), "unit go#00 (daemon) passed but its test timings are unreadable: no tests file") {
		t.Fatalf("no timing error in:\n%s", errOut)
	}
}

func TestRunRecordsAFailedUnitsUnreadableTestTimesAtInfo(t *testing.T) {
	// Arrange
	s := spec("go#00", "daemon")
	s.Tests = func([]byte, bool) ([]testtimes.Test, error) { return nil, errors.New("no tests file") }
	r, out, errOut := newRunner(1, &fakeExec{scripts: map[string]script{"go#00": {exit: 1}}})

	// Act
	_, _, err := r.Run(context.Background(), []Spec{s})

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	if !strings.Contains(out.String(), "unit go#00 (daemon) failed, and its test timings are unreadable: no tests file") ||
		strings.Contains(errOut.String(), "test timings") {
		t.Fatalf("want the unreadable timings at info only; out:\n%s\nerr:\n%s", out, errOut)
	}
}

func TestRunReadsNoTestTimesOfADeclinedUnit(t *testing.T) {
	// Arrange
	s := spec("h#00", "harness")
	s.MayDecline = true
	called := false
	s.Tests = func([]byte, bool) ([]testtimes.Test, error) { called = true; return nil, nil }
	r, _, _ := newRunner(1, &fakeExec{scripts: map[string]script{"h#00": {exit: ExitDeclined}}})

	// Act
	if _, _, err := r.Run(context.Background(), []Spec{s}); err != nil {
		t.Fatal(err)
	}

	// Assert
	if called {
		t.Fatalf("a declined unit's test times were read")
	}
}

func TestRunKillsEveryRunningUnitWhenCancelled(t *testing.T) {
	// Arrange
	hold := make(chan struct{})
	e := &fakeExec{scripts: map[string]script{"a": {hold: hold}, "b": {hold: hold}}}
	starts := make(chan string, 2)
	e.onStart = func(id string) { starts <- id }
	r, _, _ := newRunner(2, e)
	ctx, cancel := context.WithCancel(context.Background())

	// Act
	result := make(chan error)
	go func() {
		_, _, err := r.Run(ctx, []Spec{spec("a", "s"), spec("b", "s"), spec("c", "s")})
		result <- err
	}()
	<-starts
	<-starts
	cancel()
	err := <-result

	// Assert
	if !errors.Is(err, context.Canceled) {
		t.Fatalf("Run error = %v, want context.Canceled", err)
	}
	if len(e.killed) != 2 {
		t.Fatalf("killed %v, want both running units", e.killed)
	}
	if len(e.started) != 2 {
		t.Fatalf("started %v after cancellation, want no new unit", e.started)
	}
}

func TestRunHoldsAWideUnitsWholeWidth(t *testing.T) {
	// Arrange: a two-slot unit and three one-slot units on three slots, every
	// unit holding until released, so they would all overlap if allowed to.
	release := make(chan struct{})
	wide := spec("w", "e2e-emacs")
	wide.Est, wide.Slots = 10, 2
	specs := []Spec{wide, spec("a", "s"), spec("b", "s"), spec("c", "s")}
	scripts := map[string]script{}
	for _, sp := range specs {
		scripts[sp.ID] = script{hold: release}
	}
	e := &fakeExec{scripts: scripts}
	starts := make(chan string, len(specs))
	e.onStart = func(id string) { starts <- id }
	r, _, _ := newRunner(3, e)

	// Act
	result := make(chan error)
	go func() {
		_, _, err := r.Run(context.Background(), specs)
		result <- err
	}()
	<-starts
	<-starts
	close(release)
	err := <-result

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	if e.peakWidth != 3 {
		t.Fatalf("peak slots held = %d, want exactly the 3 the runner has", e.peakWidth)
	}
	if len(e.started) != 4 {
		t.Fatalf("started %v, want all four units", e.started)
	}
}

func TestRunRefusesAUnitWiderThanItsSlots(t *testing.T) {
	// Arrange
	wide := spec("w", "e2e-emacs")
	wide.Slots = 3
	e := &fakeExec{}
	r, _, _ := newRunner(2, e)

	// Act
	_, _, err := r.Run(context.Background(), []Spec{wide})

	// Assert
	if err == nil || !strings.Contains(err.Error(), `unit "w" needs 3 core slots but this host has only 2`) {
		t.Fatalf("err = %v, want the width refusal", err)
	}
	if len(e.started) != 0 {
		t.Fatalf("started %v before refusing", e.started)
	}
}

func TestRunRefusesZeroSlots(t *testing.T) {
	// Arrange
	r, _, _ := newRunner(0, &fakeExec{})

	// Act
	_, _, err := r.Run(context.Background(), []Spec{spec("a", "s")})

	// Assert
	if err == nil {
		t.Fatal("Run accepted zero slots")
	}
}

func TestRunCancelledBeforeAnythingStartsReturnsTheCancellation(t *testing.T) {
	// Arrange
	e := &fakeExec{scripts: map[string]script{}}
	r, _, _ := newRunner(2, e)
	ctx, cancel := context.WithCancel(context.Background())
	cancel()

	// Act
	_, _, err := r.Run(ctx, []Spec{spec("a", "s")})

	// Assert
	if !errors.Is(err, context.Canceled) {
		t.Fatalf("Run error = %v, want context.Canceled", err)
	}
	if len(e.started) != 0 {
		t.Fatalf("started %v after cancellation", e.started)
	}
}

func TestRunBracketsEachUnitsOutputWithItsSuite(t *testing.T) {
	// Arrange
	e := &fakeExec{scripts: map[string]script{"ert#00": {output: "line one\nline two\n"}}}
	r, out, _ := newRunner(1, e)

	// Act
	if _, _, err := r.Run(context.Background(), []Spec{spec("ert#00", "ert")}); err != nil {
		t.Fatal(err)
	}

	// Assert
	want := "[agent-repl-tests] unit ert#00 [ert] output:\nline one\nline two\n[agent-repl-tests] unit ert#00 [ert] ok, "
	if !strings.Contains(out.String(), want) {
		t.Fatalf("the block is not bracketed by its unit lines:\n%s", out)
	}
}

func TestRunShowsWhatAUnitsDisplayKeeps(t *testing.T) {
	// Arrange
	s := spec("g#00", "g")
	var sawPassed bool
	s.Display = func(out []byte, passed bool) []byte {
		sawPassed = passed
		return []byte("kept\n")
	}
	r, out, _ := newRunner(1, &fakeExec{scripts: map[string]script{"g#00": {output: "noise\n"}}})

	// Act
	if _, _, err := r.Run(context.Background(), []Spec{s}); err != nil {
		t.Fatal(err)
	}

	// Assert
	if !sawPassed || !strings.Contains(out.String(), "kept\n") || strings.Contains(out.String(), "noise") {
		t.Fatalf("display was not applied (passed=%v):\n%s", sawPassed, out)
	}
}

func TestRunNeverHoldsMoreThanTheHostsFreeCores(t *testing.T) {
	// Arrange: four slots of its own, but other runs hold all but two of the
	// host's core seats.
	release := make(chan struct{})
	scripts := map[string]script{}
	var specs []Spec
	for _, id := range []string{"a", "b", "c", "d"} {
		scripts[id] = script{hold: release}
		specs = append(specs, spec(id, "s"))
	}
	e := &fakeExec{scripts: scripts}
	starts := make(chan string, len(specs))
	e.onStart = func(id string) { starts <- id }
	c := newCores(2)
	c.waiting = make(chan string, 1)
	r, _, _ := newRunnerOn(4, c, e)

	// Act
	result := make(chan error)
	go func() {
		_, _, err := r.Run(context.Background(), specs)
		result <- err
	}()
	<-starts
	<-starts
	waiter := <-c.waiting
	close(release)
	err := <-result

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	if e.peak != 2 || c.peak != 2 {
		t.Fatalf("peak running = %d, peak seats = %d; want both 2, the host's free cores", e.peak, c.peak)
	}
	if waiter != "c" {
		t.Fatalf("the unit that waited for host cores = %q, want c, the queue's next", waiter)
	}
	if len(e.started) != 4 || len(c.released) != 4 {
		t.Fatalf("started %v, released %v; want all four started and given back", e.started, c.released)
	}
}

func TestRunHoldsAWideUnitsWidthInHostCores(t *testing.T) {
	// Arrange
	wide := spec("w", "e2e-emacs")
	wide.Slots = 2
	e := &fakeExec{scripts: map[string]script{}}
	c := newCores(2)
	r, _, _ := newRunnerOn(2, c, e)

	// Act
	_, _, err := r.Run(context.Background(), []Spec{wide})

	// Assert
	if err != nil || c.peak != 2 {
		t.Fatalf("Run = %v, peak seats = %d; want the wide unit holding 2", err, c.peak)
	}
}

func TestRunStartsAUnitOnceAnotherRunGivesCoresBack(t *testing.T) {
	// Arrange: the host's every seat is held by another run.
	e := &fakeExec{scripts: map[string]script{}}
	c := newCores(0)
	c.waiting = make(chan string, 1)
	r, _, _ := newRunnerOn(2, c, e)

	// Act
	result := make(chan error)
	go func() {
		_, _, err := r.Run(context.Background(), []Spec{spec("a", "s")})
		result <- err
	}()
	<-c.waiting
	e.mu.Lock()
	startedWhileHeld := len(e.started)
	e.mu.Unlock()
	c.give(1)
	err := <-result

	// Assert
	if startedWhileHeld != 0 {
		t.Fatal("a unit started while the host's cores were all held")
	}
	if err != nil || len(e.started) != 1 {
		t.Fatalf("Run = %v, started %v; want a started once a seat was given back", err, e.started)
	}
}

func TestRunFailsAUnitThatCannotHoldItsHostCores(t *testing.T) {
	// Arrange
	e := &fakeExec{scripts: map[string]script{}}
	c := newCores(4)
	c.holdErr = map[string]error{"a": errors.New("seats directory is gone")}
	r, _, errOut := newRunnerOn(2, c, e)

	// Act
	results, suites, err := r.Run(context.Background(), []Spec{spec("a", "s"), spec("b", "t")})

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	if got := suiteByName(suites, "s"); got.Outcome != Failed {
		t.Fatalf("suite s = %v, want Failed", got.Outcome)
	}
	if got := suiteByName(suites, "t"); got.Outcome != Passed {
		t.Fatalf("suite t = %v, want Passed: one unit's hold must not stop the rest", got.Outcome)
	}
	if !strings.Contains(errOut.String(), "unit a (s) could not start: seats directory is gone") {
		t.Fatalf("stderr = %q, want the hold's failure", errOut.String())
	}
	for _, res := range results {
		if res.Spec.ID == "a" && res.Exit != -1 {
			t.Fatalf("unit a exit = %d, want -1", res.Exit)
		}
	}
	if len(e.started) != 1 || e.started[0] != "b" {
		t.Fatalf("started %v, want only b", e.started)
	}
}

func TestRunLogsAFailedCoreRelease(t *testing.T) {
	// Arrange
	e := &fakeExec{scripts: map[string]script{}}
	c := newCores(1)
	c.releaseErr = errors.New("close: bad file descriptor")
	r, _, errOut := newRunnerOn(1, c, e)

	// Act
	_, suites, err := r.Run(context.Background(), []Spec{spec("a", "s")})

	// Assert
	if err != nil || suiteByName(suites, "s").Outcome != Passed {
		t.Fatalf("Run = %v; want the unit's own verdict to stand", err)
	}
	if !strings.Contains(errOut.String(), "unit a gave back its host cores, but closing a seat failed: close: bad file descriptor") {
		t.Fatalf("stderr = %q, want the release's failure", errOut.String())
	}
}

func TestRunCancelledWhileAUnitWaitsForHostCores(t *testing.T) {
	// Arrange: a runs; b waits for the host's only other seat, held elsewhere.
	hold := make(chan struct{})
	e := &fakeExec{scripts: map[string]script{"a": {hold: hold}}}
	starts := make(chan string, 1)
	e.onStart = func(id string) { starts <- id }
	c := newCores(1)
	c.waiting = make(chan string, 1)
	r, _, _ := newRunnerOn(2, c, e)
	ctx, cancel := context.WithCancel(context.Background())

	// Act
	result := make(chan error)
	go func() {
		_, _, err := r.Run(ctx, []Spec{spec("a", "s"), spec("b", "s")})
		result <- err
	}()
	<-starts
	<-c.waiting
	cancel()
	err := <-result

	// Assert
	if !errors.Is(err, context.Canceled) {
		t.Fatalf("Run error = %v, want context.Canceled", err)
	}
	if len(e.started) != 1 || len(e.killed) != 1 {
		t.Fatalf("started %v, killed %v; want only a, killed", e.started, e.killed)
	}
	if len(c.released) != 1 || c.released[0] != "a" {
		t.Fatalf("released %v, want a's seat given back", c.released)
	}
}

func TestRunRefusesARunWithoutAHostCorePool(t *testing.T) {
	// Arrange
	r, _, _ := newRunner(1, &fakeExec{})
	r.Cores = nil

	// Act
	_, _, err := r.Run(context.Background(), []Spec{spec("a", "s")})

	// Assert
	if err == nil || !strings.Contains(err.Error(), "no host core pool") {
		t.Fatalf("Run = %v, want the missing pool refused", err)
	}
}

func TestRunTellsItsObserverEveryUnitThatEnded(t *testing.T) {
	// Arrange: a failing build cancels the chunk that depends on it.
	specs := []Spec{spec("build", "e2e"), spec("e2e#00", "e2e", "build")}
	e := &fakeExec{scripts: map[string]script{"build": {exit: 2, output: "boom"}}}
	r, _, _ := newRunner(1, e)
	type told struct {
		id      string
		outcome Outcome
		output  string
	}
	var got []told
	r.Ended = func(res Result, output []byte) { got = append(got, told{res.Spec.ID, res.Outcome, string(output)}) }

	// Act
	if _, _, err := r.Run(context.Background(), specs); err != nil {
		t.Fatal(err)
	}

	// Assert
	want := []told{{"build", Failed, "boom"}, {"e2e#00", Cancelled, ""}}
	if len(got) != len(want) || got[0] != want[0] || got[1] != want[1] {
		t.Fatalf("observer was told %+v, want %+v", got, want)
	}
}
