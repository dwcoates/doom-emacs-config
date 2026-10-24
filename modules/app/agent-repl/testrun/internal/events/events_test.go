package events

import (
	"bytes"
	"errors"
	"io"
	"strings"
	"testing"
	"time"

	testrunv1 "agentrepl/proto/testrun/v1"

	"agentrepl/testrun/internal/run"
	"agentrepl/testrun/internal/sched"
	"agentrepl/testrun/internal/testtimes"
)

func readAll(t *testing.T, buf *bytes.Buffer) []*testrunv1.TestRunEvent {
	t.Helper()
	var out []*testrunv1.TestRunEvent
	for {
		ev, err := Read(buf)
		if errors.Is(err, io.EOF) {
			return out
		}
		if err != nil {
			t.Fatal(err)
		}
		out = append(out, ev)
	}
}

func result(id string, outcome run.Outcome, tests ...testtimes.Test) run.Result {
	start := time.Unix(100, 0)
	return run.Result{Spec: run.Spec{Unit: sched.Unit{ID: id, Suite: "s"}}, Outcome: outcome, Exit: 3, Start: start, End: start.Add(1500 * time.Millisecond), Tests: tests, CancelledBy: "dep"}
}

func TestTheEventsOfARunReadBackInOrder(t *testing.T) {
	// Arrange
	var buf bytes.Buffer
	e := New(&buf, &run.Log{Out: io.Discard, Err: io.Discard})

	// Act
	e.Planned([]run.Spec{{Unit: sched.Unit{ID: "u1", Suite: "s"}}}, "/tmp/run.log")
	e.UnitEnded(result("u1", run.Passed, testtimes.Test{Name: "TestA", Outcome: testtimes.Pass, Seconds: 0.25}), nil)
	e.Ended([]run.Result{result("u1", run.Passed)}, 0)

	// Assert
	evs := readAll(t, &buf)
	if len(evs) != 3 {
		t.Fatalf("events = %v, want planned, unit_ended, ended", evs)
	}
	if p := evs[0].GetPlanned(); p.GetLog().GetPath() != "/tmp/run.log" || p.GetUnits()[0].GetUnitId() != "u1" {
		t.Fatalf("planned = %v", p)
	}
	if u := evs[1].GetUnitEnded(); u.GetPassed() == nil || u.GetWallMs() != 1500 || u.GetTests()[0].GetName() != "TestA" || u.GetTests()[0].GetWallMs() != 250 || u.GetTests()[0].GetPassed() == nil {
		t.Fatalf("unit_ended = %v", u)
	}
	if evs[2].GetEnded().GetPassed().GetPassedUnits() != 1 {
		t.Fatalf("ended = %v, want passed with one unit", evs[2].GetEnded())
	}
}

func TestUnitEndedOf(t *testing.T) {
	cases := []struct {
		name  string
		res   run.Result
		check func(*testrunv1.TestRunUnitEnded) bool
	}{
		{name: "a failure carries its exit and output", res: result("u", run.Failed), check: func(u *testrunv1.TestRunUnitEnded) bool {
			return u.GetFailed().GetExitCode() == 3 && u.GetFailed().GetOutputTail() == "boom\n"
		}},
		{name: "a decline", res: result("u", run.Declined), check: func(u *testrunv1.TestRunUnitEnded) bool { return u.GetDeclined() != nil }},
		{name: "a cancellation names its cause and took no time", res: result("u", run.Cancelled), check: func(u *testrunv1.TestRunUnitEnded) bool {
			return u.GetCancelled().GetCancelledByUnitId() == "dep" && u.GetWallMs() == 0
		}},
		{name: "a failed and a skipped test", res: result("u", run.Failed, testtimes.Test{Name: "A", Outcome: testtimes.Fail}, testtimes.Test{Name: "B", Outcome: testtimes.Skip}), check: func(u *testrunv1.TestRunUnitEnded) bool {
			return u.GetTests()[0].GetFailed() != nil && u.GetTests()[1].GetSkipped() != nil
		}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := UnitEndedOf(tc.res, []byte("boom\n"))

			// Assert
			if !tc.check(got) {
				t.Fatalf("UnitEndedOf = %v", got)
			}
		})
	}
}

func TestAFailedUnitCarriesOnlyTheTailOfItsOutput(t *testing.T) {
	// Arrange
	output := strings.Repeat("early line\n", 1000) + "the failure\n"

	// Act
	got := UnitEndedOf(result("u", run.Failed), []byte(output)).GetFailed().GetOutputTail()

	// Assert
	if len(got) > TailBytes || !strings.HasSuffix(got, "the failure\n") || !strings.HasPrefix(got, "early line") {
		t.Fatalf("tail = %d bytes starting %q, want at most %d bytes from a line start to the end", len(got), got[:20], TailBytes)
	}
}

func TestEnded(t *testing.T) {
	cases := []struct {
		name        string
		results     []run.Result
		unplannable int
		check       func(*testrunv1.TestRunEnded) bool
	}{
		{name: "passed and declined units pass", results: []run.Result{result("a", run.Passed), result("b", run.Declined)}, check: func(e *testrunv1.TestRunEnded) bool {
			return e.GetPassed().GetPassedUnits() == 1 && e.GetPassed().GetDeclinedUnits() == 1
		}},
		{name: "a failure and a cancellation fail", results: []run.Result{result("a", run.Failed), result("b", run.Cancelled), result("c", run.Passed)}, check: func(e *testrunv1.TestRunEnded) bool {
			f := e.GetFailed()
			return f.GetFailedUnits() == 1 && f.GetCancelledUnits() == 1 && f.GetPassedUnits() == 1
		}},
		{name: "a suite that could not be planned fails", results: []run.Result{result("a", run.Passed)}, unplannable: 1, check: func(e *testrunv1.TestRunEnded) bool {
			return e.GetFailed().GetFailedUnits() == 1
		}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			var buf bytes.Buffer
			e := New(&buf, &run.Log{Out: io.Discard, Err: io.Discard})

			// Act
			e.Ended(tc.results, tc.unplannable)

			// Assert
			if got := readAll(t, &buf)[0].GetEnded(); !tc.check(got) {
				t.Fatalf("ended = %v", got)
			}
		})
	}
}

func TestRefused(t *testing.T) {
	// Arrange
	var buf bytes.Buffer
	e := New(&buf, &run.Log{Out: io.Discard, Err: io.Discard})

	// Act
	e.Refused("no suite runs it")

	// Assert
	if got := readAll(t, &buf); len(got) != 1 || got[0].GetRefused().GetDetail() != "no suite runs it" {
		t.Fatalf("events = %v", got)
	}
}

type brokenWriter struct{ writes int }

func (w *brokenWriter) Write([]byte) (int, error) { w.writes++; return 0, errors.New("broken pipe") }

func TestABrokenStreamIsSaidOnceAndTheRunGoesOn(t *testing.T) {
	// Arrange
	w := &brokenWriter{}
	var errs bytes.Buffer
	e := New(w, &run.Log{Out: io.Discard, Err: &errs})

	// Act
	e.Refused("a")
	e.Refused("b")

	// Assert
	if w.writes != 1 || strings.Count(errs.String(), "could not be written") != 1 {
		t.Fatalf("writes = %d, errors = %q; want one attempt and one error", w.writes, errs.String())
	}
}
