// Package events writes what a run reports, as testrun.v1.TestRunEvent
// messages, length-delimited, to the stream the caller handed the run (the
// daemon's pipe, --events-fd): the plan, each unit's verdict with the tests it
// ran, and how the run ended, or why it could not run.
//
// THE STREAM IS TYPED, NEVER PARSED. The daemon relays these messages to the
// agent that asked; nothing reads the run's log text to learn its verdicts.
package events

import (
	"io"
	"sync"
	"time"

	"google.golang.org/protobuf/encoding/protodelim"

	testrunv1 "agentrepl/proto/testrun/v1"

	"agentrepl/testrun/internal/run"
	"agentrepl/testrun/internal/testtimes"
)

// TailBytes bounds how much of a failed unit's output its event carries; the
// run's log holds all of it.
const TailBytes = 4000

// Emitter writes a run's events to one stream.
//
// A unit's hold on the host's cores runs beside the run's own loop, so its
// CPU wait is stated from another goroutine: every write is one at a time.
type Emitter struct {
	mu  sync.Mutex
	w   io.Writer
	log *run.Log
	// broken is set once a write fails: the reader is gone, so the run goes
	// on and says so once, rather than once per event.
	broken bool
}

// New is an emitter writing to w, stating a failed write on log.
func New(w io.Writer, log *run.Log) *Emitter { return &Emitter{w: w, log: log} }

func (e *Emitter) write(ev *testrunv1.TestRunEvent) {
	e.mu.Lock()
	defer e.mu.Unlock()
	if e.broken {
		return
	}
	if _, err := protodelim.MarshalTo(e.w, ev); err != nil {
		e.broken = true
		e.log.Errorf("the run's events could not be written, so whoever asked for this run hears nothing more of it; the run goes on: %v", err)
	}
}

// Planned states the units the run will run, and its log.
func (e *Emitter) Planned(specs []run.Spec, logPath string) {
	planned := &testrunv1.TestRunPlanned{Log: &testrunv1.TestRunLog{Path: logPath}}
	for _, s := range specs {
		planned.Units = append(planned.Units, &testrunv1.TestRunPlannedUnit{UnitId: s.ID, Suite: s.Suite})
	}
	e.write(&testrunv1.TestRunEvent{Event: &testrunv1.TestRunEvent_Planned{Planned: planned}})
}

// UnitEnded states one unit's verdict.
func (e *Emitter) UnitEnded(res run.Result, output []byte) {
	e.write(&testrunv1.TestRunEvent{Event: &testrunv1.TestRunEvent_UnitEnded{UnitEnded: UnitEndedOf(res, output)}})
}

// UnitEndedOf is the event stating one unit's verdict.
func UnitEndedOf(res run.Result, output []byte) *testrunv1.TestRunUnitEnded {
	ended := &testrunv1.TestRunUnitEnded{UnitId: res.Spec.ID, Suite: res.Spec.Suite, WallMs: res.End.Sub(res.Start).Milliseconds()}
	switch res.Outcome {
	case run.Passed:
		ended.Verdict = &testrunv1.TestRunUnitEnded_Passed{Passed: &testrunv1.TestRunUnitPassed{}}
	case run.Failed:
		ended.Verdict = &testrunv1.TestRunUnitEnded_Failed{Failed: &testrunv1.TestRunUnitFailed{ExitCode: int32(res.Exit), OutputTail: tail(output)}}
	case run.Declined:
		ended.Verdict = &testrunv1.TestRunUnitEnded_Declined{Declined: &testrunv1.TestRunUnitDeclined{}}
	case run.Cancelled:
		ended.WallMs = 0
		ended.Verdict = &testrunv1.TestRunUnitEnded_Cancelled{Cancelled: &testrunv1.TestRunUnitCancelled{CancelledByUnitId: res.CancelledBy}}
	default:
		panic("events: a unit result with no outcome the runner defines")
	}
	for _, t := range res.Tests {
		ended.Tests = append(ended.Tests, testResultOf(t))
	}
	return ended
}

func testResultOf(t testtimes.Test) *testrunv1.TestRunTestResult {
	r := &testrunv1.TestRunTestResult{Name: t.Name, WallMs: int64(t.Seconds * 1000)}
	switch t.Outcome {
	case testtimes.Pass:
		r.Outcome = &testrunv1.TestRunTestResult_Passed{Passed: &testrunv1.TestRunTestPassed{}}
	case testtimes.Fail:
		r.Outcome = &testrunv1.TestRunTestResult_Failed{Failed: &testrunv1.TestRunTestFailed{}}
	case testtimes.Skip:
		r.Outcome = &testrunv1.TestRunTestResult_Skipped{Skipped: &testrunv1.TestRunTestSkipped{}}
	default:
		panic("events: a test with an outcome testtimes does not define: " + t.Outcome)
	}
	return r
}

// tail is the end of a unit's output, at most TailBytes, cut at a line start
// where one falls inside the window.
func tail(output []byte) string {
	if len(output) <= TailBytes {
		return string(output)
	}
	cut := output[len(output)-TailBytes:]
	for i, b := range cut {
		if b == '\n' && i+1 < len(cut) {
			return string(cut[i+1:])
		}
	}
	return string(cut)
}

// Ended states how the run ended, from every unit's result and the suites
// that could not even be planned.
func (e *Emitter) Ended(results []run.Result, unplannable int) {
	var passed, declined, failed, cancelled int32
	for _, r := range results {
		switch r.Outcome {
		case run.Passed:
			passed++
		case run.Declined:
			declined++
		case run.Failed:
			failed++
		case run.Cancelled:
			cancelled++
		}
	}
	failed += int32(unplannable)
	ended := &testrunv1.TestRunEnded{}
	if failed+cancelled == 0 {
		ended.Outcome = &testrunv1.TestRunEnded_Passed{Passed: &testrunv1.TestRunEndedPassed{PassedUnits: passed, DeclinedUnits: declined}}
	} else {
		ended.Outcome = &testrunv1.TestRunEnded_Failed{Failed: &testrunv1.TestRunEndedFailed{FailedUnits: failed, CancelledUnits: cancelled, PassedUnits: passed}}
	}
	e.write(&testrunv1.TestRunEvent{Event: &testrunv1.TestRunEvent_Ended{Ended: ended}})
}

// Refused states that the selection could not be run, and why.
func (e *Emitter) Refused(detail string) {
	e.write(&testrunv1.TestRunEvent{Event: &testrunv1.TestRunEvent_Refused{Refused: &testrunv1.TestRunRefused{Detail: detail}}})
}

// WaitingForCPU states that unit id waits to start because the host is too
// busy for tests to add to.
func (e *Emitter) WaitingForCPU(id string, busyPercent float64, capPercent int) {
	e.write(&testrunv1.TestRunEvent{Event: &testrunv1.TestRunEvent_WaitingForCpu{WaitingForCpu: &testrunv1.TestRunWaitingForCpu{
		UnitId: id, BusyPercent: busyPercent, CapPercent: uint32(capPercent),
	}}})
}

// CPUWaitEnded states that unit id, which waited for the host's CPU, has its
// cores after waiting waited.
func (e *Emitter) CPUWaitEnded(id string, waited time.Duration) {
	e.write(&testrunv1.TestRunEvent{Event: &testrunv1.TestRunEvent_CpuWaitEnded{CpuWaitEnded: &testrunv1.TestRunCpuWaitEnded{
		UnitId: id, WaitedMs: waited.Milliseconds(),
	}}})
}

// Read reads the next event off r: the daemon's side of the stream.
func Read(r protodelim.Reader) (*testrunv1.TestRunEvent, error) {
	ev := &testrunv1.TestRunEvent{}
	if err := protodelim.UnmarshalFrom(r, ev); err != nil {
		return nil, err
	}
	return ev, nil
}
