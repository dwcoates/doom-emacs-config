package merge

import (
	"errors"
	"testing"
	"time"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// --- the shared bound ----------------------------------------------------

// A call the sink answers carries the sink's own verdict, never the timeout's.
func TestABoundedSinkCallReportsTheSinksOwnError(t *testing.T) {
	// Arrange.
	want := errors.New("state store down")

	// Act.
	err, expired := callSinkBounded(time.Hour, func() error { return want })

	// Assert.
	if expired {
		t.Fatal("a sink that answered was reported as expired")
	}
	if !errors.Is(err, want) {
		t.Fatalf("err = %v, want the sink's own %v", err, want)
	}
}

// The bound is what stops a wedged sink from holding its caller, which is the
// whole defect: a drain blocked inside the SSM until the daemon was bounced.
func TestABoundedSinkCallExpiresOnASinkThatNeverReturns(t *testing.T) {
	// Arrange.
	release := make(chan struct{})
	defer close(release)

	// Act.
	err, expired := callSinkBounded(time.Millisecond, func() error {
		<-release
		return nil
	})

	// Assert.
	if !expired {
		t.Fatal("a sink that never returned was not reported as expired")
	}
	if err != nil {
		t.Fatalf("err = %v, want no sink verdict for an expired call", err)
	}
}

// A sink returning AFTER the bound expired must not park its goroutine on a send
// nobody is receiving. The result channel is buffered for exactly this, and a
// regression to an unbuffered one would leak a goroutine per expiry.
func TestASinkReturningAfterTheBoundDoesNotParkItsGoroutine(t *testing.T) {
	// Arrange.
	release := make(chan struct{})
	returned := make(chan struct{})

	// Act.
	_, expired := callSinkBounded(time.Millisecond, func() error {
		<-release
		close(returned)
		return nil
	})
	close(release)

	// Assert.
	if !expired {
		t.Fatal("the wedged call was not reported as expired")
	}
	// Completes only if the call ran past its send, which an unbuffered result
	// channel would have parked forever.
	<-returned
}

// A non-positive bound is the emitters' "unset", and it must resolve to the
// production figure rather than to an instant expiry.
func TestAnUnsetBoundTakesTheProductionFigure(t *testing.T) {
	// Arrange, Act, Assert.
	for _, bound := range []time.Duration{0, -time.Second} {
		if got := boundOr(bound); got != sinkPublishBound {
			t.Fatalf("boundOr(%s) = %s, want sinkPublishBound %s", bound, got, sinkPublishBound)
		}
	}
}

// The production bound must stay well inside the two-minute observability budget
// a merge run is held to, or a single expiry eats the whole budget.
func TestTheProductionBoundIsInsideTheObservabilityBudget(t *testing.T) {
	// Arrange, Act, Assert.
	if sinkPublishBound <= 0 || sinkPublishBound >= 2*time.Minute {
		t.Fatalf("sinkPublishBound = %s, want a positive bound well inside the two-minute observability budget", sinkPublishBound)
	}
}

// --- both emitters share it ----------------------------------------------

// wedgedSink never returns from either sink method, standing in for the SSM
// blocked behind its process-wide mutex.
type wedgedSink struct{ release chan struct{} }

func (s *wedgedSink) RecordMergeTransition(string, Phase, string) error {
	<-s.release
	return nil
}

func (s *wedgedSink) RecordMergeStatus(string, Phase, string, *frontendv1.MergeStatus) error {
	<-s.release
	return nil
}

// THE POINT OF THE EXTRACTION. The pipeline writes to two sinks and both are the
// SSM, so both must be bounded — a state transition that could still hang would
// wedge a drain exactly as the status publication did. This asserts the two call
// sites genuinely share the shape rather than one having been fixed alone.
func TestBothEmittersBoundTheirSinkCall(t *testing.T) {
	// Arrange.
	sink := &wedgedSink{release: make(chan struct{})}
	defer close(sink.release)
	state := &stateEmitter{sink: sink, logf: t.Logf, bound: time.Millisecond}
	status := &statusEmitter{sink: sink, logf: t.Logf, bound: time.Millisecond}
	merged := &frontendv1.MergeStatus{
		RunId: "run-1",
		Phase: &frontendv1.MergeStatus_Merged{Merged: &frontendv1.MergeStatusMerged{}},
	}

	// Act.
	stateErr := state.emit("/ws/a", PhaseMerged, "landed")
	statusErr := status.emit("/ws/a", PhaseMerged, "landed", merged)

	// Assert.
	if stateErr == nil {
		t.Fatal("stateEmitter.emit() error = nil, want the wedged sink bounded")
	}
	if statusErr == nil {
		t.Fatal("statusEmitter.emit() error = nil, want the wedged sink bounded")
	}
}
