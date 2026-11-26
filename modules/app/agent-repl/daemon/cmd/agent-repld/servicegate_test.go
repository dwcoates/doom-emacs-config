package main

import (
	"context"
	"errors"
	"testing"
)

func isClosed(ch <-chan struct{}) bool {
	select {
	case <-ch:
		return true
	default:
		return false
	}
}

func TestServiceGateIsShutWhileTheServiceStepRuns(t *testing.T) {
	// Arrange
	gate := newServiceGate()
	var openDuringStep bool
	step := gate.step(func(context.Context) error {
		openDuringStep = isClosed(gate.Ready())
		return nil
	})

	// Act
	_ = step(context.Background())

	// Assert
	if openDuringStep {
		t.Fatal("the gate was open while the service step was still running")
	}
}

func TestServiceGateOpensOnceTheServiceStepReturns(t *testing.T) {
	// Arrange
	gate := newServiceGate()
	step := gate.step(func(context.Context) error { return nil })

	// Act
	err := step(context.Background())

	// Assert
	if err != nil || !isClosed(gate.Ready()) {
		t.Fatalf("step = %v, open = %v; want nil and an open gate", err, isClosed(gate.Ready()))
	}
}

func TestServiceGateOpensOnAFailedServiceStepAndKeepsItsError(t *testing.T) {
	// Arrange
	gate := newServiceGate()
	failure := errors.New("the store would not restart")
	step := gate.step(func(context.Context) error { return failure })

	// Act
	err := step(context.Background())

	// Assert
	if !errors.Is(err, failure) || !isClosed(gate.Ready()) {
		t.Fatalf("step = %v, open = %v; want the failure and an open gate", err, isClosed(gate.Ready()))
	}
}

func TestServiceGateSurvivesASecondStep(t *testing.T) {
	// Arrange
	gate := newServiceGate()
	step := gate.step(func(context.Context) error { return nil })
	_ = step(context.Background())

	// Act
	err := step(context.Background())

	// Assert
	if err != nil {
		t.Fatalf("a second step = %v, want nil", err)
	}
}

func TestBootServiceStepMakesTheServicesCurrentOnAnOrdinaryBoot(t *testing.T) {
	// Arrange
	var called string
	loaded := func(context.Context) error { called = "loaded"; return nil }
	current := func(context.Context) error { called = "current"; return nil }

	// Act
	_ = bootServiceStep(false, loaded, current)(context.Background())

	// Assert
	if called != "current" {
		t.Fatalf("an ordinary boot ran %q, want current", called)
	}
}

func TestBootServiceStepOnlyEnsuresTheServicesForAJoiningSuccessor(t *testing.T) {
	// Arrange
	var called string
	loaded := func(context.Context) error { called = "loaded"; return nil }
	current := func(context.Context) error { called = "current"; return nil }

	// Act
	_ = bootServiceStep(true, loaded, current)(context.Background())

	// Assert
	if called != "loaded" {
		t.Fatalf("a joining successor ran %q, want loaded", called)
	}
}

func TestSidecarStepRunsTheHalfTheStoreHalfAnswered(t *testing.T) {
	// Arrange
	var ran bool
	step := &sidecarStep{}
	_ = step.storeCurrent(func(context.Context) (func(context.Context) error, error) {
		return func(context.Context) error { ran = true; return nil }, nil
	})(context.Background())

	// Act
	err := step.run(context.Background())

	// Assert
	if err != nil || !ran {
		t.Fatalf("run = %v, ran = %v; want the recorded half run", err, ran)
	}
}

func TestSidecarStepWithNoStoreHalfIsNothingToDo(t *testing.T) {
	// Arrange: a joining successor never runs the store half.
	step := &sidecarStep{}

	// Act
	err := step.run(context.Background())

	// Assert
	if err != nil {
		t.Fatalf("run = %v, want nil", err)
	}
}

func TestSidecarStepAnswersTheStoreHalfsFailure(t *testing.T) {
	// Arrange
	failure := errors.New("store")
	step := &sidecarStep{}

	// Act
	err := step.storeCurrent(func(context.Context) (func(context.Context) error, error) {
		return nil, failure
	})(context.Background())

	// Assert
	if !errors.Is(err, failure) || step.run(context.Background()) != nil {
		t.Fatalf("store half = %v; want its failure and no sidecar half left to run", err)
	}
}

func TestSidecarStepSurfacesTheSidecarHalfsFailure(t *testing.T) {
	// Arrange
	failure := errors.New("kickstart")
	step := &sidecarStep{}
	_ = step.storeCurrent(func(context.Context) (func(context.Context) error, error) {
		return func(context.Context) error { return failure }, nil
	})(context.Background())

	// Act
	err := step.run(context.Background())

	// Assert
	if !errors.Is(err, failure) {
		t.Fatalf("run = %v, want the sidecar half's failure", err)
	}
}

func TestAfterGateDoesNotRunBeforeTheGateOpens(t *testing.T) {
	// Arrange
	gate := newServiceGate()
	started := make(chan struct{}, 1)
	ctx, cancel := context.WithCancel(context.Background())
	wrapped := afterGate(gate, func(context.Context) error { started <- struct{}{}; return nil })
	done := make(chan error, 1)
	go func() { done <- wrapped(ctx) }()

	// Act: the run is cancelled while the gate is still shut, so it returns
	// only after deciding not to run.
	cancel()
	err := <-done

	// Assert
	if err != nil || len(started) != 0 {
		t.Fatalf("wrapped = %v, started = %d; want nil and no run before the gate opened", err, len(started))
	}
}

func TestAfterGateRunsOnceTheGateOpens(t *testing.T) {
	stepFailure := errors.New("the holder could not be installed")
	cases := []struct {
		name    string
		stepErr error
	}{
		{name: "after a successful step", stepErr: nil},
		{name: "after a failed step", stepErr: stepFailure},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			gate := newServiceGate()
			release := make(chan struct{})
			step := gate.step(func(context.Context) error { <-release; return tc.stepErr })
			runResult := errors.New("the run's own answer")
			wrapped := afterGate(gate, func(context.Context) error { return runResult })
			done := make(chan error, 1)
			go func() { done <- wrapped(context.Background()) }()
			stepDone := make(chan error, 1)
			go func() { stepDone <- step(context.Background()) }()

			// Act
			close(release)
			err := <-done

			// Assert
			if !errors.Is(err, runResult) {
				t.Fatalf("wrapped = %v, want the run's own answer", err)
			}
			if got := <-stepDone; !errors.Is(got, tc.stepErr) {
				t.Fatalf("step = %v, want %v", got, tc.stepErr)
			}
		})
	}
}

func TestAfterGateRunsAtOnceWhenTheGateIsAlreadyOpen(t *testing.T) {
	// Arrange
	gate := newServiceGate()
	_ = gate.step(func(context.Context) error { return nil })(context.Background())
	var ran bool
	wrapped := afterGate(gate, func(context.Context) error { ran = true; return nil })

	// Act
	err := wrapped(context.Background())

	// Assert
	if err != nil || !ran {
		t.Fatalf("wrapped = %v, ran = %v; want the run run", err, ran)
	}
}
