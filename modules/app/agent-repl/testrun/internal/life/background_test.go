package life

import (
	"context"
	"strings"
	"testing"
	"time"
)

// neverFires is an AFTER whose bound never passes.
func neverFires(time.Duration) <-chan time.Time { return nil }

// firesNow is an AFTER whose bound has already passed.
func firesNow(time.Duration) <-chan time.Time {
	c := make(chan time.Time, 1)
	c <- time.Time{}
	return c
}

func TestBackgroundStopReturnsOnlyOnceEveryLoopHasReturned(t *testing.T) {
	// Arrange: a loop that is mid-step when the cancellation comes, and
	// finishes the step only when the test lets it.
	log := &recLog{}
	b := NewBackground(context.Background(), JoinBound, neverFires, log)
	inStep, finishStep, returned := make(chan struct{}), make(chan struct{}), make(chan struct{})
	b.Go("a stepping loop", func(ctx context.Context) {
		close(inStep)
		<-ctx.Done()
		<-finishStep
		close(returned)
	})
	<-inStep
	stopped := make(chan error, 1)

	// Act.
	go func() { stopped <- b.Stop() }()
	close(finishStep)
	err := <-stopped

	// Assert: Stop answered after the loop returned, not at its cancellation.
	select {
	case <-returned:
	default:
		t.Fatal("Stop returned while the loop was still running")
	}
	if err != nil {
		t.Fatalf("Stop() = %v, want nil", err)
	}
	if len(log.errs) != 0 {
		t.Fatalf("logged %q, want no error", log.errs)
	}
}

func TestBackgroundCancelsEveryLoopsContext(t *testing.T) {
	// Arrange.
	b := NewBackground(context.Background(), JoinBound, neverFires, &recLog{})
	cancelled := make(chan struct{}, 2)
	for _, name := range []string{"first", "second"} {
		b.Go(name, func(ctx context.Context) {
			<-ctx.Done()
			cancelled <- struct{}{}
		})
	}

	// Act.
	err := b.Stop()

	// Assert.
	if err != nil {
		t.Fatalf("Stop() = %v, want nil", err)
	}
	if len(cancelled) != 2 {
		t.Fatalf("%d loops saw their cancellation, want 2", len(cancelled))
	}
}

func TestBackgroundStopWithNoLoopsAnswersAtOnce(t *testing.T) {
	// Arrange.
	log := &recLog{}
	b := NewBackground(context.Background(), JoinBound, neverFires, log)

	// Act.
	err := b.Stop()

	// Assert.
	if err != nil || len(log.errs) != 0 {
		t.Fatalf("Stop() = %v, logged %q; want nil and no error", err, log.errs)
	}
}

func TestBackgroundStopFailsAndLogsALoopStillRunningAtItsBound(t *testing.T) {
	// Arrange: a loop that ignores its cancellation.
	log := &recLog{}
	b := NewBackground(context.Background(), JoinBound, firesNow, log)
	release := make(chan struct{})
	t.Cleanup(func() { close(release) })
	b.Go("the CPU governor", func(context.Context) { <-release })

	// Act.
	err := b.Stop()

	// Assert: the error and its one log record name the stuck loop, the
	// bound and the measured step.
	if err == nil {
		t.Fatal("Stop() = nil, want an error for the loop past its bound")
	}
	if len(log.errs) != 1 {
		t.Fatalf("logged %d errors %q, want 1", len(log.errs), log.errs)
	}
	rec := log.errs[0]
	for _, want := range []string{"join the run's background loops before its reap", "the CPU governor still running", JoinBound.String(), slowestStep.String(), "the run fails"} {
		if !strings.Contains(rec, want) {
			t.Errorf("log %q lacks %q", rec, want)
		}
	}
	if !strings.Contains(err.Error(), "the CPU governor") {
		t.Errorf("error %q does not name the stuck loop", err)
	}
}
