package clock_test

import (
	"context"
	"errors"
	"testing"
	"time"

	"agent-repld/internal/clock"
	"agent-repld/internal/clock/clocktest"
)

// repeatKey keys the value a test hands Repeat through its context.
type repeatKey struct{}

// repeating is one Repeat on its own goroutine, counting its runs.
type repeating struct {
	cancel   context.CancelFunc
	runs     chan struct{}
	finished chan error
}

func startRepeat(t *testing.T, c clock.Clock, first, every time.Duration) *repeating {
	t.Helper()
	ctx, cancel := context.WithCancel(context.Background())
	r := &repeating{cancel: cancel, runs: make(chan struct{}, 16), finished: make(chan error, 1)}
	go func() {
		r.finished <- clock.Repeat(ctx, c, first, every, func(context.Context) { r.runs <- struct{}{} })
	}()
	t.Cleanup(cancel)
	return r
}

func TestRepeatWaitsFirstBeforeTheFirstRun(t *testing.T) {
	// Arrange.
	c := clocktest.New(time.Time{})

	// Act.
	r := startRepeat(t, c, time.Minute, time.Hour)
	got := <-c.Asked

	// Assert.
	if got != time.Minute {
		t.Fatalf("the first wait = %v, want the first window", got)
	}
	select {
	case <-r.runs:
		t.Fatal("fn ran before its first wait fired")
	default:
	}
}

func TestRepeatRunsThenWaitsEvery(t *testing.T) {
	// Arrange.
	c := clocktest.New(time.Time{})
	r := startRepeat(t, c, time.Minute, time.Hour)
	<-c.Asked

	// Act.
	c.Fire <- time.Time{}
	<-r.runs
	second := <-c.Asked
	c.Fire <- time.Time{}
	<-r.runs
	third := <-c.Asked

	// Assert.
	if second != time.Hour || third != time.Hour {
		t.Fatalf("the later waits = %v, %v, want the cadence", second, third)
	}
}

func TestRepeatEndsWithItsContext(t *testing.T) {
	// Arrange.
	c := clocktest.New(time.Time{})
	r := startRepeat(t, c, time.Minute, time.Hour)
	<-c.Asked

	// Act.
	r.cancel()
	err := <-r.finished

	// Assert.
	if !errors.Is(err, context.Canceled) {
		t.Fatalf("Repeat = %v, want context.Canceled", err)
	}
	if len(r.runs) != 0 {
		t.Fatal("fn ran after the context ended")
	}
}

func TestRepeatHandsFnItsContext(t *testing.T) {
	// Arrange.
	c := clocktest.New(time.Time{})
	ctx, cancel := context.WithCancel(context.WithValue(context.Background(), repeatKey{}, "mine"))
	defer cancel()
	got := make(chan any, 1)
	go clock.Repeat(ctx, c, 0, time.Hour, func(ctx context.Context) { got <- ctx.Value(repeatKey{}) })
	<-c.Asked

	// Act.
	c.Fire <- time.Time{}

	// Assert.
	if v := <-got; v != "mine" {
		t.Fatalf("fn's context carried %v, want the caller's", v)
	}
}
