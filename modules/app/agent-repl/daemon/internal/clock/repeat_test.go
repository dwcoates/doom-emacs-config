package clock

import (
	"context"
	"errors"
	"testing"
	"time"
)

// scripted is a Clock that reports every wait asked of it and fires only when
// the test says so.
type scripted struct {
	asked chan time.Duration
	fire  chan time.Time
}

func newScripted() *scripted {
	return &scripted{asked: make(chan time.Duration, 16), fire: make(chan time.Time)}
}

func (s *scripted) Now() time.Time { return time.Time{} }

func (s *scripted) After(d time.Duration) <-chan time.Time {
	s.asked <- d
	return s.fire
}

// repeating is one Repeat on its own goroutine, counting its runs.
type repeating struct {
	cancel   context.CancelFunc
	runs     chan struct{}
	finished chan error
}

func startRepeat(t *testing.T, c Clock, first, every time.Duration) *repeating {
	t.Helper()
	ctx, cancel := context.WithCancel(context.Background())
	r := &repeating{cancel: cancel, runs: make(chan struct{}, 16), finished: make(chan error, 1)}
	go func() {
		r.finished <- Repeat(ctx, c, first, every, func(context.Context) { r.runs <- struct{}{} })
	}()
	t.Cleanup(cancel)
	return r
}

func TestRepeatWaitsFirstBeforeTheFirstRun(t *testing.T) {
	// Arrange.
	c := newScripted()

	// Act.
	r := startRepeat(t, c, time.Minute, time.Hour)
	got := <-c.asked

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
	c := newScripted()
	r := startRepeat(t, c, time.Minute, time.Hour)
	<-c.asked

	// Act.
	c.fire <- time.Time{}
	<-r.runs
	second := <-c.asked
	c.fire <- time.Time{}
	<-r.runs
	third := <-c.asked

	// Assert.
	if second != time.Hour || third != time.Hour {
		t.Fatalf("the later waits = %v, %v, want the cadence", second, third)
	}
}

func TestRepeatEndsWithItsContext(t *testing.T) {
	// Arrange.
	c := newScripted()
	r := startRepeat(t, c, time.Minute, time.Hour)
	<-c.asked

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
	c := newScripted()
	ctx, cancel := context.WithCancel(context.WithValue(context.Background(), scripted{}, "mine"))
	defer cancel()
	got := make(chan any, 1)
	go Repeat(ctx, c, 0, time.Hour, func(ctx context.Context) { got <- ctx.Value(scripted{}) })
	<-c.asked

	// Act.
	c.fire <- time.Time{}

	// Assert.
	if v := <-got; v != "mine" {
		t.Fatalf("fn's context carried %v, want the caller's", v)
	}
}
