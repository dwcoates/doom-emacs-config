package grantedclock

import (
	"context"
	"errors"
	"strings"
	"testing"
	"time"
)

// scriptedSource answers one cumulative reading per clock read: reads[0] is
// the clock's start, reads[i] its i-th Span, and the last entry repeats once
// the script runs out. Read number i fails with errs[i] when set.
//
// The clock reads the stall before the wall, so stall moves the script on and
// now answers the entry stall just moved to.
type scriptedSource struct {
	reads []reading
	errs  map[int]error
	n     int
	cur   reading
}

func (s *scriptedSource) stall() (time.Duration, error) {
	i := s.n
	s.n++
	s.cur = s.reads[min(i, len(s.reads)-1)]
	if err := s.errs[i]; err != nil {
		return 0, err
	}
	return s.cur.stall, nil
}

func (s *scriptedSource) now() time.Time { return time.Unix(0, 0).Add(s.cur.wall) }

// tick hands the bound's watcher one poll and returns once it has taken it.
// The poll is unbuffered, so the tick before it has been fully read.
func tick(poll chan<- time.Time) { poll <- time.Time{} }

func TestWithBoundEndsOnlyOnGrantedTime(t *testing.T) {
	tests := []struct {
		name      string
		reads     []reading
		ticks     int
		wantEnded bool
		wantSpan  Span
	}{
		{name: "a bound granted more than its time ends as a deadline",
			reads:     []reading{{}, {wall: time.Second}, {wall: 3 * time.Second}},
			ticks:     2,
			wantEnded: true, wantSpan: Span{Wall: 3 * time.Second, Granted: 3 * time.Second}},
		{name: "a bound past its time only in wall time keeps waiting",
			reads: []reading{{}, {wall: time.Minute, stall: time.Minute - time.Second}},
			ticks: 4},
		{name: "a bound granted exactly its time keeps waiting",
			reads: []reading{{}, {wall: 2 * time.Second}},
			ticks: 3},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			src := &scriptedSource{reads: tt.reads}
			poll := make(chan time.Time)
			ctx, cancel := withBoundOn(context.Background(), 2*time.Second, src.now, src.stall, poll)
			defer cancel()

			// Act
			for range tt.ticks {
				tick(poll)
			}

			// Assert
			if !tt.wantEnded {
				// One more tick proves the last one was fully read.
				tick(poll)
				select {
				case <-ctx.Done():
					t.Fatalf("the context ended (%v), want it still waiting", Explain(ctx))
				default:
				}
				return
			}
			<-ctx.Done()
			if !errors.Is(ctx.Err(), context.DeadlineExceeded) {
				t.Fatalf("Err() = %v, want context.DeadlineExceeded", ctx.Err())
			}
			var runOut *RunOutError
			if why := Explain(ctx); !errors.As(why, &runOut) || runOut.Span != tt.wantSpan || runOut.Bound != 2*time.Second {
				t.Fatalf("Explain() = %v, want the 2s bound run out at %+v", why, tt.wantSpan)
			}
		})
	}
}

func TestWithBoundEndsForEveryOtherCause(t *testing.T) {
	tests := []struct {
		name    string
		errs    map[int]error
		act     func(cancel, cancelParent context.CancelFunc, poll chan<- time.Time)
		wantErr error
		wantWhy string
	}{
		{name: "its cancel ends it as canceled",
			act:     func(cancel, _ context.CancelFunc, _ chan<- time.Time) { cancel() },
			wantErr: context.Canceled, wantWhy: "context canceled"},
		{name: "its parent's end ends it with the parent's error",
			act:     func(_, cancelParent context.CancelFunc, _ chan<- time.Time) { cancelParent() },
			wantErr: context.Canceled, wantWhy: "context canceled"},
		{name: "a clock that cannot be started ends it at once",
			errs:    map[int]error{0: errors.New("EPERM")},
			act:     func(context.CancelFunc, context.CancelFunc, chan<- time.Time) {},
			wantErr: context.Canceled, wantWhy: "read the CPU stall to start a granted clock: EPERM"},
		{name: "a clock that stops being readable ends it",
			errs:    map[int]error{1: errors.New("EIO")},
			act:     func(_, _ context.CancelFunc, poll chan<- time.Time) { tick(poll) },
			wantErr: context.Canceled, wantWhy: "read the CPU stall: EIO"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			src := &scriptedSource{reads: []reading{{}}, errs: tt.errs}
			poll := make(chan time.Time)
			parent, cancelParent := context.WithCancel(context.Background())
			defer cancelParent()
			ctx, cancel := withBoundOn(parent, time.Second, src.now, src.stall, poll)
			defer cancel()

			// Act
			tt.act(cancel, cancelParent, poll)

			// Assert
			<-ctx.Done()
			if !errors.Is(ctx.Err(), tt.wantErr) {
				t.Fatalf("Err() = %v, want %v", ctx.Err(), tt.wantErr)
			}
			if why := Explain(ctx); why == nil || !strings.Contains(why.Error(), tt.wantWhy) {
				t.Fatalf("Explain() = %v, want it to name %q", why, tt.wantWhy)
			}
		})
	}
}

func TestWithBoundExplainsAParentBoundThatRanOut(t *testing.T) {
	// Arrange: an inner bound with time to spare under a parent bound that
	// runs out.
	outerSrc := &scriptedSource{reads: []reading{{}, {wall: 3 * time.Second}}}
	outerPoll := make(chan time.Time)
	outer, cancelOuter := withBoundOn(context.Background(), 2*time.Second, outerSrc.now, outerSrc.stall, outerPoll)
	defer cancelOuter()
	innerSrc := &scriptedSource{reads: []reading{{}}}
	inner, cancelInner := withBoundOn(outer, time.Hour, innerSrc.now, innerSrc.stall, make(chan time.Time))
	defer cancelInner()

	// Act
	tick(outerPoll)

	// Assert
	<-inner.Done()
	var runOut *RunOutError
	if !errors.Is(inner.Err(), context.DeadlineExceeded) || !errors.As(Explain(inner), &runOut) || runOut.Bound != 2*time.Second {
		t.Fatalf("inner Err() = %v, Explain() = %v, want the parent's 2s bound run out", inner.Err(), Explain(inner))
	}
}

func TestWithBoundChildTimeoutEndsAsADeadline(t *testing.T) {
	// Arrange: a stdlib child with a wall timeout it will never reach.
	src := &scriptedSource{reads: []reading{{}, {wall: 3 * time.Second}}}
	poll := make(chan time.Time)
	ctx, cancel := withBoundOn(context.Background(), 2*time.Second, src.now, src.stall, poll)
	defer cancel()
	child, cancelChild := context.WithTimeout(ctx, time.Hour)
	defer cancelChild()

	// Act
	tick(poll)

	// Assert
	<-child.Done()
	if !errors.Is(child.Err(), context.DeadlineExceeded) {
		t.Fatalf("child Err() = %v, want context.DeadlineExceeded", child.Err())
	}
}

func TestWithBoundAnswersItsParentsDeadline(t *testing.T) {
	tests := []struct {
		name   string
		parent func() (context.Context, context.CancelFunc)
		wantOK bool
	}{
		{name: "a parent with no deadline gives it none",
			parent: func() (context.Context, context.CancelFunc) { return context.WithCancel(context.Background()) }},
		{name: "a parent's wall deadline is its own",
			parent: func() (context.Context, context.CancelFunc) {
				return context.WithTimeout(context.Background(), time.Hour)
			},
			wantOK: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			parent, cancelParent := tt.parent()
			defer cancelParent()
			src := &scriptedSource{reads: []reading{{}}}
			ctx, cancel := withBoundOn(parent, time.Second, src.now, src.stall, make(chan time.Time))
			defer cancel()

			// Act
			got, ok := ctx.Deadline()

			// Assert
			want, wantOK := parent.Deadline()
			if ok != tt.wantOK || ok != wantOK || !got.Equal(want) {
				t.Fatalf("Deadline() = %v, %v, want the parent's %v, %v", got, ok, want, wantOK)
			}
		})
	}
}

func TestExplainAnswersNothingForALiveBound(t *testing.T) {
	// Arrange
	src := &scriptedSource{reads: []reading{{}}}
	ctx, cancel := withBoundOn(context.Background(), time.Second, src.now, src.stall, make(chan time.Time))
	defer cancel()

	// Act
	why := Explain(ctx)

	// Assert
	if why != nil {
		t.Fatalf("Explain() = %v, want nil for a context that has not ended", why)
	}
}

func TestRunOutErrorNamesItsBoundAndSpan(t *testing.T) {
	// Arrange
	err := &RunOutError{Bound: 5 * time.Second, Span: Span{Wall: 9 * time.Second, Stalled: 4 * time.Second, Granted: 5 * time.Second}}

	// Act
	got := err.Error()

	// Assert
	want := "context deadline exceeded: the 5s granted bound ran out (5s granted of 9s wall, the processes it covers waiting 4s for a CPU)"
	if got != want || !errors.Is(err, context.DeadlineExceeded) {
		t.Fatalf("Error() = %q (Is deadline %v), want %q", got, errors.Is(err, context.DeadlineExceeded), want)
	}
}
