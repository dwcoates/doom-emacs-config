package grantedclock

import (
	"context"
	"fmt"
	"sync"
	"time"
)

// Tick is how often a bound context reads its clock: how late past its bound
// a run-out is noticed, and how often a darwin process tree is read.
const Tick = 10 * time.Millisecond

// WithBound answers a context that ends once the granted clock on this
// binary's Source has granted more than BOUND -- with Err
// context.DeadlineExceeded, exactly as a wall timeout ends -- or once PARENT
// ends or the cancel is called.
//
// IT HAS NO WALL DEADLINE of its own: Deadline answers PARENT's. A consumer
// that needs to know when to stop waits on Done, never on a deadline read off
// the context.
//
// A clock that cannot be read ends the context with context.Canceled, at once
// when it cannot be started; Explain names the read that failed.
func WithBound(parent context.Context, bound time.Duration) (context.Context, context.CancelFunc) {
	stall, err := Source()
	if err != nil {
		return withBoundOn(parent, bound, time.Now, func() (time.Duration, error) { return 0, err }, nil)
	}
	return withBoundOn(parent, bound, time.Now, stall, nil)
}

// withBoundOn is WithBound on a clock reading wall time from NOW and the
// stall from STALL, read on each POLL (a Tick ticker when nil).
func withBoundOn(parent context.Context, bound time.Duration, now func() time.Time, stall Stall, poll <-chan time.Time) (context.Context, context.CancelFunc) {
	c := &boundCtx{parent: parent, bound: bound, done: make(chan struct{})}
	cancel := func() { c.end(context.Canceled, context.Canceled) }
	clock, err := Start(now, stall)
	if err != nil {
		c.end(context.Canceled, err)
		return c, cancel
	}
	go c.watch(clock, poll)
	return c, cancel
}

// boundCtx is WithBound's context.
type boundCtx struct {
	parent context.Context
	bound  time.Duration
	done   chan struct{}

	once sync.Once
	mu   sync.Mutex
	err  error
	// why is Explain's answer once the context has ended.
	why error
}

// watch ends the context on the first of: its bound running out, its clock
// failing, its parent ending, or its cancel. It reads the clock on each POLL,
// or on its own Tick ticker when POLL is nil.
func (c *boundCtx) watch(clock *Clock, poll <-chan time.Time) {
	if poll == nil {
		ticker := time.NewTicker(Tick)
		defer ticker.Stop()
		poll = ticker.C
	}
	for {
		select {
		case <-c.done:
			return
		case <-c.parent.Done():
			c.end(c.parent.Err(), Explain(c.parent))
			return
		case <-poll:
		}
		span, err := clock.Span()
		if err != nil {
			c.end(context.Canceled, err)
			return
		}
		if span.Granted > c.bound {
			c.end(context.DeadlineExceeded, &RunOutError{Bound: c.bound, Span: span})
			return
		}
	}
}

// end ends the context once, with ERR as its Err and WHY as its explanation.
func (c *boundCtx) end(err, why error) {
	c.once.Do(func() {
		c.mu.Lock()
		c.err, c.why = err, why
		c.mu.Unlock()
		close(c.done)
	})
}

func (c *boundCtx) Deadline() (time.Time, bool) { return c.parent.Deadline() }
func (c *boundCtx) Done() <-chan struct{}       { return c.done }
func (c *boundCtx) Value(key any) any           { return c.parent.Value(key) }

func (c *boundCtx) Err() error {
	c.mu.Lock()
	defer c.mu.Unlock()
	return c.err
}

func (c *boundCtx) String() string {
	return fmt.Sprintf("grantedclock.WithBound(%v, %s)", c.parent, c.bound)
}

// Explain answers why CTX ended: for a WithBound context, the bound that ran
// out with its span or the clock read that failed; for any other, its Err.
// It answers nil for a context that has not ended.
func Explain(ctx context.Context) error {
	c, ok := ctx.(*boundCtx)
	if !ok {
		return ctx.Err()
	}
	c.mu.Lock()
	defer c.mu.Unlock()
	return c.why
}

// RunOutError is the explanation of a WithBound context whose bound ran out.
// It is a context.DeadlineExceeded.
type RunOutError struct {
	Bound time.Duration
	Span  Span
}

func (e *RunOutError) Error() string {
	return fmt.Sprintf("%v: the %s granted bound ran out (%s)", context.DeadlineExceeded, e.Bound, e.Span)
}

func (e *RunOutError) Is(target error) bool { return target == context.DeadlineExceeded }
