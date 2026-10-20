// Package grantedclock keeps a test layer's failure bounds on the GRANTED
// CLOCK: wall time less the time the processes a wait depends on sat runnable
// with no CPU to run on.
//
// A bound on how long a process takes to answer is a claim about THAT
// PROCESS. Wall time is not its time when the host is short of CPU: a full
// test run puts dozens of test binaries, daemons and shims on one machine, so
// a busy moment leaves a boot runnable with nowhere to run, and a wall-clock
// bound then reports the host's saturation as the process's failure. The
// granted clock charges a bound only the time the waited-on processes were
// not starved, so a starved boot is waited out and a real hang -- a process
// with nothing runnable -- is still reported once its bound of granted time
// has run.
package grantedclock

import (
	"errors"
	"fmt"
	"sync"
	"time"
)

// Stall reads a cumulative CPU stall: the total time the processes it covers
// sat runnable without a CPU, since some fixed origin.
type Stall func() (time.Duration, error)

// Span is one interval read on the granted clock: its wall time, how long
// the covered processes sat runnable without a CPU in it, and how much of it
// was granted.
//
// STALLED CAN EXCEED WALL. A source that sums its processes' stalls (the
// darwin process tree) counts two processes starved at once twice, so Granted
// is never derived from the interval's two totals: it is accrued reading by
// reading, each reading granting its own wall time less its own stall and
// never less than nothing. A starved stretch therefore grants nothing without
// also cancelling the granted time around it.
type Span struct {
	Wall, Stalled, Granted time.Duration
}

func (s Span) String() string {
	return fmt.Sprintf("%s granted of %s wall, the processes it covers waiting %s for a CPU",
		s.Granted.Round(time.Millisecond), s.Wall.Round(time.Millisecond), s.Stalled.Round(time.Millisecond))
}

// Clock measures one interval on the granted clock from the instant it was
// started. It is safe for concurrent readers: a bound context reads it on its
// own goroutine while its waiter may read it too.
type Clock struct {
	now   func() time.Time
	stall Stall

	mu        sync.Mutex
	lastWall  time.Time
	lastStall time.Duration
	span      Span
}

// Start starts a clock reading wall time from NOW and the cumulative stall
// from STALL.
func Start(now func() time.Time, stall Stall) (*Clock, error) {
	stall0, err := stall()
	if err != nil {
		return nil, fmt.Errorf("read the CPU stall to start a granted clock: %w", err)
	}
	return &Clock{now: now, stall: stall, lastWall: now(), lastStall: stall0}, nil
}

// Span reads the interval since the clock started, accruing the granted time
// since the last reading.
//
// A CUMULATIVE STALL NEVER SHRINKS. A reading below the last one is a source
// that lost count, and granting on it would hand the bound time the processes
// never had, so it fails the reading.
func (c *Clock) Span() (Span, error) {
	c.mu.Lock()
	defer c.mu.Unlock()
	stall, err := c.stall()
	if err != nil {
		return c.span, fmt.Errorf("read the CPU stall: %w", err)
	}
	if stall < c.lastStall {
		return c.span, fmt.Errorf("the CPU stall went backwards, from %s to %s", c.lastStall, stall)
	}
	now := c.now()
	wall, stalled := now.Sub(c.lastWall), stall-c.lastStall
	c.span.Wall += wall
	c.span.Stalled += stalled
	c.span.Granted += max(wall-stalled, 0)
	c.lastWall, c.lastStall = now, stall
	return c.span, nil
}

// Reader is what Await and PollUntil read their clock through.
type Reader interface {
	Span() (Span, error)
}

// Await waits for ANSWER, looking at the clock on each POLL, and gives up
// once the clock has granted more than BOUND -- answering ErrBoundRunOut -- or
// once the clock cannot be read. ANSWERED says whether the answer was taken,
// so a caller whose wait gave up knows the answer is still to come and must be
// drained.
//
// AN ANSWER THAT ARRIVED WITH THE TICK WINS: it is taken before the clock is
// read, so a call is never failed for a bound it beat.
func Await[T any](answer <-chan T, poll <-chan time.Time, clock Reader, bound time.Duration) (a T, answered bool, span Span, err error) {
	for {
		select {
		case a = <-answer:
			span, err = clock.Span()
			return a, true, span, err
		case <-poll:
		}
		select {
		case a = <-answer:
			span, err = clock.Span()
			return a, true, span, err
		default:
		}
		if span, err = clock.Span(); err != nil {
			return a, false, span, err
		}
		if span.Granted > bound {
			return a, false, span, ErrBoundRunOut
		}
	}
}

// PollUntil calls ATTEMPT, and again on each POLL, until it reports done --
// answering how long that took on the clock -- or until the clock has granted
// more than BOUND (ErrBoundRunOut) or cannot be read. A bound that runs out is
// only ever noticed after an attempt has failed, so a wait whose last attempt
// succeeds is never failed for its timing.
func PollUntil(poll <-chan time.Time, clock Reader, bound time.Duration, attempt func() bool) (Span, error) {
	for {
		if attempt() {
			return clock.Span()
		}
		span, err := clock.Span()
		if err != nil {
			return span, err
		}
		if span.Granted > bound {
			return span, ErrBoundRunOut
		}
		<-poll
	}
}

// ErrBoundRunOut is Await's and PollUntil's answer for a wait whose clock
// granted more than its bound.
var ErrBoundRunOut = errors.New("the granted bound ran out")
