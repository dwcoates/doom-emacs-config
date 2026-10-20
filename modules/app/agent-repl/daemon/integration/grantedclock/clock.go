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
	"time"
)

// Stall reads a cumulative CPU stall: the total time the processes it covers
// sat runnable without a CPU, since some fixed origin.
type Stall func() (time.Duration, error)

// Span is one interval read on the granted clock: its wall time, and how much
// of it the covered processes sat runnable without a CPU.
type Span struct {
	Wall, Stalled time.Duration
}

// Granted is the interval's wall time less its stall. The two are read from
// different clocks (CLOCK_MONOTONIC and the scheduler's), so a fully stalled
// interval can read a few microseconds negative; that skew reads as zero.
func (s Span) Granted() time.Duration {
	if g := s.Wall - s.Stalled; g > 0 {
		return g
	}
	return 0
}

func (s Span) String() string {
	return fmt.Sprintf("%s granted of %s wall, %s of it with a container task waiting for a CPU",
		s.Granted().Round(time.Millisecond), s.Wall.Round(time.Millisecond), s.Stalled.Round(time.Millisecond))
}

// Clock measures one interval on the granted clock from the instant it was
// started.
type Clock struct {
	now     func() time.Time
	stall   Stall
	started time.Time
	stall0  time.Duration
}

// Start starts a clock reading wall time from NOW and the cumulative stall
// from STALL.
func Start(now func() time.Time, stall Stall) (*Clock, error) {
	stall0, err := stall()
	if err != nil {
		return nil, fmt.Errorf("read the container's CPU stall to start a granted clock: %w", err)
	}
	return &Clock{now: now, stall: stall, started: now(), stall0: stall0}, nil
}

// Span reads the interval since the clock started.
func (c *Clock) Span() (Span, error) {
	stall, err := c.stall()
	if err != nil {
		return Span{}, fmt.Errorf("read the container's CPU stall: %w", err)
	}
	return Span{Wall: c.now().Sub(c.started), Stalled: stall - c.stall0}, nil
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
		if span.Granted() > bound {
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
		if span.Granted() > bound {
			return span, ErrBoundRunOut
		}
		<-poll
	}
}

// ErrBoundRunOut is Await's and PollUntil's answer for a wait whose clock
// granted more than its bound.
var ErrBoundRunOut = errors.New("the granted bound ran out")
