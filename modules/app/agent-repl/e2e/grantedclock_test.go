package e2e

import (
	"errors"
	"fmt"
	"os"
	"strconv"
	"strings"
	"time"
)

// THE GRANTED CLOCK, and why the Emacs layer's bounds are kept on it.
//
// A bound on how long Emacs takes to answer is a claim about EMACS. Wall time
// is not Emacs's time when the container is short of CPU: the layer runs two
// Emacsen in a container capped to two CPUs, each with its own WebKit, Xvfb,
// daemon and shim, so a busy moment in one scenario leaves the other's
// processes runnable with nowhere to run. MEASURED, merge gate 2026-10-20: a
// heartbeat probe went unanswered for 3.196s while the container was
// throttled in 32 of 32 periods, and Emacs's main thread sat in
// XGetWindowProperty waiting on an Xvfb that had no CPU to answer it; a second
// gate killed a registry await's eval while Emacs itself sat in state R. Both
// were reported as Emacs's failure.
//
// The granted clock charges only the time the container was not starved: wall
// time less the time some task in the container sat runnable without a CPU.
// The kernel keeps exactly that number for the container's cgroup -- the
// cumulative "some" stall of its CPU pressure (PSI) -- and it counts every
// cause alike: the container's own quota throttling it, and a Docker VM too
// busy to give it the quota at all. MEASURED on the sandbox's kernel
// (7.0.12-linuxkit), three spinners under a 0.5-CPU cap: "some" grew 2.53s of
// a 3.0s wall.
//
// IT IS THE CONTAINER'S STALL, NOT ONE PROCESS'S, BY DESIGN. An answer from
// Emacs can wait on any process of the scenario -- Xvfb for every X request,
// WebKit, the daemon, the shim -- and a stall charged per process would have
// to name every one of them to never charge starvation to Emacs. The union
// over the whole container cannot miss one. What it costs is only the other
// direction: a real hang is reported later while the container is saturated,
// never sooner, and never not at all once it is not.
const cgroupCPUPressurePath = "/sys/fs/cgroup/cpu.pressure"

// containerCPUStall is the container's cumulative CPU stall: the total time
// some task in it was runnable without a CPU.
func containerCPUStall() (time.Duration, error) {
	body, err := os.ReadFile(cgroupCPUPressurePath)
	if err != nil {
		return 0, err
	}
	stall, err := parseCPUPressureSome(string(body))
	if err != nil {
		return 0, fmt.Errorf("%s: %w", cgroupCPUPressurePath, err)
	}
	return stall, nil
}

// parseCPUPressureSome reads the cumulative "some" total, in microseconds,
// out of a PSI pressure file:
//
//	some avg10=0.00 avg60=0.00 avg300=0.00 total=160255
//	full avg10=0.00 avg60=0.00 avg300=0.00 total=160255
func parseCPUPressureSome(body string) (time.Duration, error) {
	for _, line := range strings.Split(strings.TrimSpace(body), "\n") {
		fields := strings.Fields(line)
		if len(fields) == 0 || fields[0] != "some" {
			continue
		}
		for _, f := range fields[1:] {
			raw, ok := strings.CutPrefix(f, "total=")
			if !ok {
				continue
			}
			usec, err := strconv.ParseInt(raw, 10, 64)
			if err != nil {
				return 0, fmt.Errorf("the some line's total %q: %w", raw, err)
			}
			return time.Duration(usec) * time.Microsecond, nil
		}
		return 0, fmt.Errorf("the some line carries no total: %q", line)
	}
	return 0, fmt.Errorf("no some line in %q", body)
}

// grantedSpan is one interval read on the granted clock: its wall time, and
// how much of it some task in the container sat runnable without a CPU.
type grantedSpan struct {
	Wall, Stalled time.Duration
}

// Granted is the interval's wall time less its stall. The two are read from
// different clocks (CLOCK_MONOTONIC and the scheduler's), so a fully stalled
// interval can read a few microseconds negative; that skew reads as zero.
func (s grantedSpan) Granted() time.Duration {
	if g := s.Wall - s.Stalled; g > 0 {
		return g
	}
	return 0
}

func (s grantedSpan) String() string {
	return fmt.Sprintf("%s granted of %s wall, %s of it with a container task waiting for a CPU",
		s.Granted().Round(time.Millisecond), s.Wall.Round(time.Millisecond), s.Stalled.Round(time.Millisecond))
}

// grantedClock measures one interval on the granted clock from the instant
// it was started.
type grantedClock struct {
	now     func() time.Time
	stall   func() (time.Duration, error)
	started time.Time
	stall0  time.Duration
}

// startGrantedClock starts a clock reading wall time from NOW and the
// container's cumulative stall from STALL.
func startGrantedClock(now func() time.Time, stall func() (time.Duration, error)) (*grantedClock, error) {
	stall0, err := stall()
	if err != nil {
		return nil, fmt.Errorf("read the container's CPU stall to start a granted clock: %w", err)
	}
	return &grantedClock{now: now, stall: stall, started: now(), stall0: stall0}, nil
}

// span reads the interval since the clock started.
func (c *grantedClock) span() (grantedSpan, error) {
	stall, err := c.stall()
	if err != nil {
		return grantedSpan{}, fmt.Errorf("read the container's CPU stall: %w", err)
	}
	return grantedSpan{Wall: c.now().Sub(c.started), Stalled: stall - c.stall0}, nil
}

// grantedReader is what awaitGranted reads its clock through.
type grantedReader interface {
	span() (grantedSpan, error)
}

// awaitGranted waits for ANSWER, looking at the clock on each POLL, and gives
// up once the clock has granted more than BOUND -- answering
// errGrantedBoundRunOut -- or once the clock cannot be read. ANSWERED says
// whether the answer was taken, so a caller whose wait gave up knows the
// answer is still to come and must be drained.
//
// AN ANSWER THAT ARRIVED WITH THE TICK WINS: it is taken before the clock is
// read, so a call is never failed for a bound it beat.
func awaitGranted[T any](answer <-chan T, poll <-chan time.Time, clock grantedReader, bound time.Duration) (a T, answered bool, span grantedSpan, err error) {
	for {
		select {
		case a = <-answer:
			span, err = clock.span()
			return a, true, span, err
		case <-poll:
		}
		select {
		case a = <-answer:
			span, err = clock.span()
			return a, true, span, err
		default:
		}
		if span, err = clock.span(); err != nil {
			return a, false, span, err
		}
		if span.Granted() > bound {
			return a, false, span, errGrantedBoundRunOut
		}
	}
}

// pollUntilGranted calls ATTEMPT, and again on each POLL, until it reports
// done -- answering how long that took on the clock -- or until the clock has
// granted more than BOUND (errGrantedBoundRunOut) or cannot be read. A bound
// that runs out is only ever noticed after an attempt has failed, so a wait
// whose last attempt succeeds is never failed for its timing.
func pollUntilGranted(poll <-chan time.Time, clock grantedReader, bound time.Duration, attempt func() bool) (grantedSpan, error) {
	for {
		if attempt() {
			return clock.span()
		}
		span, err := clock.span()
		if err != nil {
			return span, err
		}
		if span.Granted() > bound {
			return span, errGrantedBoundRunOut
		}
		<-poll
	}
}

// errGrantedBoundRunOut is awaitGranted's answer for a wait whose clock
// granted more than its bound.
var errGrantedBoundRunOut = errors.New("the granted bound ran out")
