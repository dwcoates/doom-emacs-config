package sockethold

import (
	"context"
	"net"
	"time"
)

// TakeBound bounds taking one listener from a socket holder, and probing
// one: the holder's start, when launchd is still spawning it, and the
// exchange itself. Sized at about two and a half times TakeMeasured, the
// healthy worst of 674 takes and 674 probes across the daemon's integration
// package on 2026-11-18, at load average up to 17.5 (takes: p50 5.6ms, p99
// 42ms, worst 67ms; probes: p50 6.1ms, p99 50ms, worst 101ms). Every caller
// records each take's time at INFO, so the bound is re-sized from real takes.
const TakeBound = 250 * time.Millisecond

// TakeMeasured is the healthy worst take or probe TakeBound was sized from.
const TakeMeasured = 101 * time.Millisecond

// TakeFunc is Take's shape: a caller's seam takes a stand-in of it.
type TakeFunc func(ctx context.Context, control, name string) (net.Listener, error)

// TakeTimed runs TAKE (Take, or a test's stand-in) for the socket NAME at
// CONTROL within TakeBound, and answers how long it took, for the caller to
// record through its own log.
func TakeTimed(take TakeFunc, control, name string) (net.Listener, time.Duration, error) {
	ctx, cancel := context.WithTimeout(context.Background(), TakeBound)
	defer cancel()
	began := time.Now()
	ln, err := take(ctx, control, name)
	return ln, time.Since(began), err
}

// ProbeFunc is Probe's shape: a caller's seam takes a stand-in of it.
type ProbeFunc func(ctx context.Context, control string) error

// ProbeTimed runs PROBE (Probe, or a test's stand-in) of CONTROL within
// TakeBound, the same holder start and exchange a take waits on, and answers
// how long it took, for the caller to record through its own log.
func ProbeTimed(probe ProbeFunc, control string) (time.Duration, error) {
	ctx, cancel := context.WithTimeout(context.Background(), TakeBound)
	defer cancel()
	began := time.Now()
	err := probe(ctx, control)
	return time.Since(began), err
}
