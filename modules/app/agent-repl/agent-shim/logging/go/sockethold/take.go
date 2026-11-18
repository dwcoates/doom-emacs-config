package sockethold

import (
	"context"
	"net"
	"time"
)

// TakeBound bounds taking one listener from a socket holder: the holder's
// start, when launchd is still spawning it (TakeMeasured at worst over five
// starts, the timer's resolution, at load average 3.9-4.4 on 2026-11-18
// 11:25), and the exchange itself (under 1ms). Every caller records each
// take's time at INFO, so the bound is re-sized from real takes.
const TakeBound = 30 * time.Millisecond

// TakeMeasured is the healthy worst take TakeBound was sized from.
const TakeMeasured = 10 * time.Millisecond

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
