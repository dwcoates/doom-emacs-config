package workspace

import (
	"context"
	"runtime"
	"time"
)

// A DAEMON-WIDE CAP ON VENDOR STARTS IN FLIGHT (2026-10-12). Every bring-up's
// start runs the vendor, the heaviest process a workspace spawns, and after a
// bounce every workspace starts at once: thirty Claude processes starting
// together on a loaded host each took longer than the shim's 3s liveness
// bound, so each was killed, retried every few seconds on the backoff and
// killed again, and the retries themselves kept the host saturated (load
// average 160; no session came up for the whole ten-minute retry window).
// With a few starts in flight at a time, each start gets the processor it
// needs to answer, and the rest wait their turn -- a wait that is no failure
// and spends nothing of the vendor's retry window.

// vendorStartSlots is how many vendor starts run at once: a quarter of the
// processors, at least two.
func vendorStartSlots() int {
	if n := runtime.NumCPU() / 4; n > 2 {
		return n
	}
	return 2
}

// vendorSlotWaitReported is how long a start waits for a slot before the wait
// is recorded at INFO.
const vendorSlotWaitReported = 2 * time.Second

// vendorSlots is the cap: a token per start in flight.
type vendorSlots chan struct{}

func newVendorSlots(n int) vendorSlots { return make(vendorSlots, n) }

// acquire takes a slot, waiting for one, and answers its release; it answers
// the context's cause when the context ends first.
func (s vendorSlots) acquire(ctx context.Context) (func(), error) {
	select {
	case s <- struct{}{}:
		return func() { <-s }, nil
	case <-ctx.Done():
		return nil, context.Cause(ctx)
	}
}
