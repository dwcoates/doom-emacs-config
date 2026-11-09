package drain

import (
	"time"

	"agent-repld/internal/ids"
)

// ParkRetryCeiling caps the idle sweep's backoff for a workspace whose
// park keeps FAILING.
//
// A FAILED PARK WAS RETRIED ON EVERY PASS, FOREVER. The sweep's cadence
// is min(DefaultSweepEvery, the idle cutoff), so a short cutoff is a short
// cadence: an orphaned integration daemon (cutoff 50ms) whose shim answered
// every Hibernate with a transport failure re-ran the directive, the lease
// round trip, the host republish and an ERROR record twenty times a second
// until someone killed it. A failure that nothing in this process is going to
// fix is not a reason to ask again at full speed, so each consecutive failure
// doubles the wait before the next attempt, from one cadence up to this
// ceiling. An hour is the idle sweep's own scale: the default cutoff is twelve
// hours, and a session that cannot be parked inside one more hour has
// already cost nothing but its idle shim.
const ParkRetryCeiling = time.Hour

// parkRetry is one workspace's backoff after a failed park.
type parkRetry struct {
	// failures counts the consecutive failed attempts.
	failures int
	// notBefore is the earliest instant the next attempt may be made.
	notBefore time.Time
}

// parkRetryDelay is the wait after the n-th consecutive failure (n >= 1):
// the sweep's cadence doubled n times, capped at ParkRetryCeiling. The
// first failure therefore skips exactly one pass.
func parkRetryDelay(every time.Duration, failures int) time.Duration {
	delay := every
	for i := 0; i < failures; i++ {
		if delay >= ParkRetryCeiling/2 {
			return ParkRetryCeiling
		}
		delay *= 2
	}
	if delay > ParkRetryCeiling {
		return ParkRetryCeiling
	}
	return delay
}

// backingOff answers whether a workspace's next park attempt is still
// inside its backoff at now, and the backoff it is in.
func (c *controller) backingOff(ws ids.WorkspaceID, now time.Time) (parkRetry, bool) {
	c.mu.Lock()
	defer c.mu.Unlock()
	retry, ok := c.retries[ws]
	if !ok {
		return parkRetry{}, false
	}
	return retry, now.Before(retry.notBefore)
}

// recordFailure counts one more failed park of ws at now and answers the
// backoff it now stands in.
func (c *controller) recordFailure(ws ids.WorkspaceID, now time.Time) parkRetry {
	c.mu.Lock()
	defer c.mu.Unlock()
	if c.retries == nil {
		c.retries = map[ids.WorkspaceID]parkRetry{}
	}
	retry := c.retries[ws]
	retry.failures++
	retry.notBefore = now.Add(parkRetryDelay(c.deps.SweepEvery, retry.failures))
	c.retries[ws] = retry
	return retry
}

// clearRetry forgets a workspace's backoff: its last attempt did not fail.
func (c *controller) clearRetry(ws ids.WorkspaceID) {
	c.mu.Lock()
	defer c.mu.Unlock()
	delete(c.retries, ws)
}
