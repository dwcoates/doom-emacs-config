package sessionwatcher

import (
	"sync"
	"testing"
	"time"
)

// manualClock is a clock.Clock a test fires by hand: every After is recorded
// with its delay, and nothing fires until the test says so.
type manualClock struct {
	mu     sync.Mutex
	timers []*manualTimer
	armed  chan struct{}
}

// manualTimer is one After the watcher armed.
type manualTimer struct {
	delay time.Duration
	ch    chan time.Time
}

func newManualClock() *manualClock {
	return &manualClock{armed: make(chan struct{}, 64)}
}

// Now is a fixed instant: nothing in the watcher reads it for a decision.
func (c *manualClock) Now() time.Time { return time.Unix(0, 0) }

// After records the wait and answers a channel only fire sends on.
func (c *manualClock) After(d time.Duration) <-chan time.Time {
	c.mu.Lock()
	defer c.mu.Unlock()
	t := &manualTimer{delay: d, ch: make(chan time.Time, 1)}
	c.timers = append(c.timers, t)
	c.armed <- struct{}{}
	return t.ch
}

// awaitArmed waits for the next After and answers its delay.
func (c *manualClock) awaitArmed(t *testing.T) time.Duration {
	t.Helper()
	select {
	case <-c.armed:
	case <-time.After(5 * time.Second):
		t.Fatal("no re-open was armed on the clock")
	}
	c.mu.Lock()
	defer c.mu.Unlock()
	return c.timers[len(c.timers)-1].delay
}

// noneArmed asserts nothing was armed since the last awaitArmed.
func (c *manualClock) noneArmed(t *testing.T) {
	t.Helper()
	select {
	case <-c.armed:
		t.Fatal("a re-open was armed, want none")
	default:
	}
}

// fireLast fires the most recently armed wait.
func (c *manualClock) fireLast() {
	c.mu.Lock()
	defer c.mu.Unlock()
	c.timers[len(c.timers)-1].ch <- time.Unix(0, 0)
}
