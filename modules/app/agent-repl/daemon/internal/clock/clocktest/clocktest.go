// Package clocktest is the one scripted clock.Clock for tests: Now is set by
// hand, and every After is announced on Asked and answers only when the test
// sends on Fire, so a test drives every wait without sleeping.
package clocktest

import (
	"sync"
	"time"

	"agent-repld/internal/clock"
)

// Scripted is a clock.Clock the test drives.
type Scripted struct {
	// Asked announces every wait After is asked for, in order, so a test
	// synchronizes on the code under test having reached its wait.
	Asked chan time.Duration
	// Fire answers the wait in flight: every After returns this one channel.
	Fire chan time.Time

	mu  sync.Mutex
	now time.Time
}

var _ clock.Clock = (*Scripted)(nil)

// New is a scripted clock standing at now. Asked is buffered, so code that
// asks for a wait the test never reads does not block on the announcement.
func New(now time.Time) *Scripted {
	return &Scripted{Asked: make(chan time.Duration, 16), Fire: make(chan time.Time), now: now}
}

// Now is the instant the test last set.
func (s *Scripted) Now() time.Time {
	s.mu.Lock()
	defer s.mu.Unlock()
	return s.now
}

// Set moves Now to at, firing nothing.
func (s *Scripted) Set(at time.Time) {
	s.mu.Lock()
	defer s.mu.Unlock()
	s.now = at
}

// After announces d on Asked and answers Fire.
func (s *Scripted) After(d time.Duration) <-chan time.Time {
	s.Asked <- d
	return s.Fire
}
