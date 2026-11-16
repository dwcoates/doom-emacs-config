// Package changesignal is the daemon's one broadcast of "this changed": a
// waiter takes the standing channel, and it closes at the next change.
//
// A wait on a change is a channel receive, never a poll or a sleep. The
// guarantee that no change is missed comes from WHERE the channel is taken:
// the owner reads its state and Changed together under the lock that guards
// the state, and calls Notify under the same lock, so a waiter that saw the
// old state holds the channel the next change closes.
package changesignal

import "sync"

// Signal is one broadcast. Its zero value is ready, and it is safe for
// concurrent use (an owner may take Changed under a read lock), but its
// promise that no change is missed is the owner's: Changed and Notify are
// called under the lock that guards what they announce.
type Signal struct {
	mu sync.Mutex
	ch chan struct{}
}

// Changed answers a channel that closes at the next Notify.
func (s *Signal) Changed() <-chan struct{} {
	s.mu.Lock()
	defer s.mu.Unlock()
	if s.ch == nil {
		s.ch = make(chan struct{})
	}
	return s.ch
}

// Notify closes the standing channel, waking everything that took it. A
// Changed after it answers a fresh one.
func (s *Signal) Notify() {
	s.mu.Lock()
	defer s.mu.Unlock()
	if s.ch != nil {
		close(s.ch)
		s.ch = nil
	}
}
