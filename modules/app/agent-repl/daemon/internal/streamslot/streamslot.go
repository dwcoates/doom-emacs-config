// Package streamslot holds a value that belongs to ONE standing stream: the
// stream attaches it when it opens and releases it when it ends, and the value
// stands exactly as long as that stream does.
//
// A LATER STREAM SUPERSEDES AN EARLIER ONE. A reconnect can open the next
// stream before the previous one's end is observed; each Attach takes a new
// generation, and only the current generation's release clears the value, so
// a stale stream ending cannot erase its successor's.
package streamslot

import "sync"

// Slot is one stream-scoped value. The zero Slot holds nothing.
type Slot[T any] struct {
	mu sync.Mutex
	// generation numbers the attached streams; attached is whether the
	// current one still stands.
	generation uint64
	attached   bool
	value      T
}

// Attach stands a new stream's value, answering the stream's generation and
// whether it superseded a stream that still stood.
func (s *Slot[T]) Attach(v T) (generation uint64, superseded bool) {
	s.mu.Lock()
	defer s.mu.Unlock()
	s.generation++
	superseded = s.attached
	s.attached = true
	s.value = v
	return s.generation, superseded
}

// Release ends the stream of generation, answering whether it was the current
// one: a superseded stream's release leaves its successor's value standing.
func (s *Slot[T]) Release(generation uint64) (current bool) {
	s.mu.Lock()
	defer s.mu.Unlock()
	current = s.attached && s.generation == generation
	if current {
		var zero T
		s.attached = false
		s.value = zero
	}
	return current
}

// Update moves the standing stream's value, answering the value it replaced,
// its generation and whether one stood: with none standing nothing changes
// and previous is the zero value. The previous value is read under the same
// hold the new one is written under, so a caller deciding on the change (a
// focus GAIN) decides on exactly the edge it made.
func (s *Slot[T]) Update(v T) (previous T, generation uint64, attached bool) {
	s.mu.Lock()
	defer s.mu.Unlock()
	if s.attached {
		previous = s.value
		s.value = v
	}
	return previous, s.generation, s.attached
}

// Get answers the standing stream's value, and whether one stands.
func (s *Slot[T]) Get() (T, bool) {
	s.mu.Lock()
	defer s.mu.Unlock()
	return s.value, s.attached
}
