package publish

import (
	"agent-repld/internal/inflight"
	"context"
	"sync"
)

// Events fans EVENTS out to every live subscriber. The zero Events is ready to
// use.
//
// AN EVENT IS NOT A VIEW, so Events keeps none of what makes Topic right for
// one:
//   - It records no latest value, so a subscriber arriving after an event is
//     never handed it: an event belongs to the moment it happened.
//   - It drops no repeat, so an event identical to the one before is still
//     delivered: the user clicking the same link twice is two clicks, where a
//     Topic would have dropped the second as an identical re-render
//     (2026-10-14: a second click on the same merge test log never opened it).
//
// Every other guarantee is Topic's: a subscriber receives every event emitted
// after it subscribed, in emission order, skipping none; a slow subscriber
// never blocks Emit; a subscription closes only when its context is cancelled.
type Events[T any] struct {
	mu   sync.Mutex
	subs map[*Mailbox[T]]struct{}
}

// Emit hands v to every live subscriber and answers how many it was handed
// to. With none, v is gone, by design. The count is read under the lock that
// hands v over, so a caller that must act when nobody heard an event decides
// on exactly the subscribers it reached, never on a count read before or after.
func (e *Events[T]) Emit(v T) int {
	e.mu.Lock()
	defer e.mu.Unlock()
	for s := range e.subs {
		s.enqueue(v)
	}
	return len(e.subs)
}

// Subscribe returns a channel carrying every event emitted from this call on,
// in order. The channel closes when ctx is cancelled, and only then. The
// caller must cancel ctx or the subscriber leaks.
func (e *Events[T]) Subscribe(ctx context.Context) <-chan T {
	return e.register(ctx, newSubscriber[T](false, nil))
}

// SubscribeCounted is Topic.SubscribeCounted for events: each event counts on
// the daemon's tracker from Emit until the consumer acks it.
func (e *Events[T]) SubscribeCounted(ctx context.Context) (values <-chan T, ack func()) {
	s := newSubscriber[T](false, inflight.Default)
	return e.register(ctx, s), s.ack
}

// SubscribeStream is SubscribeCounted for a stream handler: its values are
// counted on StreamPushes (see Topic.SubscribeStream).
func (e *Events[T]) SubscribeStream(ctx context.Context) (values <-chan T, ack func()) {
	s := newSubscriber[T](false, StreamPushes)
	return e.register(ctx, s), s.ack
}

// register adds S and starts its pump.
func (e *Events[T]) register(ctx context.Context, s *Mailbox[T]) <-chan T {
	e.mu.Lock()
	if e.subs == nil {
		e.subs = make(map[*Mailbox[T]]struct{})
	}
	e.subs[s] = struct{}{}
	e.mu.Unlock()

	inflight.Standing(func() { pump(ctx, s, e.remove) })
	return s.out
}

// Subscribers reports how many live subscriptions Events is serving.
func (e *Events[T]) Subscribers() int {
	e.mu.Lock()
	defer e.mu.Unlock()
	return len(e.subs)
}

// remove unregisters a subscriber so Emit stops enqueuing to it.
func (e *Events[T]) remove(s *Mailbox[T]) {
	e.mu.Lock()
	defer e.mu.Unlock()
	delete(e.subs, s)
}
