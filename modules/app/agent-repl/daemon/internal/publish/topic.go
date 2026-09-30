// Package publish is the daemon's one publication primitive.
//
// Every resolver publishes whole views through a Topic; every stream handler
// subscribes to one. The subscription guarantee — never miss an update, never
// end on a stale one — is implemented here once so no handler reimplements it.
// See ARCHITECTURE.md "publish (internal/publish)" and docs/overhaul/daemon.md
// "Resolvers and push duties".
package publish

import (
	"context"
	"reflect"
	"sync"

	"google.golang.org/protobuf/proto"
)

// Topic carries the latest value of one view and fans later values out to
// every subscriber. The zero Topic is ready to use.
//
// The guarantee, in full:
//   - Subscribe delivers the latest published value first, if one exists, and
//     then every later value in publication order, skipping none.
//   - A subscriber's channel closes only when its context is cancelled, so a
//     stream never ends holding a stale view.
//   - A slow subscriber never blocks Publish: each subscriber owns an
//     unbounded queue drained by its own goroutine.
//
// A LATEST-ONLY topic (NewLatestOnly) relaxes "skipping none": a subscriber
// that has not yet taken a queued value has it REPLACED by the newer one, so
// its queue never holds more than one value. It is for a WHOLE-VIEW topic
// whose every value supersedes the last, published at a rate a slow reader
// must not have to replay (the footer's activity cell republishes per line of
// streamed reasoning). A subscriber still receives the latest value, and a
// stream still never ends holding a stale one.
type Topic[T any] struct {
	mu     sync.Mutex
	latest T
	has    bool
	subs   map[*subscriber[T]]struct{}
	// latestOnly makes every subscriber's queue hold at most the newest
	// value. It is fixed at construction.
	latestOnly bool
}

// NewLatestOnly builds a topic whose subscribers are handed only the newest
// value they have not yet taken: a value superseded before its subscriber took
// it is dropped for that subscriber. Every other guarantee of Topic holds.
func NewLatestOnly[T any]() *Topic[T] {
	return &Topic[T]{latestOnly: true}
}

// Publish records v as the topic's latest value and hands it to every
// subscriber. It is a no-op when v equals the current latest value: proto
// messages compare with proto.Equal, other comparable values with ==, so
// identical re-renders never reach the wire.
func (t *Topic[T]) Publish(v T) {
	t.mu.Lock()
	defer t.mu.Unlock()
	if t.has && equalValues(t.latest, v) {
		return
	}
	t.latest = v
	t.has = true
	for s := range t.subs {
		s.enqueue(v)
	}
}

// Republish hands the topic's CURRENT latest value to every subscriber again,
// without changing it. It reports whether there was one to hand out.
//
// It exists because Publish deliberately drops an identical re-render, which
// is right for a resolver re-rendering the same view and wrong for the one
// caller that means the repetition: an adopted workspace whose clients must be
// repainted from what the daemon holds, not from what they happened to receive
// before the handover.
func (t *Topic[T]) Republish() bool {
	t.mu.Lock()
	defer t.mu.Unlock()
	if !t.has {
		return false
	}
	for s := range t.subs {
		s.enqueue(t.latest)
	}
	return true
}

// Latest reports the most recently published value, and whether anything has
// been published at all.
func (t *Topic[T]) Latest() (T, bool) {
	t.mu.Lock()
	defer t.mu.Unlock()
	return t.latest, t.has
}

// Subscribe returns a channel carrying the latest value (if any) followed by
// every later value in order. The channel closes when ctx is cancelled, and
// only then. The caller must cancel ctx or the subscriber leaks.
func (t *Topic[T]) Subscribe(ctx context.Context) <-chan T {
	out := make(chan T)
	s := &subscriber[T]{
		out:        out,
		signal:     make(chan struct{}, 1),
		latestOnly: t.latestOnly,
	}

	t.mu.Lock()
	if t.has {
		s.queue = append(s.queue, t.latest)
	}
	if t.subs == nil {
		t.subs = make(map[*subscriber[T]]struct{})
	}
	t.subs[s] = struct{}{}
	t.mu.Unlock()

	go t.pump(ctx, s)
	return out
}

// Subscribers reports how many live subscriptions the topic is serving. It
// exists for tests and for the daemon's own health answers.
func (t *Topic[T]) Subscribers() int {
	t.mu.Lock()
	defer t.mu.Unlock()
	return len(t.subs)
}

// pump drains one subscriber's queue onto its channel until ctx is cancelled,
// then unregisters it and closes the channel.
func (t *Topic[T]) pump(ctx context.Context, s *subscriber[T]) {
	defer func() {
		t.remove(s)
		close(s.out)
	}()
	for {
		v, ok := s.dequeue()
		if ok {
			select {
			case s.out <- v:
			case <-ctx.Done():
				return
			}
			continue
		}
		select {
		case <-s.signal:
		case <-ctx.Done():
			return
		}
	}
}

// remove unregisters a subscriber so Publish stops enqueuing to it.
func (t *Topic[T]) remove(s *subscriber[T]) {
	t.mu.Lock()
	defer t.mu.Unlock()
	delete(t.subs, s)
}

// subscriber is one live subscription: an unbounded queue, a wake-up signal,
// and the channel the pump writes to.
type subscriber[T any] struct {
	mu     sync.Mutex
	queue  []T
	out    chan T
	signal chan struct{}
	// latestOnly replaces a value still queued rather than queuing behind it.
	latestOnly bool
}

// enqueue appends v and wakes the pump. It never blocks, which is what keeps a
// slow subscriber off the publisher's back. On a latest-only subscriber a
// value still waiting in the queue is replaced, so the queue holds at most one.
func (s *subscriber[T]) enqueue(v T) {
	s.mu.Lock()
	if s.latestOnly && len(s.queue) > 0 {
		s.queue[len(s.queue)-1] = v
	} else {
		s.queue = append(s.queue, v)
	}
	s.mu.Unlock()
	select {
	case s.signal <- struct{}{}:
	default:
	}
}

// dequeue removes the oldest queued value, reporting false when the queue is
// empty.
func (s *subscriber[T]) dequeue() (T, bool) {
	s.mu.Lock()
	defer s.mu.Unlock()
	if len(s.queue) == 0 {
		var zero T
		return zero, false
	}
	v := s.queue[0]
	s.queue[0] = *new(T)
	s.queue = s.queue[1:]
	return v, true
}

// equalValues is the topic's deduplication rule: proto.Equal for proto
// messages, == for other comparable values, and "not equal" for anything else
// (an incomparable value can never be proven a duplicate, so it publishes).
func equalValues[T any](a, b T) bool {
	av, bv := any(a), any(b)
	if am, ok := av.(proto.Message); ok {
		bm, ok := bv.(proto.Message)
		return ok && proto.Equal(am, bm)
	}
	if av == nil || bv == nil {
		return av == nil && bv == nil
	}
	if !reflect.TypeOf(av).Comparable() {
		return false
	}
	return av == bv
}
