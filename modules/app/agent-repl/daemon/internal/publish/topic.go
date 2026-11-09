// Package publish is the daemon's one publication primitive.
//
// Every resolver publishes whole views through a Topic; every stream handler
// subscribes to one. An EVENT (a click, a reload request) is not a view and is
// emitted through Events instead, which neither replays nor deduplicates. The subscription guarantee — never miss an update, never
// end on a stale one — is implemented here once so no handler reimplements it.
// See ARCHITECTURE.md "publish (internal/publish)" and docs/overhaul/daemon.md
// "Resolvers and push duties".
package publish

import (
	"agent-repld/internal/inflight"
	"context"
	"fmt"
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
	subs   map[*Mailbox[T]]struct{}
	// latestOnly makes every subscriber's queue hold at most the newest
	// value. It is fixed at construction.
	latestOnly bool
	// subsChanged closes at the next change of the subscriber set and is
	// replaced by it (AwaitSubscribers).
	subsChanged chan struct{}
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

// ClearIf forgets the topic's latest value when stale reports it stale, so it
// is no longer replayed to a late subscriber and an identical value published
// later is not dropped as a duplicate of it. It reports whether it cleared.
// Values already handed to subscribers are not recalled.
//
// It exists for an EVENT topic whose latest value stops being true: a
// workspace's `transferred` notice, once the outgoing daemon has taken the
// workspace back. Left standing, the notice was replayed to the page reloaded
// onto the daemon that kept it, which went quiet as if the workspace had moved,
// and the same notice published for the next transfer was dropped as a
// repeat, so no participant ever heard of it (2026-10-12).
func (t *Topic[T]) ClearIf(stale func(T) bool) bool {
	t.mu.Lock()
	defer t.mu.Unlock()
	if !t.has || !stale(t.latest) {
		return false
	}
	t.latest = *new(T)
	t.has = false
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
	return t.subscribe(ctx, nil)
}

// SubscribeCounted is Subscribe for a consumer whose handling of each value
// is work in flight (internal/inflight): every value is counted on the
// daemon's tracker from the instant it is published until the consumer calls
// ack, once per value received, after it has written it. A stream that
// writes each view to its client is the one consumer: a push the client has
// not been sent yet is never "nothing in flight".
func (t *Topic[T]) SubscribeCounted(ctx context.Context) (values <-chan T, ack func()) {
	return t.subscribeCounted(ctx, inflight.Default)
}

// StreamPushes counts every value on its way to a client's stream: from the
// instant it is enqueued until the stream handler has sent it (its ack). It is
// a child of the daemon's tracker, so the test barrier still sees each push,
// and it is what an orderly exit waits on before it ends the streams: every
// push enqueued has been handed to its stream, so the end frame that follows
// on the same stream follows it onto the socket.
var StreamPushes = inflight.NewChild(inflight.Default)

// SubscribeStream is SubscribeCounted for a stream handler: its values are
// counted on StreamPushes.
func (t *Topic[T]) SubscribeStream(ctx context.Context) (values <-chan T, ack func()) {
	return t.subscribeCounted(ctx, StreamPushes)
}

// subscribeCounted is SubscribeCounted on TRACKER.
func (t *Topic[T]) subscribeCounted(ctx context.Context, tracker *inflight.Tracker) (<-chan T, func()) {
	s := newSubscriber[T](t.latestOnly, tracker)
	return t.register(ctx, s), s.ack
}

// subscribe registers an uncounted subscriber.
func (t *Topic[T]) subscribe(ctx context.Context, tracker *inflight.Tracker) <-chan T {
	return t.register(ctx, newSubscriber[T](t.latestOnly, tracker))
}

// register queues the latest value for S, adds it, and starts its pump.
func (t *Topic[T]) register(ctx context.Context, s *Mailbox[T]) <-chan T {
	t.mu.Lock()
	if t.has {
		s.enqueue(t.latest)
	}
	if t.subs == nil {
		t.subs = make(map[*Mailbox[T]]struct{})
	}
	t.subs[s] = struct{}{}
	t.noteSubsChangedLocked()
	t.mu.Unlock()

	inflight.Standing(func() { pump(ctx, s, t.remove) })
	return s.out
}

// AwaitSubscribers waits until the topic is serving exactly want
// subscriptions, or ctx ends. It wakes on each subscription added or ended,
// never on a timer.
func (t *Topic[T]) AwaitSubscribers(ctx context.Context, want int) error {
	for {
		t.mu.Lock()
		got := len(t.subs)
		if t.subsChanged == nil {
			t.subsChanged = make(chan struct{})
		}
		changed := t.subsChanged
		t.mu.Unlock()
		if got == want {
			return nil
		}
		select {
		case <-changed:
		case <-ctx.Done():
			return fmt.Errorf("the topic serves %d subscriptions, want %d: %w", got, want, ctx.Err())
		}
	}
}

// noteSubsChangedLocked wakes every AwaitSubscribers. The caller holds mu.
func (t *Topic[T]) noteSubsChangedLocked() {
	if t.subsChanged != nil {
		close(t.subsChanged)
		t.subsChanged = nil
	}
}

// Subscribers reports how many live subscriptions the topic is serving. It
// exists for tests and for the daemon's own health answers.
func (t *Topic[T]) Subscribers() int {
	t.mu.Lock()
	defer t.mu.Unlock()
	return len(t.subs)
}

// Run pumps the mailbox onto its channel until ctx ends, then calls left,
// ends the count of whatever it still held, and closes the channel. It is
// the mailbox's one delivery loop, a long-running one.
func (s *Mailbox[T]) Run(ctx context.Context, left func()) {
	pump(ctx, s, func(*Mailbox[T]) { left() })
}

// Out is the channel the mailbox delivers on.
func (s *Mailbox[T]) Out() <-chan T { return s.out }

// pump drains one subscriber's queue onto its channel until ctx is cancelled,
// then unregisters it through unregister, ends the count of whatever it still
// held, and closes the channel. It is the one delivery loop Topic and Events
// share.
func pump[T any](ctx context.Context, s *Mailbox[T], unregister func(*Mailbox[T])) {
	defer func() {
		unregister(s)
		s.endAll()
		close(s.out)
	}()
	for {
		it, ok := s.dequeue()
		if ok {
			// The value is the consumer's from here, so its count moves to
			// the consumer's ack before the hand-over.
			s.handedOver(it.end)
			select {
			case s.out <- it.value:
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
func (t *Topic[T]) remove(s *Mailbox[T]) {
	t.mu.Lock()
	defer t.mu.Unlock()
	delete(t.subs, s)
	t.noteSubsChangedLocked()
}

// Mailbox is one live subscription's delivery: an unbounded queue, a wake-up
// signal, and the channel its pump writes to. Topic, Events and the feed's
// tails all deliver through it, so a value's delivery -- and its count -- has
// one implementation.
//
// A COUNTED subscriber (tracker set) counts every value on the tracker from
// the instant it is enqueued until its consumer acknowledges it (ack): a
// value on its way to a stream is work in flight exactly as a goroutine is.
type Mailbox[T any] struct {
	mu     sync.Mutex
	queue  []item[T]
	out    chan T
	signal chan struct{}
	// latestOnly replaces a value still queued rather than queuing behind it.
	latestOnly bool
	// tracker counts this subscription's values; nil counts nothing.
	tracker *inflight.Tracker
	// handed are the ends of the values the pump handed to the consumer and
	// the consumer has not yet acknowledged, oldest first.
	handed []func()
}

// item is one queued value and the end of its count (nil when uncounted).
type item[T any] struct {
	value T
	end   func()
}

// NewMailbox builds an empty mailbox, counting its values on tracker when it
// is not nil: from Enqueue until the consumer's Ack.
func NewMailbox[T any](tracker *inflight.Tracker) *Mailbox[T] {
	return newSubscriber[T](false, tracker)
}

// newSubscriber builds an unregistered subscription with an empty queue,
// counting its values on tracker when it is not nil.
func newSubscriber[T any](latestOnly bool, tracker *inflight.Tracker) *Mailbox[T] {
	return &Mailbox[T]{
		out:        make(chan T),
		signal:     make(chan struct{}, 1),
		latestOnly: latestOnly,
		tracker:    tracker,
	}
}

// enqueue appends v and wakes the pump. It never blocks, which is what keeps a
// slow subscriber off the publisher's back. On a latest-only subscriber a
// value still waiting in the queue is replaced, so the queue holds at most one;
// the replaced value's count ends with it.
// Enqueue is enqueue for a mailbox outside this package.
func (s *Mailbox[T]) Enqueue(v T) { s.enqueue(v) }

func (s *Mailbox[T]) enqueue(v T) {
	var end func()
	if s.tracker != nil {
		end = s.tracker.Begin()
	}
	s.mu.Lock()
	if s.latestOnly && len(s.queue) > 0 {
		last := &s.queue[len(s.queue)-1]
		if last.end != nil {
			last.end()
		}
		*last = item[T]{value: v, end: end}
	} else {
		s.queue = append(s.queue, item[T]{value: v, end: end})
	}
	s.mu.Unlock()
	select {
	case s.signal <- struct{}{}:
	default:
	}
}

// dequeue removes the oldest queued value, reporting false when the queue is
// empty.
func (s *Mailbox[T]) dequeue() (item[T], bool) {
	s.mu.Lock()
	defer s.mu.Unlock()
	if len(s.queue) == 0 {
		return item[T]{}, false
	}
	v := s.queue[0]
	s.queue[0] = item[T]{}
	s.queue = s.queue[1:]
	return v, true
}

// handedOver records that the consumer now holds a value whose count ends at
// its ack.
func (s *Mailbox[T]) handedOver(end func()) {
	if end == nil {
		return
	}
	s.mu.Lock()
	s.handed = append(s.handed, end)
	s.mu.Unlock()
}

// Ack ends the count of the oldest value the consumer holds.
func (s *Mailbox[T]) Ack() { s.ack() }

func (s *Mailbox[T]) ack() {
	s.mu.Lock()
	if len(s.handed) == 0 {
		s.mu.Unlock()
		if s.tracker != nil {
			panic("publish: a counted subscription acknowledged a value it was never handed")
		}
		return
	}
	end := s.handed[0]
	s.handed = s.handed[1:]
	s.mu.Unlock()
	end()
}

// endAll ends the count of every value still queued or unacknowledged: the
// subscription is over, and nothing will be written for them.
func (s *Mailbox[T]) endAll() {
	s.mu.Lock()
	queued, handed := s.queue, s.handed
	s.queue, s.handed = nil, nil
	s.mu.Unlock()
	for _, it := range queued {
		if it.end != nil {
			it.end()
		}
	}
	for _, end := range handed {
		end()
	}
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

// Changes answers a channel that receives once per value T delivers to a
// subscription (the latest first, if one exists), until ctx ends, when it
// closes: for a caller that waits on the view MOVING rather than reading what
// it moved to. EACH CHANGE IS COUNTED as work in flight from the value's
// publish until the caller's ack, which it calls once it has done what the
// change woke it for: the test barrier then sees a change still being acted
// on as work, not as a quiet daemon.
func Changes[T any](ctx context.Context, t *Topic[T]) (changes <-chan struct{}, ack func()) {
	return ChangesOn(ctx, t, inflight.Default)
}

// ChangesOn is Changes counting on TRACKER.
func ChangesOn[T any](ctx context.Context, t *Topic[T], tracker *inflight.Tracker) (<-chan struct{}, func()) {
	values, ackValue := t.subscribeCounted(ctx, tracker)
	out := make(chan struct{})
	inflight.Standing(func() {
		defer close(out)
		for range values {
			select {
			case out <- struct{}{}:
			case <-ctx.Done():
				// Never handed over: the subscription ends with ctx, and its
				// end ends the count of every value it still holds.
				return
			}
		}
	})
	return out, ackValue
}
