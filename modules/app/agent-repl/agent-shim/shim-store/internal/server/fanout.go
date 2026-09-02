package server

import "sync"

// DefaultWatchBuffer is the per-subscriber frame buffer when --watch-buffer is
// not given. It is SUBSTANTIALLY above the ~1k a daemon bounce can burst,
// because the alternative to buffering a burst is ending a healthy watch.
const DefaultWatchBuffer = 8192

// sink is one standing watch: the key it follows (a book for a page-line
// watch, a run for a bash watch), the bounded channel the fan-out hands items
// to, and the overflow signal that ends it.
type sink[T any] struct {
	key       string
	tokenHash string
	items     chan T
	// overflow is CLOSED, never sent on, when this subscriber could not keep
	// up. A closed channel is readable forever, so the watch loop cannot miss
	// the signal no matter which branch of its select won a race.
	overflow chan struct{}
	// dropped is the item count lost at the moment of overflow, for the
	// warning record. Written once, under the fan-out's lock, before overflow
	// is closed; read only after observing that close.
	dropped int
}

// fanout is the registry of standing watchers of one item kind.
//
// IT IS GENERIC OVER THE ITEM because the store now has two standing streams —
// page lines keyed by book, bash rows keyed by run — with identical
// backpressure semantics. Two hand-copied registries would be two places for
// the non-blocking-publish rule to drift out of agreement.
//
// PUBLISHING NEVER BLOCKS. Every send is non-blocking under the registry lock:
// a subscriber that cannot keep up is unsubscribed and signalled, so one slow
// reader can never stall a writer's transaction acknowledgement.
type fanout[T any] struct {
	mu     sync.Mutex
	buffer int
	subs   map[*sink[T]]struct{}
	// keyOf reads the routing key off one item — the only thing that differs
	// between the two registries.
	keyOf func(T) string
}

func newFanout[T any](buffer int, keyOf func(T) string) *fanout[T] {
	if keyOf == nil {
		panic("shim-store server: fan-out with no key function")
	}
	if buffer <= 0 {
		buffer = DefaultWatchBuffer
	}
	return &fanout[T]{buffer: buffer, subs: map[*sink[T]]struct{}{}, keyOf: keyOf}
}

// subscribe registers a watcher for one key.
//
// IT IS CALLED BEFORE THE REPLAY QUERY, under this lock, which is what makes
// the replay-to-live handoff gapless: any item committed after this point is
// either found by the replay, delivered on this channel, or both — and the
// watch loop dedupes the "both" case by write ordinal.
func (f *fanout[T]) subscribe(key, tokenHash string) *sink[T] {
	sub := &sink[T]{
		key:       key,
		tokenHash: tokenHash,
		items:     make(chan T, f.buffer),
		overflow:  make(chan struct{}),
	}
	f.mu.Lock()
	defer f.mu.Unlock()
	f.subs[sub] = struct{}{}
	return sub
}

// unsubscribe removes a watcher. It is idempotent, so the watch loop's defer
// is safe after an overflow already removed the subscriber.
func (f *fanout[T]) unsubscribe(sub *sink[T]) {
	f.mu.Lock()
	defer f.mu.Unlock()
	delete(f.subs, sub)
}

// publish hands each item to every subscriber of its key and returns the
// subscribers that overflowed, so the caller logs one warning per victim.
func (f *fanout[T]) publish(items []T) []*sink[T] {
	if len(items) == 0 {
		return nil
	}
	var overflowed []*sink[T]
	f.mu.Lock()
	defer f.mu.Unlock()
	for sub := range f.subs {
		remaining := 0
		for i, item := range items {
			if f.keyOf(item) != sub.key {
				continue
			}
			select {
			case sub.items <- item:
			default:
				// The buffer is full. Count what this batch could not place,
				// signal the subscriber, and drop it from the registry: a
				// watcher that fell behind recovers by re-opening, never by
				// being silently thinned.
				for _, rest := range items[i:] {
					if f.keyOf(rest) == sub.key {
						remaining++
					}
				}
				sub.dropped = remaining
				close(sub.overflow)
				delete(f.subs, sub)
				overflowed = append(overflowed, sub)
			}
			if remaining > 0 {
				break
			}
		}
	}
	return overflowed
}

// subscribers is the number of standing watchers. Diagnostics only.
func (f *fanout[T]) subscribers() int {
	f.mu.Lock()
	defer f.mu.Unlock()
	return len(f.subs)
}

// lineKey routes a written page line by the book it belongs to.
func lineKey(line LineWritten) string { return line.AgentID }

// bashRowKey routes a written bash row by the run it belongs to.
func bashRowKey(row BashRowWritten) string { return row.RunID }
