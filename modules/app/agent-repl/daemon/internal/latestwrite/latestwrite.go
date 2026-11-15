// Package latestwrite writes the latest value of each key off its callers'
// goroutines.
//
// A DURABLE WRITE A HOT PATH MAKES ONLY FOR A LATER PROCESS IS NEVER WAITED
// ON. The roster tells its last turn results and status change instants to
// the state database, which has ONE connection (wsm/conn.go), and it told
// them in its combined commit's report, which every roster caller waits for:
// a session watcher handing its view edges over, the rollout's adoptions, the
// roster mirror. On 2026-11-15 a successor's adoption burst queued 37
// operations on that connection, the report's SetStatusChangedAt waited
// behind them, and a session watcher's view delivery held
// `sessionwatcher.views` past the lockwatch threshold
// (`daemon.lockwatch.stall`, held 7s, 1501 goroutines). What the write keeps
// is read only by a daemon that did not see the change (a successor, a
// restart), so nothing this daemon does waits for it.
//
// A Writer therefore takes a value at once and writes it on its own
// goroutine. A key written again before its write ran is written ONCE, with
// its values merged (the caller's merge: the later instant, the newer
// result), so a burst costs one write per key however many changes it holds.
//
// CLOSE WRITES EVERYTHING HELD before it returns, so an orderly exit closes
// the Writer before the state database and loses nothing. A value submitted
// after Close is written on its caller's goroutine: the Writer's lifetime has
// ended, and the write is still made, in order after every held one.
package latestwrite

import (
	"sync"

	"agent-repld/internal/inflight"
)

// Writer writes the latest value of each key on a goroutine of its own.
type Writer[K comparable, V any] struct {
	write func(K, V)
	merge func(held, next V) V

	mu sync.Mutex
	// pending holds each key's value not yet taken for writing, and order the
	// keys in the order they were first held, so writes follow submission.
	pending map[K]V
	order   []K
	// closing is set by Close; closed once the loop has written everything
	// held, after which Submit writes on its caller.
	closing bool
	closed  bool
	// wake carries one word: something is held. stop is closed by Close.
	wake chan struct{}
	stop chan struct{}
	done chan struct{}
}

// New starts a Writer that writes each key's value with WRITE, and merges a
// value submitted for a key already held with MERGE (held, then the new one).
// WRITE owns its failures: it records them through its owner's log.
func New[K comparable, V any](write func(K, V), merge func(held, next V) V) *Writer[K, V] {
	w := &Writer[K, V]{
		write:   write,
		merge:   merge,
		pending: make(map[K]V),
		wake:    make(chan struct{}, 1),
		stop:    make(chan struct{}),
		done:    make(chan struct{}),
	}
	inflight.Standing(w.run)
	return w
}

// Submit holds V as KEY's value to write, merged with one already held, and
// returns at once. After Close it writes V on the caller's goroutine.
func (w *Writer[K, V]) Submit(key K, v V) {
	w.mu.Lock()
	if w.closed {
		w.mu.Unlock()
		w.write(key, v)
		return
	}
	if held, ok := w.pending[key]; ok {
		w.pending[key] = w.merge(held, v)
	} else {
		w.pending[key] = v
		w.order = append(w.order, key)
	}
	w.mu.Unlock()
	select {
	case w.wake <- struct{}{}:
	default:
	}
}

// Close writes every value held, ends the Writer's goroutine, and returns
// once both are done. A second Close returns once the first has.
func (w *Writer[K, V]) Close() {
	w.mu.Lock()
	if !w.closing {
		w.closing = true
		close(w.stop)
	}
	w.mu.Unlock()
	<-w.done
}

// run writes what is held each time it is woken, until Close: it then writes
// what is still held and marks the Writer closed in the same hold that found
// nothing left, so no value is written by a caller while a held one is not.
func (w *Writer[K, V]) run() {
	defer close(w.done)
	for {
		select {
		case <-w.wake:
			w.writeHeld(false)
		case <-w.stop:
			w.writeHeld(true)
			return
		}
	}
}

// writeHeld writes every value held, taking them in batches until none is
// left; when FINAL, the Writer is closed in the hold that found none.
func (w *Writer[K, V]) writeHeld(final bool) {
	for {
		w.mu.Lock()
		if len(w.order) == 0 {
			if final {
				w.closed = true
			}
			w.mu.Unlock()
			return
		}
		order, pending := w.order, w.pending
		w.order, w.pending = nil, make(map[K]V)
		w.mu.Unlock()
		for _, key := range order {
			w.write(key, pending[key])
		}
	}
}
