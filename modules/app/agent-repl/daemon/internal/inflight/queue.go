package inflight

import (
	"context"
	"sync"
)

// Queue hands items to a long-running worker, each counted on its tracker
// from Put until the worker calls the item's done: the hand-off is work in
// flight exactly as a goroutine is. The queue is unbounded, so Put never
// blocks its caller.
type Queue[T any] struct {
	tracker *Tracker
	mu      sync.Mutex
	items   []queued[T]
	// ready holds a token while items wait.
	ready chan struct{}
}

type queued[T any] struct {
	value T
	end   func()
}

// NewQueue answers an empty queue counting on tracker (nil is Default).
func NewQueue[T any](tracker *Tracker) *Queue[T] {
	if tracker == nil {
		tracker = Default
	}
	return &Queue[T]{tracker: tracker, ready: make(chan struct{}, 1)}
}

// Put hands v to the worker, counted until its done is called.
func (q *Queue[T]) Put(v T) {
	end := q.tracker.Begin()
	q.mu.Lock()
	q.items = append(q.items, queued[T]{value: v, end: end})
	q.mu.Unlock()
	select {
	case q.ready <- struct{}{}:
	default:
	}
}

// Take waits for the next item, or ctx's end, answering the item and its
// done, which the worker calls once the item's work is over (and once any
// work it started is itself counted).
func (q *Queue[T]) Take(ctx context.Context) (v T, done func(), ok bool) {
	for {
		q.mu.Lock()
		if len(q.items) > 0 {
			item := q.items[0]
			q.items = q.items[1:]
			more := len(q.items) > 0
			q.mu.Unlock()
			if more {
				select {
				case q.ready <- struct{}{}:
				default:
				}
			}
			return item.value, item.end, true
		}
		q.mu.Unlock()
		select {
		case <-q.ready:
		case <-ctx.Done():
			var zero T
			return zero, nil, false
		}
	}
}

// Len reports how many items wait, for a report.
func (q *Queue[T]) Len() int {
	q.mu.Lock()
	defer q.mu.Unlock()
	return len(q.items)
}
