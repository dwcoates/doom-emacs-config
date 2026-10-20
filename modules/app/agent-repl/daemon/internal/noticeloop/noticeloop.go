// Package noticeloop is the one ORDERED, NON-BLOCKING delivery loop every
// notifier of an agent shares (mergenotice, workspacenotice): a reporter
// appends an item and never waits; one loop delivers items in the order they
// were reported; and when the loop's context ends it still delivers every item
// already reported before it returns, so nothing reported before the daemon's
// exit is lost to it. An item reported after the loop has finished is handed
// to the notifier's untold callback, never silently dropped.
package noticeloop

import (
	"context"
	"sync"
)

// Loop is one notifier's pending items and the loop that delivers them.
type Loop[T any] struct {
	mu      sync.Mutex
	pending []T
	stopped bool
	wake    chan struct{}
	untold  func(T)
}

// New builds a loop. UNTOLD receives each item reported after Run returned.
func New[T any](untold func(T)) *Loop[T] {
	return &Loop[T]{wake: make(chan struct{}, 1), untold: untold}
}

// Report appends ITEM for the loop and never blocks.
func (l *Loop[T]) Report(item T) {
	l.mu.Lock()
	if l.stopped {
		l.mu.Unlock()
		l.untold(item)
		return
	}
	l.pending = append(l.pending, item)
	l.mu.Unlock()
	select {
	case l.wake <- struct{}{}:
	default:
	}
}

// Run calls DELIVER on every reported item, in order, until ctx ends, then
// on every item still pending, and returns. Deliveries run under a context
// that ctx's end does not cancel, since ending is the signal to stop taking
// new work, never to drop what was reported.
func (l *Loop[T]) Run(ctx context.Context, deliver func(context.Context, T)) {
	deliveries := context.WithoutCancel(ctx)
	for {
		for _, item := range l.take() {
			deliver(deliveries, item)
		}
		select {
		case <-ctx.Done():
			l.finish(deliveries, deliver)
			return
		case <-l.wake:
		}
	}
}

func (l *Loop[T]) take() []T {
	l.mu.Lock()
	defer l.mu.Unlock()
	batch := l.pending
	l.pending = nil
	return batch
}

// finish delivers everything still pending and marks the loop stopped, under
// the same lock that finds nothing pending, so no item slips between the last
// take and the mark.
func (l *Loop[T]) finish(ctx context.Context, deliver func(context.Context, T)) {
	for {
		l.mu.Lock()
		if len(l.pending) == 0 {
			l.stopped = true
			l.mu.Unlock()
			return
		}
		batch := l.pending
		l.pending = nil
		l.mu.Unlock()
		for _, item := range batch {
			deliver(ctx, item)
		}
	}
}
