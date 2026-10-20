package main

import (
	"context"
	"sync"
)

// noticeLifetime runs the notifiers' delivery loops (the merge queue's
// subscriptions, the derived-workspace notices, the workspace subscriptions,
// the self-closes) on a
// lifetime of its own, which the exit ends only AFTER the merge drain: a merge
// landing inside the drain reports its landing, and the loop, once ended,
// delivers every change still pending before it returns. A background loop
// ends with the serving context, BEFORE the drain, so a landing it reported
// would be left untold.
type noticeLifetime struct {
	run func(ctx context.Context)

	mu      sync.Mutex
	started bool
	cancel  context.CancelFunc
	done    chan struct{}
}

// newNoticeLifetime builds the lifetime of RUN, which returns once its
// context ends and it has delivered what was pending.
func newNoticeLifetime(run func(ctx context.Context)) *noticeLifetime {
	return &noticeLifetime{run: run}
}

// Start runs the loop. A second Start is a no-op.
func (l *noticeLifetime) Start() {
	l.mu.Lock()
	defer l.mu.Unlock()
	if l.started {
		return
	}
	ctx, cancel := context.WithCancel(context.Background())
	l.started, l.cancel, l.done = true, cancel, make(chan struct{})
	go func() {
		defer close(l.done)
		l.run(ctx)
	}()
}

// Stop ends the loop and waits for it to deliver what was pending and
// return. A lifetime never started is started first, so changes reported
// before a failed boot reached serving are still delivered.
func (l *noticeLifetime) Stop() {
	l.Start()
	l.mu.Lock()
	cancel, done := l.cancel, l.done
	l.mu.Unlock()
	cancel()
	<-done
}

// runNotifiers runs every notifier's loop on CTX, and returns once each has
// returned: each delivers what was pending before it does.
func runNotifiers(ctx context.Context, runs ...func(ctx context.Context)) {
	var wg sync.WaitGroup
	for _, run := range runs {
		wg.Add(1)
		go func() {
			defer wg.Done()
			run(ctx)
		}()
	}
	wg.Wait()
}
