package rollout

import (
	"context"
	"fmt"
	"sync"

	"agent-repld/internal/ids"
	"agent-repld/internal/inflight"
	"agent-repld/internal/wsm"
)

// servingWatch answers every adoption waiting on its workspace's serving
// release from ONE read of the state database per change.
//
// EACH WAITER ONCE READ ITS OWN ROW ON EVERY COMMIT. Every adoption waiting on
// the incumbent woke on any commit either daemon made and re-read its
// workspace's serving row, so the reads grew as the waiters times the
// commits, and every adoption that went on committed more. The state
// database has one connection, and on 2026-11-17's rollout about 600 serving
// reads a second queued every other operation of the daemon behind them: the
// prompt queue's delivery lock was held 7s waiting for a lease read in that
// line, and the lock watchdog reported the daemon wedged. Now one loop
// follows the changes and reads every waited-on row in one statement
// (wsm.DB.ServingOf), so a commit costs one read however many adoptions
// wait.
//
// The loop runs only while a waiter is registered. A registration asks it
// for a read at once (kick), and the loop takes its change watch before its
// first read, so a release between the two is still answered.
type servingWatch struct {
	c *controller

	mu      sync.Mutex
	waiters map[*servingWaiter]struct{}
	running bool
	// kick asks the running loop for a read now: a new waiter is owed its
	// first answer without waiting for a commit.
	kick chan struct{}
}

// servingWaiter is one adoption's registration with the watch.
type servingWaiter struct {
	ws ids.WorkspaceID
	// reads carries the watch's latest answer for ws; a newer answer
	// replaces one the waiter has not taken yet.
	reads chan servingRead
}

// servingRead is one answer of the watch for one workspace: the daemon that
// serves it (nil for none), or why the watch could not say.
type servingRead struct {
	owner *ids.InstanceID
	err   error
	// watchFailed reports that err is the change watch's, not the read's.
	watchFailed bool
}

func newServingWatch(c *controller) *servingWatch {
	return &servingWatch{c: c, waiters: map[*servingWaiter]struct{}{}, kick: make(chan struct{}, 1)}
}

// watch registers a waiter for ws's serving row, starting the loop when none
// runs. stop deregisters it.
func (w *servingWatch) watch(ws ids.WorkspaceID) (*servingWaiter, func()) {
	waiter := &servingWaiter{ws: ws, reads: make(chan servingRead, 1)}
	w.mu.Lock()
	w.waiters[waiter] = struct{}{}
	start := !w.running
	w.running = true
	w.mu.Unlock()
	if start {
		lifetime := w.c.lifetime(context.Background())
		inflight.Go(func() { w.run(lifetime) })
	} else {
		select {
		case w.kick <- struct{}{}:
		default:
		}
	}
	return waiter, func() {
		w.mu.Lock()
		delete(w.waiters, waiter)
		w.mu.Unlock()
	}
}

// run reads every waited-on row, answers each waiter, and waits for the next
// change or registration, until no waiter is left.
func (w *servingWatch) run(ctx context.Context) {
	changes, stop := w.c.handoverChanges(ctx)
	defer stop()
	for {
		select {
		case <-w.kick:
		default:
		}
		waiters := w.registered()
		if waiters == nil {
			return
		}
		w.answer(ctx, waiters)
		select {
		case <-ctx.Done():
			w.fail(servingRead{err: fmt.Errorf("rollout: serving watch: %w", context.Cause(ctx)), watchFailed: true})
			return
		case <-w.kick:
		case err := <-changes:
			if err != nil {
				w.fail(servingRead{err: err, watchFailed: true})
				return
			}
		}
	}
}

// registered answers the waiters registered now, or nil -- marking the loop
// stopped in the same critical section -- when there are none, so a
// registration either sees the loop running or starts a new one.
func (w *servingWatch) registered() []*servingWaiter {
	w.mu.Lock()
	defer w.mu.Unlock()
	if len(w.waiters) == 0 {
		w.running = false
		return nil
	}
	out := make([]*servingWaiter, 0, len(w.waiters))
	for waiter := range w.waiters {
		out = append(out, waiter)
	}
	return out
}

// answer reads every waiter's row in one statement and hands each its own.
func (w *servingWatch) answer(ctx context.Context, waiters []*servingWaiter) {
	seen := map[ids.WorkspaceID]bool{}
	var workspaces []ids.WorkspaceID
	for _, waiter := range waiters {
		if !seen[waiter.ws] {
			seen[waiter.ws] = true
			workspaces = append(workspaces, waiter.ws)
		}
	}
	owners, err := w.c.deps.DB.ServingOf(ctx, workspaces)
	for _, waiter := range waiters {
		read := servingRead{err: err}
		if err == nil {
			owner, held := owners[waiter.ws]
			read.owner = owner
			if !held {
				read.err = fmt.Errorf("rollout: serving watch: workspace %s: %w", waiter.ws, wsm.ErrNotFound)
			}
		}
		waiter.give(read)
	}
}

// fail hands every registered waiter READ and marks the loop stopped: the
// waiters return with it, and a later registration starts a new loop.
func (w *servingWatch) fail(read servingRead) {
	w.mu.Lock()
	defer w.mu.Unlock()
	w.running = false
	for waiter := range w.waiters {
		waiter.give(read)
	}
}

// give hands the waiter READ, replacing an answer it has not taken. The loop
// is the channel's only sender, so the send never blocks.
func (s *servingWaiter) give(read servingRead) {
	select {
	case <-s.reads:
	default:
	}
	s.reads <- read
}
