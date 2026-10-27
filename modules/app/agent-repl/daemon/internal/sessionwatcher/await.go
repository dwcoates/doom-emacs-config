package sessionwatcher

import (
	"context"
	"errors"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
)

// ErrWatcherClosed is what a wait answers when the watcher was torn down under
// it. A closed watcher will never become free and will never see a turn end,
// so the wait is ANSWERED rather than left hanging on a state that can no
// longer change.
var ErrWatcherClosed = errors.New("sessionwatcher: the watcher closed while a wait was standing")

// closedTurnMemory is how many just-ended turns a watcher remembers so an
// AwaitTurnEnd that arrives AFTER the terminal still gets its answer. The
// window exists because the caller submits a turn and waits on it in two
// steps, and the turn can end between them.
const closedTurnMemory = 32

// turnEnd is one delivery to a turn waiter.
type turnEnd struct {
	how TurnClose
	err error
}

// AwaitFree blocks until the workspace's facts are in and leave no turn in
// flight and no live detached work (freeLocked), or until ctx ends. It is DRIVEN BY THE STREAMS: every turn
// end and every live-work change signals the standing waiters, so nothing
// polls and nothing sleeps.
func (w *watcher) AwaitFree(ctx context.Context) error {
	ch, standing, err := w.registerFreeWaiter()
	if !standing {
		return err
	}
	select {
	case err := <-ch:
		return err
	case <-ctx.Done():
		w.dropFreeWaiter(ch)
		return ctx.Err()
	}
}

// AwaitSessionFacts blocks until the session facts have been taken up --
// StartSession's own answer, or the shim's re-announcement on an adopted
// watch -- or until ctx ends. It is what an adoption of a shim mid-work waits
// on before it lets any held prompt go: until the facts are in, TurnInFlight
// answers nil for a turn the adopted shim is running. A watcher closed first
// answers ErrWatcherClosed.
func (w *watcher) AwaitSessionFacts(ctx context.Context) error {
	w.mu.Lock()
	closed := w.closed
	w.mu.Unlock()
	if closed {
		return ErrWatcherClosed
	}
	return w.awaitLatch(ctx, w.factsIn)
}

// awaitLatch blocks until LATCH closes, or until ctx ends. The watcher closing
// first answers ErrWatcherClosed, unless the latch had closed too: a latch is
// a fact that was established, and a close does not take it back.
func (w *watcher) awaitLatch(ctx context.Context, latch <-chan struct{}) error {
	select {
	case <-latch:
		return nil
	case <-w.ctx.Done():
		// The watcher's own context ends with its Close.
		select {
		case <-latch:
			return nil
		default:
			return ErrWatcherClosed
		}
	case <-ctx.Done():
		return ctx.Err()
	}
}

// markMainWatchingLocked releases AwaitMainWatching the first time the main
// agent's watch serves its opening page. Later opens (a reopen, a revival's
// fresh watch) catch up from the pointer the daemon holds, so only the first
// one is a gate.
func (w *watcher) markMainWatchingLocked() {
	if w.mainWatched {
		return
	}
	w.mainWatched = true
	close(w.mainWatching)
	w.log.Info("daemon.sessionwatcher.main_watching", "the main agent's watch is serving; turns may start", dlog.Context{
		"since_open_ms": w.clock.Now().Sub(w.openedAt).Milliseconds(),
	})
}

// AwaitMainWatching blocks until the main agent's watch has served its
// opening page, or until ctx ends. StartTurn waits on it: a turn started
// before the watch stands sends frames nothing is subscribed to, and a
// tail_only watch opened after them never sees them. A watcher closed first
// answers ErrWatcherClosed.
func (w *watcher) AwaitMainWatching(ctx context.Context) error {
	w.mu.Lock()
	closed, watched := w.closed, w.mainWatched
	w.mu.Unlock()
	if watched {
		return nil
	}
	if closed {
		return ErrWatcherClosed
	}
	return w.waitMainWatching(ctx)
}

// waitMainWatching is AwaitMainWatching's wait, once the watch was found not
// yet serving: every answer through it was a turn held back, and is recorded.
func (w *watcher) waitMainWatching(ctx context.Context) error {
	began := w.clock.Now()
	if err := w.awaitLatch(ctx, w.mainWatching); err != nil {
		return err
	}
	w.log.Info("daemon.sessionwatcher.main_watching_waited", "a turn waited for the main agent's watch to serve", dlog.Context{
		"waited_ms": w.clock.Now().Sub(began).Milliseconds(),
	})
	return nil
}

// SessionFactsIn reports whether the session facts have been taken up, the
// same fact AwaitSessionFacts waits on, without waiting.
func (w *watcher) SessionFactsIn() bool {
	w.mu.Lock()
	defer w.mu.Unlock()
	return w.started
}

// registerFreeWaiter takes the answer that needs no wait, or files a waiter.
// It is separate from AwaitFree so the registration is one atomic step: the
// caller — and an in-package test — holds the channel BEFORE the edge it is
// waiting on can be routed.
func (w *watcher) registerFreeWaiter() (ch chan error, standing bool, err error) {
	w.mu.Lock()
	defer w.mu.Unlock()
	if w.closed {
		return nil, false, ErrWatcherClosed
	}
	if w.freeLocked() {
		return nil, false, nil
	}
	ch = make(chan error, 1)
	w.freeWaiters = append(w.freeWaiters, ch)
	w.log.Debug("daemon.sessionwatcher.await_free", "a caller is waiting for freeness", dlog.Context{
		"turn_in_flight": ids.Optional(w.turn),
		"waiters":        len(w.freeWaiters),
	})
	return ch, true, nil
}

// AwaitTurnEnd blocks until the named turn ends and reports HOW it ended. A
// turn that ended just before the call is answered from the watcher's memory
// of recently closed turns rather than waiting for an edge that already
// passed.
func (w *watcher) AwaitTurnEnd(ctx context.Context, turn ids.TurnID) (TurnClose, error) {
	ch, standing, end := w.registerTurnWaiter(turn)
	if !standing {
		return end.how, end.err
	}
	select {
	case end := <-ch:
		return end.how, end.err
	case <-ctx.Done():
		w.dropTurnWaiter(turn, ch)
		return 0, ctx.Err()
	}
}

// registerTurnWaiter takes the answer the watcher already holds, or files a
// waiter, in one atomic step.
func (w *watcher) registerTurnWaiter(turn ids.TurnID) (ch chan turnEnd, standing bool, answer turnEnd) {
	w.mu.Lock()
	defer w.mu.Unlock()
	if how, ok := w.closedTurns[turn]; ok {
		return nil, false, turnEnd{how: how}
	}
	if w.closed {
		return nil, false, turnEnd{err: ErrWatcherClosed}
	}
	ch = make(chan turnEnd, 1)
	w.turnWaiters[turn] = append(w.turnWaiters[turn], ch)
	w.log.Debug("daemon.sessionwatcher.await_turn_end", "a caller is waiting for a turn to end", dlog.Context{
		"turn_id": string(turn),
	})
	return ch, true, turnEnd{}
}

// turnEndedLocked is the ONE place a turn's end is recorded: the in-flight
// turn is cleared, the lifecycle sink is told, the close is remembered for a
// late waiter, every standing waiter on that turn is answered, and the
// freeness signal is raised.
func (w *watcher) turnEndedLocked(turn ids.TurnID, how TurnClose) {
	if w.turn != nil && *w.turn == turn {
		w.turn = nil
		if w.adopted != nil && *w.adopted == turn {
			w.adopted = nil
		}
		w.standNextWaitingLocked(turn)
	} else if !w.dropWaitingLocked(turn, "a turn waiting behind the adopted turn ended") {
		w.turn = nil
	}
	// THE LIFECYCLE SINK IS TOLD OFF THE LOCK. It is the prompt queue, and a
	// turn's end is what makes the queue DELIVER the next prompt -- which
	// opens a turn back on this watcher and needs this very mutex. Told
	// inside the lock it is a self-deadlock, and every hold waiting on a turn
	// end (and every one-shot's finish action) simply never happened. The
	// VIEW sinks stay inside the lock: they are pure consumers, and their
	// ordering per stream is the reason mu is held across them.
	w.pendingTurnEnds = append(w.pendingTurnEnds, endedTurn{turn: turn, how: how})
	w.rememberClosedTurnLocked(turn, how)
	for _, ch := range w.turnWaiters[turn] {
		ch <- turnEnd{how: how}
	}
	delete(w.turnWaiters, turn)
	w.signalFreenessLocked()
}

// standNextWaitingLocked stands the next turn waiting behind `ended` in
// flight, now that the turn ahead of it ended: the OLDEST prompt sent to join
// it when one waits, since the shim runs those in the order they were sent
// (the rest of them fold into it as the vendor's prompt batch), and otherwise
// the newest turn waiting. An accepted turn takes the open edges the views
// were not given while it waited; one still opening takes them from its own
// OnTurnOpened. It runs BEFORE the ended turn's end reaches the lifecycle
// sink, so the queue finds this turn running and pops nothing into it.
func (w *watcher) standNextWaitingLocked(ended ids.TurnID) {
	if len(w.waiting) == 0 {
		return
	}
	at := len(w.waiting) - 1
	for i, entry := range w.waiting {
		if entry.joined {
			at = i
			break
		}
	}
	next := w.waiting[at]
	w.waiting = append(w.waiting[:at], w.waiting[at+1:]...)
	turn := next.turn
	w.turn = &turn
	w.turnAccepted = next.accepted || next.adopted
	if next.adopted {
		w.adopted = &turn
	}
	w.log.Info("daemon.sessionwatcher.turn_resumed", "the turn ahead ended; the turn waiting behind it stands in flight", dlog.Context{
		"turn_id": string(turn), "ended_turn": string(ended), "accepted": next.accepted, "still_waiting": len(w.waiting),
	})
	if next.accepted || next.adopted {
		w.sinks.Footer.OnTurnOpened(w.ws, turn)
		w.sinks.Feed.OnTurnOpened(w.ws, turn)
	}
}

// dropWaitingLocked takes `turn` out of the waiting stack, reporting whether
// it was there.
func (w *watcher) dropWaitingLocked(turn ids.TurnID, why string) bool {
	for i := range w.waiting {
		if w.waiting[i].turn != turn {
			continue
		}
		w.waiting = append(w.waiting[:i], w.waiting[i+1:]...)
		w.log.Debug("daemon.sessionwatcher.turn_waiting_dropped", why, dlog.Context{
			"turn_id": string(turn), "turn_in_flight": ids.Optional(w.turn), "still_waiting": len(w.waiting),
		})
		return true
	}
	return false
}

// rememberClosedTurnLocked records a turn's close, evicting the oldest once
// the memory is full.
func (w *watcher) rememberClosedTurnLocked(turn ids.TurnID, how TurnClose) {
	if _, known := w.closedTurns[turn]; !known {
		w.closedTurnOrder = append(w.closedTurnOrder, turn)
	}
	w.closedTurns[turn] = how
	for len(w.closedTurnOrder) > closedTurnMemory {
		delete(w.closedTurns, w.closedTurnOrder[0])
		w.closedTurnOrder = w.closedTurnOrder[1:]
	}
}

// signalFreenessLocked answers every standing freeness waiter once the
// workspace is actually free. It is a no-op while anything is still in flight.
func (w *watcher) signalFreenessLocked() {
	if !w.freeLocked() {
		w.busy = true
		// ONLY WORK SEEN ENDS THE ATTACH'S GRACE: an attach not yet told its
		// facts is not free either, and that is no work.
		if w.started {
			w.attaching = false
		}
		return
	}
	if w.busy {
		w.busy = false
		workEnded := !w.attaching
		w.attaching = false
		w.freeEdgeLocked(workEnded)
	}
	if len(w.freeWaiters) == 0 {
		return
	}
	w.log.Debug("daemon.sessionwatcher.await_free", "the workspace is free; releasing the waiters", dlog.Context{
		"waiters": len(w.freeWaiters),
	})
	for _, ch := range w.freeWaiters {
		ch <- nil
	}
	w.freeWaiters = nil
}

// failWaitersLocked answers every standing wait with ErrWatcherClosed. A
// closed watcher can no longer produce the edge the waiter is waiting for, so
// the wait is ended loudly rather than left to the caller's context.
func (w *watcher) failWaitersLocked() {
	for _, ch := range w.freeWaiters {
		ch <- ErrWatcherClosed
	}
	w.freeWaiters = nil
	for turn, waiters := range w.turnWaiters {
		for _, ch := range waiters {
			ch <- turnEnd{err: ErrWatcherClosed}
		}
		delete(w.turnWaiters, turn)
	}
}

// dropFreeWaiter removes a waiter whose context ended, so an abandoned wait
// does not accumulate on a long-lived watcher.
func (w *watcher) dropFreeWaiter(ch chan error) {
	w.mu.Lock()
	defer w.mu.Unlock()
	for i, c := range w.freeWaiters {
		if c == ch {
			w.freeWaiters = append(w.freeWaiters[:i], w.freeWaiters[i+1:]...)
			return
		}
	}
}

// dropTurnWaiter removes a turn waiter whose context ended.
func (w *watcher) dropTurnWaiter(turn ids.TurnID, ch chan turnEnd) {
	w.mu.Lock()
	defer w.mu.Unlock()
	waiters := w.turnWaiters[turn]
	for i, c := range waiters {
		if c == ch {
			waiters = append(waiters[:i], waiters[i+1:]...)
			if len(waiters) == 0 {
				delete(w.turnWaiters, turn)
			} else {
				w.turnWaiters[turn] = waiters
			}
			return
		}
	}
}

// freeEdgeLocked tells the lifecycle sink the workspace just fell free, OFF
// the lock and joinable through the same WaitGroup the turn-end dispatch uses.
// The sink reads this watcher (the bounce registry judges freeness itself,
// under its own lock), so telling it inline would be a self-deadlock.
func (w *watcher) freeEdgeLocked(workEnded bool) {
	if w.closed {
		return
	}
	w.log.Debug("daemon.sessionwatcher.free", "the workspace fell free; telling the lifecycle sink", dlog.Context{
		"state": "busy", "before": true, "after": false, "work_ended": workEnded,
	})
	w.dispatching.Add(1)
	go func() {
		defer w.dispatching.Done()
		w.sinks.Lifecycle.OnFree(w.ws, workEnded)
	}()
}
