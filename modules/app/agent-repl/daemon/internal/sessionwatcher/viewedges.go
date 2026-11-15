package sessionwatcher

import (
	"sync"

	"agent-repld/internal/dlog"
	"agent-repld/internal/lockwatch"
)

// THE ROSTER AND THE TOPBAR ARE TOLD OFF THE WATCHER'S LOCK.
//
// Both views do work proportional to something other than this session: the
// roster re-renders the WHOLE sidebar for a fact about one row, and the
// topbar writes the store. Told under the watcher's lock, that work held the
// lock every TurnInFlight, every SubmitPrompt and every other stream of the
// workspace waits on: 60 watchers adopting at once at the 0.424.3 rollout
// queued on the roster's one mutex, each holding its own lock, and
// `daemon.lockwatch.stall` fired on `sessionwatcher.watcher` held past 6s
// (2026-11-12); a store busy with a handover held it the same way through the
// topbar's write (2026-11-10).
//
// A route therefore QUEUES its roster and topbar edges under the lock, and
// the goroutine that releases the lock hands them over, in the order they
// were queued, once it has released it. Delivery is serialized by a lock of
// its own, so two goroutines releasing one after the other can never hand a
// view its edges out of order, and an Unlock returns only once every edge its
// own hold queued has reached its view, exactly as a call made under the
// lock had.

// viewLockName is how the stall watchdog names the view-delivery lock.
const viewLockName = "sessionwatcher.views"

// edgeMutex is the watcher's mutex: a lockwatch.Mutex whose Unlock hands
// over the view edges queued under it.
type edgeMutex struct {
	lockwatch.Mutex
	// pending are the edges queued under the mutex and not yet taken for
	// delivery. Read and written only while the mutex is held.
	pending []func()
	// delivering serializes delivery: whoever holds it hands over every edge
	// pending when it took them, in order.
	delivering lockwatch.Mutex
	// inflight counts the deliveries begun, so Close joins a delivery that
	// took its edges before Close took the mutex.
	inflight sync.WaitGroup
	// log records each delivery; nil records nothing (a zero edgeMutex).
	log dlog.Logger
}

// later queues EDGE for delivery once the mutex is released. Caller holds
// the mutex.
func (m *edgeMutex) later(edge func()) {
	m.pending = append(m.pending, edge)
}

// Unlock releases the mutex and, when this hold queued edges, hands every
// pending edge to its view before returning.
//
// A HOLD THAT QUEUED NOTHING RETURNS AT ONCE: the queue is read while the
// mutex is still held, and only a holder appends to it, so an empty queue at
// release means this hold queued nothing still undelivered.
func (m *edgeMutex) Unlock() {
	if len(m.pending) == 0 {
		m.Mutex.Unlock()
		return
	}
	m.inflight.Add(1)
	m.Mutex.Unlock()
	defer m.inflight.Done()
	m.deliver()
}

// deliver hands over every pending edge, in order, holding the delivery lock
// and not the mutex. A delivery that finds the queue already taken by the one
// before it has nothing left to do: that delivery handed this hold's edges
// over before releasing the delivery lock this one waited on.
func (m *edgeMutex) deliver() {
	m.delivering.Lock()
	defer m.delivering.Unlock()
	m.Mutex.Lock()
	edges := m.pending
	m.pending = nil
	m.Mutex.Unlock()
	for _, edge := range edges {
		edge()
	}
	if len(edges) > 0 && m.log != nil {
		m.log.Debug("daemon.sessionwatcher.view_edges_delivered", "the roster and topbar edges queued under the watcher's lock were handed over after it was released", dlog.Context{
			"edges": len(edges),
		})
	}
}

// awaitDelivered returns once every delivery begun has ended.
func (m *edgeMutex) awaitDelivered() {
	m.inflight.Wait()
}
