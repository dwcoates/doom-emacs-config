// Package turnlatch answers latches closed when a workspace's turn next ends.
//
// A WAIT ON A TURN IS A LATCH, NEVER A POLL. A caller takes the latch BEFORE it
// checks whether the turn it waits on is still running, so a turn that ends
// between the check and the wait is never missed: the latch is already closed
// by then. The latches are moved only by the lifecycle edges (wsedges): a turn
// ending, turns an adoption found ended while no daemon watched, and the
// workspace closing, which ends every turn it will ever run.
package turnlatch

import (
	"sync"

	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// Latches is the set of standing latches, keyed by workspace. It implements
// wsedges.Observer; every method returns at once.
type Latches struct {
	mu      sync.Mutex
	waiting map[ids.WorkspaceID][]chan struct{}
}

// New builds an empty set.
func New() *Latches { return &Latches{waiting: map[ids.WorkspaceID][]chan struct{}{}} }

// Next answers a latch closed when a turn of WS next ends, or when WS closes.
func (l *Latches) Next(ws ids.WorkspaceID) <-chan struct{} {
	latch := make(chan struct{})
	l.mu.Lock()
	l.waiting[ws] = append(l.waiting[ws], latch)
	l.mu.Unlock()
	return latch
}

// release closes every latch standing for WS.
func (l *Latches) release(ws ids.WorkspaceID) {
	l.mu.Lock()
	latches := l.waiting[ws]
	delete(l.waiting, ws)
	l.mu.Unlock()
	for _, latch := range latches {
		close(latch)
	}
}

// TurnEnded implements wsedges.Observer.
func (l *Latches) TurnEnded(ws ids.WorkspaceID) { l.release(ws) }

// TurnEndedUnobserved implements wsedges.Observer.
func (l *Latches) TurnEndedUnobserved(ws ids.WorkspaceID) { l.release(ws) }

// Closed implements wsedges.Observer: a closed workspace ends no further turn.
func (l *Latches) Closed(before wsm.Workspace) { l.release(before.ID) }

// TurnStarted implements wsedges.Observer: no latch waits on a start.
func (l *Latches) TurnStarted(ids.WorkspaceID) {}

// NeedsInput implements wsedges.Observer: no latch waits on an ask.
func (l *Latches) NeedsInput(ids.WorkspaceID) {}

// Landed implements wsedges.Observer: no latch waits on a landing.
func (l *Latches) Landed(ids.WorkspaceID) {}

// PullRequestOwned implements wsedges.Observer: no latch waits on a link.
func (l *Latches) PullRequestOwned(ids.WorkspaceID, wsm.PullRequestKey) {}
