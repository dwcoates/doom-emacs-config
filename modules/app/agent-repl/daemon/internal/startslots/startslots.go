// Package startslots is the daemon-wide cap on vendor starts in flight, shared
// by the fleet's bring-ups and the bounce registry's shim restarts.
//
// A DAEMON-WIDE CAP ON VENDOR STARTS IN FLIGHT (2026-10-12). Every bring-up's
// start runs the vendor, the heaviest process a workspace spawns, and after a
// bounce every workspace starts at once: thirty Claude processes starting
// together on a loaded host each took longer than the shim's 3s liveness
// bound, so each was killed, retried every few seconds on the backoff and
// killed again, and the retries themselves kept the host saturated. With a few
// starts in flight at a time, each start gets the processor it needs to answer,
// and the rest wait their turn.
//
// A SHIM RESTART WAITS IN LINE TO RESTART, NOT TO START (owner ruling,
// 2026-10-17). The cap once covered only the resume's vendor start, which runs
// AFTER the old shim was stood down, so a deploy stood every workspace's shim
// down at once and left them all session-less while they queued for a start
// (22:19:51: thirty shims down in a second, 130 failed starts, two minutes of
// vendor faults). A restart now takes its slot BEFORE anything is stood down
// and keeps it for the whole replacement; the workspace keeps serving on its
// old shim while it waits. The slot is held BY WORKSPACE, so the restart's own
// resume, which asks again, is answered from the slot its restart holds.
//
// THE LINE IS FIRST COME, FIRST SERVED. A released slot is handed to the
// longest waiter, a blocked start or a restart's callback alike.
package startslots

import (
	"context"
	"runtime"
	"sync"

	"agent-repld/internal/ids"
)

// DefaultSize is how many vendor starts run at once: a quarter of the
// processors, at least two.
func DefaultSize() int {
	if n := runtime.NumCPU() / 4; n > 2 {
		return n
	}
	return 2
}

// Release gives a slot back. Calling it more than once is a no-op.
type Release func()

// waiter is one place in line: a blocked Acquire (granted is closed when the
// slot is its) or a Notify callback (handed the slot it now owns).
type waiter struct {
	ws      ids.WorkspaceID
	granted chan struct{}
	notify  func(Release)
}

// Pool is the cap: a fixed number of slots, each held by one workspace.
type Pool struct {
	mu      sync.Mutex
	size    int
	holders map[ids.WorkspaceID]int
	held    int
	line    []*waiter
	// lined, when set, is told each time a waiter joins the line: a suite's
	// synchronization point, never set in production.
	lined func()
}

// New answers a pool of size slots, at least one.
func New(size int) *Pool {
	if size < 1 {
		size = 1
	}
	return &Pool{size: size, holders: map[ids.WorkspaceID]int{}}
}

// Size answers how many slots the pool has.
func (p *Pool) Size() int { return p.size }

// Holds reports whether ws holds a slot now.
func (p *Pool) Holds(ws ids.WorkspaceID) bool {
	p.mu.Lock()
	defer p.mu.Unlock()
	return p.holders[ws] > 0
}

// TryAcquire takes a slot for ws when one is free and nobody waits ahead,
// without waiting. A workspace that already holds one is answered a no-op
// release: its slot covers this ask too.
func (p *Pool) TryAcquire(ws ids.WorkspaceID) (Release, bool) {
	p.mu.Lock()
	defer p.mu.Unlock()
	if p.holders[ws] > 0 {
		return func() {}, true
	}
	if p.held >= p.size || len(p.line) > 0 {
		return nil, false
	}
	return p.takeLocked(ws), true
}

// Acquire takes a slot for ws, waiting in line for one, and answers its
// release; it answers the context's cause when the context ends first. A
// workspace that already holds one is answered a no-op release at once.
func (p *Pool) Acquire(ctx context.Context, ws ids.WorkspaceID) (Release, error) {
	p.mu.Lock()
	if p.holders[ws] > 0 {
		p.mu.Unlock()
		return func() {}, nil
	}
	if p.held < p.size && len(p.line) == 0 {
		release := p.takeLocked(ws)
		p.mu.Unlock()
		return release, nil
	}
	w := &waiter{ws: ws, granted: make(chan struct{})}
	p.line = append(p.line, w)
	lined := p.lined
	p.mu.Unlock()
	if lined != nil {
		lined()
	}
	select {
	case <-w.granted:
		return p.releaser(ws), nil
	case <-ctx.Done():
		p.mu.Lock()
		if p.dropLocked(w) {
			p.mu.Unlock()
			return nil, context.Cause(ctx)
		}
		p.mu.Unlock()
		// THE SLOT WAS GRANTED AS THE CONTEXT ENDED: it is ours, so it is
		// handed on rather than leaked.
		p.releaser(ws)()
		return nil, context.Cause(ctx)
	}
}

// Notify puts ws in line and calls fn, off the pool's lock and on its own
// goroutine, with the slot it now holds once its turn comes. fn owns that
// slot and must release it, even when it no longer wants it. A workspace
// already in line by Notify is not put in line twice; Notify answers whether
// it was put in line now.
func (p *Pool) Notify(ws ids.WorkspaceID, fn func(Release)) bool {
	p.mu.Lock()
	defer p.mu.Unlock()
	for _, w := range p.line {
		if w.ws == ws && w.notify != nil {
			return false
		}
	}
	// A SLOT FREE WITH NOBODY AHEAD is this caller's turn already: it is
	// handed over exactly as a released one would be, so no ask is ever left
	// waiting on a release that is not coming.
	if p.held < p.size && len(p.line) == 0 {
		release := p.takeLocked(ws)
		go fn(release)
		return true
	}
	p.line = append(p.line, &waiter{ws: ws, notify: fn})
	return true
}

// Withdraw takes ws's Notify place out of the line, if it has one, and
// answers whether it did.
func (p *Pool) Withdraw(ws ids.WorkspaceID) bool {
	p.mu.Lock()
	defer p.mu.Unlock()
	for _, w := range p.line {
		if w.ws == ws && w.notify != nil {
			return p.dropLocked(w)
		}
	}
	return false
}

// Waiting answers how many are in line.
func (p *Pool) Waiting() int {
	p.mu.Lock()
	defer p.mu.Unlock()
	return len(p.line)
}

func (p *Pool) takeLocked(ws ids.WorkspaceID) Release {
	p.held++
	p.holders[ws]++
	return p.releaser(ws)
}

// releaser answers the one release of a slot ws holds.
func (p *Pool) releaser(ws ids.WorkspaceID) Release {
	var once sync.Once
	return func() { once.Do(func() { p.release(ws) }) }
}

// release gives ws's slot back and hands it to the longest waiter.
func (p *Pool) release(ws ids.WorkspaceID) {
	p.mu.Lock()
	p.held--
	if p.holders[ws]--; p.holders[ws] <= 0 {
		delete(p.holders, ws)
	}
	if len(p.line) == 0 {
		p.mu.Unlock()
		return
	}
	next := p.line[0]
	p.line = p.line[1:]
	p.held++
	p.holders[next.ws]++
	p.mu.Unlock()
	if next.notify != nil {
		go next.notify(p.releaser(next.ws))
		return
	}
	close(next.granted)
}

// dropLocked removes w from the line, answering whether it was still in it.
func (p *Pool) dropLocked(w *waiter) bool {
	for i, in := range p.line {
		if in == w {
			p.line = append(p.line[:i], p.line[i+1:]...)
			return true
		}
	}
	return false
}
