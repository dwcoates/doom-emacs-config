// Package creategate admits workspace creates and closes them out of a
// stand-down, so no create ever straddles one.
//
// A CREATE IS NEVER CUT OFF BY ITS OWN DAEMON (2026-10-23). A create runs
// detached from its request, under the daemon's lifetime, and nothing that
// ends the daemon waited for it: a handover snapshotted the workspaces it
// served while a create was still materializing one, and the exit then cut the
// create off wherever it stood, leaving a worktree and a branch with no
// workspace and no record of how it ended. The gate makes that ordering
// structural. Every create holds a ticket for its whole run; a stand-down
// closes the gate, which refuses or holds every create not yet admitted, and
// then waits for the tickets out, so the snapshot and the exit only ever see a
// create that has finished.
package creategate

import (
	"context"
	"fmt"
	"sync"
	"time"
)

// ClosedError refuses a create the gate did not admit: the daemon is standing
// down. Successor, when set, is the address of the daemon the same create is
// to be sent to.
type ClosedError struct {
	Successor string
}

func (e *ClosedError) Error() string {
	if e.Successor != "" {
		return fmt.Sprintf("creategate: this daemon is handing over to %s and admits no new create", e.Successor)
	}
	return "creategate: this daemon is standing down and admits no new create"
}

// Gate counts the creates in flight and admits new ones while it is open.
type Gate struct {
	mu        sync.Mutex
	inFlight  int
	closed    bool
	successor string
	// idle is closed when inFlight reaches zero; it exists only while
	// somebody waits for that.
	idle chan struct{}
	// reopened is closed by Reopen; Hold waits on it while the gate is shut.
	reopened chan struct{}
}

// New answers an open gate with nothing in flight.
func New() *Gate {
	return &Gate{reopened: make(chan struct{})}
}

type ticketKey struct{}

// Admit enters a create a caller asked for, refusing it with *ClosedError
// when the gate is closed. The returned context carries the ticket, so a
// create run under it is not entered a second time; release ends the ticket
// and is safe to call more than once.
func (g *Gate) Admit(ctx context.Context) (context.Context, func(), error) {
	if held(ctx) {
		return ctx, func() {}, nil
	}
	g.mu.Lock()
	defer g.mu.Unlock()
	if g.closed {
		return ctx, func() {}, &ClosedError{Successor: g.successor}
	}
	return g.enterLocked(ctx)
}

// Hold enters a create the daemon starts on its own (a merge workspace, a
// support workspace). While the gate is closed it waits for a Reopen rather
// than refusing, so a stand-down that goes ahead ends it through ctx before
// it has started anything, and one that is abandoned lets it run.
func (g *Gate) Hold(ctx context.Context) (context.Context, func(), error) {
	if held(ctx) {
		return ctx, func() {}, nil
	}
	for {
		g.mu.Lock()
		if !g.closed {
			defer g.mu.Unlock()
			return g.enterLocked(ctx)
		}
		reopened := g.reopened
		g.mu.Unlock()
		select {
		case <-reopened:
		case <-ctx.Done():
			return ctx, func() {}, fmt.Errorf("creategate: the daemon stood down before the create could start: %w", ctx.Err())
		}
	}
}

// enterLocked counts one create in. The caller holds g.mu.
func (g *Gate) enterLocked(ctx context.Context) (context.Context, func(), error) {
	g.inFlight++
	var once sync.Once
	return context.WithValue(ctx, ticketKey{}, true), func() { once.Do(g.leave) }, nil
}

func (g *Gate) leave() {
	g.mu.Lock()
	defer g.mu.Unlock()
	g.inFlight--
	if g.inFlight < 0 {
		panic("creategate: a create left that never entered")
	}
	if g.inFlight == 0 && g.idle != nil {
		close(g.idle)
		g.idle = nil
	}
}

// Close admits no new create from now on. SUCCESSOR names the daemon a
// refused create goes to, empty when none is listening; a gate already closed
// for a successor keeps it.
func (g *Gate) Close(successor string) {
	g.mu.Lock()
	defer g.mu.Unlock()
	if !g.closed {
		g.closed = true
		g.reopened = make(chan struct{})
	}
	if g.successor == "" {
		g.successor = successor
	}
}

// Reopen admits creates again after a stand-down that was abandoned, and lets
// every create Hold is keeping waiting start.
func (g *Gate) Reopen() {
	g.mu.Lock()
	defer g.mu.Unlock()
	if !g.closed {
		return
	}
	g.closed = false
	g.successor = ""
	close(g.reopened)
}

// Idle answers a channel that is closed once no create is in flight: at once
// when none is.
func (g *Gate) Idle() <-chan struct{} {
	g.mu.Lock()
	defer g.mu.Unlock()
	if g.inFlight == 0 {
		done := make(chan struct{})
		close(done)
		return done
	}
	if g.idle == nil {
		g.idle = make(chan struct{})
	}
	return g.idle
}

// AwaitIdle waits until no create is in flight. Each time EVERY passes first,
// as AFTER measures it, HOLDOUT is told how many waits have run out and how
// many creates are still in flight, so a long wait is named while it lasts.
// It answers ctx's error when ctx ends first.
func (g *Gate) AwaitIdle(ctx context.Context, every time.Duration, after func(time.Duration) <-chan time.Time, holdout func(warnings, inFlight int)) error {
	idle := g.Idle()
	for warnings := 1; ; warnings++ {
		select {
		case <-idle:
			return nil
		case <-ctx.Done():
			return ctx.Err()
		case <-after(every):
			holdout(warnings, g.InFlight())
		}
	}
}

// InFlight answers how many creates hold a ticket, for the record.
func (g *Gate) InFlight() int {
	g.mu.Lock()
	defer g.mu.Unlock()
	return g.inFlight
}

func held(ctx context.Context) bool {
	held, _ := ctx.Value(ticketKey{}).(bool)
	return held
}
