package workspace

import (
	"context"
	"errors"
	"fmt"

	"agent-repld/internal/ids"
	"agent-repld/internal/shimclient"
)

// ErrWorkspaceForgotten is a start refused, or ended, because its workspace
// is being forgotten (FenceStarts): its row is about to be deleted, and a
// shim spawned for it would be an orphan no verb could address. It IS a
// stand-down this daemon ordered (shimclient.ErrStandDownOrdered), so every
// caller that reads a revival cut short by one as ordinary
// (bringup.IsStandDown) reads this one so too.
var ErrWorkspaceForgotten = fmt.Errorf("workspace: the workspace is being forgotten: %w", shimclient.ErrStandDownOrdered)

// endedByFence answers ERR, the outcome of a start whose context is CTX, with
// ErrWorkspaceForgotten attached when the forget's fence ended it: the start
// itself saw only its context cancelled.
func endedByFence(ctx context.Context, ws ids.WorkspaceID, err error) error {
	if err == nil || !errors.Is(context.Cause(ctx), ErrWorkspaceForgotten) || errors.Is(err, ErrWorkspaceForgotten) {
		return err
	}
	return fmt.Errorf("start session for %q: %w: %w", ws, ErrWorkspaceForgotten, err)
}

// A FORGET AND A START NEVER OVERLAP (2026-11-23: a dead shim's revival
// spawned a shim after the test's cleanup had forgotten its workspace, so the
// spawn could not be recorded and its records had no sink). A forget fences
// the workspace's starts before it judges the session and deletes the row:
//
//   - the fence is a mark every start reads under the start gate, so a start
//     asked after it begins nothing;
//   - the start under way is ENDED, by the cancel scoped to it, rather than
//     waited on, since a start can wait on its shim's attach without bound;
//   - the forget then takes the start gate, so it proceeds only once that
//     start has returned, and stands down whatever it spawned.

// enterStart is a start's entry under WS's start gate, which the caller
// holds: refused once a forget fenced WS, else answering the start's own
// context, which a forget cancels, and the release the start defers.
func (f *Fleet) enterStart(ctx context.Context, ws ids.WorkspaceID) (context.Context, func(), error) {
	f.mu.Lock()
	defer f.mu.Unlock()
	if f.fenced[ws] {
		return nil, nil, fmt.Errorf("start session for %q: %w", ws, ErrWorkspaceForgotten)
	}
	start, cancel := context.WithCancelCause(ctx)
	f.startCancels[ws] = cancel
	return start, func() {
		f.mu.Lock()
		delete(f.startCancels, ws)
		f.mu.Unlock()
		cancel(nil)
	}, nil
}

// FenceStarts fences WS's starts for a forget: no start begins from now, the
// start under way is cancelled, and FenceStarts returns once it has returned,
// holding the start gate. RELEASE lifts the fence and frees the gate; a forget
// that deleted the row leaves nothing for a later start but the row's absence.
// CTX bounds the wait for the cancelled start to return; a start that does not
// return within it lifts the fence and answers the context's error.
func (f *Fleet) FenceStarts(ctx context.Context, ws ids.WorkspaceID) (release func(), err error) {
	f.mu.Lock()
	f.fenced[ws] = true
	cancel := f.startCancels[ws]
	f.mu.Unlock()
	if cancel != nil {
		cancel(ErrWorkspaceForgotten)
	}
	lift := func() {
		f.mu.Lock()
		delete(f.fenced, ws)
		f.mu.Unlock()
	}
	gate := f.startGate(ws)
	locked := make(chan struct{})
	abandoned := make(chan struct{})
	go func() {
		gate.Lock()
		select {
		case locked <- struct{}{}:
		case <-abandoned:
			gate.Unlock()
		}
	}()
	select {
	case <-locked:
		return func() {
			gate.Unlock()
			lift()
		}, nil
	case <-ctx.Done():
		close(abandoned)
		lift()
		return nil, fmt.Errorf("fence the starts of %q: the start under way did not return: %w", ws, ctx.Err())
	}
}
