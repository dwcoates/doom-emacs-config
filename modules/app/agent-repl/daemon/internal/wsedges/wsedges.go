// Package wsedges is the ONE fan-out of a workspace's lifecycle edges -- a
// turn starting and ending, an ask opening, the workspace's branch landing,
// the workspace closing -- to every party that tells agents about them (the
// derived-workspace notices, workspace subscriptions).
//
// EACH EDGE HAS ONE ORIGIN, so no observer can miss one another sees:
//   - a turn starts where the shim took it (promptqueue's turn acknowledgement);
//   - a turn ends where the prompt queue closes it, and a turn that ended
//     while no daemon watched is told as such where an adoption closes it;
//   - an ask opens where the session watcher routes a live question or
//     permission;
//   - a branch lands where the merge queue reports the landing
//     (merge.QueueChangeLanded), which covers a landing that keeps its
//     workspace open as well as one that closes it;
//   - a workspace closes in the registry's MarkClosed, which every close path
//     goes through (ReportingCloses).
//
// AN OBSERVER NEVER BLOCKS AN EDGE: every method only appends, and its
// delivery runs elsewhere.
package wsedges

import (
	"context"
	"fmt"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/merge"
	"agent-repld/internal/wsm"
)

const op = "daemon.wsedges"

// Observer is told each lifecycle edge. Every method returns at once.
type Observer interface {
	// TurnStarted: the shim took a turn of WS.
	TurnStarted(ws ids.WorkspaceID)
	// TurnEnded: a turn of WS ended, however it ended.
	TurnEnded(ws ids.WorkspaceID)
	// TurnEndedUnobserved: turns of WS ended while no daemon watched, and an
	// adoption closed them. No observer reports such an end as news (a
	// change no daemon served is never noticed); it settles what waited on
	// the turn.
	TurnEndedUnobserved(ws ids.WorkspaceID)
	// NeedsInput: a question or a permission of WS opened.
	NeedsInput(ws ids.WorkspaceID)
	// Landed: WS's merge landed in its target.
	Landed(ws ids.WorkspaceID)
	// Closed: an open workspace closed; BEFORE is it as it stood just before,
	// so its MergedAt says whether its branch landed.
	Closed(before wsm.Workspace)
}

// Hub fans every edge out to its observers, in the order they were given.
type Hub struct{ observers []Observer }

// New builds the hub.
func New(observers ...Observer) *Hub { return &Hub{observers: observers} }

// TurnStarted implements Observer.
func (h *Hub) TurnStarted(ws ids.WorkspaceID) {
	for _, o := range h.observers {
		o.TurnStarted(ws)
	}
}

// TurnEnded implements Observer.
func (h *Hub) TurnEnded(ws ids.WorkspaceID) {
	for _, o := range h.observers {
		o.TurnEnded(ws)
	}
}

// TurnEndedUnobserved implements Observer.
func (h *Hub) TurnEndedUnobserved(ws ids.WorkspaceID) {
	for _, o := range h.observers {
		o.TurnEndedUnobserved(ws)
	}
}

// NeedsInput implements Observer.
func (h *Hub) NeedsInput(ws ids.WorkspaceID) {
	for _, o := range h.observers {
		o.NeedsInput(ws)
	}
}

// Landed implements Observer.
func (h *Hub) Landed(ws ids.WorkspaceID) {
	for _, o := range h.observers {
		o.Landed(ws)
	}
}

// Closed implements Observer.
func (h *Hub) Closed(before wsm.Workspace) {
	for _, o := range h.observers {
		o.Closed(before)
	}
}

// QueueChanged implements merge.QueueObserver: a landed change is its
// subject's landing edge.
func (h *Hub) QueueChanged(change merge.QueueChange) {
	if change.Kind == merge.QueueChangeLanded {
		h.Landed(change.Subject.Workspace)
	}
}

// ReportingCloses answers DB with every close of an open workspace told to
// HUB. Every close path -- the user's close and kill, a landing's close, the
// daemon's boot housekeeping -- goes through MarkClosed, so wrapping it is
// what makes a close that tells nobody unrepresentable, rather than a hook
// each path must remember. Re-closing a closed row tells nothing.
func ReportingCloses(db wsm.DB, hub *Hub, log dlog.Logger) wsm.DB {
	return closingDB{DB: db, hub: hub, log: log}
}

type closingDB struct {
	wsm.DB
	hub *Hub
	log dlog.Logger
}

func (d closingDB) MarkClosed(ctx context.Context, id wsm.WorkspaceID, closure wsm.Closure) error {
	before, err := d.DB.Workspace(ctx, id)
	if err != nil {
		d.log.Error(op, "could not read a workspace before closing it, so the close is refused rather than left untold",
			dlog.WithCause(dlog.Context{"workspace": string(id), "closure_by": string(closure.By)}, err))
		return fmt.Errorf("wsedges: read %s before its close: %w", id, err)
	}
	if err := d.DB.MarkClosed(ctx, id, closure); err != nil {
		return err
	}
	if !before.Closed {
		d.hub.Closed(before)
	}
	return nil
}
