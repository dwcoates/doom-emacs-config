// Package wsedges is the ONE fan-out of a workspace's lifecycle edges -- a
// turn starting and ending, an ask opening, the workspace's branch landing,
// the workspace closing, the workspace taking a pull request as its own, the
// build carrying its landing running, its merge leaving the queue unlanded,
// its session parking at the cold gate -- to every party that tells agents about them (the
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
//     goes through (ReportingEdges);
//   - a workspace takes a pull request as its own in the registry's
//     SetWorkspaceLink, the one write of an owned link (ReportingEdges);
//   - a landing's deploy ends where the deploy's payment ends (DeployEnded),
//     naming every workspace whose landing its build carries;
//   - a merge leaves the queue unlanded where the merge queue reports it
//     (merge.QueueChangeFailed and merge.QueueChangeDequeued), whatever the
//     reason;
//   - a session parks at its cold gate where a shim judges it
//     (workspace.Fleet's ColdGateRaised), not where a recorded gate is raised
//     again;
//   - a workspace is retargeted (its parent or its landing target changes)
//     in the registry's SetParent and RestoreParent, which every retarget goes
//     through (ReportingEdges).
//
// AN OBSERVER NEVER BLOCKS AN EDGE: every method only appends, and its
// delivery runs elsewhere.
package wsedges

import (
	"context"
	"fmt"
	"strings"

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
	// so its MergedAt says whether its branch landed. HOW is how it ended
	// (closed, killed or nuked).
	Closed(before wsm.Workspace, how wsm.CloseHow)
	// PullRequestOwned: WS stored an owned link to PR, a GitHub pull request
	// it did not own before.
	PullRequestOwned(ws ids.WorkspaceID, pr wsm.PullRequestKey)
	// Deployed: the deploy that carries WS's landing ended. FAILURE is why it
	// failed, and "" for a build that is now running.
	Deployed(ws ids.WorkspaceID, failure string)
	// Evicted: WS's merge left its queue without landing. REASON says why in
	// the queue's own words (its recorded cause).
	Evicted(ws ids.WorkspaceID, reason string)
	// ColdGated: WS's session parked at its cold gate, as a shim judged it
	// just now.
	ColdGated(ws ids.WorkspaceID)
	// Retargeted: a workspace's parent or landing target changed, as CHANGE
	// records it (wsm.ParentChange): it left one family and joined another.
	Retargeted(change wsm.ParentChange)
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
func (h *Hub) Closed(before wsm.Workspace, how wsm.CloseHow) {
	for _, o := range h.observers {
		o.Closed(before, how)
	}
}

// PullRequestOwned implements Observer.
func (h *Hub) PullRequestOwned(ws ids.WorkspaceID, pr wsm.PullRequestKey) {
	for _, o := range h.observers {
		o.PullRequestOwned(ws, pr)
	}
}

// Retargeted implements Observer.
func (h *Hub) Retargeted(change wsm.ParentChange) {
	for _, o := range h.observers {
		o.Retargeted(change)
	}
}

// Deployed implements Observer.
func (h *Hub) Deployed(ws ids.WorkspaceID, failure string) {
	for _, o := range h.observers {
		o.Deployed(ws, failure)
	}
}

// Evicted implements Observer.
func (h *Hub) Evicted(ws ids.WorkspaceID, reason string) {
	for _, o := range h.observers {
		o.Evicted(ws, reason)
	}
}

// ColdGated implements Observer.
func (h *Hub) ColdGated(ws ids.WorkspaceID) {
	for _, o := range h.observers {
		o.ColdGated(ws)
	}
}

// QueueChanged implements merge.QueueObserver: a landed change is its
// subject's landing edge, and a failed or dequeued one is its subject's
// eviction.
func (h *Hub) QueueChanged(change merge.QueueChange) {
	switch change.Kind {
	case merge.QueueChangeLanded:
		h.Landed(change.Subject.Workspace)
	case merge.QueueChangeFailed, merge.QueueChangeDequeued:
		h.Evicted(change.Subject.Workspace, evictionReason(change))
	}
}

// evictionReason states why a merge left its queue unlanded, in the queue's
// own words: its recorded cause, or, for a failed merge that recorded none,
// that it failed.
func evictionReason(change merge.QueueChange) string {
	if change.Cause == "" {
		return change.Kind.String()
	}
	return strings.ReplaceAll(change.Cause, "_", " ")
}

// DeployEnded is told each deploy's end (mergeworkspace.Service.PayOwed): it
// tells every workspace whose landing the deploy's build carries.
func (h *Hub) DeployEnded(end merge.DeployEnd) {
	for _, ws := range end.Landed {
		h.Deployed(ws, end.Failure)
	}
}

// Subscribed implements merge.QueueObserver: a queue subscription is no
// workspace edge.
func (h *Hub) Subscribed(ids.WorkspaceID, wsm.RepoKey, uint64) {}

// ReportingEdges answers DB with the edges its writes make told to HUB.
//
// A CLOSE of an open workspace is told: every close path -- the user's close
// and kill, a landing's close, the daemon's boot housekeeping -- goes through
// MarkClosed, so wrapping it is what makes a close that tells nobody
// unrepresentable, rather than a hook each path must remember. Re-closing a
// closed row tells nothing.
//
// A PULL REQUEST TAKEN AS THE WORKSPACE'S OWN is told: an owned link is
// stored only through SetWorkspaceLink, and a link that takes a new
// subscription (wsm.LinkSet.Subscribe) is a pull request the workspace did not
// own before. Storing it again, or relabeling it, takes none and tells
// nothing.
func ReportingEdges(db wsm.DB, hub *Hub, log dlog.Logger) wsm.DB {
	return reportingDB{DB: db, hub: hub, log: log}
}

type reportingDB struct {
	wsm.DB
	hub *Hub
	log dlog.Logger
}

func (d reportingDB) MarkClosed(ctx context.Context, id wsm.WorkspaceID, closure wsm.Closure) error {
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
		d.hub.Closed(before, closure.How)
	}
	return nil
}

func (d reportingDB) SetParent(ctx context.Context, id wsm.WorkspaceID, retarget wsm.Retarget) (wsm.ParentChange, error) {
	change, err := d.DB.SetParent(ctx, id, retarget)
	if err != nil {
		return wsm.ParentChange{}, err
	}
	d.hub.Retargeted(change)
	return change, nil
}

func (d reportingDB) RestoreParent(ctx context.Context, change wsm.ParentChange) (bool, error) {
	stale, err := d.DB.RestoreParent(ctx, change)
	if err != nil || stale {
		return stale, err
	}
	d.hub.Retargeted(wsm.ParentChange{Workspace: change.Workspace, Before: change.After, After: change.Before})
	return false, nil
}

func (d reportingDB) SetWorkspaceLink(ctx context.Context, id wsm.WorkspaceID, set wsm.LinkSet) ([]wsm.WorkspaceLink, error) {
	links, err := d.DB.SetWorkspaceLink(ctx, id, set)
	if err != nil {
		return nil, err
	}
	if set.Owned && set.Subscribe != nil {
		if set.PullRequest == nil {
			panic(fmt.Sprintf("wsedges: workspace %s stored an owned link %q taking a pull-request subscription without naming the pull request", id, set.URL))
		}
		d.hub.PullRequestOwned(id, *set.PullRequest)
	}
	return links, nil
}
