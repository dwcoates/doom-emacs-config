package inflight

import (
	"context"
	"fmt"
	"sort"
	"strconv"
)

// A WORKSPACE'S WORK. Work a workspace owns (its session's watcher, its
// prompt queue's revival, its restart) is begun on that workspace's child
// (Workspace, ForWorkspace), which counts it on the daemon's tracker too, as
// owned. A wait scoped to some workspaces (AwaitIdleFor) waits for their
// work and for every piece of work no workspace owns, never for another
// workspace's: a test on a daemon other tests share asks about its own
// workspaces, and another case's long work (a revival, a reopen) no longer
// holds it. Work begun without a workspace stays unowned, so a site not yet
// told its workspace is waited for by every scoped wait, never skipped.

// Workspace answers WS's child of t, made at its first ask and kept: every
// piece of work begun on it is counted on t too, as WS's.
func (t *Tracker) Workspace(ws string) *Tracker {
	if ws == "" {
		panic("inflight: a workspace's tracker needs the workspace's id")
	}
	t.mu.Lock()
	defer t.mu.Unlock()
	if t.workspaces == nil {
		t.workspaces = map[string]*Tracker{}
	}
	child, ok := t.workspaces[ws]
	if !ok {
		child = &Tracker{parent: t, workspace: ws}
		t.workspaces[ws] = child
	}
	return child
}

// ForWorkspace answers WS's child of the daemon's tracker (Tracker.Workspace).
func ForWorkspace(ws string) *Tracker { return Default.Workspace(ws) }

// AwaitIdleFor waits until no work no workspace owns, and no work of any of
// WORKSPACES, is in flight on t, or ctx ends. It blocks on the counts'
// changes, never on a timer.
func (t *Tracker) AwaitIdleFor(ctx context.Context, workspaces []string) error {
	children := make([]*Tracker, 0, len(workspaces))
	for _, ws := range workspaces {
		children = append(children, t.Workspace(ws))
	}
	for {
		// THE CHANGE IS TAKEN BEFORE THE COUNTS ARE READ, and every child's
		// change is t's change too, so no change between the reads is missed.
		t.mu.Lock()
		if t.changed == nil {
			t.changed = make(chan struct{})
		}
		changed, unowned := t.changed, t.n-t.owned
		t.mu.Unlock()
		busy := unowned
		for _, c := range children {
			busy += c.InFlight()
		}
		if busy == 0 {
			return nil
		}
		select {
		case <-changed:
		case <-ctx.Done():
			return fmt.Errorf("inflight: %d piece(s) of work still in flight: %w", busy, ctx.Err())
		}
	}
}

// InFlightFor reports how much of what AwaitIdleFor(WORKSPACES) waits for is
// counted now, and where it was begun, "file:line xN", sorted, for a report.
func (t *Tracker) InFlightFor(workspaces []string) (int, []string) {
	t.mu.Lock()
	n := t.n - t.owned
	counts := map[string]int{}
	for site, c := range t.sites {
		if unowned := c - t.ownedSites[site]; unowned > 0 {
			counts[site] += unowned
		}
	}
	t.mu.Unlock()
	for _, ws := range workspaces {
		child := t.Workspace(ws)
		child.mu.Lock()
		n += child.n
		for site, c := range child.sites {
			counts[site] += c
		}
		child.mu.Unlock()
	}
	out := make([]string, 0, len(counts))
	for site, c := range counts {
		out = append(out, site+" x"+strconv.Itoa(c))
	}
	sort.Strings(out)
	return n, out
}
