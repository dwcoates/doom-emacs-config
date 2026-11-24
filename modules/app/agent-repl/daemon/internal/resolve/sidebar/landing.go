package sidebar

import (
	"slices"

	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// Landing names which rule chose where the user lands when the workspace
// they stand on leaves the tab bar (closed, hidden, or folded away), for the
// record.
type Landing string

const (
	// LandedByAncestor: the most recently selected drawn ancestor.
	LandedByAncestor Landing = "ancestor"
	// LandedBySubsection: the most recently selected drawn tab of the
	// leaving workspace's subsection.
	LandedBySubsection Landing = "subsection"
	// LandedByRepository: the most recently selected drawn tab of the
	// leaving workspace's repository.
	LandedByRepository Landing = "repository"
	// LandedByMain: no drawn tab of the repository was ever selected, so its
	// main workspace.
	LandedByMain Landing = "main"
	// LandedByRecency: the repository draws no tab to land on, so the most
	// recently selected drawn tab of any repository.
	LandedByRecency Landing = "recency"
	// LandedByNeighbour: no other drawn tab was ever selected, so the
	// nearest drawn tab left of the leaving one, else right of it.
	LandedByNeighbour Landing = "neighbour"
)

// LandingTarget picks where the user lands when workspace FROM leaves the
// bar, among TABS (Tabs over REG). THE LANDING STAYS AS CLOSE TO FROM AS
// THE BAR ALLOWS (owner request, 2026-11-24), each tier taking its most
// recently selected drawn tab (wsm.Workspace.LastSelectedAt) and passing
// to the next when it holds none:
//
//  1. FROM's ancestors (its recorded Parent chain, closed links walked
//     through);
//  2. the other workspaces of FROM's subsection, when it sits in one;
//  3. the other workspaces of FROM's repository;
//  4. the repository's main workspace, selected or not;
//  5. any drawn tab of any repository.
//
// Tab order decides only when no other drawn tab was ever selected: the
// nearest drawn tab left of FROM's own, else the nearest to its right, or
// the leftmost when FROM has no tab (a closed workspace's).
//
// WITHIN, when set, names the workspaces a fold is about to take off the bar
// (wsm.FoldMembers), so a tab it will withhold is no landing: the roster asks
// this same function whether a collapse of the section holding the current
// workspace has anywhere to land (setRepoFold), and the fold verb asks it
// where to land, so the two can never disagree. It answers false when no
// other tab is drawn outside WITHIN.
func LandingTarget(reg Registry, tabs []Tab, from wsm.Workspace, within wsm.FoldMembers) (wsm.Workspace, Landing, bool) {
	at := slices.IndexFunc(tabs, func(t Tab) bool { return t.Workspace.ID == from.ID })
	lands := func(t Tab) bool {
		return t.Workspace.ID != from.ID && t.Withheld == wsm.Drawn && (within == nil || !within(t.Workspace))
	}
	ancestors := ancestorsOf(reg.Workspaces, from)
	repo, hasRepo := wsm.RepositoryWithID(reg.Repositories, from.Repo)
	tiers := []struct {
		by     Landing
		member func(wsm.Workspace) bool
	}{
		{LandedByAncestor, func(ws wsm.Workspace) bool { return ancestors[ws.ID] }},
		{LandedBySubsection, func(ws wsm.Workspace) bool {
			return from.Subsection != nil && ws.Repo == from.Repo && ws.Subsection != nil && *ws.Subsection == *from.Subsection
		}},
		{LandedByRepository, func(ws wsm.Workspace) bool { return ws.Repo == from.Repo }},
	}
	for _, tier := range tiers {
		if recent, ok := mostRecentlySelected(tabs, func(t Tab) bool { return lands(t) && tier.member(t.Workspace) }); ok {
			return recent, tier.by, true
		}
	}
	// A workspace whose repository the registry does not carry is an
	// invariant violation the roster asserts (assertRepositoryInvariant); it
	// has no main workspace to land on here.
	if hasRepo {
		if i := slices.IndexFunc(tabs, func(t Tab) bool {
			return lands(t) && t.Workspace.Repo == from.Repo && wsm.IsMainWorktree(t.Workspace.Dir, repo.Dir)
		}); i >= 0 {
			return tabs[i].Workspace, LandedByMain, true
		}
	}
	if recent, ok := mostRecentlySelected(tabs, lands); ok {
		return recent, LandedByRecency, true
	}
	for i := at - 1; i >= 0; i-- {
		if lands(tabs[i]) {
			return tabs[i].Workspace, LandedByNeighbour, true
		}
	}
	for i := at + 1; i < len(tabs); i++ {
		if lands(tabs[i]) {
			return tabs[i].Workspace, LandedByNeighbour, true
		}
	}
	return wsm.Workspace{}, "", false
}

// mostRecentlySelected answers the tab among TABS that CANDIDATE admits
// selected most recently, and false when CANDIDATE admits none ever
// selected.
func mostRecentlySelected(tabs []Tab, candidate func(Tab) bool) (wsm.Workspace, bool) {
	var recent *wsm.Workspace
	for i := range tabs {
		ws := &tabs[i].Workspace
		if !candidate(tabs[i]) || ws.LastSelectedAt == nil {
			continue
		}
		if recent == nil || ws.LastSelectedAt.After(*recent.LastSelectedAt) {
			recent = ws
		}
	}
	if recent == nil {
		return wsm.Workspace{}, false
	}
	return *recent, true
}

// ancestorsOf answers FROM's ancestors among ALL by their recorded Parent
// chain, closed ones included so the chain walks through them. A chain that
// cycles stops where it repeats, as nest renders a cycling parent at the top
// level rather than dropping it.
func ancestorsOf(all []wsm.Workspace, from wsm.Workspace) map[ids.WorkspaceID]bool {
	parents := make(map[ids.WorkspaceID]*ids.WorkspaceID, len(all))
	for _, ws := range all {
		parents[ws.ID] = ws.Parent
	}
	out := map[ids.WorkspaceID]bool{}
	for next := from.Parent; next != nil && *next != from.ID && !out[*next]; next = parents[*next] {
		out[*next] = true
	}
	return out
}
