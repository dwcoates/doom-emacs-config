package sidebar

import (
	"slices"

	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// Landing names which rule chose where the user lands when the workspace
// they stand on leaves the tab bar (hidden, or folded away), for the record.
type Landing string

const (
	// LandedByRecency: the most recently selected drawn tab.
	LandedByRecency Landing = "recency"
	// LandedByNeighbour: no other drawn tab was ever selected, so the
	// nearest drawn tab left of the leaving one, else right of it.
	LandedByNeighbour Landing = "neighbour"
)

// LandingTarget picks where the user lands when workspace FROM leaves the
// bar, among TABS (Tabs): the drawn tab other than FROM selected most
// recently (wsm.Workspace.LastSelectedAt), the one the user was on before.
// Tab order decides only when no other drawn tab was ever selected: the
// nearest drawn tab left of FROM's own, else the nearest to its right.
//
// WITHIN, when set, names the workspaces a fold is about to take off the bar
// (wsm.FoldMembers), so a tab it will withhold is no landing: the roster asks
// this same function whether a collapse of the section holding the current
// workspace has anywhere to land (setRepoFold), and the fold verb asks it
// where to land, so the two can never disagree. It answers false when FROM
// has no tab, or no other tab is drawn outside WITHIN.
func LandingTarget(tabs []Tab, from ids.WorkspaceID, within wsm.FoldMembers) (wsm.Workspace, Landing, bool) {
	at := slices.IndexFunc(tabs, func(t Tab) bool { return t.Workspace.ID == from })
	if at < 0 {
		return wsm.Workspace{}, "", false
	}
	lands := func(i int) bool {
		return i != at && tabs[i].Withheld == wsm.Drawn && (within == nil || !within(tabs[i].Workspace))
	}
	var recent *wsm.Workspace
	for i := range tabs {
		ws := &tabs[i].Workspace
		if !lands(i) || ws.LastSelectedAt == nil {
			continue
		}
		if recent == nil || ws.LastSelectedAt.After(*recent.LastSelectedAt) {
			recent = ws
		}
	}
	if recent != nil {
		return *recent, LandedByRecency, true
	}
	for i := at - 1; i >= 0; i-- {
		if lands(i) {
			return tabs[i].Workspace, LandedByNeighbour, true
		}
	}
	for i := at + 1; i < len(tabs); i++ {
		if lands(i) {
			return tabs[i].Workspace, LandedByNeighbour, true
		}
	}
	return wsm.Workspace{}, "", false
}
