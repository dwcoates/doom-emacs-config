package sidebar

import (
	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/wsm"
)

// Tab is one tab of the editor's tab bar: its workspace, and whether the bar
// withholds it.
type Tab struct {
	Workspace wsm.Workspace
	Withheld  wsm.Withholding
}

// Tabs answers the editor's tab bar: every open workspace in the
// ARRANGEMENT'S order (arrangement.go), repository by repository, and inside
// one, its unsectioned workspaces and then each subsection's.
//
// INSIDE EACH RUN THE BAR WALKS THE SIDEBAR'S OWN FAMILY TREE (owner
// requirement, 2026-10-14): the same nest() forest the repository grouping
// draws, depth-first, so a child always sits contiguously right of its
// parent and its siblings in the sidebar's order, and stepping right on the
// bar never skips a row the sidebar draws between. A closed workspace has no
// tab, and its live descendants take its place, exactly as the sidebar hoists
// them. The run's workspaces the grouping does not draw (hidden ones, and
// merged ones still open) follow its drawn ones, in roster order.
//
// A tab is withheld, not dropped, when the user hid its workspace or its
// repository's section is collapsed, so a client cycling from a withheld
// current workspace still knows its neighbours. A merged workspace files
// under recently merged whatever its hidden flag (wsm.Workspace.Withheld, the
// one statement of visibility the daemon also runs sessions by). A hidden
// merge workspace has no tab at all until it is revealed
// (wsm.Workspace.MergeHidden). It is THE ONE STATEMENT of the tab bar: the
// roster draws it (tabBar), and hiding the current workspace lands the user
// on its neighbour in it.
func Tabs(reg Registry, log dlog.Logger) []Tab {
	var tabbed []wsm.Workspace
	for _, ws := range reg.Workspaces {
		if !ws.MergeHidden() {
			tabbed = append(tabbed, ws)
		}
	}
	arranged, _ := arrange(reg, tabbed)
	var out []Tab
	add := func(ws wsm.Workspace, folded bool) {
		if !ws.Closed {
			out = append(out, Tab{Workspace: ws, Withheld: ws.Withheld(folded)})
		}
	}
	for _, placed := range arranged {
		for _, b := range placed.buckets {
			var drawn, undrawn []wsm.Workspace
			for _, ws := range b.workspaces {
				if ws.Hidden || ws.MergedAt != nil {
					undrawn = append(undrawn, ws)
					continue
				}
				drawn = append(drawn, ws)
			}
			tree := nest(drawn, log)
			tree.walk(func(ws wsm.Workspace) { add(ws, placed.repo.Folded) })
			for _, ws := range undrawn {
				add(ws, placed.repo.Folded)
			}
		}
	}
	return out
}

// tabBar resolves the editor's tab bar from Tabs.
func (r *resolver) tabBar(log dlog.Logger) *frontendv1.RosterTabBar {
	out := &frontendv1.RosterTabBar{}
	for _, t := range Tabs(r.state.reg, log) {
		tab := &frontendv1.RosterTab{Workspace: &workspacev1.WorkspaceRef{Id: string(t.Workspace.ID), Dir: t.Workspace.Dir}}
		switch t.Withheld {
		case wsm.WithheldHidden:
			tab.Visibility = &frontendv1.RosterTab_WithheldHidden{WithheldHidden: &frontendv1.RosterTabWithheldHidden{}}
		case wsm.WithheldCollapsed:
			tab.Visibility = &frontendv1.RosterTab_WithheldCollapsed{WithheldCollapsed: &frontendv1.RosterTabWithheldCollapsed{}}
		default:
			tab.Visibility = &frontendv1.RosterTab_Drawn{Drawn: &frontendv1.RosterTabDrawn{}}
		}
		out.Tabs = append(out.Tabs, tab)
	}
	return out
}
