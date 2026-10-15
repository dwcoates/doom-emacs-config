package sidebar

import (
	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"agent-repld/internal/wsm"
)

// Tab is one tab of the editor's tab bar: its workspace, and whether the bar
// withholds it.
type Tab struct {
	Workspace wsm.Workspace
	Withheld  wsm.Withholding
}

// Tabs answers the editor's tab bar: every open workspace in the
// ARRANGEMENT'S order (arrangement.go): repository by repository, and inside
// one, its unsectioned workspaces and then each subsection's, each run in
// roster order. A closed workspace has no tab. A tab is withheld, not
// dropped, when the user hid its workspace or its repository's section is
// collapsed, so a client cycling from a withheld current workspace still
// knows its neighbours. A merged workspace files under recently merged
// whatever its hidden flag (wsm.Workspace.Withheld, the one statement of
// visibility the daemon also runs sessions by). A hidden merge workspace has
// no tab at all until it is revealed (wsm.Workspace.MergeHidden). It is THE
// ONE STATEMENT of the tab bar: the roster draws it (tabBar), and hiding the
// current workspace lands the user on its neighbour in it.
func Tabs(reg Registry) []Tab {
	var open []wsm.Workspace
	for _, ws := range reg.Workspaces {
		if !ws.Closed && !ws.MergeHidden() {
			open = append(open, ws)
		}
	}
	arranged, _ := arrange(reg, open)
	var out []Tab
	for _, placed := range arranged {
		for _, b := range placed.buckets {
			for _, ws := range b.workspaces {
				out = append(out, Tab{Workspace: ws, Withheld: ws.Withheld(placed.repo.Folded)})
			}
		}
	}
	return out
}

// tabBar resolves the editor's tab bar from Tabs.
func (r *resolver) tabBar() *frontendv1.RosterTabBar {
	out := &frontendv1.RosterTabBar{}
	for _, t := range Tabs(r.state.reg) {
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
