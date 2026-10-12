package sidebar

import (
	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"
	"agentrepl/protohelpers/rosterwalk"

	"agent-repld/internal/ids"
)

// tabBar resolves the editor's tab bar: every open workspace in REGISTRY
// ORDER (sortWorkspaces), independent of the groupings, so repositories
// interleave and a child need not follow its parent. A closed workspace has no
// tab. A tab is withheld, not dropped, when the user hid its workspace or its
// repository's section is collapsed, so a client cycling from a withheld
// current workspace still knows its neighbours. A merged workspace files under
// recently merged whatever its hidden flag, so only its repository's fold can
// withhold its tab, exactly as its merged group follows that fold. A hidden
// merge workspace has no tab at all until it is revealed (wsm.Workspace.MergeHidden).
func (r *resolver) tabBar() *frontendv1.RosterTabBar {
	folded := map[ids.RepoID]bool{}
	for _, repo := range r.state.reg.Repositories {
		folded[repo.ID] = repo.Folded
	}
	out := &frontendv1.RosterTabBar{}
	for _, ws := range sortWorkspaces(r.state.reg.Workspaces) {
		if ws.Closed || ws.MergeHidden() {
			continue
		}
		tab := &frontendv1.RosterTab{Workspace: &workspacev1.WorkspaceRef{Id: string(ws.ID), Dir: ws.Dir}}
		switch {
		case ws.MergedAt == nil && ws.Hidden:
			tab.Visibility = &frontendv1.RosterTab_WithheldHidden{WithheldHidden: &frontendv1.RosterTabWithheldHidden{}}
		case folded[ws.Repo]:
			tab.Visibility = &frontendv1.RosterTab_WithheldCollapsed{WithheldCollapsed: &frontendv1.RosterTabWithheldCollapsed{}}
		default:
			tab.Visibility = &frontendv1.RosterTab_Drawn{Drawn: &frontendv1.RosterTabDrawn{}}
		}
		out.Tabs = append(out.Tabs, tab)
	}
	return out
}

// TabEntry is one open workspace in the editor's TAB ORDER.
type TabEntry struct {
	// Ref is the workspace's ref as the roster carries it.
	Ref *workspacev1.WorkspaceRef
	// Name is its display name.
	Name string
}

// TabOrder is the editor's tab order as the roster publishes it
// (WorkspaceRoster.tab_bar), withheld tabs included, each named by its row.
// It reads the published list rather than deriving one, so the daemon and the
// editor can never disagree about which workspace is first. Nil before the
// roster has been published.
func TabOrder(roster *frontendv1.WorkspaceRoster) []TabEntry {
	names := map[string]string{}
	for _, row := range rosterwalk.AllRows(roster) {
		names[row.GetWorkspace().GetWorkspace().GetId()] = row.GetName().GetText()
	}
	var out []TabEntry
	for _, tab := range roster.GetTabBar().GetTabs() {
		out = append(out, TabEntry{Ref: tab.GetWorkspace(), Name: names[tab.GetWorkspace().GetId()]})
	}
	return out
}
