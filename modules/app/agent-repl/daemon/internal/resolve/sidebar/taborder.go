package sidebar

import (
	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// tabBar resolves the editor's tab bar: every open workspace in REGISTRY
// ORDER (sortWorkspaces), independent of the groupings, so repositories
// interleave and a child need not follow its parent. A closed workspace has no
// tab. A tab is withheld, not dropped, when the user hid its workspace or its
// repository's section is collapsed, so a client cycling from a withheld
// current workspace still knows its neighbours. A merged workspace files under
// recently merged whatever its hidden flag (wsm.Workspace.Withheld, the one
// statement of visibility the daemon also runs sessions by). A hidden
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
		switch ws.Withheld(folded[ws.Repo]) {
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
