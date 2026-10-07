package sidebar

import (
	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"
)

// TabEntry is one open workspace in the editor's TAB ORDER.
type TabEntry struct {
	// Ref is the workspace's ref as the roster carries it.
	Ref *workspacev1.WorkspaceRef
	// Name is its display name.
	Name string
}

// TabOrder is THE REGISTRY ORDER the editor's tabs follow: the open rows of
// the roster's repository sections, depth-first in the resolver's order, then
// the recently-merged section's open rows. It is the same walk the Emacs tab
// bar makes over the same published roster (lisp/roster.el
// `agent-repl-roster-walk', whose membership rule is "not closed"), so the
// daemon and the editor can never disagree about which workspace is first.
// The task view regroups the same workspaces and is not walked. Nil before the
// roster has been published.
func TabOrder(roster *frontendv1.WorkspaceRoster) []TabEntry {
	var out []TabEntry
	walk := func(rows []*frontendv1.RosterRow) {
		for _, row := range FlattenRows(rows) {
			if !row.GetClosed().GetClosed() {
				out = append(out, TabEntry{Ref: row.GetWorkspace().GetWorkspace(), Name: row.GetName().GetText()})
			}
		}
	}
	for _, section := range roster.GetRepository().GetSections() {
		walk(section.GetRows().GetRows())
	}
	walk(roster.GetRecentlyMerged().GetRows().GetRows())
	return out
}
