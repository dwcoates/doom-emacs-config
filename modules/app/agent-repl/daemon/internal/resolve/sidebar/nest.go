package sidebar

import (
	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/wsm"
)

// THE SPAWNED FAMILY IS THE BRANCH LINEAGE.
//
// WSM records no parent WORKSPACE — it records the parent BRANCH each
// workspace was cut from, which is the same fact stated in git's terms: a
// spawned workspace is exactly one whose branch was cut from another
// workspace's branch. So the nesting is derived, within one repository, by
// matching a workspace's ParentBranch against another's Branch. This is also
// what the row detail already draws ("branch" over "from"), so the tree and
// the detail panel cannot disagree.
//
// A workspace cut from the repository's default branch (or from any branch no
// workspace occupies) has no parent among the rows and renders at the top
// level, which is the ordinary case.

// forest arranges one section's workspaces into parent/child order and answers
// the roots, each carrying its children in roster order.
type forest struct {
	// roots are the top-level workspaces, in roster order.
	roots []wsm.Workspace
	// children are each parent's children, in roster order, by parent id.
	children map[ids.WorkspaceID][]wsm.Workspace
}

// nest builds the forest over exactly the workspaces given. Nesting is scoped
// to the section: a workspace whose parent is NOT in this section renders at
// the section's top level, because a row can only nest under a row that is
// drawn beside it.
//
// Every branch logs, and the two ways the lineage can be ill-formed — two
// workspaces claiming the same branch, and a cycle — are recorded loudly and
// resolved by leaving the row at the top level rather than dropping it.
func nest(in []wsm.Workspace, log dlog.Logger) forest {
	ordered := sortWorkspaces(in)

	byBranch := map[string]wsm.Workspace{}
	for _, ws := range ordered {
		if ws.Branch == "" {
			continue
		}
		if prior, clash := byBranch[ws.Branch]; clash {
			log.Warn("daemon.sidebar.nest",
				"two workspaces claim one branch; the first in roster order owns the family",
				dlog.Context{
					"branch":       ws.Branch,
					"owner":        string(prior.ID),
					"also_claimed": string(ws.ID),
				})
			continue
		}
		byBranch[ws.Branch] = ws
	}

	parent := map[ids.WorkspaceID]ids.WorkspaceID{}
	for _, ws := range ordered {
		if ws.ParentBranch == "" {
			continue
		}
		p, ok := byBranch[ws.ParentBranch]
		if !ok || p.ID == ws.ID {
			continue
		}
		parent[ws.ID] = p.ID
	}

	out := forest{children: map[ids.WorkspaceID][]wsm.Workspace{}}
	for _, ws := range ordered {
		p, nested := parent[ws.ID]
		switch {
		case !nested:
			log.Debug("daemon.sidebar.nest", "a workspace renders at the section's top level",
				dlog.Context{"workspace_id": string(ws.ID), "parent_branch": ws.ParentBranch})
			out.roots = append(out.roots, ws)
		case cycles(ws.ID, parent):
			log.Warn("daemon.sidebar.nest",
				"a branch lineage cycles; the workspace renders at the top level rather than vanishing",
				dlog.Context{"workspace_id": string(ws.ID), "parent_branch": ws.ParentBranch})
			out.roots = append(out.roots, ws)
		default:
			log.Debug("daemon.sidebar.nest", "a workspace nests under the workspace its branch was cut from",
				dlog.Context{"workspace_id": string(ws.ID), "parent_id": string(p)})
			out.children[p] = append(out.children[p], ws)
		}
	}
	return out
}

// cycles reports whether following the parent chain from ws returns to ws. A
// cycle is unreachable through ordinary creation, which is exactly why it is
// checked: an ill-formed lineage must recede a row to the top level rather
// than build a tree that never terminates.
func cycles(ws ids.WorkspaceID, parent map[ids.WorkspaceID]ids.WorkspaceID) bool {
	seen := map[ids.WorkspaceID]struct{}{ws: {}}
	for at := parent[ws]; ; at = parent[at] {
		if at == "" {
			return false
		}
		if _, repeat := seen[at]; repeat {
			return true
		}
		seen[at] = struct{}{}
	}
}
