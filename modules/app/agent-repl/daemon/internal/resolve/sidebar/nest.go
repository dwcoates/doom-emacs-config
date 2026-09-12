package sidebar

import (
	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/wsm"
)

// THE SPAWNED FAMILY IS THE RECORDED PARENT, ELSE THE BRANCH LINEAGE.
//
// A workspace CREATED through the daemon from another one records that parent
// workspace at creation, and the roster nests off that record: it is the fact
// itself rather than a reconstruction of it.
//
// A workspace REGISTERED by Emacs was never created through the daemon and
// carries no parent, so its family is derived — within one repository — by
// matching its ParentBranch against another workspace's Branch, which is the
// same fact stated in git's terms. That is also what the row detail draws
// ("branch" over "from"), so the tree and the detail panel cannot disagree.
//
// A workspace with no recorded parent, cut from the repository's default
// branch (or from any branch no workspace occupies), has no parent among the
// rows and renders at the top level, which is the ordinary case.
//
// THE DEFAULT BRANCH IS NOT A FAMILY LINK, and stating that is what makes the
// paragraph above true. A repository's own main worktree is itself a
// registered workspace and it sits ON the default branch, so matching
// ParentBranch against Branch made EVERY ordinary workspace in that repository
// a child of the repository's own row. The tab bar flattens the forest
// depth-first with a parent always ahead of its children, so a priority given
// to such a "child" could never move it past its "parent": realtest 8 gave
// `realtest-8-second` P1 and the repository row P3 and the drawn bar did not
// move. A recorded parent still nests, because that is the fact itself; a
// branch cut from the default branch is not a lineage, it is the default.

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
// defaultBranches names each repository's default branch, which the branch
// lineage refuses to derive a family from; a repository it does not carry has
// no default branch to exempt and every branch is read as lineage, exactly as
// before.
//
// Every branch logs, and the two ways the lineage can be ill-formed — two
// workspaces claiming the same branch, and a cycle — are recorded loudly and
// resolved by leaving the row at the top level rather than dropping it.
func nest(in []wsm.Workspace, defaultBranches map[ids.RepoID]string, log dlog.Logger) forest {
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

	present := map[ids.WorkspaceID]struct{}{}
	for _, ws := range ordered {
		present[ws.ID] = struct{}{}
	}

	parent := map[ids.WorkspaceID]ids.WorkspaceID{}
	for _, ws := range ordered {
		// The RECORDED parent wins wherever there is one: the branch lineage
		// is a derivation of the same fact, and a derivation never overrules
		// the fact it derives.
		if ws.Parent != nil {
			if _, drawn := present[*ws.Parent]; drawn && *ws.Parent != ws.ID {
				parent[ws.ID] = *ws.Parent
			}
			continue
		}
		if ws.ParentBranch == "" {
			continue
		}
		if base, known := defaultBranches[ws.Repo]; known && base != "" && ws.ParentBranch == base {
			log.Debug("daemon.sidebar.nest",
				"a workspace cut from its repository's default branch derives no family from it",
				dlog.Context{"workspace_id": string(ws.ID), "repo_id": string(ws.Repo),
					"default_branch": base})
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
				dlog.Context{"workspace_id": string(ws.ID), "parent_branch": ws.ParentBranch,
					"recorded_parent": recordedParent(ws)})
			out.roots = append(out.roots, ws)
		case cycles(ws.ID, parent):
			log.Warn("daemon.sidebar.nest",
				"a branch lineage cycles; the workspace renders at the top level rather than vanishing",
				dlog.Context{"workspace_id": string(ws.ID), "parent_branch": ws.ParentBranch})
			out.roots = append(out.roots, ws)
		default:
			log.Debug("daemon.sidebar.nest", "a workspace nests under its parent",
				dlog.Context{"workspace_id": string(ws.ID), "parent_id": string(p),
					"recorded": ws.Parent != nil})
			out.children[p] = append(out.children[p], ws)
		}
	}
	return out
}

// recordedParent spells a workspace's recorded parent for the record, empty
// when it has none and the branch lineage is what answered.
func recordedParent(ws wsm.Workspace) string {
	if ws.Parent == nil {
		return ""
	}
	return string(*ws.Parent)
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
