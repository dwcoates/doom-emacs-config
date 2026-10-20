package sidebar

import (
	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// A ROW NESTS ONLY UNDER THE WORKSPACE IT MERGES INTO (owner ruling,
// 2026-10-11). A workspace created through the daemon as a child records that
// parent at creation, and a child is always cut from its parent's tip and
// merges back into it, so the recorded parent is exactly that workspace. A
// workspace with no recorded parent merges into its repository's primary
// branch and renders at the section's top level, whatever branch it was cut
// from: a branch lineage alone says where a workspace came from, not where it
// lands, so it nests nothing.

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
// A recorded parent that cycles is ill-formed and unreachable through ordinary
// creation; it is recorded loudly and resolved by leaving the row at the top
// level rather than dropping it.
func nest(in []wsm.Workspace, log dlog.Logger) forest {
	ordered := sortWorkspaces(in)

	parent := parentsIn(ordered)

	out := forest{children: map[ids.WorkspaceID][]wsm.Workspace{}}
	for _, ws := range ordered {
		p, nested := parent[ws.ID]
		switch {
		case !nested:
			log.Debug("daemon.sidebar.nest", "a workspace renders at the section's top level",
				dlog.Context{"workspace_id": string(ws.ID), "recorded_parent": recordedParent(ws)})
			out.roots = append(out.roots, ws)
		case cycles(ws.ID, parent):
			log.Warn("daemon.sidebar.nest",
				"a recorded parent chain cycles; the workspace renders at the top level rather than vanishing",
				dlog.Context{"workspace_id": string(ws.ID), "recorded_parent": recordedParent(ws)})
			out.roots = append(out.roots, ws)
		default:
			log.Debug("daemon.sidebar.nest", "a workspace nests under the workspace it merges into",
				dlog.Context{"workspace_id": string(ws.ID), "parent_id": string(p)})
			out.children[p] = append(out.children[p], ws)
		}
	}
	return out
}

// parentsIn answers each workspace of IN's recorded parent, for those whose
// parent is among IN too (and is not itself): the only parents a row can
// nest under.
func parentsIn(in []wsm.Workspace) map[ids.WorkspaceID]ids.WorkspaceID {
	present := map[ids.WorkspaceID]struct{}{}
	for _, ws := range in {
		present[ws.ID] = struct{}{}
	}
	parent := map[ids.WorkspaceID]ids.WorkspaceID{}
	for _, ws := range in {
		if ws.Parent == nil {
			continue
		}
		if _, drawn := present[*ws.Parent]; drawn && *ws.Parent != ws.ID {
			parent[ws.ID] = *ws.Parent
		}
	}
	return parent
}

// familyRoots answers, for every workspace of IN, the top-level row nest
// draws its row under: itself when it renders at the top level (no parent
// among IN, or a parent chain that cycles), else its parent's family root.
func familyRoots(in []wsm.Workspace) map[ids.WorkspaceID]ids.WorkspaceID {
	parent := parentsIn(in)
	out := map[ids.WorkspaceID]ids.WorkspaceID{}
	for _, ws := range in {
		at := ws.ID
		for {
			p, nested := parent[at]
			if !nested || cycles(at, parent) {
				break
			}
			at = p
		}
		out[ws.ID] = at
	}
	return out
}

// walk visits every workspace of the forest depth-first in render order: each
// root, then its children's subtrees in roster order, before the next root.
// It is the order the sidebar draws the rows in, and the tab bar's.
func (f forest) walk(visit func(wsm.Workspace)) {
	var descend func(ws wsm.Workspace)
	descend = func(ws wsm.Workspace) {
		visit(ws)
		for _, child := range f.children[ws.ID] {
			descend(child)
		}
	}
	for _, root := range f.roots {
		descend(root)
	}
}

// recordedParent spells a workspace's recorded parent for the record, empty
// when it has none.
func recordedParent(ws wsm.Workspace) string {
	return ids.Optional(ws.Parent)
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
