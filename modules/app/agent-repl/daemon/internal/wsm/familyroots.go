package wsm

// A FAMILY IS FILED BY ITS ROOT. A child workspace's row nests under its
// parent's, so a whole family is drawn in its root's repository section and
// its root's subsection, whatever repository a child itself is of. Every
// reader of where a workspace is filed (the sidebar's arrangement, the tab bar,
// a repository fold's membership) asks here, so none of them can disagree
// about a cross-repository child.

// ParentsIn answers each workspace of IN's recorded parent, for those whose
// parent is among IN too (and is not itself): the only parents a row can nest
// under.
func ParentsIn(in []Workspace) map[WorkspaceID]WorkspaceID {
	present := map[WorkspaceID]struct{}{}
	for _, ws := range in {
		present[ws.ID] = struct{}{}
	}
	parent := map[WorkspaceID]WorkspaceID{}
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

// ParentChainCycles reports whether following the parent chain from WS
// returns to WS. A cycle is unreachable through ordinary creation, which is
// exactly why it is checked: an ill-formed lineage must recede a row to the
// top level rather than build a tree that never terminates.
func ParentChainCycles(ws WorkspaceID, parent map[WorkspaceID]WorkspaceID) bool {
	seen := map[WorkspaceID]struct{}{ws: {}}
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

// FamilyRoots answers, for every workspace of IN, the top-level row its row is
// drawn under: itself when it renders at the top level (no parent among IN, or
// a parent chain that cycles), else its parent's family root.
func FamilyRoots(in []Workspace) map[WorkspaceID]WorkspaceID {
	parent := ParentsIn(in)
	out := map[WorkspaceID]WorkspaceID{}
	for _, ws := range in {
		at := ws.ID
		for {
			p, nested := parent[at]
			if !nested || ParentChainCycles(at, parent) {
				break
			}
			at = p
		}
		out[ws.ID] = at
	}
	return out
}

// SectionRepos answers, for every workspace of IN, the repository whose
// section draws it: its FAMILY ROOT's (FamilyRoots). A FAMILY NEVER SPLITS
// ACROSS SECTIONS, as it never splits across subsections: a child of another
// repository than its parent's nests under that parent, in the parent's
// section, where its row begins a repository divider
// (frontend.v1.RosterOpenRow.repository_divider).
func SectionRepos(in []Workspace) map[WorkspaceID]RepoID {
	root := FamilyRoots(in)
	repoOf := make(map[WorkspaceID]RepoID, len(in))
	for _, ws := range in {
		repoOf[ws.ID] = ws.Repo
	}
	out := make(map[WorkspaceID]RepoID, len(in))
	for _, ws := range in {
		out[ws.ID] = repoOf[root[ws.ID]]
	}
	return out
}
