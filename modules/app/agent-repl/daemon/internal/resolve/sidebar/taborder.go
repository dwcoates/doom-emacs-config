package sidebar

import (
	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// THE OPEN SECTION IS THE TAB BAR. One tree, built here (openTree), is drawn
// by the sidebar's Open section (repositoryView) and walked by the editor's
// tab bar (each RosterOpenRow states its own tab), and the daemon's own tab
// reading (Tabs) walks the very same tree, so none of them can disagree. The
// conditions it holds, each by construction:
//
//   - EVERY WORKSPACE IS IN ONE LIST. A workspace is OPEN (here), HIDDEN
//     (hiddenSection) or MERGED (mergedSection), never two: open is neither
//     closed nor hidden, and OPEN BEATS MERGED, so a merged workspace that was
//     reopened is open and listed here.
//   - IN OPEN IF AND ONLY IF IT HAS A TAB. Every open row carries a tab, and
//     nothing else does. A folded repository's rows, and a folded
//     subsection's, keep a FOLDED tab: open, but drawn by neither surface and
//     given no number (openRegion.folded).
//   - NUMBERS MATCH POSITION. Tabs are numbered 1..N with no gap in the
//     tree's depth-first walk, numbered tabs only, so tab N is always the row
//     drawn immediately before tab N+1, in the sidebar and on the bar.
//   - THE ORDER RUNS UNBROKEN across repositories (repository by repository),
//     subsections (a repository's walk, its unsectioned runs and its
//     subsections interleaved: one subsection never spans repositories,
//     arrange) and families (a subtree is
//     one run at every depth: a child follows its parent, its siblings in their
//     order, and a family never splits across subsections, because a child sits
//     in its root's subsection).
//   - A WORKSPACE WHOSE PARENT IS NOT OPEN TAKES ITS PARENT'S PLACE. The rules
//     against hiding or closing a parent with open children keep this from
//     arising; a family that predates them is still drawn, its open members
//     where the closed or hidden ancestor stood.

// openNode is one open workspace of the Open section and its open family.
type openNode struct {
	ws       wsm.Workspace
	children []openNode
}

// openRegion is one rows region of a repository's Open section: a run of its
// rows in no subsection (subsection nil, oneShot false), one named
// subsection's, or the repository's one-shot subsection's (oneShot).
type openRegion struct {
	subsection *wsm.Subsection
	oneShot    bool
	rows       []openNode
}

// folded reports whether the region's tabs are folded off the tab bar: its
// repository is collapsed, or it is a named subsection that is. The one-shot
// subsection never collapses itself (frontend.v1.RosterSubsectionNeverCollapses).
// The one statement of a folded tab, which the sidebar's numbering and the
// tab bar read alike.
func (r openRegion) folded(repo wsm.Repository) bool {
	return repo.Folded || (r.subsection != nil && r.subsection.Folded)
}

// openRepo is one repository's Open section, its regions in its walk's order.
type openRepo struct {
	repo    wsm.Repository
	regions []openRegion
}

// openTree builds the Open section from REG: every open workspace, filed by
// the arrangement and nested by family, and the workspaces REG's open ones
// descend from that are not open themselves, which are TRANSPARENT (their open
// descendants take their place). Open workspaces of a repository REG does not
// carry are answered as orphans.
func openTree(reg Registry, log dlog.Logger) ([]openRepo, map[ids.RepoID][]wsm.Workspace) {
	byID := map[ids.WorkspaceID]wsm.Workspace{}
	for _, ws := range reg.Workspaces {
		byID[ws.ID] = ws
	}
	// THE TRANSPARENT ANCESTORS: every not-open ancestor of an open workspace,
	// so a legacy child is drawn where its parent stood. Ancestors of nothing
	// open are left out, which keeps a long history of closed workspaces out of
	// every render.
	placed := map[ids.WorkspaceID]bool{}
	for _, ws := range reg.Workspaces {
		// A chat thread is in no repository section and has no tab: threads
		// are not in workspace cycling (owner ruling, 2026-10-24).
		if !ws.ListedOpen() || ws.IsThread() {
			continue
		}
		placed[ws.ID] = true
		for parent := ws.Parent; parent != nil; {
			ancestor, ok := byID[*parent]
			if !ok || placed[ancestor.ID] {
				break
			}
			placed[ancestor.ID] = true
			parent = ancestor.Parent
		}
	}
	var members []wsm.Workspace
	for _, ws := range reg.Workspaces {
		if placed[ws.ID] {
			members = append(members, ws)
		}
	}
	arranged, orphaned := arrange(reg, members)
	var out []openRepo
	for _, placedRepo := range arranged {
		repo := openRepo{repo: placedRepo.repo}
		for _, b := range placedRepo.buckets {
			tree := nest(b.workspaces, log)
			repo.regions = append(repo.regions, openRegion{subsection: b.subsection, oneShot: b.oneShot, rows: openNodes(tree, tree.roots)})
		}
		out = append(out, repo)
	}
	orphans := map[ids.RepoID][]wsm.Workspace{}
	for repo, workspaces := range orphaned {
		for _, ws := range workspaces {
			if ws.ListedOpen() {
				orphans[repo] = append(orphans[repo], ws)
			}
		}
	}
	return out, orphans
}

// openNodes answers the open nodes of LIST and their families in TREE, each
// not-open workspace replaced in place by its own open descendants.
func openNodes(tree forest, list []wsm.Workspace) []openNode {
	var out []openNode
	for _, ws := range list {
		children := openNodes(tree, tree.children[ws.ID])
		if !ws.ListedOpen() {
			out = append(out, children...)
			continue
		}
		out = append(out, openNode{ws: ws, children: children})
	}
	return out
}

// walkOpen visits every open workspace of NODES depth-first, in drawn order,
// each before its family.
func walkOpen(nodes []openNode, visit func(wsm.Workspace)) {
	for _, node := range nodes {
		visit(node.ws)
		walkOpen(node.children, visit)
	}
}

// Tab is one tab of the editor's tab bar: an open workspace, and whether its
// repository's fold keeps it from being drawn.
type Tab struct {
	Workspace wsm.Workspace
	Withheld  wsm.Withholding
}

// Tabs answers the editor's tab bar: every open workspace in the Open
// section's walk (openTree), a folded region's withheld as collapsed. It
// is the same walk the roster numbers its open rows by, so hiding the current
// workspace lands the user on the neighbour the bar actually draws.
func Tabs(reg Registry, log dlog.Logger) []Tab {
	repos, _ := openTree(reg, log)
	var out []Tab
	for _, repo := range repos {
		for _, region := range repo.regions {
			withheld := wsm.Drawn
			if region.folded(repo.repo) {
				withheld = wsm.WithheldCollapsed
			}
			walkOpen(region.rows, func(ws wsm.Workspace) { out = append(out, Tab{Workspace: ws, Withheld: withheld}) })
		}
	}
	return out
}

// tabNumberer hands out tab numbers in the Open section's walk: 1, 2, ... to
// every tab of an unfolded region, none to a folded one's.
type tabNumberer struct {
	next uint32
}

// tab answers the next open row's tab, given whether its region is folded.
func (n *tabNumberer) tab(folded bool) *frontendv1.RosterOpenRowTab {
	if folded {
		return &frontendv1.RosterOpenRowTab{Tab: &frontendv1.RosterOpenRowTab_Folded{Folded: &frontendv1.RosterOpenRowTabFolded{}}}
	}
	n.next++
	return &frontendv1.RosterOpenRowTab{Tab: &frontendv1.RosterOpenRowTab_Numbered{Numbered: &frontendv1.RosterOpenRowTabNumbered{Number: n.next}}}
}
