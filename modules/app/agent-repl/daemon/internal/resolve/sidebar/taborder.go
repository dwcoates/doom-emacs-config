package sidebar

import (
	"fmt"

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

// openNode is one open workspace of the Open section and its open family:
// its children of its own repository, then one block per other repository
// (blocks.go).
type openNode struct {
	ws       wsm.Workspace
	children []openNode
	blocks   []repoGroup[openNode]
}

// openRegion is one rows region of a repository's Open section: a run of its
// rows in no subsection (subsection nil), or one named subsection's.
type openRegion struct {
	subsection *wsm.Subsection
	rows       []openNode
}

// folded reports whether the region's tabs are folded off the tab bar. The
// one statement of a folded tab, which the sidebar's numbering and the tab bar
// read alike.
func (r openRegion) folded(repo wsm.Repository) bool {
	return r.fold(repo) != nil
}

// fold names what folds the region's tabs off the tab bar, nil when nothing
// does. ONLY THE NEAREST CONTAINER'S FOLD COUNTS (owner ruling, 2026-11-24):
// a named subsection's rows are folded by the subsection's own collapse and
// by nothing above it, so an unfolded subsection inside a collapsed
// repository section shows every one of its top-level rows; the rows in no
// subsection are folded by their repository's collapse.
func (r openRegion) fold(repo wsm.Repository) *Fold {
	if r.subsection != nil {
		if r.subsection.Folded {
			return &Fold{By: FoldedBySubsection, Repository: repo.Name, Subsection: r.subsection.Name}
		}
		return nil
	}
	if repo.Folded {
		return &Fold{By: FoldedByRepository, Repository: repo.Name}
	}
	return nil
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
			// THE SELECTION IS REG.CURRENT's, whatever the row's own flag says
			// (the resolver takes a switch between registry installs), and it
			// is what a fold draws inside itself (wsm.Visibility).
			ws.Current = reg.Current != nil && *reg.Current == ws.ID
			members = append(members, ws)
		}
	}
	arranged, orphaned := arrange(reg, members)
	order := newBlockOrder(reg)
	var out []openRepo
	for _, placedRepo := range arranged {
		repo := openRepo{repo: placedRepo.repo}
		for _, b := range placedRepo.buckets {
			tree := nest(b.workspaces, log)
			repo.regions = append(repo.regions, openRegion{subsection: b.subsection, rows: openNodes(tree, order, tree.roots)})
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
// not-open workspace replaced in place by its own open descendants. An open
// workspace's drawn children are grouped by repository under it (group, in
// ORDER), so a descendant standing in for a not-open child joins the block
// of its own repository.
func openNodes(tree forest, order blockOrder, list []wsm.Workspace) []openNode {
	var out []openNode
	for _, ws := range list {
		children := openNodes(tree, order, tree.children[ws.ID])
		if !ws.ListedOpen() {
			out = append(out, children...)
			continue
		}
		own, blocks := group(order, ws, children, func(n openNode) ids.RepoID { return n.ws.Repo })
		out = append(out, openNode{ws: ws, children: own, blocks: blocks})
	}
	return out
}

// walkOpen visits every open workspace of NODES depth-first, in drawn order,
// each before its family (its own-repository children, then its blocks'),
// saying whether the tab is FOLDED: FOLDED is the region's own fold, and a
// workspace whose children are folded folds every tab below it, block rows
// included (childrenFolded). It is the sections' own reading: which of the
// covered workspaces are drawn anyway is the visibility (visibility.go).
func walkOpen(nodes []openNode, fold *Fold, visit func(ws wsm.Workspace, fold *Fold)) {
	for _, node := range nodes {
		visit(node.ws, fold)
		walkOpen(flatten(node.children, node.blocks), familyFold(node, fold), visit)
	}
}

// childrenFolded reports whether NODE's family is folded off the tab bar,
// given whether NODE's own tab is FOLDED: familyFold's answer, as the roster's
// rows read it.
func childrenFolded(node openNode, folded bool) bool {
	return folded || familyFold(node, nil) != nil
}

// familyFold names what folds NODE's family off the tab bar, given the FOLD
// over NODE's own tab: a folded tab folds all below it under the same fold,
// and a workspace whose children are folded folds its children's.
func familyFold(node openNode, fold *Fold) *Fold {
	if fold != nil {
		return fold
	}
	if node.ws.ChildrenFolded {
		return &Fold{By: FoldedByParent, Parent: node.ws.ID, ParentName: node.ws.Name}
	}
	return nil
}

// FoldedBy is which collapse folds a tab off the tab bar.
type FoldedBy int

const (
	// FoldedByRepository: the tab's repository section is collapsed.
	FoldedByRepository FoldedBy = iota + 1
	// FoldedBySubsection: the named subsection the tab sits in is collapsed.
	FoldedBySubsection
	// FoldedByParent: an ancestor's children are folded (SetChildrenFolded).
	FoldedByParent
)

// String names the collapse for a record.
func (b FoldedBy) String() string {
	switch b {
	case FoldedByRepository:
		return "repository"
	case FoldedBySubsection:
		return "subsection"
	case FoldedByParent:
		return "parent"
	default:
		return fmt.Sprintf("FoldedBy(%d)", int(b))
	}
}

// Fold names the collapse that folds a tab off the tab bar, so a record of
// what the fold did can say which section, subsection or parent it was.
type Fold struct {
	By FoldedBy
	// Repository is the collapsed repository's name, or the one the
	// collapsed subsection sits in.
	Repository string
	// Subsection is the collapsed subsection's name.
	Subsection string
	// Parent is the workspace whose children are folded, and ParentName its
	// name.
	Parent     ids.WorkspaceID
	ParentName string
}

// Fields is the fold as a record's context.
func (f Fold) Fields() dlog.Context {
	fields := dlog.Context{"fold": f.By.String()}
	switch f.By {
	case FoldedByRepository:
		fields["fold_repository"] = f.Repository
	case FoldedBySubsection:
		fields["fold_repository"], fields["fold_subsection"] = f.Repository, f.Subsection
	case FoldedByParent:
		fields["fold_parent"], fields["fold_parent_name"] = string(f.Parent), f.ParentName
	default:
		panic(fmt.Sprintf("sidebar: Fold.Fields of a fold that names no collapse: %d", int(f.By)))
	}
	return fields
}

// Tab is one tab of the editor's tab bar: an open workspace, and whether its
// repository's fold keeps it from being drawn.
type Tab struct {
	Workspace wsm.Workspace
	Withheld  wsm.Withholding
	// Fold names the collapse that withholds the tab, nil for a drawn one.
	Fold *Fold
}

// Tabs answers the editor's tab bar: every open workspace in the Open
// section's walk (openTree), a folded region's withheld as collapsed. It
// is the same walk the roster numbers its open rows by, so hiding the current
// workspace lands the user on the neighbour the bar actually draws.
func Tabs(reg Registry, log dlog.Logger) []Tab {
	repos, _ := openTree(reg, log)
	return tabsOf(repos)
}

// tabsOf is Tabs over an Open tree already built (openTree), so the roster
// asks the tab bar's reading of the tree it is drawing rather than building
// the tree twice.
func tabsOf(repos []openRepo) []Tab {
	var out []Tab
	for _, repo := range repos {
		for _, region := range repo.regions {
			// THE TAB'S WITHHOLDING IS wsm.Workspace.Withheld, the one statement
			// the daemon's Runs and Holds read, so the bar and the daemon cannot
			// disagree about a tab. An open row is never in the hidden section,
			// so it is Drawn or WithheldCollapsed.
			walkOpen(region.rows, region.fold(repo.repo), func(ws wsm.Workspace, fold *Fold) {
				out = append(out, Tab{Workspace: ws, Withheld: ws.Withheld(fold != nil), Fold: fold})
			})
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
