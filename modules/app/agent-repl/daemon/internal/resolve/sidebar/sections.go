package sidebar

import (
	"fmt"

	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"
	"agentrepl/protohelpers/rosterwalk"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/repodivergence"
	"agent-repld/internal/wsm"
)

// MergedSectionLabel is the recently-merged section's heading. It has one
// author for the same reason every other drawn string does.
const MergedSectionLabel = "Recently Merged"

// HiddenSectionLabel is the hidden-workspaces section's heading.
const HiddenSectionLabel = "Hidden"

// OpenSectionLabel is the open-workspaces section's heading.
const OpenSectionLabel = "Open"

// repositoryView resolves the Open section from the Open tree (openTree):
// ONE SECTION PER REPOSITORY in the arrangement's order, its own rows first
// and then one subsection per subsection, each open row carrying its tab
// numbered in this very walk (tabNumberer), so the sidebar's numbers and the
// tab bar's are one fact.
//
// A repository with no rows still draws a section, and a subsection with no
// rows still draws: both are registered, and a section that vanished when its
// last workspace left would make the grouping flicker (and leave an empty
// subsection nowhere to drag a workspace into).
func (r *resolver) repositoryView(rc rowContext, log dlog.Logger) *frontendv1.RosterRepositoryView {
	repos, orphans := openTree(r.state.reg, log)
	out := &frontendv1.RosterRepositoryView{}
	numbers := &tabNumberer{}
	shown := 0
	for _, placed := range repos {
		repoLog := log.With(dlog.Context{"repo_id": string(placed.repo.ID)})
		section := &frontendv1.RosterRepoSection{
			Key:        repoKey(placed.repo),
			Divergence: repoDivergence(r.state.divergence[placed.repo.ID]),
		}
		count := 0
		for _, region := range placed.regions {
			regionLog := repoLog
			if region.subsection != nil {
				regionLog = repoLog.With(dlog.Context{"subsection": string(region.subsection.ID)})
			}
			rows := r.openRows(region.rows, region.folded(placed.repo), numbers, rc, regionLog)
			regionCount := len(rosterwalk.FlattenOpen(rows.GetRows()))
			count += regionCount
			if region.subsection == nil {
				section.OpenRows = rows
				continue
			}
			sub := &frontendv1.RosterSubsection{
				Key:      &frontendv1.RosterSubsectionKey{SubsectionId: string(region.subsection.ID)},
				Header:   sectionHeader(region.subsection.Name, regionCount),
				OpenRows: rows,
			}
			setSubsectionFold(sub, region.subsection.Folded)
			section.Subsections = append(section.Subsections, sub)
		}
		section.Header = sectionHeader(placed.repo.Name, count)
		shown += count
		setRepoFold(section, placed.repo.Folded)
		out.Sections = append(out.Sections, section)
	}
	r.assertRepositoryInvariant(orphans, "daemon.sidebar.repository_view", "the repo grouping", log)
	out.OpenCount = &frontendv1.RosterOpenCount{Workspaces: uint32(shown)}
	return out
}

// openRows composes one region's open rows, depth-first, each taking the next
// tab of NUMBERS before its family does, so a parent's number precedes its
// children's.
func (r *resolver) openRows(nodes []openNode, folded bool, numbers *tabNumberer, rc rowContext, log dlog.Logger) *frontendv1.RosterOpenRows {
	out := &frontendv1.RosterOpenRows{}
	for _, node := range nodes {
		out.Rows = append(out.Rows, r.openRow(node, folded, numbers, rc, log))
	}
	return out
}

// openRow composes one open row, its tab, and its open family below it.
func (r *resolver) openRow(node openNode, folded bool, numbers *tabNumberer, rc rowContext, log dlog.Logger) *frontendv1.RosterOpenRow {
	out := &frontendv1.RosterOpenRow{Row: r.row(node.ws, rc, log), Tab: numbers.tab(folded)}
	for _, child := range node.children {
		out.Children = append(out.Children, r.openRow(child, folded, numbers, rc, log))
	}
	return out
}

// repoKey is a repository's join key, as its section and its merged group
// both carry it.
func repoKey(repo wsm.Repository) *frontendv1.RosterRepoKey {
	return &frontendv1.RosterRepoKey{
		Repository: &workspacev1.RepositoryRef{Id: string(repo.ID), Dir: repo.Dir}}
}

// setRepoFold states the repository's fold on its section: the ONE arm the
// sidebar and the Emacs tab bar both draw from.
func setRepoFold(section *frontendv1.RosterRepoSection, folded bool) {
	if folded {
		section.Fold = &frontendv1.RosterRepoSection_Collapsed{Collapsed: &frontendv1.RosterRepoSectionCollapsed{}}
		return
	}
	section.Fold = &frontendv1.RosterRepoSection_Expanded{Expanded: &frontendv1.RosterRepoSectionExpanded{}}
}

// setSubsectionFold sets a subsection's fold arm: its OWN fold, which the
// sidebar draws. Its tabs are folded by its repository's collapse as well
// (openRegion.folded).
func setSubsectionFold(sub *frontendv1.RosterSubsection, folded bool) {
	if folded {
		sub.Fold = &frontendv1.RosterSubsection_Collapsed{Collapsed: &frontendv1.RosterSubsectionCollapsed{}}
		return
	}
	sub.Fold = &frontendv1.RosterSubsection_Expanded{Expanded: &frontendv1.RosterSubsectionExpanded{}}
}

// repoDivergence projects a repository's measure onto its section's label
// hover: no measure yet is pending. Every Measure is one of the three arms.
func repoDivergence(measure repodivergence.Measure) *frontendv1.RosterRepoDivergence {
	out := &frontendv1.RosterRepoDivergence{}
	switch m := measure.(type) {
	case nil:
		out.State = &frontendv1.RosterRepoDivergence_Pending{Pending: &frontendv1.RosterRepoDivergencePending{}}
	case repodivergence.Counted:
		out.State = &frontendv1.RosterRepoDivergence_Counted{Counted: &frontendv1.RosterRepoDivergenceCounted{
			Branch: m.Branch, Upstream: m.Upstream, Ahead: uint32(m.Ahead), Behind: uint32(m.Behind)}}
	case repodivergence.NoUpstream:
		out.State = &frontendv1.RosterRepoDivergence_NoUpstream{NoUpstream: &frontendv1.RosterRepoDivergenceNoUpstream{Branch: m.Branch}}
	case repodivergence.Failed:
		out.State = &frontendv1.RosterRepoDivergence_Failed{Failed: &frontendv1.RosterRepoDivergenceFailed{Reason: m.Reason}}
	default:
		panic(fmt.Sprintf("sidebar: repository divergence measure %T has no arm", measure))
	}
	return out
}

// assertRepositoryInvariant records a workspace whose `Repo` names no
// registered repository. IT IS UNREACHABLE, and the assertion is what says so
// out loud rather than letting the roster quietly draw one workspace fewer.
//
// A workspace whose repository is unregistered is an INVARIANT VIOLATION and
// must be impossible (owner ruling, 2026-09-13). Three layers hold it and the
// roster is none of them: `workspaces.repo_id` is `NOT NULL REFERENCES
// repositories(id)` and every state-store handle carries
// `_pragma=foreign_keys(1)`, so SQLite refuses the row; `RegisterWorkspace`
// mints the repository row through `ensureRepo` FIRST, in the workspace
// insert's own transaction; and `Create` refuses a repository the registry does
// not hold before anything is built. The state store's open reports a row that
// was already there when it boots.
//
// So reaching here is a defect in one of those, not a state to render, and it
// is recorded the way this resolver records every other assertion it cannot
// serve (see assertArm): loudly, once, naming the remedy.
func (r *resolver) assertRepositoryInvariant(byRepo map[ids.RepoID][]wsm.Workspace, operation, leftOutOf string, log dlog.Logger) {
	for repo, orphans := range byRepo {
		log.Error(operation,
			"workspaces name a repository the registry does not carry and were left out of "+leftOutOf,
			dlog.Context{
				"repo_id":             string(repo),
				"workspaces":          len(orphans),
				"invariant_violation": "workspace.Repo names no registered repository",
				"remediation":         "re-register the workspace's directory, which mints its repository row, or forget the workspace",
			})
	}
}

// mergedSection resolves the recently-merged section: the rows hoisted out of
// every grouping, ONE GROUP PER REPOSITORY in the repository grouping's order,
// each group's rows in merge order, the latest merge first
// (sortMergedNewestFirst, owner request 2026-10-13). It is rendered under every grouping
// because that these workspaces are done is the interesting fact, and its rows
// are FLAT — a settled merge has no family left to draw.
//
// A group follows its repository's fold: a collapsed repository's group rides
// the wire on its repository_collapsed arm (so every roster walker still sees
// its workspaces) and the band's count leaves its rows out.
func (r *resolver) mergedSection(merged []wsm.Workspace, rc rowContext, log dlog.Logger) *frontendv1.RosterMergedSection {
	byRepo := map[ids.RepoID][]wsm.Workspace{}
	for _, ws := range merged {
		byRepo[ws.Repo] = append(byRepo[ws.Repo], ws)
	}
	section := &frontendv1.RosterMergedSection{}
	shownRows := 0
	for _, repo := range sortRepositories(r.state.reg.Repositories) {
		workspaces, ok := byRepo[repo.ID]
		delete(byRepo, repo.ID)
		if !ok {
			continue
		}
		rows := &frontendv1.RosterMergedRows{}
		for _, ws := range sortMergedNewestFirst(workspaces) {
			rows.Rows = append(rows.Rows, &frontendv1.RosterMergedRow{Row: r.row(ws, rc, log)})
		}
		group := &frontendv1.RosterMergedRepoGroup{
			Key:        repoKey(repo),
			Header:     &frontendv1.RosterMergedRepoGroupHeader{Label: &frontendv1.RosterLabel{Text: repo.Name}},
			MergedRows: rows,
		}
		setMergedGroupDrawn(group, repo.Folded)
		if !repo.Folded {
			shownRows += len(rows.GetRows())
		}
		section.Groups = append(section.Groups, group)
	}
	r.assertRepositoryInvariant(byRepo, "daemon.sidebar.merged_section", "the recently-merged band", log)
	section.Header = sectionHeader(MergedSectionLabel, shownRows)
	setMergedFold(section, r.state.reg.View.MergedFolded)
	return section
}

// setMergedGroupDrawn states whether the sidebar draws a merged group, from
// its repository's own fold: the same fact RosterRepoSection.fold states.
func setMergedGroupDrawn(group *frontendv1.RosterMergedRepoGroup, repositoryFolded bool) {
	if repositoryFolded {
		group.Drawn = &frontendv1.RosterMergedRepoGroup_RepositoryCollapsed{
			RepositoryCollapsed: &frontendv1.RosterMergedRepoGroupRepositoryCollapsed{}}
		return
	}
	group.Drawn = &frontendv1.RosterMergedRepoGroup_Shown{Shown: &frontendv1.RosterMergedRepoGroupShown{}}
}

// hiddenSection resolves the hidden-workspaces section: the open workspaces
// the user hid (SetWorkspaceHidden), which have NO TAB. Its rows nest by
// lineage among the hidden workspaces alone, so a hidden family keeps its
// shape (a workspace with open children cannot be hidden, so a hidden row's
// family is hidden too); its top-level rows go in HIDE ORDER, the latest hide
// first (sortHiddenNewestFirst, owner request 2026-10-13), and its count is
// the rows it draws.
func (r *resolver) hiddenSection(hidden []wsm.Workspace, rc rowContext, log dlog.Logger) *frontendv1.RosterHiddenSection {
	log = log.With(dlog.Context{"section": "hidden"})
	tree := nest(hidden, log)
	rows := &frontendv1.RosterHiddenRows{}
	for _, ws := range sortHiddenNewestFirst(tree.roots) {
		rows.Rows = append(rows.Rows, r.hiddenRow(ws, tree, rc, log))
	}
	section := &frontendv1.RosterHiddenSection{
		Header:     sectionHeader(HiddenSectionLabel, len(rosterwalk.FlattenHidden(rows.GetRows()))),
		HiddenRows: rows,
	}
	setHiddenFold(section, r.state.reg.View.HiddenFolded)
	return section
}

// hiddenRow composes one hidden row and its hidden family below it.
func (r *resolver) hiddenRow(ws wsm.Workspace, tree forest, rc rowContext, log dlog.Logger) *frontendv1.RosterHiddenRow {
	out := &frontendv1.RosterHiddenRow{Row: r.row(ws, rc, log)}
	for _, child := range tree.children[ws.ID] {
		out.Children = append(out.Children, r.hiddenRow(child, tree, rc, log))
	}
	return out
}

// closedRows resolves the closed list: the closed workspaces that never
// merged, in roster order. No sidebar section draws it; the editor offers them
// again and routes their records by it.
func (r *resolver) closedRows(closed []wsm.Workspace, rc rowContext, log dlog.Logger) *frontendv1.RosterClosedRows {
	out := &frontendv1.RosterClosedRows{}
	for _, ws := range sortWorkspaces(closed) {
		out.Rows = append(out.Rows, &frontendv1.RosterClosedRow{Row: r.row(ws, rc, log)})
	}
	return out
}

// setHiddenFold states the hidden section's fold. The fold is the daemon's so
// every page draws the same one (UpdateSidebarView); it folds sidebar rows
// only, never a tab.
func setHiddenFold(section *frontendv1.RosterHiddenSection, folded bool) {
	if folded {
		section.Fold = &frontendv1.RosterHiddenSection_Collapsed{Collapsed: &frontendv1.RosterHiddenSectionCollapsed{}}
		return
	}
	section.Fold = &frontendv1.RosterHiddenSection_Expanded{Expanded: &frontendv1.RosterHiddenSectionExpanded{}}
}

// openSection resolves the open-workspaces section: its heading and its fold.
// The groupings it boxes stay on the roster, each carrying its own count
// (openCount). The fold is the daemon's so every page draws the same one
// (UpdateSidebarView); it folds sidebar rows only, never a tab.
func (r *resolver) openSection() *frontendv1.RosterOpenSection {
	section := &frontendv1.RosterOpenSection{Label: &frontendv1.RosterLabel{Text: OpenSectionLabel}}
	if r.state.reg.View.OpenFolded {
		section.Fold = &frontendv1.RosterOpenSection_Collapsed{Collapsed: &frontendv1.RosterOpenSectionCollapsed{}}
	} else {
		section.Fold = &frontendv1.RosterOpenSection_Expanded{Expanded: &frontendv1.RosterOpenSectionExpanded{}}
	}
	return section
}

// setMergedFold states the recently-merged band's fold. The fold is the
// daemon.s so every page draws the same one (UpdateSidebarView).
func setMergedFold(section *frontendv1.RosterMergedSection, folded bool) {
	if folded {
		section.Fold = &frontendv1.RosterMergedSection_Collapsed{Collapsed: &frontendv1.RosterMergedSectionCollapsed{}}
		return
	}
	section.Fold = &frontendv1.RosterMergedSection_Expanded{Expanded: &frontendv1.RosterMergedSectionExpanded{}}
}

// sectionHeader composes a repository or Recently Merged header: its label and
// the count a FOLDED section draws beside it, which its caller reads off the
// very rows the section carries, so it can never disagree with them, and no client counts rows for
// itself.
func sectionHeader(label string, count int) *frontendv1.RosterSectionHeader {
	return &frontendv1.RosterSectionHeader{
		Label: &frontendv1.RosterLabel{Text: label},
		Count: &frontendv1.RosterSectionCount{Workspaces: uint32(count)},
	}
}
