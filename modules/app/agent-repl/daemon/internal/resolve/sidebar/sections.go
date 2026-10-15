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

// repositoryView resolves the repository grouping: ONE SECTION PER REPOSITORY
// in the arrangement's order (arrangement.go), its unsectioned rows first and
// then one subsection per subsection, each nested by lineage among its own
// workspaces and ordered in roster order.
//
// A repository with no rows still draws a section, and a subsection with no
// rows still draws: both are registered, and a section that vanished when its
// last workspace left would make the grouping flicker (and leave an empty
// subsection nowhere to drag a workspace into).
func (r *resolver) repositoryView(live []wsm.Workspace, rc rowContext, log dlog.Logger) *frontendv1.RosterRepositoryView {
	arranged, orphans := arrange(r.state.reg, live)
	out := &frontendv1.RosterRepositoryView{}
	shown := 0
	for _, placed := range arranged {
		repoLog := log.With(dlog.Context{"repo_id": string(placed.repo.ID)})
		rows := r.sectionRows(placed.buckets[0].workspaces, rc, repoLog)
		count := drawnLiveRowCount(rows)
		section := &frontendv1.RosterRepoSection{
			Key:        repoKey(placed.repo),
			Rows:       rows,
			Divergence: repoDivergence(r.state.divergence[placed.repo.ID]),
		}
		for _, b := range placed.buckets[1:] {
			subRows := r.sectionRows(b.workspaces, rc, repoLog.With(dlog.Context{"subsection": string(b.subsection.ID)}))
			subCount := drawnLiveRowCount(subRows)
			count += subCount
			section.Subsections = append(section.Subsections, &frontendv1.RosterSubsection{
				Key:    &frontendv1.RosterSubsectionKey{SubsectionId: string(b.subsection.ID)},
				Header: sectionHeader(b.subsection.Name, subCount),
				Rows:   subRows,
			})
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
	rc.tree = forest{}
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
		rows := &frontendv1.RosterRows{}
		for _, ws := range sortMergedNewestFirst(workspaces) {
			rows.Rows = append(rows.Rows, r.row(ws, rc, log))
		}
		group := &frontendv1.RosterMergedRepoGroup{
			Key:    repoKey(repo),
			Header: &frontendv1.RosterMergedRepoGroupHeader{Label: &frontendv1.RosterLabel{Text: repo.Name}},
			Rows:   rows,
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

// hiddenSection resolves the hidden-workspaces section: the workspaces the
// user hid (SetWorkspaceHidden), hoisted out of every grouping. Its rows nest
// by lineage among the hidden workspaces alone, as a repository section's do
// among its own, so a hidden family keeps its shape; its top-level rows go in
// HIDE ORDER, the latest hide first (sortHiddenNewestFirst, owner request
// 2026-10-13), and its count is the rows it draws, closed rows left out,
// exactly as a repository section counts.
func (r *resolver) hiddenSection(hidden []wsm.Workspace, rc rowContext, log dlog.Logger) *frontendv1.RosterHiddenSection {
	log = log.With(dlog.Context{"section": "hidden"})
	rc.tree = nest(hidden, log)
	rc.tree.roots = sortHiddenNewestFirst(rc.tree.roots)
	rows := &frontendv1.RosterRows{}
	for _, ws := range rc.tree.roots {
		rows.Rows = append(rows.Rows, r.row(ws, rc, log))
	}
	section := &frontendv1.RosterHiddenSection{
		Header: sectionHeader(HiddenSectionLabel, drawnLiveRowCount(rows)),
		Rows:   rows,
	}
	setHiddenFold(section, r.state.reg.View.HiddenFolded)
	return section
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

// openCount is how many rows the open-workspaces section lists: every
// not-closed row of every repository section, nested family rows included, a
// folded section's too. It reads the very rows each section
// carries (drawnLiveRowCount), so it is always the sum of the sections' own
// counts and no client counts rows for itself.
func openCount[S interface{ GetRows() *frontendv1.RosterRows }](sections []S) *frontendv1.RosterOpenCount {
	n := 0
	for _, section := range sections {
		n += drawnLiveRowCount(section.GetRows())
	}
	return &frontendv1.RosterOpenCount{Workspaces: uint32(n)}
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
// very rows the section carries (drawnLiveRowCount, or the merged band's own
// rows), so it can never disagree with them, and no client counts rows for
// itself.
func sectionHeader(label string, count int) *frontendv1.RosterSectionHeader {
	return &frontendv1.RosterSectionHeader{
		Label: &frontendv1.RosterLabel{Text: label},
		Count: &frontendv1.RosterSectionCount{Workspaces: uint32(count)},
	}
}

// drawnLiveRowCount is how many rows a live section shows UNFOLDED: every row
// of its tree, nested family rows included, that is not CLOSED (owner request,
// 2026-10-08: a folded section's count equals the rows it shows unfolded).
// A closed, killed or nuked workspace still rides the wire (Emacs reconciles
// its tabs from the flag) but no client draws it, and its live descendants
// are drawn in its place (the webapp's expandVisibleRows), so the count is
// exactly the not-closed rows of the flattened tree, read off the same
// RosterRowClosed fact the client drops a row by.
func drawnLiveRowCount(rows *frontendv1.RosterRows) int {
	n := 0
	for _, row := range rosterwalk.FlattenRows(rows.GetRows()) {
		if !row.GetClosed().GetClosed() {
			n++
		}
	}
	return n
}

// sectionRows nests one section's workspaces and composes their rows. Nesting
// is scoped to the section, so the SAME workspace can be a root in the task
// view (its parent is on another task) and a child in the repo view.
func (r *resolver) sectionRows(in []wsm.Workspace, rc rowContext, log dlog.Logger) *frontendv1.RosterRows {
	rc.tree = nest(in, log)
	out := &frontendv1.RosterRows{}
	for _, ws := range rc.tree.roots {
		out.Rows = append(out.Rows, r.row(ws, rc, log))
	}
	return out
}
