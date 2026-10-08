package sidebar

import (
	"sort"

	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"
	"agentrepl/protohelpers/rosterwalk"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/wsm"
)

// MergedSectionLabel is the recently-merged section's heading. It has one
// author for the same reason every other drawn string does.
const MergedSectionLabel = "Recently Merged"

// repositoryView resolves the repository grouping: ONE SECTION PER REPOSITORY,
// its rows nested by branch lineage and ordered in roster order.
//
// A repository with no rows still draws a section: the repository is
// registered, and a section that vanished when its last workspace merged would
// make the grouping flicker.
func (r *resolver) repositoryView(live []wsm.Workspace, rc rowContext, log dlog.Logger) *frontendv1.RosterRepositoryView {
	byRepo := map[ids.RepoID][]wsm.Workspace{}
	for _, ws := range live {
		byRepo[ws.Repo] = append(byRepo[ws.Repo], ws)
	}

	repos := make([]wsm.Repository, len(r.state.reg.Repositories))
	copy(repos, r.state.reg.Repositories)
	sort.SliceStable(repos, func(i, j int) bool {
		if repos[i].Name != repos[j].Name {
			return repos[i].Name < repos[j].Name
		}
		return repos[i].ID < repos[j].ID
	})

	out := &frontendv1.RosterRepositoryView{}
	for _, repo := range repos {
		rows := r.sectionRows(byRepo[repo.ID], rc, log.With(dlog.Context{"repo_id": string(repo.ID)}))
		delete(byRepo, repo.ID)
		section := &frontendv1.RosterRepoSection{
			Key: &frontendv1.RosterRepoKey{
				Repository: &workspacev1.RepositoryRef{Id: string(repo.ID), Dir: repo.Dir}},
			Header: sectionHeader(repo.Name, drawnLiveRowCount(rows)),
			Rows:   rows,
		}
		setRepoFold(section, repo.Folded)
		out.Sections = append(out.Sections, section)
	}
	r.assertRepositoryInvariant(byRepo, log)
	return out
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
func (r *resolver) assertRepositoryInvariant(byRepo map[ids.RepoID][]wsm.Workspace, log dlog.Logger) {
	for repo, orphans := range byRepo {
		log.Error("daemon.sidebar.repository_view",
			"workspaces name a repository the registry does not carry and were left out of the repo grouping",
			dlog.Context{
				"repo_id":             string(repo),
				"workspaces":          len(orphans),
				"invariant_violation": "workspace.Repo names no registered repository",
				"remediation":         "re-register the workspace's directory, which mints its repository row, or forget the workspace",
			})
	}
}

// taskView resolves the task grouping: one section per task, with the task's
// done check on its own header.
//
// UNASSIGNED WORKSPACES ARE NOT IN THIS VIEW AT ALL. There is no "no task"
// section: the task grouping answers "what is each task's work", and a
// workspace assigned to nothing is not an answer to that.
func (r *resolver) taskView(live []wsm.Workspace, rc rowContext, log dlog.Logger) *frontendv1.RosterTaskView {
	byTask := map[ids.TaskID][]wsm.Workspace{}
	for _, ws := range live {
		if ws.Task == nil {
			continue
		}
		byTask[*ws.Task] = append(byTask[*ws.Task], ws)
	}

	tasks := make([]wsm.Task, len(r.state.reg.Tasks))
	copy(tasks, r.state.reg.Tasks)
	sort.SliceStable(tasks, func(i, j int) bool {
		if !tasks[i].CreatedAt.Equal(tasks[j].CreatedAt) {
			return tasks[i].CreatedAt.Before(tasks[j].CreatedAt)
		}
		if tasks[i].Title != tasks[j].Title {
			return tasks[i].Title < tasks[j].Title
		}
		return tasks[i].ID < tasks[j].ID
	})

	out := &frontendv1.RosterTaskView{}
	for _, task := range tasks {
		rows := r.sectionRows(byTask[task.ID], rc, log.With(dlog.Context{"task_id": string(task.ID)}))
		delete(byTask, task.ID)
		section := &frontendv1.RosterTaskSection{
			Key: &frontendv1.RosterTaskKey{TaskId: string(task.ID)},
			Header: &frontendv1.RosterTaskSectionHeader{
				Label: &frontendv1.RosterLabel{Text: task.Title},
				Done:  &frontendv1.RosterTaskDone{Done: task.Done},
			},
			Rows: rows,
		}
		setTaskFold(section, task.Folded)
		out.Sections = append(out.Sections, section)
	}
	for task, orphans := range byTask {
		log.Error("daemon.sidebar.task_view",
			"workspaces are assigned to a task the registry does not carry and were left out of the task grouping",
			dlog.Context{
				"task_id":             string(task),
				"workspaces":          len(orphans),
				"invariant_violation": "workspace.Task names no registered task",
				"remediation":         "unassign the workspace or restore the task",
			})
	}
	return out
}

// mergedSection resolves the recently-merged section: the rows hoisted out of
// BOTH groupings, most recently merged first. It is rendered under both
// groupings because that these workspaces are done is the interesting fact,
// and it is FLAT — a settled merge has no family left to draw.
func (r *resolver) mergedSection(merged []wsm.Workspace, rc rowContext, log dlog.Logger) *frontendv1.RosterMergedSection {
	rc.tree = forest{}
	rows := &frontendv1.RosterRows{}
	for _, ws := range sortMerged(merged) {
		rows.Rows = append(rows.Rows, r.row(ws, rc, log))
	}
	section := &frontendv1.RosterMergedSection{
		Header: sectionHeader(MergedSectionLabel, len(rows.GetRows())),
		Rows:   rows,
	}
	setMergedFold(section, r.state.reg.View.MergedFolded)
	return section
}

// setShownGrouping states which grouping every page shows. The grouping is
// one of the two: SetRegistry refuses a registry naming anything else.
func setShownGrouping(roster *frontendv1.WorkspaceRoster, grouping wsm.Grouping) {
	if grouping == wsm.GroupingTask {
		roster.Shown = &frontendv1.WorkspaceRoster_ShownTask{ShownTask: &frontendv1.RosterShownTask{}}
		return
	}
	roster.Shown = &frontendv1.WorkspaceRoster_ShownRepository{ShownRepository: &frontendv1.RosterShownRepository{}}
}

// setTaskFold states a task's fold on its section. The fold is the daemon's
// so every page draws the same one (UpdateSidebarView).
func setTaskFold(section *frontendv1.RosterTaskSection, folded bool) {
	if folded {
		section.Fold = &frontendv1.RosterTaskSection_Collapsed{Collapsed: &frontendv1.RosterTaskSectionCollapsed{}}
		return
	}
	section.Fold = &frontendv1.RosterTaskSection_Expanded{Expanded: &frontendv1.RosterTaskSectionExpanded{}}
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
	rc.tree = nest(in, r.defaultBranches(), log)
	out := &frontendv1.RosterRows{}
	for _, ws := range rc.tree.roots {
		out.Rows = append(out.Rows, r.row(ws, rc, log))
	}
	return out
}

// defaultBranches names every registered repository's default branch. The
// branch lineage reads it to refuse a family derived from the default branch,
// which every ordinary workspace in a repository is cut from.
func (r *resolver) defaultBranches() map[ids.RepoID]string {
	out := make(map[ids.RepoID]string, len(r.state.reg.Repositories))
	for _, repo := range r.state.reg.Repositories {
		out[repo.ID] = repo.DefaultBranch
	}
	return out
}
