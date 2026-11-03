package sidebar

import (
	"fmt"
	"slices"
	"sort"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// THE ARRANGEMENT IS ONE ORDER, walked by two surfaces (agentrepl.v1
// ArrangeSidebar). The repository grouping draws it and the editor's tab bar
// lists the open workspaces in it:
//
//   - repository by repository, in the repositories' stored place;
//   - inside one, its WALK (wsm.Walk): its families in no subsection and its
//     subsections, interleaved as arranged;
//   - inside each run, roster order (sortWorkspaces).
//
// So every workspace of an earlier repository precedes every one of a later
// repository on the tab bar, and a workspace dragged into a subsection takes
// its tab where the subsection stands. Both surfaces read the order from
// arrange, so they cannot disagree about it.

// repoArrangement is one repository's place in the arrangement: the
// repository and its buckets, in its walk's order.
type repoArrangement struct {
	repo    wsm.Repository
	buckets []bucket
}

// bucket is one run of a repository's workspaces: consecutive families in no
// subsection (subsection nil), or one subsection's workspaces, in roster
// order. Two unsectioned runs are never adjacent: a run ends only where a
// subsection stands. A one-shot is filed like any other workspace: its mark
// draws no bucket of its own (owner request, 2026-11-03).
type bucket struct {
	subsection *wsm.Subsection
	workspaces []wsm.Workspace
}

// arrange files WORKSPACES into the registry's arrangement. Every
// repository and every subsection is present, with or without workspaces, so
// a grouping keeps its shape as workspaces move. A workspace whose repository
// the registry does not carry is answered in the orphans map rather than
// placed (the caller records it, as assertRepositoryInvariant does).
func arrange(reg Registry, workspaces []wsm.Workspace) ([]repoArrangement, map[ids.RepoID][]wsm.Workspace) {
	bySubsection := map[ids.SubsectionID][]wsm.Workspace{}
	unsectioned := map[ids.RepoID][]wsm.Workspace{}
	section := sectionRepos(workspaces)
	for _, ws := range workspaces {
		repo := section[ws.ID]
		if ws.Subsection != nil {
			bySubsection[*ws.Subsection] = append(bySubsection[*ws.Subsection], ws)
			continue
		}
		unsectioned[repo] = append(unsectioned[repo], ws)
	}
	subsections := map[ids.RepoID][]wsm.Subsection{}
	for _, sub := range sortSubsections(reg.Subsections) {
		subsections[sub.Repo] = append(subsections[sub.Repo], sub)
	}
	var out []repoArrangement
	for _, repo := range sortRepositories(reg.Repositories) {
		placed := repoArrangement{repo: repo, buckets: walkBuckets(unsectioned[repo.ID], subsections[repo.ID], bySubsection)}
		delete(unsectioned, repo.ID)
		out = append(out, placed)
	}
	return out, unsectioned
}

// sectionRepos answers, for every workspace of WORKSPACES, the repository
// whose section draws it: its FAMILY ROOT's (familyRoots). A FAMILY NEVER
// SPLITS ACROSS SECTIONS, as it never splits across subsections: a child of
// another repository than its parent's nests under that parent, in the
// parent's section, where its row begins a repository divider
// (frontend.v1.RosterOpenRow.repository_divider).
func sectionRepos(workspaces []wsm.Workspace) map[ids.WorkspaceID]ids.RepoID {
	root := familyRoots(workspaces)
	repoOf := make(map[ids.WorkspaceID]ids.RepoID, len(workspaces))
	for _, ws := range workspaces {
		repoOf[ws.ID] = ws.Repo
	}
	out := make(map[ids.WorkspaceID]ids.RepoID, len(workspaces))
	for _, ws := range workspaces {
		out[ws.ID] = repoOf[root[ws.ID]]
	}
	return out
}

// walkBuckets files one repository's UNSECTIONED workspaces and its SUBS'
// workspaces (BY_SUBSECTION) into its walk: each family of the unsectioned
// ones stands where its top-level row's place puts it among the subsections
// (wsm.Walk), consecutive families sharing one run.
func walkBuckets(unsectioned []wsm.Workspace, subs []wsm.Subsection, bySubsection map[ids.SubsectionID][]wsm.Workspace) []bucket {
	root := familyRoots(unsectioned)
	family := map[ids.WorkspaceID][]wsm.Workspace{}
	var tops []wsm.Workspace
	for _, ws := range unsectioned {
		family[root[ws.ID]] = append(family[root[ws.ID]], ws)
		if root[ws.ID] == ws.ID {
			tops = append(tops, ws)
		}
	}
	var out []bucket
	for _, entry := range wsm.Walk(tops, func(ws wsm.Workspace) int64 { return ws.Place }, subs) {
		if entry.Subsection != nil {
			out = append(out, bucket{subsection: entry.Subsection, workspaces: sortWorkspaces(bySubsection[entry.Subsection.ID])})
			continue
		}
		if len(out) == 0 || out[len(out)-1].subsection != nil {
			out = append(out, bucket{})
		}
		last := &out[len(out)-1]
		last.workspaces = sortWorkspaces(append(last.workspaces, family[entry.Root.ID]...))
	}
	return out
}

// sortRepositories is THE repository order every per-repository list in the
// roster takes: the repositories' stored place (wsm's arrange.go), unique
// across the registry. The repository grouping's sections, the
// recently-merged band's groups and the tab bar all follow it. It sorts a
// copy.
func sortRepositories(in []wsm.Repository) []wsm.Repository {
	out := slices.Clone(in)
	sort.SliceStable(out, func(i, j int) bool { return out[i].Place < out[j].Place })
	return out
}

// sortSubsections orders subsections by their stored place, which is unique
// within a repository. It sorts a copy.
func sortSubsections(in []wsm.Subsection) []wsm.Subsection {
	out := slices.Clone(in)
	sort.SliceStable(out, func(i, j int) bool { return out[i].Place < out[j].Place })
	return out
}

// assertArrangement refuses a registry whose arrangement cannot be drawn: a
// workspace naming a subsection the registry does not carry, or one of
// another repository than its FAMILY's, or a subsection of an unregistered
// repository.
//
// A FAMILY'S REPOSITORY IS ITS ROOT'S (sectionRepos). A child reads its
// subsection from its root (wsm's rootSubsectionColumn), and a child of
// another repository than its parent's nests in that family, so the
// subsection it names is its root's repository's, never necessarily its own.
//
// EACH IS UNREACHABLE. SQLite refuses a root in another repository's
// subsection (wsm's workspaces_subsection_same_repo triggers), a child stores
// none (workspaces_child_stores_no_subsection), a deleted subsection sets its
// workspaces' reference to NULL in the same statement, a repository's
// subsections cascade with it, and the registry reads all three in ONE
// snapshot (wsm.ReadArrangement). Reaching here is a defect in one of those,
// so it fails hard rather than drawing a guess.
func (r *resolver) assertArrangement(reg Registry) {
	repos := map[ids.RepoID]bool{}
	for _, repo := range reg.Repositories {
		repos[repo.ID] = true
	}
	subsections := map[ids.SubsectionID]wsm.Subsection{}
	for _, sub := range reg.Subsections {
		if !repos[sub.Repo] {
			r.failArrangement("a subsection names a repository the registry does not carry",
				dlog.Context{"subsection": string(sub.ID), "repo_id": string(sub.Repo)})
		}
		subsections[sub.ID] = sub
	}
	family := sectionRepos(reg.Workspaces)
	for _, ws := range reg.Workspaces {
		if ws.Subsection == nil {
			continue
		}
		sub, ok := subsections[*ws.Subsection]
		if !ok {
			r.failArrangement("a workspace names a subsection the registry does not carry",
				dlog.Context{"workspace_id": string(ws.ID), "subsection": string(*ws.Subsection)})
		}
		if sub.Repo != family[ws.ID] {
			r.failArrangement("a workspace sits in another repository's subsection",
				dlog.Context{"workspace_id": string(ws.ID), "subsection": string(sub.ID), "repo_id": string(ws.Repo), "family_repo_id": string(family[ws.ID]), "subsection_repo_id": string(sub.Repo)})
		}
	}
}

// failArrangement records an arrangement invariant violation once, at ERROR,
// and panics with MESSAGE.
func (r *resolver) failArrangement(message string, ctx dlog.Context) {
	ctx["invariant_violation"] = "every subsection a workspace names is one of its family root's repository's, read in the same snapshot"
	ctx["remediation"] = "compose the registry from one wsm.ReadArrangement"
	r.log.Global().Error("daemon.sidebar.set_registry", message, ctx)
	panic(fmt.Sprintf("sidebar: %s: %v", message, ctx))
}
