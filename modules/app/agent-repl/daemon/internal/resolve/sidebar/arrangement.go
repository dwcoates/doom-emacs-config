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
//   - inside one, its workspaces in NO subsection first, then each subsection
//     in its stored place;
//   - inside each of those, roster order (sortWorkspaces).
//
// So every workspace of an earlier repository precedes every one of a later
// repository on the tab bar, and a workspace dragged into an empty subsection
// at the bottom of its repository becomes that repository's last tab. Both
// surfaces read the order from arrange, so they cannot disagree about it.

// repoArrangement is one repository's place in the arrangement: the
// repository and its buckets, unsectioned first.
type repoArrangement struct {
	repo    wsm.Repository
	buckets []bucket
}

// bucket is one run of a repository's workspaces: those in no subsection
// (subsection nil) or those in one subsection, in roster order.
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
	for _, ws := range workspaces {
		if ws.Subsection != nil {
			bySubsection[*ws.Subsection] = append(bySubsection[*ws.Subsection], ws)
			continue
		}
		unsectioned[ws.Repo] = append(unsectioned[ws.Repo], ws)
	}
	subsections := map[ids.RepoID][]wsm.Subsection{}
	for _, sub := range sortSubsections(reg.Subsections) {
		subsections[sub.Repo] = append(subsections[sub.Repo], sub)
	}
	var out []repoArrangement
	for _, repo := range sortRepositories(reg.Repositories) {
		placed := repoArrangement{repo: repo, buckets: []bucket{{workspaces: sortWorkspaces(unsectioned[repo.ID])}}}
		delete(unsectioned, repo.ID)
		for _, sub := range subsections[repo.ID] {
			placed.buckets = append(placed.buckets, bucket{subsection: &sub, workspaces: sortWorkspaces(bySubsection[sub.ID])})
		}
		out = append(out, placed)
	}
	return out, unsectioned
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
// another repository, or a subsection of an unregistered repository.
//
// EACH IS UNREACHABLE. SQLite refuses a workspace in another repository's
// subsection (wsm's workspaces_subsection_same_repo triggers), a deleted
// subsection sets its workspaces' reference to NULL in the same statement,
// a repository's subsections cascade with it, and the registry reads all
// three in ONE snapshot (wsm.ReadArrangement). Reaching here is a defect in
// one of those, so it fails hard rather than drawing a guess.
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
	for _, ws := range reg.Workspaces {
		if ws.Subsection == nil {
			continue
		}
		sub, ok := subsections[*ws.Subsection]
		if !ok {
			r.failArrangement("a workspace names a subsection the registry does not carry",
				dlog.Context{"workspace_id": string(ws.ID), "subsection": string(*ws.Subsection)})
		}
		if sub.Repo != ws.Repo {
			r.failArrangement("a workspace sits in another repository's subsection",
				dlog.Context{"workspace_id": string(ws.ID), "subsection": string(sub.ID), "repo_id": string(ws.Repo), "subsection_repo_id": string(sub.Repo)})
		}
	}
}

// failArrangement records an arrangement invariant violation once, at ERROR,
// and panics with MESSAGE.
func (r *resolver) failArrangement(message string, ctx dlog.Context) {
	ctx["invariant_violation"] = "every subsection a workspace names is one of its own repository's, read in the same snapshot"
	ctx["remediation"] = "compose the registry from one wsm.ReadArrangement"
	r.log.Global().Error("daemon.sidebar.set_registry", message, ctx)
	panic(fmt.Sprintf("sidebar: %s: %v", message, ctx))
}
