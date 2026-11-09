package sidebar

import (
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// A FAMILY'S CHILDREN ARE GROUPED BY REPOSITORY (owner ruling, 2026-11-09;
// frontend.v1.RosterOpenRow.repository_blocks): under each parent, first the
// children of the parent's own repository, then ONE BLOCK per other
// repository, the blocks in the parent's arranged order
// (wsm.OrderRepositoryBlocks). Inside each group the children keep the order
// they are handed in (roster order). The tab bar, the sidebar and an agent's
// arrangement read all walk the grouping this file states, so none of them
// can disagree about where a block's rows stand.

// repoGroup is one repository's children of a parent: a block when its
// repository is not the parent's.
type repoGroup[T any] struct {
	repo    ids.RepoID
	members []T
}

// blockOrder answers each parent's block order from the registry: its
// arranged blocks and the repositories' stored places.
type blockOrder struct {
	stored map[ids.WorkspaceID][]wsm.RepositoryBlock
	places map[ids.RepoID]int64
}

// newBlockOrder indexes REG's arranged blocks by parent.
func newBlockOrder(reg Registry) blockOrder {
	stored := map[ids.WorkspaceID][]wsm.RepositoryBlock{}
	for _, b := range reg.RepositoryBlocks {
		stored[b.Parent] = append(stored[b.Parent], b)
	}
	return blockOrder{stored: stored, places: wsm.RepositoryPlaces(reg.Repositories)}
}

// group splits KIDS, the children drawn under PARENT (of repository OWN), into
// its own-repository children and its blocks, in drawn order. REPO_OF answers
// a child's repository.
func group[T any](o blockOrder, parent wsm.Workspace, kids []T, repoOf func(T) ids.RepoID) (own []T, blocks []repoGroup[T]) {
	byRepo := map[ids.RepoID][]T{}
	var present []ids.RepoID
	for _, kid := range kids {
		repo := repoOf(kid)
		if repo == parent.Repo {
			own = append(own, kid)
			continue
		}
		if _, seen := byRepo[repo]; !seen {
			present = append(present, repo)
		}
		byRepo[repo] = append(byRepo[repo], kid)
	}
	for _, repo := range wsm.OrderRepositoryBlocks(present, o.stored[parent.ID], o.places) {
		blocks = append(blocks, repoGroup[T]{repo: repo, members: byRepo[repo]})
	}
	return own, blocks
}

// flatten answers OWN then every block's members, in drawn order: the order a
// reader that does not draw blocks (the tab walk, an agent's arrangement)
// takes the children in.
func flatten[T any](own []T, blocks []repoGroup[T]) []T {
	out := append([]T(nil), own...)
	for _, b := range blocks {
		out = append(out, b.members...)
	}
	return out
}
