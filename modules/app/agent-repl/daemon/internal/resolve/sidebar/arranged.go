package sidebar

import (
	"agent-repld/internal/dlog"
	"agent-repld/internal/wsm"
)

// ArrangedRepository is one repository of the arrangement as an agent reads
// it (agentrepl.v1.GetSidebarArrangement): its entries in walk order.
type ArrangedRepository struct {
	Repository wsm.Repository
	Entries    []ArrangedEntry
}

// ArrangedEntry is one entry of a repository: a family in no subsection
// (Subsection nil), or a subsection and its families.
type ArrangedEntry struct {
	Workspace  ArrangedWorkspace
	Subsection *ArrangedSubsection
}

// ArrangedSubsection is one subsection and its families.
type ArrangedSubsection struct {
	Subsection wsm.Subsection
	Workspaces []ArrangedWorkspace
}

// ArrangedWorkspace is one open workspace and its nested family.
type ArrangedWorkspace struct {
	Workspace wsm.Workspace
	Children  []ArrangedWorkspace
}

// Arranged answers the registry's arrangement for an agent: every repository
// and subsection in the order arrange() gives the sidebar and the tab bar,
// each run's families nested exactly as the sidebar nests them (nest), with
// every workspace that is neither closed, merged nor a hidden merge
// workspace. A hidden one keeps its place. A closed workspace is left out and
// its open children take its place, as the sidebar hoists them.
func Arranged(reg Registry, log dlog.Logger) []ArrangedRepository {
	var listed []wsm.Workspace
	for _, ws := range reg.Workspaces {
		if !ws.MergeHidden() && ws.MergedAt == nil {
			listed = append(listed, ws)
		}
	}
	arranged, _ := arrange(reg, listed)
	out := make([]ArrangedRepository, 0, len(arranged))
	for _, placed := range arranged {
		repo := ArrangedRepository{Repository: placed.repo}
		for _, b := range placed.buckets {
			if b.subsection != nil {
				repo.Entries = append(repo.Entries, ArrangedEntry{Subsection: &ArrangedSubsection{Subsection: *b.subsection, Workspaces: families(b.workspaces, log)}})
				continue
			}
			for _, family := range families(b.workspaces, log) {
				repo.Entries = append(repo.Entries, ArrangedEntry{Workspace: family})
			}
		}
		out = append(out, repo)
	}
	return out
}

// families nests one run's workspaces as the sidebar does and lists its open
// families root by root, a closed workspace's open children in its place.
func families(run []wsm.Workspace, log dlog.Logger) []ArrangedWorkspace {
	tree := nest(run, log)
	var list func(ws []wsm.Workspace) []ArrangedWorkspace
	list = func(ws []wsm.Workspace) []ArrangedWorkspace {
		var out []ArrangedWorkspace
		for _, w := range ws {
			children := list(tree.children[w.ID])
			if w.Closed {
				out = append(out, children...)
				continue
			}
			out = append(out, ArrangedWorkspace{Workspace: w, Children: children})
		}
		return out
	}
	return list(tree.roots)
}
