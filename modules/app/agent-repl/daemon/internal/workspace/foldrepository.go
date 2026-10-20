package workspace

import (
	"context"
	"errors"
	"fmt"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// opFoldRepository is the operation FoldRepository's records carry.
const opFoldRepository = "daemon.workspace.fold_repository"

// FoldRepository records whether a repository's roster section is collapsed,
// then republishes the roster: the sidebar hides a collapsed section's rows
// and the Emacs tab bar hides that repository's workspaces, both off the one
// pushed fold. COLLAPSING STANDS THE SECTION'S OPEN WORKSPACES' SESSIONS DOWN
// (only visible workspaces run), each as hiding it would; EXPANDING starts
// nothing, the editor asks with OpenTabs.
func (v *verbs) FoldRepository(ctx context.Context, repo ids.RepoID, folded bool) error {
	log := v.deps.Log.Global().With(dlog.Context{"repo_id": string(repo), "folded": folded})
	return v.foldTabs(ctx, log, foldTabsSpec{
		op:     opFoldRepository,
		what:   "repository",
		target: string(repo),
		folded: folded,
		write:  func() error { return v.deps.DB.SetRepositoryFolded(ctx, repo, folded) },
		unknown: func(err error) error {
			if !errors.Is(err, wsm.ErrNotFound) {
				return nil
			}
			return refuse(log, "FoldRepository", ArmUnknownRepository, fmt.Sprintf("no repository %q is registered", repo), true)
		},
		holds: func(ws wsm.Workspace) bool { return ws.Repo == repo },
	})
}

// opFoldSubsection is the operation FoldSubsection's records carry.
const opFoldSubsection = "daemon.workspace.fold_subsection"

// FoldSubsection records whether a subsection of a repository's section is
// collapsed, with the effect a repository's collapse has (FoldRepository):
// the sidebar hides its rows, the Emacs tab bar folds its workspaces' tabs,
// and COLLAPSING STANDS ITS OPEN WORKSPACES' SESSIONS DOWN. A workspace sits
// in its root's subsection, so a family folds whole. An unknown subsection is
// refused unknown_subsection.
func (v *verbs) FoldSubsection(ctx context.Context, sub ids.SubsectionID, folded bool) error {
	log := v.deps.Log.Global().With(dlog.Context{"subsection": string(sub), "folded": folded})
	return v.foldTabs(ctx, log, foldTabsSpec{
		op:     opFoldSubsection,
		what:   "subsection",
		target: string(sub),
		folded: folded,
		write:  func() error { return v.deps.DB.SetSubsectionFolded(ctx, sub, folded) },
		unknown: func(err error) error {
			var unknown *wsm.UnknownSubsectionError
			if !errors.As(err, &unknown) {
				return nil
			}
			return refuseWith(log, "UpdateSidebarView", ArmUnknownSubsection, err.Error(), true,
				map[string]any{"subsection": &agentreplv1.SubsectionRef{Id: string(sub)}})
		},
		holds: func(ws wsm.Workspace) bool { return ws.Subsection != nil && *ws.Subsection == sub },
	})
}

// foldTabsSpec is one fold that hides workspaces' tabs (a repository's, a
// subsection's): the write, its refusal of an unknown target, and which
// workspaces it holds.
type foldTabsSpec struct {
	// op is the operation every record carries; what names the target's kind
	// in them, and target its id.
	op, what, target string
	// folded is the fold asked for.
	folded bool
	// write records the fold.
	write func() error
	// unknown answers the refusal for a write that named no target, nil for
	// any other failure.
	unknown func(error) error
	// holds reports whether a workspace's tab the fold hides.
	holds func(wsm.Workspace) bool
}

// foldTabs is THE ONE PATH OF A FOLD THAT HIDES TABS: the fold recorded, the
// roster republished, and, on a collapse, every open workspace it holds stood
// down as hiding it would, so only visible workspaces run. Expanding starts
// nothing: the editor asks with OpenTabs.
func (v *verbs) foldTabs(ctx context.Context, log dlog.Logger, spec foldTabsSpec) error {
	if err := spec.write(); err != nil {
		if refused := spec.unknown(err); refused != nil {
			return refused
		}
		log.Error(spec.op, "could not record the "+spec.what+"'s fold", dlog.Context{"cause": err.Error()})
		return fmt.Errorf("fold %s %q: %w", spec.what, spec.target, err)
	}
	log.Info(spec.op, "recorded the "+spec.what+"'s fold", nil)
	v.republishRegistry(ctx, log, spec.op)
	if !spec.folded {
		return nil
	}
	workspaces, err := v.deps.DB.ListWorkspaces(ctx)
	if err != nil {
		log.Error(spec.op, "could not list the collapsed "+spec.what+"'s workspaces; their sessions were not stood down", dlog.Context{"cause": err.Error()})
		return fmt.Errorf("fold %s %q: list its workspaces: %w", spec.what, spec.target, err)
	}
	var withheld []string
	for _, ws := range workspaces {
		if !spec.holds(ws) || ws.Closed {
			continue
		}
		withheld = append(withheld, string(ws.ID))
		v.deps.StandDowns.StandDownWithheld(ws.ID)
	}
	log.Info(spec.op, "the collapsed "+spec.what+"'s open workspaces are stood down", dlog.Context{"workspaces": withheld})
	return nil
}
