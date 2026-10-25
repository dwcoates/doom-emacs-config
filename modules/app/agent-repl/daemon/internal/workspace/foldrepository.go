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

// FoldRequester is who asked for a fold: the rpc it came by, and the request
// id its client sent with it (empty when the client sent none). Every fold's
// records carry it, so a fold that stood sessions down is never anonymous.
type FoldRequester struct {
	RPC       string
	RequestID string
}

// context is the requester as a fold's records carry it.
func (r FoldRequester) context() dlog.Context {
	return dlog.Context{"requested_by_rpc": r.RPC, "request_id": r.RequestID}
}

// FoldRepository records whether a repository's roster section is collapsed,
// then republishes the roster: the sidebar hides a collapsed section's rows
// and the Emacs tab bar hides that repository's workspaces, both off the one
// pushed fold. COLLAPSING HIDES THE SECTION'S OPEN WORKSPACES and stands down
// the sessions of those it does not spare (foldTabs); EXPANDING starts
// nothing, the editor asks with OpenTabs. Selecting a workspace expands the
// folds holding it without coming here (wsm's setCurrent).
func (v *verbs) FoldRepository(ctx context.Context, repo ids.RepoID, folded bool, requester FoldRequester) error {
	log := v.deps.Log.Global().With(dlog.Merge(dlog.Context{"repo_id": string(repo), "folded": folded}, requester.context()))
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
		members: wsm.RepositoryMembers(repo),
	})
}

// opFoldSubsection is the operation FoldSubsection's records carry.
const opFoldSubsection = "daemon.workspace.fold_subsection"

// FoldSubsection records whether a subsection of a repository's section is
// collapsed, with the effect a repository's collapse has (FoldRepository):
// the sidebar hides its rows, the Emacs tab bar folds its workspaces' tabs,
// and COLLAPSING HIDES ITS OPEN WORKSPACES, standing down those it does not
// spare. A workspace sits in its root's subsection, so a family folds whole.
// An unknown subsection is refused unknown_subsection.
func (v *verbs) FoldSubsection(ctx context.Context, sub ids.SubsectionID, folded bool, requester FoldRequester) error {
	log := v.deps.Log.Global().With(dlog.Merge(dlog.Context{"subsection": string(sub), "folded": folded}, requester.context()))
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
		members: wsm.SubsectionMembers(sub),
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
	// members are the workspaces whose tabs the fold hides.
	members wsm.FoldMembers
}

// foldTabs is THE ONE PATH OF A FOLD THAT HIDES TABS: the fold recorded, the
// roster republished, and, on a collapse, every open workspace it holds
// hidden. Expanding starts nothing: the editor asks with OpenTabs.
//
// A FOLD HIDES; IT NEVER CHANGES A STATUS (owner ruling, 2026-10-25). A
// workspace its record spares (wsm.Workspace.FoldSpare: a one-shot, a merge
// pending) keeps its session. Every other one is asked to stand down, and
// the stand-down gate holds that back while a turn or detached work is in
// flight, taking it only on the free edge after (drain's withheld path).
func (v *verbs) foldTabs(ctx context.Context, log dlog.Logger, spec foldTabsSpec) error {
	workspaces, err := v.recordFold(ctx, log, spec)
	if err != nil {
		return err
	}
	log.Info(spec.op, "recorded the "+spec.what+"'s fold", nil)
	v.republishRegistry(ctx, log, spec.op)
	if !spec.folded {
		return nil
	}
	hidden, spared, standingDown := []string{}, map[string]string{}, []string{}
	for _, ws := range workspaces {
		if !spec.members.StandsDown(ws) {
			continue
		}
		hidden = append(hidden, string(ws.ID))
		if spare, ok := ws.FoldSpare(); ok {
			spared[string(ws.ID)] = string(spare)
			continue
		}
		standingDown = append(standingDown, string(ws.ID))
		v.deps.StandDowns.StandDownWithheld(ws.ID)
	}
	log.Info(spec.op, "the collapsed "+spec.what+"'s open workspaces are hidden; the spared keep running, the rest stand down once free",
		dlog.Context{"hidden": hidden, "spared": spared, "standing_down": standingDown})
	return nil
}

// recordFold writes the fold, answering the workspaces as they stood for a
// collapse to stand down (nil for an expand).
//
// A COLLAPSE NEVER STANDS DOWN THE CURRENT WORKSPACE. A collapse of a section
// holding it is refused holds_current and nothing is written. The check and
// the write run under the selection lock, which every select holds while it
// moves the selection (selectCurrent), so a switch racing the collapse lands
// wholly before the check or wholly after the write: no interleaving records
// a collapse over the workspace the user has just switched to.
func (v *verbs) recordFold(ctx context.Context, log dlog.Logger, spec foldTabsSpec) ([]wsm.Workspace, error) {
	var workspaces []wsm.Workspace
	if spec.folded {
		v.selection.Lock()
		defer v.selection.Unlock()
		listed, err := v.deps.DB.ListWorkspaces(ctx)
		if err != nil {
			log.Error(spec.op, "could not list the "+spec.what+"'s workspaces; the collapse was not recorded", dlog.Context{"cause": err.Error()})
			return nil, fmt.Errorf("fold %s %q: list its workspaces: %w", spec.what, spec.target, err)
		}
		for _, ws := range listed {
			if ws.Current && spec.members.HoldsCurrent(&ws) {
				return nil, refuse(log, "UpdateSidebarView", ArmHoldsCurrent,
					fmt.Sprintf("the %s holds the current workspace %q; collapsing it would stand that workspace down", spec.what, ws.ID),
					false)
			}
		}
		workspaces = listed
	}
	if err := spec.write(); err != nil {
		if refused := spec.unknown(err); refused != nil {
			return nil, refused
		}
		log.Error(spec.op, "could not record the "+spec.what+"'s fold", dlog.Context{"cause": err.Error()})
		return nil, fmt.Errorf("fold %s %q: %w", spec.what, spec.target, err)
	}
	return workspaces, nil
}
