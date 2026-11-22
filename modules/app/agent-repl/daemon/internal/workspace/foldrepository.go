package workspace

import (
	"context"
	"errors"
	"fmt"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/resolve/sidebar"
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
// nothing, the editor asks with OpenTabs. Selecting a workspace opens no fold:
// the roster draws the current workspace inside the fold that hides it.
func (v *verbs) FoldRepository(ctx context.Context, repo ids.RepoID, folded bool, requester FoldRequester) error {
	log := v.deps.Log.Global().With(dlog.Merge(dlog.Context{"repo_id": string(repo), "folded": folded}, requester.context()))
	return v.foldTabs(ctx, log, v.repositoryFold(ctx, log, repo, folded))
}

// repositoryFold is the fold of REPO's section as a foldTabsSpec.
func (v *verbs) repositoryFold(ctx context.Context, log dlog.Logger, repo ids.RepoID, folded bool) foldTabsSpec {
	return foldTabsSpec{
		op:        opFoldRepository,
		what:      "repository",
		target:    string(repo),
		attention: wsm.AttentionOpenedRepository,
		folded:    folded,
		landsOff:  true,
		write:     func() error { return v.deps.DB.SetRepositoryFolded(ctx, repo, folded) },
		unknown: func(err error) error {
			if !errors.Is(err, wsm.ErrNotFound) {
				return nil
			}
			return refuse(log, "FoldRepository", ArmUnknownRepository, fmt.Sprintf("no repository %q is registered", repo), true)
		},
		members: func(all []wsm.Workspace) wsm.FoldMembers { return wsm.RepositoryMembers(wsm.SectionRepos(all), repo) },
	}
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
	return v.foldTabs(ctx, log, v.subsectionFold(ctx, log, sub, folded))
}

// subsectionFold is the fold of SUB as a foldTabsSpec (repositoryFold).
func (v *verbs) subsectionFold(ctx context.Context, log dlog.Logger, sub ids.SubsectionID, folded bool) foldTabsSpec {
	return foldTabsSpec{
		op:        opFoldSubsection,
		what:      "subsection",
		target:    string(sub),
		attention: wsm.AttentionOpenedSubsection,
		folded:    folded,
		landsOff:  true,
		write:     func() error { return v.deps.DB.SetSubsectionFolded(ctx, sub, folded) },
		unknown: func(err error) error {
			var unknown *wsm.UnknownSubsectionError
			if !errors.As(err, &unknown) {
				return nil
			}
			return refuseWith(log, "UpdateSidebarView", ArmUnknownSubsection, err.Error(), true,
				map[string]any{"subsection": &agentreplv1.SubsectionRef{Id: string(sub)}})
		},
		members: func([]wsm.Workspace) wsm.FoldMembers { return wsm.SubsectionMembers(sub) },
	}
}

// opFoldChildren is the operation FoldChildren's records carry.
const opFoldChildren = "daemon.workspace.fold_children"

// FoldChildren records whether workspace ws's children are folded, with the
// effect a subsection's collapse has (FoldSubsection): the sidebar hides the
// children's rows, the Emacs tab bar folds their tabs, and COLLAPSING HIDES
// THE OPEN WORKSPACES BELOW ws, standing down those it does not spare. A
// collapse of the children of an ancestor of the current workspace SELECTS
// ws first (selectParentOverCurrentDescendant), so the collapse never stands
// the current workspace down. AN EXPAND ALSO EXPANDS EVERY FOLD ABOVE ws, in
// the same write and republish: each ancestor's children fold, the fold of
// the subsection its family sits in and the fold of its repository's section,
// since a row a fold covers is drawn only while it is selected, working or
// unviewed and so would stay hidden under an open fold of its own. Each fold
// it opens leaves the user's attention stamp. An unknown workspace is refused
// unknown_workspace.
func (v *verbs) FoldChildren(ctx context.Context, ws ids.WorkspaceID, folded bool, requester FoldRequester) error {
	log := v.deps.Log.Global().With(dlog.Merge(dlog.Context{"workspace": string(ws), "folded": folded}, requester.context()))
	if folded {
		if err := v.selectParentOverCurrentDescendant(ctx, log, ws); err != nil {
			return err
		}
	}
	var opened []wsm.OpenedFold
	return v.foldTabs(ctx, log, foldTabsSpec{
		op:        opFoldChildren,
		what:      "workspace's children",
		target:    string(ws),
		attention: wsm.AttentionOpenedChildren,
		folded:    folded,
		write: func() error {
			if folded {
				return v.deps.DB.SetChildrenFolded(ctx, ws, true)
			}
			var err error
			opened, err = v.deps.DB.ExpandChildrenWithAncestry(ctx, ws)
			return err
		},
		alsoOpened: func() []wsm.OpenedFold { return opened },
		unknown: func(err error) error {
			if !errors.Is(err, wsm.ErrNotFound) {
				return nil
			}
			return refuseWith(log, "UpdateSidebarView", ArmUnknownWorkspace, fmt.Sprintf("no workspace %q is registered", ws), true, nil)
		},
		members: func(all []wsm.Workspace) wsm.FoldMembers { return wsm.DescendantMembers(all, ws) },
	})
}

// selectParentOverCurrentDescendant is THE FOLD OF A PARENT'S CHILDREN OVER
// THE CURRENT CHILD (owner request, 2026-11-16): folding the children of a
// workspace whose descendant is current selects the workspace itself, rather
// than blocking the fold, so the user lands on the row that stays visible.
// The select is conditional on that descendant still being current, so a
// switch the user made meanwhile stands. Should one still land inside the
// family before the fold's own check, recordFold refuses the collapse
// holds_current as before.
func (v *verbs) selectParentOverCurrentDescendant(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID) error {
	listed, err := v.deps.DB.ListWorkspaces(ctx)
	if err != nil {
		log.Error(opFoldChildren, "could not list the workspaces; the collapse was not recorded", dlog.Context{"cause": err.Error()})
		return fmt.Errorf("fold workspace's children %q: list its workspaces: %w", ws, err)
	}
	members := wsm.DescendantMembers(listed, ws)
	for _, w := range listed {
		if !w.Current || !members.HoldsCurrent(&w) {
			continue
		}
		log.Info(opFoldChildren, "the collapsed children hold the current workspace; the parent is selected instead of the collapse being refused",
			dlog.Context{"current": string(w.ID)})
		_, err := v.selectCurrent(ctx, log, ws, &w.ID, Switch{History: RecordSelection})
		return err
	}
	return nil
}

// foldTabsSpec is one fold that hides workspaces' tabs (a repository's, a
// subsection's): the write, its refusal of an unknown target, and which
// workspaces it holds.
type foldTabsSpec struct {
	// op is the operation every record carries; what names the target's kind
	// in them, and target its id.
	op, what, target string
	// attention is the stamp an EXPAND of the fold leaves: the user opened
	// it themselves, which the deploy's fold sweep spares for an hour
	// (internal/foldsweep).
	attention wsm.AttentionKind
	// folded is the fold asked for.
	folded bool
	// write records the fold.
	write func() error
	// alsoOpened answers the other folds an expand's write opened, read after
	// it; each leaves its own attention stamp beside the target's. Nil for a
	// fold that opens only its target.
	alsoOpened func() []wsm.OpenedFold
	// unknown answers the refusal for a write that named no target, nil for
	// any other failure.
	unknown func(error) error
	// members are the workspaces whose tabs the fold hides, given every
	// workspace as it stands (a subsection's membership reads none of them).
	members func(all []wsm.Workspace) wsm.FoldMembers
	// landsOff is set by the folds of a section (a repository's, a
	// subsection's): a collapse holding the current workspace moves the
	// selection off it (landOffFold). A fold without it (a workspace's
	// children) refuses such a collapse holds_current, as the roster says.
	landsOff bool
}

// foldTabs is THE ONE PATH OF A FOLD THAT HIDES TABS: the fold recorded, the
// roster republished, and, on a collapse, every open workspace it holds
// hidden. Expanding starts nothing: the editor asks with OpenTabs.
//
// A FOLD HIDES; IT NEVER CHANGES A STATUS (owner ruling, 2026-10-25). A
// workspace its record spares (wsm.Workspace.FoldSpare: a one-shot, a merge
// pending) keeps its session. Every other one is asked to stand down
// drain.FoldStandDownDelay after the latest collapse that folded it (owner
// request, 2026-10-30: the pages go at once, the shims the delay later), and
// the stand-down gate holds that back while a turn or detached work is in
// flight, taking it only on the free edge after (drain's withheld path).
func (v *verbs) foldTabs(ctx context.Context, log dlog.Logger, spec foldTabsSpec) error {
	workspaces, members, err := v.recordFold(ctx, log, spec)
	if err != nil {
		return err
	}
	log.Info(spec.op, "recorded the "+spec.what+"'s fold", nil)
	v.republishRegistry(ctx, log, spec.op)
	if !spec.folded {
		// EVERY EXPAND IS THE USER'S OWN: only the sidebar's rpcs reach a
		// fold, and the sweep never expands.
		stamps := []wsm.OpenedFold{{Kind: spec.attention, Target: spec.target}}
		if spec.alsoOpened != nil {
			stamps = append(stamps, spec.alsoOpened()...)
		}
		for _, stamp := range stamps {
			if err := v.deps.DB.StampUserAttention(ctx, stamp.Kind, stamp.Target, v.now()); err != nil {
				log.Error(spec.op, "could not stamp the user's open of the "+spec.what, dlog.Context{"cause": err.Error(), "attention": string(stamp.Kind), "stamped": stamp.Target})
				return fmt.Errorf("stamp the open of %s %q: %w", spec.what, stamp.Target, err)
			}
		}
		return nil
	}
	hidden, spared, standingDown := []string{}, map[string]string{}, []string{}
	for _, ws := range workspaces {
		if !members.StandsDown(ws) {
			continue
		}
		hidden = append(hidden, string(ws.ID))
		if spare, ok := ws.FoldSpare(); ok {
			spared[string(ws.ID)] = string(spare)
			continue
		}
		standingDown = append(standingDown, string(ws.ID))
		v.deps.StandDowns.StandDownFolded(ws.ID)
	}
	log.Info(spec.op, "the collapsed "+spec.what+"'s open workspaces are hidden; the spared keep running, the rest stand down once the fold's delay is over and they are free",
		dlog.Context{"hidden": hidden, "spared": spared, "standing_down": standingDown})
	return nil
}

// recordFold writes the fold, answering the workspaces as they stood for a
// collapse to stand down and the members of the fold (both nil for an expand).
//
// A COLLAPSE NEVER STANDS DOWN THE CURRENT WORKSPACE. A collapse of a section
// holding it first moves the selection to the workspace the user was on
// before, among the tabs the collapse leaves drawn (landOffFold), and a
// collapse with no such workspace is refused holds_current, nothing written.
// The check, the landing and the write run under the selection lock, which
// every select holds while it moves the selection (selectCurrent), so a
// switch racing the collapse lands wholly before the check or wholly after
// the write: no interleaving records a collapse over the workspace the user
// has just switched to.
func (v *verbs) recordFold(ctx context.Context, log dlog.Logger, spec foldTabsSpec) ([]wsm.Workspace, wsm.FoldMembers, error) {
	var workspaces []wsm.Workspace
	var members wsm.FoldMembers
	if spec.folded {
		v.selection.Lock()
		defer v.selection.Unlock()
		listed, err := v.deps.DB.ListWorkspaces(ctx)
		if err != nil {
			log.Error(spec.op, "could not list the "+spec.what+"'s workspaces; the collapse was not recorded", dlog.Context{"cause": err.Error()})
			return nil, nil, fmt.Errorf("fold %s %q: list its workspaces: %w", spec.what, spec.target, err)
		}
		members = spec.members(listed)
		for _, ws := range listed {
			if !ws.Current || !members.HoldsCurrent(&ws) {
				continue
			}
			if !spec.landsOff {
				return nil, nil, refuse(log, "UpdateSidebarView", ArmHoldsCurrent,
					fmt.Sprintf("the %s holds the current workspace %q; collapsing it would stand that workspace down", spec.what, ws.ID),
					false)
			}
			if err := v.landOffFold(ctx, log, spec, members, ws.ID); err != nil {
				return nil, nil, err
			}
		}
		workspaces = listed
	}
	if err := spec.write(); err != nil {
		if refused := spec.unknown(err); refused != nil {
			return nil, nil, refused
		}
		log.Error(spec.op, "could not record the "+spec.what+"'s fold", dlog.Context{"cause": err.Error()})
		return nil, nil, fmt.Errorf("fold %s %q: %w", spec.what, spec.target, err)
	}
	return workspaces, members, nil
}

// landOffFold moves the selection off CURRENT, which the collapse SPEC is
// about to fold away (its MEMBERS), to the workspace the user was on before
// among the tabs the collapse leaves drawn (sidebar.LandingTarget, the
// question the roster asks to decide whether a collapse may be asked). It runs
// under the selection lock, which recordFold holds. With nowhere to land the
// collapse is refused holds_current and the selection stands.
func (v *verbs) landOffFold(ctx context.Context, log dlog.Logger, spec foldTabsSpec, members wsm.FoldMembers, current ids.WorkspaceID) error {
	reg, err := v.readRegistry(ctx, log, spec.op)
	if err != nil {
		return fmt.Errorf("fold %s %q: the registry could not be read to land off %q: %w", spec.what, spec.target, current, err)
	}
	target, by, found := sidebar.LandingTarget(sidebar.Tabs(reg, log), current, members)
	if !found {
		return refuse(log, "UpdateSidebarView", ArmHoldsCurrent,
			fmt.Sprintf("the %s holds the current workspace %q and no other open workspace is drawn to land on; collapsing it would stand that workspace down", spec.what, current),
			false)
	}
	_, targetLog, err := v.owned(ctx, "SelectWorkspace", target.ID)
	if err != nil {
		return err
	}
	applied, err := v.selectLocked(ctx, targetLog, target.ID, &current, Switch{History: RecordSelection})
	if err != nil {
		return fmt.Errorf("fold %s %q: land off %q: %w", spec.what, spec.target, current, err)
	}
	if !applied {
		log.Error(spec.op, "the selection moved under the selection lock while a collapse landed off it", dlog.Context{"workspace": string(current)})
		return fmt.Errorf("fold %s %q: land off %q: the selection moved under the selection lock", spec.what, spec.target, current)
	}
	targetLog.Info(spec.op, "the collapse moved the selection off the folded "+spec.what+" to the workspace selected before",
		dlog.Context{"from": string(current), "place": target.Place, "by": string(by)})
	return nil
}
