package workspace

import (
	"context"
	"errors"
	"fmt"

	"agent-repld/internal/dlog"
	"agent-repld/internal/wsm"
)

// The arm names a create's placement is refused under
// (agentrepl.v1.CreateWorkspaceError, tags 55 to 61).
const (
	// ArmPlacementOnChild is a placement on a workspace with a parent.
	ArmPlacementOnChild = "placement_on_child"
	// ArmPlacementOnMergeWorkspace is a placement beside an async merge.
	ArmPlacementOnMergeWorkspace = "placement_on_merge_workspace"
	// ArmPlacementRequiresWorkspace is a placement asked by the user.
	ArmPlacementRequiresWorkspace = "placement_requires_workspace"
	// ArmUnknownPlacementSubsection names a subsection the repository lacks.
	ArmUnknownPlacementSubsection = "unknown_placement_subsection"
	// ArmBlankPlacementSubsectionName is a new subsection with a blank name.
	ArmBlankPlacementSubsectionName = "blank_placement_subsection_name"
	// ArmGatherUnknownWorkspace names a gathered workspace the repository lacks.
	ArmGatherUnknownWorkspace = "gather_unknown_workspace"
	// ArmGatherNotGatherable names a gathered workspace that cannot be gathered.
	ArmGatherNotGatherable = "gather_not_gatherable"
)

// CreatePlacement is where a create files the workspace it makes
// (agentrepl.v1.CreateWorkspaceRequest.placement): an existing subsection by
// name, or a new one gathering other workspaces by name. Exactly one is set.
// The requester is the new subsection's creator.
type CreatePlacement struct {
	// Subsection is the name of an existing subsection of the repository.
	Subsection string
	// NewSubsection is a subsection created with the workspace in it.
	NewSubsection *wsm.NewSubsectionPlacement
}

// facts is the placement as the register transaction writes it, CREATOR being
// the workspace whose agent files the new one.
func (p CreatePlacement) facts(creator wsm.WorkspaceID) wsm.RegisterPlacement {
	return wsm.RegisterPlacement{Into: p.Subsection, New: p.NewSubsection, CreatedBy: creator}
}

// registerPlacement is the placement of the spec as RegisterFacts carries it, nil when
// the create places nothing. validatePlacement has already required the
// requester a placement needs.
func (s CreateSpec) registerPlacement() *wsm.RegisterPlacement {
	if s.Placement == nil {
		return nil
	}
	facts := s.Placement.facts(*s.Requester)
	return &facts
}

// validatePlacement refuses a placement the create's own shape cannot carry,
// before anything is read: the user has no agent to file for, a child sits
// wherever its owner does, and a merge workspace is drawn nowhere.
func validatePlacement(log dlog.Logger, spec CreateSpec) error {
	if spec.Placement == nil {
		return nil
	}
	switch {
	case spec.Requester == nil:
		return refuse(log, "CreateWorkspace", ArmPlacementRequiresWorkspace,
			"a placement files the workspace on behalf of a workspace's agent, and this create was asked for by the user", false)
	case spec.MergeOf != nil:
		return refuse(log, "CreateWorkspace", ArmPlacementOnMergeWorkspace,
			"a merge workspace is drawn nowhere until revealed, so it is never placed", false)
	case spec.Parent != nil:
		return refuse(log, "CreateWorkspace", ArmPlacementOnChild,
			fmt.Sprintf("the workspace is a child of %s and sits wherever its owner does", *spec.Parent), false)
	}
	if (spec.Placement.Subsection == "") == (spec.Placement.NewSubsection == nil) {
		panic(fmt.Sprintf("workspace: a create's placement names exactly one of an existing subsection and a new one: %+v", *spec.Placement))
	}
	return nil
}

// checkPlacement asks the registry whether the create's placement would be
// accepted, before a worktree is made or a name minted. The register
// transaction judges it again with the same code (wsm.checkPlacement), so a
// registry that moved in between refuses there with the same arm.
func (v *verbs) checkPlacement(ctx context.Context, log dlog.Logger, spec CreateSpec, repoDir string) error {
	if spec.Placement == nil {
		return nil
	}
	err := v.deps.DB.RefusePlacement(ctx, repoDir, *spec.registerPlacement(), spec.Parent, spec.MergeOf != nil)
	if err == nil {
		return nil
	}
	if refusal, ok := placementRefusal(log, err); ok {
		return refusal
	}
	log.Error(opCreate, "could not read whether the placement would be accepted", dlog.Context{"repo_dir": repoDir, "cause": err.Error()})
	return fmt.Errorf("create: check the placement in %q: %w", repoDir, err)
}

// placementRefusal answers the typed refusal for a placement error the
// registry raised, recording it at INFO as every refusal is.
func placementRefusal(log dlog.Logger, err error) (*Refusal, bool) {
	var unknown *wsm.UnknownPlacementSubsectionError
	var gatherUnknown *wsm.GatherUnknownError
	var gatherRefused *wsm.GatherRefusedError
	switch {
	case errors.Is(err, wsm.ErrPlacementOnChild):
		return refuse(log, "CreateWorkspace", ArmPlacementOnChild, err.Error(), false), true
	case errors.Is(err, wsm.ErrPlacementOnMerge):
		return refuse(log, "CreateWorkspace", ArmPlacementOnMergeWorkspace, err.Error(), false), true
	case errors.Is(err, wsm.ErrBlankSubsectionName):
		return refuse(log, "CreateWorkspace", ArmBlankPlacementSubsectionName, "the new subsection's name is blank", false), true
	case errors.As(err, &unknown):
		return refuseWith(log, "CreateWorkspace", ArmUnknownPlacementSubsection, err.Error(), false, map[string]any{"name": unknown.Name}), true
	case errors.As(err, &gatherUnknown):
		return refuseWith(log, "CreateWorkspace", ArmGatherUnknownWorkspace, err.Error(), false, map[string]any{"name": gatherUnknown.Name}), true
	case errors.As(err, &gatherRefused):
		return refuseNamingWorkspace(log, "CreateWorkspace", ArmGatherNotGatherable, err.Error(), false, gatherRefused.Workspace), true
	}
	return nil, false
}
