package main

import (
	"errors"
	"fmt"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
)

// THE LANDING-TARGET RESTATEMENT is the one way to give a workspace merge
// geometry it lacks: its creation job was never keyed to it (a create cut
// short), or it was registered by the editor rather than created. Geometry is
// never inferred, so the refusals that meet its absence all name the
// statement that records it.
const restateLanding = "record it by restating where the workspace lands: `agent-repld workspace set %s -lands-into main` (or `-lands-into WS`)"

// errUndescribedWorkspaceRefusal is a finish, one-shot or retarget refusal
// whose arm this command has no sentence for: a new arm landed without one, a
// bug the unit tests catch.
var errUndescribedWorkspaceRefusal = errors.New("the daemon refused with an arm this command has no sentence for")

// describeFinishRefusal says in one plain sentence why WS, as the caller named
// it, was not finished. Every FinishWorkspaceError arm has its own sentence.
func describeFinishRefusal(ws string, e *agentreplv1.FinishWorkspaceError) (string, error) {
	switch r := e.GetCause().(type) {
	case *agentreplv1.FinishWorkspaceError_UnknownWorkspace:
		return sayNoWorkspace(ws), nil
	case *agentreplv1.FinishWorkspaceError_WorkspaceRefMismatch:
		return sayLivesElsewhere(ws, r.WorkspaceRefMismatch.GetRegistryDir(), "the request"), nil
	case *agentreplv1.FinishWorkspaceError_TransferringAway:
		return sayHandedOver(ws, r.TransferringAway.GetAddress(), "finish it again"), nil
	case *agentreplv1.FinishWorkspaceError_NotYetAdopted:
		return sayNotYetAdopted(ws, "finish it again"), nil
	case *agentreplv1.FinishWorkspaceError_NoLayoutFacts:
		return fmt.Sprintf("%s records no merge geometry, so nothing says where it lands; "+restateLanding+", then finish it again", ws, ws), nil
	case *agentreplv1.FinishWorkspaceError_SessionDeleted:
		return fmt.Sprintf("%s's session has been deleted", ws), nil
	case *agentreplv1.FinishWorkspaceError_AlreadyQueued:
		return fmt.Sprintf("%s's deploy is already waiting in the queue", ws), nil
	case *agentreplv1.FinishWorkspaceError_AlreadyDeploying:
		return fmt.Sprintf("%s's deploy is already running", ws), nil
	case *agentreplv1.FinishWorkspaceError_VersionIncrementRequired:
		return fmt.Sprintf("%s lands on a versioned main branch; finish it again with -version-part minor or patch and -changelog-line TEXT", ws), nil
	case *agentreplv1.FinishWorkspaceError_VersionIncrementUnexpected:
		return fmt.Sprintf("%s does not land on a versioned main branch; finish it again without -version-part or -changelog-line", ws), nil
	case *agentreplv1.FinishWorkspaceError_HasOpenChildren:
		return fmt.Sprintf("%s has %d open children; resolve and close each before finishing it", ws, len(r.HasOpenChildren.Children)), nil
	case *agentreplv1.FinishWorkspaceError_OwnerLands:
		return fmt.Sprintf("%s is landed by its owner %s, not by itself", ws, r.OwnerLands.GetOwner().GetDir()), nil
	case *agentreplv1.FinishWorkspaceError_RequesterMayNotLand:
		return fmt.Sprintf("the requester is neither %s, nor its owner, nor the user, so it may not finish it", ws), nil
	case *agentreplv1.FinishWorkspaceError_NoBranchCheckedOut:
		return fmt.Sprintf("%s's worktree %s has no branch checked out, so there is no branch to deploy", ws, r.NoBranchCheckedOut.GetDir()), nil
	case *agentreplv1.FinishWorkspaceError_NoDeployConfig:
		return fmt.Sprintf("%s's repository has no %s, so nothing says what finishing does there", ws, r.NoDeployConfig.GetPath()), nil
	case *agentreplv1.FinishWorkspaceError_DeployConfigInvalid:
		return fmt.Sprintf("%s's repository's %s cannot be read (%s)", ws, r.DeployConfigInvalid.GetPath(), r.DeployConfigInvalid.GetDetail()), nil
	case *agentreplv1.FinishWorkspaceError_PlacementBehindNotInLine:
		return fmt.Sprintf("the merge %s that %s was to go behind is not in its queue", r.PlacementBehindNotInLine.GetBehind(), ws), nil
	}
	return "", fmt.Errorf("%w: finishing %s: %v", errUndescribedWorkspaceRefusal, ws, e)
}

// describeOneShotRefusal says in one plain sentence why WS's one-shot standing
// was not changed. Every SetWorkspaceOneShotError arm has its own sentence.
func describeOneShotRefusal(ws string, e *agentreplv1.SetWorkspaceOneShotError) (string, error) {
	switch r := e.GetCause().(type) {
	case *agentreplv1.SetWorkspaceOneShotError_UnknownWorkspace:
		return sayNoWorkspace(ws), nil
	case *agentreplv1.SetWorkspaceOneShotError_WorkspaceRefMismatch:
		return sayLivesElsewhere(ws, r.WorkspaceRefMismatch.GetRegistryDir(), "the request"), nil
	case *agentreplv1.SetWorkspaceOneShotError_Closed:
		return fmt.Sprintf("%s is closed", ws), nil
	case *agentreplv1.SetWorkspaceOneShotError_NoCreationJob:
		return fmt.Sprintf("%s records no creation job to mark; "+restateLanding+", then mark it again", ws, ws), nil
	case *agentreplv1.SetWorkspaceOneShotError_RequesterMayNotSet:
		return fmt.Sprintf("the requester is neither %s, nor its owner, nor the user, so it may not change its one-shot standing", ws), nil
	case *agentreplv1.SetWorkspaceOneShotError_MainWorkspace:
		return fmt.Sprintf("%s is its repository's main workspace, which lands nothing itself and is never a one-shot", ws), nil
	}
	return "", fmt.Errorf("%w: the one-shot standing of %s: %v", errUndescribedWorkspaceRefusal, ws, e)
}

// describeRetargetRefusal says in one plain sentence why WS was not
// retargeted. Every SetWorkspaceParentError arm has its own sentence.
func describeRetargetRefusal(ws string, e *agentreplv1.SetWorkspaceParentError) (string, error) {
	switch r := e.GetCause().(type) {
	case *agentreplv1.SetWorkspaceParentError_UnknownWorkspace:
		return sayNoWorkspace(ws), nil
	case *agentreplv1.SetWorkspaceParentError_WorkspaceRefMismatch:
		return sayLivesElsewhere(ws, r.WorkspaceRefMismatch.GetRegistryDir(), "the request"), nil
	case *agentreplv1.SetWorkspaceParentError_TransferringAway:
		return sayHandedOver(ws, r.TransferringAway.GetAddress(), "retarget it again"), nil
	case *agentreplv1.SetWorkspaceParentError_NotYetAdopted:
		return sayNotYetAdopted(ws, "retarget it again"), nil
	case *agentreplv1.SetWorkspaceParentError_UnknownParent:
		return "the daemon has no workspace by the new parent's name", nil
	case *agentreplv1.SetWorkspaceParentError_ParentRefMismatch:
		return fmt.Sprintf("the new parent lives at %s, not at the directory the request named", r.ParentRefMismatch.GetRegistryDir()), nil
	case *agentreplv1.SetWorkspaceParentError_Cycle:
		return fmt.Sprintf("the new parent is %s itself or one of its descendants", ws), nil
	case *agentreplv1.SetWorkspaceParentError_NoMergeLayout:
		return fmt.Sprintf("%s records no merge geometry to keep; restate where it lands with -lands-into main or -lands-into WS (a repository's main workspace never has any)", ws), nil
	case *agentreplv1.SetWorkspaceParentError_ColdGate:
		return fmt.Sprintf("%s is parked at its cold gate; the user must answer the gate before it can be retargeted", ws), nil
	case *agentreplv1.SetWorkspaceParentError_ParentNotOpen:
		return fmt.Sprintf("the new parent or landing target %s is not open", r.ParentNotOpen.GetParent().GetDir()), nil
	case *agentreplv1.SetWorkspaceParentError_UnknownLandingTarget:
		return fmt.Sprintf("%s cannot land into %s: it is unknown, of another repository, or %s itself or a workspace landing into it", ws, r.UnknownLandingTarget.GetWorkspace().GetDir(), ws), nil
	}
	return "", fmt.Errorf("%w: retargeting %s: %v", errUndescribedWorkspaceRefusal, ws, e)
}
