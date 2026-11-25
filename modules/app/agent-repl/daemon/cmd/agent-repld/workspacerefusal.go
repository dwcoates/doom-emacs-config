package main

import (
	"errors"
	"fmt"
	"strings"

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
// placementSpelled is P as `-placement` spells it.
func placementSpelled(p *agentreplv1.MergeQueuePlacement) string {
	switch {
	case p.GetFront() != nil:
		return "front"
	case p.GetBack() != nil:
		return "back"
	}
	return "behind=" + p.GetBehind().GetMerge()
}

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
	case *agentreplv1.FinishWorkspaceError_RequesterMayNotLand:
		return fmt.Sprintf("the requester is neither %s, nor its owner, nor the user, so it may not finish it", ws), nil
	case *agentreplv1.FinishWorkspaceError_NoBranchCheckedOut:
		return fmt.Sprintf("%s's worktree %s has no branch checked out, so there is no branch to deploy", ws, r.NoBranchCheckedOut.GetDir()), nil
	case *agentreplv1.FinishWorkspaceError_NoDeployConfig:
		return fmt.Sprintf("%s's repository has no %s, so nothing says what finishing does there", ws, r.NoDeployConfig.GetPath()), nil
	case *agentreplv1.FinishWorkspaceError_DeployConfigInvalid:
		return fmt.Sprintf("%s's repository's %s cannot be read (%s)", ws, r.DeployConfigInvalid.GetPath(), r.DeployConfigInvalid.GetDetail()), nil
	case *agentreplv1.FinishWorkspaceError_PlacementBehindNotInLine:
		return sayBehindNotInLine(ws, r.PlacementBehindNotInLine), nil
	case *agentreplv1.FinishWorkspaceError_MergeOffQueue:
		return fmt.Sprintf("%s's merge left its queue without landing and its request stands; put it back with `agent-repld merge-queue requeue %s`, or end it with `agent-repld merge-queue discard %s` before finishing again", ws, ws, ws), nil
	case *agentreplv1.FinishWorkspaceError_PlacementConflictsWithStored:
		return fmt.Sprintf("%s's create fixed its finish at the %s of the merge queue, and this finish asked for %s; finish again with no -placement",
			ws, placementSpelled(r.PlacementConflictsWithStored.GetStored()), placementSpelled(r.PlacementConflictsWithStored.GetRequested())), nil
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

// describeBackloggedRefusal says in one plain sentence why WS was not hidden or
// shown. Every SetWorkspaceBackloggedError arm has its own sentence.
func describeBackloggedRefusal(ws string, e *agentreplv1.SetWorkspaceBackloggedError) (string, error) {
	switch r := e.GetCause().(type) {
	case *agentreplv1.SetWorkspaceBackloggedError_UnknownWorkspace:
		return sayNoWorkspace(ws), nil
	case *agentreplv1.SetWorkspaceBackloggedError_WorkspaceRefMismatch:
		return sayLivesElsewhere(ws, r.WorkspaceRefMismatch.GetRegistryDir(), "the request"), nil
	case *agentreplv1.SetWorkspaceBackloggedError_TransferringAway:
		return sayHandedOver(ws, r.TransferringAway.GetAddress(), "set it again"), nil
	case *agentreplv1.SetWorkspaceBackloggedError_NotYetAdopted:
		return sayNotYetAdopted(ws, "set it again"), nil
	case *agentreplv1.SetWorkspaceBackloggedError_HasOpenChildren:
		return sayOpenChildren(ws, r.HasOpenChildren, "hide it again"), nil
	case *agentreplv1.SetWorkspaceBackloggedError_ParentNotOpen:
		return sayParentNotOpen(r.ParentNotOpen, "show "+ws+" again")
	}
	return "", undescribed("hiding or showing "+ws, e)
}

// describePriorityRefusal says in one plain sentence why WS's priority was not
// set. Every SetWorkspacePriorityError arm has its own sentence.
func describePriorityRefusal(ws string, e *agentreplv1.SetWorkspacePriorityError) (string, error) {
	switch r := e.GetCause().(type) {
	case *agentreplv1.SetWorkspacePriorityError_UnknownWorkspace:
		return sayNoWorkspace(ws), nil
	case *agentreplv1.SetWorkspacePriorityError_WorkspaceRefMismatch:
		return sayLivesElsewhere(ws, r.WorkspaceRefMismatch.GetRegistryDir(), "the request"), nil
	case *agentreplv1.SetWorkspacePriorityError_TransferringAway:
		return sayHandedOver(ws, r.TransferringAway.GetAddress(), "set it again"), nil
	case *agentreplv1.SetWorkspacePriorityError_NotYetAdopted:
		return sayNotYetAdopted(ws, "set it again"), nil
	}
	return "", undescribed("the priority of "+ws, e)
}

// describeNameRefusal says in one plain sentence why WS was not renamed.
// Every SetWorkspaceNameError arm has its own sentence.
func describeNameRefusal(ws string, e *agentreplv1.SetWorkspaceNameError) (string, error) {
	switch r := e.GetCause().(type) {
	case *agentreplv1.SetWorkspaceNameError_UnknownWorkspace:
		return sayNoWorkspace(ws), nil
	case *agentreplv1.SetWorkspaceNameError_WorkspaceRefMismatch:
		return sayLivesElsewhere(ws, r.WorkspaceRefMismatch.GetRegistryDir(), "the request"), nil
	case *agentreplv1.SetWorkspaceNameError_TransferringAway:
		return sayHandedOver(ws, r.TransferringAway.GetAddress(), "rename it again"), nil
	case *agentreplv1.SetWorkspaceNameError_NotYetAdopted:
		return sayNotYetAdopted(ws, "rename it again"), nil
	case *agentreplv1.SetWorkspaceNameError_InvalidName:
		return sayInvalidName(r.InvalidName)
	case *agentreplv1.SetWorkspaceNameError_NameTaken:
		t := r.NameTaken
		switch h := t.GetHolder().(type) {
		case *agentreplv1.SetWorkspaceNameTaken_Workspace:
			return fmt.Sprintf("the name %s is already the name or branch of the workspace at %s", t.GetName(), h.Workspace.GetDir()), nil
		case *agentreplv1.SetWorkspaceNameTaken_Pending:
			return fmt.Sprintf("the name %s is reserved by a pending async merge or claimed by a create in flight", t.GetName()), nil
		}
	}
	return "", undescribed("renaming "+ws, e)
}

// describeRemoteControlRefusal says in one plain sentence why WS's remote
// control mode was not set. Every UpdateRemoteControlModeError arm has its
// own sentence.
func describeRemoteControlRefusal(ws string, e *agentreplv1.UpdateRemoteControlModeError) (string, error) {
	switch r := e.GetCause().(type) {
	case *agentreplv1.UpdateRemoteControlModeError_UnknownWorkspace:
		return sayNoWorkspace(ws), nil
	case *agentreplv1.UpdateRemoteControlModeError_WorkspaceRefMismatch:
		return sayLivesElsewhere(ws, r.WorkspaceRefMismatch.GetRegistryDir(), "the request"), nil
	case *agentreplv1.UpdateRemoteControlModeError_TransferringAway:
		return sayHandedOver(ws, r.TransferringAway.GetAddress(), "set it again"), nil
	case *agentreplv1.UpdateRemoteControlModeError_NotYetAdopted:
		return sayNotYetAdopted(ws, "set it again"), nil
	case *agentreplv1.UpdateRemoteControlModeError_VendorRefused:
		return fmt.Sprintf("the vendor refused %s's remote-control bridge (%s); the mode is still recorded as asked, and the refusal rides its warning strip", ws, r.VendorRefused.GetDetail()), nil
	}
	return "", undescribed("the remote control of "+ws, e)
}

// describeQueueBypassRefusal says in one plain sentence why WS's queue bypass
// mode was not set. Every UpdateQueueBypassModeError arm has its own
// sentence.
func describeQueueBypassRefusal(ws string, e *agentreplv1.UpdateQueueBypassModeError) (string, error) {
	switch r := e.GetCause().(type) {
	case *agentreplv1.UpdateQueueBypassModeError_UnknownWorkspace:
		return sayNoWorkspace(ws), nil
	case *agentreplv1.UpdateQueueBypassModeError_WorkspaceRefMismatch:
		return sayLivesElsewhere(ws, r.WorkspaceRefMismatch.GetRegistryDir(), "the request"), nil
	case *agentreplv1.UpdateQueueBypassModeError_TransferringAway:
		return sayHandedOver(ws, r.TransferringAway.GetAddress(), "set it again"), nil
	case *agentreplv1.UpdateQueueBypassModeError_WorkAccount:
		return fmt.Sprintf("%s's session spends as a work account, which never holds the queue bypass mode", ws), nil
	case *agentreplv1.UpdateQueueBypassModeError_HeldElsewhere:
		return fmt.Sprintf("the workspace at %s already holds its repository's queue bypass mode", r.HeldElsewhere.GetWorkspace().GetDir()), nil
	case *agentreplv1.UpdateQueueBypassModeError_TurnInFlight:
		return fmt.Sprintf("%s has a turn in flight; end or interrupt it, then set the mode again", ws), nil
	case *agentreplv1.UpdateQueueBypassModeError_SwitchFailed:
		return fmt.Sprintf("%s's conversation switch did not complete (%s)", ws, r.SwitchFailed.GetDetail()), nil
	case *agentreplv1.UpdateQueueBypassModeError_NoBypassConversation:
		return fmt.Sprintf("the context named %s's parked bypass conversation, and it parks none; nothing changed", ws), nil
	case *agentreplv1.UpdateQueueBypassModeError_CompactionFailed:
		return fmt.Sprintf("the compaction or summary the context asked for did not complete (%s); %s holds the mode with its bypass conversation bound, and its next start asks for the compaction again", r.CompactionFailed.GetDetail(), ws), nil
	}
	return "", undescribed("the queue bypass mode of "+ws, e)
}

// describeCorrespondenceRefusal says in one plain sentence why what WS awaits
// was not marked. Every SetWorkspaceCorrespondenceError arm has its own
// sentence.
func describeCorrespondenceRefusal(ws string, e *agentreplv1.SetWorkspaceCorrespondenceError) (string, error) {
	switch r := e.GetCause().(type) {
	case *agentreplv1.SetWorkspaceCorrespondenceError_UnknownWorkspace:
		return sayNoWorkspace(ws), nil
	case *agentreplv1.SetWorkspaceCorrespondenceError_WorkspaceRefMismatch:
		return sayLivesElsewhere(ws, r.WorkspaceRefMismatch.GetRegistryDir(), "the request"), nil
	case *agentreplv1.SetWorkspaceCorrespondenceError_Closed:
		return fmt.Sprintf("%s is closed, so it runs no agent to wait", ws), nil
	case *agentreplv1.SetWorkspaceCorrespondenceError_UnknownAwaited:
		return fmt.Sprintf("the awaited workspace %s is not registered, or is %s itself", r.UnknownAwaited.GetAwaited().GetDir(), ws), nil
	case *agentreplv1.SetWorkspaceCorrespondenceError_RequesterMayNotSet:
		return fmt.Sprintf("the requester is neither %s nor the user, so it may not mark what %s awaits", ws, ws), nil
	}
	return "", undescribed("what "+ws+" awaits", e)
}

// describeSummaryRefusal says in one plain sentence why WS's summary was not
// set. Every SetWorkspaceSummaryError arm has its own sentence.
func describeSummaryRefusal(ws string, e *agentreplv1.SetWorkspaceSummaryError) (string, error) {
	switch r := e.GetCause().(type) {
	case *agentreplv1.SetWorkspaceSummaryError_UnknownWorkspace:
		return sayNoWorkspace(ws), nil
	case *agentreplv1.SetWorkspaceSummaryError_WorkspaceRefMismatch:
		return sayLivesElsewhere(ws, r.WorkspaceRefMismatch.GetRegistryDir(), "the request"), nil
	case *agentreplv1.SetWorkspaceSummaryError_TransferringAway:
		return sayHandedOver(ws, r.TransferringAway.GetAddress(), "set the summary again"), nil
	case *agentreplv1.SetWorkspaceSummaryError_NotYetAdopted:
		return sayNotYetAdopted(ws, "set the summary again"), nil
	case *agentreplv1.SetWorkspaceSummaryError_InvalidSummary:
		return fmt.Sprintf("the summary %q is not one plain sentence of two to fifteen words (%s)", r.InvalidSummary.GetSummary(), r.InvalidSummary.GetReason()), nil
	}
	return "", undescribed("the summary of "+ws, e)
}

// describeLinksRefusal says in one plain sentence why WS's links were not
// changed. Every ChangeWorkspaceLinksError arm has its own sentence.
func describeLinksRefusal(ws string, e *agentreplv1.ChangeWorkspaceLinksError) (string, error) {
	switch r := e.GetCause().(type) {
	case *agentreplv1.ChangeWorkspaceLinksError_UnknownWorkspace:
		return sayNoWorkspace(ws), nil
	case *agentreplv1.ChangeWorkspaceLinksError_WorkspaceRefMismatch:
		return sayLivesElsewhere(ws, r.WorkspaceRefMismatch.GetRegistryDir(), "the request"), nil
	case *agentreplv1.ChangeWorkspaceLinksError_TransferringAway:
		return sayHandedOver(ws, r.TransferringAway.GetAddress(), "change the links again"), nil
	case *agentreplv1.ChangeWorkspaceLinksError_NotYetAdopted:
		return sayNotYetAdopted(ws, "change the links again"), nil
	case *agentreplv1.ChangeWorkspaceLinksError_InvalidLink:
		return fmt.Sprintf("%s is not a link the browser opens (%s)", r.InvalidLink.GetUrl(), r.InvalidLink.GetReason()), nil
	case *agentreplv1.ChangeWorkspaceLinksError_InvalidLabel:
		return fmt.Sprintf("the label %q is not two to eight words in one sentence (%s)", r.InvalidLabel.GetLabel(), r.InvalidLabel.GetReason()), nil
	case *agentreplv1.ChangeWorkspaceLinksError_UnknownLink:
		return fmt.Sprintf("%s holds no link %s", ws, r.UnknownLink.GetUrl()), nil
	case *agentreplv1.ChangeWorkspaceLinksError_TooManyShown:
		return fmt.Sprintf("the menu shows at most %d links, and %d were named", r.TooManyShown.GetLimit(), r.TooManyShown.GetNamed()), nil
	case *agentreplv1.ChangeWorkspaceLinksError_UnknownAnnouncedLink:
		return fmt.Sprintf("the link announces %s, which %s does not hold", r.UnknownAnnouncedLink.GetUrl(), ws), nil
	case *agentreplv1.ChangeWorkspaceLinksError_PullRequestOwnedElsewhere:
		o := r.PullRequestOwnedElsewhere
		return fmt.Sprintf("the pull request %s is already owned by %s at %s, and a pull request has one owning workspace", o.GetUrl(), o.GetOwnerName(), o.GetOwner().GetDir()), nil
	case *agentreplv1.ChangeWorkspaceLinksError_SubpageOfOwnedLink:
		o := r.SubpageOfOwnedLink
		return fmt.Sprintf("%s is a subpage of the GNS page %s, which %s already owns: store it as a reference, or leave it out", o.GetUrl(), o.GetOwnedPageUrl(), ws), nil
	case *agentreplv1.ChangeWorkspaceLinksError_OwnedLinkUnshown:
		o := r.OwnedLinkUnshown
		return fmt.Sprintf("the show leaves the owned link %s out while it names the reference %s: show every owned link first", o.GetOwnedUrl(), o.GetReferenceUrl()), nil
	}
	return "", undescribed("the links of "+ws, e)
}

// describeFindRefusal says in one plain sentence why no workspaces were
// found. Every FindWorkspacesError arm has its own sentence.
func describeFindRefusal(e *agentreplv1.FindWorkspacesError) (string, error) {
	switch r := e.GetCause().(type) {
	case *agentreplv1.FindWorkspacesError_UnknownRepository:
		return "the daemon has no repository by the one -repository named", nil
	case *agentreplv1.FindWorkspacesError_UnknownCreator:
		return "the daemon has no workspace by the one -created-by named", nil
	case *agentreplv1.FindWorkspacesError_UnknownCaller:
		return "the daemon has no workspace for the calling directory", nil
	case *agentreplv1.FindWorkspacesError_UnknownLoginWorkspace:
		return "the daemon has no workspace by the one -login named", nil
	case *agentreplv1.FindWorkspacesError_UnknownLoginRoot:
		return fmt.Sprintf("the -login-root root is not an account root the daemon knows; it knows %s", strings.Join(r.UnknownLoginRoot.GetKnown(), ", ")), nil
	}
	return "", undescribed("finding workspaces", e)
}

// describeArrangeRefusal says in one plain sentence why the sidebar was not
// rearranged. Every ArrangeSidebarError arm has its own sentence.
func describeArrangeRefusal(e *agentreplv1.ArrangeSidebarError) (string, error) {
	const named = "a workspace the change named"
	switch r := e.GetCause().(type) {
	case *agentreplv1.ArrangeSidebarError_UnknownRepository:
		return "the daemon has no repository by the one the change named", nil
	case *agentreplv1.ArrangeSidebarError_UnknownSubsection:
		return fmt.Sprintf("the repository has no subsection %s", r.UnknownSubsection.GetSubsection().GetId()), nil
	case *agentreplv1.ArrangeSidebarError_UnknownWorkspace:
		return sayNoWorkspace(r.UnknownWorkspace.GetWorkspace().GetDir()), nil
	case *agentreplv1.ArrangeSidebarError_WorkspaceRefMismatch:
		return sayLivesElsewhere(r.WorkspaceRefMismatch.GetWorkspace().GetId(), r.WorkspaceRefMismatch.GetRegistryDir(), "the change"), nil
	case *agentreplv1.ArrangeSidebarError_TransferringAway:
		return sayHandedOver(named, r.TransferringAway.GetAddress(), "place it again"), nil
	case *agentreplv1.ArrangeSidebarError_NotYetAdopted:
		return sayNotYetAdopted(named, "place it again"), nil
	case *agentreplv1.ArrangeSidebarError_BlankName:
		return "a subsection's name is blank", nil
	case *agentreplv1.ArrangeSidebarError_OtherRepository:
		return "the change relates workspaces or subsections of different repositories", nil
	case *agentreplv1.ArrangeSidebarError_AnchorElsewhere:
		return "what the change placed things before moved since the sidebar was read; read it again and place them again", nil
	case *agentreplv1.ArrangeSidebarError_NoOpenWorkspace:
		return "the new subsection would hold no open workspace, so the change that created it would delete it", nil
	case *agentreplv1.ArrangeSidebarError_MainWorkspaceNotSortable:
		return "the change would move a repository's main workspace, or the family it roots, which agents never sort", nil
	case *agentreplv1.ArrangeSidebarError_NoRepositoryBlock:
		return "the change named a repository block the parent does not have", nil
	case *agentreplv1.ArrangeSidebarError_NotAChild:
		return "the workspace is top-level, so it has no parent whose children it could be reordered among", nil
	case *agentreplv1.ArrangeSidebarError_NotSiblings:
		return "the workspace to move before is not a child of the moved child's parent", nil
	}
	return "", undescribed("arranging the sidebar", e)
}
