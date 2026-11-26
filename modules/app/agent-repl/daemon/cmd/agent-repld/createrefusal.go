package main

import (
	"fmt"
	"strings"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
)

// describeCreateRefusal says in one plain sentence why no workspace was
// created. Every CreateWorkspaceError arm has its own sentence.
func describeCreateRefusal(e *agentreplv1.CreateWorkspaceError) (string, error) {
	switch r := e.GetCause().(type) {
	case *agentreplv1.CreateWorkspaceError_UngatedWithoutConsent:
		return "an ungated permission mode was asked for without the explicit consent", nil
	case *agentreplv1.CreateWorkspaceError_NoSlug:
		return "no slug could be derived for the workspace's branch and directory", nil
	case *agentreplv1.CreateWorkspaceError_BriefMissing:
		return fmt.Sprintf("the brief %s is absent", r.BriefMissing.GetName()), nil
	case *agentreplv1.CreateWorkspaceError_UnknownRepository:
		return "the daemon has no repository by the one the create named", nil
	case *agentreplv1.CreateWorkspaceError_WorktreeCreationFailed:
		return fmt.Sprintf("creating the worktree failed (%s)", r.WorktreeCreationFailed.GetDetail()), nil
	case *agentreplv1.CreateWorkspaceError_SpawnFailed:
		return fmt.Sprintf("spawning the new workspace's shim failed (%s)", r.SpawnFailed.GetDetail()), nil
	case *agentreplv1.CreateWorkspaceError_OneShotPolicyMissing:
		p := r.OneShotPolicyMissing
		return fmt.Sprintf("a one-shot was asked for in %s, which states no one-shot policy of its own (%s lacks %s)",
			p.GetRepositoryRoot(), p.GetPolicyDir(), strings.Join(p.GetMissingFiles(), ", ")), nil
	case *agentreplv1.CreateWorkspaceError_NamingFailed:
		n := r.NamingFailed
		return fmt.Sprintf("the daemon could not name the workspace (%s after %d attempts with %s); name it with -name", n.GetCause(), n.GetAttempts(), n.GetModel()), nil
	case *agentreplv1.CreateWorkspaceError_InsideTemporaryDirectory:
		return fmt.Sprintf("%s lies inside the temporary directory %s", r.InsideTemporaryDirectory.GetDir(), r.InsideTemporaryDirectory.GetTemporaryRoot()), nil
	case *agentreplv1.CreateWorkspaceError_UnknownForkSource:
		return fmt.Sprintf("the daemon has no workspace %s to fork", r.UnknownForkSource.GetWorkspace().GetDir()), nil
	case *agentreplv1.CreateWorkspaceError_ForkSourceHasNoConversation:
		return fmt.Sprintf("%s has no conversation to fork", r.ForkSourceHasNoConversation.GetWorkspace().GetDir()), nil
	case *agentreplv1.CreateWorkspaceError_ForkSourceHasNoFinishedTurn:
		return fmt.Sprintf("%s is still in its first turn, so a fork would carry nothing; create the workspace with a fresh context, carrying what it should know in the prompt, or fork once that turn ends",
			r.ForkSourceHasNoFinishedTurn.GetWorkspace().GetDir()), nil
	case *agentreplv1.CreateWorkspaceError_NameHeld:
		h := r.NameHeld
		standing := "closed"
		if h.GetOpen() {
			standing = "open"
		}
		return fmt.Sprintf("the name %s is already the name or branch of the %s workspace at %s, which has not landed", h.GetBranch(), standing, h.GetWorkspace().GetDir()), nil
	case *agentreplv1.CreateWorkspaceError_NameReserved:
		return fmt.Sprintf("the name %s is claimed by a create in flight; ask again once that create settles", r.NameReserved.GetBranch()), nil
	case *agentreplv1.CreateWorkspaceError_NameBranchExists:
		return fmt.Sprintf("the branch %s already exists (at %s), and no workspace the daemon serves holds it; choose another name", r.NameBranchExists.GetBranch(), r.NameBranchExists.GetHead()), nil
	case *agentreplv1.CreateWorkspaceError_InvalidName:
		return sayInvalidName(r.InvalidName)
	case *agentreplv1.CreateWorkspaceError_UnknownContextWorkspace:
		return fmt.Sprintf("the daemon has no workspace %s for the one-shot's context", r.UnknownContextWorkspace.GetWorkspace().GetDir()), nil
	case *agentreplv1.CreateWorkspaceError_ParentNotOpen:
		return sayParentNotOpen(r.ParentNotOpen, "create it again")
	case *agentreplv1.CreateWorkspaceError_UnknownOwner:
		return fmt.Sprintf("the daemon has no workspace %s to be the parent", r.UnknownOwner.GetOwner().GetDir()), nil
	case *agentreplv1.CreateWorkspaceError_UnknownLandingTarget:
		return fmt.Sprintf("the workspace to land into, %s, is unknown or of another repository, and no branch crosses repositories", r.UnknownLandingTarget.GetWorkspace().GetDir()), nil
	case *agentreplv1.CreateWorkspaceError_UnknownRequester:
		return fmt.Sprintf("the daemon has no requesting workspace %s", r.UnknownRequester.GetWorkspace().GetDir()), nil
	case *agentreplv1.CreateWorkspaceError_UnknownSourceBranch:
		return fmt.Sprintf("the source branch %s does not exist in the repository", r.UnknownSourceBranch.GetBranch()), nil
	case *agentreplv1.CreateWorkspaceError_UnknownSubagent:
		return fmt.Sprintf("no workspace's session of the repository holds the subagent %s", r.UnknownSubagent.GetAgentId()), nil
	case *agentreplv1.CreateWorkspaceError_UnknownCloseAfter:
		return fmt.Sprintf("the workspace to close once the one-shot lands, %s, is not open, is in another repository, or is the requester itself", r.UnknownCloseAfter.GetWorkspace().GetDir()), nil
	case *agentreplv1.CreateWorkspaceError_OriginFetchFailed:
		return fmt.Sprintf("fetching origin's primary branch failed (%s)", r.OriginFetchFailed.GetDetail()), nil
	case *agentreplv1.CreateWorkspaceError_TransferringAway:
		return fmt.Sprintf("the daemon is handing its workspaces to its successor at %s and admits no new create; create it again there", r.TransferringAway.GetAddress()), nil
	case *agentreplv1.CreateWorkspaceError_StandingDown:
		return "the daemon is standing down and admits no new create; create it again once the next daemon serves", nil
	case *agentreplv1.CreateWorkspaceError_ModelNotInCatalog:
		m := r.ModelNotInCatalog
		options := make([]string, 0, len(m.GetOptions()))
		for _, o := range m.GetOptions() {
			options = append(options, o.GetModel().GetName())
		}
		return fmt.Sprintf("the model %s is not in the catalog of the login at %s, which offers %s", m.GetModel(), m.GetConfigDir(), strings.Join(options, ", ")), nil
	case *agentreplv1.CreateWorkspaceError_ModelCatalogUnknown:
		m := r.ModelCatalogUnknown
		return fmt.Sprintf("no session on the login at %s has served a model catalog yet, so the model %s is unknown; create it without -model to run under the daemon's default", m.GetConfigDir(), m.GetModel()), nil
	case *agentreplv1.CreateWorkspaceError_LifecycleConfigInvalid:
		return fmt.Sprintf("the repository's %s is unusable (%s), so its creation stage cannot be read", r.LifecycleConfigInvalid.GetPath(), r.LifecycleConfigInvalid.GetDetail()), nil
	case *agentreplv1.CreateWorkspaceError_CreationHookFailed:
		h := r.CreationHookFailed
		return fmt.Sprintf("the creation hook %s failed (%s; its log is %s); the create was undone, and the repository's main workspace was asked to find out why", h.GetPath(), h.GetDetail(), h.GetLogPath()), nil
	case *agentreplv1.CreateWorkspaceError_PlacementOnChild:
		return fmt.Sprintf("the placement names a child, which sits wherever its parent does (%s)", r.PlacementOnChild.GetDetail()), nil
	case *agentreplv1.CreateWorkspaceError_PlacementRequiresWorkspace:
		return fmt.Sprintf("the user asked, not a workspace, and only a workspace places what it creates (%s)", r.PlacementRequiresWorkspace.GetDetail()), nil
	case *agentreplv1.CreateWorkspaceError_UnknownPlacementSubsection:
		return fmt.Sprintf("the repository has no subsection %s", r.UnknownPlacementSubsection.GetName()), nil
	case *agentreplv1.CreateWorkspaceError_BlankPlacementSubsectionName:
		return "the new subsection's name is blank", nil
	case *agentreplv1.CreateWorkspaceError_GatherUnknownWorkspace:
		return fmt.Sprintf("the repository holds no workspace %s to gather", r.GatherUnknownWorkspace.GetName()), nil
	case *agentreplv1.CreateWorkspaceError_GatherNotGatherable:
		return fmt.Sprintf("the workspace at %s cannot be gathered (%s)", r.GatherNotGatherable.GetWorkspace().GetDir(), r.GatherNotGatherable.GetDetail()), nil
	case *agentreplv1.CreateWorkspaceError_RequestedByDeployMediator:
		return fmt.Sprintf("the requester at %s is its repository's deploy mediator, which never creates a workspace: %s",
			r.RequestedByDeployMediator.GetWorkspace().GetDir(), r.RequestedByDeployMediator.GetDetail()), nil
	case *agentreplv1.CreateWorkspaceError_RequestedByProductionMediator:
		return fmt.Sprintf("the requester at %s is its repository's production mediator, which creates its children only by dispatching them: %s",
			r.RequestedByProductionMediator.GetWorkspace().GetDir(), r.RequestedByProductionMediator.GetDetail()), nil
	}
	return "", undescribed("creating a workspace", e)
}
