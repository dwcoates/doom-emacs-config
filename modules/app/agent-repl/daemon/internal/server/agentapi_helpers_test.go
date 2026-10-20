package server

import (
	agentreplv1 "agentrepl/proto/agentrepl/v1"
	workspacev1 "agentrepl/proto/workspace/v1"
)

// The agent API's required choices, spelled once for every test request.

func userRequester() *agentreplv1.WorkspaceRequester {
	return &agentreplv1.WorkspaceRequester{Asker: &agentreplv1.WorkspaceRequester_User{User: &agentreplv1.WorkspaceRequesterUser{}}}
}

func workspaceRequester(ws *workspacev1.WorkspaceRef) *agentreplv1.WorkspaceRequester {
	return &agentreplv1.WorkspaceRequester{Asker: &agentreplv1.WorkspaceRequester_Workspace{Workspace: &agentreplv1.WorkspaceRequesterWorkspace{Workspace: ws}}}
}

func freshContext() *agentreplv1.WorkspaceContextOrigin {
	return &agentreplv1.WorkspaceContextOrigin{Origin: &agentreplv1.WorkspaceContextOrigin_Fresh{Fresh: &agentreplv1.WorkspaceContextOriginFresh{}}}
}

func forkContext(ws *workspacev1.WorkspaceRef) *agentreplv1.WorkspaceContextOrigin {
	return &agentreplv1.WorkspaceContextOrigin{Origin: &agentreplv1.WorkspaceContextOrigin_Fork{Fork: &agentreplv1.WorkspaceContextOriginFork{Workspace: ws}}}
}

func primaryTopology() *agentreplv1.WorkspaceTopology {
	return &agentreplv1.WorkspaceTopology{Shape: &agentreplv1.WorkspaceTopology_Primary{Primary: &agentreplv1.WorkspaceTopologyPrimary{}}}
}

// childTopology is a child of OWNER, carrying NOTICE to it when non-empty.
func childTopology(owner *workspacev1.WorkspaceRef, notice string) *agentreplv1.WorkspaceTopology {
	child := &agentreplv1.WorkspaceTopologyChild{Owner: owner}
	if notice != "" {
		child.OwnerNotice = &agentreplv1.WorkspaceTopologyChildOwnerNotice{Text: notice}
	}
	return &agentreplv1.WorkspaceTopology{Shape: &agentreplv1.WorkspaceTopology_Child{Child: child}}
}

func backPlacement() *agentreplv1.MergeQueuePlacement {
	return &agentreplv1.MergeQueuePlacement{Place: &agentreplv1.MergeQueuePlacement_Back{Back: &agentreplv1.MergeQueuePlacementBack{}}}
}

func frontPlacement() *agentreplv1.MergeQueuePlacement {
	return &agentreplv1.MergeQueuePlacement{Place: &agentreplv1.MergeQueuePlacement_Front{Front: &agentreplv1.MergeQueuePlacementFront{}}}
}

// validCreate is a well-formed standard create in repo-1, changed by EDIT.
func validCreate(edit func(*agentreplv1.CreateWorkspaceRequest)) *agentreplv1.CreateWorkspaceRequest {
	req := &agentreplv1.CreateWorkspaceRequest{
		Repository:    &workspacev1.RepositoryRef{Id: "repo-1"},
		Form:          &agentreplv1.CreateWorkspaceRequest_Standard{Standard: &agentreplv1.CreateWorkspaceStandard{Topology: primaryTopology()}},
		Requester:     userRequester(),
		ContextOrigin: freshContext(),
	}
	if edit != nil {
		edit(req)
	}
	return req
}

// validAsyncMerge is a well-formed async merge asked by the harness
// workspace of its own hand-cut branch, onto the primary branch, changed by
// EDIT.
func validAsyncMerge(edit func(*agentreplv1.CreateWorkspaceAsyncMerge)) *agentreplv1.CreateWorkspaceRequest {
	async := &agentreplv1.CreateWorkspaceAsyncMerge{
		Name: "reconnect-fix",
		Source: &agentreplv1.CreateWorkspaceAsyncMergeSource{Branch: "agent-1/fix", Maker: &agentreplv1.CreateWorkspaceAsyncMergeMaker{
			Maker: &agentreplv1.CreateWorkspaceAsyncMergeMaker_Workspace{Workspace: &agentreplv1.CreateWorkspaceAsyncMergeMakerWorkspace{Workspace: ref()}}}},
		Target:    &agentreplv1.CreateWorkspaceAsyncMergeTarget{Target: &agentreplv1.CreateWorkspaceAsyncMergeTarget_Primary{Primary: &agentreplv1.CreateWorkspaceAsyncMergeTargetPrimary{}}},
		Placement: backPlacement(),
	}
	if edit != nil {
		edit(async)
	}
	return validCreate(func(req *agentreplv1.CreateWorkspaceRequest) {
		req.Form = &agentreplv1.CreateWorkspaceRequest_AsyncMerge{AsyncMerge: async}
		req.Requester = workspaceRequester(ref())
		req.ContextOrigin = forkContext(ref())
	})
}
