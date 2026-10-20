package server

import (
	"context"
	"errors"
	"fmt"
	"strings"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/merge"
	"agent-repld/internal/subagentmaker"
	"agent-repld/internal/workspace"
	"agent-repld/internal/wsm"
)

// SubagentMakers answers which workspace spawned a subagent
// (subagentmaker.Finder): the maker of an async merge of the subagent's
// branch.
type SubagentMakers interface {
	Find(ctx context.Context, repo wsm.RepoID, agentID string) (wsm.Workspace, error)
}

// asyncMergeRenames spells the merge queue's refusals of the requester's own
// row as the create's arms.
var asyncMergeRenames = map[string]string{
	merge.ArmAlreadyQueued:  "async_merge_requester_queued",
	merge.ArmAlreadyMerging: "async_merge_requester_merging",
	merge.ArmSessionDeleted: "async_merge_requester_session_deleted",
}

// createAsyncMerge records an async merge (agentrepl.v1.CreateWorkspaceAsyncMerge):
// the branch, its maker and its landing, in the requester's queue row, put
// in line when the requesting turn ends. Its merge workspace is created when
// the merge starts, so the answer names only the branch reserved for it.
func (s *server) createAsyncMerge(
	ctx context.Context,
	rpc string,
	resp *agentreplv1.CreateWorkspaceResponse,
	repository wsm.Repository,
	req *agentreplv1.CreateWorkspaceRequest,
) (*connect.Response[agentreplv1.CreateWorkspaceResponse], error) {
	async := req.GetAsyncMerge()
	if req.OpId != nil {
		return answer(resp, s.refuse(s.log, rpc, resp, s.fill(refusal{Arm: "op_id_with_async_merge",
			Reason: "an async merge's workspace is created by the queue when the merge starts, so no progress is keyed to the request"})))
	}
	// validateCreateWorkspaceRequest guaranteed a requesting workspace.
	requesterRef := req.GetRequester().GetWorkspace().GetWorkspace()
	requester, refused, done := s.resolveNaming(ctx, rpc, resp, requesterRef,
		map[string]string{workspace.ArmUnknownWorkspace: workspace.ArmUnknownRequester})
	if done {
		return answer(resp, refused)
	}
	log := requester.Log
	if requester.Record.Repo != repository.ID {
		return answer(resp, s.refuse(log, rpc, resp, s.fill(refusal{Arm: workspace.ArmUnknownRequester, NotFound: true,
			Reason: fmt.Sprintf("the requester %s is not a workspace of the repository %s", requester.Record.ID, repository.ID),
			Fields: map[string]any{"workspace": requesterRef}})))
	}
	maker, cerr, done := s.asyncMergeMaker(ctx, log, rpc, resp, repository, async.GetSource().GetMaker())
	if done {
		return answer(resp, cerr)
	}
	if fork := forkSourceID(req.GetContextOrigin()); fork == nil || *fork != maker {
		return answer(resp, s.refuse(log, rpc, resp, s.fill(refusal{Arm: "async_merge_context_not_maker",
			Reason: fmt.Sprintf("an async merge forks the conversation of its maker %s", maker),
			Fields: map[string]any{"maker": &workspacev1.WorkspaceRef{Id: string(maker)}}})))
	}
	var closes ids.WorkspaceID
	if after := async.GetCloseAfter(); after != nil {
		closes = ids.WorkspaceID(after.GetWorkspace().GetId())
	}
	request := merge.Request{
		Workspace: requester.Record.ID,
		By:        merge.RequestedByAgent,
		Asker:     requester.Record.ID,
		Source: wsm.MergeSource{
			Kind:    wsm.MergeSourceBranch,
			Branch:  strings.TrimSpace(async.GetSource().GetBranch()),
			Front:   async.GetPlacement().GetFront() != nil,
			Version: versionIncrement(async.GetVersionIncrement()),
		},
		Detached: merge.NewDetachedRun(async.GetName(), async.GetInstruction(), async.GetCompactAfter(), closes,
			async.GetCleanupWorktree(), s.mergeActions(async.GetMergeActions()), maker, async.GetTarget().GetMaker() != nil),
	}
	enqueued, err := s.deps.Merge.Enqueue(ctx, request)
	if err != nil {
		return answer(resp, s.answerRefusal(log, rpc, resp, err, asyncMergeRenames))
	}
	log.Info("daemon.server.create_workspace", "accepted an async merge", dlog.Context{
		"requester": string(requester.Record.ID), "maker": string(maker), "branch": request.Source.Branch,
		"into_maker": request.Detached.IntoMaker, "front": request.Source.Front, "merge_workspace_branch": enqueued.MergeBranch})
	resp.Result = &agentreplv1.CreateWorkspaceResponse_AsyncMergeAccepted{
		AsyncMergeAccepted: &agentreplv1.CreateWorkspaceAsyncMergeAccepted{MergeWorkspaceBranch: enqueued.MergeBranch},
	}
	return connect.NewResponse(resp), nil
}

// asyncMergeMaker resolves an async merge's maker: the workspace that spawned
// the subagent, read from disk, or the workspace the request names, whose
// registration the merge queue checks. `done` reports that the caller must
// answer at once.
func (s *server) asyncMergeMaker(
	ctx context.Context,
	log dlog.Logger,
	rpc string,
	resp *agentreplv1.CreateWorkspaceResponse,
	repository wsm.Repository,
	maker *agentreplv1.CreateWorkspaceAsyncMergeMaker,
) (ids.WorkspaceID, *connect.Error, bool) {
	if named := maker.GetWorkspace(); named != nil {
		return ids.WorkspaceID(named.GetWorkspace().GetId()), nil, false
	}
	// validateCreateWorkspaceRequest guaranteed one of the two arms.
	agentID := strings.TrimSpace(maker.GetSubagent().GetAgentId())
	spawner, err := s.deps.SubagentMakers.Find(ctx, repository.ID, agentID)
	if errors.Is(err, subagentmaker.ErrNotFound) {
		return "", s.refuse(log, rpc, resp, s.fill(refusal{Arm: "async_merge_unknown_subagent", NotFound: true,
			Reason: fmt.Sprintf("no workspace's session of the repository holds the subagent %s", agentID),
			Fields: map[string]any{"agent_id": agentID}})), true
	}
	if err != nil {
		// The finder recorded the cause at ERROR.
		return "", fail(log, rpc, err), true
	}
	return spawner.ID, nil, false
}
