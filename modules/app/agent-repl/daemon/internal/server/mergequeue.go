package server

import (
	"context"
	"fmt"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/merge"
	"agent-repld/internal/workspace"
	"agent-repld/internal/wsm"
)

// The merge queue's agent-facing reads and follows: its status, and a
// workspace's subscription to its changes.

// subscriberRenames spells an unknown subscriber as the subscription rpcs'
// own arm.
var subscriberRenames = map[string]string{workspace.ArmUnknownWorkspace: "unknown_subscriber"}

// GetMergeQueueStatus answers each repository's merge queue as it stands.
func (s *server) GetMergeQueueStatus(
	ctx context.Context,
	req *connect.Request[agentreplv1.GetMergeQueueStatusRequest],
) (*connect.Response[agentreplv1.GetMergeQueueStatusResponse], error) {
	const rpc = "GetMergeQueueStatus"
	if err := validateGetMergeQueueStatusRequest(req.Msg); err != nil {
		return nil, err
	}
	resp := &agentreplv1.GetMergeQueueStatusResponse{}
	queues, err := s.mergeQueueStatus(ctx, req.Msg.Repository)
	if err != nil {
		return answer(resp, s.answerRefusal(s.log, rpc, resp, err, nil))
	}
	s.log.Debug("daemon.server.merge_queue_status", "answered the merge queues' status", dlog.Context{"queues": len(queues)})
	resp.Result = &agentreplv1.GetMergeQueueStatusResponse_Success{
		Success: &agentreplv1.GetMergeQueueStatusSuccess{Queues: queues},
	}
	return connect.NewResponse(resp), nil
}

// SubscribeMergeQueue subscribes a workspace's agent to a merge queue's
// changes and answers the queues as they stand, so nothing slips between
// reading the queue and following it.
func (s *server) SubscribeMergeQueue(
	ctx context.Context,
	req *connect.Request[agentreplv1.SubscribeMergeQueueRequest],
) (*connect.Response[agentreplv1.SubscribeMergeQueueResponse], error) {
	const rpc = "SubscribeMergeQueue"
	if err := validateSubscribeMergeQueueRequest(req.Msg); err != nil {
		return nil, err
	}
	resp := &agentreplv1.SubscribeMergeQueueResponse{}
	subject, refused, done := s.resolveNaming(ctx, rpc, resp, req.Msg.GetSubscriber(), subscriberRenames)
	if done {
		return answer(resp, refused)
	}
	replaced, err := s.deps.Merge.Subscribe(ctx, subject.Record.ID, scopeOf(req.Msg.Repository), subscriptionDetailOf(req.Msg.GetDetail()))
	if err != nil {
		return answer(resp, s.answerRefusal(subject.Log, rpc, resp, err, nil))
	}
	queues, err := s.mergeQueueStatus(ctx, req.Msg.Repository)
	if err != nil {
		return answer(resp, s.answerRefusal(subject.Log, rpc, resp, err, nil))
	}
	taken := &agentreplv1.SubscribeMergeQueueSuccessTaken{Taken: &agentreplv1.SubscribeMergeQueueSuccessTaken_Subscribed{
		Subscribed: &agentreplv1.SubscribeMergeQueueSuccessTakenSubscribed{}}}
	if replaced {
		taken.Taken = &agentreplv1.SubscribeMergeQueueSuccessTaken_Resubscribed{
			Resubscribed: &agentreplv1.SubscribeMergeQueueSuccessTakenResubscribed{}}
	}
	resp.Result = &agentreplv1.SubscribeMergeQueueResponse_Success{
		Success: &agentreplv1.SubscribeMergeQueueSuccess{Taken: taken, Queues: queues},
	}
	return connect.NewResponse(resp), nil
}

// UnsubscribeMergeQueue ends a workspace's subscription. A workspace holding
// none in the scope is an answer, not a refusal.
func (s *server) UnsubscribeMergeQueue(
	ctx context.Context,
	req *connect.Request[agentreplv1.UnsubscribeMergeQueueRequest],
) (*connect.Response[agentreplv1.UnsubscribeMergeQueueResponse], error) {
	const rpc = "UnsubscribeMergeQueue"
	if err := validateUnsubscribeMergeQueueRequest(req.Msg); err != nil {
		return nil, err
	}
	resp := &agentreplv1.UnsubscribeMergeQueueResponse{}
	subject, refused, done := s.resolveNaming(ctx, rpc, resp, req.Msg.GetSubscriber(), subscriberRenames)
	if done {
		return answer(resp, refused)
	}
	success := &agentreplv1.UnsubscribeMergeQueueSuccess{Ended: &agentreplv1.UnsubscribeMergeQueueSuccess_Unsubscribed{
		Unsubscribed: &agentreplv1.UnsubscribeMergeQueueSuccessUnsubscribed{}}}
	err := s.deps.Merge.Unsubscribe(ctx, subject.Record.ID, scopeOf(req.Msg.Repository))
	if refusal, ok := merge.Refused(err); ok && refusal.Arm == merge.ArmNotSubscribed {
		success.Ended = &agentreplv1.UnsubscribeMergeQueueSuccess_NotSubscribed{
			NotSubscribed: &agentreplv1.UnsubscribeMergeQueueSuccessNotSubscribed{}}
		err = nil
	}
	if err != nil {
		return answer(resp, s.answerRefusal(subject.Log, rpc, resp, err, nil))
	}
	resp.Result = &agentreplv1.UnsubscribeMergeQueueResponse_Success{Success: success}
	return connect.NewResponse(resp), nil
}

// mergeQueueStatus reads the queues REF scopes (every repository when nil)
// as the wire's status.
func (s *server) mergeQueueStatus(ctx context.Context, ref *workspacev1.RepositoryRef) ([]*agentreplv1.MergeQueueStatus, error) {
	statuses, err := s.deps.Merge.Status(ctx, scopeOf(ref))
	if err != nil {
		return nil, err
	}
	out := make([]*agentreplv1.MergeQueueStatus, 0, len(statuses))
	for _, status := range statuses {
		out = append(out, mergeQueueStatusOf(status))
	}
	return out, nil
}

// mergeQueueStatusOf renders one queue's status.
func mergeQueueStatusOf(status merge.QueueStatus) *agentreplv1.MergeQueueStatus {
	advance := &agentreplv1.MergeQueueStatusAdvance{State: &agentreplv1.MergeQueueStatusAdvance_Running{Running: &agentreplv1.MergeQueueStatusAdvanceRunning{}}}
	if status.Queue.Paused {
		advance.State = &agentreplv1.MergeQueueStatusAdvance_Paused{Paused: &agentreplv1.MergeQueueStatusAdvancePaused{}}
	}
	out := &agentreplv1.MergeQueueStatus{
		Repository: &workspacev1.RepositoryRef{Id: string(status.Repository.ID), Dir: status.Repository.Dir},
		Advance:    advance,
	}
	for _, member := range status.Queue.Members {
		kind := &agentreplv1.MergeQueueStatusEntryKind{Kind: &agentreplv1.MergeQueueStatusEntryKind_OwnBranch{OwnBranch: &agentreplv1.MergeQueueStatusEntryKindOwnBranch{}}}
		if member.Requester != "" {
			kind.Kind = &agentreplv1.MergeQueueStatusEntryKind_AsyncMerge{AsyncMerge: &agentreplv1.MergeQueueStatusEntryKindAsyncMerge{
				Requester: &workspacev1.WorkspaceRef{Id: string(member.Requester)}}}
		}
		stage := &agentreplv1.MergeQueueStatusEntryStage{Stage: &agentreplv1.MergeQueueStatusEntryStage_Waiting{Waiting: &agentreplv1.MergeQueueStatusEntryStageWaiting{}}}
		if member.Started {
			stage.Stage = &agentreplv1.MergeQueueStatusEntryStage_Running{Running: &agentreplv1.MergeQueueStatusEntryStageRunning{
				StepDescription: member.Step.Words()}}
		}
		out.Entries = append(out.Entries, &agentreplv1.MergeQueueStatusEntry{
			Workspace:     &workspacev1.WorkspaceRef{Id: string(member.Workspace)},
			Name:          member.Name,
			Branch:        member.Branch,
			Kind:          kind,
			Stage:         stage,
			PlacedAtFront: member.Front,
		})
	}
	return out
}

// subscriptionDetailOf renders a subscription's detail arm as the store's
// detail. validateSubscribeMergeQueueRequest has refused a detail with no
// arm, so an arm this switch does not know is a contract arm added without
// its mapping, and fails hard.
func subscriptionDetailOf(detail *agentreplv1.SubscribeMergeQueueDetail) wsm.MergeQueueDetail {
	switch detail.GetLevel().(type) {
	case *agentreplv1.SubscribeMergeQueueDetail_EveryChange:
		return wsm.MergeQueueDetailEveryChange
	case *agentreplv1.SubscribeMergeQueueDetail_Advances:
		return wsm.MergeQueueDetailAdvances
	case *agentreplv1.SubscribeMergeQueueDetail_Depletes:
		return wsm.MergeQueueDetailDepletes
	}
	panic(fmt.Sprintf("server: a merge queue subscription detail %T has no store detail", detail.GetLevel()))
}
