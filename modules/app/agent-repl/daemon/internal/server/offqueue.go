package server

import (
	"context"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/merge"
)

// The merges off their queues (merge/offqueue.go): putting one back in line
// as itself, and discarding one.

// RequeueMerge puts a merge that left its queue without landing back in line
// as itself.
func (s *server) RequeueMerge(
	ctx context.Context,
	req *connect.Request[agentreplv1.RequeueMergeRequest],
) (*connect.Response[agentreplv1.RequeueMergeResponse], error) {
	const rpc = "RequeueMerge"
	if err := validateRequeueMergeRequest(req.Msg); err != nil {
		return nil, err
	}
	resp := &agentreplv1.RequeueMergeResponse{}
	subject, cerr, done := s.subjectFor(ctx, rpc, req.Msg.GetWorkspace(), resp)
	if done {
		return answer(resp, cerr)
	}
	requeue := requeueOf(subject.Record.ID, req.Msg)
	s.recordOffQueueAsk(rpc, subject.Record.ID, req.Msg.GetRequester())
	// THE MODEL IS CHECKED AGAINST THE LOGIN'S CATALOG before anything
	// changes: the session will start under it.
	if requeue.Model != "" {
		if err := s.deps.Verbs.RequireCatalogModel(ctx, subject.Record.ID, requeue.Model); err != nil {
			return answer(resp, s.answerRefusal(subject.Log, rpc, resp, err, nil))
		}
	}
	requeued, err := s.deps.Merge.Requeue(ctx, requeue)
	if err != nil {
		return answer(resp, s.answerRefusal(subject.Log, rpc, resp, err, nil))
	}
	success := &agentreplv1.RequeueMergeSuccess{}
	if requeued.InLine {
		position := uint32(requeued.Position)
		success.Position = &position
	}
	resp.Result = &agentreplv1.RequeueMergeResponse_Success{Success: success}
	return connect.NewResponse(resp), nil
}

// requeueOf reads the request into the orchestrator's requeue of WS.
func requeueOf(ws ids.WorkspaceID, req *agentreplv1.RequeueMergeRequest) merge.Requeue {
	out := merge.Requeue{Workspace: ws, Model: req.GetModel().GetName()}
	out.By, out.Asker = askerOf(req.GetRequester())
	out.Front = req.GetPlacement().GetFront() != nil
	out.PlaceBehind = placedBehind(req.GetPlacement())
	return out
}

// DiscardMerge ends a request standing off its queue without landing it: only
// the request goes.
func (s *server) DiscardMerge(
	ctx context.Context,
	req *connect.Request[agentreplv1.DiscardMergeRequest],
) (*connect.Response[agentreplv1.DiscardMergeResponse], error) {
	const rpc = "DiscardMerge"
	if err := validateDiscardMergeRequest(req.Msg); err != nil {
		return nil, err
	}
	resp := &agentreplv1.DiscardMergeResponse{}
	subject, cerr, done := s.subjectFor(ctx, rpc, req.Msg.GetWorkspace(), resp)
	if done {
		return answer(resp, cerr)
	}
	ws := subject.Record.ID
	s.recordOffQueueAsk(rpc, ws, req.Msg.GetRequester())
	if err := s.deps.Merge.Discard(ctx, ws); err != nil {
		return answer(resp, s.answerRefusal(subject.Log, rpc, resp, err, nil))
	}
	resp.Result = &agentreplv1.DiscardMergeResponse_Success{Success: &agentreplv1.DiscardMergeSuccess{}}
	return connect.NewResponse(resp), nil
}

// recordOffQueueAsk records WHO asked to requeue or discard a merge, before
// it is acted on, as recordMergeQueueAsk records every other queue action.
func (s *server) recordOffQueueAsk(rpc string, ws ids.WorkspaceID, requester *agentreplv1.WorkspaceRequester) {
	by, asker := askerOf(requester)
	fields := dlog.Context{"rpc": rpc, "target": string(ws), "requested_by": by.String()}
	if asker != "" {
		fields["asker"] = string(asker)
	}
	s.log.Info("daemon.server.off_queue_merge", "asked to act on a merge off its queue", fields)
}

// offQueueStatusOf renders one repository's merges off its queue
// (agentrepl.v1.MergeQueueStatus.off_queue).
func offQueueStatusOf(members []merge.OffQueueMember) []*agentreplv1.MergeQueueStatusOffQueue {
	out := make([]*agentreplv1.MergeQueueStatusOffQueue, 0, len(members))
	for _, member := range members {
		out = append(out, &agentreplv1.MergeQueueStatusOffQueue{
			Workspace: &workspacev1.WorkspaceRef{Id: string(member.Workspace)},
			Name:      member.Name,
			Branch:    member.Branch,
			Cause:     merge.OffQueueCause(member.Cause),
			LeftAtMs:  member.LeftAt.UnixMilli(),
		})
	}
	return out
}
