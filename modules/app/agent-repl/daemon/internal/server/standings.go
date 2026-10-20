package server

import (
	"context"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"agent-repld/internal/ids"
)

// SetWorkspaceOneShot marks a workspace a one-shot, or unmarks it, and
// answers the completion directive a marked workspace follows.
func (s *server) SetWorkspaceOneShot(
	ctx context.Context,
	req *connect.Request[agentreplv1.SetWorkspaceOneShotRequest],
) (*connect.Response[agentreplv1.SetWorkspaceOneShotResponse], error) {
	const rpc = "SetWorkspaceOneShot"
	if err := validateWorkspaceRequester("requester", req.Msg.GetRequester()); err != nil {
		return nil, err
	}
	resp := &agentreplv1.SetWorkspaceOneShotResponse{}
	subject, cerr, done := s.subjectFor(ctx, rpc, req.Msg.GetWorkspace(), resp)
	if done {
		return answer(resp, cerr)
	}
	directive, err := s.deps.Verbs.SetOneShot(ctx, subject.Record.ID, requesterID(req.Msg.GetRequester()), req.Msg.GetOneShot())
	if err != nil {
		return answer(resp, s.answerRefusal(subject.Log, rpc, resp, err, nil))
	}
	success := &agentreplv1.SetWorkspaceOneShotSuccess{}
	if req.Msg.GetOneShot() {
		success.CompletionDirective = &directive
	}
	resp.Result = &agentreplv1.SetWorkspaceOneShotResponse_Success{Success: success}
	return connect.NewResponse(resp), nil
}

// SetWorkspaceCorrespondence marks a workspace awaiting another, or clears the
// mark.
func (s *server) SetWorkspaceCorrespondence(
	ctx context.Context,
	req *connect.Request[agentreplv1.SetWorkspaceCorrespondenceRequest],
) (*connect.Response[agentreplv1.SetWorkspaceCorrespondenceResponse], error) {
	const rpc = "SetWorkspaceCorrespondence"
	if err := validateSetWorkspaceCorrespondenceRequest(req.Msg); err != nil {
		return nil, err
	}
	resp := &agentreplv1.SetWorkspaceCorrespondenceResponse{}
	subject, cerr, done := s.subjectFor(ctx, rpc, req.Msg.GetWorkspace(), resp)
	if done {
		return answer(resp, cerr)
	}
	var awaited *ids.WorkspaceID
	if mark := req.Msg.GetAwaiting(); mark != nil {
		id := ids.WorkspaceID(mark.GetAwaited().GetId())
		awaited = &id
	}
	if err := s.deps.Verbs.SetCorrespondence(ctx, subject.Record.ID, requesterID(req.Msg.GetRequester()), awaited); err != nil {
		return answer(resp, s.answerRefusal(subject.Log, rpc, resp, err, nil))
	}
	// The footer's background arm moved; the host view is recomposed from it.
	s.PublishHostWorkspace(ctx, subject.Record.ID)
	resp.Result = &agentreplv1.SetWorkspaceCorrespondenceResponse_Success{Success: &agentreplv1.SetWorkspaceCorrespondenceSuccess{}}
	return connect.NewResponse(resp), nil
}
