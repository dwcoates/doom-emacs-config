package server

import (
	"context"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
)

// The session-shape verbs (model, permission mode) and the daemon-hold tray's
// two verbs. The model and mode both travel the QUEUE's one delivery path, so
// neither can overtake a queued prompt; that ordering lives in the verbs.

// SetModel switches the session's model to the echoed catalog token.
func (s *server) SetModel(
	ctx context.Context,
	req *connect.Request[agentreplv1.SetModelRequest],
) (*connect.Response[agentreplv1.SetModelResponse], error) {
	const rpc = "SetModel"
	if err := validateSetModelRequest(req.Msg); err != nil {
		return nil, err
	}
	resp := &agentreplv1.SetModelResponse{}
	subject, cerr, done := s.subjectFor(ctx, rpc, req.Msg.GetWorkspace(), resp)
	if done {
		return answer(resp, cerr)
	}
	if err := s.deps.Verbs.SetModel(ctx, subject.Record.ID, req.Msg.GetModel().GetName()); err != nil {
		return answer(resp, s.answerRefusal(subject.Log, rpc, resp, err, nil))
	}
	resp.Result = &agentreplv1.SetModelResponse_Success{Success: &agentreplv1.SetModelSuccess{}}
	return connect.NewResponse(resp), nil
}

// SetPermissionMode switches the permission mode, validated against EXACTLY
// what the topbar's picker served.
func (s *server) SetPermissionMode(
	ctx context.Context,
	req *connect.Request[agentreplv1.SetPermissionModeRequest],
) (*connect.Response[agentreplv1.SetPermissionModeResponse], error) {
	const rpc = "SetPermissionMode"
	if err := validateSetPermissionModeRequest(req.Msg); err != nil {
		return nil, err
	}
	resp := &agentreplv1.SetPermissionModeResponse{}
	subject, cerr, done := s.subjectFor(ctx, rpc, req.Msg.GetWorkspace(), resp)
	if done {
		return answer(resp, cerr)
	}
	if err := s.deps.Verbs.SetPermissionMode(ctx, subject.Record.ID, req.Msg.GetMode()); err != nil {
		return answer(resp, s.answerRefusal(subject.Log, rpc, resp, err, nil))
	}
	resp.Result = &agentreplv1.SetPermissionModeResponse_Success{
		Success: &agentreplv1.SetPermissionModeSuccess{},
	}
	return connect.NewResponse(resp), nil
}

// UpdateHeldPrompt acts on one held prompt: deliver it now, discard it, or
// accept a hold_for_turn_end verdict. All three are the queue's own verbs.
func (s *server) UpdateHeldPrompt(
	ctx context.Context,
	req *connect.Request[agentreplv1.UpdateHeldPromptRequest],
) (*connect.Response[agentreplv1.UpdateHeldPromptResponse], error) {
	const rpc = "UpdateHeldPrompt"
	if err := validateUpdateHeldPromptRequest(req.Msg); err != nil {
		return nil, err
	}
	resp := &agentreplv1.UpdateHeldPromptResponse{}
	subject, cerr, done := s.subjectFor(ctx, rpc, req.Msg.GetWorkspace(), resp)
	if done {
		return answer(resp, cerr)
	}
	turn := ids.TurnID(req.Msg.GetTurn().GetValue())
	var err error
	switch {
	case req.Msg.GetRelease() != nil:
		err = s.deps.Queue.Release(ctx, subject.Record.ID, turn)
	case req.Msg.GetDrop() != nil:
		err = s.deps.Queue.Drop(ctx, subject.Record.ID, turn)
	default:
		err = s.deps.Queue.Accept(ctx, subject.Record.ID, turn)
	}
	if err != nil {
		return answer(resp, s.answerRefusal(subject.Log, rpc, resp, err, nil))
	}
	subject.Log.Debug("daemon.server.update_held_prompt", "acted on a held prompt",
		dlog.Context{"turn": string(turn)})
	resp.Result = &agentreplv1.UpdateHeldPromptResponse_Success{
		Success: &agentreplv1.UpdateHeldPromptSuccess{},
	}
	return connect.NewResponse(resp), nil
}

// AnswerHeldOffer answers a question the daemon parked in the tray. The one
// offer that exists is the merge dequeue: keep the queued merge, or release it.
func (s *server) AnswerHeldOffer(
	ctx context.Context,
	req *connect.Request[agentreplv1.AnswerHeldOfferRequest],
) (*connect.Response[agentreplv1.AnswerHeldOfferResponse], error) {
	const rpc = "AnswerHeldOffer"
	if err := validateAnswerHeldOfferRequest(req.Msg); err != nil {
		return nil, err
	}
	resp := &agentreplv1.AnswerHeldOfferResponse{}
	subject, cerr, done := s.subjectFor(ctx, rpc, req.Msg.GetWorkspace(), resp)
	if done {
		return answer(resp, cerr)
	}
	keep := req.Msg.GetMergeDequeue().GetKeep() != nil
	if err := s.deps.Merge.AnswerDequeue(ctx, subject.Record.ID, keep); err != nil {
		return answer(resp, s.answerRefusal(subject.Log, rpc, resp, err, nil))
	}
	subject.Log.Debug("daemon.server.answer_held_offer", "answered the merge dequeue offer",
		dlog.Context{"keep": keep})
	resp.Result = &agentreplv1.AnswerHeldOfferResponse_Success{
		Success: &agentreplv1.AnswerHeldOfferSuccess{},
	}
	return connect.NewResponse(resp), nil
}
