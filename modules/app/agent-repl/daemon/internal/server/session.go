package server

import (
	"context"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/workspace"
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
		// The shim spells the catalog refusal `model_not_in_catalog` and
		// `SetModelError` spells the SAME condition `not_in_catalog`, so the
		// shim's arm is RENAMED onto the rpc's rather than left to answer as
		// an unlanded arm: a refused model must come back as a refused model,
		// never as a daemon the client could not reach.
		return answer(resp, s.answerRefusal(subject.Log, rpc, resp, err,
			map[string]string{workspace.ArmShimModelNotInCatalog: workspace.ArmNotInCatalog}))
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

// SelectAccount makes the workspace's session spend as one of the roots the
// topbar's account cell served.
//
// THE SWITCH IS THE ANSWER, AND THE LOGIN IS NOT. A chosen root that holds no
// login is still a success carrying `logged_in: false`: the client opens that
// root's login flow next, exactly as the logged-out cell's own click does.
func (s *server) SelectAccount(
	ctx context.Context,
	req *connect.Request[agentreplv1.SelectAccountRequest],
) (*connect.Response[agentreplv1.SelectAccountResponse], error) {
	const rpc = "SelectAccount"
	if err := validateSelectAccountRequest(req.Msg); err != nil {
		return nil, err
	}
	resp := &agentreplv1.SelectAccountResponse{}
	subject, cerr, done := s.subjectFor(ctx, rpc, req.Msg.GetWorkspace(), resp)
	if done {
		return answer(resp, cerr)
	}
	loggedIn, err := s.deps.Verbs.SelectAccount(ctx, subject.Record.ID, req.Msg.GetConfigDir())
	if err != nil {
		return answer(resp, s.answerRefusal(subject.Log, rpc, resp, err, nil))
	}
	subject.Log.Info("daemon.server.select_account", "the workspace switched account roots",
		dlog.Context{"config_dir": req.Msg.GetConfigDir(), "logged_in": loggedIn})
	resp.Result = &agentreplv1.SelectAccountResponse_Success{
		Success: &agentreplv1.SelectAccountSuccess{LoggedIn: loggedIn},
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

// EditHeldPrompt runs one step of a held-prompt edit: begin, commit or
// cancel. All three are the queue's own verbs; the begin's editor probe is
// this server's own knowledge of whether a host stream stands for the
// workspace, which is what the claim is scoped to.
func (s *server) EditHeldPrompt(
	ctx context.Context,
	req *connect.Request[agentreplv1.EditHeldPromptRequest],
) (*connect.Response[agentreplv1.EditHeldPromptResponse], error) {
	const rpc = "EditHeldPrompt"
	if err := validateEditHeldPromptRequest(req.Msg); err != nil {
		return nil, err
	}
	resp := &agentreplv1.EditHeldPromptResponse{}
	subject, cerr, done := s.subjectFor(ctx, rpc, req.Msg.GetWorkspace(), resp)
	if done {
		return answer(resp, cerr)
	}
	ws := subject.Record.ID
	turn := ids.TurnID(req.Msg.GetTurn().GetValue())
	var (
		err  error
		step string
	)
	switch {
	case req.Msg.GetBegin() != nil:
		step = "begin"
		err = s.deps.Queue.BeginEdit(ctx, ws, turn, func() bool { return s.hostStreamHeld(ws) })
	case req.Msg.GetCommit() != nil:
		step = "commit"
		err = s.deps.Queue.CommitEdit(ctx, ws, turn, req.Msg.GetCommit().GetSaid())
	default:
		step = "cancel"
		err = s.deps.Queue.CancelEdit(ctx, ws, turn)
	}
	if err != nil {
		return answer(resp, s.answerRefusal(subject.Log, rpc, resp, err, nil))
	}
	subject.Log.Debug("daemon.server.edit_held_prompt", "ran a held-prompt edit step",
		dlog.Context{"turn": string(turn), "step": step})
	resp.Result = &agentreplv1.EditHeldPromptResponse_Success{
		Success: &agentreplv1.EditHeldPromptSuccess{},
	}
	return connect.NewResponse(resp), nil
}

// FoldHeldPrompt folds one held prompt into the held prompt directly ahead of
// it. It is the queue's own verb; the two typed echo tokens travel to it
// unchanged, so the queue refuses a fold into anything but the entry the
// client saw ahead.
func (s *server) FoldHeldPrompt(
	ctx context.Context,
	req *connect.Request[agentreplv1.FoldHeldPromptRequest],
) (*connect.Response[agentreplv1.FoldHeldPromptResponse], error) {
	const rpc = "FoldHeldPrompt"
	if err := validateFoldHeldPromptRequest(req.Msg); err != nil {
		return nil, err
	}
	resp := &agentreplv1.FoldHeldPromptResponse{}
	subject, cerr, done := s.subjectFor(ctx, rpc, req.Msg.GetWorkspace(), resp)
	if done {
		return answer(resp, cerr)
	}
	turn := ids.TurnID(req.Msg.GetTurn().GetValue())
	above := ids.TurnID(req.Msg.GetAbove().GetValue())
	if err := s.deps.Queue.Fold(ctx, subject.Record.ID, turn, above); err != nil {
		return answer(resp, s.answerRefusal(subject.Log, rpc, resp, err, nil))
	}
	subject.Log.Debug("daemon.server.fold_held_prompt", "folded a held prompt into the one ahead",
		dlog.Context{"turn": string(turn), "above_turn": string(above)})
	resp.Result = &agentreplv1.FoldHeldPromptResponse_Success{
		Success: &agentreplv1.FoldHeldPromptSuccess{},
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
