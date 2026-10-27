package server

import (
	"context"
	"fmt"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/merge"
	"agent-repld/internal/workspace"
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

// SetDefaultModel makes the echoed catalog token the default of the login the
// workspace's session spends as. Every strip on that login is republished on
// the topbar stream; no running session changes.
func (s *server) SetDefaultModel(
	ctx context.Context,
	req *connect.Request[agentreplv1.SetDefaultModelRequest],
) (*connect.Response[agentreplv1.SetDefaultModelResponse], error) {
	const rpc = "SetDefaultModel"
	if err := validateSetDefaultModelRequest(req.Msg); err != nil {
		return nil, err
	}
	resp := &agentreplv1.SetDefaultModelResponse{}
	subject, cerr, done := s.subjectFor(ctx, rpc, req.Msg.GetWorkspace(), resp)
	if done {
		return answer(resp, cerr)
	}
	if err := s.deps.Verbs.SetDefaultModel(ctx, subject.Record.ID, req.Msg.GetModel().GetName()); err != nil {
		return answer(resp, s.answerRefusal(subject.Log, rpc, resp, err, nil))
	}
	resp.Result = &agentreplv1.SetDefaultModelResponse_Success{Success: &agentreplv1.SetDefaultModelSuccess{}}
	return connect.NewResponse(resp), nil
}

// SetEffort switches the session's reasoning effort to the echoed level. It is
// answered once the shim has the level in effect, which is after the running
// turn ends; the selector's new state arrives on the topbar stream.
func (s *server) SetEffort(
	ctx context.Context,
	req *connect.Request[agentreplv1.SetEffortRequest],
) (*connect.Response[agentreplv1.SetEffortResponse], error) {
	const rpc = "SetEffort"
	if err := validateSetEffortRequest(req.Msg); err != nil {
		return nil, err
	}
	resp := &agentreplv1.SetEffortResponse{}
	subject, cerr, done := s.subjectFor(ctx, rpc, req.Msg.GetWorkspace(), resp)
	if done {
		return answer(resp, cerr)
	}
	if err := s.deps.Verbs.SetEffort(ctx, subject.Record.ID, req.Msg.GetEffort()); err != nil {
		return answer(resp, s.answerRefusal(subject.Log, rpc, resp, err, nil))
	}
	resp.Result = &agentreplv1.SetEffortResponse_Success{Success: &agentreplv1.SetEffortSuccess{}}
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

// UpdateHeldPrompt acts on one held prompt: deliver it now, or discard it.
// Both are the queue's own verbs.
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
	default:
		err = s.deps.Queue.Drop(ctx, subject.Record.ID, turn)
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

// AnswerMergeQueueOffer answers the merge-queue question an interrupt raised
// in a workspace's pinned merge bubble: evict, prioritize, or cancel.
func (s *server) AnswerMergeQueueOffer(
	ctx context.Context,
	req *connect.Request[agentreplv1.AnswerMergeQueueOfferRequest],
) (*connect.Response[agentreplv1.AnswerMergeQueueOfferResponse], error) {
	const rpc = "AnswerMergeQueueOffer"
	if err := validateAnswerMergeQueueOfferRequest(req.Msg); err != nil {
		return nil, err
	}
	resp := &agentreplv1.AnswerMergeQueueOfferResponse{}
	subject, cerr, done := s.subjectFor(ctx, rpc, req.Msg.GetWorkspace(), resp)
	if done {
		return answer(resp, cerr)
	}
	decision := queueOfferAnswer(req.Msg)
	if err := s.deps.Merge.AnswerQueueOffer(ctx, subject.Record.ID, decision); err != nil {
		return answer(resp, s.answerRefusal(subject.Log, rpc, resp, err, nil))
	}
	subject.Log.Debug("daemon.server.answer_merge_queue_offer", "answered the merge-queue question",
		dlog.Context{"answer": decision.String()})
	resp.Result = &agentreplv1.AnswerMergeQueueOfferResponse_Success{
		Success: &agentreplv1.AnswerMergeQueueOfferSuccess{},
	}
	return connect.NewResponse(resp), nil
}

// queueOfferAnswer maps a validated request's decision arm to the
// orchestrator's answer. Validation has already refused an unset arm, so an
// arm this switch does not know is an invariant violation.
func queueOfferAnswer(req *agentreplv1.AnswerMergeQueueOfferRequest) merge.QueueOfferAnswer {
	switch req.GetDecision().(type) {
	case *agentreplv1.AnswerMergeQueueOfferRequest_Evict:
		return merge.QueueOfferEvict
	case *agentreplv1.AnswerMergeQueueOfferRequest_Prioritize:
		return merge.QueueOfferPrioritize
	case *agentreplv1.AnswerMergeQueueOfferRequest_Cancel:
		return merge.QueueOfferCancel
	}
	panic(fmt.Sprintf("server: undeclared AnswerMergeQueueOfferRequest.decision arm %T", req.GetDecision()))
}
