package server

import (
	"context"
	"fmt"
	"time"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/feedid"
	"claude-repld/internal/ids"
	"claude-repld/internal/workspace"
)

// The four "act on what is on screen" verbs. Each one decodes the clicked row's
// FeedId into the ask it addresses, composes the TYPED answer the verbs take,
// and delegates; the echo rules (what was served, what may be granted) live in
// the verbs and are not repeated here.

// askIDFrom decodes a clicked row's FeedId into the ask id it carries, refusing
// a value that does not decode and one that belongs to another workspace.
func askIDFrom(rpc string, ws ids.WorkspaceID, id *frontendv1.FeedId) (feedid.Ref, *refusal) {
	ref, err := feedid.Decode(id)
	if err != nil {
		return feedid.Ref{}, &refusal{
			Arm:    "feed_undecodable",
			Reason: fmt.Sprintf("%s: the row id %q does not decode: %v", rpc, id.GetValue(), err),
		}
	}
	if ref.WS != ws {
		return feedid.Ref{}, &refusal{
			Arm: "feed_not_in_workspace",
			Reason: fmt.Sprintf("%s: the row id %q belongs to workspace %q",
				rpc, id.GetValue(), ref.WS),
		}
	}
	return ref, nil
}

// Interrupt stops what the target names. "Nothing was running" is a SUCCESS
// outcome, not a failure; the unconfirmed turn interrupt with live detached
// agents answers the confirm_required challenge.
func (s *server) Interrupt(
	ctx context.Context,
	req *connect.Request[agentreplv1.InterruptRequest],
) (*connect.Response[agentreplv1.InterruptResponse], error) {
	const rpc = "Interrupt"
	if err := validateInterruptRequest(req.Msg); err != nil {
		return nil, err
	}
	resp := &agentreplv1.InterruptResponse{}
	subject, r, err := s.resolveRef(ctx, rpc, req.Msg.GetWorkspace())
	if err != nil {
		return nil, fail(s.log, rpc, err)
	}
	if r != nil {
		return answer(resp, s.refuse(s.log, rpc, resp, *r))
	}

	var target workspace.InterruptTarget
	switch {
	case req.Msg.GetTurn() != nil:
		target.Turn = true
	case req.Msg.GetAllAgents() != nil:
		target.AllAgents = true
	default:
		ref, refused := askIDFrom(rpc, subject.Record.ID, req.Msg.GetDetached())
		if refused != nil {
			// Interrupt has no feed arms; `not_detached_work` is the arm the
			// contract carries for a target that is not detached work.
			return answer(resp, s.refuse(subject.Log, rpc, resp, s.fill(refusal{
				Arm:    workspace.ArmNotDetachedWork,
				Reason: refused.Reason,
			})))
		}
		target.Detached = &ref
	}

	outcome, err := s.deps.Verbs.Interrupt(ctx, subject.Record.ID, target, req.Msg.GetConfirmAgents())
	if err != nil {
		if refused, ok := s.asRefusal(err); ok {
			return answer(resp, s.refuse(subject.Log, rpc, resp, refused))
		}
		return nil, fail(subject.Log, rpc, err)
	}
	success := &agentreplv1.InterruptSuccess{}
	switch {
	case outcome.NothingRunning:
		success.Outcome = &agentreplv1.InterruptSuccess_NothingRunning{
			NothingRunning: &agentreplv1.InterruptNothingRunning{},
		}
	case outcome.Turn:
		success.Outcome = &agentreplv1.InterruptSuccess_InterruptedTurn{
			InterruptedTurn: &agentreplv1.InterruptedTurn{},
		}
	default:
		success.Outcome = &agentreplv1.InterruptSuccess_InterruptedDetached{
			InterruptedDetached: &agentreplv1.InterruptedDetached{
				Count: int64(outcome.DetachedCount),
			},
		}
	}
	subject.Log.Debug("daemon.server.interrupt", "answered what the interrupt stopped",
		dlog.Context{"nothing_running": outcome.NothingRunning, "detached": outcome.DetachedCount})
	resp.Result = &agentreplv1.InterruptResponse_Success{Success: success}
	return connect.NewResponse(resp), nil
}

// AnswerPermission delivers the permission card's verdict. `allow_standing`
// carries no standing of its own: the verbs substitute the grant the ask
// actually OFFERED, so a client can never write a rule it was not shown.
func (s *server) AnswerPermission(
	ctx context.Context,
	req *connect.Request[agentreplv1.AnswerPermissionRequest],
) (*connect.Response[agentreplv1.AnswerPermissionResponse], error) {
	const rpc = "AnswerPermission"
	if err := validateAnswerPermissionRequest(req.Msg); err != nil {
		return nil, err
	}
	resp := &agentreplv1.AnswerPermissionResponse{}
	subject, r, err := s.resolveRef(ctx, rpc, req.Msg.GetWorkspace())
	if err != nil {
		return nil, fail(s.log, rpc, err)
	}
	if r != nil {
		return answer(resp, s.refuse(s.log, rpc, resp, *r))
	}
	ref, refused := askIDFrom(rpc, subject.Record.ID, req.Msg.GetPermission())
	if refused != nil {
		return answer(resp, s.refuse(subject.Log, rpc, resp, s.fill(refusal{
			Arm: "ask_not_standing", Reason: refused.Reason, NotFound: true,
		})))
	}

	decision := &conversationv1.AgentPermissionDecision{
		Ask: &conversationv1.AgentPermissionId{Value: ref.Row.ID},
	}
	switch {
	case req.Msg.GetAllowOnce() != nil:
		decision.Decision = &conversationv1.AgentPermissionDecision_Allowed{
			Allowed: &conversationv1.AgentPermissionAllowed{
				Scope: &conversationv1.AgentPermissionAllowed_Once{
					Once: &conversationv1.AgentPermissionAllowedOnce{},
				},
			},
		}
	case req.Msg.GetAllowStanding() != nil:
		decision.Decision = &conversationv1.AgentPermissionDecision_Allowed{
			Allowed: &conversationv1.AgentPermissionAllowed{
				Scope: &conversationv1.AgentPermissionAllowed_Standing{
					// The grant is filled in by the verbs from what the ask
					// OFFERED; the request carries no standing of its own.
					Standing: &conversationv1.AgentPermissionAllowedStanding{},
				},
			},
		}
	default:
		decision.Decision = &conversationv1.AgentPermissionDecision_Denied{
			Denied: &conversationv1.AgentPermissionDeniedByUser{
				Message: req.Msg.GetDeny().GetReason().GetText(),
			},
		}
	}

	err = s.deps.Verbs.AnswerPermission(ctx, subject.Record.ID, &conversationv1.AgentAnswer{
		Answer: &conversationv1.AgentAnswer_PermissionDecision{PermissionDecision: decision},
	})
	if err != nil {
		return answer(resp, s.answerRefusal(subject.Log, rpc, resp, err, map[string]string{
			workspace.ArmUnservedAnswer: "ask_not_standing",
		}))
	}
	resp.Result = &agentreplv1.AnswerPermissionResponse_Success{
		Success: &agentreplv1.AnswerPermissionSuccess{},
	}
	return connect.NewResponse(resp), nil
}

// AnswerQuestion delivers the question card's answers, each echoing the served
// question text and option labels.
func (s *server) AnswerQuestion(
	ctx context.Context,
	req *connect.Request[agentreplv1.AnswerQuestionRequest],
) (*connect.Response[agentreplv1.AnswerQuestionResponse], error) {
	const rpc = "AnswerQuestion"
	if err := validateAnswerQuestionRequest(req.Msg); err != nil {
		return nil, err
	}
	resp := &agentreplv1.AnswerQuestionResponse{}
	subject, r, err := s.resolveRef(ctx, rpc, req.Msg.GetWorkspace())
	if err != nil {
		return nil, fail(s.log, rpc, err)
	}
	if r != nil {
		return answer(resp, s.refuse(s.log, rpc, resp, *r))
	}
	ref, refused := askIDFrom(rpc, subject.Record.ID, req.Msg.GetQuestion())
	if refused != nil {
		return answer(resp, s.refuse(subject.Log, rpc, resp, s.fill(refusal{
			Arm: "ask_not_standing", Reason: refused.Reason, NotFound: true,
		})))
	}

	selections := make([]*conversationv1.AgentQuestionSelection, 0, len(req.Msg.GetAnswers()))
	for _, given := range req.Msg.GetAnswers() {
		selection := &conversationv1.AgentQuestionSelection{
			Question: &conversationv1.AgentQuestionText{Text: given.GetQuestionText()},
		}
		for _, chosen := range given.GetChosen() {
			selection.Chosen = append(selection.Chosen, &conversationv1.AgentQuestionChoice{
				Label: &conversationv1.AgentQuestionOptionLabel{Label: chosen},
			})
		}
		if given.OtherText != nil {
			selection.FreeText = &conversationv1.AgentQuestionFreeText{
				Text: given.GetOtherText().GetText(),
			}
		}
		selections = append(selections, selection)
	}

	err = s.deps.Verbs.AnswerQuestion(ctx, subject.Record.ID, &conversationv1.AgentAnswer{
		Answer: &conversationv1.AgentAnswer_QuestionAnswer{
			QuestionAnswer: &conversationv1.AgentQuestionAnswer{
				Ask:     &conversationv1.AgentQuestionId{Value: ref.Row.ID},
				Answers: &conversationv1.AgentQuestionAnswers{Answers: selections},
			},
		},
	})
	if err != nil {
		// AnswerQuestion spells the two conditions apart with TWO ARMS, and so
		// does the verb: a card that is not standing raises
		// workspace.ArmAskNotStanding, which AnswerQuestionError carries under
		// that very name, and a value the standing batch never offered raises
		// ArmUnservedAnswer, renamed here onto `unserved_value{text}`.
		return answer(resp, s.answerRefusal(subject.Log, rpc, resp, err,
			map[string]string{workspace.ArmUnservedAnswer: "unserved_value"}))
	}
	resp.Result = &agentreplv1.AnswerQuestionResponse_Success{
		Success: &agentreplv1.AnswerQuestionSuccess{},
	}
	return connect.NewResponse(resp), nil
}

// AnswerColdGate resolves the standing cold gate: pay, clear, or
// compact{model, scope}. The scope travels beside the answer because the
// resolved trace carries only the model while the shim's remediation needs
// both, and a scope is never defaulted.
func (s *server) AnswerColdGate(
	ctx context.Context,
	req *connect.Request[agentreplv1.AnswerColdGateRequest],
) (*connect.Response[agentreplv1.AnswerColdGateResponse], error) {
	const rpc = "AnswerColdGate"
	if err := validateAnswerColdGateRequest(req.Msg); err != nil {
		return nil, err
	}
	resp := &agentreplv1.AnswerColdGateResponse{}
	subject, r, err := s.resolveRef(ctx, rpc, req.Msg.GetWorkspace())
	if err != nil {
		return nil, fail(s.log, rpc, err)
	}
	if r != nil {
		return answer(resp, s.refuse(s.log, rpc, resp, *r))
	}

	resolvedGate := &frontendv1.FeedColdGateResolved{AtMs: time.Now().UnixMilli()}
	scope := conversationv1.SessionCompactScope_SESSION_COMPACT_SCOPE_UNSPECIFIED
	switch {
	case req.Msg.GetPay() != nil:
		resolvedGate.Choice = &frontendv1.FeedColdGateResolved_Pay{
			Pay: &frontendv1.FeedColdGateResolvedPay{},
		}
	case req.Msg.GetClear() != nil:
		resolvedGate.Choice = &frontendv1.FeedColdGateResolved_Clear{
			Clear: &frontendv1.FeedColdGateResolvedClear{},
		}
	default:
		compact := req.Msg.GetCompact()
		scope = compact.GetScope()
		resolvedGate.Choice = &frontendv1.FeedColdGateResolved_Compact{
			Compact: &frontendv1.FeedColdGateResolvedCompact{
				Model: &frontendv1.FeedColdGateModel{Model: compact.GetModel()},
				Scope: scope,
			},
		}
	}

	if err := s.deps.Verbs.AnswerColdGate(ctx, subject.Record.ID, resolvedGate, scope); err != nil {
		return answer(resp, s.answerRefusal(subject.Log, rpc, resp, err, nil))
	}
	resp.Result = &agentreplv1.AnswerColdGateResponse_Success{
		Success: &agentreplv1.AnswerColdGateSuccess{},
	}
	return connect.NewResponse(resp), nil
}
