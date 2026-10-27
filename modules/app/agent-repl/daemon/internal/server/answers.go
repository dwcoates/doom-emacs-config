package server

import (
	"context"
	"errors"
	"fmt"

	"connectrpc.com/connect"
	"google.golang.org/protobuf/proto"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/feedid"
	"agent-repld/internal/ids"
	"agent-repld/internal/merge"
	"agent-repld/internal/replyquote"
	"agent-repld/internal/resolve/feed"
	"agent-repld/internal/workspace"
	"agent-repld/internal/wsm"
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

// interruptRow decodes the row an Interrupt targets, answering a row that does
// not decode or belongs to another workspace as ARM, the target's own refusal:
// Interrupt has no feed arms of its own.
func (s *server) interruptRow(rpc string, ws ids.WorkspaceID, id *frontendv1.FeedId, arm string) (feedid.Ref, *refusal) {
	ref, refused := askIDFrom(rpc, ws, id)
	if refused != nil {
		filled := s.fill(refusal{Arm: arm, Reason: refused.Reason})
		return feedid.Ref{}, &filled
	}
	return ref, nil
}

// interruptMergeTab answers an Interrupt aimed at an agentic merge tab
// (InterruptRequest.merge_tab): a row that does not decode, belongs to another
// workspace, or is no live agentic tab is the `not_merge_tab` refusal.
func (s *server) interruptMergeTab(ctx context.Context, rpc string, subject resolved, tab *frontendv1.FeedId, resp *agentreplv1.InterruptResponse) (*connect.Response[agentreplv1.InterruptResponse], error) {
	ref, refused := s.interruptRow(rpc, subject.Record.ID, tab, merge.ArmNotMergeTab)
	if refused != nil {
		return answer(resp, s.refuse(subject.Log, rpc, resp, *refused))
	}
	running, err := s.deps.Merge.InterruptTab(ctx, subject.Record.ID, ref)
	if err != nil {
		if refused, ok := s.asRefusal(err); ok {
			return answer(resp, s.refuse(subject.Log, rpc, resp, refused))
		}
		return nil, fail(subject.Log, rpc, err)
	}
	success := &agentreplv1.InterruptSuccess{}
	if running {
		success.Outcome = &agentreplv1.InterruptSuccess_InterruptedTurn{InterruptedTurn: &agentreplv1.InterruptedTurn{}}
	} else {
		success.Outcome = &agentreplv1.InterruptSuccess_NothingRunning{NothingRunning: &agentreplv1.InterruptNothingRunning{}}
	}
	subject.Log.Debug("daemon.server.interrupt", "answered what the merge-tab interrupt stopped", dlog.Context{"running": running})
	resp.Result = &agentreplv1.InterruptResponse_Success{Success: success}
	return connect.NewResponse(resp), nil
}

// askRow is the card verbs' shared prologue: resolve the request's workspace
// and decode the clicked row into the ask it addresses. A refused workspace
// answers its own arm, and a row that does not decode or belongs to another
// workspace answers `ask_not_standing`; either way the response is filled, and
// done reports that the caller answers it as it stands. err is a failure to
// resolve at all, already reported.
func (s *server) askRow(
	ctx context.Context,
	rpc string,
	ws *workspacev1.WorkspaceRef,
	row *frontendv1.FeedId,
	resp proto.Message,
) (subject resolved, ref feedid.Ref, done *connect.Error, answered bool, err error) {
	subject, r, err := s.resolveRef(ctx, rpc, ws)
	if err != nil {
		return resolved{}, feedid.Ref{}, nil, false, fail(s.log, rpc, err)
	}
	if r != nil {
		return resolved{}, feedid.Ref{}, s.refuse(s.log, rpc, resp, *r), true, nil
	}
	ref, refused := askIDFrom(rpc, subject.Record.ID, row)
	if refused != nil {
		return resolved{}, feedid.Ref{}, s.refuse(subject.Log, rpc, resp, s.fill(refusal{
			Arm: "ask_not_standing", Reason: refused.Reason, NotFound: true,
		})), true, nil
	}
	return subject, ref, nil, false, nil
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

	// A MERGE TAB'S AGENT is the merge orchestrator's to stop: it knows which
	// workspace's turn the tab draws, and an async merge's workspace is
	// revealed by the stop.
	if tab := req.Msg.GetMergeTab(); tab != nil {
		return s.interruptMergeTab(ctx, rpc, subject, tab, resp)
	}
	var target workspace.InterruptTarget
	// unanswered is the plan of a stop that rolls its unanswered prompt back.
	var unanswered *rollbackPlan
	switch {
	case req.Msg.GetTurn() != nil:
		target.Turn = true
		if req.Msg.GetTurn().GetRollBackUnanswered() != nil {
			// A PROMPT HELD BEHIND THE RUNNING TURN THAT NOTHING HAS ANSWERED
			// SINCE is what the stop takes back first, and the turn runs on.
			held, ok, err := s.deps.Queue.RollBackHeld(ctx, subject.Record.ID)
			if err != nil {
				return nil, fail(subject.Log, rpc, err)
			}
			if ok {
				return s.rolledBackHeld(subject.Log, held, resp)
			}
			plan, ok, err := s.unansweredRollback(ctx, subject.Record.ID, subject.Log)
			if err != nil {
				return nil, fail(subject.Log, rpc, err)
			}
			if ok {
				unanswered = &plan
				target.RollBack = &workspace.RollbackRequest{Turns: plan.turns, Since: plan.since, DropHeld: plan.dropHeld}
			}
		}
	case req.Msg.GetAllAgents() != nil:
		target.AllAgents = true
	default:
		// Interrupt has no feed arms; `not_detached_work` is the arm the
		// contract carries for a target that is not detached work.
		ref, refused := s.interruptRow(rpc, subject.Record.ID, req.Msg.GetDetached(), workspace.ArmNotDetachedWork)
		if refused != nil {
			return answer(resp, s.refuse(subject.Log, rpc, resp, *refused))
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
	case outcome.MergeQueueOffered:
		success.Outcome = &agentreplv1.InterruptSuccess_MergeQueueOffered{
			MergeQueueOffered: &agentreplv1.InterruptMergeQueueOffered{},
		}
	case outcome.NothingRunning:
		nothing := &agentreplv1.InterruptNothingRunning{}
		if target.Turn {
			// A TURN STOP WITH NOTHING RUNNING IS THE STOP KEY'S SECOND PRESS:
			// it offers rolling the latest prompt back.
			offer, err := s.interruptRollbackOffer(ctx, subject.Record.ID, subject.Log)
			if err != nil {
				return nil, fail(subject.Log, rpc, err)
			}
			nothing.Rollback = offer
		}
		success.Outcome = &agentreplv1.InterruptSuccess_NothingRunning{NothingRunning: nothing}
	case outcome.RolledBack:
		if unanswered == nil {
			return nil, fail(subject.Log, rpc, errors.New("the verbs rolled back a prompt the stop never planned"))
		}
		// THE ROLLED-BACK PROMPT'S SELECTION ENDS WITH IT: its row is gone.
		s.endSelection(subject.Log, subject.Record.ID, unanswered.row, returnToTail(), "rolled_back")
		success.Outcome = &agentreplv1.InterruptSuccess_RolledBackUnanswered{
			RolledBackUnanswered: &agentreplv1.InterruptRolledBackUnanswered{Prompt: replyquote.Words(unanswered.said)},
		}
	case outcome.Turn:
		interrupted := &agentreplv1.InterruptedTurn{}
		if outcome.RollbackRefused != nil {
			cause, err := s.rollBackErrorOf(subject.Log, rpc, outcome.RollbackRefused)
			if err != nil {
				return nil, err
			}
			interrupted.NotRolledBack = &agentreplv1.InterruptedTurnNotRolledBack{Cause: cause}
		}
		success.Outcome = &agentreplv1.InterruptSuccess_InterruptedTurn{InterruptedTurn: interrupted}
	default:
		success.Outcome = &agentreplv1.InterruptSuccess_InterruptedDetached{
			InterruptedDetached: &agentreplv1.InterruptedDetached{
				Count: int64(outcome.DetachedCount),
			},
		}
	}
	subject.Log.Debug("daemon.server.interrupt", "answered what the interrupt stopped",
		dlog.Context{"nothing_running": outcome.NothingRunning, "merge_queue_offered": outcome.MergeQueueOffered, "detached": outcome.DetachedCount,
			"rolled_back": outcome.RolledBack, "rollback_refused": outcome.RollbackRefused != nil})
	resp.Result = &agentreplv1.InterruptResponse_Success{Success: success}
	return connect.NewResponse(resp), nil
}

// rolledBackHeld answers a turn stop that took back the held prompt HELD
// rather than stopping the turn: its words return to the composer exactly as
// a rolled-back sent prompt's do (InterruptRolledBackUnanswered).
func (s *server) rolledBackHeld(log dlog.Logger, held wsm.HeldPrompt, resp *agentreplv1.InterruptResponse) (*connect.Response[agentreplv1.InterruptResponse], error) {
	log.Debug("daemon.server.interrupt", "answered that the stop took back a held prompt and left the turn running",
		dlog.Context{"turn": string(held.Turn)})
	resp.Result = &agentreplv1.InterruptResponse_Success{Success: &agentreplv1.InterruptSuccess{
		Outcome: &agentreplv1.InterruptSuccess_RolledBackUnanswered{
			RolledBackUnanswered: &agentreplv1.InterruptRolledBackUnanswered{Prompt: replyquote.Words(held.Said)},
		},
	}}
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
	subject, ref, refusedWith, refused, err := s.askRow(ctx, rpc, req.Msg.GetWorkspace(), req.Msg.GetPermission(), resp)
	if err != nil {
		return nil, err
	}
	if refused {
		return answer(resp, refusedWith)
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
		decision.Decision = workspace.DeniedByUser(req.Msg.GetDeny().GetReason().GetText())
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
	subject, ref, refusedWith, refused, err := s.askRow(ctx, rpc, req.Msg.GetWorkspace(), req.Msg.GetQuestion(), resp)
	if err != nil {
		return nil, err
	}
	if refused {
		return answer(resp, refusedWith)
	}

	selections := make([]*conversationv1.AgentQuestionSelection, 0, len(req.Msg.GetAnswers()))
	for _, given := range req.Msg.GetAnswers() {
		var other *string
		if given.OtherText != nil {
			text := given.GetOtherText().GetText()
			other = &text
		}
		selections = append(selections, feed.QuestionSelection(given.GetQuestionText(), given.GetChosen(), other))
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

// UpdateQuestionDraft applies one click to a standing question card's
// daemon-owned draft; the click that answers the batch's last question
// delivers it. Each arm echoes the served question text (and a pick its served
// label), which the verbs check against the batch.
func (s *server) UpdateQuestionDraft(
	ctx context.Context,
	req *connect.Request[agentreplv1.UpdateQuestionDraftRequest],
) (*connect.Response[agentreplv1.UpdateQuestionDraftResponse], error) {
	const rpc = "UpdateQuestionDraft"
	if err := validateUpdateQuestionDraftRequest(req.Msg); err != nil {
		return nil, err
	}
	resp := &agentreplv1.UpdateQuestionDraftResponse{}
	subject, ref, refusedWith, refused, err := s.askRow(ctx, rpc, req.Msg.GetWorkspace(), req.Msg.GetQuestion(), resp)
	if err != nil {
		return nil, err
	}
	if refused {
		return answer(resp, refusedWith)
	}

	var edit feed.QuestionEdit
	switch arm := req.Msg.GetEdit().(type) {
	case *agentreplv1.UpdateQuestionDraftRequest_Show:
		edit = feed.QuestionEdit{Kind: feed.EditShow, Question: arm.Show.GetQuestionText()}
	case *agentreplv1.UpdateQuestionDraftRequest_Pick:
		edit = feed.QuestionEdit{Kind: feed.EditPick, Question: arm.Pick.GetQuestionText(), Label: arm.Pick.GetLabel()}
	case *agentreplv1.UpdateQuestionDraftRequest_Confirm:
		edit = feed.QuestionEdit{Kind: feed.EditConfirm, Question: arm.Confirm.GetQuestionText()}
	default:
		// VALIDATED ABOVE: an unset arm never reaches here.
		panic(fmt.Sprintf("UpdateQuestionDraft: validated request carries edit %T", arm))
	}

	if err := s.deps.Verbs.UpdateQuestionDraft(ctx, subject.Record.ID, ref.Row.ID, edit); err != nil {
		// A value the batch never served is `unserved_value{text}`; the shim's
		// `no_open_ask` (the agent stopped asking before the delivery reached
		// it) is the card no longer standing.
		return answer(resp, s.answerRefusal(subject.Log, rpc, resp, err, map[string]string{
			workspace.ArmUnservedAnswer: "unserved_value",
			workspace.ArmShimNoOpenAsk:  "ask_not_standing",
		}))
	}
	resp.Result = &agentreplv1.UpdateQuestionDraftResponse_Success{
		Success: &agentreplv1.UpdateQuestionDraftSuccess{},
	}
	return connect.NewResponse(resp), nil
}

// AnswerColdGate resolves the standing cold gate with the request's choice:
// pay, clear, compact{model, scope} or compact_and_resume{model, scope}.
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

	if err := s.deps.Verbs.AnswerColdGate(ctx, subject.Record.ID, req.Msg); err != nil {
		return answer(resp, s.answerRefusal(subject.Log, rpc, resp, err, nil))
	}
	resp.Result = &agentreplv1.AnswerColdGateResponse_Success{
		Success: &agentreplv1.AnswerColdGateSuccess{},
	}
	return connect.NewResponse(resp), nil
}
