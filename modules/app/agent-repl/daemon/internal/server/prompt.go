package server

import (
	"context"
	"fmt"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/feedid"
	"claude-repld/internal/prompthandler"
	"claude-repld/internal/wsm"
)

// SubmitPrompt is the composer's submission, whole. The handler validates,
// resolves the workspace, decodes the addressed feed and DELEGATES to the
// prompt handler; recognition, mirroring and delivery all live there.
func (s *server) SubmitPrompt(
	ctx context.Context,
	req *connect.Request[agentreplv1.SubmitPromptRequest],
) (*connect.Response[agentreplv1.SubmitPromptResponse], error) {
	const rpc = "SubmitPrompt"
	if err := validateSubmitPromptRequest(req.Msg); err != nil {
		return nil, err
	}
	resp := &agentreplv1.SubmitPromptResponse{}
	subject, r, err := s.resolveRef(ctx, rpc, req.Msg.GetWorkspace())
	if err != nil {
		return nil, fail(s.log, rpc, err)
	}
	if r != nil {
		return answer(resp, s.refuse(s.log, rpc, resp, *r))
	}

	// The FeedId is decoded HERE, before the handler is called: a value that
	// does not decode never becomes a handler call, which is why the prompt
	// handler produces no feed_undecodable of its own.
	var target *feedid.Ref
	if req.Msg.Feed != nil {
		ref, decodeErr := feedid.Decode(req.Msg.GetFeed())
		if decodeErr != nil {
			return answer(resp, s.refuse(subject.Log, rpc, resp, s.fill(refusal{
				Arm: "feed_undecodable",
				Reason: fmt.Sprintf("the feed id %q does not decode: %v",
					req.Msg.GetFeed().GetValue(), decodeErr),
			})))
		}
		if ref.WS != subject.Record.ID {
			return answer(resp, s.refuse(subject.Log, rpc, resp, s.fill(refusal{
				Arm: "feed_not_in_workspace",
				Reason: fmt.Sprintf("the feed id %q belongs to workspace %q",
					req.Msg.GetFeed().GetValue(), ref.WS),
			})))
		}
		target = &ref
	}

	outcome, err := s.deps.Prompts.Submit(ctx, subject.Record.ID, req.Msg.GetSaid(),
		req.Msg.GetIdempotencyKey(), req.Msg.GetOrigin(), target)
	if err != nil {
		if refused, ok := s.asRefusal(err); ok {
			return answer(resp, s.refuse(subject.Log, rpc, resp, refused))
		}
		return nil, fail(subject.Log, rpc, err)
	}
	return answer(resp, s.encodeSubmitOutcome(subject.Log, resp, outcome))
}

// encodeSubmitOutcome renders the handler's outcome as SubmitPromptSuccess.
//
// A HOLD IS AN ANSWER: a prompt the queue parked still answers with the turn it
// minted, because the composer matches its own row against that turn id.
//
// A SESSION-ACT command (/clear, /compact, /model <arg>) has NO success arm
// yet: `SubmitPromptSuccess.command_acted` is accepted for landing 6 (project
// lead), and until it lands an acted command answers the LOUD unlanded-arm
// sentinel naming it — never a fabricated turn and never a fabricated panel.
func (s *server) encodeSubmitOutcome(
	log dlog.Logger,
	resp *agentreplv1.SubmitPromptResponse,
	outcome prompthandler.Outcome,
) *connect.Error {
	switch outcome.Recognition {
	case prompthandler.RecognizedPanel:
		log.Debug("daemon.server.submit_prompt", "answered a recognized command panel", nil)
		resp.Result = &agentreplv1.SubmitPromptResponse_Success{
			Success: &agentreplv1.SubmitPromptSuccess{
				Outcome: &agentreplv1.SubmitPromptSuccess_CommandPanel{CommandPanel: outcome.Panel},
			},
		}
		return nil
	case prompthandler.RecognizedRefused:
		log.Debug("daemon.server.submit_prompt", "answered a recognized command refusal",
			dlog.Context{"command": outcome.RefusedCommand})
		resp.Result = &agentreplv1.SubmitPromptResponse_Success{
			Success: &agentreplv1.SubmitPromptSuccess{
				Outcome: &agentreplv1.SubmitPromptSuccess_CommandRefused{
					CommandRefused: &agentreplv1.SubmitPromptCommandRefused{
						Command: outcome.RefusedCommand,
					},
				},
			},
		}
		return nil
	case prompthandler.RecognizedAct:
		return UnlandedArm(log, "SubmitPrompt", "command_acted",
			fmt.Sprintf("the daemon acted on session command %q; the success arm lands in landing 6",
				outcome.Act.Kind), false)
	default:
		if arm := outcome.Disposition.RefusedArm; arm != "" {
			return s.refuse(log, "SubmitPrompt", resp, s.fill(refusal{
				Arm:    arm,
				Reason: "the delivery path refused the submission",
			}))
		}
		log.Debug("daemon.server.submit_prompt", "answered the minted turn",
			dlog.Context{"turn": string(outcome.Turn), "parked": outcome.Disposition.Parked()})
		resp.Result = &agentreplv1.SubmitPromptResponse_Success{
			Success: &agentreplv1.SubmitPromptSuccess{
				Outcome: &agentreplv1.SubmitPromptSuccess_Turn{
					Turn: &agentreplv1.SubmitPromptTurn{
						Turn: &conversationv1.TurnId{Value: string(outcome.Turn)},
					},
				},
			},
		}
		return nil
	}
}

// RequestCommandSupport spawns a support workspace from the daemon-composed
// add-support brief. The brief's absence is LOUD, never papered over.
func (s *server) RequestCommandSupport(
	ctx context.Context,
	req *connect.Request[agentreplv1.RequestCommandSupportRequest],
) (*connect.Response[agentreplv1.RequestCommandSupportResponse], error) {
	const rpc = "RequestCommandSupport"
	if err := validateRequestCommandSupportRequest(req.Msg); err != nil {
		return nil, err
	}
	resp := &agentreplv1.RequestCommandSupportResponse{}
	subject, r, err := s.resolveRef(ctx, rpc, req.Msg.GetWorkspace())
	if err != nil {
		return nil, fail(s.log, rpc, err)
	}
	if r != nil {
		return answer(resp, s.refuse(s.log, rpc, resp, *r))
	}
	created, err := s.deps.Verbs.RequestCommandSupport(ctx, subject.Record.ID, req.Msg.GetCommand())
	if err != nil {
		if refused, ok := s.asRefusal(err); ok {
			return answer(resp, s.refuse(subject.Log, rpc, resp, refused))
		}
		return nil, fail(subject.Log, rpc, err)
	}
	subject.Log.Info("daemon.server.request_command_support", "spawned a support workspace",
		dlog.Context{"command": req.Msg.GetCommand(), "created": string(created.ID)})
	resp.Result = &agentreplv1.RequestCommandSupportResponse_Success{
		Success: &agentreplv1.RequestCommandSupportSuccess{Workspace: refOf(created)},
	}
	return connect.NewResponse(resp), nil
}

// refOf renders a registry record as the ref clients echo back.
func refOf(record wsm.Workspace) *workspacev1.WorkspaceRef {
	return &workspacev1.WorkspaceRef{Id: string(record.ID), Dir: record.Dir}
}
