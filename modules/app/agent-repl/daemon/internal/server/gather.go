package server

import (
	"context"
	"errors"
	"fmt"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/gather"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// Gathers runs workspace gathers (internal/gather).
type Gathers interface {
	// Create makes a gather and answers it.
	Create(ctx context.Context, req gather.Request) (*agentreplv1.GatherWorkspacesResponse, error)
	// Status answers one gather as it stands now.
	Status(ctx context.Context, id wsm.GatherID) (*agentreplv1.GetGatherStatusResponse, error)
}

// GatherWorkspaces asks every named workspace the same question on the
// caller's behalf. Each target is prompted through SubmitPrompt itself, as an
// agent-sent prompt from the caller, so a target meets exactly the refusals
// `workspace prompt` meets and both feeds draw the send.
func (s *server) GatherWorkspaces(
	ctx context.Context,
	req *connect.Request[agentreplv1.GatherWorkspacesRequest],
) (*connect.Response[agentreplv1.GatherWorkspacesResponse], error) {
	if err := validateGatherWorkspacesRequest(req.Msg); err != nil {
		return nil, err
	}
	msg := req.Msg
	caller := msg.GetCaller()
	submit := func(ctx context.Context, target wsm.Workspace, key string) (ids.TurnID, *agentreplv1.SubmitPromptError, error) {
		resp, err := s.SubmitPrompt(ctx, connect.NewRequest(&agentreplv1.SubmitPromptRequest{
			Workspace: target.Ref(), Said: msg.GetSaid(), IdempotencyKey: key,
			Origin: conversationv1.PromptOrigin_PROMPT_ORIGIN_AGENT_SENT, SentBy: caller, AsUser: msg.GetAsUser(),
		}))
		if err != nil {
			return "", nil, err
		}
		if refused := resp.Msg.GetError(); refused != nil {
			return "", refused, nil
		}
		turn := resp.Msg.GetSuccess().GetTurn().GetTurn().GetValue()
		if turn == "" {
			// AN AGENT'S PROMPT ALWAYS MINTS A TURN: it is never a command
			// (refused slash_command_from_agent), never a standing ask's
			// answer, never a revision.
			return "", nil, fmt.Errorf("an agent-sent prompt to %q was accepted as %T, not a turn", target.Name, resp.Msg.GetSuccess().GetOutcome())
		}
		return ids.TurnID(turn), nil, nil
	}
	resp, err := s.deps.Gathers.Create(ctx, gather.Request{
		Caller: ids.WorkspaceID(caller.GetId()), Names: msg.GetTargetNames(), PromptText: saidText(msg.GetSaid()),
		Key: msg.GetIdempotencyKey(), Submit: submit,
	})
	if err != nil {
		return nil, gatherFailure(err)
	}
	return connect.NewResponse(resp), nil
}

// GetGatherStatus answers one gather as it stands now.
func (s *server) GetGatherStatus(
	ctx context.Context,
	req *connect.Request[agentreplv1.GetGatherStatusRequest],
) (*connect.Response[agentreplv1.GetGatherStatusResponse], error) {
	if err := validateGetGatherStatusRequest(req.Msg); err != nil {
		return nil, err
	}
	resp, err := s.deps.Gathers.Status(ctx, wsm.GatherID(req.Msg.GetGather().GetId()))
	if err != nil {
		return nil, gatherFailure(err)
	}
	return connect.NewResponse(resp), nil
}

// gatherFailure answers a failure the gather service already recorded: a
// caller that went away is cancelled, a service that stopped is unavailable,
// and anything else is internal.
func gatherFailure(err error) *connect.Error {
	switch {
	case errors.Is(err, context.Canceled), errors.Is(err, context.DeadlineExceeded):
		return connect.NewError(connect.CodeCanceled, err)
	case errors.Is(err, gather.ErrStopped):
		return connect.NewError(connect.CodeUnavailable, err)
	}
	return connect.NewError(connect.CodeInternal, err)
}
