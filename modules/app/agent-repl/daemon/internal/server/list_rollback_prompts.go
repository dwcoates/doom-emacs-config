package server

import (
	"context"
	"errors"
	"slices"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/wsm"
)

// ListRollbackPrompts answers the prompts a rollback can reach, newest first,
// each with when it was sent and how full the context was just before it. See
// endpoint_list_rollback_prompts.proto.
func (s *server) ListRollbackPrompts(
	ctx context.Context,
	req *connect.Request[agentreplv1.ListRollbackPromptsRequest],
) (*connect.Response[agentreplv1.ListRollbackPromptsResponse], error) {
	const rpc = "ListRollbackPrompts"
	if err := validateWorkspaceRef("workspace", req.Msg.GetWorkspace()); err != nil {
		return nil, err
	}
	resp := &agentreplv1.ListRollbackPromptsResponse{}
	subject, cerr, done := s.subjectFor(ctx, rpc, req.Msg.GetWorkspace(), resp)
	if done {
		return answer(resp, cerr)
	}
	ws := subject.Record.ID
	choices := s.deps.Feed.RollbackChoices(ws)
	success := &agentreplv1.ListRollbackPromptsSuccess{}
	for _, choice := range slices.Backward(choices) {
		entry := &agentreplv1.RollbackPromptEntry{
			Prompt:        choice.Row,
			Excerpt:       excerptOf(choice.Said),
			ContextTokens: choice.ContextTokens,
		}
		turn, err := s.deps.DB.Turn(ctx, ws, choice.Turn)
		switch {
		case errors.Is(err, wsm.ErrNotFound):
			// A PROMPT THE WORKSPACE NEVER RECORDED SENDING (adopted from the
			// vendor's transcript) has no send time to state.
		case err != nil:
			return nil, fail(subject.Log, rpc, err)
		default:
			sent := turn.StartedAt.UnixMilli()
			entry.SentAtMs = &sent
		}
		success.Prompts = append(success.Prompts, entry)
	}
	subject.Log.Info(opRollback, "listed the prompts a rollback can reach for the reader to choose among",
		dlog.Context{"prompts": len(success.Prompts)})
	resp.Result = &agentreplv1.ListRollbackPromptsResponse_Success{Success: success}
	return connect.NewResponse(resp), nil
}
