package server

import (
	"context"
	"fmt"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
)

// FoldMergeBubble records the reader's fold on a merge bubble. See
// endpoint_fold_merge_bubble.proto.
//
// THE READER'S FOLD IS DAEMON-HELD, on the bubble's durable head row, and it
// wins over the daemon's default (open, save a success) on every push, reload
// and restart (resolve/feed/mergefold.go).
func (s *server) FoldMergeBubble(
	ctx context.Context,
	req *connect.Request[agentreplv1.FoldMergeBubbleRequest],
) (*connect.Response[agentreplv1.FoldMergeBubbleResponse], error) {
	const rpc = "FoldMergeBubble"
	if err := validateFoldMergeBubbleRequest(req.Msg); err != nil {
		return nil, err
	}
	resp := &agentreplv1.FoldMergeBubbleResponse{}
	subject, cerr, done := s.subjectFor(ctx, rpc, req.Msg.GetWorkspace(), resp)
	if done {
		return answer(resp, cerr)
	}
	row := req.Msg.GetRow()
	folded := req.Msg.GetClose() != nil
	if !s.deps.Feed.SetMergeFold(subject.Record.ID, row, folded) {
		return answer(resp, s.refuse(subject.Log, rpc, resp, refusal{
			Arm:    "not_a_merge_bubble",
			Reason: fmt.Sprintf("row %q is not a merge bubble's head in this workspace's root feed", row.GetValue()),
			Fields: map[string]any{"row": row},
		}))
	}
	resp.Result = &agentreplv1.FoldMergeBubbleResponse_Success{Success: &agentreplv1.FoldMergeBubbleSuccess{}}
	return connect.NewResponse(resp), nil
}
