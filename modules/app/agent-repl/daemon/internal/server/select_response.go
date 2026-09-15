package server

import (
	"context"
	"errors"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
)

// SelectResponse moves or clears the per-workspace response-selection cursor
// (reply-to-a-past-response mode). See endpoint_select_response.proto.
//
// PROTO FOUNDATION STUB. The wire contract is landed and the daemon builds
// against it, but the selection state machine — ordering the final-response
// rows, computing prev/next with wrap, pushing FeedSelection on the feed
// watch, and clearing on double-escape — is fanned out separately. Until that
// lands the handler answers Unimplemented rather than fake a selection; it is
// NOT an error branch to weaken but the honest answer for an unbuilt verb.
func (s *server) SelectResponse(
	ctx context.Context,
	req *connect.Request[agentreplv1.SelectResponseRequest],
) (*connect.Response[agentreplv1.SelectResponseResponse], error) {
	return nil, connect.NewError(connect.CodeUnimplemented, errors.New("SelectResponse is not implemented yet"))
}
