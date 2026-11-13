package kit

import (
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	storev1 "agentrepl/proto/store/v1"
	"agentrepl/proto/store/v1/storev1connect"
	"context"
	"fmt"

	"connectrpc.com/connect"
)

// EndsTurn matches TURN's FeedTurnEnded row.
func EndsTurn(turn *conversationv1.TurnId) func(*frontendv1.FeedRow) bool {
	return func(row *frontendv1.FeedRow) bool {
		return row.GetTurn().GetValue() == turn.GetValue() && row.GetTurnEnded() != nil
	}
}

// CursorIngested reports whether a cursor of the file at path has read to
// size or past it.
func CursorIngested(cursors []*storev1.CursorState, path string, size int64) bool {
	for _, c := range cursors {
		if c.GetPath() == path && c.GetOffset() >= size {
			return true
		}
	}
	return false
}

// HookRow answers the one FeedHook card in a feed snapshot, or nil if the
// feed drew none — which, per bubbles.go's drawHook, is the CORRECT outcome
// for a succeeded or a cancelled hook, not a gap in this test.
func HookRow(rows []*frontendv1.FeedRow) *frontendv1.FeedHook {
	if hooks := HookRows(rows); len(hooks) > 0 {
		return hooks[0]
	}
	return nil
}

// HookRows answers every FeedHook card in a feed snapshot, in feed order.
func HookRows(rows []*frontendv1.FeedRow) []*frontendv1.FeedHook {
	var hooks []*frontendv1.FeedHook
	for _, row := range rows {
		if hook := row.GetActivity().GetHook(); hook != nil {
			hooks = append(hooks, hook)
		}
	}
	return hooks
}

// ReadAgentBook answers AGENT's newest page straight from the store, the
// suite's one read-only door to a book for a fact no daemon surface carries
// (a file-plane-only record, a prompt an Emacs-layer world cannot ask the
// daemon about). A transport failure and a failure answer are both errors,
// so a caller that polls can retry and one that must not can fail.
func ReadAgentBook(ctx context.Context, client storev1connect.ShimStoreClient, agent *conversationv1.AgentId) (*storev1.AgentSessionPage, error) {
	resp, err := client.OpenAgentSession(ctx, connect.NewRequest(&storev1.OpenAgentSessionRequest{Agent: agent}))
	if err != nil {
		return nil, fmt.Errorf("OpenAgentSession(%s): %w", agent.GetValue(), err)
	}
	success := resp.Msg.GetSuccess()
	if success == nil {
		return nil, fmt.Errorf("OpenAgentSession(%s) = %v, want success", agent.GetValue(), resp.Msg)
	}
	return success.GetPage(), nil
}
