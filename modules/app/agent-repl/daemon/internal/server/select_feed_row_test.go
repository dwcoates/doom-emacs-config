package server

import (
	"context"
	"testing"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"
)

// feedIDs renders values as a final-response slice, oldest first.
func feedIDs(values ...string) []*frontendv1.FeedId {
	out := make([]*frontendv1.FeedId, len(values))
	for i, v := range values {
		out[i] = &frontendv1.FeedId{Value: v}
	}
	return out
}

// TestNextSelection is the whole cursor state machine: prev/next start at the
// most recent, wrap at each end, clear and the empty set answer none, and a
// stale cursor restarts at the most recent.
func TestNextSelection(t *testing.T) {
	const (
		prev  = agentreplv1.SelectResponseDirection_SELECT_RESPONSE_DIRECTION_PREV
		next  = agentreplv1.SelectResponseDirection_SELECT_RESPONSE_DIRECTION_NEXT
		clear = agentreplv1.SelectResponseDirection_SELECT_RESPONSE_DIRECTION_CLEAR
	)
	tests := []struct {
		name      string
		finals    []*frontendv1.FeedId
		current   string
		direction agentreplv1.SelectResponseDirection
		want      string
	}{
		{name: "next from none starts at most recent", finals: feedIDs("a", "b", "c"), current: "", direction: next, want: "c"},
		{name: "prev from none starts at most recent", finals: feedIDs("a", "b", "c"), current: "", direction: prev, want: "c"},
		{name: "prev walks older", finals: feedIDs("a", "b", "c"), current: "c", direction: prev, want: "b"},
		{name: "next walks newer", finals: feedIDs("a", "b", "c"), current: "a", direction: next, want: "b"},
		{name: "prev wraps past the oldest to the newest", finals: feedIDs("a", "b", "c"), current: "a", direction: prev, want: "c"},
		{name: "next wraps past the newest to the oldest", finals: feedIDs("a", "b", "c"), current: "c", direction: next, want: "a"},
		{name: "clear answers none", finals: feedIDs("a", "b", "c"), current: "b", direction: clear, want: ""},
		{name: "prev on the empty set answers none", finals: nil, current: "", direction: prev, want: ""},
		{name: "next on the empty set answers none", finals: nil, current: "", direction: next, want: ""},
		{name: "stale cursor restarts at the most recent", finals: feedIDs("a", "b", "c"), current: "gone", direction: prev, want: "c"},
		{name: "single row prev wraps to itself", finals: feedIDs("a"), current: "a", direction: prev, want: "a"},
		{name: "single row next wraps to itself", finals: feedIDs("a"), current: "a", direction: next, want: "a"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			var current *frontendv1.FeedId
			if tc.current != "" {
				current = &frontendv1.FeedId{Value: tc.current}
			}

			// Act.
			got := nextSelection(tc.finals, current, tc.direction)

			// Assert.
			if got.GetValue() != tc.want {
				t.Fatalf("nextSelection = %q, want %q", got.GetValue(), tc.want)
			}
		})
	}
}

// TestSelectResponseAcksTheSelectedFeedid pins that the handler answers the row
// the direction lands on — the most recent when nothing was selected.
func TestSelectResponseAcksTheSelectedFeedid(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Feed.finals = feedIDs("a", "b", "c")

	// Act.
	resp, err := h.Client.SelectResponse(context.Background(),
		connect.NewRequest(&agentreplv1.SelectResponseRequest{
			Workspace: ref(),
			Direction: agentreplv1.SelectResponseDirection_SELECT_RESPONSE_DIRECTION_NEXT,
		}))

	// Assert.
	if err != nil {
		t.Fatalf("SelectResponse: %v", err)
	}
	if got := resp.Msg.GetSuccess().GetSelected().GetValue(); got != "c" {
		t.Fatalf("selected = %q, want the most recent c", got)
	}
}

// TestSelectResponseClearAnswersNone pins that CLEAR drops the selection and
// answers none.
func TestSelectResponseClearAnswersNone(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Feed.finals = feedIDs("a", "b", "c")
	if _, err := h.Client.SelectResponse(context.Background(),
		connect.NewRequest(&agentreplv1.SelectResponseRequest{
			Workspace: ref(),
			Direction: agentreplv1.SelectResponseDirection_SELECT_RESPONSE_DIRECTION_PREV,
		})); err != nil {
		t.Fatalf("seed selection: %v", err)
	}

	// Act.
	resp, err := h.Client.SelectResponse(context.Background(),
		connect.NewRequest(&agentreplv1.SelectResponseRequest{
			Workspace: ref(),
			Direction: agentreplv1.SelectResponseDirection_SELECT_RESPONSE_DIRECTION_CLEAR,
		}))

	// Assert.
	if err != nil {
		t.Fatalf("SelectResponse: %v", err)
	}
	if resp.Msg.GetSuccess() == nil {
		t.Fatalf("result = %v, want success", resp.Msg.GetResult())
	}
	if resp.Msg.GetSuccess().Selected != nil {
		t.Fatalf("selected = %v, want none after a clear", resp.Msg.GetSuccess().GetSelected())
	}
}

// TestSelectResponseEmptySetIsANoOpNotAnError pins that PREV/NEXT with zero
// final responses answer none rather than refusing.
func TestSelectResponseEmptySetIsANoOpNotAnError(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Feed.finals = nil

	// Act.
	resp, err := h.Client.SelectResponse(context.Background(),
		connect.NewRequest(&agentreplv1.SelectResponseRequest{
			Workspace: ref(),
			Direction: agentreplv1.SelectResponseDirection_SELECT_RESPONSE_DIRECTION_NEXT,
		}))

	// Assert.
	if err != nil {
		t.Fatalf("SelectResponse: %v", err)
	}
	if resp.Msg.GetSuccess() == nil || resp.Msg.GetSuccess().Selected != nil {
		t.Fatalf("result = %v, want success with no selection", resp.Msg.GetResult())
	}
}

// TestSelectResponseRefusesAnUnknownWorkspace pins that the ref refusals mirror
// SelectWorkspace: an unregistered id answers the unknown_workspace arm.
func TestSelectResponseRefusesAnUnknownWorkspace(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	resp, err := h.Client.SelectResponse(context.Background(),
		connect.NewRequest(&agentreplv1.SelectResponseRequest{
			Workspace: &workspacev1.WorkspaceRef{Id: "ws-nope"},
			Direction: agentreplv1.SelectResponseDirection_SELECT_RESPONSE_DIRECTION_NEXT,
		}))

	// Assert.
	if err != nil {
		t.Fatalf("SelectResponse: %v", err)
	}
	if resp.Msg.GetError().GetUnknownWorkspace() == nil {
		t.Fatalf("result = %v, want unknown_workspace", resp.Msg.GetResult())
	}
}

// TestSelectResponseRefusesAnUnspecifiedDirection pins that UNSPECIFIED is a
// validation failure (InvalidArgument), never a typed arm.
func TestSelectResponseRefusesAnUnspecifiedDirection(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	_, err := h.Client.SelectResponse(context.Background(),
		connect.NewRequest(&agentreplv1.SelectResponseRequest{
			Workspace: ref(),
			Direction: agentreplv1.SelectResponseDirection_SELECT_RESPONSE_DIRECTION_UNSPECIFIED,
		}))

	// Assert.
	if connectCode(t, err) != connect.CodeInvalidArgument {
		t.Fatalf("code = %v, want InvalidArgument", connectCode(t, err))
	}
}
