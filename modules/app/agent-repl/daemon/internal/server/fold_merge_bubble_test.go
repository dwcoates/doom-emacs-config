package server

import (
	"context"
	"testing"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
)

// foldMerge sends the reader's fold of ROW.
func foldMerge(t *testing.T, h *harness, row string, closed bool) (*connect.Response[agentreplv1.FoldMergeBubbleResponse], error) {
	t.Helper()
	req := &agentreplv1.FoldMergeBubbleRequest{Workspace: ref(), Row: &frontendv1.FeedId{Value: row}}
	if closed {
		req.Fold = &agentreplv1.FoldMergeBubbleRequest_Close{Close: &agentreplv1.FoldMergeBubbleClose{}}
	} else {
		req.Fold = &agentreplv1.FoldMergeBubbleRequest_Open{Open: &agentreplv1.FoldMergeBubbleOpen{}}
	}
	return h.Client.FoldMergeBubble(context.Background(), connect.NewRequest(req))
}

func TestFoldMergeBubbleRecordsTheReadersFold(t *testing.T) {
	tests := []struct {
		name   string
		closed bool
	}{
		{name: "the reader opened it", closed: false},
		{name: "the reader closed it", closed: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			h.Feed.mergeHeads = map[string]bool{"merge-head": true}

			// Act
			resp, err := foldMerge(t, h, "merge-head", tt.closed)

			// Assert
			if err != nil || resp.Msg.GetSuccess() == nil {
				t.Fatalf("FoldMergeBubble = (%v, %v), want success", resp, err)
			}
			if got, ok := h.Feed.mergeFolds["merge-head"]; !ok || got != tt.closed {
				t.Fatalf("recorded fold = (%v, %v), want folded %v", got, ok, tt.closed)
			}
		})
	}
}

func TestFoldMergeBubbleRefusesARowThatIsNotAMergeBubble(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	resp, err := foldMerge(t, h, "final-1", false)

	// Assert
	if err != nil || resp.Msg.GetError().GetNotAMergeBubble().GetRow().GetValue() != "final-1" {
		t.Fatalf("FoldMergeBubble = (%v, %v), want not_a_merge_bubble echoing final-1", resp, err)
	}
}

func TestFoldMergeBubbleRefusesARequestWithNoFold(t *testing.T) {
	// Arrange
	h := newHarness(t)
	req := &agentreplv1.FoldMergeBubbleRequest{Workspace: ref(), Row: &frontendv1.FeedId{Value: "merge-head"}}

	// Act
	_, err := h.Client.FoldMergeBubble(context.Background(), connect.NewRequest(req))

	// Assert
	if connect.CodeOf(err) != connect.CodeInvalidArgument {
		t.Fatalf("FoldMergeBubble with no fold = %v, want InvalidArgument", err)
	}
}
