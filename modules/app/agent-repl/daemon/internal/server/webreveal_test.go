package server

import (
	"context"
	"testing"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
)

// openWeb opens the test workspace's web stream for as long as the test runs.
func openWeb(t *testing.T, h *harness) *connect.ServerStreamForClient[agentreplv1.WatchWebWorkspaceResponse] {
	t.Helper()
	ctx, cancel := context.WithCancel(context.Background())
	t.Cleanup(cancel)
	stream, err := h.Client.WatchWebWorkspace(ctx,
		connect.NewRequest(&agentreplv1.WatchWebWorkspaceRequest{Workspace: ref(), WebappBuild: "webapp-test"}))
	if err != nil {
		t.Fatalf("open the web stream: %v", err)
	}
	return stream
}

// selectRevealing selects the test workspace, naming ROW to reveal ("" names none).
func selectRevealing(t *testing.T, h *harness, row string) {
	t.Helper()
	req := &agentreplv1.SelectWorkspaceRequest{Workspace: ref()}
	if row != "" {
		req.Reveal = &frontendv1.FeedId{Value: row}
	}
	if _, err := h.Client.SelectWorkspace(context.Background(), connect.NewRequest(req)); err != nil {
		t.Fatalf("SelectWorkspace: %v", err)
	}
}

func TestASelectionNamingARowRevealsItOnThePage(t *testing.T) {
	tests := []struct {
		name          string
		attachedFirst bool
	}{
		{name: "a page that listens", attachedFirst: true},
		{name: "a page that attaches after the selection", attachedFirst: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			var stream *connect.ServerStreamForClient[agentreplv1.WatchWebWorkspaceResponse]
			if tt.attachedFirst {
				stream = openWeb(t, h)
			}

			// Act.
			selectRevealing(t, h, "row-merge-head")
			if !tt.attachedFirst {
				stream = openWeb(t, h)
			}

			// Assert.
			if got := receiveWebEvent(t, stream).GetRevealRow().GetRow().GetValue(); got != "row-merge-head" {
				t.Fatalf("revealed %q, want the selection's row", got)
			}
		})
	}
}

func TestARevealIsSentOnce(t *testing.T) {
	// Arrange: a page took the reveal asked before it attached.
	h := newHarness(t)
	selectRevealing(t, h, "row-merge-head")
	first := openWeb(t, h)
	if receiveWebEvent(t, first).GetRevealRow() == nil {
		t.Fatal("the first page was not handed the reveal")
	}
	later := openWeb(t, h)

	// Act.
	h.Server.PushTransferred(testWorkspaceID, "127.0.0.1:4242")

	// Assert.
	if push := receiveWebEvent(t, later); push.GetTransferred() == nil {
		t.Fatalf("a later page was handed %v, want no second reveal before the next event", push)
	}
}

func TestASelectionNamingNoRowRevealsNothing(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	selectRevealing(t, h, "")
	stream := openWeb(t, h)

	// Act.
	h.Server.PushTransferred(testWorkspaceID, "127.0.0.1:4242")

	// Assert.
	if push := receiveWebEvent(t, stream); push.GetTransferred() == nil {
		t.Fatalf("the page was handed %v, want no reveal", push)
	}
}
