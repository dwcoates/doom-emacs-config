package server

import (
	"context"
	"errors"
	"fmt"
	"testing"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"claude-repld/internal/feedid"
	"claude-repld/internal/resolve/feed"
)

// loadThrough calls LoadFeedThrough for TARGET and answers every frame.
func (h *harness) loadThrough(t *testing.T, target *frontendv1.FeedId) []*agentreplv1.LoadFeedThroughResponse {
	t.Helper()
	stream, err := h.Client.LoadFeedThrough(context.Background(), connect.NewRequest(&agentreplv1.LoadFeedThroughRequest{
		Workspace: ref(), Target: target,
	}))
	if err != nil {
		t.Fatalf("LoadFeedThrough: %v", err)
	}
	defer stream.Close()
	var frames []*agentreplv1.LoadFeedThroughResponse
	for stream.Receive() {
		frames = append(frames, stream.Msg())
	}
	if err := stream.Err(); err != nil {
		t.Fatalf("LoadFeedThrough stream: %v", err)
	}
	return frames
}

// onePage is a page carrying one row.
func onePage(row string) *frontendv1.FeedPage {
	return &frontendv1.FeedPage{Result: &frontendv1.FeedPage_Success{Success: &frontendv1.FeedPageSuccess{
		Rows: []*frontendv1.FeedRow{{Id: &frontendv1.FeedId{Value: row}}},
	}}}
}

func TestLoadFeedThroughStreamsEveryPageThenReached(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Feed.throughPages = []*frontendv1.FeedPage{onePage("r-2"), onePage("r-1")}
	target := feedIDFor(testWorkspaceID)

	// Act.
	frames := h.loadThrough(t, target)

	// Assert.
	if len(frames) != 3 || frames[0].GetPage() == nil || frames[1].GetPage() == nil {
		t.Fatalf("frames = %v, want two pages then the terminal", frames)
	}
	if got := frames[2].GetReached().GetTarget().GetValue(); got != target.GetValue() {
		t.Fatalf("reached = %q, want the target", got)
	}
}

func TestLoadFeedThroughWalksTheReadersRootWalk(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	h.loadThrough(t, feedIDFor(testWorkspaceID))

	// Assert: the walk is keyed exactly as GetFeedPage keys a root-feed walk.
	if got := string(h.Feed.throughReader); got == "" || got[len(got)-1] != '|' {
		t.Fatalf("reader = %q, want the root-feed reader key", got)
	}
}

func TestLoadFeedThroughNotFoundEndsWithTheArm(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Feed.throughPages = []*frontendv1.FeedPage{onePage("r-1")}
	h.Feed.throughErr = feed.ErrTargetNotFound

	// Act.
	frames := h.loadThrough(t, feedIDFor(testWorkspaceID))

	// Assert: the page streamed stays, then the terminal.
	if len(frames) != 2 || frames[1].GetError().GetNotFound() == nil {
		t.Fatalf("frames = %v, want the page then not_found", frames)
	}
}

func TestLoadFeedThroughNotFoundIsAFooterFaultLine(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Feed.throughErr = feed.ErrTargetNotFound

	// Act.
	h.loadThrough(t, feedIDFor(testWorkspaceID))

	// Assert: non-escalating, so a transient line.
	faults := h.Footer.openedFaults()
	if len(faults) != 1 || faults[0].Kind != faultFeedEntryNotFound || faults[0].Status != "" {
		t.Fatalf("footer faults = %+v, want one transient feed_entry_not_found", faults)
	}
}

func TestLoadFeedThroughHistoryUnavailableCarriesTheDetail(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Feed.throughErr = fmt.Errorf("%w: store unreachable", feed.ErrHistoryUnavailable)

	// Act.
	frames := h.loadThrough(t, feedIDFor(testWorkspaceID))

	// Assert.
	last := frames[len(frames)-1].GetError().GetHistoryUnavailable()
	if last == nil || last.GetDetail() == "" {
		t.Fatalf("frames = %v, want history_unavailable with its detail", frames)
	}
}

func TestLoadFeedThroughHistoryUnavailableIsAFooterFaultLine(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Feed.throughErr = fmt.Errorf("%w: store unreachable", feed.ErrHistoryUnavailable)

	// Act.
	h.loadThrough(t, feedIDFor(testWorkspaceID))

	// Assert.
	faults := h.Footer.openedFaults()
	if len(faults) != 1 || faults[0].Kind != faultFeedHistoryUnavailable {
		t.Fatalf("footer faults = %+v, want one feed_history_unavailable", faults)
	}
}

func TestLoadFeedThroughRefusesATargetOffTheRootFeed(t *testing.T) {
	tests := []struct {
		name   string
		target *frontendv1.FeedId
	}{
		{name: "undecodable", target: &frontendv1.FeedId{Value: "not-a-feed-id"}},
		{name: "another workspace's row", target: feedIDFor("ws-other")},
		{name: "a sub-feed's row", target: feedid.Encode(feedid.Ref{
			WS:   testWorkspaceID,
			Feed: feedid.Feed{Agent: &conversationv1.AgentId{Value: "sub-1"}},
			Row:  feedid.RowKey{Kind: feedid.KindPrompt, ID: "row-1"},
		})},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)

			// Act.
			frames := h.loadThrough(t, tt.target)

			// Assert.
			if len(frames) != 1 || frames[0].GetError().GetTargetUndecodable() == nil {
				t.Fatalf("frames = %v, want target_undecodable", frames)
			}
		})
	}
}

func TestLoadFeedThroughRefusesAnUnknownWorkspace(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	stream, err := h.Client.LoadFeedThrough(context.Background(), connect.NewRequest(&agentreplv1.LoadFeedThroughRequest{
		Workspace: &workspacev1.WorkspaceRef{Id: "ws-unknown", Dir: "/tmp/unknown"}, Target: feedIDFor("ws-unknown"),
	}))
	if err != nil {
		t.Fatalf("LoadFeedThrough: %v", err)
	}
	defer stream.Close()
	var last *agentreplv1.LoadFeedThroughResponse
	for stream.Receive() {
		last = stream.Msg()
	}

	// Assert.
	if last.GetError().GetUnknownWorkspace() == nil {
		t.Fatalf("terminal = %v (err %v), want unknown_workspace", last, stream.Err())
	}
}

func TestLoadFeedThroughValidatesTheTarget(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	stream, err := h.Client.LoadFeedThrough(context.Background(), connect.NewRequest(&agentreplv1.LoadFeedThroughRequest{Workspace: ref()}))
	if err == nil {
		for stream.Receive() {
		}
		err = stream.Err()
		stream.Close()
	}

	// Assert.
	if connect.CodeOf(err) != connect.CodeInvalidArgument {
		t.Fatalf("err = %v, want InvalidArgument", err)
	}
}

func TestLoadFeedThroughOtherFailureIsATransportError(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Feed.throughErr = errors.New("the reader went away")

	// Act.
	stream, err := h.Client.LoadFeedThrough(context.Background(), connect.NewRequest(&agentreplv1.LoadFeedThroughRequest{
		Workspace: ref(), Target: feedIDFor(testWorkspaceID),
	}))
	if err == nil {
		for stream.Receive() {
		}
		err = stream.Err()
		stream.Close()
	}

	// Assert.
	if err == nil {
		t.Fatal("LoadFeedThrough ended cleanly, want the failure surfaced")
	}
}
