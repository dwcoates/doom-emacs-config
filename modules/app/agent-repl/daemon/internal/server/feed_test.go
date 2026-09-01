package server

import (
	"context"
	"strings"
	"testing"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/resolve/feed"
)

// page is one answered feed page.
func page() *frontendv1.FeedPage {
	return &frontendv1.FeedPage{
		Result: &frontendv1.FeedPage_Success{
			Success: &frontendv1.FeedPageSuccess{
				Edge: &frontendv1.FeedPageSuccess_AtStart{AtStart: &frontendv1.FeedPageAtStart{}},
			},
		},
	}
}

// TestOpenFeedMintsTheWatchToken pins that OpenFeed answers the newest page AND
// the token WatchFeed echoes: the pin is what keeps a row from falling between
// the page and the stream.
func TestOpenFeedMintsTheWatchToken(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Feed.page = page()
	h.Feed.token = &agentreplv1.FeedWatchToken{Value: "ft-1"}

	// Act.
	resp, err := h.Client.OpenFeed(context.Background(),
		connect.NewRequest(&agentreplv1.OpenFeedRequest{Workspace: ref()}))

	// Assert.
	if err != nil {
		t.Fatalf("OpenFeed: %v", err)
	}
	if got := resp.Msg.GetSuccess().GetWatch().GetValue(); got != "ft-1" {
		t.Fatalf("token = %q, want ft-1", got)
	}
}

// TestOpenFeedDefaultsToTheRootFeed pins that an UNSET feed id addresses the
// root feed, which is the only feed every workspace always has.
func TestOpenFeedDefaultsToTheRootFeed(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Feed.page = page()
	h.Feed.token = &agentreplv1.FeedWatchToken{Value: "ft-1"}

	// Act.
	if _, err := h.Client.OpenFeed(context.Background(),
		connect.NewRequest(&agentreplv1.OpenFeedRequest{Workspace: ref()})); err != nil {
		t.Fatalf("OpenFeed: %v", err)
	}

	// Assert.
	if !h.Feed.lastFeed.Root {
		t.Fatalf("feed = %+v, want the root feed", h.Feed.lastFeed)
	}
}

// TestOpenFeedRefusesAForeignFeed pins that a feed id belonging to another
// workspace is refused rather than served.
func TestOpenFeedRefusesAForeignFeed(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	resp, err := h.Client.OpenFeed(context.Background(),
		connect.NewRequest(&agentreplv1.OpenFeedRequest{
			Workspace: ref(),
			Feed:      feedAddressFor("ws-other"),
		}))

	// Assert.
	if err != nil {
		t.Fatalf("OpenFeed: %v", err)
	}
	if resp.Msg.GetError().GetFeedNotInWorkspace() == nil {
		t.Fatalf("result = %v, want feed_not_in_workspace", resp.Msg.GetResult())
	}
}

// TestGetFeedPageNextWithNoWalkIsRefused pins that the page walk is EPHEMERAL
// and per-connection: a `next` with no walk standing is refused rather than
// silently restarted from the top.
func TestGetFeedPageNextWithNoWalkIsRefused(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Feed.nextErr = feed.ErrNoWalk

	// Act.
	resp, err := h.Client.GetFeedPage(context.Background(),
		connect.NewRequest(&agentreplv1.GetFeedPageRequest{
			Workspace: ref(),
			Page:      &agentreplv1.GetFeedPageRequest_Next{Next: &agentreplv1.GetFeedPageNext{}},
		}))

	// Assert.
	if err != nil {
		t.Fatalf("GetFeedPage: %v", err)
	}
	if resp.Msg.GetError().GetNoWalkStanding() == nil {
		t.Fatalf("result = %v, want no_walk_standing", resp.Msg.GetResult())
	}
}

// TestGetFeedPageFirstOpensTheWalk pins that `first` opens the per-connection
// walk through the resolver's OpenPage.
func TestGetFeedPageFirstOpensTheWalk(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Feed.page = page()
	h.Feed.token = &agentreplv1.FeedWatchToken{Value: "ft-1"}

	// Act.
	resp, err := h.Client.GetFeedPage(context.Background(),
		connect.NewRequest(&agentreplv1.GetFeedPageRequest{
			Workspace: ref(),
			Page:      &agentreplv1.GetFeedPageRequest_First{First: &agentreplv1.GetFeedPageFirst{}},
		}))

	// Assert.
	if err != nil {
		t.Fatalf("GetFeedPage: %v", err)
	}
	if resp.Msg.GetSuccess() == nil || h.Feed.openCalls != 1 {
		t.Fatalf("first opened %d walks and answered %v", h.Feed.openCalls, resp.Msg.GetResult())
	}
}

// TestWatchFeedRefusesAnUnmintedToken pins that a token this daemon never
// minted is refused before any frame.
func TestWatchFeedRefusesAnUnmintedToken(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	// Act.
	stream, err := h.Client.WatchFeed(ctx, connect.NewRequest(&agentreplv1.WatchFeedRequest{
		Watch: &agentreplv1.FeedWatchToken{Value: "ft-never"},
	}))
	if err == nil {
		stream.Receive()
		err = stream.Err()
	}

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "WatchFeedError.unknown_token") {
		t.Fatalf("error = %v, want the unknown_token sentinel", err)
	}
}

// TestWatchFeedTailsFromTheMintedToken pins that a token OpenFeed minted opens
// its feed's tail and delivers the rows.
func TestWatchFeedTailsFromTheMintedToken(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Feed.page = page()
	h.Feed.token = &agentreplv1.FeedWatchToken{Value: "ft-1"}
	h.Feed.tail = &fakeTail{rows: make(chan *frontendv1.FeedRow, 1), token: h.Feed.token}
	if _, err := h.Client.OpenFeed(context.Background(),
		connect.NewRequest(&agentreplv1.OpenFeedRequest{Workspace: ref()})); err != nil {
		t.Fatalf("OpenFeed: %v", err)
	}
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	// Act.
	stream, err := h.Client.WatchFeed(ctx, connect.NewRequest(&agentreplv1.WatchFeedRequest{
		Watch: &agentreplv1.FeedWatchToken{Value: "ft-1"},
	}))
	if err != nil {
		t.Fatalf("WatchFeed: %v", err)
	}
	h.Feed.tail.rows <- &frontendv1.FeedRow{Id: &frontendv1.FeedId{Value: "row-1"}}
	if !stream.Receive() {
		t.Fatalf("receive the row: %v", stream.Err())
	}

	// Assert.
	if got := stream.Msg().GetRow().GetId().GetValue(); got != "row-1" {
		t.Fatalf("row = %q, want row-1", got)
	}
}
