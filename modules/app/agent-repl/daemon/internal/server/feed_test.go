package server

import (
	"context"
	"strings"
	"testing"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/resolve/feed"
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

// TestOpenFeedCarriesTheResolversHeldSection pins that the opening answers the
// held section the resolver opened the root feed with.
func TestOpenFeedCarriesTheResolversHeldSection(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Feed.page = page()
	h.Feed.held = &frontendv1.FeedHeld{}

	// Act.
	resp, err := h.Client.OpenFeed(context.Background(),
		connect.NewRequest(&agentreplv1.OpenFeedRequest{Workspace: ref()}))

	// Assert.
	if err != nil {
		t.Fatalf("OpenFeed: %v", err)
	}
	if resp.Msg.GetSuccess().GetHeld() == nil {
		t.Fatalf("result = %v, want the held section carried", resp.Msg.GetResult())
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

// TestGetFeedPageFirstServesTheNewestPage pins that `first` is answered by the
// resolver's OpenPage.
func TestGetFeedPageFirstServesTheNewestPage(t *testing.T) {
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

// TestGetFeedPageOlderThanAsksAboveTheNamedRow pins that `older_than` asks the
// resolver for the page above exactly the oldest held row the reader names.
func TestGetFeedPageOlderThanAsksAboveTheNamedRow(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Feed.olderPage = page()

	// Act.
	resp, err := h.Client.GetFeedPage(context.Background(),
		connect.NewRequest(&agentreplv1.GetFeedPageRequest{
			Workspace: ref(),
			Page: &agentreplv1.GetFeedPageRequest_OlderThan{OlderThan: &agentreplv1.GetFeedPageOlderThan{
				OldestHeld: &frontendv1.FeedRowOrder{Key: "k-7"},
			}},
		}))

	// Assert.
	if err != nil || resp.Msg.GetSuccess() == nil {
		t.Fatalf("GetFeedPage{older_than} = %v, %v; want the older page", resp, err)
	}
	if len(h.Feed.olderKeys) != 1 || h.Feed.olderKeys[0] != "k-7" || h.Feed.openCalls != 0 {
		t.Fatalf("older asked above %v after %d openings, want exactly k-7 and no opening", h.Feed.olderKeys, h.Feed.openCalls)
	}
}

// TestGetFeedPageOlderThanWithoutAKeyIsInvalid pins that an older_than naming
// no oldest held row, or one without its key, is refused before the resolver.
func TestGetFeedPageOlderThanWithoutAKeyIsInvalid(t *testing.T) {
	tests := []struct {
		name  string
		older *agentreplv1.GetFeedPageOlderThan
	}{
		{name: "no oldest held row", older: &agentreplv1.GetFeedPageOlderThan{}},
		{name: "an oldest held row without its key", older: &agentreplv1.GetFeedPageOlderThan{OldestHeld: &frontendv1.FeedRowOrder{}}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)

			// Act.
			_, err := h.Client.GetFeedPage(context.Background(),
				connect.NewRequest(&agentreplv1.GetFeedPageRequest{
					Workspace: ref(),
					Page:      &agentreplv1.GetFeedPageRequest_OlderThan{OlderThan: tt.older},
				}))

			// Assert.
			if connect.CodeOf(err) != connect.CodeInvalidArgument || len(h.Feed.olderKeys) != 0 {
				t.Fatalf("err = %v after %d older asks, want InvalidArgument and none", err, len(h.Feed.olderKeys))
			}
		})
	}
}

// TestGetFeedPageRefusesAForeignFeed pins that a feed id belonging to another
// workspace is refused rather than walked.
func TestGetFeedPageRefusesAForeignFeed(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	resp, err := h.Client.GetFeedPage(context.Background(),
		connect.NewRequest(&agentreplv1.GetFeedPageRequest{
			Workspace: ref(),
			Feed:      feedAddressFor("ws-other"),
			Page:      &agentreplv1.GetFeedPageRequest_First{First: &agentreplv1.GetFeedPageFirst{}},
		}))

	// Assert.
	if err != nil {
		t.Fatalf("GetFeedPage: %v", err)
	}
	if resp.Msg.GetError().GetFeedNotInWorkspace() == nil || h.Feed.openCalls != 0 {
		t.Fatalf("result = %v after %d openings, want feed_not_in_workspace and no walk", resp.Msg.GetResult(), h.Feed.openCalls)
	}
}

// TestGetFeedPageRefusesAnUndecodableFeed pins that a feed id that decodes to
// no feed is refused in band rather than walked.
func TestGetFeedPageRefusesAnUndecodableFeed(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	resp, err := h.Client.GetFeedPage(context.Background(),
		connect.NewRequest(&agentreplv1.GetFeedPageRequest{
			Workspace: ref(),
			Feed:      &frontendv1.FeedId{Value: "not-a-feed-id"},
			Page:      &agentreplv1.GetFeedPageRequest_First{First: &agentreplv1.GetFeedPageFirst{}},
		}))

	// Assert.
	if err != nil {
		t.Fatalf("GetFeedPage: %v", err)
	}
	if resp.Msg.GetError().GetFeedUndecodable() == nil || h.Feed.openCalls != 0 {
		t.Fatalf("result = %v after %d openings, want feed_undecodable and no walk", resp.Msg.GetResult(), h.Feed.openCalls)
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
	err = firstStreamErr(stream, err)

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "WatchFeed closed the stream: unknown_token") {
		t.Fatalf("error = %v, want the transport-closed unknown_token cause", err)
	}
}

// TestWatchFeedTailsFromTheMintedToken pins that a token OpenFeed minted opens
// its feed's tail and delivers the rows.
func TestWatchFeedTailsFromTheMintedToken(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Feed.page = page()
	h.Feed.token = &agentreplv1.FeedWatchToken{Value: "ft-1"}
	h.Feed.tail = &fakeTail{pubs: make(chan feed.Publication, 1), token: h.Feed.token}
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
	h.Feed.tail.pubs <- feed.Publication{Row: &frontendv1.FeedRow{Id: &frontendv1.FeedId{Value: "row-1"}}}
	if !stream.Receive() {
		t.Fatalf("receive the row: %v", stream.Err())
	}

	// Assert.
	if got := stream.Msg().GetRow().GetId().GetValue(); got != "row-1" {
		t.Fatalf("row = %q, want row-1", got)
	}
}

// openRootWatch opens the root feed and its watch, returning the live stream —
// the shared arrange step for the selection-push tests.
func openRootWatch(t *testing.T, h *harness, ctx context.Context) *connect.ServerStreamForClient[agentreplv1.WatchFeedResponse] {
	t.Helper()
	h.Feed.page = page()
	h.Feed.token = &agentreplv1.FeedWatchToken{Value: "ft-1"}
	h.Feed.tail = &fakeTail{pubs: make(chan feed.Publication, 1), token: h.Feed.token}
	if _, err := h.Client.OpenFeed(context.Background(),
		connect.NewRequest(&agentreplv1.OpenFeedRequest{Workspace: ref()})); err != nil {
		t.Fatalf("OpenFeed: %v", err)
	}
	stream, err := h.Client.WatchFeed(ctx, connect.NewRequest(&agentreplv1.WatchFeedRequest{
		Watch: &agentreplv1.FeedWatchToken{Value: "ft-1"},
	}))
	if err != nil {
		t.Fatalf("WatchFeed: %v", err)
	}
	return stream
}

// receiveSelection reads the watch stream until a selection frame arrives,
// skipping any row frames.
func receiveSelection(
	t *testing.T,
	stream *connect.ServerStreamForClient[agentreplv1.WatchFeedResponse],
) *frontendv1.FeedSelection {
	t.Helper()
	for stream.Receive() {
		if sel := stream.Msg().GetSelection(); sel != nil {
			return sel
		}
	}
	t.Fatalf("the watch stream ended before a selection arrived: %v", stream.Err())
	return nil
}

// TestWatchFeedPushesTheSelectionOnTheRootFeed pins that a SelectFeedRow
// turn-boundary step reaches the root feed's watch as a FeedSelection frame
// carrying the selected row under the response arm.
func TestWatchFeedPushesTheSelectionOnTheRootFeed(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Feed.finals = feedIDs("a", "b", "c")
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	stream := openRootWatch(t, h, ctx)

	// Act.
	if _, err := h.Client.SelectFeedRow(context.Background(), connect.NewRequest(boundaryStep(older))); err != nil {
		t.Fatalf("SelectFeedRow: %v", err)
	}
	sel := receiveSelection(t, stream)

	// Assert.
	if got := sel.GetResponse().GetRow().GetValue(); got != "c" {
		t.Fatalf("selected response = %q, want the most recent c", got)
	}
}

// TestWatchFeedPushesTheClearedSelection pins that CLEAR pushes a
// none{return_to_tail} frame, which is what returns the webapp to the feed
// bottom.
func TestWatchFeedPushesTheClearedSelection(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Feed.finals = feedIDs("a", "b", "c")
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	stream := openRootWatch(t, h, ctx)
	if _, err := h.Client.SelectFeedRow(context.Background(), connect.NewRequest(boundaryStep(older))); err != nil {
		t.Fatalf("seed selection: %v", err)
	}
	if got := receiveSelection(t, stream).GetResponse().GetRow().GetValue(); got != "c" {
		t.Fatalf("seed selection = %q, want c", got)
	}

	// Act.
	if _, err := h.Client.SelectFeedRow(context.Background(), connect.NewRequest(clearMove())); err != nil {
		t.Fatalf("SelectFeedRow clear: %v", err)
	}
	sel := receiveSelection(t, stream)

	// Assert.
	if sel.GetNone().GetReturnToTail() == nil {
		t.Fatalf("selection = %v, want none{return_to_tail} after a clear", sel)
	}
}

// TestOpenFeedRecordsTheOpenAtInfoWithItsDuration pins that every open is one
// INFO record naming how long the daemon spent on it: a page's first paint
// waits on it, and the page's own feed.open-answered says how long it waited.
func TestOpenFeedRecordsTheOpenAtInfoWithItsDuration(t *testing.T) {
	// Arrange.
	log := dlog.NewTestLogger()
	h := newHarness(t, func(deps *Deps) { deps.Log = &fakeSurfaces{global: log, workspace: log} })
	h.Feed.page = page()
	h.Feed.token = &agentreplv1.FeedWatchToken{Value: "ft-1"}

	// Act.
	if _, err := h.Client.OpenFeed(context.Background(),
		connect.NewRequest(&agentreplv1.OpenFeedRequest{Workspace: ref()})); err != nil {
		t.Fatalf("OpenFeed: %v", err)
	}

	// Assert.
	for _, rec := range log.Records() {
		if rec.Operation == "daemon.server.open_feed" && rec.Message == "opened a feed and minted its watch token" {
			if _, ok := rec.Context["ms"]; rec.Level != "info" || !ok {
				t.Fatalf("open_feed record = %+v, want info carrying ms", rec)
			}
			return
		}
	}
	t.Fatalf("records = %+v, want a daemon.server.open_feed record", log.Records())
}
