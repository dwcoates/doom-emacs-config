//go:build integration

package integration

import (
	"testing"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/integration/harness"
	"claude-repld/internal/feedid"
	"claude-repld/internal/ids"
)

// FEED PAGING ON DEMAND (docs/protobuf-design/feed-paging-on-demand.md): a
// page is the store's page, read daemon → shim → store when a reader asks for
// it. The fake store pages in pagingStorePage entries, so a short book is
// several pages.

const (
	// pagingStorePage is the fake store's page size these tests run at.
	pagingStorePage = 4
	// pagingRows is how many response rows the book carries: three pages and
	// a part, below the turn's prompt.
	pagingRows = 14
)

// pagedFeed opens a workspace on a small-paged store, writes one turn of
// pagingRows responses into its book, and answers the fixture and the row of
// the turn's oldest response, as the tail saw it.
func pagedFeed(t *testing.T) (*fixture, *frontendv1.FeedRow) {
	t.Helper()
	f := newOpenedWithProfile(t, harness.Opts{}, harness.ShimProfile{HistoryPageSize: pagingStorePage})
	f.submit("go", "k-paging", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	tail := f.watchRootFeed()
	for i := range pagingRows {
		f.shim.PushAgentFrame(mainAgent, feedRowLabeledResponse(i))
	}
	var oldest *frontendv1.FeedRow
	awaitRow(t, f, tail, "the last pushed row", func(r *frontendv1.FeedRow) bool {
		md := r.GetActivity().GetResponse().GetSuccess().GetProse().GetMarkdown()
		if md == "row 0" {
			oldest = r
		}
		return md == "row "+itoa(pagingRows-1)
	})
	if oldest == nil {
		t.Fatal("the tail never carried the oldest response row")
	}
	return f, oldest
}

// feedPage asks GetFeedPage for the first or next page of the root feed.
func (f *fixture) feedPage(first bool) *frontendv1.FeedPage {
	f.t.Helper()
	req := &agentreplv1.GetFeedPageRequest{Workspace: f.ws, Page: &agentreplv1.GetFeedPageRequest_Next{Next: &agentreplv1.GetFeedPageNext{}}}
	if first {
		req.Page = &agentreplv1.GetFeedPageRequest_First{First: &agentreplv1.GetFeedPageFirst{}}
	}
	resp, err := f.d.Client().GetFeedPage(f.d.Ctx(), connect.NewRequest(req))
	if err != nil || resp.Msg.GetSuccess().GetSuccess() == nil {
		f.t.Fatalf("GetFeedPage = %v, %v; want a page", resp, err)
	}
	return resp.Msg.GetSuccess()
}

func TestAFeedIsWalkedOneStorePageAtATimeToItsStart(t *testing.T) {
	t.Parallel()
	// Arrange.
	f, _ := pagedFeed(t)

	// Act: the newest page, then next until the start.
	pages := []*frontendv1.FeedPage{f.feedPage(true)}
	for pages[len(pages)-1].GetSuccess().GetHasMore() != nil && len(pages) <= pagingRows {
		pages = append(pages, f.feedPage(false))
	}

	// Assert: more than one page, each row served once, every response reached.
	if len(pages) < 3 {
		t.Fatalf("walked %d pages, want the book's several store pages", len(pages))
	}
	if got := len(pages[0].GetSuccess().GetRows()); got > pagingStorePage {
		t.Fatalf("the newest page served %d rows, want at most one store page's %d", got, pagingStorePage)
	}
	seen := map[string]bool{}
	responses := 0
	for _, page := range pages {
		for _, row := range page.GetSuccess().GetRows() {
			if seen[row.GetId().GetValue()] {
				t.Fatalf("row %v was served twice", row.GetId())
			}
			seen[row.GetId().GetValue()] = true
			if row.GetActivity().GetResponse() != nil {
				responses++
			}
		}
	}
	if responses != pagingRows {
		t.Fatalf("the walk served %d responses, want all %d", responses, pagingRows)
	}
}

func TestLoadFeedThroughStreamsEveryPageDownToTheTarget(t *testing.T) {
	t.Parallel()
	// Arrange: the reader holds the newest page only.
	f, oldest := pagedFeed(t)
	f.feedPage(true)

	// Act.
	stream, err := f.d.Client().LoadFeedThrough(f.d.Ctx(), connect.NewRequest(&agentreplv1.LoadFeedThroughRequest{
		Workspace: f.ws, Target: oldest.GetId(),
	}))
	if err != nil {
		t.Fatalf("LoadFeedThrough: %v", err)
	}
	var frames []*agentreplv1.LoadFeedThroughResponse
	for stream.Receive() {
		frames = append(frames, stream.Msg())
	}
	if err := stream.Err(); err != nil {
		t.Fatalf("LoadFeedThrough stream: %v", err)
	}

	// Assert: pages, then reached at the target, the target on the last page.
	if len(frames) < 2 {
		t.Fatalf("frames = %v, want pages then the terminal", frames)
	}
	if got := frames[len(frames)-1].GetReached().GetTarget().GetValue(); got != oldest.GetId().GetValue() {
		t.Fatalf("terminal = %v, want reached at the oldest response", frames[len(frames)-1])
	}
	found := false
	for _, row := range frames[len(frames)-2].GetPage().GetSuccess().GetRows() {
		found = found || row.GetId().GetValue() == oldest.GetId().GetValue()
	}
	if !found {
		t.Fatalf("the last page streamed did not carry the target")
	}
}

func TestANextAfterLoadFeedThroughContinuesBelowIt(t *testing.T) {
	t.Parallel()
	// Arrange.
	f, oldest := pagedFeed(t)
	f.feedPage(true)
	stream, err := f.d.Client().LoadFeedThrough(f.d.Ctx(), connect.NewRequest(&agentreplv1.LoadFeedThroughRequest{
		Workspace: f.ws, Target: oldest.GetId(),
	}))
	if err != nil {
		t.Fatalf("LoadFeedThrough: %v", err)
	}
	served := map[string]bool{}
	for stream.Receive() {
		for _, row := range stream.Msg().GetPage().GetSuccess().GetRows() {
			served[row.GetId().GetValue()] = true
		}
	}
	if err := stream.Err(); err != nil {
		t.Fatalf("LoadFeedThrough stream: %v", err)
	}

	// Act.
	page := f.feedPage(false)

	// Assert: nothing the walk already delivered is served again.
	for _, row := range page.GetSuccess().GetRows() {
		if served[row.GetId().GetValue()] {
			t.Fatalf("next re-served %v, which LoadFeedThrough delivered", row.GetId())
		}
	}
}

func TestLoadFeedThroughATargetTheConversationNeverHadIsNotFound(t *testing.T) {
	t.Parallel()
	// Arrange: a well-formed row of this workspace's root that nothing drew.
	f, _ := pagedFeed(t)
	f.feedPage(true)
	ghost := feedid.Encode(feedid.Ref{
		WS:   ids.WorkspaceID(f.ws.GetId()),
		Feed: feedid.Feed{Root: true},
		Row:  feedid.RowKey{Kind: feedid.KindPrompt, ID: "never-drawn"},
	})

	// Act.
	stream, err := f.d.Client().LoadFeedThrough(f.d.Ctx(), connect.NewRequest(&agentreplv1.LoadFeedThroughRequest{
		Workspace: f.ws, Target: ghost,
	}))
	if err != nil {
		t.Fatalf("LoadFeedThrough: %v", err)
	}
	var last *agentreplv1.LoadFeedThroughResponse
	for stream.Receive() {
		last = stream.Msg()
	}

	// Assert.
	if last.GetError().GetNotFound() == nil {
		t.Fatalf("terminal = %v (err %v), want not_found", last, stream.Err())
	}
}
