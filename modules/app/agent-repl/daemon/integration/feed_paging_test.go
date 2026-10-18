//go:build integration

package integration

import (
	"testing"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/integration/harness"
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
	f, rows := pagedFeedRows(t)
	return f, rows["row 0"]
}

// pagedFeedRows is pagedFeed answering every response row the tail saw, by its
// markdown.
func pagedFeedRows(t *testing.T) (*fixture, map[string]*frontendv1.FeedRow) {
	t.Helper()
	f := newOpenedWithProfile(t, harness.Opts{}, harness.ShimProfile{HistoryPageSize: pagingStorePage})
	f.submit("go", "k-paging", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	tail := f.watchRootFeed()
	for i := range pagingRows {
		f.shim.PushAgentFrame(mainAgent, feedRowLabeledResponse(i))
	}
	rows := map[string]*frontendv1.FeedRow{}
	awaitRow(t, f, tail, "the last pushed row", func(r *frontendv1.FeedRow) bool {
		md := r.GetActivity().GetResponse().GetSuccess().GetProse().GetMarkdown()
		rows[md] = r
		return md == "row "+itoa(pagingRows-1)
	})
	if rows["row 0"] == nil {
		t.Fatal("the tail never carried the oldest response row")
	}
	return f, rows
}

// feedPage asks GetFeedPage for the first page of the root feed, or the page
// above the oldest row the fixture's pages served it (f.oldestHeld).
func (f *fixture) feedPage(first bool) *frontendv1.FeedPage {
	f.t.Helper()
	req := &agentreplv1.GetFeedPageRequest{Workspace: f.ws, Page: &agentreplv1.GetFeedPageRequest_OlderThan{
		OlderThan: &agentreplv1.GetFeedPageOlderThan{OldestHeld: f.oldestHeld},
	}}
	if first {
		req.Page = &agentreplv1.GetFeedPageRequest_First{First: &agentreplv1.GetFeedPageFirst{}}
	}
	resp, err := f.d.Client().GetFeedPage(f.d.Ctx(), connect.NewRequest(req))
	if err != nil || resp.Msg.GetSuccess().GetSuccess() == nil {
		f.t.Fatalf("GetFeedPage = %v, %v; want a page", resp, err)
	}
	if rows := resp.Msg.GetSuccess().GetSuccess().GetRows(); len(rows) > 0 {
		f.oldestHeld = rows[0].GetOrder()
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
