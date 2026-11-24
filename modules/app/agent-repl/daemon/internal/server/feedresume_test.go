package server

import (
	"context"
	"slices"
	"testing"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
)

// revisedRow is a page row with an id and the revision it was published at.
func revisedRow(id, revision string) *frontendv1.FeedRow {
	return &frontendv1.FeedRow{
		Id:       &frontendv1.FeedId{Value: id},
		Revision: &frontendv1.FeedRowRevision{Digest: revision},
	}
}

// pageOf is a successful page holding ROWS.
func pageOf(rows ...*frontendv1.FeedRow) *frontendv1.FeedPage {
	p := page()
	p.GetSuccess().Rows = rows
	return p
}

// heldAt is one row a reader holds at REVISION.
func heldAt(id, revision string) *agentreplv1.FeedResumeHeldRow {
	return &agentreplv1.FeedResumeHeldRow{
		Id:       &frontendv1.FeedId{Value: id},
		Revision: &frontendv1.FeedRowRevision{Digest: revision},
	}
}

// idsOf answers the values of IDS, in order.
func idsOf(ids []*frontendv1.FeedId) []string {
	out := make([]string, 0, len(ids))
	for _, id := range ids {
		out = append(out, id.GetValue())
	}
	return out
}

// rowIDsOf answers the ids of ROWS, in order.
func rowIDsOf(rows []*frontendv1.FeedRow) []string {
	out := make([]string, 0, len(rows))
	for _, row := range rows {
		out = append(out, row.GetId().GetValue())
	}
	return out
}

// TestResumedOpenFeed pins what a resumed open answers against what the
// reader holds: only the rows it does not hold at their revision, with the
// whole page named as standing.
func TestResumedOpenFeed(t *testing.T) {
	tests := []struct {
		name         string
		held         []*agentreplv1.FeedResumeHeldRow
		wantRows     []string
		wantStanding []string
	}{
		{
			name:         "a reader holding every row at its revision is sent no row",
			held:         []*agentreplv1.FeedResumeHeldRow{heldAt("a", "r1"), heldAt("b", "r2")},
			wantRows:     []string{},
			wantStanding: []string{"a", "b"},
		},
		{
			name:         "a row held at an older revision is sent again",
			held:         []*agentreplv1.FeedResumeHeldRow{heldAt("a", "r1"), heldAt("b", "r-old")},
			wantRows:     []string{"b"},
			wantStanding: []string{"a", "b"},
		},
		{
			name:         "a row the reader does not hold is sent",
			held:         []*agentreplv1.FeedResumeHeldRow{heldAt("a", "r1")},
			wantRows:     []string{"b"},
			wantStanding: []string{"a", "b"},
		},
		{
			name:         "an empty resume is sent the whole page",
			held:         nil,
			wantRows:     []string{"a", "b"},
			wantStanding: []string{"a", "b"},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			h.Feed.page = pageOf(revisedRow("a", "r1"), revisedRow("b", "r2"))
			h.Feed.token = &agentreplv1.FeedWatchToken{Value: "ft-1", Instance: string(testInstance)}

			// Act.
			resp, err := h.Client.OpenFeed(context.Background(),
				connect.NewRequest(&agentreplv1.OpenFeedRequest{
					Workspace: ref(),
					Resume:    &agentreplv1.FeedResume{Held: tt.held},
				}))

			// Assert.
			if err != nil {
				t.Fatalf("OpenFeed: %v", err)
			}
			success := resp.Msg.GetSuccess()
			if got := rowIDsOf(success.GetPage().GetSuccess().GetRows()); !slices.Equal(got, tt.wantRows) {
				t.Fatalf("rows = %v, want %v", got, tt.wantRows)
			}
			if success.GetResumed() == nil {
				t.Fatalf("resumed = nil, want it set on a resumed open")
			}
			if got := idsOf(success.GetResumed().GetStanding()); !slices.Equal(got, tt.wantStanding) {
				t.Fatalf("standing = %v, want %v", got, tt.wantStanding)
			}
		})
	}
}

// TestFirstOpenFeedIsNotResumed pins that an open naming nothing held is
// answered the whole page and no resumed arm.
func TestFirstOpenFeedIsNotResumed(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Feed.page = pageOf(revisedRow("a", "r1"))
	h.Feed.token = &agentreplv1.FeedWatchToken{Value: "ft-1", Instance: string(testInstance)}

	// Act.
	resp, err := h.Client.OpenFeed(context.Background(),
		connect.NewRequest(&agentreplv1.OpenFeedRequest{Workspace: ref()}))

	// Assert.
	if err != nil {
		t.Fatalf("OpenFeed: %v", err)
	}
	if resp.Msg.GetSuccess().GetResumed() != nil {
		t.Fatalf("resumed = %v, want unset on a first open", resp.Msg.GetSuccess().GetResumed())
	}
	if got := len(resp.Msg.GetSuccess().GetPage().GetSuccess().GetRows()); got != 1 {
		t.Fatalf("rows = %d, want the whole page", got)
	}
}

// TestResumedOpenOfAFailedPageAnswersTheFailure pins that a page the daemon
// could not complete is answered whole, with nothing standing.
func TestResumedOpenOfAFailedPageAnswersTheFailure(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Feed.page = &frontendv1.FeedPage{Result: &frontendv1.FeedPage_Error{Error: &frontendv1.FeedPageError{}}}
	h.Feed.token = &agentreplv1.FeedWatchToken{Value: "ft-1", Instance: string(testInstance)}

	// Act.
	resp, err := h.Client.OpenFeed(context.Background(),
		connect.NewRequest(&agentreplv1.OpenFeedRequest{
			Workspace: ref(),
			Resume:    &agentreplv1.FeedResume{Held: []*agentreplv1.FeedResumeHeldRow{heldAt("a", "r1")}},
		}))

	// Assert.
	if err != nil {
		t.Fatalf("OpenFeed: %v", err)
	}
	if resp.Msg.GetSuccess().GetPage().GetError() == nil {
		t.Fatalf("page = %v, want the failure answered", resp.Msg.GetSuccess().GetPage())
	}
	if got := len(resp.Msg.GetSuccess().GetResumed().GetStanding()); got != 0 {
		t.Fatalf("standing = %d, want none", got)
	}
}

// TestResumeWithAMalformedHeldRowIsInvalidArgument pins that a held row with
// no id or no revision is refused as malformed.
func TestResumeWithAMalformedHeldRowIsInvalidArgument(t *testing.T) {
	tests := []struct {
		name string
		held *agentreplv1.FeedResumeHeldRow
	}{
		{name: "no id", held: &agentreplv1.FeedResumeHeldRow{Revision: &frontendv1.FeedRowRevision{Digest: "r1"}}},
		{name: "an empty revision", held: heldAt("a", "")},
		{name: "no revision", held: &agentreplv1.FeedResumeHeldRow{Id: &frontendv1.FeedId{Value: "a"}}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)

			// Act.
			_, err := h.Client.OpenFeed(context.Background(),
				connect.NewRequest(&agentreplv1.OpenFeedRequest{
					Workspace: ref(),
					Resume:    &agentreplv1.FeedResume{Held: []*agentreplv1.FeedResumeHeldRow{tt.held}},
				}))

			// Assert.
			if code := connectCode(t, err); code != connect.CodeInvalidArgument {
				t.Fatalf("code = %v, want InvalidArgument", code)
			}
		})
	}
}
