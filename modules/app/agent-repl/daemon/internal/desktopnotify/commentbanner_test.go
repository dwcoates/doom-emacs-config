package desktopnotify

import (
	"context"
	"testing"

	"agent-repld/internal/ids"
)

// fakeCommentSummaries answers one summary and records what it was asked.
type fakeCommentSummaries struct {
	summary  string
	asked    []string
	askedFor []ids.WorkspaceID
}

func (f *fakeCommentSummaries) SummarizeComment(_ context.Context, ws ids.WorkspaceID, comment string) string {
	f.asked = append(f.asked, comment)
	f.askedFor = append(f.askedFor, ws)
	return f.summary
}

func TestCommentBannersCommented(t *testing.T) {
	cases := []struct {
		name     string
		summary  string
		wantBody string
	}{
		{name: "summarized", summary: "Asks to rename the flag.", wantBody: "Comment from octocat: " + Italic("Asks to rename the flag.")},
		{name: "blank comment", summary: "", wantBody: "Comment from octocat"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			poster, summaries := &fakePoster{}, &fakeCommentSummaries{summary: tc.summary}
			c := NewCommentBanners(poster, summaries)

			// Act
			c.Commented("ws1", "octocat", "please rename the flag")

			// Assert
			want := Banner{Title: "💬 my-ws", Body: tc.wantBody}
			if len(poster.banners) != 1 || poster.banners[0] != want || poster.kinds[0] != KindPullRequestComment || poster.workspaces[0] != "ws1" {
				t.Fatalf("posted %+v (kinds %v, workspaces %v), want %+v as %s for ws1", poster.banners, poster.kinds, poster.workspaces, want, KindPullRequestComment)
			}
			if len(summaries.asked) != 1 || summaries.asked[0] != "please rename the flag" || summaries.askedFor[0] != "ws1" {
				t.Fatalf("summaries asked %v for %v, want the comment for ws1", summaries.asked, summaries.askedFor)
			}
		})
	}
}

func TestNewCommentBannersPanicsOnAMissingCollaborator(t *testing.T) {
	cases := []struct {
		name      string
		poster    Poster
		summaries CommentSummaries
	}{
		{name: "no poster", summaries: &fakeCommentSummaries{}},
		{name: "no summaries", poster: &fakePoster{}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Assert
			defer func() {
				if recover() == nil {
					t.Fatal("a missing collaborator was accepted")
				}
			}()

			// Act
			NewCommentBanners(tc.poster, tc.summaries)
		})
	}
}
