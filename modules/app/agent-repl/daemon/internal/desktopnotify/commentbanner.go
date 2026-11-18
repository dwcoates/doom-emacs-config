package desktopnotify

import (
	"context"

	"agent-repld/internal/ids"
)

// KindPullRequestComment is the banner a comment on a followed pull request
// raises when someone other than the user wrote it.
const KindPullRequestComment = "pull_request_comment"

// CommentSummaries writes a comment banner's summary
// (Summarizer.SummarizeComment).
type CommentSummaries interface {
	SummarizeComment(ctx context.Context, ws ids.WorkspaceID, comment string) string
}

// CommentBanners raises the banner of a comment someone other than the user
// left on a pull request a workspace follows: titled by the workspace, so its
// click selects it, and saying who commented over a one-sentence Haiku
// shortening of what they said.
type CommentBanners struct {
	poster    Poster
	summaries CommentSummaries
}

// NewCommentBanners builds the comment banners. It panics on a missing
// collaborator.
func NewCommentBanners(poster Poster, summaries CommentSummaries) *CommentBanners {
	if poster == nil || summaries == nil {
		panic("desktopnotify: NewCommentBanners requires a Poster and CommentSummaries")
	}
	return &CommentBanners{poster: poster, summaries: summaries}
}

// Commented posts the banner of AUTHOR's comment, whose markdown is COMMENT,
// for the workspace WS that follows its pull request. The summary is asked on
// the banner's own goroutine, so the caller never waits on the model.
func (c *CommentBanners) Commented(ws ids.WorkspaceID, author, comment string) {
	c.poster.Post(ws, KindPullRequestComment, func(ctx context.Context, name string) Banner {
		return Banner{Title: CommentTitle(name), Body: CommentBody(author, c.summaries.SummarizeComment(ctx, ws, comment))}
	})
}

// CommentTitle is a comment banner's first line.
func CommentTitle(name string) string {
	return eventTitle("💬", name)
}

// CommentBody is a comment banner's text: who commented, and the summary in
// italics (Italic) when there is one.
func CommentBody(author, summary string) string {
	body := "Comment from " + author
	if summary == "" {
		return body
	}
	return body + ": " + Italic(summary)
}
