package prwatch

import (
	"context"
	"encoding/json"
	"fmt"
	"strings"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
)

// opBanner is the operation a comment's desktop banner records under.
const opBanner = "daemon.prwatch.comment_banner"

// CommentSink raises the desktop banner of one comment
// (desktopnotify.CommentBanners.Commented).
type CommentSink interface {
	Commented(ws ids.WorkspaceID, author, comment string)
}

// postedComment is one comment a subscriber is owed: who wrote it and its
// markdown.
type postedComment struct {
	author, markdown string
}

// commentsOf answers the comments among TOLD: each new conversation comment,
// line comment, and review that carries a body. An edit or a deletion is no
// new comment, and a review with no body says nothing to shorten.
func commentsOf(told []*agentreplv1.PullRequestActivity) []postedComment {
	var out []postedComment
	for _, a := range told {
		switch arm := a.GetActivity().(type) {
		case *agentreplv1.PullRequestActivity_Comment:
			out = append(out, postedComment{arm.Comment.GetAuthor().GetLogin(), arm.Comment.GetText().GetMarkdown()})
		case *agentreplv1.PullRequestActivity_ReviewComment:
			out = append(out, postedComment{arm.ReviewComment.GetAuthor().GetLogin(), arm.ReviewComment.GetText().GetMarkdown()})
		case *agentreplv1.PullRequestActivity_Review:
			if arm.Review.GetText() != nil {
				out = append(out, postedComment{arm.Review.GetAuthor().GetLogin(), arm.Review.GetText().GetMarkdown()})
			}
		}
	}
	return out
}

// bannerComments raises a desktop banner for SUBSCRIBER of each comment among
// TOLD that someone other than the user wrote. TOLD is what the subscription's
// compare-and-set just owed it, so each comment raises its banner once.
//
// THE USER IS THE ACCOUNT agent-repl READS GITHUB AS: its agents' own replies
// (`pr-comment`) post as that account too, so neither the user's comments nor
// their agents' raise a banner.
func (w *Watcher) bannerComments(ctx context.Context, subscriber ids.WorkspaceID, told []*agentreplv1.PullRequestActivity, fields dlog.Context) {
	comments := commentsOf(told)
	if len(comments) == 0 {
		return
	}
	fields = dlog.Merge(fields, dlog.Context{"comments": len(comments)})
	viewer, err := w.viewer(ctx)
	if err != nil {
		if ctx.Err() != nil {
			w.deps.Log.Debug(opBanner, "the user's GitHub login was not read before the context ended; the comments raise no desktop banner", fields)
			return
		}
		w.deps.Log.Error(opBanner, "could not read the user's GitHub login; the comments raise no desktop banner", dlog.WithCause(fields, err))
		return
	}
	raised := 0
	for _, c := range comments {
		if strings.EqualFold(c.author, viewer) {
			continue
		}
		author := c.author
		if author == "" {
			author = "an unknown account"
		}
		w.deps.Comments.Commented(subscriber, author, c.markdown)
		raised++
	}
	w.deps.Log.Info(opBanner, "raised the desktop banners of the comments someone other than the user wrote", dlog.Merge(fields, dlog.Context{"raised": raised, "viewer": viewer}))
}

// viewer answers the login agent-repl reads GitHub as. The read is
// conditional, as every page's is, so an unchanged account costs a 304.
func (w *Watcher) viewer(ctx context.Context) (string, error) {
	body, err := w.reader.get(ctx, "user")
	if err != nil {
		return "", err
	}
	var u userJSON
	if err := json.Unmarshal(body, &u); err != nil {
		return "", fmt.Errorf("prwatch: the authenticated user is unreadable: %w", err)
	}
	if u.Login == "" {
		return "", fmt.Errorf("prwatch: GitHub named no login for the authenticated user")
	}
	return u.Login, nil
}
