package prwatch

import (
	"context"
	"reflect"
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ghclient"
	"agent-repld/internal/wsm"
)

func TestCommentsSomeoneElseWroteRaiseOneBannerEach(t *testing.T) {
	line := map[string]any{"path": "a.go", "line": 3}
	cases := []struct {
		name     string
		comments []any
		lines    []any
		reviews  []any
		want     []raisedComment
	}{
		{name: "a conversation comment", comments: []any{commentJSONOf(11, "alice", "please rename it", t0, t0, nil)},
			want: []raisedComment{{author: "@alice", markdown: "please rename it"}}},
		{name: "a line comment", lines: []any{commentJSONOf(12, "bob", "off by one", t0, t0, line)},
			want: []raisedComment{{author: "@bob", markdown: "off by one"}}},
		{name: "a review with a body", reviews: []any{reviewJSONOf(13, "carol", "CHANGES_REQUESTED", "needs tests", t0)},
			want: []raisedComment{{author: "@carol", markdown: "needs tests"}}},
		{name: "a review with no body", reviews: []any{reviewJSONOf(14, "carol", "APPROVED", "", t0)}},
		{name: "the user's own comment", comments: []any{commentJSONOf(15, viewerLogin, "done", t0, t0, nil)}},
		{name: "the user's own comment in another case", comments: []any{commentJSONOf(16, "ME", "done", t0, t0, nil)}},
		{name: "an author GitHub names no login for", comments: []any{commentJSONOf(17, "", "hi", t0, t0, nil)},
			want: []raisedComment{{author: "an unknown account", markdown: "hi"}}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			r := newRig(t)
			r.subscribe(t, wsm.PullRequestEventComment, wsm.PullRequestEventReviewComment, wsm.PullRequestEventReview)
			r.gh.set(pathComments(), mustJSON(append([]any{}, tc.comments...)))
			r.gh.set(pathLines(), mustJSON(append([]any{}, tc.lines...)))
			r.gh.set(pathReviews(), mustJSON(append([]any{}, tc.reviews...)))
			for i := range tc.want {
				tc.want[i].ws = r.ws
			}

			// Act
			r.tick()
			r.tick()

			// Assert
			if !reflect.DeepEqual(r.banners.raised, tc.want) {
				t.Fatalf("raised %+v, want %+v", r.banners.raised, tc.want)
			}
		})
	}
}

func TestACommentTheSubscriptionDoesNotAskForRaisesNoBanner(t *testing.T) {
	// Arrange
	r := newRig(t)
	r.subscribe(t, wsm.PullRequestEventReview)
	r.gh.set(pathComments(), mustJSON([]any{commentJSONOf(11, "alice", "hi", t0, t0, nil)}))

	// Act
	r.tick()

	// Assert
	if len(r.banners.raised) != 0 {
		t.Fatalf("raised %+v, want nothing for a comment the subscription does not ask for", r.banners.raised)
	}
}

func TestAnEditedCommentRaisesNoBanner(t *testing.T) {
	// Arrange
	r := newRig(t)
	r.subscribe(t, wsm.PullRequestEventComment)
	r.gh.set(pathComments(), mustJSON([]any{commentJSONOf(11, "alice", "hi", t0, t0, nil)}))
	r.tick()
	r.banners.raised = nil
	r.gh.set(pathComments(), mustJSON([]any{commentJSONOf(11, "alice", "hi there", t0, t0.Add(1e9), nil)}))

	// Act
	r.tick()

	// Assert
	if len(r.told()) != 2 || len(r.banners.raised) != 0 {
		t.Fatalf("told %q raised %+v, want the edit told and no banner", r.told(), r.banners.raised)
	}
}

func TestBannerCommentsRecordsWhatItRaised(t *testing.T) {
	// Arrange
	r := newRig(t)
	r.subscribe(t, wsm.PullRequestEventComment)
	r.gh.set(pathComments(), mustJSON([]any{commentJSONOf(11, "alice", "hi", t0, t0, nil), commentJSONOf(12, viewerLogin, "ok", t0, t0, nil)}))

	// Act
	r.tick()

	// Assert
	rec, ok := dlog.FindRecord(r.log.Records(), dlog.RecordQuery{Level: "info", Message: "raised the desktop banners of the comments someone other than the user wrote"})
	if !ok || rec.Context["comments"] != 2 || rec.Context["raised"] != 1 || rec.Context["viewer"] != viewerLogin || rec.Context["subscriber"] != string(r.ws) {
		t.Fatalf("record = %+v (found %v), want two comments, one raised, the viewer and the subscriber", rec, ok)
	}
}

func TestAnUnreadableViewerRaisesNoBannerAndStillTellsTheSubscriber(t *testing.T) {
	cases := []struct {
		name   string
		answer any
		cause  string
	}{
		{name: "the read fails", answer: &ghclient.Failure{Kind: ghclient.KindUnreachable, Message: "offline"}, cause: "offline"},
		{name: "the body does not decode", answer: `[]`, cause: "prwatch: the authenticated user is unreadable"},
		{name: "no login", answer: `{"login":""}`, cause: "prwatch: GitHub named no login for the authenticated user"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			r := newRig(t)
			r.subscribe(t, wsm.PullRequestEventComment)
			r.gh.set("user", tc.answer)
			r.gh.set(pathComments(), mustJSON([]any{commentJSONOf(11, "alice", "hi", t0, t0, nil)}))

			// Act
			r.tick()

			// Assert
			if len(r.banners.raised) != 0 || len(r.told()) != 1 {
				t.Fatalf("raised %+v told %q, want no banner and the comment still told", r.banners.raised, r.told())
			}
			rec, ok := dlog.FindRecord(r.log.Records(), dlog.RecordQuery{Level: "error", Message: "could not read the user's GitHub login; the comments raise no desktop banner"})
			if !ok || rec.Context["comments"] != 1 || rec.Context["subscriber"] != string(r.ws) || !contains(rec.Context["cause"].(string), tc.cause) {
				t.Fatalf("record = %+v (found %v), want the cause %q, the subscriber and the count", rec, ok, tc.cause)
			}
		})
	}
}

func TestAViewerReadCutShortByItsContextIsNoError(t *testing.T) {
	// Arrange
	r := newRig(t)
	r.gh.set("user", context.Canceled)
	ctx, cancel := context.WithCancel(context.Background())
	cancel()
	told := commentsTold("alice")

	// Act
	r.w.bannerComments(ctx, r.ws, told, dlog.Context{})

	// Assert
	if _, bad := dlog.FindRecord(r.log.Records(), dlog.RecordQuery{Level: "error"}); bad || len(r.banners.raised) != 0 {
		t.Fatalf("records %+v raised %+v, want no error and no banner", r.log.Records(), r.banners.raised)
	}
	if _, ok := dlog.FindRecord(r.log.Records(), dlog.RecordQuery{Level: "debug", Message: "the user's GitHub login was not read before the context ended; the comments raise no desktop banner"}); !ok {
		t.Fatal("the cut-short read left no DEBUG record")
	}
}

func TestNewRefusesAMissingCommentSink(t *testing.T) {
	// Arrange
	r := newRig(t)
	deps := r.w.deps
	deps.Comments = nil

	// Act
	_, err := New(deps)

	// Assert
	if err == nil {
		t.Fatal("New with no comment sink = nil, want refused")
	}
}

// commentsTold is one conversation comment by AUTHOR, as the diff tells it.
func commentsTold(login string) []*agentreplv1.PullRequestActivity {
	snap := snapshot{Comments: []remoteComment{{ID: 1, Author: author{Login: login}, Body: "hi", Created: t0, Updated: t0}}}
	prev, _, _ := decodeTold("")
	_, activity := diff(snap, prev, true)
	return activity
}
