package prwatch

import (
	"context"
	"errors"
	"fmt"
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"agent-repld/internal/ghclient"
	"agent-repld/internal/wsm"
)

func TestAnchorEndsASubscriptionToAPullRequestThatAlreadyMerged(t *testing.T) {
	// Arrange
	r := newRig(t)
	r.gh.set(pathPull(), pullJSON("closed", false, true, "h"))

	// Act
	sub, status, err := r.w.Anchor(context.Background(), r.ws, prK, wsm.AllPullRequestEvents(), wsm.PullRequestOriginOwnedLink)

	// Assert
	if err != nil || sub.Ended == nil || !sub.Ended.Merged || status.GetState().GetMerged() == nil || sub.Told == "" {
		t.Fatalf("Anchor = %+v, %v, %v; want an anchored subscription ended by the merge", sub, status, err)
	}
}

func TestAnchorThatCannotReadAnswersAnUnanchoredSubscription(t *testing.T) {
	// Arrange
	r := newRig(t)
	r.gh.set(pathPull(), &ghclient.Failure{Kind: ghclient.KindUnreachable, Message: "down"})

	// Act
	sub, _, err := r.w.Anchor(context.Background(), r.ws, prK, wsm.AllPullRequestEvents(), wsm.PullRequestOriginOwnedLink)

	// Assert
	if err == nil || sub.Told != "" || sub.Subscriber != r.ws {
		t.Fatalf("Anchor = %+v, %v; want an unanchored subscription and the error", sub, err)
	}
}

func TestACommentTheWorkspacePostedIsNeverToldBackToIt(t *testing.T) {
	tests := []struct {
		name      string
		inReplyTo *uint64
		post      string
		list      string
	}{
		{name: "a conversation comment", post: fmt.Sprintf("%s/issues/%d/comments", base, prK.Number), list: pathComments()},
		{name: "a reply in a thread", inReplyTo: ptr(uint64(5)), post: fmt.Sprintf("%s/pulls/%d/comments/5/replies", base, prK.Number), list: pathLines()},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			r := newRig(t)
			r.subscribe(t, wsm.PullRequestEventComment, wsm.PullRequestEventReviewComment)
			r.gh.posts[tt.post] = mustJSON(map[string]any{"id": 77, "html_url": "u77", "updated_at": t0})

			// Act
			posted, err := r.w.Comment(context.Background(), r.ws, prK, tt.inReplyTo, "done")
			r.gh.set(tt.list, mustJSON([]any{commentJSONOf(77, "dodge", "done", t0, t0, map[string]any{"path": "a.go"})}))
			r.tick()

			// Assert
			if err != nil || posted != (Posted{ID: 77, URL: "u77"}) {
				t.Fatalf("Comment = %+v, %v", posted, err)
			}
			if told := r.told(); len(told) != 0 {
				t.Fatalf("told = %q, want the workspace's own comment untold", told)
			}
		})
	}
}

func TestTheUsersCommentUnderTheSameLoginIsTold(t *testing.T) {
	// Arrange
	r := newRig(t)
	r.subscribe(t, wsm.PullRequestEventComment)
	r.gh.posts[fmt.Sprintf("%s/issues/%d/comments", base, prK.Number)] = mustJSON(map[string]any{"id": 77, "html_url": "u77", "updated_at": t0})
	if _, err := r.w.Comment(context.Background(), r.ws, prK, nil, "done"); err != nil {
		t.Fatal(err)
	}

	// Act
	r.gh.set(pathComments(), mustJSON([]any{commentJSONOf(77, "dodge", "done", t0, t0, nil), commentJSONOf(78, "dodge", "now do X", t0, t0, nil)}))
	r.tick()

	// Assert
	if told := r.told(); len(told) != 1 || !contains(told[0], "now do X") || contains(told[0], "~~~untrusted\ndone") {
		t.Fatalf("told = %q, want only the user's comment", told)
	}
}

func TestAReplyToNoThreadIsRefused(t *testing.T) {
	// Arrange
	r := newRig(t)
	r.gh.posts[fmt.Sprintf("%s/pulls/%d/comments/9/replies", base, prK.Number)] = &ghclient.Failure{Kind: ghclient.KindNotFound, Status: 404}

	// Act
	_, err := r.w.Comment(context.Background(), r.ws, prK, ptr(uint64(9)), "x")

	// Assert
	if !errors.Is(err, ErrUnknownThread) {
		t.Fatalf("Comment = %v, want ErrUnknownThread", err)
	}
}

func TestAPostGitHubRefusesIsItsFailure(t *testing.T) {
	// Arrange
	r := newRig(t)
	refused := &ghclient.Failure{Kind: ghclient.KindRefused, Status: 422, Message: "Validation Failed"}
	r.gh.posts[fmt.Sprintf("%s/issues/%d/comments", base, prK.Number)] = refused

	// Act
	_, err := r.w.Comment(context.Background(), r.ws, prK, nil, "x")

	// Assert
	got := FailureProto(err).GetRefused()
	if !errors.Is(err, refused) || got.GetHttpStatus() != 422 || got.GetMessage() != "Validation Failed" {
		t.Fatalf("Comment = %v (%v), want the refusal", err, got)
	}
}

func TestSubscriptionProtoSpellsTheStoredSubscription(t *testing.T) {
	// Arrange
	sub := wsm.PullRequestSubscription{Subscriber: "ws", PullRequest: prK, Events: wsm.NewPullRequestEvents(wsm.PullRequestEventChecks, wsm.PullRequestEventComment),
		Origin: wsm.PullRequestOriginOwnedLink, Failing: &wsm.PullRequestFailure{Since: t0, Kind: wsm.PullRequestFailureNotFound}}

	// Act
	got := SubscriptionProto(sub)

	// Assert
	if got.GetOrigin().GetOwnedLink() == nil || got.GetHealth().GetFailing().GetCause().GetNotFound() == nil || len(got.GetEvents()) != 2 || KeyOf(got.GetPullRequest()) != prK {
		t.Fatalf("SubscriptionProto = %v", got)
	}
	if EventsOf(got.GetEvents()) != sub.Events {
		t.Fatalf("EventsOf(EventProto) = %v, want %v", EventsOf(got.GetEvents()), sub.Events)
	}
}

func TestEventsOfFailsHardOnAnUnmappedEvent(t *testing.T) {
	// Arrange
	defer func() {
		if recover() == nil {
			t.Fatal("EventsOf did not panic")
		}
	}()

	// Act
	EventsOf([]agentreplv1.PullRequestNoticeEvent{agentreplv1.PullRequestNoticeEvent_PULL_REQUEST_NOTICE_EVENT_UNSPECIFIED})
}

func ptr[T any](v T) *T { return &v }
