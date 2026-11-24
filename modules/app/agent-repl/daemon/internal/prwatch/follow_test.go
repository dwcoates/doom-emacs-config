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

func TestTheMergeQueueEventRoundTripsThroughTheContract(t *testing.T) {
	// Act
	spelled := EventProto(wsm.PullRequestEventMergeQueue)
	stored := EventsOf([]agentreplv1.PullRequestNoticeEvent{spelled})
	sub := SubscriptionProto(wsm.PullRequestSubscription{Subscriber: "ws1", PullRequest: prK, Events: wsm.AllPullRequestEvents(), Origin: wsm.PullRequestOriginOwnedLink})

	// Assert
	if spelled != agentreplv1.PullRequestNoticeEvent_PULL_REQUEST_NOTICE_EVENT_MERGE_QUEUE || stored != wsm.NewPullRequestEvents(wsm.PullRequestEventMergeQueue) {
		t.Fatalf("spelled %v, stored %v", spelled, stored)
	}
	if got := sub.GetEvents(); len(got) != 7 || got[5] != spelled {
		t.Fatalf("an owned link's events = %v, want every event, the merge queue sixth", got)
	}
}

func TestTheMergedEventRoundTripsThroughTheContract(t *testing.T) {
	// Act
	spelled := EventProto(wsm.PullRequestEventMerged)
	stored := EventsOf([]agentreplv1.PullRequestNoticeEvent{spelled})

	// Assert
	if spelled != agentreplv1.PullRequestNoticeEvent_PULL_REQUEST_NOTICE_EVENT_MERGED || stored != wsm.NewPullRequestEvents(wsm.PullRequestEventMerged) {
		t.Fatalf("spelled %v, stored %v", spelled, stored)
	}
}

func TestSubscriptionProtoSpellsEachOrigin(t *testing.T) {
	tests := []struct {
		origin wsm.PullRequestOrigin
		want   func(*agentreplv1.PullRequestSubscriptionOrigin) bool
	}{
		{origin: wsm.PullRequestOriginOwnedLink, want: func(o *agentreplv1.PullRequestSubscriptionOrigin) bool { return o.GetOwnedLink() != nil }},
		{origin: wsm.PullRequestOriginRequested, want: func(o *agentreplv1.PullRequestSubscriptionOrigin) bool { return o.GetRequested() != nil }},
		{origin: wsm.PullRequestOriginFollowedWorkspace, want: func(o *agentreplv1.PullRequestSubscriptionOrigin) bool { return o.GetFollowedWorkspace() != nil }},
	}
	for _, tt := range tests {
		t.Run(string(tt.origin), func(t *testing.T) {
			// Act
			got := SubscriptionProto(wsm.PullRequestSubscription{Subscriber: "ws", PullRequest: prK, Events: wsm.NewPullRequestEvents(wsm.PullRequestEventMerged), Origin: tt.origin})

			// Assert
			if !tt.want(got.GetOrigin()) {
				t.Fatalf("origin = %v, want the %s arm", got.GetOrigin(), tt.origin)
			}
		})
	}
}

func TestSubscriptionProtoFailsHardOnAnUndeclaredOrigin(t *testing.T) {
	// Arrange
	defer func() {
		if recover() == nil {
			t.Fatal("SubscriptionProto did not panic")
		}
	}()

	// Act
	SubscriptionProto(wsm.PullRequestSubscription{Subscriber: "ws", PullRequest: prK, Events: wsm.NewPullRequestEvents(wsm.PullRequestEventMerged), Origin: "inherited"})
}

// followFromFirstRead subscribes the rig's workspace to the pull request as
// an owned link does: without a read.
func (r *rig) followFromFirstRead(t *testing.T) wsm.PullRequestSubscription {
	t.Helper()
	sub := r.w.FollowFromFirstRead(r.ws, prK, wsm.AllPullRequestEvents(), wsm.PullRequestOriginOwnedLink)
	if _, err := r.db.SubscribePullRequest(context.Background(), sub); err != nil {
		t.Fatalf("SubscribePullRequest: %v", err)
	}
	return sub
}

func TestFollowFromFirstReadReadsNothing(t *testing.T) {
	// Arrange
	r := newRig(t)
	r.gh.set(pathPull(), &ghclient.Failure{Kind: ghclient.KindUnreachable, Message: "down"})

	// Act
	sub := r.w.FollowFromFirstRead(r.ws, prK, wsm.AllPullRequestEvents(), wsm.PullRequestOriginOwnedLink)

	// Assert
	r.gh.mu.Lock()
	calls := append([]string(nil), r.gh.calls...)
	r.gh.mu.Unlock()
	if len(calls) != 0 {
		t.Fatalf("GitHub was asked %v, want no read", calls)
	}
	if sub.Subscriber != r.ws || sub.PullRequest != prK || sub.Origin != wsm.PullRequestOriginOwnedLink || !sub.SubscribedAt.Equal(t0) || sub.Told == "" {
		t.Fatalf("FollowFromFirstRead = %+v, want the owned link's subscription, marked for its first read", sub)
	}
}

func TestTheFirstReadOfASubscriptionTakenWithoutOneTellsNothingAlreadyThere(t *testing.T) {
	// Arrange
	r := newRig(t)
	r.followFromFirstRead(t)
	r.gh.set(pathComments(), mustJSON([]any{commentJSONOf(11, "alice", "already here", t0, t0, nil)}))

	// Act
	r.tick()

	// Assert
	if told := r.told(); len(told) != 0 {
		t.Fatalf("told = %q, want nothing: the first read anchors", told)
	}
}

func TestASubscriptionTakenWithoutAReadTellsWhatComesAfterItsFirst(t *testing.T) {
	// Arrange
	r := newRig(t)
	r.followFromFirstRead(t)
	r.gh.set(pathComments(), mustJSON([]any{commentJSONOf(11, "alice", "already here", t0, t0, nil)}))
	r.tick()

	// Act
	r.gh.set(pathComments(), mustJSON([]any{commentJSONOf(11, "alice", "already here", t0, t0, nil), commentJSONOf(12, "bob", "a new one", t0, t0, nil)}))
	r.tick()

	// Assert
	told := r.told()
	if len(told) != 1 || !contains(told[0], "a new one") || contains(told[0], "already here") {
		t.Fatalf("told = %q, want only the comment after the first read", told)
	}
}

func TestTheFirstReadOfASubscriptionTakenWithoutOneStillTellsItsEnd(t *testing.T) {
	// Arrange
	r := newRig(t)
	r.followFromFirstRead(t)
	r.gh.set(pathPull(), pullJSON("closed", false, true, "head1"))

	// Act
	r.tick()

	// Assert
	told := r.told()
	if len(told) != 1 || !contains(told[0], endedSentence) {
		t.Fatalf("told = %q, want the subscription's end", told)
	}
}

func TestACommentPostedBeforeTheFirstReadIsAnchoredWithTheRest(t *testing.T) {
	// Arrange
	r := newRig(t)
	sub := r.followFromFirstRead(t)
	r.gh.posts[fmt.Sprintf("%s/issues/%d/comments", base, prK.Number)] = mustJSON(map[string]any{"id": 77, "html_url": "u77", "updated_at": t0})

	// Act
	_, err := r.w.Comment(context.Background(), r.ws, prK, nil, "done")

	// Assert
	subs, readErr := r.db.WorkspacePullRequestSubscriptions(context.Background(), r.ws)
	if err != nil || readErr != nil || len(subs) != 1 || subs[0].Told != sub.Told {
		t.Fatalf("Comment = %v; subscriptions %+v, %v; want the snapshot left for the first read", err, subs, readErr)
	}
}
