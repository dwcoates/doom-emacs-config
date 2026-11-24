package wsm

import (
	"context"
	"errors"
	"reflect"
	"testing"
)

// followedPRURL is the url the followed workspace stores prKey under.
const followedPRURL = "https://github.com/ChessCom/explanation-engine/pull/42"

// followedSub is the subscription a follower is expected to hold to prKey.
func followedSub(subscriber WorkspaceID) PullRequestSubscription {
	return PullRequestSubscription{Subscriber: subscriber, PullRequest: prKey, Events: NewPullRequestEvents(PullRequestEventMerged),
		Origin: PullRequestOriginFollowedWorkspace, SubscribedAt: instant}
}

// mustFollow subscribes SUBSCRIBER to FOLLOWED's lifecycle edges.
func mustFollow(t *testing.T, s *store, subscriber, followed WorkspaceID) {
	t.Helper()
	if _, err := s.SubscribeWorkspace(context.Background(), following(subscriber, followed, WorkspaceEventLanded)); err != nil {
		t.Fatalf("SubscribeWorkspace: %v", err)
	}
}

// followedLink is the reference link a follower is expected to hold to
// prKey, carrying SUB.
func followedLink(sub *PullRequestSubscription) WorkspaceLink {
	pr := prKey
	return WorkspaceLink{URL: followedPRURL, Label: "the pull request", Shown: true, PullRequest: &pr, Follow: sub}
}

func TestSubscribingToAWorkspaceFollowsTheMergeOfItsOwnedPullRequest(t *testing.T) {
	// Arrange
	s, log := testStore(t)
	parent := testWorkspace(t, s).ID
	child := testWorkspaceNamed(t, s, "child").ID
	mustSetLink(t, s, child, ownedSet(child, followedPRURL, prKey))

	// Act
	mustFollow(t, s, parent, child)

	// Assert
	want := followedSub(parent)
	if got := subsOf(t, s, parent); !reflect.DeepEqual(got, []PullRequestSubscription{want}) {
		t.Fatalf("subscriptions = %+v, want %+v", got, want)
	}
	if got := linksNow(t, s, parent); !reflect.DeepEqual(got, []WorkspaceLink{followedLink(&want)}) {
		t.Fatalf("links = %+v, want the child's pull request as a followed reference", got)
	}
	if !hasRecord(log, "info", "daemon.wsm.subscribe_workspace", "subscribed a follower to the merge of a pull request its followed workspace owns") {
		t.Fatal("no info record of the follow")
	}
}

func TestStoringAnOwnedPullRequestFollowsItForTheWorkspacesFollowers(t *testing.T) {
	// Arrange
	s, log := testStore(t)
	parent := testWorkspace(t, s).ID
	child := testWorkspaceNamed(t, s, "child").ID
	bystander := testWorkspaceNamed(t, s, "bystander").ID
	mustFollow(t, s, parent, child)

	// Act
	mustSetLink(t, s, child, ownedSet(child, followedPRURL, prKey))

	// Assert
	subs := subsOf(t, s, parent)
	if len(subs) != 1 || subs[0].Events != NewPullRequestEvents(PullRequestEventMerged) || subs[0].Origin != PullRequestOriginFollowedWorkspace || subs[0].Told != "" {
		t.Fatalf("follower subscriptions = %+v, want the merge alone, unanchored", subs)
	}
	if links := linksNow(t, s, parent); len(links) != 1 || links[0].Owned || links[0].URL != followedPRURL || links[0].Label != "the pull request" {
		t.Fatalf("follower links = %+v, want the child's pull request as a reference under its label", links)
	}
	if got := subsOf(t, s, bystander); len(got) != 0 {
		t.Fatalf("bystander subscriptions = %+v, want none", got)
	}
	if !hasRecord(log, "info", "daemon.wsm.set_workspace_link", "subscribed a follower to the merge of a pull request its followed workspace owns") {
		t.Fatal("no info record of the follow")
	}
}

func TestAFollowedWorkspacesPullRequestIsNotFollowed(t *testing.T) {
	ended := func(t *testing.T, s *store, child WorkspaceID) {
		mustSetLink(t, s, child, ownedSet(child, followedPRURL, prKey))
		mustAdvance(t, s, PullRequestAdvance{Subscriber: child, PullRequest: prKey, From: "a", To: "b", End: &PullRequestEnd{Merged: true, MergeSHA: "sha"}, ReadAt: instant})
	}
	tests := []struct {
		name    string
		arrange func(t *testing.T, s *store, child WorkspaceID)
	}{
		{name: "when it is stored as a reference", arrange: func(t *testing.T, s *store, child WorkspaceID) {
			pr := prKey
			mustSetLink(t, s, child, LinkSet{URL: followedPRURL, Label: "the pull request", PullRequest: &pr})
		}},
		{name: "when its owned subscription already ended", arrange: ended},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			parent := testWorkspace(t, s).ID
			child := testWorkspaceNamed(t, s, "child").ID
			tt.arrange(t, s, child)

			// Act
			mustFollow(t, s, parent, child)

			// Assert
			if subs, links := subsOf(t, s, parent), linksNow(t, s, parent); len(subs) != 0 || len(links) != 0 {
				t.Fatalf("follower subscriptions %+v, links %+v; want none", subs, links)
			}
		})
	}
}

func TestAFollowersOwnHoldOnThePullRequestIsKept(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	parent := testWorkspace(t, s).ID
	child := testWorkspaceNamed(t, s, "child").ID
	held := requested(parent, "told", PullRequestEventComment)
	mustSubscribePR(t, s, held)
	pr := prKey
	if _, err := s.AddWorkspaceLinkIfAbsent(context.Background(), parent, followedPRURL, "my own label", &pr); err != nil {
		t.Fatalf("AddWorkspaceLinkIfAbsent: %v", err)
	}
	mustSetLink(t, s, child, ownedSet(child, followedPRURL, prKey))

	// Act
	mustFollow(t, s, parent, child)

	// Assert
	if got := subsOf(t, s, parent); !reflect.DeepEqual(got, []PullRequestSubscription{held}) {
		t.Fatalf("subscriptions = %+v, want the held one unchanged", got)
	}
	if links := linksNow(t, s, parent); len(links) != 1 || links[0].Label != "my own label" {
		t.Fatalf("links = %+v, want the held link unchanged", links)
	}
}

func TestResubscribingTakesTheFollowOnce(t *testing.T) {
	// Arrange
	s, log := testStore(t)
	parent := testWorkspace(t, s).ID
	child := testWorkspaceNamed(t, s, "child").ID
	mustSetLink(t, s, child, ownedSet(child, followedPRURL, prKey))
	mustFollow(t, s, parent, child)

	// Act
	mustFollow(t, s, parent, child)

	// Assert
	if subs, links := subsOf(t, s, parent), linksNow(t, s, parent); len(subs) != 1 || len(links) != 1 {
		t.Fatalf("follower subscriptions %+v, links %+v; want one each", subs, links)
	}
	n := 0
	for _, r := range log.Records() {
		if r.Message == "subscribed a follower to the merge of a pull request its followed workspace owns" {
			n++
		}
	}
	if n != 1 {
		t.Fatalf("follow records = %d, want 1", n)
	}
}

func TestARefusedSubscriptionFollowsNothing(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	parent := testWorkspace(t, s)
	child := testWorkspaceNamed(t, s, "child").ID
	mustSetLink(t, s, child, ownedSet(child, followedPRURL, prKey))
	if err := s.MarkClosed(context.Background(), parent.ID, Closure{At: instant, By: CloseByUser}); err != nil {
		t.Fatalf("MarkClosed: %v", err)
	}

	// Act
	_, err := s.SubscribeWorkspace(context.Background(), following(parent.ID, child, WorkspaceEventLanded))

	// Assert
	if !errors.Is(err, ErrSubscriberClosed) {
		t.Fatalf("SubscribeWorkspace = %v, want %v", err, ErrSubscriberClosed)
	}
	if subs, links := subsOf(t, s, parent.ID), linksNow(t, s, parent.ID); len(subs) != 0 || len(links) != 0 {
		t.Fatalf("follower subscriptions %+v, links %+v; want none", subs, links)
	}
}
