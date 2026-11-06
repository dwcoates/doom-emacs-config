package wsm

import (
	"context"
	"errors"
	"reflect"
	"testing"
	"time"

	"agent-repld/internal/dlog"
)

// prKey is the pull request every test follows.
var prKey = PullRequestKey{Owner: "ChessCom", Repo: "explanation-engine", Number: 42}

// requested is SUBSCRIBER's requested subscription to prKey's EVENTS, told TOLD.
func requested(subscriber WorkspaceID, told string, events ...PullRequestEvent) PullRequestSubscription {
	return PullRequestSubscription{Subscriber: subscriber, PullRequest: prKey, Events: NewPullRequestEvents(events...),
		Origin: PullRequestOriginRequested, SubscribedAt: instant, Told: told}
}

// subsOf reads SUBSCRIBER's subscriptions, failing the test on an error.
func subsOf(t *testing.T, s *store, subscriber WorkspaceID) []PullRequestSubscription {
	t.Helper()
	subs, err := s.WorkspacePullRequestSubscriptions(context.Background(), subscriber)
	if err != nil {
		t.Fatalf("WorkspacePullRequestSubscriptions: %v", err)
	}
	return subs
}

func TestPullRequestKeyRoundTrips(t *testing.T) {
	tests := []struct {
		name string
		text string
		want PullRequestKey
		ok   bool
	}{
		{name: "a key", text: "ChessCom/explanation-engine#42", want: prKey, ok: true},
		{name: "no number", text: "ChessCom/explanation-engine", ok: false},
		{name: "no repo", text: "ChessCom#42", ok: false},
		{name: "a zero number", text: "a/b#0", ok: false},
		{name: "a padded number", text: "a/b#042", ok: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got, err := ParsePullRequestKey(tt.text)

			// Assert
			if (err == nil) != tt.ok || got != tt.want {
				t.Fatalf("ParsePullRequestKey(%q) = %v, %v; want %v ok=%v", tt.text, got, err, tt.want, tt.ok)
			}
			if tt.ok && got.String() != tt.text {
				t.Fatalf("String = %q, want %q", got.String(), tt.text)
			}
		})
	}
}

func TestPullRequestEventsAreASet(t *testing.T) {
	tests := []struct {
		name      string
		set       PullRequestEvents
		wantValid bool
		wantName  string
	}{
		{name: "every event", set: AllPullRequestEvents(), wantValid: true, wantName: "comment,review,review_comment,state,checks,merge_queue"},
		{name: "the merge queue is a member", set: NewPullRequestEvents(PullRequestEventMergeQueue, PullRequestEventState), wantValid: true, wantName: "state,merge_queue"},
		{name: "a repeat is one member", set: NewPullRequestEvents(PullRequestEventChecks, PullRequestEventChecks), wantValid: true, wantName: "checks"},
		{name: "no event", set: NewPullRequestEvents(), wantValid: false},
		{name: "an undeclared event", set: NewPullRequestEvents(9), wantValid: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Assert
			if tt.set.Valid() != tt.wantValid {
				t.Fatalf("Valid = %v, want %v", tt.set.Valid(), tt.wantValid)
			}
			if tt.wantValid && tt.set.String() != tt.wantName {
				t.Fatalf("String = %q, want %q", tt.set.String(), tt.wantName)
			}
		})
	}
}

func TestAPullRequestSubscriptionRoundTripsAndKeepsWhatWasTold(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	first := requested(ws.ID, "snapshot-1", PullRequestEventComment)
	second := requested(ws.ID, "ignored", PullRequestEventReview, PullRequestEventChecks)

	// Act
	replacedFirst, errFirst := s.SubscribePullRequest(context.Background(), first)
	replacedSecond, errSecond := s.SubscribePullRequest(context.Background(), second)

	// Assert
	if errFirst != nil || replacedFirst || errSecond != nil || !replacedSecond {
		t.Fatalf("SubscribePullRequest = (%v, %v), (%v, %v); want fresh then replaced", replacedFirst, errFirst, replacedSecond, errSecond)
	}
	want := first
	want.Events = second.Events
	if got := subsOf(t, s, ws.ID); !reflect.DeepEqual(got, []PullRequestSubscription{want}) {
		t.Fatalf("subscriptions = %+v, want %+v", got, want)
	}
}

func TestSubscribePullRequestRefuses(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(t *testing.T, s *store) PullRequestSubscription
		want    error
	}{
		{name: "an unregistered subscriber", arrange: func(*testing.T, *store) PullRequestSubscription {
			return requested("absent", "", PullRequestEventComment)
		}, want: ErrNotFound},
		{name: "a closed subscriber", arrange: func(t *testing.T, s *store) PullRequestSubscription {
			ws := testWorkspace(t, s)
			if err := s.MarkClosed(context.Background(), ws.ID, Closure{At: instant, By: CloseByUser}); err != nil {
				t.Fatalf("MarkClosed: %v", err)
			}
			return requested(ws.ID, "", PullRequestEventComment)
		}, want: ErrSubscriberClosed},
		{name: "a subscription whose pull request ended", arrange: func(t *testing.T, s *store) PullRequestSubscription {
			ws := testWorkspace(t, s)
			sub := requested(ws.ID, "a", PullRequestEventComment)
			mustSubscribePR(t, s, sub)
			mustAdvance(t, s, PullRequestAdvance{Subscriber: ws.ID, PullRequest: prKey, From: "a", To: "b", End: &PullRequestEnd{}, ReadAt: instant})
			return sub
		}, want: ErrSubscriptionEnded},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			sub := tt.arrange(t, s)

			// Act
			_, err := s.SubscribePullRequest(context.Background(), sub)

			// Assert
			if !errors.Is(err, tt.want) {
				t.Fatalf("SubscribePullRequest = %v, want %v", err, tt.want)
			}
		})
	}
}

func TestSubscribePullRequestFailsHardOnAContradictorySubscription(t *testing.T) {
	tests := []struct {
		name string
		sub  func(WorkspaceID) PullRequestSubscription
	}{
		{name: "no events", sub: func(ws WorkspaceID) PullRequestSubscription { return requested(ws, "") }},
		{name: "no pull request", sub: func(ws WorkspaceID) PullRequestSubscription {
			sub := requested(ws, "", PullRequestEventComment)
			sub.PullRequest = PullRequestKey{}
			return sub
		}},
		{name: "no origin", sub: func(ws WorkspaceID) PullRequestSubscription {
			sub := requested(ws, "", PullRequestEventComment)
			sub.Origin = ""
			return sub
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, log := testStore(t)
			ws := testWorkspace(t, s)

			// Act
			_, err := s.SubscribePullRequest(context.Background(), tt.sub(ws.ID))

			// Assert
			if err == nil || len(subsOf(t, s, ws.ID)) != 0 {
				t.Fatalf("SubscribePullRequest = %v with %d rows, want refused and nothing written", err, len(subsOf(t, s, ws.ID)))
			}
			if !hasRecord(log, "error", "daemon.wsm.subscribe_pull_request", "refused a pull-request subscription its caller should never have made") {
				t.Fatalf("no error record of the refusal: %+v", log.Records())
			}
		})
	}
}

func TestAdvancePullRequestToldComparesAndSets(t *testing.T) {
	tests := []struct {
		name        string
		from        string
		wantApplied bool
		wantTold    string
		wantOwed    []string
	}{
		{name: "the snapshot read still stands", from: "a", wantApplied: true, wantTold: "b", wantOwed: []string{"one.", "two."}},
		{name: "another write moved the snapshot", from: "stale", wantApplied: false, wantTold: "a"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			ws := testWorkspace(t, s)
			mustSubscribePR(t, s, requested(ws.ID, "a", PullRequestEventComment))

			// Act
			applied, err := s.AdvancePullRequestTold(context.Background(), PullRequestAdvance{
				Subscriber: ws.ID, PullRequest: prKey, From: tt.from, To: "b", Owed: []string{"one.", "two."}, ReadAt: instant})

			// Assert
			if err != nil || applied != tt.wantApplied {
				t.Fatalf("AdvancePullRequestTold = %v, %v; want %v", applied, err, tt.wantApplied)
			}
			if got := subsOf(t, s, ws.ID)[0].Told; got != tt.wantTold {
				t.Fatalf("told = %q, want %q", got, tt.wantTold)
			}
			owed, err := s.OwedPullRequestNotices(context.Background())
			if err != nil {
				t.Fatalf("OwedPullRequestNotices: %v", err)
			}
			var got []string
			if len(owed) == 1 {
				got = owed[0].Pending
			}
			if !reflect.DeepEqual(got, tt.wantOwed) {
				t.Fatalf("owed = %v, want %v", got, tt.wantOwed)
			}
		})
	}
}

func TestAdvancePullRequestToldEndsTheSubscriptionAndClearsItsFailure(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	mustSubscribePR(t, s, requested(ws.ID, "a", PullRequestEventState))
	if _, err := s.RecordPullRequestReadFailure(context.Background(), prKey, PullRequestFailure{Since: instant, Kind: PullRequestFailureUnreachable, Detail: "down"}); err != nil {
		t.Fatalf("RecordPullRequestReadFailure: %v", err)
	}
	readAt := instant.Add(time.Minute)

	// Act
	mustAdvance(t, s, PullRequestAdvance{Subscriber: ws.ID, PullRequest: prKey, From: "a", To: "b",
		End: &PullRequestEnd{Merged: true, MergeSHA: "abc"}, ReadAt: readAt})

	// Assert
	sub := subsOf(t, s, ws.ID)[0]
	if !reflect.DeepEqual(sub.Ended, &PullRequestEnd{Merged: true, MergeSHA: "abc"}) || sub.Failing != nil || !sub.LastRead.Equal(readAt) {
		t.Fatalf("subscription = %+v, want ended by a merge, healthy, read at %v", sub, readAt)
	}
	live, err := s.LivePullRequestSubscriptions(context.Background())
	if err != nil || len(live) != 0 {
		t.Fatalf("LivePullRequestSubscriptions = %v, %v; want none", live, err)
	}
}

func TestRecordPullRequestReadFailureAnswersOnlyTheStretchesItBegan(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	first := testWorkspace(t, s)
	second := testWorkspaceNamed(t, s, "second")
	mustSubscribePR(t, s, requested(first.ID, "a", PullRequestEventComment))
	ctx := context.Background()

	// Act
	beganFirst, errFirst := s.RecordPullRequestReadFailure(ctx, prKey, PullRequestFailure{Since: instant, Kind: PullRequestFailureNotFound})
	mustSubscribePR(t, s, requested(second.ID, "a", PullRequestEventComment))
	reset := instant.Add(time.Hour)
	beganSecond, errSecond := s.RecordPullRequestReadFailure(ctx, prKey, PullRequestFailure{Since: instant.Add(time.Minute), Kind: PullRequestFailureRateLimited, ResetsAt: reset})

	// Assert
	if errFirst != nil || errSecond != nil {
		t.Fatalf("RecordPullRequestReadFailure = %v, %v", errFirst, errSecond)
	}
	if !reflect.DeepEqual(beganFirst, []WorkspaceID{first.ID}) || !reflect.DeepEqual(beganSecond, []WorkspaceID{second.ID}) {
		t.Fatalf("began = %v then %v, want each subscriber once", beganFirst, beganSecond)
	}
	want := &PullRequestFailure{Since: instant, Kind: PullRequestFailureRateLimited, ResetsAt: reset}
	if got := subsOf(t, s, first.ID)[0].Failing; !reflect.DeepEqual(got, want) {
		t.Fatalf("first's failure = %+v, want its stretch's start kept with the latest cause %+v", got, want)
	}
}

func TestRecordPullRequestNoticeSubmittedKeepsSentencesOwedSince(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	mustSubscribePR(t, s, requested(ws.ID, "a", PullRequestEventComment))
	mustAdvance(t, s, PullRequestAdvance{Subscriber: ws.ID, PullRequest: prKey, From: "a", To: "b", Owed: []string{"one."}, ReadAt: instant})
	mustAdvance(t, s, PullRequestAdvance{Subscriber: ws.ID, PullRequest: prKey, From: "b", To: "c", Owed: []string{"two."}, Status: "open now.", ReadAt: instant})

	// Act
	err := s.RecordPullRequestNoticeSubmitted(context.Background(), ws.ID, prKey, "turn-1", []string{"one."}, 1)

	// Assert
	if err != nil {
		t.Fatalf("RecordPullRequestNoticeSubmitted: %v", err)
	}
	owed, err := s.OwedPullRequestNotices(context.Background())
	want := []PullRequestOwed{{Subscriber: ws.ID, PullRequest: prKey, Turn: "turn-1", Sentences: []string{"one."}, Pending: []string{"two."}, Status: "open now."}}
	if err != nil || !reflect.DeepEqual(owed, want) {
		t.Fatalf("OwedPullRequestNotices = %+v, %v; want %+v", owed, err, want)
	}
}

func TestRecordPullRequestNoticeSubmittedRefusesANoticeThatSaysNothing(t *testing.T) {
	// Arrange
	s, log := testStore(t)
	ws := testWorkspace(t, s)

	// Act
	err := s.RecordPullRequestNoticeSubmitted(context.Background(), ws.ID, prKey, "turn-1", nil, 0)

	// Assert
	if err == nil || !hasRecord(log, "error", "daemon.wsm.record_pull_request_notice_submitted", "refused an untold pull-request notice that says no change") {
		t.Fatalf("RecordPullRequestNoticeSubmitted = %v, want refused and recorded: %+v", err, log.Records())
	}
}

func TestClosingASubscriberEndsItsPullRequestSubscriptionsAndNotices(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	closing := testWorkspace(t, s)
	other := testWorkspaceNamed(t, s, "other")
	for _, ws := range []WorkspaceID{closing.ID, other.ID} {
		mustSubscribePR(t, s, requested(ws, "a", PullRequestEventComment))
		mustAdvance(t, s, PullRequestAdvance{Subscriber: ws, PullRequest: prKey, From: "a", To: "b", Owed: []string{"one."}, ReadAt: instant})
	}

	// Act
	err := s.MarkClosed(context.Background(), closing.ID, Closure{At: instant, By: CloseByUser})

	// Assert
	if err != nil {
		t.Fatalf("MarkClosed: %v", err)
	}
	if len(subsOf(t, s, closing.ID)) != 0 || len(subsOf(t, s, other.ID)) != 1 {
		t.Fatalf("subscriptions: closing %d, other %d; want 0 and 1", len(subsOf(t, s, closing.ID)), len(subsOf(t, s, other.ID)))
	}
	owed, err := s.OwedPullRequestNotices(context.Background())
	if err != nil || len(owed) != 1 || owed[0].Subscriber != other.ID {
		t.Fatalf("owed = %+v, %v; want only the other workspace's", owed, err)
	}
}

func TestAnOwnedPullRequestLinkFollowsItsPullRequest(t *testing.T) {
	url := "https://github.com/ChessCom/explanation-engine/pull/42"
	pr := prKey
	tests := []struct {
		name       string
		then       func(t *testing.T, s *store, ws WorkspaceID) []WorkspaceLink
		wantFollow bool
	}{
		{name: "storing it owned subscribes", then: func(t *testing.T, s *store, ws WorkspaceID) []WorkspaceLink {
			return linksNow(t, s, ws)
		}, wantFollow: true},
		{name: "storing it again owned keeps the one subscription", then: func(t *testing.T, s *store, ws WorkspaceID) []WorkspaceLink {
			return mustSetLink(t, s, ws, ownedSet(ws, url, pr))
		}, wantFollow: true},
		{name: "demoting it to referenced ends the subscription", then: func(t *testing.T, s *store, ws WorkspaceID) []WorkspaceLink {
			return mustSetLink(t, s, ws, LinkSet{URL: url, Label: "the pull request", PullRequest: &pr})
		}},
		{name: "removing it ends the subscription", then: func(t *testing.T, s *store, ws WorkspaceID) []WorkspaceLink {
			if _, err := s.RemoveWorkspaceLink(context.Background(), ws, url); err != nil {
				t.Fatalf("RemoveWorkspaceLink: %v", err)
			}
			return nil
		}},
		{name: "clearing the links ends the subscription", then: func(t *testing.T, s *store, ws WorkspaceID) []WorkspaceLink {
			if err := s.ClearWorkspaceLinks(context.Background(), ws); err != nil {
				t.Fatalf("ClearWorkspaceLinks: %v", err)
			}
			return nil
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			ws := testWorkspace(t, s).ID
			mustSetLink(t, s, ws, ownedSet(ws, url, pr))

			// Act
			links := tt.then(t, s, ws)

			// Assert
			subs := subsOf(t, s, ws)
			if got := len(subs) == 1; got != tt.wantFollow {
				t.Fatalf("subscriptions = %+v, want following %v", subs, tt.wantFollow)
			}
			if tt.wantFollow && (len(links) != 1 || !links[0].Owned || links[0].Follow == nil || links[0].Follow.Origin != PullRequestOriginOwnedLink) {
				t.Fatalf("links = %+v, want the owned link carrying its subscription", links)
			}
		})
	}
}

func TestUnsubscribingLeavesTheOwnedLinkUnfollowed(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s).ID
	url := "https://github.com/ChessCom/explanation-engine/pull/42"
	mustSetLink(t, s, ws, ownedSet(ws, url, prKey))

	// Act
	ended, err := s.UnsubscribePullRequest(context.Background(), ws, prKey)

	// Assert
	links := linksNow(t, s, ws)
	if err != nil || !ended || len(links) != 1 || !links[0].Owned || links[0].Follow != nil {
		t.Fatalf("UnsubscribePullRequest = %v, %v; links %+v; want the link still owned and unfollowed", ended, err, links)
	}
}

func TestSetWorkspaceLinkFailsHardOnASubscriptionForAnUnownedLink(t *testing.T) {
	// Arrange
	s, log := testStore(t)
	ws := testWorkspace(t, s).ID
	set := ownedSet(ws, "https://github.com/ChessCom/explanation-engine/pull/42", prKey)
	set.Owned = false

	// Act
	_, err := s.SetWorkspaceLink(context.Background(), ws, set)

	// Assert
	if err == nil || len(linksNow(t, s, ws)) != 0 || !hasRecord(log, "error", "daemon.wsm.set_workspace_link", "refused a link its caller should never have made") {
		t.Fatalf("SetWorkspaceLink = %v, want refused, recorded and nothing written", err)
	}
}

// ownedSet is an owned link to PR at URL that subscribes WS.
func ownedSet(ws WorkspaceID, url string, pr PullRequestKey) LinkSet {
	sub := PullRequestSubscription{Subscriber: ws, PullRequest: pr, Events: AllPullRequestEvents(), Origin: PullRequestOriginOwnedLink, SubscribedAt: instant, Told: "a"}
	return LinkSet{URL: url, Label: "the pull request", Owned: true, PullRequest: &pr, Subscribe: &sub}
}

func mustSetLink(t *testing.T, s *store, ws WorkspaceID, set LinkSet) []WorkspaceLink {
	t.Helper()
	links, err := s.SetWorkspaceLink(context.Background(), ws, set)
	if err != nil {
		t.Fatalf("SetWorkspaceLink: %v", err)
	}
	return links
}

func linksNow(t *testing.T, s *store, ws WorkspaceID) []WorkspaceLink {
	t.Helper()
	links, err := s.WorkspaceLinks(context.Background(), ws)
	if err != nil {
		t.Fatalf("WorkspaceLinks: %v", err)
	}
	return links
}

func mustSubscribePR(t *testing.T, s *store, sub PullRequestSubscription) {
	t.Helper()
	if _, err := s.SubscribePullRequest(context.Background(), sub); err != nil {
		t.Fatalf("SubscribePullRequest: %v", err)
	}
}

func mustAdvance(t *testing.T, s *store, a PullRequestAdvance) {
	t.Helper()
	if applied, err := s.AdvancePullRequestTold(context.Background(), a); err != nil || !applied {
		t.Fatalf("AdvancePullRequestTold = %v, %v; want applied", applied, err)
	}
}

func hasRecord(log *dlog.TestLogger, level, op, message string) bool {
	_, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: level, Operation: op, Message: message})
	return ok
}

func TestOwePullRequestNoticeKeepsTheStatusAndWhatWasTold(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	mustSubscribePR(t, s, requested(ws.ID, "a", PullRequestEventComment))
	mustAdvance(t, s, PullRequestAdvance{Subscriber: ws.ID, PullRequest: prKey, From: "a", To: "b", Owed: []string{"one."}, Status: "open now.", ReadAt: instant})

	// Act
	err := s.OwePullRequestNotice(context.Background(), ws.ID, prKey, []string{"reads are failing."})

	// Assert
	owed, readErr := s.OwedPullRequestNotices(context.Background())
	want := []PullRequestOwed{{Subscriber: ws.ID, PullRequest: prKey, Sentences: []string{}, Pending: []string{"one.", "reads are failing."}, Status: "open now."}}
	if err != nil || readErr != nil || !reflect.DeepEqual(owed, want) || subsOf(t, s, ws.ID)[0].Told != "b" {
		t.Fatalf("owed = %+v (%v, %v), want %+v with told kept", owed, err, readErr, want)
	}
}

func TestAnAdvanceNoReadMadeLeavesTheReadHealth(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	mustSubscribePR(t, s, requested(ws.ID, "a", PullRequestEventComment))
	failure := PullRequestFailure{Since: instant, Kind: PullRequestFailureUnreachable, Detail: "down"}
	if _, err := s.RecordPullRequestReadFailure(context.Background(), prKey, failure); err != nil {
		t.Fatalf("RecordPullRequestReadFailure: %v", err)
	}

	// Act
	mustAdvance(t, s, PullRequestAdvance{Subscriber: ws.ID, PullRequest: prKey, From: "a", To: "b"})

	// Assert
	sub := subsOf(t, s, ws.ID)[0]
	if sub.Told != "b" || !reflect.DeepEqual(sub.Failing, &failure) || !sub.LastRead.IsZero() {
		t.Fatalf("subscription = %+v, want told moved and the failing stretch kept", sub)
	}
}

func TestAnOwnedLinkToAnEndedPullRequestIsRecordedEnded(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s).ID
	set := ownedSet(ws, "https://github.com/ChessCom/explanation-engine/pull/42", prKey)
	set.Subscribe.Ended = &PullRequestEnd{Merged: true, MergeSHA: "abc"}

	// Act
	links := mustSetLink(t, s, ws, set)

	// Assert
	live, err := s.LivePullRequestSubscriptions(context.Background())
	if err != nil || len(live) != 0 || links[0].Follow == nil || !reflect.DeepEqual(links[0].Follow.Ended, set.Subscribe.Ended) {
		t.Fatalf("links = %+v, live %v (%v); want the link's follow ended by the merge and nothing polled", links, live, err)
	}
}
