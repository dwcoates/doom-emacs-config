package wsm

import (
	"context"
	"errors"
	"reflect"
	"testing"

	"agent-repld/internal/dlog"
)

// following is SUBSCRIBER's subscription to FOLLOWED's EVENTS, taken at
// instant.
func following(subscriber, followed WorkspaceID, events ...WorkspaceEvent) WorkspaceSubscription {
	return WorkspaceSubscription{Subscriber: subscriber, Followed: followed, Events: NewWorkspaceEvents(events...), SubscribedAt: instant}
}

// subscribersOf reads FOLLOWED's subscriptions back, failing the test on an
// error.
func subscribersOf(t *testing.T, s *store, followed WorkspaceID) []WorkspaceSubscription {
	t.Helper()
	subs, err := s.WorkspaceSubscribers(context.Background(), followed)
	if err != nil {
		t.Fatalf("WorkspaceSubscribers: %v", err)
	}
	return subs
}

func TestWorkspaceEventsAreASet(t *testing.T) {
	tests := []struct {
		name      string
		events    []WorkspaceEvent
		wantValid bool
		wantName  string
	}{
		{name: "one event", events: []WorkspaceEvent{WorkspaceEventLanded}, wantValid: true, wantName: "landed"},
		{name: "a repeat is one member", events: []WorkspaceEvent{WorkspaceEventClosed, WorkspaceEventClosed}, wantValid: true, wantName: "closed"},
		{name: "members name in order", events: []WorkspaceEvent{WorkspaceEventClosed, WorkspaceEventTurnStarted}, wantValid: true, wantName: "turn_started,closed"},
		{name: "no event", wantValid: false},
		{name: "an undeclared event", events: []WorkspaceEvent{9}, wantValid: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			set := NewWorkspaceEvents(tt.events...)

			// Assert
			if set.Valid() != tt.wantValid {
				t.Errorf("Valid = %v, want %v", set.Valid(), tt.wantValid)
			}
			if tt.wantValid && set.String() != tt.wantName {
				t.Errorf("String = %q, want %q", set.String(), tt.wantName)
			}
		})
	}
}

func TestAWorkspaceSubscriptionRoundTripsAndReplaces(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	subscriber := testWorkspace(t, s)
	followed := testWorkspaceNamed(t, s, "followed")
	first := following(subscriber.ID, followed.ID, WorkspaceEventLanded)
	second := following(subscriber.ID, followed.ID, WorkspaceEventTurnEnded, WorkspaceEventClosed)

	// Act
	replacedFirst, errFirst := s.SubscribeWorkspace(context.Background(), first)
	replacedSecond, errSecond := s.SubscribeWorkspace(context.Background(), second)

	// Assert
	if errFirst != nil || replacedFirst || errSecond != nil || !replacedSecond {
		t.Fatalf("SubscribeWorkspace = (%v, %v), (%v, %v); want fresh then replaced", replacedFirst, errFirst, replacedSecond, errSecond)
	}
	if got := subscribersOf(t, s, followed.ID); !reflect.DeepEqual(got, []WorkspaceSubscription{second}) {
		t.Fatalf("subscribers = %+v, want %+v", got, second)
	}
}

func TestSubscribeWorkspaceRefuses(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(t *testing.T, s *store) WorkspaceSubscription
		want    error
	}{
		{name: "an unregistered subscriber", arrange: func(t *testing.T, s *store) WorkspaceSubscription {
			return following("ws-none", testWorkspace(t, s).ID, WorkspaceEventLanded)
		}, want: ErrNotFound},
		{name: "a closed subscriber", arrange: func(t *testing.T, s *store) WorkspaceSubscription {
			subscriber := testWorkspace(t, s)
			if err := s.MarkClosed(context.Background(), subscriber.ID, Closure{At: instant, By: CloseByUser}); err != nil {
				t.Fatalf("MarkClosed: %v", err)
			}
			return following(subscriber.ID, testWorkspaceNamed(t, s, "followed").ID, WorkspaceEventLanded)
		}, want: ErrSubscriberClosed},
		{name: "an unregistered followed workspace", arrange: func(t *testing.T, s *store) WorkspaceSubscription {
			return following(testWorkspace(t, s).ID, "ws-none", WorkspaceEventLanded)
		}, want: ErrUnknownFollowed},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			sub := tt.arrange(t, s)

			// Act
			_, err := s.SubscribeWorkspace(context.Background(), sub)

			// Assert
			if !errors.Is(err, tt.want) {
				t.Fatalf("SubscribeWorkspace = %v, want %v", err, tt.want)
			}
			if got := subscribersOf(t, s, sub.Followed); len(got) != 0 {
				t.Fatalf("subscribers = %+v, want none recorded", got)
			}
		})
	}
}

func TestSubscribeWorkspaceFailsHardOnAContradictorySubscription(t *testing.T) {
	tests := []struct {
		name string
		sub  func(a, b WorkspaceID) WorkspaceSubscription
	}{
		{name: "no events", sub: func(a, b WorkspaceID) WorkspaceSubscription { return following(a, b) }},
		{name: "a workspace following itself", sub: func(a, _ WorkspaceID) WorkspaceSubscription { return following(a, a, WorkspaceEventLanded) }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, log := testStore(t)
			a, b := testWorkspace(t, s), testWorkspaceNamed(t, s, "b")

			// Act
			_, err := s.SubscribeWorkspace(context.Background(), tt.sub(a.ID, b.ID))

			// Assert
			if err == nil {
				t.Fatal("SubscribeWorkspace = nil, want a hard failure")
			}
			if _, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: "error", Operation: "daemon.wsm.subscribe_workspace",
				Message: "refused a workspace subscription its caller should never have made"}); !ok {
				t.Errorf("no error record: %+v", log.Records())
			}
		})
	}
}

func TestUnsubscribeWorkspaceAnswersWhetherOneStood(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	subscriber := testWorkspace(t, s)
	followed := testWorkspaceNamed(t, s, "followed")
	if _, err := s.SubscribeWorkspace(context.Background(), following(subscriber.ID, followed.ID, WorkspaceEventLanded)); err != nil {
		t.Fatalf("SubscribeWorkspace: %v", err)
	}

	// Act
	first, errFirst := s.UnsubscribeWorkspace(context.Background(), subscriber.ID, followed.ID)
	second, errSecond := s.UnsubscribeWorkspace(context.Background(), subscriber.ID, followed.ID)

	// Assert
	if errFirst != nil || !first || errSecond != nil || second {
		t.Fatalf("UnsubscribeWorkspace = (%v, %v), (%v, %v); want ended then nothing", first, errFirst, second, errSecond)
	}
}

func TestClosingASubscriberEndsItsWorkspaceSubscriptionsOnly(t *testing.T) {
	// Arrange
	s, log := testStore(t)
	closing := testWorkspace(t, s)
	other := testWorkspaceNamed(t, s, "other")
	followed := testWorkspaceNamed(t, s, "followed")
	kept := following(other.ID, followed.ID, WorkspaceEventClosed)
	for _, sub := range []WorkspaceSubscription{following(closing.ID, followed.ID, WorkspaceEventLanded), kept} {
		if _, err := s.SubscribeWorkspace(context.Background(), sub); err != nil {
			t.Fatalf("SubscribeWorkspace: %v", err)
		}
	}

	// Act
	err := s.MarkClosed(context.Background(), closing.ID, Closure{At: instant, By: CloseByUser})

	// Assert
	if err != nil {
		t.Fatalf("MarkClosed: %v", err)
	}
	if got := subscribersOf(t, s, followed.ID); !reflect.DeepEqual(got, []WorkspaceSubscription{kept}) {
		t.Fatalf("subscribers = %+v, want only the other workspace's", got)
	}
	if record, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: "info", Message: "the close ended the workspace's subscriptions to other workspaces"}); !ok || record.Context["subscriptions"] != int64(1) {
		t.Fatalf("no record of the one ended subscription: %+v", log.Records())
	}
}

func TestForgettingEitherWorkspaceEndsTheSubscription(t *testing.T) {
	tests := []struct {
		name   string
		forget func(subscriber, followed Workspace) WorkspaceID
	}{
		{name: "the subscriber", forget: func(subscriber, _ Workspace) WorkspaceID { return subscriber.ID }},
		{name: "the followed workspace", forget: func(_, followed Workspace) WorkspaceID { return followed.ID }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			subscriber := testWorkspace(t, s)
			followed := testWorkspaceNamed(t, s, "followed")
			if _, err := s.SubscribeWorkspace(context.Background(), following(subscriber.ID, followed.ID, WorkspaceEventLanded)); err != nil {
				t.Fatalf("SubscribeWorkspace: %v", err)
			}

			// Act
			_, err := s.Forget(context.Background(), tt.forget(subscriber, followed))

			// Assert
			if err != nil {
				t.Fatalf("Forget: %v", err)
			}
			if got := subscribersOf(t, s, followed.ID); len(got) != 0 {
				t.Fatalf("subscribers = %+v, want none", got)
			}
		})
	}
}
