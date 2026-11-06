package wsm

import (
	"context"
	"errors"
	"reflect"
	"testing"
	"time"

	"agent-repld/internal/dlog"
)

// ghQueueKey is the queue every test follows.
var ghQueueKey = GithubMergeQueueKey{Owner: "ChessCom", Repo: "explanation-engine", Branch: "main"}

// queueSub is SUBSCRIBER's subscription to ghQueueKey, told TOLD.
func queueSub(subscriber WorkspaceID, told string) GithubMergeQueueSubscription {
	return GithubMergeQueueSubscription{Subscriber: subscriber, Queue: ghQueueKey, SubscribedAt: instant, Told: told}
}

// queueSubsOf reads SUBSCRIBER's queue subscriptions, failing the test on an
// error.
func queueSubsOf(t *testing.T, s *store, subscriber WorkspaceID) []GithubMergeQueueSubscription {
	t.Helper()
	subs, err := s.WorkspaceGithubMergeQueueSubscriptions(context.Background(), subscriber)
	if err != nil {
		t.Fatalf("WorkspaceGithubMergeQueueSubscriptions: %v", err)
	}
	return subs
}

// mustSubscribeQueue records SUB, failing the test on an error.
func mustSubscribeQueue(t *testing.T, s *store, sub GithubMergeQueueSubscription) {
	t.Helper()
	if _, err := s.SubscribeGithubMergeQueue(context.Background(), sub); err != nil {
		t.Fatalf("SubscribeGithubMergeQueue: %v", err)
	}
}

// mustAdvanceQueue applies A, failing the test unless it applied.
func mustAdvanceQueue(t *testing.T, s *store, a GithubMergeQueueAdvance) {
	t.Helper()
	if applied, err := s.AdvanceGithubMergeQueueTold(context.Background(), a); err != nil || !applied {
		t.Fatalf("AdvanceGithubMergeQueueTold = %v, %v", applied, err)
	}
}

func TestGithubMergeQueueKeyRoundTrips(t *testing.T) {
	tests := []struct {
		name string
		text string
		want GithubMergeQueueKey
		ok   bool
	}{
		{name: "a key", text: "ChessCom/explanation-engine@main", want: ghQueueKey, ok: true},
		{name: "a branch holding a slash and an at", text: "o/r@release/v1@2", want: GithubMergeQueueKey{Owner: "o", Repo: "r", Branch: "release/v1@2"}, ok: true},
		{name: "no branch", text: "o/r@", ok: false},
		{name: "no repo", text: "o@main", ok: false},
		{name: "a pull request key", text: "o/r#4", ok: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got, err := ParseGithubMergeQueueKey(tt.text)

			// Assert
			if (err == nil) != tt.ok || got != tt.want {
				t.Fatalf("ParseGithubMergeQueueKey(%q) = %v, %v; want %v ok=%v", tt.text, got, err, tt.want, tt.ok)
			}
			if tt.ok && got.String() != tt.text {
				t.Fatalf("String = %q, want %q", got.String(), tt.text)
			}
		})
	}
}

func TestAGithubMergeQueueSubscriptionRoundTripsAndKeepsWhatWasTold(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	first, second := queueSub(ws.ID, "snapshot-1"), queueSub(ws.ID, "snapshot-2")

	// Act
	heldFirst, errFirst := s.SubscribeGithubMergeQueue(context.Background(), first)
	heldSecond, errSecond := s.SubscribeGithubMergeQueue(context.Background(), second)

	// Assert
	if errFirst != nil || heldFirst || errSecond != nil || !heldSecond {
		t.Fatalf("SubscribeGithubMergeQueue = (%v, %v), (%v, %v); want fresh then held", heldFirst, errFirst, heldSecond, errSecond)
	}
	if got := queueSubsOf(t, s, ws.ID); !reflect.DeepEqual(got, []GithubMergeQueueSubscription{first}) {
		t.Fatalf("subscriptions = %+v, want %+v", got, first)
	}
	live, err := s.LiveGithubMergeQueueSubscriptions(context.Background())
	if err != nil || !reflect.DeepEqual(live, []GithubMergeQueueSubscription{first}) {
		t.Fatalf("live = %+v, %v; want %+v", live, err, first)
	}
}

func TestSubscribeGithubMergeQueueRefuses(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(t *testing.T, s *store) GithubMergeQueueSubscription
		want    error
	}{
		{name: "an unregistered subscriber", arrange: func(*testing.T, *store) GithubMergeQueueSubscription {
			return queueSub("absent", "a")
		}, want: ErrNotFound},
		{name: "a closed subscriber", arrange: func(t *testing.T, s *store) GithubMergeQueueSubscription {
			ws := testWorkspace(t, s)
			if err := s.MarkClosed(context.Background(), ws.ID, Closure{At: instant, By: CloseByUser}); err != nil {
				t.Fatalf("MarkClosed: %v", err)
			}
			return queueSub(ws.ID, "a")
		}, want: ErrSubscriberClosed},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			sub := tt.arrange(t, s)

			// Act
			_, err := s.SubscribeGithubMergeQueue(context.Background(), sub)

			// Assert
			if !errors.Is(err, tt.want) {
				t.Fatalf("SubscribeGithubMergeQueue = %v, want %v", err, tt.want)
			}
		})
	}
}

func TestSubscribeGithubMergeQueueFailsHardOnAContradictorySubscription(t *testing.T) {
	tests := []struct {
		name string
		sub  func(ws WorkspaceID) GithubMergeQueueSubscription
	}{
		{name: "a queue missing its branch", sub: func(ws WorkspaceID) GithubMergeQueueSubscription {
			sub := queueSub(ws, "a")
			sub.Queue.Branch = ""
			return sub
		}},
		{name: "a subscription told nothing", sub: func(ws WorkspaceID) GithubMergeQueueSubscription { return queueSub(ws, "") }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, log := testStore(t)
			ws := testWorkspace(t, s)

			// Act
			_, err := s.SubscribeGithubMergeQueue(context.Background(), tt.sub(ws.ID))

			// Assert
			if err == nil {
				t.Fatal("SubscribeGithubMergeQueue = nil, want an error")
			}
			if _, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: "error", Operation: "daemon.wsm.subscribe_github_merge_queue",
				Message: "refused a merge-queue subscription its caller should never have made"}); !ok {
				t.Fatalf("no refusal record: %+v", log.Records())
			}
			if len(queueSubsOf(t, s, ws.ID)) != 0 {
				t.Fatal("a refused subscription was recorded")
			}
		})
	}
}

func TestUnsubscribeGithubMergeQueueEndsItAndWhatItWasOwed(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	mustSubscribeQueue(t, s, queueSub(ws.ID, "a"))
	mustAdvanceQueue(t, s, GithubMergeQueueAdvance{Subscriber: ws.ID, Queue: ghQueueKey, From: "a", To: "b", Owed: []string{"one."}, ReadAt: instant})

	// Act
	ended, errEnded := s.UnsubscribeGithubMergeQueue(context.Background(), ws.ID, ghQueueKey)
	again, errAgain := s.UnsubscribeGithubMergeQueue(context.Background(), ws.ID, ghQueueKey)

	// Assert
	if errEnded != nil || !ended || errAgain != nil || again {
		t.Fatalf("Unsubscribe = (%v, %v), (%v, %v); want ended then none", ended, errEnded, again, errAgain)
	}
	owed, err := s.OwedGithubMergeQueueNotices(context.Background())
	if err != nil || len(owed) != 0 || len(queueSubsOf(t, s, ws.ID)) != 0 {
		t.Fatalf("owed = %+v, %v; want nothing left", owed, err)
	}
}

func TestAdvanceGithubMergeQueueToldComparesAndSets(t *testing.T) {
	tests := []struct {
		name        string
		from        string
		wantApplied bool
		wantTold    string
		wantOwed    int
	}{
		{name: "the snapshot read still stands", from: "a", wantApplied: true, wantTold: "b", wantOwed: 1},
		{name: "the snapshot moved since it was read", from: "stale", wantApplied: false, wantTold: "a", wantOwed: 0},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			ws := testWorkspace(t, s)
			mustSubscribeQueue(t, s, queueSub(ws.ID, "a"))
			if _, err := s.RecordGithubMergeQueueReadFailure(context.Background(), ghQueueKey, PullRequestFailure{Since: instant, Kind: PullRequestFailureUnreachable, Detail: "down"}); err != nil {
				t.Fatal(err)
			}

			// Act
			applied, err := s.AdvanceGithubMergeQueueTold(context.Background(), GithubMergeQueueAdvance{Subscriber: ws.ID, Queue: ghQueueKey, From: tt.from, To: "b",
				Owed: []string{"one."}, Status: "two entries", ReadAt: instant.Add(time.Minute)})

			// Assert
			if err != nil || applied != tt.wantApplied {
				t.Fatalf("Advance = %v, %v; want %v", applied, err, tt.wantApplied)
			}
			got := queueSubsOf(t, s, ws.ID)[0]
			if got.Told != tt.wantTold || (tt.wantApplied && (got.Failing != nil || !got.LastRead.Equal(instant.Add(time.Minute)))) {
				t.Fatalf("subscription = %+v", got)
			}
			owed, _ := s.OwedGithubMergeQueueNotices(context.Background())
			if len(owed) != tt.wantOwed || (tt.wantOwed == 1 && (owed[0].Status != "two entries" || !reflect.DeepEqual(owed[0].Pending, []string{"one."}))) {
				t.Fatalf("owed = %+v, want %d", owed, tt.wantOwed)
			}
		})
	}
}

func TestAdvanceGithubMergeQueueToldRefusesAnAdvanceNoReadMade(t *testing.T) {
	// Arrange
	s, log := testStore(t)
	ws := testWorkspace(t, s)
	mustSubscribeQueue(t, s, queueSub(ws.ID, "a"))

	// Act
	applied, err := s.AdvanceGithubMergeQueueTold(context.Background(), GithubMergeQueueAdvance{Subscriber: ws.ID, Queue: ghQueueKey, From: "a", To: "b"})

	// Assert
	if err == nil || applied {
		t.Fatalf("Advance = %v, %v; want a refusal", applied, err)
	}
	if _, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: "error", Operation: "daemon.wsm.advance_github_merge_queue_told", Message: "refused a merge-queue advance no read made"}); !ok {
		t.Fatalf("no refusal record: %+v", log.Records())
	}
	if got := queueSubsOf(t, s, ws.ID)[0].Told; got != "a" {
		t.Fatalf("told = %q, want it untouched", got)
	}
}

func TestRecordGithubMergeQueueReadFailureAnswersOnlyTheStretchesItBegan(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	mustSubscribeQueue(t, s, queueSub(ws.ID, "a"))
	ctx := context.Background()
	reset := instant.Add(time.Hour)

	// Act
	beganFirst, errFirst := s.RecordGithubMergeQueueReadFailure(ctx, ghQueueKey, PullRequestFailure{Since: instant, Kind: PullRequestFailureNotFound})
	beganSecond, errSecond := s.RecordGithubMergeQueueReadFailure(ctx, ghQueueKey, PullRequestFailure{Since: instant.Add(time.Minute), Kind: PullRequestFailureRateLimited, ResetsAt: reset})

	// Assert
	if errFirst != nil || errSecond != nil || !reflect.DeepEqual(beganFirst, []WorkspaceID{ws.ID}) || len(beganSecond) != 0 {
		t.Fatalf("began = %v (%v), %v (%v); want the subscriber once", beganFirst, errFirst, beganSecond, errSecond)
	}
	want := &PullRequestFailure{Since: instant, Kind: PullRequestFailureRateLimited, ResetsAt: reset}
	if got := queueSubsOf(t, s, ws.ID)[0].Failing; !reflect.DeepEqual(got, want) {
		t.Fatalf("failing = %+v, want %+v", got, want)
	}
}

func TestAGithubMergeQueueNoticeIsOwedUntilSubmitted(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	mustSubscribeQueue(t, s, queueSub(ws.ID, "a"))
	ctx := context.Background()
	if err := s.OweGithubMergeQueueNotice(ctx, ws.ID, ghQueueKey, []string{"reads failing."}); err != nil {
		t.Fatal(err)
	}

	// Act
	before, errBefore := s.OwedGithubMergeQueueNotices(ctx)
	errSubmit := s.RecordGithubMergeQueueNoticeSubmitted(ctx, ws.ID, ghQueueKey, "turn-1", []string{"reads failing."}, 1)
	after, errAfter := s.OwedGithubMergeQueueNotices(ctx)

	// Assert
	want := []GithubMergeQueueOwed{{Subscriber: ws.ID, Queue: ghQueueKey, Sentences: []string{}, Pending: []string{"reads failing."}}}
	if errBefore != nil || !reflect.DeepEqual(before, want) {
		t.Fatalf("owed = %+v, %v; want %+v", before, errBefore, want)
	}
	if errSubmit != nil || errAfter != nil || len(after) != 0 {
		t.Fatalf("after submitting: %v, %+v, %v; want nothing owed", errSubmit, after, errAfter)
	}
}

func TestClosingASubscriberEndsItsGithubMergeQueueSubscriptionsAndNotices(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	closing := testWorkspace(t, s)
	other := testWorkspaceNamed(t, s, "other")
	for _, ws := range []WorkspaceID{closing.ID, other.ID} {
		mustSubscribeQueue(t, s, queueSub(ws, "a"))
		mustAdvanceQueue(t, s, GithubMergeQueueAdvance{Subscriber: ws, Queue: ghQueueKey, From: "a", To: "b", Owed: []string{"one."}, ReadAt: instant})
	}

	// Act
	err := s.MarkClosed(context.Background(), closing.ID, Closure{At: instant, By: CloseByUser})

	// Assert
	if err != nil {
		t.Fatalf("MarkClosed: %v", err)
	}
	if len(queueSubsOf(t, s, closing.ID)) != 0 || len(queueSubsOf(t, s, other.ID)) != 1 {
		t.Fatalf("subscriptions: closing %d, other %d; want 0 and 1", len(queueSubsOf(t, s, closing.ID)), len(queueSubsOf(t, s, other.ID)))
	}
	owed, err := s.OwedGithubMergeQueueNotices(context.Background())
	if err != nil || len(owed) != 1 || owed[0].Subscriber != other.ID {
		t.Fatalf("owed = %+v, %v; want only the other workspace's", owed, err)
	}
}
