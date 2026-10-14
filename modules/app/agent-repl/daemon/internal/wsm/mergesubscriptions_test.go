package wsm

import (
	"context"
	"errors"
	"path/filepath"
	"reflect"
	"testing"

	"agent-repld/internal/dlog"
)

// subscription is WS's subscription to REPO at DETAIL, taken at instant.
func subscription(ws WorkspaceID, repo RepoKey, detail MergeQueueDetail) MergeQueueSubscription {
	return MergeQueueSubscription{Subscriber: ws, Repo: repo, Detail: detail, SubscribedAt: instant}
}

// queueKey is a fresh directory keyed as the queue keys it, so a
// subscription's scope reads back as it was written.
func queueKey(t *testing.T) RepoKey {
	t.Helper()
	key, err := normalizeRepoKey(RepoKey(t.TempDir()))
	if err != nil {
		t.Fatalf("normalizeRepoKey: %v", err)
	}
	return key
}

// mustSubscribe records SUB, failing the test on any error.
func mustSubscribe(t *testing.T, s *store, sub MergeQueueSubscription) {
	t.Helper()
	if _, err := s.SubscribeMergeQueue(context.Background(), sub); err != nil {
		t.Fatalf("SubscribeMergeQueue(%+v): %v", sub, err)
	}
}

// subscriptionsOf reads every subscription back, failing the test on an error.
func subscriptionsOf(t *testing.T, s *store) []MergeQueueSubscription {
	t.Helper()
	subs, err := s.MergeQueueSubscriptions(context.Background())
	if err != nil {
		t.Fatalf("MergeQueueSubscriptions: %v", err)
	}
	return subs
}

func TestASubscriptionRoundTripsAtEachDetail(t *testing.T) {
	tests := []struct {
		name   string
		detail MergeQueueDetail
	}{
		{name: "every change", detail: MergeQueueDetailEveryChange},
		{name: "advances", detail: MergeQueueDetailAdvances},
		{name: "depletes", detail: MergeQueueDetailDepletes},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			ws := testWorkspace(t, s)
			repo := queueKey(t)
			want := subscription(ws.ID, repo, tt.detail)

			// Act
			replaced, err := s.SubscribeMergeQueue(context.Background(), want)

			// Assert
			if err != nil || replaced {
				t.Fatalf("SubscribeMergeQueue = %v, %v; want a fresh subscription", replaced, err)
			}
			if got := subscriptionsOf(t, s); !reflect.DeepEqual(got, []MergeQueueSubscription{want}) {
				t.Fatalf("subscriptions = %+v, want %+v", got, want)
			}
		})
	}
}

func TestSubscribingAgainInTheSameScopeReplacesTheDetail(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	repo := queueKey(t)
	mustSubscribe(t, s, subscription(ws.ID, repo, MergeQueueDetailEveryChange))
	want := subscription(ws.ID, repo, MergeQueueDetailDepletes)

	// Act
	replaced, err := s.SubscribeMergeQueue(context.Background(), want)

	// Assert
	if err != nil || !replaced {
		t.Fatalf("SubscribeMergeQueue = %v, %v; want the standing subscription replaced", replaced, err)
	}
	if got := subscriptionsOf(t, s); !reflect.DeepEqual(got, []MergeQueueSubscription{want}) {
		t.Fatalf("subscriptions = %+v, want only %+v", got, want)
	}
}

func TestAScopedAndAnUnscopedSubscriptionStandTogether(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	repo := queueKey(t)
	every := subscription(ws.ID, EveryRepository, MergeQueueDetailAdvances)
	scoped := subscription(ws.ID, repo, MergeQueueDetailEveryChange)

	// Act
	mustSubscribe(t, s, every)
	mustSubscribe(t, s, scoped)

	// Assert
	if got := subscriptionsOf(t, s); !reflect.DeepEqual(got, []MergeQueueSubscription{every, scoped}) {
		t.Fatalf("subscriptions = %+v, want both scopes", got)
	}
}

func TestASubscriptionsScopeIsKeyedAsTheQueueKeysIt(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	dir := t.TempDir()
	want, err := normalizeRepoKey(RepoKey(dir))
	if err != nil {
		t.Fatalf("normalizeRepoKey: %v", err)
	}

	// Act
	mustSubscribe(t, s, subscription(ws.ID, RepoKey(dir+string(filepath.Separator)), MergeQueueDetailAdvances))

	// Assert
	if got := subscriptionsOf(t, s); len(got) != 1 || got[0].Repo != want {
		t.Fatalf("subscriptions = %+v, want one keyed %q", got, want)
	}
}

func TestASubscriptionWithAnUndeclaredDetailIsRefused(t *testing.T) {
	// Arrange
	s, log := testStore(t)
	ws := testWorkspace(t, s)

	// Act
	_, err := s.SubscribeMergeQueue(context.Background(), subscription(ws.ID, EveryRepository, MergeQueueDetail(0)))

	// Assert
	if err == nil {
		t.Fatal("SubscribeMergeQueue accepted an undeclared detail")
	}
	if _, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: "error", Operation: "daemon.wsm.subscribe_merge_queue"}); !ok {
		t.Fatalf("no error record for the refused detail: %+v", log.Records())
	}
	if got := subscriptionsOf(t, s); len(got) != 0 {
		t.Fatalf("subscriptions = %+v, want none written", got)
	}
}

func TestASubscriptionOfAnUnknownWorkspaceIsNotFound(t *testing.T) {
	// Arrange
	s, _ := testStore(t)

	// Act
	_, err := s.SubscribeMergeQueue(context.Background(), subscription("no-such-workspace", EveryRepository, MergeQueueDetailAdvances))

	// Assert
	if !errors.Is(err, ErrNotFound) {
		t.Fatalf("SubscribeMergeQueue = %v, want ErrNotFound", err)
	}
	if got := subscriptionsOf(t, s); len(got) != 0 {
		t.Fatalf("subscriptions = %+v, want none written", got)
	}
}

func TestAClosedWorkspaceCannotSubscribe(t *testing.T) {
	// Arrange
	s, log := testStore(t)
	ws := testWorkspace(t, s)
	if err := s.MarkClosed(context.Background(), ws.ID, Closure{At: instant, By: CloseByUser}); err != nil {
		t.Fatalf("MarkClosed: %v", err)
	}

	// Act
	_, err := s.SubscribeMergeQueue(context.Background(), subscription(ws.ID, EveryRepository, MergeQueueDetailAdvances))

	// Assert
	if !errors.Is(err, ErrSubscriberClosed) {
		t.Fatalf("SubscribeMergeQueue = %v, want ErrSubscriberClosed", err)
	}
	if _, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: "error"}); ok {
		t.Fatalf("a refusal was recorded at error: %+v", log.Records())
	}
	if got := subscriptionsOf(t, s); len(got) != 0 {
		t.Fatalf("subscriptions = %+v, want none written", got)
	}
}

func TestUnsubscribingEndsOnlyThatScope(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	repo := queueKey(t)
	scoped := subscription(ws.ID, repo, MergeQueueDetailEveryChange)
	mustSubscribe(t, s, subscription(ws.ID, EveryRepository, MergeQueueDetailAdvances))
	mustSubscribe(t, s, scoped)

	// Act
	ended, err := s.UnsubscribeMergeQueue(context.Background(), ws.ID, EveryRepository)

	// Assert
	if err != nil || !ended {
		t.Fatalf("UnsubscribeMergeQueue = %v, %v; want the unscoped subscription ended", ended, err)
	}
	if got := subscriptionsOf(t, s); !reflect.DeepEqual(got, []MergeQueueSubscription{scoped}) {
		t.Fatalf("subscriptions = %+v, want only the scoped one left", got)
	}
}

func TestUnsubscribingWithNoSubscriptionAnswersNoneEnded(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)

	// Act
	ended, err := s.UnsubscribeMergeQueue(context.Background(), ws.ID, RepoKey(t.TempDir()))

	// Assert
	if err != nil || ended {
		t.Fatalf("UnsubscribeMergeQueue = %v, %v; want nothing ended", ended, err)
	}
}

func TestClosingAWorkspaceEndsEverySubscriptionItHolds(t *testing.T) {
	// Arrange
	s, log := testStore(t)
	closing := testWorkspace(t, s)
	other := testWorkspaceNamed(t, s, "other")
	kept := subscription(other.ID, EveryRepository, MergeQueueDetailAdvances)
	mustSubscribe(t, s, subscription(closing.ID, EveryRepository, MergeQueueDetailAdvances))
	mustSubscribe(t, s, subscription(closing.ID, RepoKey(t.TempDir()), MergeQueueDetailDepletes))
	mustSubscribe(t, s, kept)

	// Act
	err := s.MarkClosed(context.Background(), closing.ID, Closure{At: instant, By: CloseByUser})

	// Assert
	if err != nil {
		t.Fatalf("MarkClosed: %v", err)
	}
	if got := subscriptionsOf(t, s); !reflect.DeepEqual(got, []MergeQueueSubscription{kept}) {
		t.Fatalf("subscriptions = %+v, want only the other workspace's", got)
	}
	record, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: "info", Message: "the close ended the workspace's merge queue subscriptions"})
	if !ok {
		t.Fatalf("no record of the ended subscriptions: %+v", log.Records())
	}
	if record.Context["subscriptions"] != int64(2) || record.Context["workspace"] != string(closing.ID) {
		t.Fatalf("record context = %+v, want 2 subscriptions of %s", record.Context, closing.ID)
	}
}

func TestClosingAWorkspaceWithNoSubscriptionRecordsNone(t *testing.T) {
	// Arrange
	s, log := testStore(t)
	ws := testWorkspace(t, s)

	// Act
	err := s.MarkClosed(context.Background(), ws.ID, Closure{At: instant, By: CloseByUser})

	// Assert
	if err != nil {
		t.Fatalf("MarkClosed: %v", err)
	}
	if _, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Message: "the close ended the workspace's merge queue subscriptions"}); ok {
		t.Fatalf("a close with no subscription recorded ending some: %+v", log.Records())
	}
}

func TestReopeningAWorkspaceDoesNotRestoreItsSubscriptions(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	mustSubscribe(t, s, subscription(ws.ID, EveryRepository, MergeQueueDetailAdvances))
	if err := s.MarkClosed(context.Background(), ws.ID, Closure{At: instant, By: CloseByUser}); err != nil {
		t.Fatalf("MarkClosed: %v", err)
	}

	// Act
	err := s.Reopen(context.Background(), ws.ID)

	// Assert
	if err != nil {
		t.Fatalf("Reopen: %v", err)
	}
	if got := subscriptionsOf(t, s); len(got) != 0 {
		t.Fatalf("subscriptions = %+v, want none after the re-open", got)
	}
}

func TestForgettingAWorkspaceEndsItsSubscriptions(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	mustSubscribe(t, s, subscription(ws.ID, EveryRepository, MergeQueueDetailAdvances))

	// Act
	_, err := s.Forget(context.Background(), ws.ID)

	// Assert
	if err != nil {
		t.Fatalf("Forget: %v", err)
	}
	if got := subscriptionsOf(t, s); len(got) != 0 {
		t.Fatalf("subscriptions = %+v, want none after the forget", got)
	}
}

func TestASubscriptionSurvivesARestart(t *testing.T) {
	// Arrange
	path := filepath.Join(t.TempDir(), "wsm.db")
	first, err := Open(context.Background(), path, WithUnsyncedWrites())
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	ws := testWorkspace(t, first.(*store))
	want := subscription(ws.ID, EveryRepository, MergeQueueDetailDepletes)
	mustSubscribe(t, first.(*store), want)
	if err := first.Close(); err != nil {
		t.Fatalf("Close: %v", err)
	}

	// Act
	second, err := Open(context.Background(), path, WithUnsyncedWrites())
	if err != nil {
		t.Fatalf("reopen: %v", err)
	}
	t.Cleanup(func() { second.Close() })
	got, err := second.MergeQueueSubscriptions(context.Background())

	// Assert
	if err != nil {
		t.Fatalf("MergeQueueSubscriptions: %v", err)
	}
	if !reflect.DeepEqual(got, []MergeQueueSubscription{want}) {
		t.Fatalf("subscriptions after the restart = %+v, want %+v", got, want)
	}
}

func TestAStoredUndeclaredDetailRefusesTheRead(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	mustSubscribe(t, s, subscription(ws.ID, EveryRepository, MergeQueueDetailAdvances))
	corrupt(t, s, `UPDATE merge_queue_subscriptions SET detail = 9`)

	// Act
	got, err := s.MergeQueueSubscriptions(context.Background())

	// Assert
	if err == nil {
		t.Fatalf("MergeQueueSubscriptions = %+v, want the undeclared detail refused", got)
	}
}

func TestMergeQueueDetailNamesEachDetail(t *testing.T) {
	tests := []struct {
		detail MergeQueueDetail
		want   string
	}{
		{detail: MergeQueueDetailEveryChange, want: "every_change"},
		{detail: MergeQueueDetailAdvances, want: "advances"},
		{detail: MergeQueueDetailDepletes, want: "depletes"},
		{detail: MergeQueueDetail(7), want: "MergeQueueDetail(7)"},
	}
	for _, tt := range tests {
		t.Run(tt.want, func(t *testing.T) {
			// Act
			got := tt.detail.String()

			// Assert
			if got != tt.want {
				t.Fatalf("String() = %q, want %q", got, tt.want)
			}
		})
	}
}

func TestParseMergeQueueDetailReadsEachDetailsSpelling(t *testing.T) {
	for _, want := range []MergeQueueDetail{MergeQueueDetailEveryChange, MergeQueueDetailAdvances, MergeQueueDetailDepletes} {
		t.Run(want.String(), func(t *testing.T) {
			// Act.
			got, err := ParseMergeQueueDetail(want.String())

			// Assert.
			if err != nil || got != want {
				t.Fatalf("ParseMergeQueueDetail(%q) = %v, %v; want %v", want.String(), got, err, want)
			}
		})
	}
}

func TestParseMergeQueueDetailRefusesAnUndeclaredSpelling(t *testing.T) {
	// Act.
	_, err := ParseMergeQueueDetail("sometimes")

	// Assert.
	if err == nil {
		t.Fatal("ParseMergeQueueDetail accepted an undeclared detail")
	}
}
