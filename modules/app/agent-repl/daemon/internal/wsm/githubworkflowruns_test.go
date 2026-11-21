package wsm

import (
	"context"
	"errors"
	"reflect"
	"testing"
	"time"

	"agent-repld/internal/dlog"
)

// ghRunKey is the run every test waits on.
var ghRunKey = GithubWorkflowRunKey{Owner: "ChessCom", Repo: "explanation-engine", RunID: 43771255095}

// runSub is SUBSCRIBER's subscription to ghRunKey.
func runSub(subscriber WorkspaceID) GithubWorkflowRunSubscription {
	return GithubWorkflowRunSubscription{Subscriber: subscriber, Run: ghRunKey, SubscribedAt: instant, LastRead: instant}
}

// runSubsOf reads SUBSCRIBER's run subscriptions, failing the test on an
// error.
func runSubsOf(t *testing.T, s *store, subscriber WorkspaceID) []GithubWorkflowRunSubscription {
	t.Helper()
	subs, err := s.WorkspaceGithubWorkflowRunSubscriptions(context.Background(), subscriber)
	if err != nil {
		t.Fatalf("WorkspaceGithubWorkflowRunSubscriptions: %v", err)
	}
	return subs
}

// mustSubscribeRun records SUB, failing the test on an error.
func mustSubscribeRun(t *testing.T, s *store, sub GithubWorkflowRunSubscription) {
	t.Helper()
	if _, err := s.SubscribeGithubWorkflowRun(context.Background(), sub); err != nil {
		t.Fatalf("SubscribeGithubWorkflowRun: %v", err)
	}
}

// owedRunsOf reads every owed run notice, failing the test on an error.
func owedRunsOf(t *testing.T, s *store) []GithubWorkflowRunOwed {
	t.Helper()
	owed, err := s.OwedGithubWorkflowRunNotices(context.Background())
	if err != nil {
		t.Fatalf("OwedGithubWorkflowRunNotices: %v", err)
	}
	return owed
}

// completion is ghRunKey's completion owed to SUBSCRIBER.
func completion(subscriber WorkspaceID, held bool) GithubWorkflowRunCompletion {
	return GithubWorkflowRunCompletion{Subscriber: subscriber, Run: ghRunKey, Sentences: []string{"finished."}, Status: "head abc", Held: held}
}

func TestGithubWorkflowRunKeyRoundTrips(t *testing.T) {
	tests := []struct {
		name string
		text string
		want GithubWorkflowRunKey
		ok   bool
	}{
		{name: "a key past 32 bits", text: "ChessCom/explanation-engine/runs/43771255095", want: ghRunKey, ok: true},
		{name: "a zero id", text: "o/r/runs/0", ok: false},
		{name: "no id", text: "o/r/runs/", ok: false},
		{name: "a merge queue key", text: "o/r@main", ok: false},
		{name: "an id past 64 bits", text: "o/r/runs/18446744073709551616", ok: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got, err := ParseGithubWorkflowRunKey(tt.text)

			// Assert
			if (err == nil) != tt.ok || got != tt.want {
				t.Fatalf("ParseGithubWorkflowRunKey(%q) = %v, %v; want %v ok=%v", tt.text, got, err, tt.want, tt.ok)
			}
			if tt.ok && got.String() != tt.text {
				t.Fatalf("String = %q, want %q", got.String(), tt.text)
			}
		})
	}
}

func TestAGithubWorkflowRunSubscriptionRoundTrips(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)

	// Act
	heldFirst, errFirst := s.SubscribeGithubWorkflowRun(context.Background(), runSub(ws.ID))
	heldSecond, errSecond := s.SubscribeGithubWorkflowRun(context.Background(), runSub(ws.ID))

	// Assert
	if errFirst != nil || errSecond != nil || heldFirst || !heldSecond {
		t.Fatalf("held = %v (%v), %v (%v); want a new one, then the one held", heldFirst, errFirst, heldSecond, errSecond)
	}
	want := []GithubWorkflowRunSubscription{runSub(ws.ID)}
	if got := runSubsOf(t, s, ws.ID); !reflect.DeepEqual(got, want) {
		t.Fatalf("subscriptions = %+v, want %+v", got, want)
	}
	if live, err := s.LiveGithubWorkflowRunSubscriptions(context.Background()); err != nil || !reflect.DeepEqual(live, want) {
		t.Fatalf("live = %+v, %v; want %+v", live, err, want)
	}
}

func TestSubscribeGithubWorkflowRunRefuses(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(t *testing.T, s *store) GithubWorkflowRunSubscription
		want    error
	}{
		{name: "an unregistered subscriber", arrange: func(*testing.T, *store) GithubWorkflowRunSubscription {
			return runSub("absent")
		}, want: ErrNotFound},
		{name: "a closed subscriber", arrange: func(t *testing.T, s *store) GithubWorkflowRunSubscription {
			ws := testWorkspace(t, s)
			if err := s.MarkClosed(context.Background(), ws.ID, Closure{At: instant, By: CloseByUser}); err != nil {
				t.Fatalf("MarkClosed: %v", err)
			}
			return runSub(ws.ID)
		}, want: ErrSubscriberClosed},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			sub := tt.arrange(t, s)

			// Act
			_, err := s.SubscribeGithubWorkflowRun(context.Background(), sub)

			// Assert
			if !errors.Is(err, tt.want) {
				t.Fatalf("SubscribeGithubWorkflowRun = %v, want %v", err, tt.want)
			}
		})
	}
}

func TestSubscribeGithubWorkflowRunFailsHardOnAContradictorySubscription(t *testing.T) {
	tests := []struct {
		name string
		sub  func(ws WorkspaceID) GithubWorkflowRunSubscription
	}{
		{name: "a run missing its id", sub: func(ws WorkspaceID) GithubWorkflowRunSubscription {
			sub := runSub(ws)
			sub.Run.RunID = 0
			return sub
		}},
		{name: "a subscription no read made", sub: func(ws WorkspaceID) GithubWorkflowRunSubscription {
			sub := runSub(ws)
			sub.LastRead = time.Time{}
			return sub
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, log := testStore(t)
			ws := testWorkspace(t, s)

			// Act
			_, err := s.SubscribeGithubWorkflowRun(context.Background(), tt.sub(ws.ID))

			// Assert
			if err == nil {
				t.Fatal("SubscribeGithubWorkflowRun = nil, want an error")
			}
			if _, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: "error", Operation: "daemon.wsm.subscribe_github_workflow_run",
				Message: "refused a workflow-run subscription its caller should never have made"}); !ok {
				t.Fatalf("no refusal record: %+v", log.Records())
			}
			if len(runSubsOf(t, s, ws.ID)) != 0 {
				t.Fatal("a refused subscription was recorded")
			}
		})
	}
}

func TestUnsubscribeGithubWorkflowRunEndsItAndWhatItWasOwed(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	mustSubscribeRun(t, s, runSub(ws.ID))
	if err := s.OweGithubWorkflowRunNotice(context.Background(), ws.ID, ghRunKey, []string{"credentials."}); err != nil {
		t.Fatal(err)
	}

	// Act
	endedFirst, errFirst := s.UnsubscribeGithubWorkflowRun(context.Background(), ws.ID, ghRunKey)
	endedSecond, errSecond := s.UnsubscribeGithubWorkflowRun(context.Background(), ws.ID, ghRunKey)

	// Assert
	if errFirst != nil || errSecond != nil || !endedFirst || endedSecond {
		t.Fatalf("ended = %v (%v), %v (%v); want one that ended, then none", endedFirst, errFirst, endedSecond, errSecond)
	}
	if len(runSubsOf(t, s, ws.ID)) != 0 || len(owedRunsOf(t, s)) != 0 {
		t.Fatal("the subscription or its owed notice survived the unsubscribe")
	}
}

func TestOweGithubWorkflowRunCompletion(t *testing.T) {
	tests := []struct {
		name       string
		subscribed bool
		held       bool
		wantOwed   bool
	}{
		{name: "the poller's completion of a held subscription ends it and owes the notice", subscribed: true, held: true, wantOwed: true},
		{name: "the poller's completion of a subscription gone owes nothing", subscribed: false, held: true, wantOwed: false},
		{name: "a subscribe to a finished run owes the notice with nothing held", subscribed: false, held: false, wantOwed: true},
		{name: "a subscribe to a finished run ends the subscription that stands", subscribed: true, held: false, wantOwed: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			ws := testWorkspace(t, s)
			if tt.subscribed {
				mustSubscribeRun(t, s, runSub(ws.ID))
			}

			// Act
			owed, err := s.OweGithubWorkflowRunCompletion(context.Background(), completion(ws.ID, tt.held))

			// Assert
			if err != nil || owed != tt.wantOwed {
				t.Fatalf("OweGithubWorkflowRunCompletion = %v, %v; want %v", owed, err, tt.wantOwed)
			}
			if len(runSubsOf(t, s, ws.ID)) != 0 {
				t.Fatal("the subscription survived its run's completion")
			}
			want := []GithubWorkflowRunOwed{}
			if tt.wantOwed {
				want = []GithubWorkflowRunOwed{{Subscriber: ws.ID, Run: ghRunKey, Sentences: []string{}, Pending: []string{"finished."}, Status: "head abc"}}
			}
			if got := owedRunsOf(t, s); !reflect.DeepEqual(got, want) {
				t.Fatalf("owed = %+v, want %+v", got, want)
			}
		})
	}
}

func TestOweGithubWorkflowRunCompletionIsOwedOnceToAHeldSubscription(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	mustSubscribeRun(t, s, runSub(ws.ID))

	// Act
	first, errFirst := s.OweGithubWorkflowRunCompletion(context.Background(), completion(ws.ID, true))
	second, errSecond := s.OweGithubWorkflowRunCompletion(context.Background(), completion(ws.ID, true))

	// Assert
	if errFirst != nil || errSecond != nil || !first || second {
		t.Fatalf("owed = %v (%v), %v (%v); want once", first, errFirst, second, errSecond)
	}
	if got := owedRunsOf(t, s); len(got) != 1 || !reflect.DeepEqual(got[0].Pending, []string{"finished."}) {
		t.Fatalf("owed = %+v, want the one sentence once", got)
	}
}

func TestOweGithubWorkflowRunCompletionRefuses(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(t *testing.T, s *store) GithubWorkflowRunCompletion
		want    error
		wantLog bool
	}{
		{name: "an unregistered subscriber", arrange: func(*testing.T, *store) GithubWorkflowRunCompletion {
			return completion("absent", false)
		}, want: ErrNotFound},
		{name: "a closed subscriber", arrange: func(t *testing.T, s *store) GithubWorkflowRunCompletion {
			ws := testWorkspace(t, s)
			if err := s.MarkClosed(context.Background(), ws.ID, Closure{At: instant, By: CloseByUser}); err != nil {
				t.Fatalf("MarkClosed: %v", err)
			}
			return completion(ws.ID, false)
		}, want: ErrSubscriberClosed},
		{name: "a completion that says nothing", arrange: func(t *testing.T, s *store) GithubWorkflowRunCompletion {
			c := completion(testWorkspace(t, s).ID, false)
			c.Sentences = nil
			return c
		}, wantLog: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, log := testStore(t)
			c := tt.arrange(t, s)

			// Act
			owed, err := s.OweGithubWorkflowRunCompletion(context.Background(), c)

			// Assert
			if owed || err == nil || (tt.want != nil && !errors.Is(err, tt.want)) {
				t.Fatalf("OweGithubWorkflowRunCompletion = %v, %v; want %v", owed, err, tt.want)
			}
			if _, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: "error", Operation: "daemon.wsm.owe_github_workflow_run_completion",
				Message: "refused a workflow-run completion its caller should never have owed"}); ok != tt.wantLog {
				t.Fatalf("refusal record found %v, want %v: %+v", ok, tt.wantLog, log.Records())
			}
			if len(owedRunsOf(t, s)) != 0 {
				t.Fatal("a refused completion was owed")
			}
		})
	}
}

func TestRecordGithubWorkflowRunReadEndsTheFailingStretch(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	mustSubscribeRun(t, s, runSub(ws.ID))
	ctx := context.Background()
	if _, err := s.RecordGithubWorkflowRunReadFailure(ctx, ghRunKey, PullRequestFailure{Since: instant, Kind: PullRequestFailureUnreachable, Detail: "down"}); err != nil {
		t.Fatal(err)
	}
	read := instant.Add(time.Minute)

	// Act
	err := s.RecordGithubWorkflowRunRead(ctx, ghRunKey, read)

	// Assert
	got := runSubsOf(t, s, ws.ID)[0]
	if err != nil || got.Failing != nil || !got.LastRead.Equal(read) {
		t.Fatalf("after the read: %v, failing %+v, last read %v; want healthy at %v", err, got.Failing, got.LastRead, read)
	}
}

func TestRecordGithubWorkflowRunReadRefusesAReadWithNoTime(t *testing.T) {
	// Arrange
	s, log := testStore(t)

	// Act
	err := s.RecordGithubWorkflowRunRead(context.Background(), ghRunKey, time.Time{})

	// Assert
	if err == nil {
		t.Fatal("RecordGithubWorkflowRunRead = nil, want an error")
	}
	if _, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: "error", Operation: "daemon.wsm.record_github_workflow_run_read",
		Message: "refused a workflow-run read no read made"}); !ok {
		t.Fatalf("no refusal record: %+v", log.Records())
	}
}

func TestRecordGithubWorkflowRunReadFailureAnswersOnlyTheStretchesItBegan(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	mustSubscribeRun(t, s, runSub(ws.ID))
	ctx := context.Background()

	// Act
	beganFirst, errFirst := s.RecordGithubWorkflowRunReadFailure(ctx, ghRunKey, PullRequestFailure{Since: instant, Kind: PullRequestFailureUnauthorized})
	beganSecond, errSecond := s.RecordGithubWorkflowRunReadFailure(ctx, ghRunKey, PullRequestFailure{Since: instant.Add(time.Minute), Kind: PullRequestFailureUnauthorized})

	// Assert
	if errFirst != nil || errSecond != nil || !reflect.DeepEqual(beganFirst, []WorkspaceID{ws.ID}) || len(beganSecond) != 0 {
		t.Fatalf("began = %v (%v), %v (%v); want the subscriber once", beganFirst, errFirst, beganSecond, errSecond)
	}
	want := &PullRequestFailure{Since: instant, Kind: PullRequestFailureUnauthorized}
	if got := runSubsOf(t, s, ws.ID)[0].Failing; !reflect.DeepEqual(got, want) {
		t.Fatalf("failing = %+v, want %+v", got, want)
	}
}

func TestAGithubWorkflowRunNoticeIsOwedUntilSubmitted(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	if _, err := s.OweGithubWorkflowRunCompletion(context.Background(), completion(ws.ID, false)); err != nil {
		t.Fatal(err)
	}

	// Act
	errSubmit := s.RecordGithubWorkflowRunNoticeSubmitted(context.Background(), ws.ID, ghRunKey, "turn-1", []string{"finished."}, 1)

	// Assert
	if errSubmit != nil || len(owedRunsOf(t, s)) != 0 {
		t.Fatalf("after submitting: %v, %+v; want nothing owed", errSubmit, owedRunsOf(t, s))
	}
}

func TestClosingASubscriberEndsItsGithubWorkflowRunSubscriptionsAndNotices(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	closing := testWorkspace(t, s)
	other := testWorkspaceNamed(t, s, "other")
	for _, ws := range []WorkspaceID{closing.ID, other.ID} {
		mustSubscribeRun(t, s, runSub(ws))
		if err := s.OweGithubWorkflowRunNotice(context.Background(), ws, ghRunKey, []string{"credentials."}); err != nil {
			t.Fatal(err)
		}
	}

	// Act
	err := s.MarkClosed(context.Background(), closing.ID, Closure{At: instant, By: CloseByUser})

	// Assert
	if err != nil {
		t.Fatalf("MarkClosed: %v", err)
	}
	if len(runSubsOf(t, s, closing.ID)) != 0 || len(runSubsOf(t, s, other.ID)) != 1 {
		t.Fatal("the closed workspace's subscription survived, or the other's did not")
	}
	if owed := owedRunsOf(t, s); len(owed) != 1 || owed[0].Subscriber != other.ID {
		t.Fatalf("owed = %+v; want only the other workspace's", owed)
	}
}
