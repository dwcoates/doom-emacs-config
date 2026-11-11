package wsm

import (
	"context"
	"errors"
	"reflect"
	"testing"
	"time"
)

// slackKey is the thread every test follows.
var slackKey = SlackThreadKey{Channel: "C0439CR3S3A", ThreadTS: "1784225803.782699"}

// slackSub is SUBSCRIBER's subscription to slackKey in MODE at cursor 7.
func slackSub(subscriber WorkspaceID, mode SlackThreadMode) SlackThreadSubscription {
	return SlackThreadSubscription{Subscriber: subscriber, Thread: slackKey, URL: "https://chesscom.slack.com/archives/C0439CR3S3A/p1784225803782699",
		Mode: mode, Origin: SlackThreadOriginRequested, SubscribedAt: instant, Cursor: 7}
}

// mustSubscribeSlack records SUB, failing the test on an error.
func mustSubscribeSlack(t *testing.T, s *store, sub SlackThreadSubscription) {
	t.Helper()
	if _, _, err := s.SubscribeSlackThread(context.Background(), sub); err != nil {
		t.Fatalf("SubscribeSlackThread: %v", err)
	}
}

// slackSubsOf reads SUBSCRIBER's subscriptions, failing the test on an error.
func slackSubsOf(t *testing.T, s *store, subscriber WorkspaceID) []SlackThreadSubscription {
	t.Helper()
	subs, err := s.WorkspaceSlackThreadSubscriptions(context.Background(), subscriber)
	if err != nil {
		t.Fatalf("WorkspaceSlackThreadSubscriptions: %v", err)
	}
	return subs
}

// msg is a message at TS saying TEXT, read at instant.
func msg(ts, text string) SlackThreadMessage {
	return SlackThreadMessage{TS: ts, Author: "alice", Text: text, ReceivedAt: instant}
}

func TestASlackThreadSubscriptionRoundTrips(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)

	// Act
	previous, held, err := s.SubscribeSlackThread(context.Background(), slackSub(ws.ID, SlackThreadModeRespond))

	// Assert
	if err != nil || held || previous != "" {
		t.Fatalf("SubscribeSlackThread = %q, %v, %v; want a fresh subscription", previous, held, err)
	}
	subs := slackSubsOf(t, s, ws.ID)
	want := slackSub(ws.ID, SlackThreadModeRespond)
	want.UntoldMessages = []SlackThreadMessage{}
	if len(subs) != 1 || !reflect.DeepEqual(subs[0], want) {
		t.Fatalf("subscriptions = %+v, want %+v", subs, want)
	}
}

func TestSubscribingAgainChangesTheModeAndKeepsTheCursor(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	mustSubscribeSlack(t, s, slackSub(ws.ID, SlackThreadModeListen))
	again := slackSub(ws.ID, SlackThreadModeInteract)
	again.Cursor = 99

	// Act
	previous, held, err := s.SubscribeSlackThread(context.Background(), again)

	// Assert
	if err != nil || !held || previous != SlackThreadModeListen {
		t.Fatalf("SubscribeSlackThread = %q, %v, %v; want the held listen subscription", previous, held, err)
	}
	if got := slackSubsOf(t, s, ws.ID)[0]; got.Mode != SlackThreadModeInteract || got.Cursor != 7 {
		t.Fatalf("subscription = %+v, want interact at the kept cursor 7", got)
	}
}

func TestSubscribingRefusesAClosedOrUnknownSubscriber(t *testing.T) {
	tests := []struct {
		name  string
		setup func(t *testing.T, s *store) WorkspaceID
		want  error
	}{
		{name: "an unknown subscriber", setup: func(t *testing.T, s *store) WorkspaceID { return "ws-nobody" }, want: ErrNotFound},
		{name: "a closed subscriber", setup: func(t *testing.T, s *store) WorkspaceID {
			ws := testWorkspace(t, s)
			if err := s.MarkClosed(context.Background(), ws.ID, Closure{At: instant, By: CloseByUser}); err != nil {
				t.Fatal(err)
			}
			return ws.ID
		}, want: ErrSubscriberClosed},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			ws := tt.setup(t, s)

			// Act
			_, _, err := s.SubscribeSlackThread(context.Background(), slackSub(ws, SlackThreadModeRespond))

			// Assert
			if !errors.Is(err, tt.want) {
				t.Fatalf("SubscribeSlackThread = %v, want %v", err, tt.want)
			}
			if n := scalar[int](t, s, `SELECT COUNT(*) FROM slack_thread_subscriptions`); n != 0 {
				t.Fatalf("%d subscriptions written by a refused subscribe", n)
			}
		})
	}
}

func TestSubscribingRefusesAndRecordsAMalformedSubscription(t *testing.T) {
	// Arrange
	s, log := testStore(t)
	ws := testWorkspace(t, s)
	bad := slackSub(ws.ID, "shout")

	// Act
	_, _, err := s.SubscribeSlackThread(context.Background(), bad)

	// Assert
	if err == nil {
		t.Fatal("SubscribeSlackThread accepted mode \"shout\"")
	}
	if !hasRecord(log, "error", "daemon.wsm.subscribe_slack_thread", "refused a Slack-thread subscription its caller should never have made") {
		t.Fatalf("no error record: %+v", log.Records())
	}
}

func TestSetSlackThreadMode(t *testing.T) {
	tests := []struct {
		name       string
		subscribed bool
		reset      bool
		wantPosts  int
		wantErr    error
	}{
		{name: "a held subscription keeps its posts", subscribed: true, wantPosts: 3},
		{name: "a reset clears its posts", subscribed: true, reset: true, wantPosts: 0},
		{name: "no subscription is not found", wantErr: ErrNotFound},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			ws := testWorkspace(t, s)
			if tt.subscribed {
				mustSubscribeSlack(t, s, slackSub(ws.ID, SlackThreadModeListen))
				corrupt(t, s, `UPDATE slack_thread_subscriptions SET posts_made = 3`)
			}

			// Act
			previous, err := s.SetSlackThreadMode(context.Background(), ws.ID, slackKey, SlackThreadModeRespond, tt.reset)

			// Assert
			if !errors.Is(err, tt.wantErr) {
				t.Fatalf("SetSlackThreadMode = %v, want %v", err, tt.wantErr)
			}
			if tt.wantErr != nil {
				return
			}
			got := slackSubsOf(t, s, ws.ID)[0]
			if previous != SlackThreadModeListen || got.Mode != SlackThreadModeRespond || got.PostsMade != tt.wantPosts {
				t.Fatalf("previous %q, subscription %+v; want listen → respond with %d posts", previous, got, tt.wantPosts)
			}
		})
	}
}

func TestRecordingMessagesStoresThemAndAdvancesTheCursor(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	mustSubscribeSlack(t, s, slackSub(ws.ID, SlackThreadModeRespond))
	if _, err := s.RecordSlackThreadStreamFailure(context.Background(), ws.ID, slackKey, instant, "gns pipe read: exit 1"); err != nil {
		t.Fatal(err)
	}

	// Act
	stored, err := s.RecordSlackThreadMessages(context.Background(), SlackThreadRead{Subscriber: ws.ID, Thread: slackKey,
		Messages: []SlackThreadMessage{msg("1.1", "first"), msg("1.2", "second")}, Cursor: 12, At: instant})

	// Assert
	if err != nil || len(stored) != 2 {
		t.Fatalf("RecordSlackThreadMessages = %v, %v; want both stored", stored, err)
	}
	got := slackSubsOf(t, s, ws.ID)[0]
	if got.Cursor != 12 || got.Pending != 2 || got.FailingSince != nil {
		t.Fatalf("subscription = %+v, want cursor 12, two pending and healthy", got)
	}
}

func TestRecordingMessagesDropsOwnPostsAndRepeats(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	mustSubscribeSlack(t, s, slackSub(ws.ID, SlackThreadModeRespond))
	if err := s.ReserveSlackThreadPost(context.Background(), ws.ID, slackKey, "a1b2c3d4", instant); err != nil {
		t.Fatal(err)
	}
	read := SlackThreadRead{Subscriber: ws.ID, Thread: slackKey, Cursor: 9, At: instant,
		Messages: []SlackThreadMessage{msg("1.1", "hello"), msg("1.2", ":memo: done\n\n"+SlackPostFooter("sample", "a1b2c3d4"))}}
	if _, err := s.RecordSlackThreadMessages(context.Background(), read); err != nil {
		t.Fatal(err)
	}

	// Act
	again, err := s.RecordSlackThreadMessages(context.Background(), read)

	// Assert
	if err != nil || len(again) != 0 {
		t.Fatalf("a repeated read stored %v, %v; want nothing", again, err)
	}
	pending, err := s.SlackThreadPending(context.Background(), ws.ID, slackKey)
	if err != nil || len(pending) != 1 || pending[0].TS != "1.1" {
		t.Fatalf("pending = %v, %v; want only the other person's message", pending, err)
	}
}

func TestRecordingMessagesOfAnEndedSubscriptionKeepsNothing(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)

	// Act
	stored, err := s.RecordSlackThreadMessages(context.Background(), SlackThreadRead{Subscriber: ws.ID, Thread: slackKey,
		Messages: []SlackThreadMessage{msg("1.1", "late")}, Cursor: 3, At: instant})

	// Assert
	if err != nil || stored != nil {
		t.Fatalf("RecordSlackThreadMessages = %v, %v; want nothing kept", stored, err)
	}
	if n := scalar[int](t, s, `SELECT COUNT(*) FROM slack_thread_pending`); n != 0 {
		t.Fatalf("%d pending rows for no subscription", n)
	}
}

func TestStreamFailuresBeginOneStretch(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	mustSubscribeSlack(t, s, slackSub(ws.ID, SlackThreadModeRespond))

	// Act
	first, err1 := s.RecordSlackThreadStreamFailure(context.Background(), ws.ID, slackKey, instant, "one")
	second, err2 := s.RecordSlackThreadStreamFailure(context.Background(), ws.ID, slackKey, instant.Add(time.Minute), "two")

	// Assert
	if err1 != nil || err2 != nil || !first || second {
		t.Fatalf("failures began %v then %v (%v, %v); want the first only", first, second, err1, err2)
	}
	got := slackSubsOf(t, s, ws.ID)[0]
	if got.FailingSince == nil || !got.FailingSince.Equal(instant) || got.FailureDetail != "two" {
		t.Fatalf("subscription = %+v, want failing since the first with the latest detail", got)
	}
}

func TestPostsAreReservedThenCounted(t *testing.T) {
	tests := []struct {
		name    string
		counted bool
		want    int
	}{
		{name: "an agent's post is counted", counted: true, want: 1},
		{name: "an announcement is not", counted: false, want: 0},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			ws := testWorkspace(t, s)
			mustSubscribeSlack(t, s, slackSub(ws.ID, SlackThreadModeRespond))
			if err := s.ReserveSlackThreadPost(context.Background(), ws.ID, slackKey, "n1", instant); err != nil {
				t.Fatal(err)
			}

			// Act
			made, err := s.RecordSlackThreadPost(context.Background(), ws.ID, slackKey, "n1", "1.9", tt.counted)

			// Assert
			if err != nil || made != tt.want {
				t.Fatalf("RecordSlackThreadPost = %d, %v; want %d", made, err, tt.want)
			}
			nonces, err := s.SlackThreadPostNonces(context.Background(), ws.ID, slackKey)
			if err != nil || !reflect.DeepEqual(nonces, []string{"n1"}) {
				t.Fatalf("nonces = %v, %v", nonces, err)
			}
		})
	}
}

func TestReservingAPostWithoutASubscriptionIsNotFound(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)

	// Act
	err := s.ReserveSlackThreadPost(context.Background(), ws.ID, slackKey, "n1", instant)

	// Assert
	if !errors.Is(err, ErrNotFound) {
		t.Fatalf("ReserveSlackThreadPost = %v, want ErrNotFound", err)
	}
}

func TestRecordingADigestTakesItsMessagesAndKeepsWhatItSaid(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	mustSubscribeSlack(t, s, slackSub(ws.ID, SlackThreadModeRespond))
	if _, err := s.RecordSlackThreadMessages(context.Background(), SlackThreadRead{Subscriber: ws.ID, Thread: slackKey,
		Messages: []SlackThreadMessage{msg("1.1", "a"), msg("1.2", "b")}, Cursor: 2, At: instant}); err != nil {
		t.Fatal(err)
	}
	said := []SlackThreadMessage{msg("1.1", "a")}

	// Act
	err := s.RecordSlackThreadDigest(context.Background(), SlackThreadDigest{Subscriber: ws.ID, Thread: slackKey, Turn: "turn-1", Messages: said, Took: []string{"1.1"}})

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	got := slackSubsOf(t, s, ws.ID)[0]
	if got.Pending != 1 || got.UntoldTurn != "turn-1" || !reflect.DeepEqual(got.UntoldMessages, said) {
		t.Fatalf("subscription = %+v, want one pending and the digest recorded", got)
	}
}

func TestUnsubscribingEndsTheSubscriptionAndItsRows(t *testing.T) {
	tests := []struct {
		name string
		end  func(t *testing.T, s *store, ws WorkspaceID)
	}{
		{name: "an unsubscribe", end: func(t *testing.T, s *store, ws WorkspaceID) {
			if ended, err := s.UnsubscribeSlackThread(context.Background(), ws, slackKey); err != nil || !ended {
				t.Fatalf("UnsubscribeSlackThread = %v, %v", ended, err)
			}
		}},
		{name: "the subscriber's close", end: func(t *testing.T, s *store, ws WorkspaceID) {
			if err := s.MarkClosed(context.Background(), ws, Closure{At: instant, By: CloseByUser}); err != nil {
				t.Fatal(err)
			}
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			ws := testWorkspace(t, s)
			mustSubscribeSlack(t, s, slackSub(ws.ID, SlackThreadModeRespond))
			if err := s.ReserveSlackThreadPost(context.Background(), ws.ID, slackKey, "n1", instant); err != nil {
				t.Fatal(err)
			}
			if _, err := s.RecordSlackThreadMessages(context.Background(), SlackThreadRead{Subscriber: ws.ID, Thread: slackKey,
				Messages: []SlackThreadMessage{msg("1.1", "a")}, Cursor: 2, At: instant}); err != nil {
				t.Fatal(err)
			}

			// Act
			tt.end(t, s, ws.ID)

			// Assert
			for _, table := range []string{"slack_thread_subscriptions", "slack_thread_pending", "slack_thread_posts"} {
				if n := scalar[int](t, s, `SELECT COUNT(*) FROM `+table); n != 0 {
					t.Fatalf("%d rows left in %s", n, table)
				}
			}
		})
	}
}

func TestUnsubscribingWithoutASubscriptionAnswersNoneEnded(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)

	// Act
	ended, err := s.UnsubscribeSlackThread(context.Background(), ws.ID, slackKey)

	// Assert
	if err != nil || ended {
		t.Fatalf("UnsubscribeSlackThread = %v, %v; want nothing ended", ended, err)
	}
}

func TestCarriesNonce(t *testing.T) {
	tests := []struct {
		name   string
		text   string
		nonces []string
		want   bool
	}{
		{name: "a footer carrying the nonce", text: "hi\n\n" + SlackPostFooter("ws", "abcd1234"), nonces: []string{"zz", "abcd1234"}, want: true},
		{name: "another workspace's nonce", text: "hi\n\n" + SlackPostFooter("ws", "abcd1234"), nonces: []string{"ffff0000"}},
		{name: "no nonces at all", text: "hi\n\n" + SlackPostFooter("ws", "abcd1234")},
		{name: "a bare mention of the word ref", text: "see ref docs", nonces: []string{"abcd1234"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := CarriesNonce(tt.text, tt.nonces); got != tt.want {
				t.Fatalf("CarriesNonce = %v, want %v", got, tt.want)
			}
		})
	}
}
