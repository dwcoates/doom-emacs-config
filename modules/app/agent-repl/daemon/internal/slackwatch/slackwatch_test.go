package slackwatch

import (
	"context"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"reflect"
	"slices"
	"strconv"
	"strings"
	"testing"
	"time"

	"agent-repld/internal/dlog"
	"agent-repld/internal/gnsclient"
	"agent-repld/internal/ids"
	"agent-repld/internal/merge"
	"agent-repld/internal/wsm"
)

func TestNewRefusesAMissingCollaborator(t *testing.T) {
	if _, err := New(Deps{}); err == nil {
		t.Fatal("New accepted no collaborators")
	}
}

func TestUrgent(t *testing.T) {
	tests := []struct {
		text string
		want bool
	}{
		{"stop, that is the wrong branch", true},
		{"  Hold on a second", true},
		{"@agent please rebase", true},
		{"this broke :rotating_light:", true},
		{"please don't stop", false},
		{"looks good to me", false},
		{"", false},
	}
	for _, tt := range tests {
		t.Run(tt.text, func(t *testing.T) {
			if got := Urgent(tt.text); got != tt.want {
				t.Fatalf("Urgent(%q) = %v, want %v", tt.text, got, tt.want)
			}
		})
	}
}

func TestSecrets(t *testing.T) {
	tests := []struct {
		name string
		text string
		want []string
	}{
		{name: "clean", text: ":memo: all good"},
		{name: "a GitHub token", text: "token ghp_" + strings.Repeat("a", 36), want: []string{"GitHub personal access token"}},
		{name: "two kinds", text: "AKIA" + strings.Repeat("B", 16) + " and -----BEGIN RSA PRIVATE KEY-----", want: []string{"AWS access key ID", "PEM-encoded private key"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := Secrets(tt.text); !reflect.DeepEqual(got, tt.want) {
				t.Fatalf("Secrets = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestEmojiRefusal(t *testing.T) {
	tests := []struct {
		text string
		want string
	}{
		{":white_check_mark: tests pass", ""},
		{":+1: yes", ""},
		{"tests pass :white_check_mark:", "-"},
		{":white_check_mark::rocket: two", "-"},
		{":rocket:no space", "-"},
		{":peepo_wave_happy: hi", "peepo_wave_happy"},
	}
	for _, tt := range tests {
		t.Run(tt.text, func(t *testing.T) {
			if got := emojiRefusal(tt.text); got != tt.want {
				t.Fatalf("emojiRefusal(%q) = %q, want %q", tt.text, got, tt.want)
			}
		})
	}
}

func TestPostBlocksCarryTheBannerAndTheNonceFooter(t *testing.T) {
	// Act
	got := string(postBlocks("follower", "n1", ":memo: hi\n"))

	// Assert
	for _, want := range []string{`"type":"context"`, "`follower`", `:memo: hi\n\n_via agent-repl · ` + "`follower`" + ` · ref n1_`} {
		if !strings.Contains(got, want) {
			t.Fatalf("blocks %s lack %q", got, want)
		}
	}
}

func TestDueAt(t *testing.T) {
	at := func(d time.Duration, urgent bool) wsm.SlackThreadMessage {
		return wsm.SlackThreadMessage{ReceivedAt: t0.Add(d), Urgent: urgent}
	}
	tests := []struct {
		name    string
		pending []wsm.SlackThreadMessage
		want    time.Time
	}{
		{name: "a quiet thread waits the long period after its last message", pending: []wsm.SlackThreadMessage{at(0, false), at(10*time.Second, false)}, want: t0.Add(10*time.Second + LowQuiet)},
		{name: "an urgent message shortens the wait", pending: []wsm.SlackThreadMessage{at(0, false), at(2*time.Second, true)}, want: t0.Add(2*time.Second + HighQuiet)},
		{name: "a busy thread is told at the cap", pending: []wsm.SlackThreadMessage{at(0, false), at(MaxWait, false)}, want: t0.Add(MaxWait)},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := DueAt(tt.pending); !got.Equal(tt.want) {
				t.Fatalf("DueAt = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestDueAtFailsHardOnNothingPending(t *testing.T) {
	defer func() {
		if recover() == nil {
			t.Fatal("DueAt of nothing did not panic")
		}
	}()
	DueAt(nil)
}

func TestSubscribeStartsAtThePipesHeadAndAnnouncesItself(t *testing.T) {
	// Arrange
	r := newRig(t)

	// Act
	sub, held, err := r.w.Subscribe(context.Background(), r.ws, threadURL, wsm.SlackThreadModeRespond, wsm.SlackThreadOriginRequested)

	// Assert
	if err != nil || held || sub.Thread != threadKey || sub.Source != wsm.SlackThreadSourceEchoPipe || sub.Cursor != 40 || sub.CursorTS != "" ||
		sub.Mode != wsm.SlackThreadModeRespond || sub.PostsMade != 0 {
		t.Fatalf("Subscribe = %+v, %v, %v", sub, held, err)
	}
	if !reflect.DeepEqual(r.gns.channels, []string{threadKey.Channel}) || len(r.gns.posts) != 1 || !strings.Contains(r.gns.posts[0], ":peepo_wave_happy: agent attached in *respond* mode") {
		t.Fatalf("channels %v, posts %v", r.gns.channels, r.gns.posts)
	}
}

func TestSubscribingAgainAnnouncesOnlyAChangedMode(t *testing.T) {
	tests := []struct {
		name  string
		mode  wsm.SlackThreadMode
		posts int
	}{
		{name: "the same mode", mode: wsm.SlackThreadModeRespond, posts: 1},
		{name: "a new mode", mode: wsm.SlackThreadModeInteract, posts: 2},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			r := newRig(t)
			r.subscribe(t, wsm.SlackThreadModeRespond)
			r.gns.head = 99

			// Act
			sub, held, err := r.w.Subscribe(context.Background(), r.ws, threadURL, tt.mode, wsm.SlackThreadOriginRequested)

			// Assert
			if err != nil || !held || sub.Mode != tt.mode || sub.Cursor != 40 || len(r.gns.posts) != tt.posts {
				t.Fatalf("Subscribe = %+v, %v, %v; posts %d", sub, held, err, len(r.gns.posts))
			}
		})
	}
}

func TestSubscribeRefusesWhenSlackCannotBeRead(t *testing.T) {
	// Arrange
	r := newRig(t)
	r.gns.headErr = gnsFailure

	// Act
	_, _, err := r.w.Subscribe(context.Background(), r.ws, threadURL, wsm.SlackThreadModeRespond, wsm.SlackThreadOriginRequested)

	// Assert
	if refusal := RefusalOf(err); refusal == nil || refusal.Kind != RefusedUnreachable || !strings.Contains(refusal.Detail, "Invalid or expired JWT") {
		t.Fatalf("Subscribe = %v; want an unreachable refusal", err)
	}
	if subs, _ := r.db.SlackThreadSubscriptions(context.Background()); len(subs) != 0 {
		t.Fatalf("a refused subscribe stored %+v", subs)
	}
}

func TestUnsubscribeTellsTheThreadThenEnds(t *testing.T) {
	// Arrange
	r := newRig(t)
	r.subscribe(t, wsm.SlackThreadModeRespond)

	// Act
	ended, announced, err := r.w.Unsubscribe(context.Background(), r.ws, threadURL)
	again, _, err2 := r.w.Unsubscribe(context.Background(), r.ws, threadURL)

	// Assert
	if err != nil || err2 != nil || !ended || !announced || again {
		t.Fatalf("Unsubscribe = %v, %v, %v; again %v, %v", ended, announced, err, again, err2)
	}
	if last := r.gns.posts[len(r.gns.posts)-1]; !strings.Contains(last, ":peepo_run_cry: agent detached") {
		t.Fatalf("the last post is %s", last)
	}
}

func TestAnUnsubscribeWhoseAnnouncementFailsStillEnds(t *testing.T) {
	// Arrange
	r := newRig(t)
	r.subscribe(t, wsm.SlackThreadModeRespond)
	r.gns.postErr = gnsFailure

	// Act
	ended, announced, err := r.w.Unsubscribe(context.Background(), r.ws, threadURL)

	// Assert
	if err != nil || !ended || announced {
		t.Fatalf("Unsubscribe = %v, %v, %v; want ended unannounced", ended, announced, err)
	}
	r.record(t, "error", opVerb, "could not announce the subscription's change in the thread; the change stands")
}

func TestSetModeResetsThePostsAndAnnounces(t *testing.T) {
	// Arrange
	r := newRig(t)
	r.subscribe(t, wsm.SlackThreadModeListen)
	if _, _, err := r.w.Post(context.Background(), r.ws, threadURL, ":memo: forced", true); err != nil {
		t.Fatal(err)
	}

	// Act
	sub, err := r.w.SetMode(context.Background(), r.ws, threadURL, wsm.SlackThreadModeInteract)

	// Assert
	if err != nil || sub.Mode != wsm.SlackThreadModeInteract || sub.PostsMade != 0 {
		t.Fatalf("SetMode = %+v, %v", sub, err)
	}
	if last := r.gns.posts[len(r.gns.posts)-1]; !strings.Contains(last, "agent mode changed to *interact*") {
		t.Fatalf("the last post is %s", last)
	}
}

func TestSetModeRefusesAnUnsubscribedWorkspace(t *testing.T) {
	r := newRig(t)

	_, err := r.w.SetMode(context.Background(), r.ws, threadURL, wsm.SlackThreadModeInteract)

	if refusal := RefusalOf(err); refusal == nil || refusal.Kind != RefusedNotSubscribed {
		t.Fatalf("SetMode = %v", err)
	}
}

func TestPostRefusals(t *testing.T) {
	tests := []struct {
		name      string
		subscribe wsm.SlackThreadMode
		made      int
		text      string
		force     bool
		want      RefusalKind
	}{
		{name: "no subscription", text: ":memo: hi", want: RefusedNotSubscribed},
		{name: "no emoji", subscribe: wsm.SlackThreadModeRespond, text: "hi", want: RefusedNoEmojiPrefix},
		{name: "a reserved emoji", subscribe: wsm.SlackThreadModeRespond, text: ":peepo_run_cry: bye", want: RefusedReservedEmoji},
		{name: "a secret, even forced", subscribe: wsm.SlackThreadModeRespond, text: ":memo: ghp_" + strings.Repeat("a", 36), force: true, want: RefusedSecret},
		{name: "listen mode", subscribe: wsm.SlackThreadModeListen, text: ":memo: hi", want: RefusedListenMode},
		{name: "the post limit", subscribe: wsm.SlackThreadModeRespond, made: DefaultPostLimit, text: ":memo: hi", want: RefusedPostLimit},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			r := newRig(t)
			if tt.subscribe != "" {
				r.subscribe(t, tt.subscribe)
			}
			for range tt.made {
				if _, _, err := r.w.post(context.Background(), r.ws, r.sub(t), ":memo: x", true); err != nil {
					t.Fatal(err)
				}
			}
			before := len(r.gns.posts)

			// Act
			_, _, err := r.w.Post(context.Background(), r.ws, threadURL, tt.text, tt.force)

			// Assert
			if refusal := RefusalOf(err); refusal == nil || refusal.Kind != tt.want {
				t.Fatalf("Post = %v, want refusal %d", err, tt.want)
			}
			if len(r.gns.posts) != before {
				t.Fatalf("a refused post was sent: %v", r.gns.posts[before:])
			}
		})
	}
}

func TestAForcedPostGoesThroughListenMode(t *testing.T) {
	// Arrange
	r := newRig(t)
	r.subscribe(t, wsm.SlackThreadModeListen)

	// Act
	ts, posts, err := r.w.Post(context.Background(), r.ws, threadURL, ":memo: approved", true)

	// Assert
	if err != nil || ts != "9.2" || posts != (Posts{Made: 1, Limit: DefaultPostLimit}) {
		t.Fatalf("Post = %q, %+v, %v", ts, posts, err)
	}
}

func TestReachingThePostLimitSwitchesToListen(t *testing.T) {
	// Arrange
	r := newRig(t)
	r.subscribe(t, wsm.SlackThreadModeRespond)
	for range DefaultPostLimit - 1 {
		if _, _, err := r.w.post(context.Background(), r.ws, r.sub(t), ":memo: x", true); err != nil {
			t.Fatal(err)
		}
	}

	// Act
	_, posts, err := r.w.Post(context.Background(), r.ws, threadURL, ":memo: the last", false)

	// Assert
	if err != nil || posts.Made != DefaultPostLimit {
		t.Fatalf("Post = %+v, %v", posts, err)
	}
	if sub := r.sub(t); sub.Mode != wsm.SlackThreadModeListen {
		t.Fatalf("mode = %s, want listen", sub.Mode)
	}
	if last := r.gns.posts[len(r.gns.posts)-1]; !strings.Contains(last, "reached its limit") {
		t.Fatalf("the last post is %s", last)
	}
}

func TestAPostSlackRefusesIsUnreachable(t *testing.T) {
	// Arrange
	r := newRig(t)
	r.subscribe(t, wsm.SlackThreadModeRespond)
	r.gns.postErr = gnsFailure

	// Act
	_, _, err := r.w.Post(context.Background(), r.ws, threadURL, ":memo: hi", false)

	// Assert
	if refusal := RefusalOf(err); refusal == nil || refusal.Kind != RefusedUnreachable {
		t.Fatalf("Post = %v", err)
	}
}

func TestSubscribeFollowsADirectMessageByPollingFromItsLatestMessage(t *testing.T) {
	tests := []struct {
		name  string
		later []gnsclient.ThreadMessage
		want  string
	}{
		{name: "a thread of its root alone starts at the root", want: dmKey.ThreadTS},
		{name: "a thread with replies starts at the latest", later: []gnsclient.ThreadMessage{reply("1795570600.000001", "a"), reply("1795570700.000002", "b")}, want: "1795570700.000002"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			r := newRig(t)
			r.gns.noPipe, r.gns.later = true, tt.later

			// Act
			sub, held, err := r.w.Subscribe(context.Background(), r.ws, dmURL, wsm.SlackThreadModeInteract, wsm.SlackThreadOriginRequested)

			// Assert
			if err != nil || held || sub.Thread != dmKey || sub.Source != wsm.SlackThreadSourceThreadPoll || sub.CursorTS != tt.want || sub.Cursor != 0 {
				t.Fatalf("Subscribe = %+v, %v, %v; want a poll from %s", sub, held, err, tt.want)
			}
			if !reflect.DeepEqual(r.gns.afters, []string{dmKey.ThreadTS}) || len(r.gns.posts) != 1 {
				t.Fatalf("read after %v, posts %v; want one read after the root and the announcement", r.gns.afters, r.gns.posts)
			}
		})
	}
}

func TestSubscribingToADirectMessageRecordsThatItIsPolled(t *testing.T) {
	// Arrange
	r := newRig(t)

	// Act
	r.subscribeDM(t, wsm.SlackThreadModeInteract)

	// Assert
	rec := r.record(t, "info", opVerb, "GNS keeps no echo pipe for the thread's channel; the thread is followed by polling it")
	want := dlog.Context{"subscriber": string(r.ws.ID), "thread": dmKey.String(), "channel": "D01ABTZFLBA", "pipe": "user.dodge.tools.slack.channel.D01ABTZFLBA.echo",
		"cursor_ts": dmKey.ThreadTS, "poll_every": "15s"}
	if !reflect.DeepEqual(rec.Context, want) {
		t.Fatalf("record context = %+v, want %+v", rec.Context, want)
	}
}

func TestSubscribingToADirectMessageRefusesWhenItsThreadCannotBeRead(t *testing.T) {
	// Arrange
	r := newRig(t)
	r.gns.noPipe, r.gns.laterErr = true, gnsFailure

	// Act
	_, _, err := r.w.Subscribe(context.Background(), r.ws, dmURL, wsm.SlackThreadModeInteract, wsm.SlackThreadOriginRequested)

	// Assert
	if refusal := RefusalOf(err); refusal == nil || refusal.Kind != RefusedUnreachable || !strings.Contains(refusal.Detail, "Invalid or expired JWT") {
		t.Fatalf("Subscribe = %v; want an unreachable refusal", err)
	}
	if subs, _ := r.db.SlackThreadSubscriptions(context.Background()); len(subs) != 0 || len(r.gns.posts) != 0 {
		t.Fatalf("a refused subscribe stored %+v and posted %v", subs, r.gns.posts)
	}
}

func TestPollOnceStoresOthersMessagesAndAdvancesTheCursor(t *testing.T) {
	// Arrange
	r := newRig(t)
	sub := r.subscribeDM(t, wsm.SlackThreadModeInteract)
	own := r.gns.posts[0]
	r.gns.later = []gnsclient.ThreadMessage{reply("1795570600.000001", "can you rebase?"), reply("1795570700.000002", own)}

	// Act
	err := r.w.PollOnce(context.Background(), sub)

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	if got := r.gns.afters[len(r.gns.afters)-1]; got != dmKey.ThreadTS {
		t.Fatalf("polled after %s, want the subscription's cursor %s", got, dmKey.ThreadTS)
	}
	pending, _ := r.db.SlackThreadPending(context.Background(), r.ws.ID, dmKey)
	if got := r.subOf(t, dmKey); len(pending) != 1 || pending[0].TS != "1795570600.000001" || got.CursorTS != "1795570700.000002" {
		t.Fatalf("pending %+v, cursor %s; want only the other's message, the cursor at the latest", pending, got.CursorTS)
	}
	if wait, ok := r.w.untilNextDue(); !ok || wait != LowQuiet {
		t.Fatalf("the digest is due in %v (%v), want %v", wait, ok, LowQuiet)
	}
}

func TestAPollThatReadsNothing(t *testing.T) {
	tests := []struct {
		name        string
		failing     bool
		wantFailing bool
	}{
		{name: "leaves a healthy subscription as it stood"},
		{name: "ends a failing stretch", failing: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			r := newRig(t)
			r.subscribeDM(t, wsm.SlackThreadModeInteract)
			if tt.failing {
				r.gns.laterErr = gnsFailure
				r.w.recordStreamFailure(context.Background(), subKey{ws: r.ws.ID, thread: dmKey}, r.w.PollOnce(context.Background(), r.subOf(t, dmKey)))
				r.gns.laterErr = nil
			}

			// Act
			err := r.w.PollOnce(context.Background(), r.subOf(t, dmKey))

			// Assert
			if got := r.subOf(t, dmKey); err != nil || (got.FailingSince != nil) != tt.wantFailing || got.CursorTS != dmKey.ThreadTS || got.Pending != 0 {
				t.Fatalf("PollOnce = %v, subscription %+v; want healthy at the root's cursor", err, got)
			}
		})
	}
}

func TestAFailingPollBeginsOneStretch(t *testing.T) {
	// Arrange
	r := newRig(t)
	sub := r.subscribeDM(t, wsm.SlackThreadModeInteract)
	r.gns.laterErr = gnsFailure

	// Act
	err := r.w.PollOnce(context.Background(), sub)
	r.w.recordStreamFailure(context.Background(), keyOf(sub), err)

	// Assert
	if got := r.subOf(t, dmKey); got.FailingSince == nil || !strings.Contains(got.FailureDetail, "Invalid or expired JWT") || got.CursorTS != dmKey.ThreadTS {
		t.Fatalf("subscription = %+v, want failing at the kept cursor", got)
	}
	r.record(t, "info", opStream, "the thread's stream began failing; it is read again each window until it succeeds")
}

func TestFollowOnceFollowsBySource(t *testing.T) {
	tests := []struct {
		name       string
		dm         bool
		wantPolled bool
	}{
		{name: "an echo pipe is streamed"},
		{name: "a direct message is polled", dm: true, wantPolled: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			r := newRig(t)
			sub := r.subscribe(t, wsm.SlackThreadModeRespond)
			if tt.dm {
				sub = r.subscribeDM(t, wsm.SlackThreadModeRespond)
			}
			reads := len(r.gns.afters)
			r.gns.stream = []gnsclient.PipeMessage{pipe(41, "2.1", "from the pipe")}

			// Act
			err := r.w.FollowOnce(context.Background(), sub)

			// Assert
			polled := len(r.gns.afters) > reads
			pending, _ := r.db.SlackThreadPending(context.Background(), r.ws.ID, threadKey)
			if err != nil || polled != tt.wantPolled || (len(pending) == 1) == tt.wantPolled {
				t.Fatalf("FollowOnce = %v, polled %v, pipe messages %d; want polled %v", err, polled, len(pending), tt.wantPolled)
			}
		})
	}
}

func TestFollowOnceFailsHardOnAnUndeclaredSource(t *testing.T) {
	// Arrange
	r := newRig(t)
	sub := r.subscribe(t, wsm.SlackThreadModeRespond)
	sub.Source = "carrier_pigeon"
	defer func() {
		// Assert
		if recover() == nil {
			t.Fatal("FollowOnce followed an undeclared source")
		}
	}()

	// Act
	_ = r.w.FollowOnce(context.Background(), sub)
}

func TestAPolledThreadIsPolledAgainAfterThePollInterval(t *testing.T) {
	// Arrange
	r := newRig(t)
	sub := r.subscribeDM(t, wsm.SlackThreadModeInteract)
	reads := len(r.gns.afters)
	ctx, cancel := context.WithCancel(context.Background())
	done := make(chan struct{})

	// Act
	go func() {
		defer close(done)
		r.w.work(ctx, keyOf(sub))
	}()
	wait := <-r.clock.Asked
	cancel()
	<-done

	// Assert
	r.gns.mu.Lock()
	defer r.gns.mu.Unlock()
	if wait != ThreadPollEvery || len(r.gns.afters) != reads+1 {
		t.Fatalf("the worker waited %v after %d polls; want %v after one", wait, len(r.gns.afters)-reads, ThreadPollEvery)
	}
}

func TestStreamOnceStoresOthersMessagesAndDropsTheWorkspacesOwn(t *testing.T) {
	// Arrange
	r := newRig(t)
	sub := r.subscribe(t, wsm.SlackThreadModeRespond)
	own := r.gns.posts[0]
	r.gns.stream = []gnsclient.PipeMessage{pipe(41, "2.1", "can you rebase?"), pipe(42, "2.2", own)}
	r.gns.streamEnd = 45

	// Act
	err := r.w.StreamOnce(context.Background(), sub)

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	pending, _ := r.db.SlackThreadPending(context.Background(), r.ws.ID, threadKey)
	if len(pending) != 1 || pending[0].TS != "2.1" || r.sub(t).Cursor != 45 {
		t.Fatalf("pending %+v, cursor %d; want only the other's message at cursor 45", pending, r.sub(t).Cursor)
	}
	if wait, ok := r.w.untilNextDue(); !ok || wait != LowQuiet {
		t.Fatalf("the digest is due in %v (%v), want %v", wait, ok, LowQuiet)
	}
}

func TestStreamOnceFailsWithoutALogin(t *testing.T) {
	// Arrange
	r := newRig(t)
	sub := r.subscribe(t, wsm.SlackThreadModeRespond)
	r.w.username, r.gns.whoamiErr = "", gnsFailure

	// Act
	err := r.w.StreamOnce(context.Background(), sub)

	// Assert
	if err == nil {
		t.Fatal("StreamOnce streamed with no login")
	}
}

func TestAFailingStreamBeginsOneStretch(t *testing.T) {
	// Arrange
	r := newRig(t)
	sub := r.subscribe(t, wsm.SlackThreadModeRespond)
	r.gns.streamErr = gnsFailure

	// Act
	err := r.w.StreamOnce(context.Background(), sub)
	r.w.recordStreamFailure(context.Background(), keyOf(sub), err)

	// Assert
	if got := r.sub(t); got.FailingSince == nil || !strings.Contains(got.FailureDetail, "Invalid or expired JWT") {
		t.Fatalf("subscription = %+v, want failing", got)
	}
	r.record(t, "info", opStream, "the thread's stream began failing; it is read again each window until it succeeds")
}

// pend stores TEXTS as pending messages read at t0, and moves the clock past
// their digest's due time.
func (r *rig) pend(t *testing.T, texts ...string) {
	t.Helper()
	var msgs []incoming
	for i, text := range texts {
		msgs = append(msgs, incoming{TS: "3." + string(rune('a'+i)), Author: "alice", Text: text})
	}
	r.w.ingest(context.Background(), keyOf(r.sub(t)), msgs, 60, "")
	r.clock.Set(t0.Add(MaxWait))
}

func TestFlushDueTellsNothingBeforeItIsDue(t *testing.T) {
	// Arrange
	r := newRig(t)
	r.subscribe(t, wsm.SlackThreadModeRespond)
	r.w.ingest(context.Background(), keyOf(r.sub(t)), []incoming{{TS: "3.a", Author: "alice", Text: "hi"}}, 50, "")

	// Act
	r.w.FlushDue(context.Background(), r.prompts)

	// Assert
	if len(r.prompts.got) != 0 {
		t.Fatalf("told %+v before it was due", r.prompts.got)
	}
}

func TestFlushDueTellsOneDigestUnderItsDelivery(t *testing.T) {
	tests := []struct {
		name     string
		texts    []string
		delivery wsm.Delivery
	}{
		{name: "an ordinary digest is deferred", texts: []string{"looks good", "one more thing"}, delivery: wsm.DeliveryDeferred},
		{name: "an urgent digest may be interjected", texts: []string{"looks good", "stop, wrong branch"}, delivery: wsm.DeliveryOrdinary},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			r := newRig(t)
			r.subscribe(t, wsm.SlackThreadModeRespond)
			r.pend(t, tt.texts...)

			// Act
			r.w.FlushDue(context.Background(), r.prompts)

			// Assert
			if len(r.prompts.got) != 1 {
				t.Fatalf("told %d digests, want one", len(r.prompts.got))
			}
			got := r.prompts.got[0]
			if got.origin != Origin || got.delivery != tt.delivery || got.ws != r.ws.ID {
				t.Fatalf("told %+v", got)
			}
			if observed := r.notices.ObservedNotices(); len(observed) != 1 || observed[0].Notice != BriefNotice {
				t.Fatalf("measured %+v, want the one digest under %q", observed, BriefNotice)
			}
			for _, want := range []string{BriefNotice, "contract=" + BriefContractAutonomous, "mode=respond", "thread=" + threadURL, "- alice (ts 3.a): " + tt.texts[0], "- alice (ts 3.b): " + tt.texts[1]} {
				if !strings.Contains(got.text, want) {
					t.Fatalf("the digest %q lacks %q", got.text, want)
				}
			}
			if sub := r.sub(t); sub.Pending != 0 || sub.UntoldTurn != "turn-1" || len(sub.UntoldMessages) != 2 {
				t.Fatalf("subscription = %+v, want the digest recorded and nothing pending", sub)
			}
		})
	}
}

func TestAListenDigestCarriesTheListenContract(t *testing.T) {
	// Arrange
	r := newRig(t)
	r.subscribe(t, wsm.SlackThreadModeListen)
	r.pend(t, "hi")

	// Act
	r.w.FlushDue(context.Background(), r.prompts)

	// Assert
	if len(r.prompts.got) != 1 || !strings.Contains(r.prompts.got[0].text, "contract="+BriefContractListen) {
		t.Fatalf("told %+v", r.prompts.got)
	}
}

func TestASupersededDigestIsSaidAgainFirst(t *testing.T) {
	// Arrange
	r := newRig(t)
	r.subscribe(t, wsm.SlackThreadModeRespond)
	r.pend(t, "first")
	r.w.FlushDue(context.Background(), r.prompts)
	r.holds.retired = true
	r.w.ingest(context.Background(), keyOf(r.sub(t)), []incoming{{TS: "4.a", Author: "alice", Text: "second"}}, 70, "")
	r.clock.Set(t0.Add(2 * MaxWait))

	// Act
	r.w.FlushDue(context.Background(), r.prompts)

	// Assert
	if len(r.prompts.got) != 2 {
		t.Fatalf("told %d digests", len(r.prompts.got))
	}
	text := r.prompts.got[1].text
	if first, second := strings.Index(text, "first"), strings.Index(text, "second"); first < 0 || second < first {
		t.Fatalf("the second digest %q does not say the superseded one first", text)
	}
}

func TestAFailedDigestStaysPendingAndIsOwedAgain(t *testing.T) {
	// Arrange
	r := newRig(t)
	r.subscribe(t, wsm.SlackThreadModeRespond)
	r.pend(t, "hi")
	r.prompts.fail = errBoom

	// Act
	r.w.FlushDue(context.Background(), r.prompts)

	// Assert
	r.record(t, "error", opNotice, "could not submit a Slack digest to its subscriber; it is told later")
	if sub := r.sub(t); sub.Pending != 1 {
		t.Fatalf("pending = %d, want the message kept", sub.Pending)
	}
	if wait, ok := r.w.untilNextDue(); !ok || wait != LowQuiet {
		t.Fatalf("owed again in %v (%v), want %v", wait, ok, LowQuiet)
	}
}

func TestADaemonThatDoesNotServeTellsNothing(t *testing.T) {
	// Arrange
	r := newRig(t)
	r.subscribe(t, wsm.SlackThreadModeRespond)
	r.pend(t, "hi")
	r.serves = false

	// Act
	r.w.FlushDue(context.Background(), r.prompts)

	// Assert
	if len(r.prompts.got) != 0 {
		t.Fatalf("a daemon that does not serve told %+v", r.prompts.got)
	}
}

func TestReadMarksTheWorkspacesOwnPosts(t *testing.T) {
	// Arrange
	r := newRig(t)
	r.subscribe(t, wsm.SlackThreadModeRespond)
	r.gns.thread = []gnsclient.ThreadMessage{{TS: "1.1", Author: "alice", Text: "root"}, {TS: "1.2", Author: "dodge", Text: r.gns.posts[0]}}

	// Act
	key, msgs, err := r.w.Read(context.Background(), r.ws.ID, threadURL, 25)

	// Assert
	if err != nil || key != threadKey || len(msgs) != 2 || msgs[0].OwnPost || !msgs[1].OwnPost {
		t.Fatalf("Read = %v, %+v, %v", key, msgs, err)
	}
}

func TestStatusNamesWhatTheReactionSettingsMiss(t *testing.T) {
	// Arrange
	r := newRig(t)

	// Act
	login, subs, _, polls, err := r.w.Status(context.Background())

	// Assert
	if err != nil || login.Username != "dodge" || len(subs) != 0 || !reflect.DeepEqual(polls.Missing, []string{"a watch user", "a repository", "a binding"}) {
		t.Fatalf("Status = %+v, %v, %+v, %v", login, subs, polls, err)
	}
}

// settle completes the reaction settings with BINDINGS.
func (r *rig) settle(t *testing.T, bindings ...wsm.SlackReactionBinding) {
	t.Helper()
	ctx := context.Background()
	if _, err := r.db.SetSlackReactionWatchUser(ctx, "U7HH4U1J4"); err != nil {
		t.Fatal(err)
	}
	if _, err := r.db.SetSlackReactionRepository(ctx, r.repo); err != nil {
		t.Fatal(err)
	}
	for _, b := range bindings {
		if _, err := r.db.BindSlackReaction(ctx, b); err != nil {
			t.Fatal(err)
		}
	}
}

var ackBinding = wsm.SlackReactionBinding{Emoji: "agent-ack", Thread: wsm.SlackReactionAnnounced}

func reacted(ts string, emojis ...string) gnsclient.ReactedMessage {
	return gnsclient.ReactedMessage{Channel: "C1", TS: ts, Permalink: "https://chesscom.slack.com/archives/C1/p" + strings.ReplaceAll(ts, ".", ""), Emojis: emojis}
}

func TestDispatchIDIsTheTriggersHash(t *testing.T) {
	if got := DispatchID("C1", "1.2", "agent-ack"); got != DispatchID("C1", "1.2", "agent-ack") || len(got) != 16 || got == DispatchID("C1", "1.2", "eyes") {
		t.Fatalf("DispatchID = %q", got)
	}
}

func TestAnIncompletePollIsInactive(t *testing.T) {
	// Arrange
	r := newRig(t)

	// Act
	polled := r.w.PollReactions(context.Background())

	// Assert
	if polled || len(r.w.polls.Missing) != 3 {
		t.Fatalf("polled %v, state %+v", polled, r.w.polls)
	}
}

func TestTheFirstReactionsReadIsTheBaselineAndFiresNothing(t *testing.T) {
	// Arrange
	r := newRig(t)
	r.settle(t, ackBinding)
	r.gns.reacted = []gnsclient.ReactedMessage{reacted("1.1", "agent-ack", "eyes")}

	// Act
	r.w.PollReactions(context.Background())
	r.w.PollReactions(context.Background())

	// Assert
	if len(r.intakes.got) != 0 {
		t.Fatalf("a standing reaction fired %+v", r.intakes.got)
	}
	r.record(t, "info", opReactions, "recorded the standing reactions as the baseline; only reactions added from now on fire")
}

func TestAReactionAddedAfterTheBaselineFiresOnce(t *testing.T) {
	// Arrange
	r := newRig(t)
	r.settle(t, ackBinding)
	r.w.PollReactions(context.Background())
	r.gns.reacted = []gnsclient.ReactedMessage{reacted("1.2", "agent-ack")}

	// Act
	r.w.PollReactions(context.Background())
	r.w.PollReactions(context.Background())

	// Assert
	if len(r.intakes.got) != 1 {
		t.Fatalf("fired %d intakes, want one", len(r.intakes.got))
	}
	got := r.intakes.got[0]
	if got.RepoID != r.repo || got.Name != "slack-intake-agent-ack-1-2" || !strings.Contains(r.intakeText(t, got), "-originator emoji-ack") || r.w.polls.Fired != 0 {
		t.Fatalf("intake %+v, polls %+v", got, r.w.polls)
	}
}

func TestATypedShortcodeInTheWindowFires(t *testing.T) {
	// Arrange
	r := newRig(t)
	r.settle(t, ackBinding)
	r.w.PollReactions(context.Background())
	recent, old := t0.Add(-time.Minute).Unix(), t0.Add(-time.Hour).Unix()
	r.gns.pages = []gnsclient.SearchPage{{Pages: 1, Messages: []gnsclient.SearchMessage{
		{Channel: "C1", TS: strconvTS(recent), Text: "please :agent-ack:", Permalink: "https://x.slack.com/archives/C1/p1"},
		{Channel: "C1", TS: strconvTS(old), Text: "earlier :agent-ack:", Permalink: "https://x.slack.com/archives/C1/p2"},
	}}}

	// Act
	r.w.PollReactions(context.Background())

	// Assert
	if len(r.intakes.got) != 1 || !strings.Contains(r.intakeText(t, r.intakes.got[0]), "https://x.slack.com/archives/C1/p1") {
		t.Fatalf("fired %+v; want only the message in the window", r.intakes.got)
	}
}

// strconvTS is a Slack timestamp SEC seconds into the epoch.
func strconvTS(sec int64) string { return strconv.FormatInt(sec, 10) + ".000100" }

func TestSearchWindowStart(t *testing.T) {
	recent, old := t0.Add(-time.Hour), t0.Add(-48*time.Hour)
	tests := []struct {
		name     string
		searched *time.Time
		want     time.Time
	}{
		{name: "the first search reads back MaxLookback", searched: nil, want: t0.Add(-MaxLookback)},
		{name: "a later search reads on from the last one, less the allowance", searched: &recent, want: recent.Add(-IndexAllowance)},
		{name: "a search after a long gap reads back no further than MaxLookback", searched: &old, want: t0.Add(-MaxLookback)},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := SearchWindowStart(t0, tt.searched)

			// Assert
			if !got.Equal(tt.want) {
				t.Fatalf("SearchWindowStart = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestSearchQueryNamesTheUserByID(t *testing.T) {
	if got := SearchQuery("U1"); got != "from:<@U1>" {
		t.Fatalf("SearchQuery = %q", got)
	}
}

func TestAPollSearchesTheWatchUserByID(t *testing.T) {
	// Arrange
	r := newRig(t)
	r.settle(t, ackBinding)

	// Act
	r.w.PollReactions(context.Background())

	// Assert: the poll's query is SearchQuery's, never one spelled apart.
	if !reflect.DeepEqual(r.gns.queries, []string{SearchQuery("U7HH4U1J4")}) {
		t.Fatalf("searched %q", r.gns.queries)
	}
}

// searchedThrough reads where the next search reads on from.
func (r *rig) searchedThrough(t *testing.T) *time.Time {
	t.Helper()
	settings, err := r.db.SlackReactionSettings(context.Background())
	if err != nil {
		t.Fatal(err)
	}
	return settings.SearchedThrough
}

func TestACompleteSearchRecordsItsStart(t *testing.T) {
	// Arrange
	r := newRig(t)
	r.settle(t, ackBinding)

	// Act
	r.w.PollReactions(context.Background())

	// Assert
	if got := r.searchedThrough(t); got == nil || !got.Equal(t0) {
		t.Fatalf("searched through %v, want %v", got, t0)
	}
}

func TestAFailedSearchRecordsNothing(t *testing.T) {
	// Arrange
	r := newRig(t)
	r.settle(t, ackBinding)
	r.gns.searchErr = errBoom

	// Act
	r.w.PollReactions(context.Background())

	// Assert
	if got := r.searchedThrough(t); got != nil {
		t.Fatalf("a failed search recorded %v", got)
	}
	rec := r.record(t, "info", opReactions, "a reaction poll failed in part; the next poll reads again")
	if !strings.Contains(r.w.polls.Failure, "boom") || !strings.Contains(fmt.Sprint(rec.Context["cause"]), "boom") {
		t.Fatalf("failure %q, record %+v", r.w.polls.Failure, rec.Context)
	}
}

func TestASearchAtItsPageCapRecordsNothing(t *testing.T) {
	// Arrange: every page of the cap lies inside the window, and more follow.
	r := newRig(t)
	r.settle(t, ackBinding)
	for range searchPageCap {
		r.gns.pages = append(r.gns.pages, gnsclient.SearchPage{Pages: searchPageCap + 5, Messages: []gnsclient.SearchMessage{
			{Channel: "C1", TS: strconvTS(t0.Add(-time.Minute).Unix()), Text: "chatter"}}})
	}

	// Act
	r.w.PollReactions(context.Background())

	// Assert
	if got := r.searchedThrough(t); got != nil || !strings.Contains(r.w.polls.Failure, "cap of 20 pages") {
		t.Fatalf("searched through %v, failure %q", got, r.w.polls.Failure)
	}
}

// failingSearchRecord is a store whose record of a search's start fails.
type failingSearchRecord struct{ Store }

func (failingSearchRecord) RecordSlackReactionSearch(context.Context, time.Time) error {
	return errBoom
}

func TestASearchWhoseStartCannotBeRecordedFails(t *testing.T) {
	// Arrange
	r := newRig(t)
	r.settle(t, ackBinding)
	r.w.deps.Store = failingSearchRecord{r.db}

	// Act
	r.w.PollReactions(context.Background())

	// Assert
	rec := r.record(t, "info", opReactions, "a reaction poll failed in part; the next poll reads again")
	if !strings.Contains(r.w.polls.Failure, "record where the next search reads on from") || !strings.Contains(fmt.Sprint(rec.Context["cause"]), "boom") {
		t.Fatalf("failure %q, record %+v", r.w.polls.Failure, rec.Context)
	}
}

func TestTheNextSearchReadsOnFromTheLast(t *testing.T) {
	// Arrange: the last complete search began an hour ago.
	r := newRig(t)
	r.settle(t, ackBinding)
	searched := t0.Add(-time.Hour)
	if err := r.db.RecordSlackReactionSearch(context.Background(), searched); err != nil {
		t.Fatal(err)
	}
	lateTS := strconvTS(searched.Add(-5 * time.Minute).Unix())
	r.gns.pages = []gnsclient.SearchPage{{Pages: 1, Messages: []gnsclient.SearchMessage{
		{Channel: "C1", TS: lateTS, Text: "late :agent-ack:", Permalink: "https://x.slack.com/archives/C1/p1"},
		{Channel: "C1", TS: strconvTS(searched.Add(-IndexAllowance - time.Minute).Unix()), Text: "older :agent-ack:", Permalink: "https://x.slack.com/archives/C1/p2"},
	}}}

	// Act
	r.w.PollReactions(context.Background())

	// Assert: only the message inside the allowance fires, and its late
	// indexing is recorded with the lag it shows.
	if len(r.intakes.got) != 1 || !strings.Contains(r.intakeText(t, r.intakes.got[0]), "https://x.slack.com/archives/C1/p1") {
		t.Fatalf("fired %+v; want only the message inside the allowance", r.intakes.got)
	}
	rec := r.record(t, "info", opReactions, "a typed trigger surfaced only in a later search; Slack's search indexed it late")
	late, _ := slackTime(lateTS)
	if rec.Context["index_lag_ms"] != searched.Sub(late).Milliseconds() || rec.Context["emoji"] != "agent-ack" {
		t.Fatalf("record %+v", rec.Context)
	}
}

func TestAnIntakeThatCannotBeCreatedIsRecordedAndNotFiredAgain(t *testing.T) {
	// Arrange
	r := newRig(t)
	r.settle(t, ackBinding)
	r.w.PollReactions(context.Background())
	r.gns.reacted = []gnsclient.ReactedMessage{reacted("1.2", "agent-ack")}
	r.intakes.fail = errBoom

	// Act
	r.w.PollReactions(context.Background())
	r.intakes.fail = nil
	r.w.PollReactions(context.Background())

	// Assert
	r.record(t, "error", opReactions, "could not create a fired binding's intake workspace; the trigger is recorded fired and creates nothing")
	if len(r.intakes.got) != 0 {
		t.Fatalf("a failed trigger fired again: %+v", r.intakes.got)
	}
}

func TestATriggerSlackNamesNoLinkForIsRecordedOnceAndNotFiredAgain(t *testing.T) {
	// Arrange
	r := newRig(t)
	r.settle(t, ackBinding)
	r.w.PollReactions(context.Background())
	linkless := reacted("1.2", "agent-ack")
	linkless.Permalink = ""
	r.gns.reacted = []gnsclient.ReactedMessage{linkless}
	message := "Slack named no link for a triggering message; the trigger is recorded fired and creates nothing"

	// Act
	r.w.PollReactions(context.Background())
	r.w.PollReactions(context.Background())

	// Assert
	rec := r.record(t, "error", opReactions, message)
	if rec.Context["dispatch_id"] != DispatchID("C1", "1.2", "agent-ack") || rec.Context["ts"] != "1.2" || rec.Context["channel"] != "C1" {
		t.Fatalf("record context = %+v", rec.Context)
	}
	n := len(dlog.FilterRecords(r.log.Records(), dlog.RecordQuery{Level: "error", Operation: opReactions, Message: message}))
	if n != 1 || len(r.intakes.got) != 0 {
		t.Fatalf("logged %d times and fired %+v; want one record and no intake", n, r.intakes.got)
	}
}

func TestForkCommand(t *testing.T) {
	p2 := wsm.PriorityP2
	const link = "https://x.slack.com/archives/C1/p1"
	base := "agent-repld workspace create -repository here -relationship peer -lands-into main -origin-master -context fork -fork here -name <NAME> -prompt <PRESCRIPTION>"
	tests := []struct {
		name string
		b    wsm.SlackReactionBinding
		want string
	}{
		{name: "announced", b: ackBinding, want: base + " -slack-thread " + link + " -originator emoji-ack"},
		{name: "subscribed one-shot with a priority", b: wsm.SlackReactionBinding{Emoji: "p2-one-shot", Thread: wsm.SlackReactionSubscribed, Mode: wsm.SlackThreadModeRespond, OneShot: true, Priority: &p2},
			want: base + " -priority p2 -slack-thread " + link + " -slack-subscribe respond -one-shot"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := ForkCommand(tt.b, link); got != tt.want {
				t.Fatalf("ForkCommand =\n%s\nwant\n%s", got, tt.want)
			}
		})
	}
}

func TestEveryBriefComposesFromTheShippedPrompts(t *testing.T) {
	// Arrange
	r := newRig(t)
	r.w.deps.Briefs = merge.BriefsFrom(filepath.Join("..", "..", "..", "prompts"))
	p1 := wsm.PriorityP1
	r.subscribe(t, wsm.SlackThreadModeListen)
	r.pend(t, "hi")

	// Act
	r.w.FlushDue(context.Background(), r.prompts)
	listenValues, err1 := r.w.intakeValues(ackBinding, "C1", "1.2", "https://x.slack.com/archives/C1/p12")
	subscribedValues, err2 := r.w.intakeValues(wsm.SlackReactionBinding{Emoji: "p1-one-shot", Thread: wsm.SlackReactionSubscribed, Mode: wsm.SlackThreadModeRespond, OneShot: true, Priority: &p1},
		"C1", "1.2", "https://x.slack.com/archives/C1/p12")
	listen := r.intakeText(t, IntakeRequest{Brief: BriefIntake, Values: listenValues})
	subscribed := r.intakeText(t, IntakeRequest{Brief: BriefIntake, Values: subscribedValues})
	_, err3 := r.w.deps.Briefs(BriefContractAutonomous, map[string]string{"thread": threadURL})

	// Assert
	if err1 != nil || err2 != nil || err3 != nil || len(r.prompts.got) != 1 {
		t.Fatalf("composing failed: %v, %v, %v; told %d", err1, err2, err3, len(r.prompts.got))
	}
	if !strings.Contains(r.prompts.got[0].text, "Listen mode") || !strings.Contains(listen, "-originator emoji-ack") ||
		!strings.Contains(subscribed, "-slack-subscribe respond -one-shot") || !strings.Contains(subscribed, "ONE-SHOT MODE") {
		t.Fatalf("composed:\n%s\n---\n%s\n---\n%s", r.prompts.got[0].text, listen, subscribed)
	}
}

func TestSubscribeAtCreationRecordsACreationSubscription(t *testing.T) {
	// Arrange
	r := newRig(t)

	// Act
	err := r.w.SubscribeAtCreation(context.Background(), r.ws.ID, r.ws.Name, threadURL, wsm.SlackThreadModeRespond)

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	if sub := r.sub(t); sub.Origin != wsm.SlackThreadOriginCreation || sub.Mode != wsm.SlackThreadModeRespond {
		t.Fatalf("subscription = %+v, want a respond subscription taken at creation", sub)
	}
}

// TestTheDigestSubmitsThroughTheComposer holds the digest to the notifiers'
// one shape (briefprompt.TestEveryNotifierSubmitsThroughTheComposer): it
// submits through briefprompt.Composer, and never calls the prompt handler
// or mints a key itself. It reads its own package, which its unit may.
func TestTheDigestSubmitsThroughTheComposer(t *testing.T) {
	// Arrange
	raw, err := os.ReadFile("digest.go")
	if err != nil {
		t.Fatal(err)
	}
	source := string(raw)

	// Assert
	if !strings.Contains(source, "briefprompt.Composer{") {
		t.Error("digest.go does not submit through briefprompt.Composer")
	}
	if strings.Contains(source, "prompts.Submit(") || strings.Contains(source, ".MintKey(\"") {
		t.Error("digest.go composes or submits a brief prompt by hand")
	}
}

func TestSubscriptionChangesRepublishTheWorkspacesLinks(t *testing.T) {
	tests := []struct {
		name string
		act  func(t *testing.T, r *rig)
		want int
	}{
		{name: "a fresh subscription", act: func(t *testing.T, r *rig) { r.subscribe(t, wsm.SlackThreadModeRespond) }, want: 1},
		{name: "a held subscription's mode change", act: func(t *testing.T, r *rig) {
			r.subscribe(t, wsm.SlackThreadModeRespond)
			r.subscribe(t, wsm.SlackThreadModeInteract)
		}, want: 1},
		{name: "an unsubscribe that ends one", act: func(t *testing.T, r *rig) {
			r.subscribe(t, wsm.SlackThreadModeRespond)
			if _, _, err := r.w.Unsubscribe(context.Background(), r.ws, threadURL); err != nil {
				t.Fatal(err)
			}
		}, want: 2},
		{name: "an unsubscribe that holds none", act: func(t *testing.T, r *rig) {
			if _, _, err := r.w.Unsubscribe(context.Background(), r.ws, threadURL); err != nil {
				t.Fatal(err)
			}
		}, want: 0},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			r := newRig(t)

			// Act
			tt.act(t, r)

			// Assert
			want := slices.Repeat([]ids.WorkspaceID{r.ws.ID}, tt.want)
			if !slices.Equal(r.links.got, want) {
				t.Fatalf("republished %v, want %v", r.links.got, want)
			}
		})
	}
}

func TestAFailedLinksRepublishFailsTheVerbAndIsRecorded(t *testing.T) {
	tests := []struct {
		name string
		verb string
		act  func(r *rig) error
	}{
		{name: "subscribe", verb: "subscribe", act: func(r *rig) error {
			_, _, err := r.w.Subscribe(context.Background(), r.ws, threadURL, wsm.SlackThreadModeRespond, wsm.SlackThreadOriginRequested)
			return err
		}},
		{name: "unsubscribe", verb: "unsubscribe", act: func(r *rig) error {
			_, _, err := r.w.Unsubscribe(context.Background(), r.ws, threadURL)
			return err
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			r := newRig(t)
			if tt.verb == "unsubscribe" {
				r.subscribe(t, wsm.SlackThreadModeRespond)
			}
			r.links.fail = errBoom

			// Act
			err := tt.act(r)

			// Assert
			if !errors.Is(err, errBoom) {
				t.Fatalf("%s = %v; want the republish's failure", tt.verb, err)
			}
			rec := r.record(t, dlog.LevelError, opVerb, "could not republish the workspace's links after its Slack-thread subscription changed")
			if rec.Context["subscriber"] != string(r.ws.ID) || rec.Context["thread"] != threadKey.String() || rec.Context["verb"] != tt.verb || rec.Context["cause"] != errBoom.Error() {
				t.Fatalf("record context = %+v", rec.Context)
			}
		})
	}
}
