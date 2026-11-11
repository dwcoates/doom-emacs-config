package slackwatch

import (
	"context"
	"path/filepath"
	"reflect"
	"strconv"
	"strings"
	"testing"
	"time"

	"agent-repld/internal/gnsclient"
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
	if err != nil || held || sub.Thread != threadKey || sub.Cursor != 40 || sub.Mode != wsm.SlackThreadModeRespond || sub.PostsMade != 0 {
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
	var msgs []gnsclient.PipeMessage
	for i, text := range texts {
		msgs = append(msgs, pipe(int64(50+i), "3."+string(rune('a'+i)), text))
	}
	r.w.ingest(context.Background(), keyOf(r.sub(t)), msgs, 60)
	r.clock.Set(t0.Add(MaxWait))
}

func TestFlushDueTellsNothingBeforeItIsDue(t *testing.T) {
	// Arrange
	r := newRig(t)
	r.subscribe(t, wsm.SlackThreadModeRespond)
	r.w.ingest(context.Background(), keyOf(r.sub(t)), []gnsclient.PipeMessage{pipe(50, "3.a", "hi")}, 50)

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
	r.w.ingest(context.Background(), keyOf(r.sub(t)), []gnsclient.PipeMessage{pipe(70, "4.a", "second")}, 70)
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
	polled := r.w.PollReactions(context.Background(), Lookback)

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
	r.w.PollReactions(context.Background(), Lookback)
	r.w.PollReactions(context.Background(), Lookback)

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
	r.w.PollReactions(context.Background(), Lookback)
	r.gns.reacted = []gnsclient.ReactedMessage{reacted("1.2", "agent-ack")}

	// Act
	r.w.PollReactions(context.Background(), Lookback)
	r.w.PollReactions(context.Background(), Lookback)

	// Assert
	if len(r.intakes.got) != 1 {
		t.Fatalf("fired %d intakes, want one", len(r.intakes.got))
	}
	got := r.intakes.got[0]
	if got.RepoID != r.repo || got.Name != "slack-intake-agent-ack-1-2" || !strings.Contains(got.Prompt, "-originator emoji-ack") || r.w.polls.Fired != 0 {
		t.Fatalf("intake %+v, polls %+v", got, r.w.polls)
	}
}

func TestATypedShortcodeInTheWindowFires(t *testing.T) {
	// Arrange
	r := newRig(t)
	r.settle(t, ackBinding)
	r.w.PollReactions(context.Background(), Lookback)
	recent, old := t0.Add(-time.Minute).Unix(), t0.Add(-time.Hour).Unix()
	r.gns.pages = []gnsclient.SearchPage{{Pages: 1, Messages: []gnsclient.SearchMessage{
		{Channel: "C1", TS: strconvTS(recent), Text: "please :agent-ack:", Permalink: "https://x.slack.com/archives/C1/p1"},
		{Channel: "C1", TS: strconvTS(old), Text: "earlier :agent-ack:", Permalink: "https://x.slack.com/archives/C1/p2"},
	}}}

	// Act
	r.w.PollReactions(context.Background(), Lookback)

	// Assert
	if len(r.intakes.got) != 1 || !strings.Contains(r.intakes.got[0].Prompt, "https://x.slack.com/archives/C1/p1") {
		t.Fatalf("fired %+v; want only the message in the window", r.intakes.got)
	}
}

// strconvTS is a Slack timestamp SEC seconds into the epoch.
func strconvTS(sec int64) string { return strconv.FormatInt(sec, 10) + ".000100" }

func TestAnIntakeThatCannotBeCreatedIsRecordedAndNotFiredAgain(t *testing.T) {
	// Arrange
	r := newRig(t)
	r.settle(t, ackBinding)
	r.w.PollReactions(context.Background(), Lookback)
	r.gns.reacted = []gnsclient.ReactedMessage{reacted("1.2", "agent-ack")}
	r.intakes.fail = errBoom

	// Act
	r.w.PollReactions(context.Background(), Lookback)
	r.intakes.fail = nil
	r.w.PollReactions(context.Background(), Lookback)

	// Assert
	r.record(t, "error", opReactions, "could not create a fired binding's intake workspace; the trigger is recorded fired and creates nothing")
	if len(r.intakes.got) != 0 {
		t.Fatalf("a failed trigger fired again: %+v", r.intakes.got)
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
	listen, err1 := r.w.intakePrompt(ackBinding, "C1", "1.2", "https://x.slack.com/archives/C1/p12")
	subscribed, err2 := r.w.intakePrompt(wsm.SlackReactionBinding{Emoji: "p1-one-shot", Thread: wsm.SlackReactionSubscribed, Mode: wsm.SlackThreadModeRespond, OneShot: true, Priority: &p1},
		"C1", "1.2", "https://x.slack.com/archives/C1/p12")
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
