package server

import (
	"context"
	"errors"
	"strings"
	"testing"
	"time"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"agent-repld/internal/ids"
	"agent-repld/internal/slackwatch"
	"agent-repld/internal/wsm"
)

// fakeSlack is the Slack watcher: it answers what a test scripts and records
// what it was asked.
type fakeSlack struct {
	sub      wsm.SlackThreadSubscription
	held     bool
	err      error
	ended    bool
	posted   string
	posts    slackwatch.Posts
	messages []slackwatch.Message
	settings wsm.SlackReactionSettings
	polls    slackwatch.PollState
	asked    []string
}

func (f *fakeSlack) Subscribe(_ context.Context, ws slackwatch.Workspace, url string, mode wsm.SlackThreadMode, origin wsm.SlackThreadOrigin) (wsm.SlackThreadSubscription, bool, error) {
	f.asked = append(f.asked, "subscribe|"+ws.Name+"|"+url+"|"+string(mode)+"|"+string(origin))
	return f.sub, f.held, f.err
}
func (f *fakeSlack) Unsubscribe(_ context.Context, ws slackwatch.Workspace, url string) (bool, bool, error) {
	f.asked = append(f.asked, "unsubscribe|"+url)
	return f.ended, f.ended, f.err
}
func (f *fakeSlack) SetMode(_ context.Context, _ slackwatch.Workspace, url string, mode wsm.SlackThreadMode) (wsm.SlackThreadSubscription, error) {
	f.asked = append(f.asked, "mode|"+string(mode))
	return f.sub, f.err
}
func (f *fakeSlack) Post(_ context.Context, _ slackwatch.Workspace, _, text string, force bool) (string, slackwatch.Posts, error) {
	f.asked = append(f.asked, "post|"+text)
	return f.posted, f.posts, f.err
}
func (f *fakeSlack) Read(_ context.Context, _ ids.WorkspaceID, _ string, limit int) (wsm.SlackThreadKey, []slackwatch.Message, error) {
	f.asked = append(f.asked, "read|"+strings.Repeat("x", limit))
	return slackKey, f.messages, f.err
}
func (f *fakeSlack) Status(context.Context) (slackwatch.Login, []wsm.SlackThreadSubscription, wsm.SlackReactionSettings, slackwatch.PollState, error) {
	return slackwatch.Login{Username: "dodge"}, []wsm.SlackThreadSubscription{f.sub}, f.settings, f.polls, f.err
}

func (d *fakeDB) WorkspaceSlackThreadSubscriptions(context.Context, ids.WorkspaceID) ([]wsm.SlackThreadSubscription, error) {
	return d.slackSubs, nil
}
func (d *fakeDB) SlackThreadSubscriptions(context.Context) ([]wsm.SlackThreadSubscription, error) {
	return d.slackSubs, nil
}
func (d *fakeDB) SetSlackReactionWatchUser(_ context.Context, user string) (wsm.SlackReactionSettings, error) {
	d.slackSettings.WatchUser = user
	return d.slackSettings, nil
}
func (d *fakeDB) SetSlackReactionRepository(_ context.Context, repo string) (wsm.SlackReactionSettings, error) {
	d.slackSettings.RepoID = repo
	return d.slackSettings, nil
}
func (d *fakeDB) BindSlackReaction(_ context.Context, b wsm.SlackReactionBinding) (wsm.SlackReactionSettings, error) {
	d.slackSettings.Bindings = append(d.slackSettings.Bindings, b)
	return d.slackSettings, nil
}
func (d *fakeDB) UnbindSlackReaction(_ context.Context, emoji string) (wsm.SlackReactionSettings, error) {
	if _, ok := d.slackSettings.Binding(emoji); !ok {
		return wsm.SlackReactionSettings{}, wsm.ErrNotFound
	}
	d.slackSettings.Bindings = nil
	return d.slackSettings, nil
}

const slackURL = "https://acme.slack.com/archives/C1/p1730000000123456"

var slackKey = wsm.SlackThreadKey{Channel: "C1", ThreadTS: "1730000000.123456"}

func slackSubscription() wsm.SlackThreadSubscription {
	return wsm.SlackThreadSubscription{Subscriber: testWorkspaceID, Thread: slackKey, URL: slackURL, Mode: wsm.SlackThreadModeRespond,
		Origin: wsm.SlackThreadOriginRequested, SubscribedAt: time.Date(2026, 11, 11, 12, 0, 0, 0, time.UTC)}
}

func respond() *agentreplv1.SlackThreadMode {
	return slackwatch.ModeProto(wsm.SlackThreadModeRespond)
}

func TestNewRefusesAMissingSlackWatcher(t *testing.T) {
	// Arrange
	h := newHarness(t)
	deps := h.withoutDep(func(d *Deps) { d.Slack = nil })

	// Act
	_, err := New(deps)

	// Assert
	if err == nil || !strings.Contains(err.Error(), "the Slack watcher") {
		t.Fatalf("New = %v", err)
	}
}

func TestSubscribeSlackThreadAnswersWhatWasTaken(t *testing.T) {
	tests := []struct {
		name string
		held bool
		want string
	}{
		{name: "a new subscription", want: "subscribed"},
		{name: "a held one", held: true, want: "resubscribed"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			h.Slack.sub, h.Slack.held = slackSubscription(), tt.held

			// Act
			resp, err := h.Client.SubscribeSlackThread(context.Background(), connect.NewRequest(&agentreplv1.SubscribeSlackThreadRequest{Subscriber: ref(), ThreadUrl: slackURL, Mode: respond()}))

			// Assert
			if err != nil {
				t.Fatal(err)
			}
			success := resp.Msg.GetSuccess()
			if got := success.ProtoReflect().WhichOneof(success.ProtoReflect().Descriptor().Oneofs().ByName("taken")).Name(); string(got) != tt.want {
				t.Fatalf("taken = %s, want %s", got, tt.want)
			}
			if h.Slack.asked[0] != "subscribe|test|"+slackURL+"|respond|requested" {
				t.Fatalf("asked %v", h.Slack.asked)
			}
			sub := success.GetSubscribed().GetSubscription()
			if tt.held {
				sub = success.GetResubscribed().GetSubscription()
			}
			if sub.GetSubscriber().GetDir() != testWorkspaceDir || sub.GetThread().GetThreadTs() != slackKey.ThreadTS || sub.GetPosts().GetLimit() != slackwatch.DefaultPostLimit {
				t.Fatalf("subscription = %v", sub)
			}
		})
	}
}

func TestSubscribeSlackThreadRefusals(t *testing.T) {
	tests := []struct {
		name string
		err  error
		want string
	}{
		{name: "Slack unreachable", err: &slackwatch.Refusal{Kind: slackwatch.RefusedUnreachable, Detail: "gns: JWT"}, want: "unreachable"},
		{name: "closed meanwhile", err: wsm.ErrSubscriberClosed, want: "subscriber_closed"},
		{name: "forgotten meanwhile", err: wsm.ErrNotFound, want: "unknown_subscriber"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			h.Slack.err = tt.err

			// Act
			resp, err := h.Client.SubscribeSlackThread(context.Background(), connect.NewRequest(&agentreplv1.SubscribeSlackThreadRequest{Subscriber: ref(), ThreadUrl: slackURL, Mode: respond()}))

			// Assert
			if err != nil {
				t.Fatal(err)
			}
			cause := resp.Msg.GetError()
			if got := cause.ProtoReflect().WhichOneof(cause.ProtoReflect().Descriptor().Oneofs().ByName("cause")); got == nil || string(got.Name()) != tt.want {
				t.Fatalf("error = %v, want %s", cause, tt.want)
			}
		})
	}
}

func TestSubscribeSlackThreadRefusesAClosedSubscriber(t *testing.T) {
	// Arrange
	h := newHarness(t)
	closed := h.DB.workspaces[testWorkspaceID]
	closed.Closed = true
	h.DB.workspaces[testWorkspaceID] = closed

	// Act
	resp, err := h.Client.SubscribeSlackThread(context.Background(), connect.NewRequest(&agentreplv1.SubscribeSlackThreadRequest{Subscriber: ref(), ThreadUrl: slackURL, Mode: respond()}))

	// Assert
	if err != nil || resp.Msg.GetError().GetSubscriberClosed() == nil || len(h.Slack.asked) != 0 {
		t.Fatalf("resp %v, %v; asked %v", resp.Msg, err, h.Slack.asked)
	}
}

func TestSlackRequestsAreValidated(t *testing.T) {
	tests := []struct {
		name string
		call func(h *harness) error
	}{
		{name: "a link that is no Slack link", call: func(h *harness) error {
			_, err := h.Client.SubscribeSlackThread(context.Background(), connect.NewRequest(&agentreplv1.SubscribeSlackThreadRequest{Subscriber: ref(), ThreadUrl: "https://example.com", Mode: respond()}))
			return err
		}},
		{name: "no mode", call: func(h *harness) error {
			_, err := h.Client.SetSlackThreadMode(context.Background(), connect.NewRequest(&agentreplv1.SetSlackThreadModeRequest{Subscriber: ref(), ThreadUrl: slackURL}))
			return err
		}},
		{name: "an empty post", call: func(h *harness) error {
			_, err := h.Client.PostToSlackThread(context.Background(), connect.NewRequest(&agentreplv1.PostToSlackThreadRequest{Poster: ref(), ThreadUrl: slackURL}))
			return err
		}},
		{name: "a read past its bound", call: func(h *harness) error {
			n := uint32(500)
			_, err := h.Client.ReadSlackThread(context.Background(), connect.NewRequest(&agentreplv1.ReadSlackThreadRequest{Reader: ref(), ThreadUrl: slackURL, Limit: &n}))
			return err
		}},
		{name: "a malformed watch user", call: func(h *harness) error {
			_, err := h.Client.UpdateSlackReactionSettings(context.Background(), connect.NewRequest(&agentreplv1.UpdateSlackReactionSettingsRequest{
				Change: &agentreplv1.UpdateSlackReactionSettingsRequest_WatchUser{WatchUser: &agentreplv1.SlackReactionWatchUser{UserId: "dodge"}}}))
			return err
		}},
		{name: "a binding with no thread arm", call: func(h *harness) error {
			_, err := h.Client.UpdateSlackReactionSettings(context.Background(), connect.NewRequest(&agentreplv1.UpdateSlackReactionSettingsRequest{
				Change: &agentreplv1.UpdateSlackReactionSettingsRequest_Bind{Bind: &agentreplv1.SlackReactionBinding{Emoji: "agent-ack", Action: &agentreplv1.SlackReactionAction{}}}}))
			return err
		}},
		{name: "no change", call: func(h *harness) error {
			_, err := h.Client.UpdateSlackReactionSettings(context.Background(), connect.NewRequest(&agentreplv1.UpdateSlackReactionSettingsRequest{}))
			return err
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)

			// Act
			err := tt.call(h)

			// Assert
			if connect.CodeOf(err) != connect.CodeInvalidArgument || len(h.Slack.asked) != 0 {
				t.Fatalf("err = %v, asked %v; want InvalidArgument and nothing asked", err, h.Slack.asked)
			}
		})
	}
}

func TestUnsubscribeSlackThreadAnswersWhatWasFound(t *testing.T) {
	tests := []struct {
		name  string
		ended bool
	}{
		{name: "ended", ended: true},
		{name: "none held", ended: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			h.Slack.ended = tt.ended

			// Act
			resp, err := h.Client.UnsubscribeSlackThread(context.Background(), connect.NewRequest(&agentreplv1.UnsubscribeSlackThreadRequest{Subscriber: ref(), ThreadUrl: slackURL}))

			// Assert
			if err != nil || (resp.Msg.GetSuccess().GetUnsubscribed() != nil) != tt.ended || (resp.Msg.GetSuccess().GetNotSubscribed() != nil) == tt.ended {
				t.Fatalf("resp %v, %v", resp.Msg, err)
			}
		})
	}
}

func TestUnsubscribeSlackThreadFailsOnTheStoresFailure(t *testing.T) {
	h := newHarness(t)
	h.Slack.err = errors.New("the store is gone")

	_, err := h.Client.UnsubscribeSlackThread(context.Background(), connect.NewRequest(&agentreplv1.UnsubscribeSlackThreadRequest{Subscriber: ref(), ThreadUrl: slackURL}))

	if connect.CodeOf(err) != connect.CodeInternal {
		t.Fatalf("err = %v, want Internal", err)
	}
}

func TestSetSlackThreadMode(t *testing.T) {
	tests := []struct {
		name string
		err  error
		ok   bool
	}{
		{name: "a held subscription", ok: true},
		{name: "none held", err: &slackwatch.Refusal{Kind: slackwatch.RefusedNotSubscribed}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			h.Slack.sub, h.Slack.err = slackSubscription(), tt.err

			// Act
			resp, err := h.Client.SetSlackThreadMode(context.Background(), connect.NewRequest(&agentreplv1.SetSlackThreadModeRequest{Subscriber: ref(), ThreadUrl: slackURL, Mode: respond()}))

			// Assert
			if err != nil || (resp.Msg.GetSuccess() != nil) != tt.ok || (resp.Msg.GetError().GetNotSubscribed() != nil) == tt.ok {
				t.Fatalf("resp %v, %v", resp.Msg, err)
			}
		})
	}
}

func TestPostToSlackThreadAnswersEachRefusalAsItsArm(t *testing.T) {
	tests := []struct {
		name  string
		err   error
		check func(*agentreplv1.PostToSlackThreadError) bool
	}{
		{name: "not subscribed", err: &slackwatch.Refusal{Kind: slackwatch.RefusedNotSubscribed}, check: func(e *agentreplv1.PostToSlackThreadError) bool { return e.GetNotSubscribed() != nil }},
		{name: "no emoji", err: &slackwatch.Refusal{Kind: slackwatch.RefusedNoEmojiPrefix}, check: func(e *agentreplv1.PostToSlackThreadError) bool { return e.GetNoEmojiPrefix() != nil }},
		{name: "a reserved emoji", err: &slackwatch.Refusal{Kind: slackwatch.RefusedReservedEmoji, Emoji: "peepo_run_cry"},
			check: func(e *agentreplv1.PostToSlackThreadError) bool {
				return e.GetReservedEmoji().GetEmoji() == "peepo_run_cry"
			}},
		{name: "listen mode", err: &slackwatch.Refusal{Kind: slackwatch.RefusedListenMode}, check: func(e *agentreplv1.PostToSlackThreadError) bool { return e.GetListenMode() != nil }},
		{name: "the post limit", err: &slackwatch.Refusal{Kind: slackwatch.RefusedPostLimit, Posts: slackwatch.Posts{Made: 20, Limit: 20}},
			check: func(e *agentreplv1.PostToSlackThreadError) bool {
				return e.GetPostLimitReached().GetPosts().GetMade() == 20
			}},
		{name: "a secret", err: &slackwatch.Refusal{Kind: slackwatch.RefusedSecret, Secrets: []string{"Slack token"}},
			check: func(e *agentreplv1.PostToSlackThreadError) bool { return len(e.GetSecret().GetKinds()) == 1 }},
		{name: "unreachable", err: &slackwatch.Refusal{Kind: slackwatch.RefusedUnreachable, Detail: "gns: JWT"},
			check: func(e *agentreplv1.PostToSlackThreadError) bool { return e.GetUnreachable().GetDetail() == "gns: JWT" }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			h.Slack.err = tt.err

			// Act
			resp, err := h.Client.PostToSlackThread(context.Background(), connect.NewRequest(&agentreplv1.PostToSlackThreadRequest{Poster: ref(), ThreadUrl: slackURL, Text: ":memo: hi"}))

			// Assert
			if err != nil || !tt.check(resp.Msg.GetError()) {
				t.Fatalf("resp %v, %v", resp.Msg, err)
			}
		})
	}
}

func TestPostToSlackThreadAnswersThePost(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.Slack.posted, h.Slack.posts = "1.9", slackwatch.Posts{Made: 3, Limit: 20}

	// Act
	resp, err := h.Client.PostToSlackThread(context.Background(), connect.NewRequest(&agentreplv1.PostToSlackThreadRequest{Poster: ref(), ThreadUrl: slackURL, Text: ":memo: hi"}))

	// Assert
	if err != nil || resp.Msg.GetSuccess().GetTs() != "1.9" || resp.Msg.GetSuccess().GetPosts().GetMade() != 3 {
		t.Fatalf("resp %v, %v", resp.Msg, err)
	}
}

func TestReadSlackThreadAnswersTheMessagesWithTheDefaultLimit(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.Slack.messages = []slackwatch.Message{{TS: "1.1", Author: "alice", Text: "hi"}, {TS: "1.2", Author: "dodge", Text: "yo", OwnPost: true}}

	// Act
	resp, err := h.Client.ReadSlackThread(context.Background(), connect.NewRequest(&agentreplv1.ReadSlackThreadRequest{Reader: ref(), ThreadUrl: slackURL}))

	// Assert
	msgs := resp.Msg.GetSuccess().GetMessages()
	if err != nil || len(msgs) != 2 || !msgs[1].GetOwnPost() || resp.Msg.GetSuccess().GetThread().GetThreadTs() != slackKey.ThreadTS {
		t.Fatalf("resp %v, %v", resp.Msg, err)
	}
	if h.Slack.asked[0] != "read|"+strings.Repeat("x", defaultSlackReadLimit) {
		t.Fatalf("asked %v", h.Slack.asked)
	}
}

func TestListSlackThreadSubscriptions(t *testing.T) {
	tests := []struct {
		name       string
		subscriber *workspacev1.WorkspaceRef
	}{
		{name: "one workspace's", subscriber: ref()},
		{name: "every workspace's"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			h.DB.slackSubs = []wsm.SlackThreadSubscription{slackSubscription()}

			// Act
			resp, err := h.Client.ListSlackThreadSubscriptions(context.Background(), connect.NewRequest(&agentreplv1.ListSlackThreadSubscriptionsRequest{Subscriber: tt.subscriber}))

			// Assert
			subs := resp.Msg.GetSuccess().GetSubscriptions()
			if err != nil || len(subs) != 1 || subs[0].GetSubscriber().GetDir() != testWorkspaceDir {
				t.Fatalf("resp %v, %v", resp.Msg, err)
			}
		})
	}
}

func TestGetSlackStatusAnswersTheLoginSubscriptionsAndPolls(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.Slack.sub = slackSubscription()
	h.Slack.polls = slackwatch.PollState{Missing: []string{"a binding"}}

	// Act
	resp, err := h.Client.GetSlackStatus(context.Background(), connect.NewRequest(&agentreplv1.GetSlackStatusRequest{}))

	// Assert
	success := resp.Msg.GetSuccess()
	if err != nil || success.GetLogin().GetKnown().GetUsername() != "dodge" || len(success.GetSubscriptions()) != 1 ||
		len(success.GetReactionPolls().GetInactive().GetMissing()) != 1 || success.GetReactionSettings().GetPending() == nil {
		t.Fatalf("resp %v, %v", resp.Msg, err)
	}
}

func TestUpdateSlackReactionSettings(t *testing.T) {
	tests := []struct {
		name   string
		change *agentreplv1.UpdateSlackReactionSettingsRequest
		check  func(*agentreplv1.UpdateSlackReactionSettingsResponse) bool
	}{
		{name: "the watch user", change: &agentreplv1.UpdateSlackReactionSettingsRequest{Change: &agentreplv1.UpdateSlackReactionSettingsRequest_WatchUser{WatchUser: &agentreplv1.SlackReactionWatchUser{UserId: "U7HH4U1J4"}}},
			check: func(r *agentreplv1.UpdateSlackReactionSettingsResponse) bool {
				return r.GetSuccess().GetSettings().GetWatchUser().GetUserId() == "U7HH4U1J4"
			}},
		{name: "a binding", change: &agentreplv1.UpdateSlackReactionSettingsRequest{Change: &agentreplv1.UpdateSlackReactionSettingsRequest_Bind{Bind: slackwatch.BindingProto(wsm.SlackReactionBinding{Emoji: "agent-ack", Thread: wsm.SlackReactionAnnounced})}},
			check: func(r *agentreplv1.UpdateSlackReactionSettingsResponse) bool {
				return len(r.GetSuccess().GetSettings().GetBindings()) == 1
			}},
		{name: "an unbound emoji", change: &agentreplv1.UpdateSlackReactionSettingsRequest{Change: &agentreplv1.UpdateSlackReactionSettingsRequest_Unbind{Unbind: "eyes"}},
			check: func(r *agentreplv1.UpdateSlackReactionSettingsResponse) bool {
				return r.GetError().GetNotBound().GetEmoji() == "eyes"
			}},
		{name: "an unknown repository", change: &agentreplv1.UpdateSlackReactionSettingsRequest{Change: &agentreplv1.UpdateSlackReactionSettingsRequest_Repository{Repository: &workspacev1.RepositoryRef{Id: "repo-nobody"}}},
			check: func(r *agentreplv1.UpdateSlackReactionSettingsResponse) bool {
				return r.GetError().GetUnknownRepository() != nil
			}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)

			// Act
			resp, err := h.Client.UpdateSlackReactionSettings(context.Background(), connect.NewRequest(tt.change))

			// Assert
			if err != nil || !tt.check(resp.Msg) {
				t.Fatalf("resp %v, %v", resp.Msg, err)
			}
		})
	}
}

// TestSlackSubscribeOf pins the mode a create subscribes in, including a
// create no Slack message started, which carries no origin at all.
func TestSlackSubscribeOf(t *testing.T) {
	for _, tc := range []struct {
		name   string
		origin *agentreplv1.CreateWorkspaceSlackOrigin
		want   wsm.SlackThreadMode
	}{
		{name: "no Slack origin", origin: nil, want: ""},
		{name: "an origin asking no subscription", origin: &agentreplv1.CreateWorkspaceSlackOrigin{ThreadUrl: slackURL}, want: ""},
		{name: "an origin subscribing in respond", origin: &agentreplv1.CreateWorkspaceSlackOrigin{ThreadUrl: slackURL, Subscribe: slackwatch.ModeProto(wsm.SlackThreadModeRespond)}, want: wsm.SlackThreadModeRespond},
	} {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := slackSubscribeOf(tc.origin)

			// Assert
			if got != tc.want {
				t.Errorf("slackSubscribeOf = %q, want %q", got, tc.want)
			}
		})
	}
}

// TestSlackSubscribeOfPanicsOnAModelessSubscription pins that a subscription
// validation let through with no mode fails hard rather than subscribing in none.
func TestSlackSubscribeOfPanicsOnAModelessSubscription(t *testing.T) {
	// Arrange
	origin := &agentreplv1.CreateWorkspaceSlackOrigin{ThreadUrl: slackURL, Subscribe: &agentreplv1.SlackThreadMode{}}
	defer func() {
		// Assert
		if recover() == nil {
			t.Fatal("slackSubscribeOf returned for a subscription naming no mode, want a panic")
		}
	}()

	// Act
	slackSubscribeOf(origin)
}
