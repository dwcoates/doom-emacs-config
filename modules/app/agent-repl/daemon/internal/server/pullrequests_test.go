package server

import (
	"context"
	"errors"
	"testing"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ghclient"
	"agent-repld/internal/ids"
	"agent-repld/internal/prwatch"
	"agent-repld/internal/wsm"
)

// fakePullRequests anchors from a script and posts what it is told.
type fakePullRequests struct {
	anchorErr error
	ended     *wsm.PullRequestEnd
	posted    prwatch.Posted
	postErr   error
	comments  []string
}

func (f *fakePullRequests) Anchor(_ context.Context, ws ids.WorkspaceID, pr wsm.PullRequestKey, events wsm.PullRequestEvents, origin wsm.PullRequestOrigin) (wsm.PullRequestSubscription, *agentreplv1.PullRequestStatus, error) {
	sub := wsm.PullRequestSubscription{Subscriber: ws, PullRequest: pr, Events: events, Origin: origin, Told: "anchored", Ended: f.ended}
	if f.anchorErr != nil {
		return sub, nil, f.anchorErr
	}
	return sub, &agentreplv1.PullRequestStatus{Title: "Fix it"}, nil
}

func (f *fakePullRequests) Comment(_ context.Context, _ ids.WorkspaceID, _ wsm.PullRequestKey, inReplyTo *uint64, markdown string) (prwatch.Posted, error) {
	f.comments = append(f.comments, markdown)
	return f.posted, f.postErr
}

// fakeRemotes answers every workspace's origin with url.
type fakeRemotes struct{ url string }

func (f *fakeRemotes) RemoteURL(context.Context, string, string) (string, error) {
	if f.url == "" {
		return "", errors.New("fakeRemotes: no origin scripted")
	}
	return f.url, nil
}

func (d *fakeDB) SubscribePullRequest(_ context.Context, sub wsm.PullRequestSubscription) (bool, error) {
	if d.prSubscribeErr != nil {
		return false, d.prSubscribeErr
	}
	d.prSubs = append(d.prSubs, sub)
	return d.prReplaced, nil
}

func (d *fakeDB) UnsubscribePullRequest(_ context.Context, _ ids.WorkspaceID, pr wsm.PullRequestKey) (bool, error) {
	for i, sub := range d.prSubs {
		if sub.PullRequest == pr {
			d.prSubs = append(d.prSubs[:i], d.prSubs[i+1:]...)
			return true, nil
		}
	}
	return false, nil
}

func (d *fakeDB) WorkspacePullRequestSubscriptions(context.Context, ids.WorkspaceID) ([]wsm.PullRequestSubscription, error) {
	return d.prSubs, nil
}

var (
	testPR    = wsm.PullRequestKey{Owner: "ChessCom", Repo: "explanation-engine", Number: 8047}
	allEvents = []agentreplv1.PullRequestNoticeEvent{agentreplv1.PullRequestNoticeEvent_PULL_REQUEST_NOTICE_EVENT_COMMENT}
)

func subscribePR(text string) *connect.Request[agentreplv1.SubscribePullRequestRequest] {
	return connect.NewRequest(&agentreplv1.SubscribePullRequestRequest{Subscriber: ref(),
		PullRequest: &agentreplv1.SubscribePullRequestRequest_Text{Text: text}, Events: allEvents})
}

func TestSubscribePullRequestRecordsTheAnchoredSubscription(t *testing.T) {
	tests := []struct {
		name      string
		text      string
		origin    string
		replaced  bool
		wantTaken string
	}{
		{name: "a url", text: "https://github.com/ChessCom/explanation-engine/pull/8047", wantTaken: "subscribed"},
		{name: "a number in the subscriber's repository", text: "#8047", origin: "git@github.com:ChessCom/explanation-engine.git", wantTaken: "subscribed"},
		{name: "a replaced subscription", text: "https://github.com/ChessCom/explanation-engine/pull/8047", replaced: true, wantTaken: "resubscribed"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			h.Remotes.url = tt.origin
			h.DB.prReplaced = tt.replaced

			// Act
			resp, err := h.Client.SubscribePullRequest(context.Background(), subscribePR(tt.text))

			// Assert
			if err != nil {
				t.Fatalf("SubscribePullRequest: %v", err)
			}
			success := resp.Msg.GetSuccess()
			taken, sub := "subscribed", success.GetSubscribed().GetSubscription()
			if r := success.GetResubscribed(); r != nil {
				taken, sub = "resubscribed", r.GetSubscription()
			}
			if taken != tt.wantTaken || success.GetStatus().GetTitle() != "Fix it" || prwatch.KeyOf(success.GetPullRequest()) != testPR {
				t.Fatalf("success = %v, want %s with the status", success, tt.wantTaken)
			}
			if sub.GetSubscriber().GetDir() != testWorkspaceDir || sub.GetOrigin().GetRequested() == nil {
				t.Fatalf("subscription = %v, want the requested subscription naming its subscriber's directory", sub)
			}
			if len(h.DB.prSubs) != 1 || h.DB.prSubs[0].Told != "anchored" || h.DB.prSubs[0].Events != wsm.NewPullRequestEvents(wsm.PullRequestEventComment) {
				t.Fatalf("recorded = %+v, want the anchored subscription to comments", h.DB.prSubs)
			}
		})
	}
}

func TestSubscribePullRequestToAnEndedPullRequestIsAnAnswer(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.PRs.ended = &wsm.PullRequestEnd{Merged: true}

	// Act
	resp, err := h.Client.SubscribePullRequest(context.Background(), subscribePR("https://github.com/ChessCom/explanation-engine/pull/8047"))

	// Assert
	if err != nil || resp.Msg.GetSuccess().GetAlreadyEnded() == nil || len(h.DB.prSubs) != 0 {
		t.Fatalf("SubscribePullRequest = %v, %v; want already_ended and nothing recorded", resp, err)
	}
}

func TestSubscribePullRequestIsRefused(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(h *harness) *connect.Request[agentreplv1.SubscribePullRequestRequest]
		got     func(*agentreplv1.SubscribePullRequestError) bool
	}{
		{name: "text that names no pull request", arrange: func(*harness) *connect.Request[agentreplv1.SubscribePullRequestRequest] {
			return subscribePR("the PR")
		}, got: func(e *agentreplv1.SubscribePullRequestError) bool { return e.GetUnparsable().GetText() == "the PR" }},
		{name: "a pull request GitHub will not show", arrange: func(h *harness) *connect.Request[agentreplv1.SubscribePullRequestRequest] {
			h.PRs.anchorErr = &ghclient.Failure{Kind: ghclient.KindNotFound, Status: 404}
			return subscribePR("https://github.com/ChessCom/explanation-engine/pull/8047")
		}, got: func(e *agentreplv1.SubscribePullRequestError) bool {
			return e.GetUnreadable().GetCause().GetNotFound() != nil
		}},
		{name: "a subscriber that closed meanwhile", arrange: func(h *harness) *connect.Request[agentreplv1.SubscribePullRequestRequest] {
			h.DB.prSubscribeErr = wsm.ErrSubscriberClosed
			return subscribePR("https://github.com/ChessCom/explanation-engine/pull/8047")
		}, got: func(e *agentreplv1.SubscribePullRequestError) bool { return e.GetSubscriberClosed() != nil }},
		{name: "an unknown subscriber", arrange: func(*harness) *connect.Request[agentreplv1.SubscribePullRequestRequest] {
			return connect.NewRequest(&agentreplv1.SubscribePullRequestRequest{Subscriber: &workspacev1.WorkspaceRef{Id: "ws-none"},
				PullRequest: &agentreplv1.SubscribePullRequestRequest_Text{Text: "#1"}, Events: allEvents})
		}, got: func(e *agentreplv1.SubscribePullRequestError) bool { return e.GetUnknownSubscriber() != nil }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			req := tt.arrange(h)

			// Act
			resp, err := h.Client.SubscribePullRequest(context.Background(), req)

			// Assert
			if err != nil || !tt.got(resp.Msg.GetError()) {
				t.Fatalf("SubscribePullRequest = %v, %v; want the refusal", resp, err)
			}
		})
	}
}

func TestSubscribePullRequestValidation(t *testing.T) {
	tests := []struct {
		name string
		req  *agentreplv1.SubscribePullRequestRequest
	}{
		{name: "no pull request", req: &agentreplv1.SubscribePullRequestRequest{Subscriber: ref(), Events: allEvents}},
		{name: "no events", req: &agentreplv1.SubscribePullRequestRequest{Subscriber: ref(), PullRequest: &agentreplv1.SubscribePullRequestRequest_Text{Text: "#1"}}},
		{name: "an unspecified event", req: &agentreplv1.SubscribePullRequestRequest{Subscriber: ref(), PullRequest: &agentreplv1.SubscribePullRequestRequest_Text{Text: "#1"},
			Events: []agentreplv1.PullRequestNoticeEvent{agentreplv1.PullRequestNoticeEvent_PULL_REQUEST_NOTICE_EVENT_UNSPECIFIED}}},
		{name: "a ref missing its number", req: &agentreplv1.SubscribePullRequestRequest{Subscriber: ref(),
			PullRequest: &agentreplv1.SubscribePullRequestRequest_Ref{Ref: &agentreplv1.PullRequestRef{Owner: "o", Repo: "r"}}, Events: allEvents}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)

			// Act
			_, err := h.Client.SubscribePullRequest(context.Background(), connect.NewRequest(tt.req))

			// Assert
			if connect.CodeOf(err) != connect.CodeInvalidArgument || len(h.DB.prSubs) != 0 {
				t.Fatalf("SubscribePullRequest = %v, want invalid_argument and nothing recorded", err)
			}
		})
	}
}

func TestUnsubscribePullRequestAnswersWhetherOneStood(t *testing.T) {
	for _, held := range []bool{true, false} {
		t.Run(map[bool]string{true: "a subscription ended", false: "none held"}[held], func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			if held {
				h.DB.prSubs = []wsm.PullRequestSubscription{{Subscriber: testWorkspaceID, PullRequest: testPR}}
			}

			// Act
			resp, err := h.Client.UnsubscribePullRequest(context.Background(), connect.NewRequest(&agentreplv1.UnsubscribePullRequestRequest{
				Subscriber: ref(), PullRequest: prwatch.RefProto(testPR)}))

			// Assert
			if err != nil || (resp.Msg.GetSuccess().GetUnsubscribed() != nil) != held || (resp.Msg.GetSuccess().GetNotSubscribed() != nil) == held {
				t.Fatalf("UnsubscribePullRequest = %v, %v; want ended %v", resp, err, held)
			}
		})
	}
}

func TestListPullRequestSubscriptionsLeavesOutTheEnded(t *testing.T) {
	// Arrange
	h := newHarness(t)
	other := wsm.PullRequestKey{Owner: "o", Repo: "r", Number: 1}
	h.DB.prSubs = []wsm.PullRequestSubscription{
		{Subscriber: testWorkspaceID, PullRequest: testPR, Events: wsm.AllPullRequestEvents(), Origin: wsm.PullRequestOriginOwnedLink},
		{Subscriber: testWorkspaceID, PullRequest: other, Events: wsm.AllPullRequestEvents(), Origin: wsm.PullRequestOriginRequested, Ended: &wsm.PullRequestEnd{}},
	}

	// Act
	resp, err := h.Client.ListPullRequestSubscriptions(context.Background(), connect.NewRequest(&agentreplv1.ListPullRequestSubscriptionsRequest{Subscriber: ref()}))

	// Assert
	subs := resp.Msg.GetSuccess().GetSubscriptions()
	if err != nil || len(subs) != 1 || prwatch.KeyOf(subs[0].GetSubscription().GetPullRequest()) != testPR {
		t.Fatalf("ListPullRequestSubscriptions = %v, %v; want only the live one", resp, err)
	}
}

func TestCommentOnPullRequestAnswersWhatWasPosted(t *testing.T) {
	refused := &ghclient.Failure{Kind: ghclient.KindRefused, Status: 422, Message: "Validation Failed"}
	tests := []struct {
		name    string
		postErr error
		check   func(*agentreplv1.CommentOnPullRequestResponse) bool
	}{
		{name: "posted", check: func(r *agentreplv1.CommentOnPullRequestResponse) bool {
			return r.GetSuccess().GetCommentId() == 77 && r.GetSuccess().GetUrl() == "u77"
		}},
		{name: "a reply to no thread", postErr: prwatch.ErrUnknownThread, check: func(r *agentreplv1.CommentOnPullRequestResponse) bool {
			return r.GetError().GetUnknownThread() != nil
		}},
		{name: "refused by GitHub", postErr: refused, check: func(r *agentreplv1.CommentOnPullRequestResponse) bool {
			return r.GetError().GetNotPosted().GetCause().GetRefused().GetHttpStatus() == 422
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			h.PRs.posted, h.PRs.postErr = prwatch.Posted{ID: 77, URL: "u77"}, tt.postErr

			// Act
			resp, err := h.Client.CommentOnPullRequest(context.Background(), connect.NewRequest(&agentreplv1.CommentOnPullRequestRequest{
				Workspace: ref(), PullRequest: prwatch.RefProto(testPR),
				Placement: &agentreplv1.CommentOnPullRequestRequest_Conversation{Conversation: &agentreplv1.CommentOnPullRequestConversation{Markdown: "done"}}}))

			// Assert
			if err != nil || !tt.check(resp.Msg) || len(h.PRs.comments) != 1 {
				t.Fatalf("CommentOnPullRequest = %v, %v", resp, err)
			}
		})
	}
}

func TestCommentOnPullRequestThatFailsIsRecorded(t *testing.T) {
	// Arrange
	h := newHarness(t)
	log := dlog.NewTestLogger()
	h.Surfaces.workspace = log
	h.PRs.postErr = &ghclient.Failure{Kind: ghclient.KindUnreachable, Message: "down"}

	// Act
	_, err := h.Client.CommentOnPullRequest(context.Background(), connect.NewRequest(&agentreplv1.CommentOnPullRequestRequest{
		Workspace: ref(), PullRequest: prwatch.RefProto(testPR),
		Placement: &agentreplv1.CommentOnPullRequestRequest_Conversation{Conversation: &agentreplv1.CommentOnPullRequestConversation{Markdown: "done"}}}))

	// Assert
	if err != nil {
		t.Fatalf("CommentOnPullRequest: %v", err)
	}
	if _, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: "info", Operation: opPullRequests, Message: "the comment was not posted"}); !ok {
		t.Fatalf("no record of the failed post")
	}
}

func TestCommentOnPullRequestValidation(t *testing.T) {
	tests := []struct {
		name string
		req  *agentreplv1.CommentOnPullRequestRequest
	}{
		{name: "no placement", req: &agentreplv1.CommentOnPullRequestRequest{Workspace: ref(), PullRequest: prwatch.RefProto(testPR)}},
		{name: "a blank comment", req: &agentreplv1.CommentOnPullRequestRequest{Workspace: ref(), PullRequest: prwatch.RefProto(testPR),
			Placement: &agentreplv1.CommentOnPullRequestRequest_Conversation{Conversation: &agentreplv1.CommentOnPullRequestConversation{Markdown: " "}}}},
		{name: "a reply naming no comment", req: &agentreplv1.CommentOnPullRequestRequest{Workspace: ref(), PullRequest: prwatch.RefProto(testPR),
			Placement: &agentreplv1.CommentOnPullRequestRequest_Reply{Reply: &agentreplv1.CommentOnPullRequestReply{Markdown: "x"}}}},
		{name: "no pull request", req: &agentreplv1.CommentOnPullRequestRequest{Workspace: ref(),
			Placement: &agentreplv1.CommentOnPullRequestRequest_Conversation{Conversation: &agentreplv1.CommentOnPullRequestConversation{Markdown: "x"}}}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)

			// Act
			_, err := h.Client.CommentOnPullRequest(context.Background(), connect.NewRequest(tt.req))

			// Assert
			if connect.CodeOf(err) != connect.CodeInvalidArgument || len(h.PRs.comments) != 0 {
				t.Fatalf("CommentOnPullRequest = %v, want invalid_argument and nothing posted", err)
			}
		})
	}
}
