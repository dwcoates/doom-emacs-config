package server

import (
	"context"
	"reflect"
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

func (d *fakeDB) SubscribeGithubMergeQueue(_ context.Context, sub wsm.GithubMergeQueueSubscription) (bool, error) {
	if d.queueSubscribeErr != nil {
		return false, d.queueSubscribeErr
	}
	for _, held := range d.queueSubs {
		if held.Queue == sub.Queue {
			return true, nil
		}
	}
	d.queueSubs = append(d.queueSubs, sub)
	return false, nil
}

func (d *fakeDB) UnsubscribeGithubMergeQueue(_ context.Context, _ ids.WorkspaceID, key wsm.GithubMergeQueueKey) (bool, error) {
	for i, sub := range d.queueSubs {
		if sub.Queue == key {
			d.queueSubs = append(d.queueSubs[:i], d.queueSubs[i+1:]...)
			return true, nil
		}
	}
	return false, nil
}

func (d *fakeDB) WorkspaceGithubMergeQueueSubscriptions(context.Context, ids.WorkspaceID) ([]wsm.GithubMergeQueueSubscription, error) {
	return d.queueSubs, nil
}

var testQueue = wsm.GithubMergeQueueKey{Owner: "ChessCom", Repo: "explanation-engine", Branch: "main"}

func subscribeNamedQueue(branch *string) *connect.Request[agentreplv1.SubscribeGithubMergeQueueRequest] {
	return connect.NewRequest(&agentreplv1.SubscribeGithubMergeQueueRequest{Subscriber: ref(), Branch: branch,
		Repository: &agentreplv1.SubscribeGithubMergeQueueRequest_Named{Named: &agentreplv1.GithubRepositoryRef{Owner: "ChessCom", Repo: "explanation-engine"}}})
}

func TestSubscribeGithubMergeQueueRecordsTheAnchoredSubscription(t *testing.T) {
	tests := []struct {
		name       string
		req        *connect.Request[agentreplv1.SubscribeGithubMergeQueueRequest]
		origin     string
		held       bool
		wantAnchor string
		wantTaken  string
	}{
		{name: "a named repository's default branch", req: subscribeNamedQueue(nil), wantAnchor: "ChessCom/explanation-engine@", wantTaken: "subscribed"},
		{name: "a named branch", req: subscribeNamedQueue(strPtr("main")), wantAnchor: "ChessCom/explanation-engine@main", wantTaken: "subscribed"},
		{name: "the subscriber's own repository", req: connect.NewRequest(&agentreplv1.SubscribeGithubMergeQueueRequest{Subscriber: ref(),
			Repository: &agentreplv1.SubscribeGithubMergeQueueRequest_Subscribers{Subscribers: &agentreplv1.SubscribeGithubMergeQueueSubscribersRepository{}}}),
			origin: "git@github.com:ChessCom/explanation-engine.git", wantAnchor: "ChessCom/explanation-engine@", wantTaken: "subscribed"},
		{name: "a queue already followed", req: subscribeNamedQueue(nil), held: true, wantAnchor: "ChessCom/explanation-engine@", wantTaken: "already_subscribed"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			h.Remotes.url = tt.origin
			if tt.held {
				h.DB.queueSubs = []wsm.GithubMergeQueueSubscription{{Subscriber: testWorkspaceID, Queue: testQueue, Told: "earlier"}}
			}

			// Act
			resp, err := h.Client.SubscribeGithubMergeQueue(context.Background(), tt.req)

			// Assert
			if err != nil {
				t.Fatalf("SubscribeGithubMergeQueue: %v", err)
			}
			success := resp.Msg.GetSuccess()
			taken, sub := "subscribed", success.GetSubscribed().GetSubscription()
			if a := success.GetAlreadySubscribed(); a != nil {
				taken, sub = "already_subscribed", a.GetSubscription()
			}
			if taken != tt.wantTaken || prwatch.QueueKeyOf(success.GetMergeQueue()) != testQueue || success.GetStatus().GetUrl() != "queue-url" {
				t.Fatalf("success = %v, want %s of the main queue", success, tt.wantTaken)
			}
			if sub.GetSubscriber().GetDir() != testWorkspaceDir || prwatch.QueueKeyOf(sub.GetMergeQueue()) != testQueue {
				t.Fatalf("subscription = %v, want the main queue naming its subscriber's directory", sub)
			}
			if !reflect.DeepEqual(h.PRs.anchoredQueues, []string{tt.wantAnchor}) || len(h.DB.queueSubs) != 1 {
				t.Fatalf("anchored %v with %d recorded; want %s once", h.PRs.anchoredQueues, len(h.DB.queueSubs), tt.wantAnchor)
			}
		})
	}
}

func TestSubscribeGithubMergeQueueIsRefused(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(h *harness) *connect.Request[agentreplv1.SubscribeGithubMergeQueueRequest]
		got     func(*agentreplv1.SubscribeGithubMergeQueueError) bool
	}{
		{name: "a repository not on github.com", arrange: func(h *harness) *connect.Request[agentreplv1.SubscribeGithubMergeQueueRequest] {
			h.Remotes.url = "git@gitlab.com:o/r.git"
			return connect.NewRequest(&agentreplv1.SubscribeGithubMergeQueueRequest{Subscriber: ref(),
				Repository: &agentreplv1.SubscribeGithubMergeQueueRequest_Subscribers{Subscribers: &agentreplv1.SubscribeGithubMergeQueueSubscribersRepository{}}})
		}, got: func(e *agentreplv1.SubscribeGithubMergeQueueError) bool { return e.GetNotOnGithub().GetOrigin() == "git@gitlab.com:o/r.git" }},
		{name: "a branch with no merge queue", arrange: func(h *harness) *connect.Request[agentreplv1.SubscribeGithubMergeQueueRequest] {
			h.PRs.queueAnchorErr = prwatch.ErrNoMergeQueue
			return subscribeNamedQueue(nil)
		}, got: func(e *agentreplv1.SubscribeGithubMergeQueueError) bool {
			return prwatch.QueueKeyOf(e.GetNoMergeQueue().GetMergeQueue()) == testQueue
		}},
		{name: "a queue GitHub will not show", arrange: func(h *harness) *connect.Request[agentreplv1.SubscribeGithubMergeQueueRequest] {
			h.PRs.queueAnchorErr = &ghclient.Failure{Kind: ghclient.KindUnauthorized, Status: 401}
			return subscribeNamedQueue(nil)
		}, got: func(e *agentreplv1.SubscribeGithubMergeQueueError) bool { return e.GetUnreadable().GetCause().GetUnauthorized() != nil }},
		{name: "a subscriber that closed meanwhile", arrange: func(h *harness) *connect.Request[agentreplv1.SubscribeGithubMergeQueueRequest] {
			h.DB.queueSubscribeErr = wsm.ErrSubscriberClosed
			return subscribeNamedQueue(nil)
		}, got: func(e *agentreplv1.SubscribeGithubMergeQueueError) bool { return e.GetSubscriberClosed() != nil }},
		{name: "a subscriber forgotten meanwhile", arrange: func(h *harness) *connect.Request[agentreplv1.SubscribeGithubMergeQueueRequest] {
			h.DB.queueSubscribeErr = wsm.ErrNotFound
			return subscribeNamedQueue(nil)
		}, got: func(e *agentreplv1.SubscribeGithubMergeQueueError) bool { return e.GetUnknownSubscriber() != nil }},
		{name: "an unknown subscriber", arrange: func(*harness) *connect.Request[agentreplv1.SubscribeGithubMergeQueueRequest] {
			req := subscribeNamedQueue(nil)
			req.Msg.Subscriber = &workspacev1.WorkspaceRef{Id: "ws-none"}
			return req
		}, got: func(e *agentreplv1.SubscribeGithubMergeQueueError) bool { return e.GetUnknownSubscriber() != nil }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			req := tt.arrange(h)

			// Act
			resp, err := h.Client.SubscribeGithubMergeQueue(context.Background(), req)

			// Assert
			if err != nil || !tt.got(resp.Msg.GetError()) || len(h.DB.queueSubs) != 0 {
				t.Fatalf("SubscribeGithubMergeQueue = %v, %v with %d recorded; want the refusal and nothing recorded", resp, err, len(h.DB.queueSubs))
			}
		})
	}
}

func TestSubscribeGithubMergeQueueRecordsItsRefusal(t *testing.T) {
	// Arrange
	h := newHarness(t)
	log := dlog.NewTestLogger()
	h.Surfaces.workspace = log
	h.PRs.queueAnchorErr = prwatch.ErrNoMergeQueue

	// Act
	_, err := h.Client.SubscribeGithubMergeQueue(context.Background(), subscribeNamedQueue(nil))

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	record, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: "info", Operation: opGithubMergeQueues, Message: "refused a GitHub merge-queue subscription"})
	if !ok || record.Context["reason"] != "GitHub runs no merge queue for ChessCom/explanation-engine@main" {
		t.Fatalf("no refusal record naming the queue: %+v", log.Records())
	}
}

func TestSubscribeGithubMergeQueueFailsWhenTheOriginCannotBeRead(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	_, err := h.Client.SubscribeGithubMergeQueue(context.Background(), connect.NewRequest(&agentreplv1.SubscribeGithubMergeQueueRequest{Subscriber: ref(),
		Repository: &agentreplv1.SubscribeGithubMergeQueueRequest_Subscribers{Subscribers: &agentreplv1.SubscribeGithubMergeQueueSubscribersRepository{}}}))

	// Assert
	if connect.CodeOf(err) != connect.CodeInternal || len(h.PRs.anchoredQueues) != 0 {
		t.Fatalf("SubscribeGithubMergeQueue = %v with anchors %v; want an internal failure and no read", err, h.PRs.anchoredQueues)
	}
}

func TestSubscribeGithubMergeQueueValidation(t *testing.T) {
	tests := []struct {
		name string
		req  *agentreplv1.SubscribeGithubMergeQueueRequest
	}{
		{name: "no repository", req: &agentreplv1.SubscribeGithubMergeQueueRequest{Subscriber: ref()}},
		{name: "a repository missing its owner", req: &agentreplv1.SubscribeGithubMergeQueueRequest{Subscriber: ref(),
			Repository: &agentreplv1.SubscribeGithubMergeQueueRequest_Named{Named: &agentreplv1.GithubRepositoryRef{Repo: "r"}}}},
		{name: "an empty branch", req: subscribeNamedQueue(strPtr(" ")).Msg},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)

			// Act
			_, err := h.Client.SubscribeGithubMergeQueue(context.Background(), connect.NewRequest(tt.req))

			// Assert
			if connect.CodeOf(err) != connect.CodeInvalidArgument || len(h.PRs.anchoredQueues) != 0 {
				t.Fatalf("SubscribeGithubMergeQueue = %v; want InvalidArgument and no read", err)
			}
		})
	}
}

func TestUnsubscribeGithubMergeQueueAnswersWhetherOneStood(t *testing.T) {
	for _, held := range []bool{true, false} {
		t.Run(map[bool]string{true: "a subscription ended", false: "none held"}[held], func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			if held {
				h.DB.queueSubs = []wsm.GithubMergeQueueSubscription{{Subscriber: testWorkspaceID, Queue: testQueue}}
			}

			// Act
			resp, err := h.Client.UnsubscribeGithubMergeQueue(context.Background(), connect.NewRequest(&agentreplv1.UnsubscribeGithubMergeQueueRequest{
				Subscriber: ref(), MergeQueue: prwatch.QueueRefProto(testQueue)}))

			// Assert
			if err != nil || (resp.Msg.GetSuccess().GetUnsubscribed() != nil) != held || (resp.Msg.GetSuccess().GetNotSubscribed() != nil) == held {
				t.Fatalf("UnsubscribeGithubMergeQueue = %v, %v; want ended %v", resp, err, held)
			}
		})
	}
}

func TestUnsubscribeGithubMergeQueueValidation(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	_, err := h.Client.UnsubscribeGithubMergeQueue(context.Background(), connect.NewRequest(&agentreplv1.UnsubscribeGithubMergeQueueRequest{
		Subscriber: ref(), MergeQueue: &agentreplv1.GithubMergeQueueRef{Repository: &agentreplv1.GithubRepositoryRef{Owner: "o", Repo: "r"}}}))

	// Assert
	if connect.CodeOf(err) != connect.CodeInvalidArgument {
		t.Fatalf("UnsubscribeGithubMergeQueue = %v; want InvalidArgument for a queue naming no branch", err)
	}
}

func TestListGithubMergeQueueSubscriptionsAnswersEveryOne(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.DB.queueSubs = []wsm.GithubMergeQueueSubscription{{Subscriber: testWorkspaceID, Queue: testQueue}}

	// Act
	resp, err := h.Client.ListGithubMergeQueueSubscriptions(context.Background(), connect.NewRequest(&agentreplv1.ListGithubMergeQueueSubscriptionsRequest{Subscriber: ref()}))

	// Assert
	subs := resp.Msg.GetSuccess().GetSubscriptions()
	if err != nil || len(subs) != 1 || prwatch.QueueKeyOf(subs[0].GetMergeQueue()) != testQueue || subs[0].GetSubscriber().GetDir() != testWorkspaceDir {
		t.Fatalf("ListGithubMergeQueueSubscriptions = %v, %v; want the one held", resp, err)
	}
}
