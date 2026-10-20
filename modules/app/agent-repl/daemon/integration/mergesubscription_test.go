//go:build integration

package integration

import (
	"testing"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"agent-repld/integration/harness"
)

// subscribeDepletes subscribes WS's agent to every repository's merge queue,
// told only when a queue runs out of merges.
func subscribeDepletes(t *testing.T, d *harness.Daemon, ws *workspacev1.WorkspaceRef) *agentreplv1.SubscribeMergeQueueResponse {
	t.Helper()
	resp, err := d.Client().SubscribeMergeQueue(d.Ctx(), connect.NewRequest(&agentreplv1.SubscribeMergeQueueRequest{
		Subscriber: ws,
		Detail: &agentreplv1.SubscribeMergeQueueDetail{
			Level: &agentreplv1.SubscribeMergeQueueDetail_Depletes{Depletes: &agentreplv1.SubscribeMergeQueueDepletes{}},
		},
	}))
	if err != nil {
		t.Fatalf("SubscribeMergeQueue = error %v", err)
	}
	return resp.Msg
}

// unsubscribeEvery ends WS's every-repository subscription.
func unsubscribeEvery(t *testing.T, d *harness.Daemon, ws *workspacev1.WorkspaceRef) *agentreplv1.UnsubscribeMergeQueueResponse {
	t.Helper()
	resp, err := d.Client().UnsubscribeMergeQueue(d.Ctx(), connect.NewRequest(&agentreplv1.UnsubscribeMergeQueueRequest{Subscriber: ws}))
	if err != nil {
		t.Fatalf("UnsubscribeMergeQueue = error %v", err)
	}
	return resp.Msg
}

// A SUBSCRIPTION IS DURABLE: a landing's own deploy restarts the daemon, so a
// subscription a restart dropped would end at the change it exists to report.
func TestAMergeQueueSubscriptionSurvivesADaemonRestart(t *testing.T) {
	t.Parallel()
	// Arrange: a subscription taken on one daemon, which then stops.
	d := newDaemon(t, harness.Opts{DurableState: true})
	repo := harness.NewRepo(t)
	ws := harness.Register(t, d, worktreeOf(t, repo, "subscriber"))
	if got := subscribeDepletes(t, d, ws); got.GetSuccess() == nil {
		t.Fatalf("subscribe = %v, want success", got)
	}
	d.Stop()

	// Act: the next daemon on the same state ends it.
	nd := harness.StartDaemon(t, harness.Opts{StateDir: d.StateDir})
	first := unsubscribeEvery(t, nd, ws)
	second := unsubscribeEvery(t, nd, ws)

	// Assert: the subscription was still standing, and only once.
	if first.GetSuccess().GetUnsubscribed() == nil {
		t.Fatalf("unsubscribe after the restart = %v, want unsubscribed: the subscription survived", first)
	}
	if second.GetSuccess().GetNotSubscribed() == nil {
		t.Fatalf("second unsubscribe = %v, want not_subscribed", second)
	}
}

// A SUBSCRIPTION ENDS WITH ITS WORKSPACE'S CLOSE, and a closed workspace
// cannot take one.
func TestClosingAWorkspaceEndsItsMergeQueueSubscription(t *testing.T) {
	t.Parallel()
	// Arrange.
	d := newDaemon(t, harness.Opts{})
	repo := harness.NewRepo(t)
	ws := harness.Register(t, d, worktreeOf(t, repo, "subscriber"))
	if got := subscribeDepletes(t, d, ws); got.GetSuccess() == nil {
		t.Fatalf("subscribe = %v, want success", got)
	}

	// Act.
	if _, err := d.Client().CloseWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.CloseWorkspaceRequest{Requester: harness.User(), Workspace: ws})); err != nil {
		t.Fatalf("CloseWorkspace: %v", err)
	}

	// Assert.
	if got := unsubscribeEvery(t, d, ws); got.GetSuccess().GetNotSubscribed() == nil {
		t.Fatalf("unsubscribe after the close = %v, want not_subscribed: the close ended it", got)
	}
	if got := subscribeDepletes(t, d, ws); got.GetError().GetSubscriberClosed() == nil {
		t.Fatalf("subscribe while closed = %v, want subscriber_closed", got)
	}
}
