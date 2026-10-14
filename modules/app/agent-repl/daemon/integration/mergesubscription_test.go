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
func subscribeDepletes(t *testing.T, d *harness.Daemon, ws *workspacev1.WorkspaceRef) *agentreplv1.UpdateMergeQueueResponse {
	t.Helper()
	resp, err := d.Client().UpdateMergeQueue(d.Ctx(), connect.NewRequest(&agentreplv1.UpdateMergeQueueRequest{
		Action: &agentreplv1.UpdateMergeQueueRequest_Subscribe{Subscribe: &agentreplv1.UpdateMergeQueueSubscribe{
			Subscriber: ws,
			Detail: &agentreplv1.UpdateMergeQueueSubscribeDetail{
				Level: &agentreplv1.UpdateMergeQueueSubscribeDetail_Depletes{Depletes: &agentreplv1.UpdateMergeQueueSubscribeDepletes{}},
			},
		}},
	}))
	if err != nil {
		t.Fatalf("UpdateMergeQueue(subscribe) = error %v", err)
	}
	return resp.Msg
}

// unsubscribeEvery ends WS's every-repository subscription.
func unsubscribeEvery(t *testing.T, d *harness.Daemon, ws *workspacev1.WorkspaceRef) *agentreplv1.UpdateMergeQueueResponse {
	t.Helper()
	resp, err := d.Client().UpdateMergeQueue(d.Ctx(), connect.NewRequest(&agentreplv1.UpdateMergeQueueRequest{
		Action: &agentreplv1.UpdateMergeQueueRequest_Unsubscribe{Unsubscribe: &agentreplv1.UpdateMergeQueueUnsubscribe{Subscriber: ws}},
	}))
	if err != nil {
		t.Fatalf("UpdateMergeQueue(unsubscribe) = error %v", err)
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
	if first.GetSuccess() == nil {
		t.Fatalf("unsubscribe after the restart = %v, want success: the subscription survived", first)
	}
	if second.GetError().GetNotSubscribed() == nil {
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
	if _, err := d.Client().CloseWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.CloseWorkspaceRequest{Workspace: ws})); err != nil {
		t.Fatalf("CloseWorkspace: %v", err)
	}

	// Assert.
	if got := unsubscribeEvery(t, d, ws); got.GetError().GetNotSubscribed() == nil {
		t.Fatalf("unsubscribe after the close = %v, want not_subscribed: the close ended it", got)
	}
	if got := subscribeDepletes(t, d, ws); got.GetError().GetSubscriberClosed() == nil {
		t.Fatalf("subscribe while closed = %v, want subscriber_closed", got)
	}
}
