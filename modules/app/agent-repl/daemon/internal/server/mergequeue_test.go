package server

import (
	"context"
	"errors"
	"reflect"
	"testing"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"agent-repld/internal/ids"
	"agent-repld/internal/merge"
	"agent-repld/internal/resolve/footer"
	"agent-repld/internal/wsm"
)

// advancesDetail is the advances detail level.
func advancesDetail() *agentreplv1.SubscribeMergeQueueDetail {
	return &agentreplv1.SubscribeMergeQueueDetail{Level: &agentreplv1.SubscribeMergeQueueDetail_Advances{Advances: &agentreplv1.SubscribeMergeQueueAdvances{}}}
}

// subscribeRequest subscribes the harness workspace at DETAIL, scoped to
// REPO when it is set.
func subscribeRequest(detail *agentreplv1.SubscribeMergeQueueDetail, repo *workspacev1.RepositoryRef) *connect.Request[agentreplv1.SubscribeMergeQueueRequest] {
	return connect.NewRequest(&agentreplv1.SubscribeMergeQueueRequest{Subscriber: ref(), Repository: repo, Detail: detail})
}

// aQueue is a repository's queue with one merge running and one waiting at
// the front.
func aQueue() merge.QueueStatus {
	return merge.QueueStatus{
		Repository: wsm.Repository{ID: "repo-1", Dir: "/repo"},
		Queue: merge.QueueSnapshot{Paused: true, Members: []merge.QueueMember{
			{Workspace: "ws-merge", Name: "merge", Branch: "agent/merge", Started: true, Requester: "ws-asker", Step: footer.StepTesting},
			{Workspace: "ws-own", Name: "own", Branch: "agent/own", Front: true},
		}},
	}
}

func TestGetMergeQueueStatusRendersEachQueue(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.Merge.statuses = []merge.QueueStatus{aQueue()}

	// Act
	resp, err := h.Client.GetMergeQueueStatus(context.Background(), connect.NewRequest(&agentreplv1.GetMergeQueueStatusRequest{}))

	// Assert
	if err != nil {
		t.Fatalf("GetMergeQueueStatus: %v", err)
	}
	queues := resp.Msg.GetSuccess().GetQueues()
	if len(queues) != 1 || queues[0].GetRepository().GetId() != "repo-1" || queues[0].GetAdvance().GetPaused() == nil {
		t.Fatalf("queues = %v, want repo-1's paused queue", queues)
	}
	entries := queues[0].GetEntries()
	if len(entries) != 2 {
		t.Fatalf("entries = %v, want two", entries)
	}
	running, waiting := entries[0], entries[1]
	if running.GetKind().GetAsyncMerge().GetRequester().GetId() != "ws-asker" || running.GetStage().GetRunning().GetStepDescription() != footer.StepTesting.Words() {
		t.Errorf("running entry = %v, want an async merge of ws-asker at its testing step", running)
	}
	if waiting.GetKind().GetOwnBranch() == nil || waiting.GetStage().GetWaiting() == nil || !waiting.GetPlacedAtFront() {
		t.Errorf("waiting entry = %v, want an own-branch merge waiting at the front", waiting)
	}
	if len(h.Merge.statusScopes) != 1 || h.Merge.statusScopes[0] != nil {
		t.Errorf("scopes = %v, want one unscoped read", h.Merge.statusScopes)
	}
}

func TestGetMergeQueueStatusListsAMergeRemediatingAfterTheLine(t *testing.T) {
	// Arrange
	h := newHarness(t)
	queue := aQueue()
	queue.Queue.Remediating = []merge.QueueMember{{Workspace: "ws-fixing", Name: "fixing", Branch: "agent/fixing", Started: true}}
	h.Merge.statuses = []merge.QueueStatus{queue}

	// Act
	resp, err := h.Client.GetMergeQueueStatus(context.Background(), connect.NewRequest(&agentreplv1.GetMergeQueueStatusRequest{}))

	// Assert
	if err != nil {
		t.Fatalf("GetMergeQueueStatus: %v", err)
	}
	entries := resp.Msg.GetSuccess().GetQueues()[0].GetEntries()
	if len(entries) != 3 || entries[2].GetName() != "fixing" || entries[2].GetStage().GetRemediating() == nil {
		t.Fatalf("entries = %v, want the remediating merge last, at the remediating stage", entries)
	}
}

func TestGetMergeQueueStatusAnswersAnUnknownRepository(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.Merge.statusErr = &merge.RefusalError{Arm: merge.ArmUnknownRepository, Reason: "no such repository"}

	// Act
	resp, err := h.Client.GetMergeQueueStatus(context.Background(), connect.NewRequest(&agentreplv1.GetMergeQueueStatusRequest{
		Repository: &workspacev1.RepositoryRef{Id: "repo-9"}}))

	// Assert
	if err != nil || resp.Msg.GetError().GetUnknownRepository() == nil {
		t.Fatalf("GetMergeQueueStatus = %v, %v; want unknown_repository", resp, err)
	}
}

func TestSubscribeMergeQueueReachesTheOrchestratorAtEachDetail(t *testing.T) {
	tests := []struct {
		name   string
		detail *agentreplv1.SubscribeMergeQueueDetail
		want   wsm.MergeQueueDetail
	}{
		{name: "every change", want: wsm.MergeQueueDetailEveryChange, detail: &agentreplv1.SubscribeMergeQueueDetail{
			Level: &agentreplv1.SubscribeMergeQueueDetail_EveryChange{EveryChange: &agentreplv1.SubscribeMergeQueueEveryChange{}}}},
		{name: "advances", want: wsm.MergeQueueDetailAdvances, detail: advancesDetail()},
		{name: "depletes", want: wsm.MergeQueueDetailDepletes, detail: &agentreplv1.SubscribeMergeQueueDetail{
			Level: &agentreplv1.SubscribeMergeQueueDetail_Depletes{Depletes: &agentreplv1.SubscribeMergeQueueDepletes{}}}},
		{name: "deploys", want: wsm.MergeQueueDetailDeploys, detail: &agentreplv1.SubscribeMergeQueueDetail{
			Level: &agentreplv1.SubscribeMergeQueueDetail_Deploys{Deploys: &agentreplv1.SubscribeMergeQueueDeploys{}}}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			h.Merge.statuses = []merge.QueueStatus{aQueue()}

			// Act
			resp, err := h.Client.SubscribeMergeQueue(context.Background(), subscribeRequest(tt.detail, nil))

			// Assert
			if err != nil || resp.Msg.GetSuccess().GetTaken().GetSubscribed() == nil {
				t.Fatalf("SubscribeMergeQueue = %v, %v; want subscribed", resp, err)
			}
			if len(resp.Msg.GetSuccess().GetQueues()) != 1 {
				t.Errorf("queues = %v, want the queues as they stand", resp.Msg.GetSuccess().GetQueues())
			}
			want := []fakeSubscription{{ws: ids.WorkspaceID(ref().GetId()), detail: tt.want}}
			if !reflect.DeepEqual(h.Merge.subscriptions, want) {
				t.Fatalf("subscriptions = %+v, want %+v", h.Merge.subscriptions, want)
			}
		})
	}
}

func TestSubscribeMergeQueueAgainIsResubscribed(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.Merge.replaced = true

	// Act
	resp, err := h.Client.SubscribeMergeQueue(context.Background(), subscribeRequest(advancesDetail(), nil))

	// Assert
	if err != nil || resp.Msg.GetSuccess().GetTaken().GetResubscribed() == nil {
		t.Fatalf("SubscribeMergeQueue = %v, %v; want resubscribed", resp, err)
	}
}

func TestSubscribeMergeQueueCarriesItsRepositoryScope(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	_, err := h.Client.SubscribeMergeQueue(context.Background(), subscribeRequest(advancesDetail(), &workspacev1.RepositoryRef{Id: "repo-1"}))

	// Assert
	if err != nil {
		t.Fatalf("SubscribeMergeQueue: %v", err)
	}
	if len(h.Merge.subscriptions) != 1 || h.Merge.subscriptions[0].scope == nil || h.Merge.subscriptions[0].scope.ID != "repo-1" {
		t.Fatalf("subscriptions = %+v, want one scoped to repo-1", h.Merge.subscriptions)
	}
	if len(h.Merge.statusScopes) != 1 || h.Merge.statusScopes[0].ID != "repo-1" {
		t.Fatalf("status scopes = %v, want the subscription's scope", h.Merge.statusScopes)
	}
}

func TestSubscribeMergeQueueRefuses(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(h *harness)
		got     func(*agentreplv1.SubscribeMergeQueueError) bool
	}{
		{name: "a closed subscriber", arrange: func(h *harness) {
			h.Merge.subscribeErr = &merge.RefusalError{Arm: merge.ArmSubscriberClosed, Reason: "closed"}
		}, got: func(e *agentreplv1.SubscribeMergeQueueError) bool { return e.GetSubscriberClosed() != nil }},
		{name: "an unknown repository", arrange: func(h *harness) {
			h.Merge.subscribeErr = &merge.RefusalError{Arm: merge.ArmUnknownRepository, Reason: "none"}
		}, got: func(e *agentreplv1.SubscribeMergeQueueError) bool { return e.GetUnknownRepository() != nil }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			tt.arrange(h)

			// Act
			resp, err := h.Client.SubscribeMergeQueue(context.Background(), subscribeRequest(advancesDetail(), nil))

			// Assert
			if err != nil || !tt.got(resp.Msg.GetError()) {
				t.Fatalf("SubscribeMergeQueue = %v, %v; want the refusal", resp, err)
			}
		})
	}
}

func TestSubscribeMergeQueueOfAnUnknownSubscriberIsAnAnswer(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	resp, err := h.Client.SubscribeMergeQueue(context.Background(), connect.NewRequest(&agentreplv1.SubscribeMergeQueueRequest{
		Subscriber: &workspacev1.WorkspaceRef{Id: "ws-none"}, Detail: advancesDetail()}))

	// Assert
	if err != nil || resp.Msg.GetError().GetUnknownSubscriber() == nil {
		t.Fatalf("SubscribeMergeQueue = %v, %v; want unknown_subscriber", resp, err)
	}
}

func TestAMergeQueueSubscriptionMissingAFieldIsInvalid(t *testing.T) {
	tests := []struct {
		name string
		call func(h *harness) error
	}{
		{name: "a subscribe with no detail", call: func(h *harness) error {
			_, err := h.Client.SubscribeMergeQueue(context.Background(), subscribeRequest(&agentreplv1.SubscribeMergeQueueDetail{}, nil))
			return err
		}},
		{name: "a subscribe with no subscriber", call: func(h *harness) error {
			_, err := h.Client.SubscribeMergeQueue(context.Background(), connect.NewRequest(&agentreplv1.SubscribeMergeQueueRequest{Detail: advancesDetail()}))
			return err
		}},
		{name: "an unsubscribe with no subscriber", call: func(h *harness) error {
			_, err := h.Client.UnsubscribeMergeQueue(context.Background(), connect.NewRequest(&agentreplv1.UnsubscribeMergeQueueRequest{}))
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
			if connect.CodeOf(err) != connect.CodeInvalidArgument {
				t.Fatalf("err = %v, want InvalidArgument", err)
			}
			if len(h.Merge.subscriptions) != 0 || len(h.Merge.unsubscriptions) != 0 {
				t.Fatalf("subscriptions %+v, unsubscriptions %+v; want none", h.Merge.subscriptions, h.Merge.unsubscriptions)
			}
		})
	}
}

func TestUnsubscribeMergeQueueAnswersWhetherOneStood(t *testing.T) {
	tests := []struct {
		name    string
		err     error
		unsubbd bool
	}{
		{name: "a subscription ended", unsubbd: true},
		{name: "none held", err: &merge.RefusalError{Arm: merge.ArmNotSubscribed, Reason: "none"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			h.Merge.unsubscribeErr = tt.err

			// Act
			resp, err := h.Client.UnsubscribeMergeQueue(context.Background(), connect.NewRequest(&agentreplv1.UnsubscribeMergeQueueRequest{Subscriber: ref()}))

			// Assert
			if err != nil {
				t.Fatalf("UnsubscribeMergeQueue: %v", err)
			}
			success := resp.Msg.GetSuccess()
			if (success.GetUnsubscribed() != nil) != tt.unsubbd || (success.GetNotSubscribed() != nil) == tt.unsubbd {
				t.Fatalf("success = %v, want unsubscribed %v", success, tt.unsubbd)
			}
			want := []fakeSubscription{{ws: ids.WorkspaceID(ref().GetId())}}
			if !reflect.DeepEqual(h.Merge.unsubscriptions, want) {
				t.Fatalf("unsubscriptions = %+v, want %+v", h.Merge.unsubscriptions, want)
			}
		})
	}
}

func TestUnsubscribeMergeQueueFailureIsATransportError(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.Merge.unsubscribeErr = errors.New("db gone")

	// Act
	_, err := h.Client.UnsubscribeMergeQueue(context.Background(), connect.NewRequest(&agentreplv1.UnsubscribeMergeQueueRequest{Subscriber: ref()}))

	// Assert
	if err == nil {
		t.Fatal("UnsubscribeMergeQueue answered a failed unsubscribe without an error")
	}
}
