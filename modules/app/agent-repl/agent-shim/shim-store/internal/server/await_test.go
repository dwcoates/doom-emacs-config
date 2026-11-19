package server

import (
	"context"
	"errors"
	"strings"
	"testing"
	"time"

	"connectrpc.com/connect"

	conversationv1 "agentrepl/proto/conversation/v1"
	storev1 "agentrepl/proto/store/v1"
)

// awaitBound bounds a wait the test expects to answer. 1s: an in-process
// httptest server over a fake store answers in single-digit milliseconds even
// under -race (the harness's own shutdown basis).
const awaitBound = time.Second

func scopedAwait() *storev1.AwaitAgentByVendorTaskRequest {
	return &storev1.AwaitAgentByVendorTaskRequest{Session: &conversationv1.AgentId{Value: "main-1"}, VendorTaskId: "a1b2"}
}

func recorded(store *fakeStore) {
	store.mu.Lock()
	defer store.mu.Unlock()
	store.byTaskAgent, store.byTaskFound = "toolu_spawn", true
	store.commission = &conversationv1.AgentSubagentPrompt{Text: "review the branch"}
}

func TestAwaitAgentByVendorTaskAnswersARecordAlreadyHeld(t *testing.T) {
	// Arrange
	store := newFakeStore()
	recorded(store)
	h := newHarness(t, store, 0)
	ctx, cancel := context.WithTimeout(context.Background(), awaitBound)
	defer cancel()

	// Act
	res, err := h.client.AwaitAgentByVendorTask(ctx, connect.NewRequest(scopedAwait()))

	// Assert
	if err != nil || res.Msg.GetSuccess().GetAgent().GetValue() != "toolu_spawn" || res.Msg.GetSuccess().GetCommission().GetText() != "review the branch" {
		t.Fatalf("AwaitAgentByVendorTask = (%v, %v), want the agent and its commission", res, err)
	}
	if store.byTaskSession != "main-1" || store.byTaskID != "a1b2" {
		t.Fatalf("read (%q, %q), want the request's session and locator", store.byTaskSession, store.byTaskID)
	}
}

func TestAwaitAgentByVendorTaskAnswersOnceACommitRecordsIt(t *testing.T) {
	tests := []struct {
		name   string
		before func(*fakeStore)
	}{
		{name: "the pairing is not on record yet"},
		{name: "the pairing is on record but not the start", before: func(s *fakeStore) {
			s.mu.Lock()
			s.byTaskAgent, s.byTaskFound = "toolu_spawn", true
			s.mu.Unlock()
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			store := newFakeStore()
			if tt.before != nil {
				tt.before(store)
			}
			store.byTaskReads = make(chan struct{}, 4)
			h := newHarness(t, store, 0)
			ctx, cancel := context.WithTimeout(context.Background(), awaitBound)
			defer cancel()
			done := make(chan *connect.Response[storev1.AwaitAgentByVendorTaskResponse], 1)
			go func() {
				res, err := h.client.AwaitAgentByVendorTask(ctx, connect.NewRequest(scopedAwait()))
				if err != nil {
					t.Errorf("AwaitAgentByVendorTask: %v", err)
				}
				done <- res
			}()
			<-store.byTaskReads

			// Act: the sidecar's write lands.
			recorded(store)
			h.server.commits.fire()

			// Assert
			res := <-done
			if res.Msg.GetSuccess().GetCommission().GetText() != "review the branch" {
				t.Fatalf("result = %v, want the agent answered after the commit", res.Msg.GetResult())
			}
		})
	}
}

func TestAwaitAgentByVendorTaskEndsWithItsCaller(t *testing.T) {
	// Arrange
	store := newFakeStore()
	store.byTaskReads = make(chan struct{}, 4)
	h := newHarness(t, store, 0)
	ctx, cancel := context.WithCancel(context.Background())
	done := make(chan error, 1)
	go func() {
		_, err := h.client.AwaitAgentByVendorTask(ctx, connect.NewRequest(scopedAwait()))
		done <- err
	}()
	<-store.byTaskReads

	// Act
	cancel()

	// Assert
	if err := <-done; connect.CodeOf(err) != connect.CodeCanceled {
		t.Fatalf("AwaitAgentByVendorTask = %v, want canceled", err)
	}
}

func TestAwaitAgentByVendorTaskMapsAStorageFailureToTheStorageFailureArm(t *testing.T) {
	// Arrange
	store := newFakeStore()
	store.byTaskErr = errors.New("database is locked")
	h := newHarness(t, store, 0)

	// Act
	res, err := h.client.AwaitAgentByVendorTask(context.Background(), connect.NewRequest(scopedAwait()))

	// Assert
	if err != nil || res.Msg.GetFailure().GetStorageFailure() == nil || !strings.Contains(res.Msg.GetFailure().GetDetail(), "database is locked") {
		t.Fatalf("AwaitAgentByVendorTask = (%v, %v), want the storage_failure arm with the cause", res, err)
	}
	if !strings.Contains(h.logs.String(), "store.rpc.await-agent-by-vendor-task") {
		t.Fatalf("logs %q, want the failure recorded under the wait's operation", h.logs.String())
	}
}

func TestAwaitAgentByVendorTaskRefusesAnIncompleteRequest(t *testing.T) {
	tests := []struct {
		name    string
		request *storev1.AwaitAgentByVendorTaskRequest
		field   string
	}{
		{name: "session unset", request: &storev1.AwaitAgentByVendorTaskRequest{VendorTaskId: "a1b2"}, field: "session"},
		{name: "locator empty", request: &storev1.AwaitAgentByVendorTaskRequest{Session: &conversationv1.AgentId{Value: "main-1"}}, field: "vendor_task_id"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			store := newFakeStore()
			h := newHarness(t, store, 0)

			// Act
			res, err := h.client.AwaitAgentByVendorTask(context.Background(), connect.NewRequest(tt.request))

			// Assert
			if err != nil || res.Msg.GetFailure().GetInvalidRequest().GetField() != tt.field {
				t.Fatalf("AwaitAgentByVendorTask = (%v, %v), want invalid_request naming %s", res, err, tt.field)
			}
			if store.byTaskAsked {
				t.Fatal("an invalid wait read the record")
			}
		})
	}
}

func TestACommittedWriteBatchWakesTheWaiters(t *testing.T) {
	// Arrange
	h := newHarness(t, newFakeStore(), 0)
	taken := h.server.commits.next()

	// Act
	writeOne(t, h)

	// Assert
	select {
	case <-taken:
	default:
		t.Fatal("a committed write left the waiters asleep")
	}
}
