package server

import (
	"context"
	"testing"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/merge"
	"agent-repld/internal/workspace"
	"agent-repld/internal/wsm"
)

// requeueRequest is the user's requeue of the harness workspace's merge at
// the back.
func requeueRequest() *agentreplv1.RequeueMergeRequest {
	return &agentreplv1.RequeueMergeRequest{Workspace: ref(), Requester: userRequester(),
		Placement: &agentreplv1.MergeQueuePlacement{Place: &agentreplv1.MergeQueuePlacement_Back{Back: &agentreplv1.MergeQueuePlacementBack{}}}}
}

func TestRequeueMergeAnswersThePlaceInLine(t *testing.T) {
	cases := []struct {
		name     string
		requeued merge.Requeued
		want     *uint32
	}{
		{name: "in line, its place is answered", requeued: merge.Requeued{Position: 3, InLine: true}, want: ptr(uint32(3))},
		{name: "the daemon draining, no place is answered", requeued: merge.Requeued{}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			h.Merge.requeued = tc.requeued

			// Act.
			resp, err := h.Client.RequeueMerge(context.Background(), connect.NewRequest(requeueRequest()))

			// Assert.
			if err != nil || resp.Msg.GetSuccess() == nil {
				t.Fatalf("RequeueMerge = %v, %v; want success", resp, err)
			}
			got := resp.Msg.GetSuccess().Position
			if (got == nil) != (tc.want == nil) || (got != nil && *got != *tc.want) {
				t.Fatalf("position = %v, want %v", got, tc.want)
			}
			if len(h.Merge.requeues) != 1 || h.Merge.requeues[0].By != merge.RequestedByUser || h.Merge.requeues[0].Front {
				t.Fatalf("requeues = %+v, want the user's one requeue at the back", h.Merge.requeues)
			}
		})
	}
}

func ptr[T any](v T) *T { return &v }

func TestRequeueMergeChecksTheModelAgainstTheCatalogFirst(t *testing.T) {
	cases := []struct {
		name        string
		refused     bool
		wantRequeue bool
	}{
		{name: "a model in the catalog is requeued on", wantRequeue: true},
		{name: "a model out of it is refused and nothing requeued", refused: true},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			if tc.refused {
				h.Verbs.catalogErr = &workspace.Refusal{Arm: workspace.ArmNotInCatalog, Reason: "not offered"}
			}
			req := requeueRequest()
			req.Model = &conversationv1.AgentModel{Name: "claude-opus-5-5"}

			// Act.
			resp, err := h.Client.RequeueMerge(context.Background(), connect.NewRequest(req))

			// Assert.
			if err != nil {
				t.Fatalf("RequeueMerge: %v", err)
			}
			if len(h.Verbs.catalogChecked) != 1 || h.Verbs.catalogChecked[0] != "claude-opus-5-5" {
				t.Fatalf("catalog checks = %v, want the requeue's model", h.Verbs.catalogChecked)
			}
			if tc.refused != (resp.Msg.GetError().GetNotInCatalog() != nil) {
				t.Fatalf("result = %v, want refused %v", resp.Msg.GetResult(), tc.refused)
			}
			if (len(h.Merge.requeues) == 1) != tc.wantRequeue || (tc.wantRequeue && h.Merge.requeues[0].Model != "claude-opus-5-5") {
				t.Fatalf("requeues = %+v, want requeued %v on the model", h.Merge.requeues, tc.wantRequeue)
			}
		})
	}
}

func TestRequeueMergeAnswersEachRefusalAsItsArm(t *testing.T) {
	cases := []struct {
		arm  string
		want func(*agentreplv1.RequeueMergeError) bool
	}{
		{arm: merge.ArmUnknownMerge, want: func(e *agentreplv1.RequeueMergeError) bool { return e.GetUnknownMerge() != nil }},
		{arm: merge.ArmStillQueued, want: func(e *agentreplv1.RequeueMergeError) bool { return e.GetStillQueued() != nil }},
		{arm: merge.ArmBranchGone, want: func(e *agentreplv1.RequeueMergeError) bool { return e.GetBranchGone().GetBranch() == "feature" }},
		{arm: merge.ArmAlreadyLanded, want: func(e *agentreplv1.RequeueMergeError) bool { return e.GetAlreadyLanded() != nil }},
		{arm: merge.ArmVersionAlreadyRecorded, want: func(e *agentreplv1.RequeueMergeError) bool { return e.GetVersionAlreadyRecorded() != nil }},
		{arm: merge.ArmWorkspaceClosed, want: func(e *agentreplv1.RequeueMergeError) bool { return e.GetWorkspaceClosed() != nil }},
		{arm: merge.ArmVersionIncrementRequired, want: func(e *agentreplv1.RequeueMergeError) bool { return e.GetVersionIncrementRequired() != nil }},
		{arm: merge.ArmPlacementBehindNotInLine, want: func(e *agentreplv1.RequeueMergeError) bool { return e.GetPlacementBehindNotInLine() != nil }},
	}
	for _, tc := range cases {
		t.Run(tc.arm, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			h.Merge.requeueErr = &merge.RefusalError{Arm: tc.arm, Workspace: testWorkspaceID, Reason: "refused", Fields: map[string]any{"branch": "feature"}}

			// Act.
			resp, err := h.Client.RequeueMerge(context.Background(), connect.NewRequest(requeueRequest()))

			// Assert.
			if err != nil || !tc.want(resp.Msg.GetError()) {
				t.Fatalf("RequeueMerge = %v, %v; want the %s arm", resp.Msg.GetResult(), err, tc.arm)
			}
		})
	}
}

func TestRequeueMergeRefusesAMissingPlacement(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	req := requeueRequest()
	req.Placement = nil

	// Act.
	_, err := h.Client.RequeueMerge(context.Background(), connect.NewRequest(req))

	// Assert.
	if got := connectCode(t, err); got != connect.CodeInvalidArgument {
		t.Fatalf("code = %v, want InvalidArgument", got)
	}
}

func TestDiscardMergeClosesAnAsyncMergesWorkspaceAsTheAskerDoes(t *testing.T) {
	cases := []struct {
		name      string
		requester *agentreplv1.WorkspaceRequester
		discarded merge.Discarded
		wantClose []wsm.CloseCause
	}{
		{name: "a workspace's own merge closes nothing", requester: userRequester()},
		{name: "the user's async discard closes as the user", requester: userRequester(), discarded: merge.Discarded{Async: true, CleanupWorktreeRemoved: true}, wantClose: []wsm.CloseCause{wsm.CloseByUser}},
		{name: "an agent's async discard closes as the agent", requester: agent(), discarded: merge.Discarded{Async: true}, wantClose: []wsm.CloseCause{wsm.CloseByAgent}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			h.Merge.discarded = tc.discarded

			// Act.
			resp, err := h.Client.DiscardMerge(context.Background(), connect.NewRequest(&agentreplv1.DiscardMergeRequest{Workspace: ref(), Requester: tc.requester}))

			// Assert.
			if err != nil || resp.Msg.GetSuccess() == nil {
				t.Fatalf("DiscardMerge = %v, %v; want success", resp, err)
			}
			if got := resp.Msg.GetSuccess().GetAsyncMerge(); (got != nil) != tc.discarded.Async || got.GetCleanupWorktreeRemoved() != tc.discarded.CleanupWorktreeRemoved {
				t.Fatalf("success = %v, want async %v removed %v", resp.Msg.GetSuccess(), tc.discarded.Async, tc.discarded.CleanupWorktreeRemoved)
			}
			if len(h.Verbs.closedBy) != len(tc.wantClose) || (len(tc.wantClose) == 1 && h.Verbs.closedBy[0] != tc.wantClose[0]) {
				t.Fatalf("closes = %v, want %v", h.Verbs.closedBy, tc.wantClose)
			}
		})
	}
}

func TestDiscardMergeAnswersEachRefusalAsItsArm(t *testing.T) {
	cases := []struct {
		arm  string
		want func(*agentreplv1.DiscardMergeError) bool
	}{
		{arm: merge.ArmUnknownMerge, want: func(e *agentreplv1.DiscardMergeError) bool { return e.GetUnknownMerge() != nil }},
		{arm: merge.ArmStillQueued, want: func(e *agentreplv1.DiscardMergeError) bool { return e.GetStillQueued() != nil }},
	}
	for _, tc := range cases {
		t.Run(tc.arm, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			h.Merge.discardErr = &merge.RefusalError{Arm: tc.arm, Workspace: testWorkspaceID, Reason: "refused"}

			// Act.
			resp, err := h.Client.DiscardMerge(context.Background(), connect.NewRequest(&agentreplv1.DiscardMergeRequest{Workspace: ref(), Requester: userRequester()}))

			// Assert.
			if err != nil || !tc.want(resp.Msg.GetError()) {
				t.Fatalf("DiscardMerge = %v, %v; want the %s arm", resp.Msg.GetResult(), err, tc.arm)
			}
		})
	}
}

func TestStatusListsTheMergesOffTheQueueUnderTheirCause(t *testing.T) {
	// Arrange.
	status := merge.QueueStatus{OffQueue: []merge.OffQueueMember{{Workspace: "ws-m", Name: "fix", Branch: "fix", Requester: "ws-r", Cause: string(merge.CauseUserDrop)}}}

	// Act.
	got := mergeQueueStatusOf(status).GetOffQueue()

	// Assert.
	if len(got) != 1 || got[0].GetName() != "fix" || got[0].GetCause().GetEvicted() == nil || got[0].GetKind().GetAsyncMerge().GetRequester().GetId() != "ws-r" {
		t.Fatalf("off queue = %v, want the evicted async merge", got)
	}
}
