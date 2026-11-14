package server

import (
	"context"
	"errors"
	"strings"
	"testing"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"agent-repld/internal/ids"
	"agent-repld/internal/workspace"
	"agent-repld/internal/wsm"
)

// holdingBypass makes the test workspace hold its repository's queue bypass
// mode.
func holdingBypass(h *harness) {
	h.DB.bypass = map[ids.WorkspaceID]wsm.QueueBypass{testWorkspaceID: {Holds: true}}
}

// bypassOneShotCreate is a one-shot create the test workspace asks for.
func bypassOneShotCreate(h *harness) *agentreplv1.CreateWorkspaceRequest {
	req := validCreate(func(r *agentreplv1.CreateWorkspaceRequest) {
		r.Form = &agentreplv1.CreateWorkspaceRequest_OneShot{OneShot: &agentreplv1.CreateWorkspaceOneShot{
			Prompt: workspace.SaidText("fix the feed"), Relationship: peerRelationship()}}
		r.Requester = workspaceRequester(ref())
	})
	req.Repository = registeredRepoRef(h)
	return req
}

// bypassStandardCreate is a standard create the test workspace asks for,
// under RELATIONSHIP.
func bypassStandardCreate(h *harness, relationship *agentreplv1.WorkspaceRelationship) *agentreplv1.CreateWorkspaceRequest {
	req := validCreate(func(r *agentreplv1.CreateWorkspaceRequest) {
		r.Form = &agentreplv1.CreateWorkspaceRequest_Standard{Standard: &agentreplv1.CreateWorkspaceStandard{
			InitialPrompt: workspace.SaidText("fix the feed"), Relationship: relationship}}
		r.Requester = workspaceRequester(ref())
	})
	req.Repository = registeredRepoRef(h)
	return req
}

// TestEveryCreateAskedWhileHoldingTheBypassModeIsMade covers the owner's
// ruling (2026-11-14): a workspace in queue bypass mode creates its peers,
// children and one-shots as any workspace does, since the queue they land
// through keeps running while the mode is held.
func TestEveryCreateAskedWhileHoldingTheBypassModeIsMade(t *testing.T) {
	tests := []struct {
		name string
		req  func(h *harness) *agentreplv1.CreateWorkspaceRequest
	}{
		{name: "a peer", req: func(h *harness) *agentreplv1.CreateWorkspaceRequest {
			return bypassStandardCreate(h, peerRelationship())
		}},
		{name: "a child", req: func(h *harness) *agentreplv1.CreateWorkspaceRequest {
			return bypassStandardCreate(h, childRelationship(ref()))
		}},
		{name: "a one-shot", req: bypassOneShotCreate},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			holdingBypass(h)
			h.Verbs.createRec = wsm.Workspace{ID: "ws-created", Dir: "/tmp/ws", Name: "minted"}

			// Act
			resp, err := h.Client.CreateWorkspace(context.Background(), connect.NewRequest(tt.req(h)))

			// Assert
			if err != nil || resp.Msg.GetSuccess() == nil || len(h.Verbs.createSpecs) != 1 {
				t.Fatalf("CreateWorkspace = %v, %v (creates %d); want it made", resp, err, len(h.Verbs.createSpecs))
			}
		})
	}
}

func TestAOneShotAskedWithoutTheBypassModeIsCreated(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.Verbs.createRec = wsm.Workspace{ID: "ws-created", Dir: "/tmp/ws", Name: "minted"}

	// Act
	resp, err := h.Client.CreateWorkspace(context.Background(), connect.NewRequest(bypassOneShotCreate(h)))

	// Assert
	if err != nil || resp.Msg.GetSuccess() == nil {
		t.Fatalf("CreateWorkspace = %v, %v; want success", resp, err)
	}
}

func TestMarkingAWorkspaceHoldingTheBypassModeAOneShotIsRefused(t *testing.T) {
	// Arrange
	h := newHarness(t)
	holdingBypass(h)

	// Act
	_, err := h.Client.SetWorkspaceOneShot(context.Background(), connect.NewRequest(&agentreplv1.SetWorkspaceOneShotRequest{
		Workspace: ref(), Requester: workspaceRequester(ref()), OneShot: true}))

	// Assert
	if err == nil || !strings.Contains(err.Error(), "queue bypass mode") || len(h.Verbs.api.oneShots) != 0 {
		t.Fatalf("err %v, marks %+v; want the bypass refusal and no mark", err, h.Verbs.api.oneShots)
	}
}

func TestAOneShotMarkWhoseStandingCannotBeReadFails(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.DB.bypassErr = errors.New("store down")

	// Act
	_, err := h.Client.SetWorkspaceOneShot(context.Background(), connect.NewRequest(&agentreplv1.SetWorkspaceOneShotRequest{
		Workspace: ref(), Requester: workspaceRequester(ref()), OneShot: true}))

	// Assert
	if err == nil || len(h.Verbs.api.oneShots) != 0 {
		t.Fatalf("err %v, marks %+v; want the read's failure and no mark", err, h.Verbs.api.oneShots)
	}
}

func TestAnAsyncMergeAskedWhileHoldingTheBypassModeIsAccepted(t *testing.T) {
	// Arrange
	h, req := asyncHarness(t)
	holdingBypass(h)
	async := req.GetAsyncMerge()
	async.Target = &agentreplv1.CreateWorkspaceAsyncMergeTarget{Target: &agentreplv1.CreateWorkspaceAsyncMergeTarget_Maker{Maker: &agentreplv1.CreateWorkspaceAsyncMergeTargetMaker{}}}
	async.Placement = frontPlacement()
	async.Source.Branch = "agent-1/fix"

	// Act
	resp, err := h.Client.CreateWorkspace(context.Background(), connect.NewRequest(req))

	// Assert
	if err != nil || resp.Msg.GetAsyncMergeAccepted() == nil || len(h.Merge.requests) != 1 {
		t.Fatalf("CreateWorkspace = %v, %v; want the async merge accepted", resp, err)
	}
}
