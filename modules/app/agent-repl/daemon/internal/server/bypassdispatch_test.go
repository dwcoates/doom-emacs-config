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

func TestAOneShotAskedWhileHoldingTheBypassModeIsRefusedWithItsReminder(t *testing.T) {
	// Arrange
	h := newHarness(t)
	holdingBypass(h)

	// Act
	resp, err := h.Client.CreateWorkspace(context.Background(), connect.NewRequest(bypassOneShotCreate(h)))

	// Assert
	if err != nil {
		t.Fatalf("CreateWorkspace: %v", err)
	}
	refusal := resp.Msg.GetError().GetRequesterBypassingQueue()
	if refusal == nil || !strings.Contains(refusal.GetDetail(), "land-direct.sh") {
		t.Fatalf("answer = %v, want the bypass refusal with its reminder", resp.Msg)
	}
	if len(h.Verbs.createSpecs) != 0 {
		t.Fatalf("creates = %+v, want none", h.Verbs.createSpecs)
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

func TestAOneShotWhoseRequestersStandingCannotBeReadFails(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.DB.bypassErr = errors.New("store down")

	// Act
	_, err := h.Client.CreateWorkspace(context.Background(), connect.NewRequest(bypassOneShotCreate(h)))

	// Assert
	if err == nil || len(h.Verbs.createSpecs) != 0 {
		t.Fatalf("err %v, creates %d; want the read's failure and nothing created", err, len(h.Verbs.createSpecs))
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
