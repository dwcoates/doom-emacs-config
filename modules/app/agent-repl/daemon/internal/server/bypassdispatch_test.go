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

func TestAChildAskedWhileHoldingTheBypassModeIsRefusedWithItsReminder(t *testing.T) {
	// Arrange
	h := newHarness(t)
	holdingBypass(h)

	// Act
	resp, err := h.Client.CreateWorkspace(context.Background(), connect.NewRequest(bypassStandardCreate(h, childRelationship(ref()))))

	// Assert
	if err != nil {
		t.Fatalf("CreateWorkspace: %v", err)
	}
	refusal := resp.Msg.GetError().GetRequesterBypassingQueue()
	if refusal == nil || !strings.Contains(refusal.GetDetail(), "a child workspace refused") || !strings.Contains(refusal.GetDetail(), "no child workspace") {
		t.Fatalf("answer = %v, want the bypass refusal naming the child", resp.Msg)
	}
	if len(h.Verbs.createSpecs) != 0 {
		t.Fatalf("creates = %+v, want none", h.Verbs.createSpecs)
	}
}

func TestAPeerAskedWhileHoldingTheBypassModeIsCreated(t *testing.T) {
	// Arrange
	h := newHarness(t)
	holdingBypass(h)
	h.Verbs.createRec = wsm.Workspace{ID: "ws-created", Dir: "/tmp/ws", Name: "minted"}

	// Act
	resp, err := h.Client.CreateWorkspace(context.Background(), connect.NewRequest(bypassStandardCreate(h, peerRelationship())))

	// Assert
	if err != nil || resp.Msg.GetSuccess() == nil {
		t.Fatalf("CreateWorkspace = %v, %v; want success", resp, err)
	}
}

func TestBypassRefusedCreateNamesEveryCreateTheModeRefuses(t *testing.T) {
	parent := ids.WorkspaceID("ws-parent")
	tests := []struct {
		name string
		spec workspace.CreateSpec
		want string
	}{
		{name: "a one-shot", spec: workspace.CreateSpec{OneShot: true}, want: "a one-shot workspace"},
		{name: "a one-shot child", spec: workspace.CreateSpec{OneShot: true, Parent: &parent}, want: "a one-shot workspace"},
		{name: "a child", spec: workspace.CreateSpec{Parent: &parent}, want: "a child workspace"},
		{name: "a peer", spec: workspace.CreateSpec{}, want: ""},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := bypassRefusedCreate(tt.spec)

			// Assert
			if got != tt.want {
				t.Fatalf("bypassRefusedCreate = %q, want %q", got, tt.want)
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
