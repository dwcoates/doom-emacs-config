package server

import (
	"context"
	"slices"
	"testing"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"agent-repld/internal/ids"
)

func TestParkWorkspaceAsksTheCycleToParkTheWorkspace(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	resp, err := h.Client.ParkWorkspace(context.Background(),
		connect.NewRequest(&agentreplv1.ParkWorkspaceRequest{Workspace: ref()}))

	// Assert.
	if err != nil || resp.Msg.GetSuccess() == nil {
		t.Fatalf("ParkWorkspace = %v, %v; want success", resp, err)
	}
	if !slices.Equal(h.Drain.cycled, []ids.WorkspaceID{testWorkspaceID}) {
		t.Fatalf("cycled = %v, want the workspace parked", h.Drain.cycled)
	}
}

func TestParkWorkspaceRefusesAnUnknownWorkspace(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	unknown := ref()
	unknown.Id = "ws-unknown"

	// Act.
	resp, err := h.Client.ParkWorkspace(context.Background(),
		connect.NewRequest(&agentreplv1.ParkWorkspaceRequest{Workspace: unknown}))

	// Assert.
	if err != nil || resp.Msg.GetError().GetUnknownWorkspace() == nil || len(h.Drain.cycled) != 0 {
		t.Fatalf("ParkWorkspace = %v, %v with cycled %v; want unknown_workspace and nothing parked", resp, err, h.Drain.cycled)
	}
}
