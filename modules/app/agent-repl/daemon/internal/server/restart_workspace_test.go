package server

import (
	"context"
	"testing"

	"connectrpc.com/connect"
	"google.golang.org/protobuf/proto"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"agent-repld/internal/workspace"
)

func TestRestartWorkspaceChoosesItsMode(t *testing.T) {
	tests := []struct {
		name             string
		whenIdle         bool
		wantRestarts     int
		wantIdleRestarts int
	}{
		{name: "immediate by default", wantRestarts: 1},
		{name: "once idle when asked", whenIdle: true, wantIdleRestarts: 1},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)

			// Act.
			resp, err := h.Client.RestartWorkspace(context.Background(),
				connect.NewRequest(&agentreplv1.RestartWorkspaceRequest{Workspace: ref(), WhenIdle: tt.whenIdle}))

			// Assert.
			if err != nil || resp.Msg.GetSuccess() == nil {
				t.Fatalf("RestartWorkspace = %v, %v; want success", resp, err)
			}
			if h.Verbs.restarts != tt.wantRestarts || h.Verbs.idleRestarts != tt.wantIdleRestarts {
				t.Fatalf("restarts %d, idle restarts %d; want %d, %d", h.Verbs.restarts, h.Verbs.idleRestarts, tt.wantRestarts, tt.wantIdleRestarts)
			}
		})
	}
}

func TestRestartWorkspaceWhenIdleAnswersScheduled(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Verbs.idleRestart = workspace.IdleRestart{Scheduled: true, TurnInFlight: true, DetachedWork: 3, AlreadyPending: true}

	// Act.
	resp, err := h.Client.RestartWorkspace(context.Background(),
		connect.NewRequest(&agentreplv1.RestartWorkspaceRequest{Workspace: ref(), WhenIdle: true}))

	// Assert.
	want := &agentreplv1.RestartWorkspaceScheduled{TurnInFlight: true, DetachedWork: 3, AlreadyPending: true}
	if err != nil || !proto.Equal(resp.Msg.GetScheduled(), want) {
		t.Fatalf("RestartWorkspace = %v, %v; want scheduled %v", resp, err, want)
	}
}

func TestRestartWorkspaceWhenIdleAnswersARelaunchFailure(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Verbs.idleRestartErr = &workspace.Refusal{
		Rpc: "RestartWorkspace", Arm: "relaunch_failed", Reason: "the restart failed",
		Fields: map[string]any{"detail": "prelaunch failed"},
	}

	// Act.
	resp, err := h.Client.RestartWorkspace(context.Background(),
		connect.NewRequest(&agentreplv1.RestartWorkspaceRequest{Workspace: ref(), WhenIdle: true}))

	// Assert.
	if err != nil || resp.Msg.GetError().GetRelaunchFailed().GetDetail() != "prelaunch failed" {
		t.Fatalf("RestartWorkspace = %v, %v; want relaunch_failed carrying its detail", resp, err)
	}
}
