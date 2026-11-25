package server

import (
	"context"
	"strings"
	"testing"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	workspacev1 "agentrepl/proto/workspace/v1"
)

func TestSendBackloggedKeepalivesDelegatesAndAnswersTheKeepersResponse(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.BackloggedKeepalives.response = &agentreplv1.SendBackloggedKeepalivesResponse{
		Result: &agentreplv1.SendBackloggedKeepalivesResponse_Success{Success: &agentreplv1.SendBackloggedKeepalivesSuccess{
			Workspaces: []*agentreplv1.SendBackloggedKeepalivesWorkspace{{
				Workspace: &workspacev1.WorkspaceRef{Id: "w1", Dir: "/work/w1"},
				Outcome:   &agentreplv1.SendBackloggedKeepalivesWorkspace_Sent{Sent: &agentreplv1.SendBackloggedKeepalivesSent{}},
			}},
		}},
	}

	// Act.
	resp, err := h.Client.SendBackloggedKeepalives(context.Background(),
		connect.NewRequest(&agentreplv1.SendBackloggedKeepalivesRequest{}))

	// Assert.
	if err != nil {
		t.Fatalf("SendBackloggedKeepalives() error = %v", err)
	}
	if resp.Msg.GetSuccess().GetWorkspaces()[0].GetSent() == nil || h.BackloggedKeepalives.calls != 1 {
		t.Fatalf("response = %v after %d calls, want the keeper's success from one call", resp.Msg, h.BackloggedKeepalives.calls)
	}
}

func TestSendBackloggedKeepalivesAnswersTheKeepersRefusal(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.BackloggedKeepalives.response = &agentreplv1.SendBackloggedKeepalivesResponse{
		Result: &agentreplv1.SendBackloggedKeepalivesResponse_Error{Error: &agentreplv1.SendBackloggedKeepalivesError{
			Cause: &agentreplv1.SendBackloggedKeepalivesError_RegistryUnreadable{
				RegistryUnreadable: &agentreplv1.SendBackloggedKeepalivesRegistryUnreadable{Detail: "database is locked"},
			},
		}},
	}

	// Act.
	resp, err := h.Client.SendBackloggedKeepalives(context.Background(),
		connect.NewRequest(&agentreplv1.SendBackloggedKeepalivesRequest{}))

	// Assert.
	if err != nil || resp.Msg.GetError().GetRegistryUnreadable().GetDetail() != "database is locked" {
		t.Fatalf("SendBackloggedKeepalives() = %v, %v; want the keeper's refusal", resp, err)
	}
}

func TestNewRefusesAMissingBackloggedKeepalivesKeeper(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	deps := h.withoutDep(func(d *Deps) { d.BackloggedKeepalives = nil })

	// Act.
	_, err := New(deps)

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "the hidden workspaces' cache keeper") {
		t.Fatalf("New = %v, want the refusal naming the cache keeper", err)
	}
}
