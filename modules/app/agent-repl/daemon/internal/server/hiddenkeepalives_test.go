package server

import (
	"context"
	"strings"
	"testing"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	workspacev1 "agentrepl/proto/workspace/v1"
)

func TestSendHiddenKeepalivesDelegatesAndAnswersTheKeepersResponse(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.HiddenKeepalives.response = &agentreplv1.SendHiddenKeepalivesResponse{
		Result: &agentreplv1.SendHiddenKeepalivesResponse_Success{Success: &agentreplv1.SendHiddenKeepalivesSuccess{
			Workspaces: []*agentreplv1.SendHiddenKeepalivesWorkspace{{
				Workspace: &workspacev1.WorkspaceRef{Id: "w1", Dir: "/work/w1"},
				Outcome:   &agentreplv1.SendHiddenKeepalivesWorkspace_Sent{Sent: &agentreplv1.SendHiddenKeepalivesSent{}},
			}},
		}},
	}

	// Act.
	resp, err := h.Client.SendHiddenKeepalives(context.Background(),
		connect.NewRequest(&agentreplv1.SendHiddenKeepalivesRequest{}))

	// Assert.
	if err != nil {
		t.Fatalf("SendHiddenKeepalives() error = %v", err)
	}
	if resp.Msg.GetSuccess().GetWorkspaces()[0].GetSent() == nil || h.HiddenKeepalives.calls != 1 {
		t.Fatalf("response = %v after %d calls, want the keeper's success from one call", resp.Msg, h.HiddenKeepalives.calls)
	}
}

func TestSendHiddenKeepalivesAnswersTheKeepersRefusal(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.HiddenKeepalives.response = &agentreplv1.SendHiddenKeepalivesResponse{
		Result: &agentreplv1.SendHiddenKeepalivesResponse_Error{Error: &agentreplv1.SendHiddenKeepalivesError{
			Cause: &agentreplv1.SendHiddenKeepalivesError_RegistryUnreadable{
				RegistryUnreadable: &agentreplv1.SendHiddenKeepalivesRegistryUnreadable{Detail: "database is locked"},
			},
		}},
	}

	// Act.
	resp, err := h.Client.SendHiddenKeepalives(context.Background(),
		connect.NewRequest(&agentreplv1.SendHiddenKeepalivesRequest{}))

	// Assert.
	if err != nil || resp.Msg.GetError().GetRegistryUnreadable().GetDetail() != "database is locked" {
		t.Fatalf("SendHiddenKeepalives() = %v, %v; want the keeper's refusal", resp, err)
	}
}

func TestNewRefusesAMissingHiddenKeepalivesKeeper(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	deps := h.withoutDep(func(d *Deps) { d.HiddenKeepalives = nil })

	// Act.
	_, err := New(deps)

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "the hidden workspaces' cache keeper") {
		t.Fatalf("New = %v, want the refusal naming the cache keeper", err)
	}
}
