package server

import (
	"context"
	"testing"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"agent-repld/internal/workspace"
	"agent-repld/internal/wsm"
)

// remoteControlRequest is the request on the fixture workspace for ACTION's
// arm ("on", "off" or "toggle"); any other names no arm.
func remoteControlRequest(action string) *agentreplv1.UpdateRemoteControlModeRequest {
	req := &agentreplv1.UpdateRemoteControlModeRequest{Workspace: ref()}
	switch action {
	case "on":
		req.Action = &agentreplv1.UpdateRemoteControlModeRequest_On{On: &agentreplv1.UpdateRemoteControlModeOn{}}
	case "off":
		req.Action = &agentreplv1.UpdateRemoteControlModeRequest_Off{Off: &agentreplv1.UpdateRemoteControlModeOff{}}
	case "toggle":
		req.Action = &agentreplv1.UpdateRemoteControlModeRequest_Toggle{Toggle: &agentreplv1.UpdateRemoteControlModeToggle{}}
	}
	return req
}

func TestUpdateRemoteControlModeRelaysEachActionToTheVerb(t *testing.T) {
	tests := []struct {
		action string
		want   wsm.RemoteControlChange
	}{
		{"on", wsm.RemoteControlOn},
		{"off", wsm.RemoteControlOff},
		{"toggle", wsm.RemoteControlToggle},
	}
	for _, tt := range tests {
		t.Run(tt.action, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)

			// Act.
			_, err := h.Client.UpdateRemoteControlMode(context.Background(), connect.NewRequest(remoteControlRequest(tt.action)))

			// Assert.
			if err != nil || len(h.Verbs.remoteChanges) != 1 || h.Verbs.remoteChanges[0] != tt.want {
				t.Fatalf("UpdateRemoteControlMode = %v, verb saw %v; want one %s", err, h.Verbs.remoteChanges, tt.want)
			}
		})
	}
}

func TestUpdateRemoteControlModeAnswersTheModeLeft(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Verbs.remoteOn = true

	// Act.
	resp, err := h.Client.UpdateRemoteControlMode(context.Background(), connect.NewRequest(remoteControlRequest("toggle")))

	// Assert.
	if err != nil || resp.Msg.GetSuccess() == nil || !resp.Msg.GetSuccess().GetOn() {
		t.Fatalf("UpdateRemoteControlMode = (%v, %v), want a success stating on", resp, err)
	}
}

func TestUpdateRemoteControlModeRefusesARequestNamingNoAction(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	_, err := h.Client.UpdateRemoteControlMode(context.Background(), connect.NewRequest(remoteControlRequest("")))

	// Assert.
	if connect.CodeOf(err) != connect.CodeInvalidArgument || len(h.Verbs.remoteChanges) != 0 {
		t.Fatalf("err = %v, verb saw %v; want InvalidArgument and no change", err, h.Verbs.remoteChanges)
	}
}

func TestUpdateRemoteControlModeRefusesARequestNamingNoWorkspace(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	req := remoteControlRequest("on")
	req.Workspace = nil

	// Act.
	_, err := h.Client.UpdateRemoteControlMode(context.Background(), connect.NewRequest(req))

	// Assert.
	if connect.CodeOf(err) != connect.CodeInvalidArgument {
		t.Fatalf("err = %v, want InvalidArgument", err)
	}
}

func TestUpdateRemoteControlModeAnswersAMismatchedRef(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	req := remoteControlRequest("on")
	req.Workspace = &workspacev1.WorkspaceRef{Id: string(testWorkspaceID), Dir: "/elsewhere"}

	// Act.
	resp, err := h.Client.UpdateRemoteControlMode(context.Background(), connect.NewRequest(req))

	// Assert.
	if err != nil || resp.Msg.GetError().GetWorkspaceRefMismatch() == nil || len(h.Verbs.remoteChanges) != 0 {
		t.Fatalf("UpdateRemoteControlMode = (%v, %v), want workspace_ref_mismatch and no change", resp, err)
	}
}

func TestUpdateRemoteControlModeAnswersEachRefusalByItsArm(t *testing.T) {
	tests := []struct {
		name string
		err  error
		want func(*agentreplv1.UpdateRemoteControlModeError) bool
	}{
		{
			name: "an unknown workspace",
			err:  &workspace.Refusal{Arm: workspace.ArmUnknownWorkspace, Reason: "gone", NotFound: true},
			want: func(e *agentreplv1.UpdateRemoteControlModeError) bool { return e.GetUnknownWorkspace() != nil },
		},
		{
			name: "a workspace handed to a successor",
			err:  &workspace.Refusal{Arm: workspace.ArmTransferringAway, Reason: "handed over"},
			want: func(e *agentreplv1.UpdateRemoteControlModeError) bool { return e.GetTransferringAway() != nil },
		},
		{
			name: "a workspace not yet adopted",
			err:  &workspace.Refusal{Arm: workspace.ArmNotYetAdopted, Reason: "joining"},
			want: func(e *agentreplv1.UpdateRemoteControlModeError) bool { return e.GetNotYetAdopted() != nil },
		},
		{
			name: "the vendor's refusal carries its detail",
			err:  &workspace.ShimRefusal{Verb: "SetSessionRemoteControl", Arm: workspace.ArmShimVendorRefused, Detail: "disabled by policy"},
			want: func(e *agentreplv1.UpdateRemoteControlModeError) bool {
				return e.GetVendorRefused().GetDetail() == "disabled by policy"
			},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			h.Verbs.remoteErr = tt.err

			// Act.
			resp, err := h.Client.UpdateRemoteControlMode(context.Background(), connect.NewRequest(remoteControlRequest("on")))

			// Assert.
			if err != nil {
				t.Fatalf("UpdateRemoteControlMode answered a transport error: %v", err)
			}
			if !tt.want(resp.Msg.GetError()) {
				t.Fatalf("result = %v, want the arm", resp.Msg.GetResult())
			}
		})
	}
}

func TestUpdateRemoteControlModeAnswersAnUnspecifiedShimRefusalAsUnlanded(t *testing.T) {
	// Arrange: ERROR-ARMS.md's row.
	h := newHarness(t)
	h.Verbs.remoteErr = &workspace.ShimRefusal{Verb: "SetSessionRemoteControl", Arm: workspace.ArmShimUnspecified, Detail: "no cause"}

	// Act.
	_, err := h.Client.UpdateRemoteControlMode(context.Background(), connect.NewRequest(remoteControlRequest("on")))

	// Assert.
	if connect.CodeOf(err) != connect.CodeFailedPrecondition {
		t.Fatalf("err = %v, want the unlanded arm's FailedPrecondition", err)
	}
}
