//go:build integration

package integration

import (
	"testing"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/integration/harness"
)

// toggleRemoteControl turns the fixture workspace's remote control over and
// answers the mode the daemon says it left.
func toggleRemoteControl(t *testing.T, f *fixture) bool {
	t.Helper()
	resp, err := f.d.Client().UpdateRemoteControlMode(f.d.Ctx(), connect.NewRequest(&agentreplv1.UpdateRemoteControlModeRequest{
		Workspace: f.ws,
		Action:    &agentreplv1.UpdateRemoteControlModeRequest_Toggle{Toggle: &agentreplv1.UpdateRemoteControlModeToggle{}},
	}))
	if err != nil || resp.Msg.GetSuccess() == nil {
		t.Fatalf("UpdateRemoteControlMode(toggle) = (%v, %v), want a success", resp, err)
	}
	return resp.Msg.GetSuccess().GetOn()
}

// TestRemoteControlIsPutInForceAndTurnedOverThroughTheShim is the endpoint's
// wiring through the real daemon against the fake shim: a main worktree's
// default on reaches the shim at its bring-up, each toggle reaches it and the
// topbar's chip, and the claude.ai session the first bridge answered is
// re-bound by the next on. Its steps share one workspace in order, so they
// run in sequence.
func TestRemoteControlIsPutInForceAndTurnedOverThroughTheShim(t *testing.T) {
	t.Parallel()
	f := newOpened(t, harness.Opts{})
	topbar := f.d.WatchTopbar(f.ws)

	t.Run("the bring-up puts a main worktree's default on in force", func(t *testing.T) {
		// Arrange: the opened fixture. Act: its bring-up. Assert.
		req := f.shim.ExpectSetSessionRemoteControl()
		if !req.GetEnabled() || req.GetName() == "" || req.ReattachBridgeSessionId != nil {
			t.Fatalf("SetSessionRemoteControl = %v, want on under the workspace's name, re-binding nothing", req)
		}
		awaitTopbar(t, f, topbar, "the chip on", func(v *frontendv1.TopbarView) bool {
			return v.GetRemoteControl().GetOn() != nil
		})
	})

	t.Run("a toggle turns the shim's bridge off and the chip over", func(t *testing.T) {
		// Act.
		on := toggleRemoteControl(t, f)

		// Assert.
		if on {
			t.Fatal("the toggle answered on, want off from the main worktree's default")
		}
		if req := f.shim.ExpectSetSessionRemoteControl(); req.GetEnabled() {
			t.Fatalf("SetSessionRemoteControl = %v, want off", req)
		}
		awaitTopbar(t, f, topbar, "the chip off", func(v *frontendv1.TopbarView) bool {
			return v.GetRemoteControl().GetOff() != nil
		})
	})

	t.Run("a toggle back re-binds the claude.ai session the first bridge answered", func(t *testing.T) {
		// Act.
		on := toggleRemoteControl(t, f)

		// Assert.
		if !on {
			t.Fatal("the toggle answered off, want on")
		}
		if req := f.shim.ExpectSetSessionRemoteControl(); !req.GetEnabled() || req.GetReattachBridgeSessionId() != harness.FakeBridgeSessionID {
			t.Fatalf("SetSessionRemoteControl = %v, want on re-binding %q", req, harness.FakeBridgeSessionID)
		}
		awaitTopbar(t, f, topbar, "the chip on again", func(v *frontendv1.TopbarView) bool {
			return v.GetRemoteControl().GetOn() != nil
		})
	})
}
