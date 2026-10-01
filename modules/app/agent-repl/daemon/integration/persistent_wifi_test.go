//go:build integration

package integration

import (
	"testing"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"claude-repld/integration/harness"
)

// ==========================================================================
// Persistent wifi mode. The daemon reads the machine's standing before it
// serves, pushes it on every Emacs WatchDaemon stream, and changes it through
// UpdatePersistentWifiMode — all against the harness's fake host tools.
// ==========================================================================

func TestAnEmacsStreamIsToldTheStandingAndAToggleTurnsItOver(t *testing.T) {
	t.Parallel()
	// Arrange.
	d := harness.StartDaemon(t, harness.Opts{})
	stream := d.WatchEmacsDaemonStream(harness.UnfocusedEditor())
	ctx, cancel := d.WaitCtx()
	defer cancel()
	first := harness.AwaitView(t, ctx, stream, "the standing read at boot", func(r *agentreplv1.WatchDaemonResponse) bool {
		return r.GetPersistentWifi() != nil
	}).GetPersistentWifi()
	if first.GetOff() == nil || first.GetJoined().GetNetworkName() != harness.FakeHostNetwork {
		t.Fatalf("boot standing = %v, want off and joined to %q", first, harness.FakeHostNetwork)
	}

	// Act.
	resp, err := d.Client().UpdatePersistentWifiMode(ctx, connect.NewRequest(&agentreplv1.UpdatePersistentWifiModeRequest{
		Action: &agentreplv1.UpdatePersistentWifiModeRequest_Toggle{Toggle: &agentreplv1.UpdatePersistentWifiModeToggle{}},
	}))

	// Assert.
	if err != nil {
		t.Fatalf("UpdatePersistentWifiMode() error = %v", err)
	}
	success := resp.Msg.GetSuccess()
	if success.GetState().GetOn() == nil || success.GetHotspot().GetJoined().GetNetworkName() != harness.FakeHotspot ||
		success.GetDisplay().GetDimmed() == nil {
		t.Fatalf("response = %v, want on, the hotspot joined and the display dimmed", resp.Msg)
	}
	harness.AwaitView(t, ctx, stream, "the toggled standing", func(r *agentreplv1.WatchDaemonResponse) bool {
		return r.GetPersistentWifi().GetOn() != nil
	})
}
