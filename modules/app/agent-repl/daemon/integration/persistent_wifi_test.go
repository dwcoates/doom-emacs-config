//go:build integration

package integration

import (
	"testing"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"agent-repld/integration/harness"
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
	// EACH WAIT TAKES ITS OWN BOUND (harness.Daemon.WaitCtx): the toggle
	// spent whatever the boot standing's wait had left, and under a
	// concurrent suite run its host tools outran the remainder (2026-10-20).
	bootCtx, cancelBoot := d.WaitCtx()
	defer cancelBoot()
	first := harness.AwaitView(t, bootCtx, stream, "the standing read at boot", func(r *agentreplv1.WatchDaemonResponse) bool {
		return r.GetPersistentWifi() != nil
	}).GetPersistentWifi()
	if first.GetOff() == nil || first.GetJoined().GetNetworkName() != harness.FakeHostNetwork {
		t.Fatalf("boot standing = %v, want off and joined to %q", first, harness.FakeHostNetwork)
	}

	// Act.
	ctx, cancel := d.WaitCtx()
	defer cancel()
	resp, err := d.Client().UpdatePersistentWifiMode(ctx, connect.NewRequest(&agentreplv1.UpdatePersistentWifiModeRequest{
		Action: &agentreplv1.UpdatePersistentWifiModeRequest_Toggle{Toggle: &agentreplv1.UpdatePersistentWifiModeToggle{}},
	}))

	// Assert.
	if err != nil {
		t.Fatalf("UpdatePersistentWifiMode() error = %v", err)
	}
	success := resp.Msg.GetSuccess()
	if success.GetState().GetOn() == nil || success.GetHotspot().GetJoined().GetNetworkName() != harness.FakeHotspot ||
		success.GetEnergyMode().GetLowPower() == nil || success.GetDisplay().GetDimmed() == nil {
		t.Fatalf("response = %v, want on, the hotspot joined, low power and the display dimmed", resp.Msg)
	}
	toggledCtx, cancelToggled := d.WaitCtx()
	defer cancelToggled()
	harness.AwaitView(t, toggledCtx, stream, "the toggled standing", func(r *agentreplv1.WatchDaemonResponse) bool {
		return r.GetPersistentWifi().GetOn() != nil
	})
}
