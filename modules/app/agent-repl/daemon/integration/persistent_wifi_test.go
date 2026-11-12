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
	// THE STANDING ONCE THE LID IS HEARD: the boot read precedes the lid's
	// subscription, so the first push a stream gets may or may not carry it.
	first := harness.AwaitView(t, bootCtx, stream, "the standing with the lid heard", func(r *agentreplv1.WatchDaemonResponse) bool {
		return r.GetPersistentWifi().GetLidOpen() != nil
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
		success.GetEnergyMode().GetLowPower() == nil || success.GetDisplay().GetUnchanged() == nil {
		t.Fatalf("response = %v, want on, the hotspot joined, low power and the display left alone with the lid open", resp.Msg)
	}
	if got := harness.FakeBrightnessLevel(t, d.HostToolsDir); got != harness.FakeBrightness {
		t.Fatalf("brightness = %s, want the user's own %s with the lid open", got, harness.FakeBrightness)
	}
	toggledCtx, cancelToggled := d.WaitCtx()
	defer cancelToggled()
	harness.AwaitView(t, toggledCtx, stream, "the toggled standing", func(r *agentreplv1.WatchDaemonResponse) bool {
		return r.GetPersistentWifi().GetOn() != nil
	})
}

func TestWithTheModeOnClosingTheLidQuietsTheMachineAndOpeningItWakes(t *testing.T) {
	t.Parallel()
	// Arrange.
	d := harness.StartDaemon(t, harness.Opts{})
	stream := d.WatchEmacsDaemonStream(harness.UnfocusedEditor())
	openCtx, cancelOpen := d.WaitCtx()
	defer cancelOpen()
	harness.AwaitView(t, openCtx, stream, "the lid heard open", func(r *agentreplv1.WatchDaemonResponse) bool {
		return r.GetPersistentWifi().GetLidOpen() != nil
	})
	onCtx, cancelOn := d.WaitCtx()
	defer cancelOn()
	if _, err := d.Client().UpdatePersistentWifiMode(onCtx, connect.NewRequest(&agentreplv1.UpdatePersistentWifiModeRequest{
		Action: &agentreplv1.UpdatePersistentWifiModeRequest_On{On: &agentreplv1.UpdatePersistentWifiModeOn{}},
	})); err != nil {
		t.Fatalf("UpdatePersistentWifiMode(on) error = %v", err)
	}

	// Act: close the lid.
	d.Lid.Close()

	// Assert: the quiet state stands, the display asleep at the user's level.
	closedCtx, cancelClosed := d.WaitCtx()
	defer cancelClosed()
	harness.AwaitView(t, closedCtx, stream, "the closed lid quieted the machine", func(r *agentreplv1.WatchDaemonResponse) bool {
		s := r.GetPersistentWifi()
		return s.GetLidClosed() != nil && s.GetQuiet().GetDisplayAsleep() != nil
	})
	if got := harness.FakeDisplay(t, d.HostToolsDir); got != "asleep" {
		t.Fatalf("display with the lid closed = %s, want asleep", got)
	}
	if got := harness.FakeBrightnessLevel(t, d.HostToolsDir); got != harness.FakeBrightness {
		t.Fatalf("brightness with the lid closed = %s, want the user's own %s", got, harness.FakeBrightness)
	}

	// Act: open it.
	d.Lid.Open()

	// Assert: the quiet state is over and the display awake.
	reopenedCtx, cancelReopened := d.WaitCtx()
	defer cancelReopened()
	harness.AwaitView(t, reopenedCtx, stream, "the opened lid left the quiet state", func(r *agentreplv1.WatchDaemonResponse) bool {
		s := r.GetPersistentWifi()
		return s.GetLidOpen() != nil && s.GetQuiet() == nil
	})
	if got := harness.FakeDisplay(t, d.HostToolsDir); got != "awake" {
		t.Fatalf("display after opening = %s, want awake", got)
	}
}

func TestTurningTheModeOffUnderAClosedLidWakesTheDisplay(t *testing.T) {
	t.Parallel()
	// Arrange.
	d := harness.StartDaemon(t, harness.Opts{})
	stream := d.WatchEmacsDaemonStream(harness.UnfocusedEditor())
	d.Lid.Close()
	closedCtx, cancelClosed := d.WaitCtx()
	defer cancelClosed()
	harness.AwaitView(t, closedCtx, stream, "the lid heard closed", func(r *agentreplv1.WatchDaemonResponse) bool {
		return r.GetPersistentWifi().GetLidClosed() != nil
	})
	onCtx, cancelOn := d.WaitCtx()
	defer cancelOn()
	on, err := d.Client().UpdatePersistentWifiMode(onCtx, connect.NewRequest(&agentreplv1.UpdatePersistentWifiModeRequest{
		Action: &agentreplv1.UpdatePersistentWifiModeRequest_On{On: &agentreplv1.UpdatePersistentWifiModeOn{}},
	}))
	if err != nil || on.Msg.GetSuccess().GetDisplay().GetSlept() == nil {
		t.Fatalf("UpdatePersistentWifiMode(on) = %v, %v, want the display slept for the closed lid", on, err)
	}

	// Act.
	offCtx, cancelOff := d.WaitCtx()
	defer cancelOff()
	off, err := d.Client().UpdatePersistentWifiMode(offCtx, connect.NewRequest(&agentreplv1.UpdatePersistentWifiModeRequest{
		Action: &agentreplv1.UpdatePersistentWifiModeRequest_Off{Off: &agentreplv1.UpdatePersistentWifiModeOff{}},
	}))

	// Assert.
	if err != nil || off.Msg.GetSuccess().GetDisplay().GetWoke() == nil || off.Msg.GetSuccess().GetState().GetQuiet() != nil {
		t.Fatalf("UpdatePersistentWifiMode(off) = %v, %v, want the display woken and no quiet state", off, err)
	}
	if got := harness.FakeDisplay(t, d.HostToolsDir); got != "awake" {
		t.Fatalf("display = %s, want awake", got)
	}
}
