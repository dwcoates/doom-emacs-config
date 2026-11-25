//go:build integration

package integration

import (
	"path/filepath"
	"testing"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"agent-repld/integration/harness"
	"agent-repld/internal/persistentwifi"
)

// recordedEnergyMode answers the energy mode d's quiet state recorded, empty
// when none is kept.
func recordedEnergyMode(t *testing.T, d *harness.Daemon) string {
	t.Helper()
	rec, ok, err := persistentwifi.EnergyFileStore{Path: filepath.Join(d.StateDir, "persistent-wifi", persistentwifi.EnergyRecordFile)}.Load()
	if err != nil {
		t.Fatalf("read the recorded energy mode: %v", err)
	}
	if !ok {
		return ""
	}
	return rec.Mode
}

// awaitPowerSource waits for d to have heard its power source as source
// ("ac" or "battery"), so a lid or mode change after it is decided on it.
func awaitPowerSource(d *harness.Daemon, source string) {
	d.AwaitLogRecord(d.RunLogPath(), "the power source heard as "+source, func(r harness.LogRecord) bool {
		return r.Operation == "daemon.persistentwifi.power_source" && r.Message == "the power source changed" &&
			r.Context["power_source"] == source
	})
}

// awaitEnergyRecord waits for d's energy step to record msg with trigger.
func awaitEnergyRecord(d *harness.Daemon, msg, trigger string) {
	d.AwaitLogRecord(d.RunLogPath(), msg+" on "+trigger, func(r harness.LogRecord) bool {
		return r.Operation == "daemon.persistentwifi.energy" && r.Message == msg && r.Context["trigger"] == trigger
	})
}

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
	first := harness.AwaitBootWifiStanding(t, bootCtx, stream)
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
		success.GetEnergyMode().GetUnchanged() == nil || success.GetDisplay().GetUnchanged() == nil {
		t.Fatalf("response = %v, want on, the hotspot joined, and the energy mode and the display left alone with the lid open", resp.Msg)
	}
	if got := harness.FakeEnergyModeIn(t, d.HostToolsDir); got != harness.FakeEnergyMode {
		t.Fatalf("energy mode = %s, want the user's own %s with the lid open", got, harness.FakeEnergyMode)
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
	harness.AwaitBootWifiStanding(t, openCtx, stream)
	onCtx, cancelOn := d.WaitCtx()
	defer cancelOn()
	if _, err := d.Client().UpdatePersistentWifiMode(onCtx, connect.NewRequest(&agentreplv1.UpdatePersistentWifiModeRequest{
		Action: &agentreplv1.UpdatePersistentWifiModeRequest_On{On: &agentreplv1.UpdatePersistentWifiModeOn{}},
	})); err != nil {
		t.Fatalf("UpdatePersistentWifiMode(on) error = %v", err)
	}

	// Act: close the lid, on battery power.
	d.PowerSource.Unplug()
	awaitPowerSource(d, "battery")
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
	if got, kept := harness.FakeEnergyModeIn(t, d.HostToolsDir), recordedEnergyMode(t, d); got != "1" || kept != harness.FakeEnergyMode {
		t.Fatalf("energy mode with the lid closed = %s, recorded %q, want Low Power Mode with the user's %s recorded", got, kept, harness.FakeEnergyMode)
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
	if got, kept := harness.FakeEnergyModeIn(t, d.HostToolsDir), recordedEnergyMode(t, d); got != harness.FakeEnergyMode || kept != "" {
		t.Fatalf("energy mode after opening = %s, recorded %q, want the user's %s restored and the record forgotten", got, kept, harness.FakeEnergyMode)
	}
}

func TestTurningTheModeOffUnderAClosedLidWakesTheDisplay(t *testing.T) {
	t.Parallel()
	// Arrange.
	d := harness.StartDaemon(t, harness.Opts{})
	stream := d.WatchEmacsDaemonStream(harness.UnfocusedEditor())
	d.PowerSource.Unplug()
	awaitPowerSource(d, "battery")
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
	if err != nil || on.Msg.GetSuccess().GetDisplay().GetSlept() == nil || on.Msg.GetSuccess().GetEnergyMode().GetLowPower() == nil {
		t.Fatalf("UpdatePersistentWifiMode(on) = %v, %v, want the display slept and Low Power Mode set for the closed lid", on, err)
	}

	// Act.
	offCtx, cancelOff := d.WaitCtx()
	defer cancelOff()
	off, err := d.Client().UpdatePersistentWifiMode(offCtx, connect.NewRequest(&agentreplv1.UpdatePersistentWifiModeRequest{
		Action: &agentreplv1.UpdatePersistentWifiModeRequest_Off{Off: &agentreplv1.UpdatePersistentWifiModeOff{}},
	}))

	// Assert.
	if err != nil || off.Msg.GetSuccess().GetDisplay().GetWoke() == nil || off.Msg.GetSuccess().GetState().GetQuiet() != nil ||
		off.Msg.GetSuccess().GetEnergyMode().GetRestored().GetPowerMode() != harness.FakeEnergyMode {
		t.Fatalf("UpdatePersistentWifiMode(off) = %v, %v, want the display woken, no quiet state and the user's energy mode restored", off, err)
	}
	if got, kept := harness.FakeEnergyModeIn(t, d.HostToolsDir), recordedEnergyMode(t, d); got != harness.FakeEnergyMode || kept != "" {
		t.Fatalf("energy mode = %s, recorded %q, want the user's %s restored and the record forgotten", got, kept, harness.FakeEnergyMode)
	}
	if got := harness.FakeDisplay(t, d.HostToolsDir); got != "awake" {
		t.Fatalf("display = %s, want awake", got)
	}
}

func TestUnderAClosedLidLowPowerModeFollowsThePowerSource(t *testing.T) {
	t.Parallel()
	// Arrange: the mode on, the machine on AC power.
	d := harness.StartDaemon(t, harness.Opts{})
	stream := d.WatchEmacsDaemonStream(harness.UnfocusedEditor())
	awaitPowerSource(d, "ac")
	onCtx, cancelOn := d.WaitCtx()
	defer cancelOn()
	if _, err := d.Client().UpdatePersistentWifiMode(onCtx, connect.NewRequest(&agentreplv1.UpdatePersistentWifiModeRequest{
		Action: &agentreplv1.UpdatePersistentWifiModeRequest_On{On: &agentreplv1.UpdatePersistentWifiModeOn{}},
	})); err != nil {
		t.Fatalf("UpdatePersistentWifiMode(on) error = %v", err)
	}

	t.Run("a lid closed on AC power quiets the machine and holds High Power Mode, recording the user's mode", func(t *testing.T) {
		// Act
		d.Lid.Close()

		// Assert
		closedCtx, cancelClosed := d.WaitCtx()
		defer cancelClosed()
		harness.AwaitView(t, closedCtx, stream, "the closed lid quieted the machine", func(r *agentreplv1.WatchDaemonResponse) bool {
			s := r.GetPersistentWifi()
			return s.GetLidClosed() != nil && s.GetQuiet().GetDisplayAsleep() != nil
		})
		if got, kept := harness.FakeEnergyModeIn(t, d.HostToolsDir), recordedEnergyMode(t, d); got != "2" || kept != harness.FakeEnergyMode {
			t.Fatalf("energy mode = %s, recorded %q, want High Power Mode held with the user's %s recorded", got, kept, harness.FakeEnergyMode)
		}
	})

	t.Run("losing AC power under the closed lid sets Low Power Mode", func(t *testing.T) {
		// Act
		d.PowerSource.Unplug()

		// Assert
		awaitEnergyRecord(d, "the lid-closed quiet state put the machine in Low Power Mode", "power_source_battery")
		if got, kept := harness.FakeEnergyModeIn(t, d.HostToolsDir), recordedEnergyMode(t, d); got != "1" || kept != harness.FakeEnergyMode {
			t.Fatalf("energy mode = %s, recorded %q, want Low Power Mode with the user's %s recorded", got, kept, harness.FakeEnergyMode)
		}
	})

	t.Run("regaining AC power under the closed lid raises High Power Mode and keeps the record", func(t *testing.T) {
		// Act
		d.PowerSource.PlugIn()

		// Assert
		awaitEnergyRecord(d, "the lid-closed quiet state put the machine in High Power Mode", "power_source_ac")
		if got, kept := harness.FakeEnergyModeIn(t, d.HostToolsDir), recordedEnergyMode(t, d); got != "2" || kept != harness.FakeEnergyMode {
			t.Fatalf("energy mode = %s, recorded %q, want High Power Mode raised with the user's %s still recorded", got, kept, harness.FakeEnergyMode)
		}
	})
}
