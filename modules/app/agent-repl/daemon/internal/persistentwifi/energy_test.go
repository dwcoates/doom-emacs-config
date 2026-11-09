package persistentwifi

import (
	"context"
	"errors"
	"testing"

	"agent-repld/internal/dlog"
)

func TestParsePowerSource(t *testing.T) {
	cases := []struct {
		name           string
		out            string
		wantAC, wantOK bool
	}{
		{name: "AC power", out: onAC, wantAC: true, wantOK: true},
		{name: "battery power", out: onBattery, wantOK: true},
		{name: "a UPS is not AC", out: "Now drawing from 'UPS Power'\n", wantOK: true},
		{name: "no source line", out: "garbage\n"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			ac, ok := parsePowerSource(tc.out)
			if ac != tc.wantAC || ok != tc.wantOK {
				t.Fatalf("parsePowerSource() = (%v, %v), want (%v, %v)", ac, ok, tc.wantAC, tc.wantOK)
			}
		})
	}
}

func TestParseEnergyMode(t *testing.T) {
	cases := []struct{ name, out, want string }{
		{name: "low", out: pmsetIn(true, "1"), want: "1"},
		{name: "high", out: pmsetIn(true, "2"), want: "2"},
		{name: "none named", out: pmsetIn(true, ""), want: ""},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			if got := parseEnergyMode(tc.out); got != tc.want {
				t.Fatalf("parseEnergyMode() = %q, want %q", got, tc.want)
			}
		})
	}
}

func TestEnergyModeFor(t *testing.T) {
	cases := []struct {
		name    string
		on      bool
		lid     lidKind
		source  string
		want    string
		wantErr bool
		reads   int
	}{
		{name: "mode off is high", on: false, lid: lidClosed, want: energyModeHighPower},
		{name: "open lid on AC is low", on: true, lid: lidOpen, want: energyModeLowPower},
		{name: "unheard lid is low", on: true, lid: lidUnheard, want: energyModeLowPower},
		{name: "closed lid on AC is high", on: true, lid: lidClosed, source: onAC, want: energyModeHighPower, reads: 1},
		{name: "closed lid on battery is low", on: true, lid: lidClosed, source: onBattery, want: energyModeLowPower, reads: 1},
		{name: "closed lid with an unreadable source errors", on: true, lid: lidClosed, source: "garbage\n", wantErr: true, reads: 1},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			h := newHarness(t)
			h.c.lid = lidFact{kind: tc.lid}
			if tc.source != "" {
				h.r.on(argPowerSource, answer{out: tc.source})
			}

			got, err := h.c.energyModeFor(context.Background(), tc.on)

			if (err != nil) != tc.wantErr || got != tc.want {
				t.Fatalf("energyModeFor() = (%q, %v), want (%q, err %v)", got, err, tc.want, tc.wantErr)
			}
			if n := h.r.count(argPowerSource); n != tc.reads {
				t.Fatalf("power source reads = %d, want %d", n, tc.reads)
			}
		})
	}
}

func TestEnergyModeForReportsAToolFailure(t *testing.T) {
	h := newHarness(t).withLid(lidClosed)
	h.r.on(argPowerSource, answer{err: errors.New("boom")})

	_, err := h.c.energyModeFor(context.Background(), true)

	if err == nil {
		t.Fatal("energyModeFor() error = nil, want the unreadable source")
	}
}

func TestUpdateOnWithTheLidClosedOnACRunsHighPower(t *testing.T) {
	h := newHarness(t).standing(pmsetOff, summary(true, hotspot)).withLid(lidClosed)
	h.r.on(argPowerSource, answer{out: onAC})
	scriptPower(h.r, "1")
	h.r.on(argEnergy(false), answer{})
	h.r.on(argBrightnessRead, answer{out: "0.5"})
	h.r.on(argLidDim, answer{})

	resp := h.c.Update(context.Background(), update("on"))

	s := resp.GetSuccess()
	if s == nil || s.GetEnergyMode().GetHighPower() == nil {
		t.Fatalf("Update() = %v, want success with high power", resp)
	}
	if h.r.count(argEnergy(false)) != 1 || h.r.count(argEnergy(true)) != 0 {
		t.Fatalf("calls = %v, want one high power write and no low power one", h.r.ran())
	}
}

// reconcileHarness is a mode-on standing with the energy mode in use.
func reconcileHarness(t *testing.T, energy string, lid lidKind) *harness {
	t.Helper()
	h := newHarness(t).standing(pmsetIn(true, energy), summary(true, "Home")).withLid(lid)
	return h
}

func TestReconcileEnergyFollowsTheLidAndThePowerSource(t *testing.T) {
	cases := []struct {
		name   string
		energy string
		lid    lidKind
		source string
		want   string // the powermode written, empty for none
	}{
		{name: "closed on AC in low moves to high", energy: energyModeLowPower, lid: lidClosed, source: onAC, want: energyModeHighPower},
		{name: "closed on AC already high is left", energy: energyModeHighPower, lid: lidClosed, source: onAC},
		{name: "closed unplugged in high reverts to low", energy: energyModeHighPower, lid: lidClosed, source: onBattery, want: energyModeLowPower},
		{name: "closed on battery already low is left", energy: energyModeLowPower, lid: lidClosed, source: onBattery},
		{name: "opened in high reverts to low", energy: energyModeHighPower, lid: lidOpen, want: energyModeLowPower},
		{name: "open already low is left", energy: energyModeLowPower, lid: lidOpen},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			h := reconcileHarness(t, tc.energy, tc.lid)
			if tc.source != "" {
				h.r.on(argPowerSource, answer{out: tc.source})
			}
			if tc.want != "" {
				h.r.on(argPower("powermode", tc.want), answer{})
			}
			h.dims.level = ptr(0.5) // a dim in force keeps the display step quiet
			h.r.on(argBrightnessRead, answer{out: "0.5"})
			h.r.on(argBrightness("0.5"), answer{})
			rd := h.c.readLocked(context.Background())

			h.c.reconcileEnergyLocked(context.Background(), rd, "beat")

			writes := h.r.count(argPower("powermode", energyModeLowPower)) + h.r.count(argPower("powermode", energyModeHighPower))
			if tc.want == "" && writes != 0 || tc.want != "" && h.r.count(argPower("powermode", tc.want)) != 1 {
				t.Fatalf("calls = %v, want powermode %q written (none if empty)", h.r.ran(), tc.want)
			}
		})
	}
}

func TestReconcileEnergyLeavesAMachineWithNoEnergyModeAlone(t *testing.T) {
	h := reconcileHarness(t, "", lidClosed)
	rd := h.c.readLocked(context.Background())

	h.c.reconcileEnergyLocked(context.Background(), rd, "beat")

	if h.r.count(argPowerSource) != 0 || len(h.records("error", opEnergy)) != 0 {
		t.Fatalf("calls = %v, want the power source unread", h.r.ran())
	}
}

func TestReconcileEnergyLeavesAModeThatIsOffAlone(t *testing.T) {
	h := newHarness(t).standing(pmsetIn(false, energyModeLowPower), summary(true, "Home")).withLid(lidClosed)
	rd := h.c.readLocked(context.Background())

	h.c.reconcileEnergyLocked(context.Background(), rd, "beat")

	if h.r.count(argPowerSource) != 0 {
		t.Fatalf("calls = %v, want nothing run for the energy mode", h.r.ran())
	}
}

func TestReconcileEnergyRecordsAFailureOncePerCause(t *testing.T) {
	h := reconcileHarness(t, energyModeLowPower, lidClosed)
	h.r.on(argPowerSource, answer{out: onAC})
	h.r.on(argPower("powermode", energyModeHighPower), answer{out: "sudo: a password is required", code: 1})
	rd := h.c.readLocked(context.Background())

	h.c.reconcileEnergyLocked(context.Background(), rd, "beat")
	h.c.reconcileEnergyLocked(context.Background(), rd, "beat")

	errs := h.records(dlog.LevelError, opEnergy)
	if len(errs) != 1 || errs[0].Context["energy"] != energyModeLowPower || errs[0].Context["lid"] != "closed" {
		t.Fatalf("energy ERROR records = %+v, want one carrying the lid and energy mode", errs)
	}
	if h.r.count(argPower("powermode", energyModeHighPower)) != 2 {
		t.Fatalf("calls = %v, want the write retried on the second beat", h.r.ran())
	}
}

func TestReconcileEnergyRecordsAnUnreadablePowerSource(t *testing.T) {
	h := reconcileHarness(t, energyModeLowPower, lidClosed)
	h.r.on(argPowerSource, answer{out: "garbage\n"})
	rd := h.c.readLocked(context.Background())

	h.c.reconcileEnergyLocked(context.Background(), rd, "lid_closed")

	errs := h.records(dlog.LevelError, opEnergy)
	if len(errs) != 1 || errs[0].Context["trigger"] != "lid_closed" {
		t.Fatalf("energy ERROR records = %+v, want one for the unreadable source", errs)
	}
}

func TestATickMovesTheEnergyModeWhenTheLidIsClosedAndPluggedIn(t *testing.T) {
	h := reconcileHarness(t, energyModeLowPower, lidClosed)
	h.r.on(argPowerSource, answer{out: onAC})
	h.r.on(argPower("powermode", energyModeHighPower), answer{})

	h.c.tick(context.Background())

	if h.r.count(argPower("powermode", energyModeHighPower)) != 1 {
		t.Fatalf("calls = %v, want one high power write", h.r.ran())
	}
}

func TestALidCloseMovesTheEnergyModeWhenPluggedIn(t *testing.T) {
	h := newHarness(t).standing(pmsetIn(true, energyModeLowPower), summary(true, "Home"))
	h.r.on(argPowerSource, answer{out: onAC})
	h.r.on(argPower("powermode", energyModeHighPower), answer{})
	h.r.on(argBrightnessRead, answer{out: "0.5"})
	h.r.on(argLidDim, answer{})

	h.hear(LidClosed)

	if h.r.count(argPower("powermode", energyModeHighPower)) != 1 {
		t.Fatalf("calls = %v, want one high power write", h.r.ran())
	}
}

func TestALidOpenMovesTheEnergyModeBackToLow(t *testing.T) {
	h := newHarness(t).standing(pmsetIn(true, energyModeHighPower), summary(true, "Home"))
	h.dims.level = ptr(0.5)
	h.r.on(argBrightness("0.5"), answer{})
	h.r.on(argPower("powermode", energyModeLowPower), answer{})

	h.hear(LidOpen)

	if h.r.count(argPower("powermode", energyModeLowPower)) != 1 {
		t.Fatalf("calls = %v, want one low power write", h.r.ran())
	}
}
