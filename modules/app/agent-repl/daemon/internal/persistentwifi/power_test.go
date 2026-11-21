package persistentwifi

import (
	"context"
	"errors"
	"strings"
	"testing"

	"agent-repld/internal/dlog"
)

// hearSource hands the controller one power source, as WatchPowerSource's
// heard does.
func (h *harness) hearSource(p PowerSource) {
	h.c.powerSourceHeard(context.Background(), heardPowerSource(p))
}

// energyInfos answers the energy step's INFO records carrying msg.
func energyInfos(h *harness, msg string) []dlog.Record {
	return dlog.FilterRecords(h.log.Records(), dlog.RecordQuery{Level: dlog.LevelInfo, Operation: opEnergy, Message: msg})
}

const (
	msgLowered = "the lid-closed quiet state put the machine in Low Power Mode"
	msgRestore = "the recorded energy mode was restored"
	msgRaised  = "the lid-closed quiet state put the machine in High Power Mode"
)

func TestEnergyWanted(t *testing.T) {
	cases := []struct {
		name        string
		quiet       bool
		lid         lidKind
		source      powerSourceKind
		want        energyHold
		wantBecause string
	}{
		{name: "an unheard lid writes nothing", lid: lidUnheard, source: powerSourceBattery, want: energyKeep,
			wantBecause: "the lid is not yet heard"},
		{name: "an open lid restores", lid: lidOpen, source: powerSourceBattery, want: energyRestore,
			wantBecause: "the lid-closed quiet state does not stand"},
		{name: "a closed lid with the mode off restores", lid: lidClosed, source: powerSourceBattery, want: energyRestore,
			wantBecause: "the lid-closed quiet state does not stand"},
		{name: "the quiet state on battery lowers", quiet: true, lid: lidClosed, source: powerSourceBattery, want: energyLower,
			wantBecause: "the lid is closed on battery power"},
		{name: "the quiet state on AC raises", quiet: true, lid: lidClosed, source: powerSourceAC, want: energyRaise,
			wantBecause: "the lid is closed on AC power"},
		{name: "the quiet state with the power source unheard writes nothing", quiet: true, lid: lidClosed, source: powerSourceUnheard,
			want: energyKeep, wantBecause: "the power source is not yet heard"},
		{name: "the quiet state with the power source unwatched restores", quiet: true, lid: lidClosed, source: powerSourceUnwatched,
			want: energyRestore, wantBecause: "the power source is not heard"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got, because := energyWanted(tc.quiet, tc.lid, tc.source)

			// Assert
			if got != tc.want || because != tc.wantBecause {
				t.Fatalf("energyWanted() = %v, %q, want %v, %q", got, because, tc.want, tc.wantBecause)
			}
		})
	}
}

func TestEnergyWantedPanicsOnAnUnnamedPowerSource(t *testing.T) {
	// Assert
	defer func() {
		if recover() == nil {
			t.Fatal("energyWanted() did not panic on an unnamed power source")
		}
	}()

	// Act
	energyWanted(true, lidClosed, powerSourceKind(99))
}

func TestALidClosedOnACPowerSetsHighPowerMode(t *testing.T) {
	cases := []struct {
		name       string
		energy     string // the mode in use when the lid closes
		wantWrites int
		wantArm    string
	}{
		{name: "the user's automatic mode is recorded and High Power Mode written", energy: "0", wantWrites: 1, wantArm: "high_power_held"},
		{name: "High Power Mode already in use is recorded and nothing written", energy: energyModeHighPower, wantWrites: 0, wantArm: "unchanged"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := closedHarness(t, tc.energy).withPowerSource(powerSourceAC)
			h.r.on(argPowerMode(energyModeHighPower), answer{})

			// Act
			h.hear(LidClosed)

			// Assert
			if !h.c.quiet {
				t.Fatal("the quiet state does not stand, want it entered on AC power")
			}
			if n := powerModeWrites(h); n != tc.wantWrites || h.r.count(argLowPower) != 0 || h.energy.kept() != tc.energy {
				t.Fatalf("calls = %v, record %q, want %d High Power writes, no Low Power write and the user's mode recorded", h.r.ran(), h.energy.kept(), tc.wantWrites)
			}
			entered := h.records(dlog.LevelInfo, opQuiet)
			if len(entered) != 1 || entered[0].Context["energy"] != tc.wantArm || entered[0].Context["power_source"] != "ac" {
				t.Fatalf("quiet INFO records = %+v, want the entry naming AC power and energy %s", entered, tc.wantArm)
			}
			if raised := energyInfos(h, msgRaised); (len(raised) == 1) != (tc.wantWrites == 1) {
				t.Fatalf("raised INFO records = %+v, want one only when High Power Mode was written", raised)
			}
		})
	}
}

func TestALidClosedOnBatteryRecordsTheCauseOfTheLowPowerWrite(t *testing.T) {
	// Arrange
	h := closedHarness(t, energyModeHighPower)
	h.r.on(argLowPower, answer{})

	// Act
	h.hear(LidClosed)

	// Assert
	lowered := energyInfos(h, msgLowered)
	if len(lowered) != 1 || lowered[0].Context["trigger"] != "lid_closed" ||
		lowered[0].Context["because"] != "the lid is closed on battery power" || lowered[0].Context["power_source"] != "battery" {
		t.Fatalf("energy INFO records = %+v, want the Low Power write on the closed lid with its cause", h.records(dlog.LevelInfo, opEnergy))
	}
}

func TestLosingACPowerUnderAClosedLidSetsLowPowerMode(t *testing.T) {
	// Arrange: the quiet state stands on AC power with High Power Mode.
	h := closedHarness(t, energyModeHighPower).withPowerSource(powerSourceAC)
	h.r.on(argLowPower, answer{})
	h.hear(LidClosed)

	// Act
	h.hearSource(PowerBattery)

	// Assert
	if h.r.count(argLowPower) != 1 || h.energy.kept() != energyModeHighPower {
		t.Fatalf("calls = %v, record %q: want High Power Mode recorded and Low Power written once", h.r.ran(), h.energy.kept())
	}
	lowered := energyInfos(h, msgLowered)
	if len(lowered) != 1 || lowered[0].Context["trigger"] != "power_source_battery" ||
		lowered[0].Context["because"] != "the lid is closed on battery power" {
		t.Fatalf("energy INFO records = %+v, want the Low Power write on the lost AC power with its cause", h.records(dlog.LevelInfo, opEnergy))
	}
	changed := h.records(dlog.LevelInfo, opPowerSource)
	if len(changed) != 1 || changed[0].Context["was"] != "ac" || changed[0].Context["power_source"] != "battery" {
		t.Fatalf("power source INFO records = %+v, want one change from ac to battery", changed)
	}
	if _, ok := changed[0].Context["lock_wait_ms"]; !ok {
		t.Fatalf("power source change record %+v names no lock wait", changed[0])
	}
}

func TestRegainingACPowerUnderAClosedLidSetsHighPowerKeepingTheRecord(t *testing.T) {
	// Arrange: the quiet state stands on battery, automatic mode recorded.
	h := closedHarness(t, "0")
	h.r.on(argLowPower, answer{})
	h.hear(LidClosed)
	h.r.on(argPmsetRead, answer{out: pmsetIn(true, energyModeLowPower)})
	h.r.on(argPowerMode(energyModeHighPower), answer{})

	// Act
	h.hearSource(PowerAC)

	// Assert
	if !h.c.quiet {
		t.Fatal("the quiet state was left, want it standing under the closed lid")
	}
	if h.r.count(argPowerMode(energyModeHighPower)) != 1 || h.energy.kept() != "0" || h.energy.clears != 0 {
		t.Fatalf("calls = %v, record %q, clears %d: want High Power Mode written and the user's mode still recorded", h.r.ran(), h.energy.kept(), h.energy.clears)
	}
	raised := energyInfos(h, msgRaised)
	if len(raised) != 1 || raised[0].Context["trigger"] != "power_source_ac" ||
		raised[0].Context["because"] != "the lid is closed on AC power" || raised[0].Context["power_source"] != "ac" {
		t.Fatalf("energy INFO records = %+v, want the High Power write on the regained AC power with its cause", h.records(dlog.LevelInfo, opEnergy))
	}
}

func TestPowerCyclingUnderAClosedLidKeepsTheOriginalRecord(t *testing.T) {
	// Arrange: lowered on battery, then raised on AC.
	h := closedHarness(t, "0")
	h.r.on(argLowPower, answer{})
	h.hear(LidClosed)
	h.r.on(argPmsetRead, answer{out: pmsetIn(true, energyModeLowPower)}, answer{out: pmsetIn(true, energyModeHighPower)})
	h.r.on(argPowerMode(energyModeHighPower), answer{})
	h.hearSource(PowerAC)

	// Act
	h.hearSource(PowerBattery)

	// Assert: the user's mode, not an in-between one, is what stays recorded.
	if h.r.count(argLowPower) != 2 || h.energy.kept() != "0" || h.energy.saves != 1 {
		t.Fatalf("calls = %v, record %q after %d saves: want the automatic mode recorded once and Low Power written again",
			h.r.ran(), h.energy.kept(), h.energy.saves)
	}
}

func TestOpeningTheLidOnACPowerWithHighPowerAlreadyInUseWritesNothing(t *testing.T) {
	// Arrange: High Power Mode was the user's own, so entering recorded it.
	h := closedHarness(t, energyModeHighPower).withPowerSource(powerSourceAC)
	h.hear(LidClosed)

	// Act
	h.hear(LidOpen)

	// Assert
	if h.c.quiet || powerModeWrites(h) != 0 || h.energy.saves != 1 || h.energy.clears != 1 || h.energy.kept() != "" {
		t.Fatalf("quiet %v, calls %v, saves %d, clears %d: want the quiet state left, the record forgotten and nothing written",
			h.c.quiet, h.r.ran(), h.energy.saves, h.energy.clears)
	}
}

func TestOpeningTheLidOnACPowerRestoresTheModeBeforeHighPower(t *testing.T) {
	// Arrange: High Power Mode was set over the user's automatic mode.
	h := closedHarness(t, "0").withPowerSource(powerSourceAC)
	h.r.on(argPowerMode(energyModeHighPower), answer{})
	h.hear(LidClosed)
	h.r.on(argPmsetRead, answer{out: pmsetIn(true, energyModeHighPower)})
	h.r.on(argPowerMode("0"), answer{})

	// Act
	h.hear(LidOpen)

	// Assert
	if h.c.quiet || h.r.count(argPowerMode("0")) != 1 || h.energy.kept() != "" {
		t.Fatalf("quiet %v, calls %v, record %q: want the automatic mode restored and forgotten", h.c.quiet, h.r.ran(), h.energy.kept())
	}
}

func TestAPowerSourceHeardWithTheLidOpenNeverWritesTheEnergyMode(t *testing.T) {
	cases := []struct {
		name   string
		source PowerSource
	}{
		{name: "AC power", source: PowerAC},
		{name: "battery power", source: PowerBattery},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t).standing(pmsetIn(true, energyModeHighPower), summary(true, "Home")).withLid(lidOpen)

			// Act
			h.hearSource(tc.source)

			// Assert
			if n := powerModeWrites(h); n != 0 || h.energy.saves != 0 {
				t.Fatalf("calls = %v, saves = %d, want no energy mode written with the lid open", h.r.ran(), h.energy.saves)
			}
		})
	}
}

func TestAClosedLidWithThePowerSourceUnheardWritesNothingUntilItIsHeard(t *testing.T) {
	// Arrange
	h := closedHarness(t, energyModeHighPower).withPowerSource(powerSourceUnheard)
	h.r.on(argLowPower, answer{})
	h.hear(LidClosed)
	if n := powerModeWrites(h); n != 0 || h.energy.saves != 0 {
		t.Fatalf("calls = %v, saves = %d, want nothing written before the power source is heard", h.r.ran(), h.energy.saves)
	}

	// Act
	h.hearSource(PowerBattery)

	// Assert
	if h.r.count(argLowPower) != 1 {
		t.Fatalf("calls = %v, want Low Power written once the battery is heard", h.r.ran())
	}
}

func TestARestartUnderAClosedLidKeepsTheRecordedState(t *testing.T) {
	cases := []struct {
		name        string
		energy      string // the mode in use the successor reads
		source      PowerSource
		wantRestore bool
		wantKept    string
	}{
		{name: "on battery the earlier Low Power Mode is adopted with nothing written", energy: energyModeLowPower,
			source: PowerBattery, wantKept: energyModeHighPower},
		{name: "on AC power High Power Mode is written over the earlier Low Power Mode", energy: energyModeLowPower,
			source: PowerAC, wantRestore: true, wantKept: energyModeHighPower},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: an earlier daemon recorded High Power Mode and lowered;
			// this one boots with nothing heard and beats before the lid.
			h := closedHarness(t, tc.energy).withPowerSource(powerSourceUnheard)
			h.energy.rec = &EnergyRecord{Mode: energyModeHighPower}
			h.r.on(argPowerMode(energyModeHighPower), answer{})
			h.c.tick(context.Background())

			// Act
			h.hear(LidClosed)
			h.hearSource(tc.source)

			// Assert
			if got := h.r.count(argPowerMode(energyModeHighPower)) == 1; got != tc.wantRestore || h.r.count(argLowPower) != 0 {
				t.Fatalf("calls = %v, want High Power write %v and no Low Power write", h.r.ran(), tc.wantRestore)
			}
			if h.energy.saves != 0 || h.energy.kept() != tc.wantKept {
				t.Fatalf("record %q after %d saves, want %q never overwritten", h.energy.kept(), h.energy.saves, tc.wantKept)
			}
		})
	}
}

func TestABeatBeforeTheLidIsHeardNeverRestoresAKeptRecord(t *testing.T) {
	// Arrange: a successor booting under a closed lid, before its watch
	// hears it.
	h := closedHarness(t, energyModeLowPower)
	h.energy.rec = &EnergyRecord{Mode: energyModeHighPower}

	// Act
	h.c.tick(context.Background())

	// Assert
	if n := powerModeWrites(h); n != 0 || h.energy.kept() != energyModeHighPower {
		t.Fatalf("calls = %v, record %q: want nothing written and the record kept", h.r.ran(), h.energy.kept())
	}
}

func TestWatchPowerSourceHandsEveryHeardSourceToTheController(t *testing.T) {
	// Arrange
	h := newHarness(t).standing(pmsetOff, summary(true, "Home")).withPowerSource(powerSourceUnheard)
	done := make(chan error, 1)
	go func() { done <- h.c.WatchPowerSource(context.Background()) }()

	// Act
	h.source.sources <- PowerAC
	h.source.sources <- PowerBattery
	close(h.source.sources)
	<-done

	// Assert
	var heard []string
	for _, r := range h.records(dlog.LevelInfo, opPowerSource) {
		heard = append(heard, r.Context["power_source"].(string))
	}
	if strings.Join(heard, ",") != "ac,battery" || h.c.powerSource.kind != powerSourceUnwatched {
		t.Fatalf("heard %v, then %v: want ac, battery, then unwatched once the watch ended", heard, h.c.powerSource)
	}
}

func TestAnUnwatchedPowerSourceRestoresTheRecordedEnergyMode(t *testing.T) {
	// Arrange: the quiet state stands on battery, High Power Mode recorded,
	// when the power source's watch fails.
	h := closedHarness(t, energyModeHighPower)
	h.r.on(argLowPower, answer{})
	h.hear(LidClosed)
	h.r.on(argPmsetRead, answer{out: pmsetIn(true, energyModeLowPower)})
	h.r.on(argPowerMode(energyModeHighPower), answer{})
	h.source.end = errors.New("IOPSCreateLimitedPowerNotification answered nothing")
	close(h.source.sources)

	// Act
	err := h.c.WatchPowerSource(context.Background())

	// Assert
	if err != nil {
		t.Fatalf("WatchPowerSource() = %v, want nil: the daemon runs on without the power source", err)
	}
	if !h.c.quiet || h.r.count(argPowerMode(energyModeHighPower)) != 1 || h.energy.kept() != "" {
		t.Fatalf("quiet %v, calls %v, record %q: want the quiet state kept, High Power Mode restored and forgotten",
			h.c.quiet, h.r.ran(), h.energy.kept())
	}
	errs := h.records(dlog.LevelError, opPowerSource)
	if len(errs) != 1 || errs[0].Context["was"] != "battery" || !strings.Contains(errs[0].Context["cause"].(string), "IOPSCreateLimitedPowerNotification") {
		t.Fatalf("power source ERROR records = %+v, want one carrying the cause", errs)
	}
	restored := energyInfos(h, msgRestore)
	if len(restored) != 1 || restored[0].Context["trigger"] != "power_source_unwatched" ||
		restored[0].Context["because"] != "the power source is not heard" {
		t.Fatalf("energy INFO records = %+v, want the restore on the unwatched power source", h.records(dlog.LevelInfo, opEnergy))
	}
}

func TestWatchPowerSourceEndedByItsContextRecordsNothing(t *testing.T) {
	// Arrange
	h := newHarness(t).withPowerSource(powerSourceUnheard)
	ctx, cancel := context.WithCancel(context.Background())
	cancel()

	// Act
	err := h.c.WatchPowerSource(ctx)

	// Assert
	if err != nil || len(h.records(dlog.LevelError, opPowerSource)) != 0 || h.c.powerSource.kind != powerSourceUnheard {
		t.Fatalf("WatchPowerSource() = %v with %v and records %+v, want a quiet end", err, h.c.powerSource, h.log.Records())
	}
}

func TestPowerSourceNamesAndFacts(t *testing.T) {
	cases := []struct {
		source PowerSource
		name   string
		kind   powerSourceKind
	}{
		{source: PowerAC, name: "ac", kind: powerSourceAC},
		{source: PowerBattery, name: "battery", kind: powerSourceBattery},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			fact := heardPowerSource(tc.source)
			parsed, err := parsePowerSourceLine(tc.name)

			// Assert
			if tc.source.String() != tc.name || fact.kind != tc.kind || fact.name() != tc.name || err != nil || parsed != tc.source {
				t.Fatalf("%v names %q, fact %v, parsed %v, %v; want %q", tc.source, tc.source.String(), fact, parsed, err, tc.name)
			}
		})
	}
}

func TestPowerSourceFactNamesTheUnheardAndTheUnwatched(t *testing.T) {
	cases := []struct {
		fact powerSourceFact
		want string
	}{
		{fact: powerSourceFact{}, want: "unheard"},
		{fact: powerSourceFact{kind: powerSourceUnwatched, cause: "gone"}, want: "unwatched"},
	}
	for _, tc := range cases {
		t.Run(tc.want, func(t *testing.T) {
			// Act, Assert
			if got := tc.fact.name(); got != tc.want {
				t.Fatalf("name() = %q, want %q", got, tc.want)
			}
		})
	}
}

func TestAnUnnamedPowerSourcePanics(t *testing.T) {
	cases := []struct {
		name string
		act  func()
	}{
		{name: "its name", act: func() { _ = PowerSource(9).String() }},
		{name: "its fact", act: func() { heardPowerSource(PowerSource(9)) }},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Assert
			defer func() {
				if recover() == nil {
					t.Fatal("did not panic on an unnamed power source")
				}
			}()

			// Act
			tc.act()
		})
	}
}

func TestPowerSourceOf(t *testing.T) {
	// Act, Assert
	if powerSourceOf(true) != PowerBattery || powerSourceOf(false) != PowerAC {
		t.Fatalf("powerSourceOf(true), powerSourceOf(false) = %v, %v, want battery, ac", powerSourceOf(true), powerSourceOf(false))
	}
}

func TestSocketPowerSourceHearsEachLine(t *testing.T) {
	// Arrange
	path, _ := lineServer(t, "ac\nbattery\nbogus\n", false)
	var heard []PowerSource

	// Act
	err := SocketPowerSource(path).Watch(context.Background(), func(p PowerSource) { heard = append(heard, p) })

	// Assert
	if len(heard) != 2 || heard[0] != PowerAC || heard[1] != PowerBattery {
		t.Fatalf("heard = %v, want ac, battery", heard)
	}
	if err == nil || !strings.Contains(err.Error(), `"bogus" names no power source state`) {
		t.Fatalf("Watch() = %v, want the unnamed line refused", err)
	}
}

func TestSocketPowerSourceRefusesAMissingSocketNamingThePowerSource(t *testing.T) {
	// Act
	err := SocketPowerSource("/tmp/pwl-absent-power").Watch(context.Background(), func(PowerSource) {})

	// Assert
	if err == nil || !strings.Contains(err.Error(), "connect to the power source socket") {
		t.Fatalf("Watch() = %v, want the connection refused", err)
	}
}

func TestFixedPowerSourceIsHeardOnceUntilItsContextEnds(t *testing.T) {
	// Arrange
	ctx, cancel := context.WithCancel(context.Background())
	var heard []PowerSource

	// Act
	err := FixedPowerSource(PowerBattery).Watch(ctx, func(p PowerSource) { heard = append(heard, p); cancel() })

	// Assert
	if err != nil || len(heard) != 1 || heard[0] != PowerBattery {
		t.Fatalf("Watch() = %v having heard %v, want battery once and nil", err, heard)
	}
}

func TestConfigPowerSourcePicksTheWatcherItsValueNames(t *testing.T) {
	cases := []struct {
		fake string
		want PowerSourceWatcher
	}{
		{fake: "", want: SystemPowerSource()},
		{fake: "battery", want: FixedPowerSource(PowerBattery)},
		{fake: "/tmp/power", want: SocketPowerSource("/tmp/power")},
	}
	for _, tc := range cases {
		t.Run(tc.fake, func(t *testing.T) {
			// Act, Assert
			if got := (Config{FakePowerSource: tc.fake}).PowerSource(); got != tc.want {
				t.Fatalf("PowerSource() = %#v, want %#v", got, tc.want)
			}
		})
	}
}
