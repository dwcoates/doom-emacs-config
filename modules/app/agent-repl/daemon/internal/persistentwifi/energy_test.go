package persistentwifi

import (
	"context"
	"errors"
	"slices"
	"testing"

	"agent-repld/internal/dlog"
)

func TestDecideRestore(t *testing.T) {
	cases := []struct {
		name, energy, kept string
		want               restoreAction
	}{
		{name: "an unread mode waits", energy: "", kept: "2", want: restoreWait},
		{name: "the recorded mode already in use is forgotten", energy: "2", kept: "2", want: restoreAlready},
		{name: "a recorded Low Power Mode in use is forgotten", energy: "1", kept: "1", want: restoreAlready},
		{name: "a mode the user chose since stands", energy: "0", kept: "2", want: restoreUserMoved},
		{name: "the quiet state's Low Power Mode is restored", energy: "1", kept: "2", want: restoreWrite},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			if got := decideRestore(tc.energy, tc.kept, energyModeLowPower); got != tc.want {
				t.Fatalf("decideRestore(%q, %q) = %v, want %v", tc.energy, tc.kept, got, tc.want)
			}
		})
	}
}

// closedHarness is a mode-on machine running energy, about to hear its lid
// close: the display sleep it then asks for takes.
func closedHarness(t *testing.T, energy string) *harness {
	t.Helper()
	h := newHarness(t).standing(pmsetIn(true, energy), summary(true, "Home"))
	h.r.on(argDisplaySleep, answer{})
	h.r.on(argDisplayWake, answer{})
	return h
}

// powerModeWrites counts every `pmset -a powermode` write.
func powerModeWrites(h *harness) int {
	n := 0
	for _, mode := range []string{"0", energyModeLowPower, energyModeHighPower} {
		n += h.r.count(argPowerMode(mode))
	}
	return n
}

func TestEnteringTheQuietStateRecordsThenLowers(t *testing.T) {
	cases := []struct {
		name       string
		energy     string
		wantKept   string
		wantLowers int
	}{
		{name: "the user's High Power Mode is recorded, then lowered", energy: energyModeHighPower, wantKept: energyModeHighPower, wantLowers: 1},
		{name: "the user's Low Power Mode is recorded with nothing written", energy: energyModeLowPower, wantKept: energyModeLowPower},
		{name: "a machine that names no energy mode records nothing", energy: ""},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := closedHarness(t, tc.energy)
			h.r.on(argLowPower, answer{})

			// Act
			h.hear(LidClosed)

			// Assert
			if got := h.energy.kept(); got != tc.wantKept {
				t.Fatalf("recorded = %q, want %q", got, tc.wantKept)
			}
			if n := h.r.count(argLowPower); n != tc.wantLowers {
				t.Fatalf("calls = %v, want %d Low Power writes", h.r.ran(), tc.wantLowers)
			}
			if tc.wantLowers > 0 && h.energy.savedAfter[0] > slices.Index(h.r.ran(), argLowPower) {
				t.Fatalf("record saved after %d runs, Low Power written at %d: want the record first", h.energy.savedAfter[0], slices.Index(h.r.ran(), argLowPower))
			}
			if errs := h.records(dlog.LevelError, opEnergy); len(errs) != 0 {
				t.Fatalf("energy ERROR records = %+v, want none", errs)
			}
		})
	}
}

func TestLeavingTheQuietStateRestoresThenForgets(t *testing.T) {
	cases := []struct {
		name        string
		kept        string
		energy      string // the mode in use when the lid opens
		wantRestore bool
		wantMessage string
	}{
		{name: "the recorded High Power Mode is restored", kept: energyModeHighPower, energy: energyModeLowPower, wantRestore: true,
			wantMessage: "the recorded energy mode was restored"},
		{name: "a recorded Low Power Mode is forgotten with nothing written", kept: energyModeLowPower, energy: energyModeLowPower,
			wantMessage: "the machine already runs the recorded energy mode; the record is forgotten"},
		{name: "a mode the user chose while closed stands", kept: energyModeHighPower, energy: "0",
			wantMessage: "the energy mode was changed after the lid-closed quiet state set Low Power Mode; that choice stands and the recorded mode is forgotten"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: the quiet state stands with tc.kept recorded.
			h := closedHarness(t, tc.kept)
			h.r.on(argLowPower, answer{})
			h.hear(LidClosed)
			h.r.on(argPmsetRead, answer{out: pmsetIn(true, tc.energy)})
			h.r.on(argPowerMode(tc.kept), answer{})
			before := len(h.r.ran())

			// Act
			h.hear(LidOpen)

			// Assert
			restores := 0
			for _, c := range h.r.ran()[before:] {
				if c == argPowerMode(tc.kept) {
					restores++
				}
			}
			if (restores == 1) != tc.wantRestore || restores > 1 {
				t.Fatalf("calls = %v, want restore %v", h.r.ran()[before:], tc.wantRestore)
			}
			if h.energy.kept() != "" || h.c.energyKept != "" {
				t.Fatalf("record = %q, held = %q, want both forgotten", h.energy.kept(), h.c.energyKept)
			}
			if tc.wantRestore && h.energy.clearedAfter[0] <= slices.Index(h.r.ran()[before:], argPowerMode(tc.kept))+before {
				t.Fatalf("cleared after %d runs, want after the restore", h.energy.clearedAfter[0])
			}
			infos := dlog.FilterRecords(h.log.Records(), dlog.RecordQuery{Level: dlog.LevelInfo, Operation: opEnergy, Message: tc.wantMessage})
			if len(infos) != 1 || infos[0].Context["trigger"] != "lid_open" {
				t.Fatalf("energy INFO records = %+v, want one %q", h.records(dlog.LevelInfo, opEnergy), tc.wantMessage)
			}
		})
	}
}

func TestABeatWithTheLidOpenNeverWritesTheEnergyMode(t *testing.T) {
	cases := []struct {
		name   string
		on     bool
		energy string
	}{
		{name: "the mode on in Low Power Mode", on: true, energy: energyModeLowPower},
		{name: "the mode on in High Power Mode", on: true, energy: energyModeHighPower},
		{name: "the mode off in Low Power Mode", on: false, energy: energyModeLowPower},
		{name: "the mode off in High Power Mode", on: false, energy: energyModeHighPower},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t).standing(pmsetIn(tc.on, tc.energy), summary(true, "Home")).withLid(lidOpen)

			// Act
			h.c.tick(context.Background())

			// Assert
			if n := powerModeWrites(h); n != 0 || h.energy.saves != 0 {
				t.Fatalf("calls = %v, saves = %d, want no energy mode written or recorded", h.r.ran(), h.energy.saves)
			}
		})
	}
}

func TestTheUsersHighPowerModeWithTheLidOpenStaysAcrossLidAndBeat(t *testing.T) {
	// Arrange: the user chose High Power Mode with the mode on and the lid
	// open (the owner's 2026-11-13 report: the next beat put it back to Low).
	h := newHarness(t).standing(pmsetIn(true, energyModeHighPower), summary(true, "Home"))

	// Act
	h.hear(LidOpen)
	h.c.tick(context.Background())

	// Assert
	if n := powerModeWrites(h); n != 0 {
		t.Fatalf("calls = %v, want the user's High Power Mode left alone", h.r.ran())
	}
}

func TestARestartWhileClosedAdoptsTheRecordAndRestoresOnOpen(t *testing.T) {
	// Arrange: an earlier daemon recorded High Power Mode and lowered; this
	// one boots with the lid closed.
	h := closedHarness(t, energyModeLowPower)
	h.energy.rec = &EnergyRecord{Mode: energyModeHighPower}
	h.r.on(argPowerMode(energyModeHighPower), answer{})
	h.hear(LidClosed)

	// Act
	h.hear(LidOpen)

	// Assert
	if h.energy.saves != 0 {
		t.Fatalf("saves = %d, want the earlier record adopted, never overwritten", h.energy.saves)
	}
	if h.r.count(argPowerMode(energyModeHighPower)) != 1 || h.r.count(argLowPower) != 0 {
		t.Fatalf("calls = %v, want the adopted High Power Mode restored and no Low Power write", h.r.ran())
	}
	if h.energy.kept() != "" {
		t.Fatalf("record = %q, want it forgotten after the restore", h.energy.kept())
	}
	adopted := dlog.FilterRecords(h.log.Records(), dlog.RecordQuery{Level: dlog.LevelInfo, Operation: opEnergy,
		Message: "the lid-closed quiet state adopted the recorded energy mode"})
	if len(adopted) != 1 || adopted[0].Context["recorded"] != energyModeHighPower {
		t.Fatalf("energy INFO records = %+v, want one adoption of High Power Mode", h.records(dlog.LevelInfo, opEnergy))
	}
}

func TestLeavingWithNoRecordRestoresNothing(t *testing.T) {
	// Arrange: the quiet state was entered by an earlier build, which kept
	// no record.
	h := closedHarness(t, energyModeLowPower).withLid(lidClosed)
	h.c.quiet = true

	// Act
	h.hear(LidOpen)

	// Assert
	if n := powerModeWrites(h); n != 0 || h.energy.clears != 0 {
		t.Fatalf("calls = %v, clears = %d, want nothing restored or cleared", h.r.ran(), h.energy.clears)
	}
	infos := dlog.FilterRecords(h.log.Records(), dlog.RecordQuery{Level: dlog.LevelInfo, Operation: opEnergy,
		Message: "the lid-closed quiet state was left with no energy mode recorded (Low Power Mode was never set: on AC power, or entered by an earlier build); nothing is restored"})
	if len(infos) != 1 || infos[0].Context["trigger"] != "lid_open" {
		t.Fatalf("energy INFO records = %+v, want the missing record noted once", h.records(dlog.LevelInfo, opEnergy))
	}
}

func TestACorruptRecordWritesNothing(t *testing.T) {
	cases := []struct {
		name    string
		quiet   bool // the quiet state stands before the lid is heard
		lid     Lid
		trigger string
		message string
	}{
		{name: "on entering", lid: LidClosed, trigger: "lid_closed",
			message: "the energy mode in use could not be recorded, so Low Power Mode was not set; it is tried again on the next beat"},
		{name: "on leaving", quiet: true, lid: LidOpen, trigger: "lid_open",
			message: "the recorded energy mode could not be read; nothing was restored, and it is tried again on the next beat"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := closedHarness(t, energyModeHighPower)
			h.energy.loadErr = errors.New("the recorded energy mode /s/energy-mode.json is not a record")
			h.c.quiet = tc.quiet
			if tc.quiet {
				h.c.lid = lidFact{kind: lidClosed}
			}

			// Act
			h.hear(tc.lid)

			// Assert
			if n := powerModeWrites(h); n != 0 || h.energy.saves != 0 || h.energy.clears != 0 || h.c.energyKept != "" {
				t.Fatalf("calls = %v, saves %d, clears %d, held %q: want nothing written, kept or cleared",
					h.r.ran(), h.energy.saves, h.energy.clears, h.c.energyKept)
			}
			errs := h.records(dlog.LevelError, opEnergy)
			if len(errs) != 1 || errs[0].Message != tc.message || errs[0].Context["trigger"] != tc.trigger ||
				errs[0].Context["cause"] != h.energy.loadErr.Error() {
				t.Fatalf("energy ERROR records = %+v, want one %q carrying the cause", errs, tc.message)
			}
		})
	}
}

func TestARecordThatCannotBeKeptLowersNothing(t *testing.T) {
	// Arrange
	h := closedHarness(t, energyModeHighPower)
	h.energy.saveErr = errors.New("keep the recorded energy mode: read-only file system")

	// Act
	h.hear(LidClosed)

	// Assert
	if n := powerModeWrites(h); n != 0 || h.c.energyKept != "" {
		t.Fatalf("calls = %v, held %q: want no Low Power write and nothing held", h.r.ran(), h.c.energyKept)
	}
	errs := h.records(dlog.LevelError, opEnergy)
	if len(errs) != 1 || errs[0].Context["cause"] != h.energy.saveErr.Error() {
		t.Fatalf("energy ERROR records = %+v, want one carrying the save's cause", errs)
	}
}

func TestAFailedLowerIsRetriedByTheBeat(t *testing.T) {
	// Arrange: the Low Power write is refused once, then takes.
	h := closedHarness(t, energyModeHighPower)
	h.r.on(argLowPower, answer{out: "sudo: a password is required", code: 1}, answer{})
	h.hear(LidClosed)

	// Act
	h.c.tick(context.Background())

	// Assert
	if h.r.count(argLowPower) != 2 {
		t.Fatalf("calls = %v, want the Low Power write retried on the beat", h.r.ran())
	}
	if h.energy.kept() != energyModeHighPower || h.energy.saves != 1 {
		t.Fatalf("record = %q after %d saves, want High Power Mode recorded once", h.energy.kept(), h.energy.saves)
	}
	errs := h.records(dlog.LevelError, opEnergy)
	if len(errs) != 1 || errs[0].Context["trigger"] != "lid_closed" || h.c.energyCause != "" {
		t.Fatalf("energy ERROR records = %+v, cause %q: want the refusal once and nothing standing after the retry", errs, h.c.energyCause)
	}
}

func TestAFailedLowerIsRecordedOncePerCause(t *testing.T) {
	// Arrange
	h := closedHarness(t, energyModeHighPower)
	h.r.on(argLowPower, answer{out: "sudo: a password is required", code: 1})
	h.hear(LidClosed)

	// Act
	h.c.tick(context.Background())

	// Assert
	if h.r.count(argLowPower) != 2 {
		t.Fatalf("calls = %v, want the write tried on the lid and the beat", h.r.ran())
	}
	if errs := h.records(dlog.LevelError, opEnergy); len(errs) != 1 {
		t.Fatalf("energy ERROR records = %+v, want one for the one cause", errs)
	}
}

func TestAFailedRestoreKeepsTheRecordAndIsRetriedByTheBeat(t *testing.T) {
	// Arrange: the quiet state stands with High Power Mode recorded; its
	// restore is refused once, then takes.
	h := closedHarness(t, energyModeHighPower)
	h.r.on(argLowPower, answer{})
	h.hear(LidClosed)
	h.r.on(argPmsetRead, answer{out: pmsetIn(true, energyModeLowPower)})
	h.r.on(argPowerMode(energyModeHighPower), answer{out: "sudo: a password is required", code: 1}, answer{})
	h.hear(LidOpen)
	if h.energy.kept() != energyModeHighPower {
		t.Fatalf("record = %q after the failed restore, want High Power Mode kept", h.energy.kept())
	}

	// Act
	h.c.tick(context.Background())

	// Assert
	if h.r.count(argPowerMode(energyModeHighPower)) != 2 || h.energy.kept() != "" {
		t.Fatalf("calls = %v, record %q: want the restore retried and the record then forgotten", h.r.ran(), h.energy.kept())
	}
	errs := h.records(dlog.LevelError, opEnergy)
	if len(errs) != 1 || errs[0].Message != "the recorded energy mode could not be restored; the record is kept and the restore tried again on the next beat" {
		t.Fatalf("energy ERROR records = %+v, want the failed restore once", errs)
	}
}

func TestARecordThatCannotBeClearedIsClearedOnTheNextBeat(t *testing.T) {
	// Arrange: the restore takes, the record's clear fails once.
	h := closedHarness(t, energyModeHighPower)
	h.r.on(argLowPower, answer{})
	h.hear(LidClosed)
	h.r.on(argPmsetRead, answer{out: pmsetIn(true, energyModeLowPower)}, answer{out: pmsetIn(true, energyModeHighPower)})
	h.r.on(argPowerMode(energyModeHighPower), answer{})
	h.energy.clrErr = errors.New("clear the recorded energy mode: permission denied")
	h.hear(LidOpen)
	if h.c.energyKept != energyModeHighPower {
		t.Fatalf("held = %q after the failed clear, want High Power Mode still held", h.c.energyKept)
	}
	h.energy.clrErr = nil

	// Act
	h.c.tick(context.Background())

	// Assert
	if h.r.count(argPowerMode(energyModeHighPower)) != 1 || h.energy.kept() != "" || h.c.energyKept != "" {
		t.Fatalf("calls = %v, record %q, held %q: want one restore, then the record forgotten", h.r.ran(), h.energy.kept(), h.c.energyKept)
	}
	errs := h.records(dlog.LevelError, opEnergy)
	if len(errs) != 1 || errs[0].Context["cause"] != "clear the recorded energy mode: permission denied" {
		t.Fatalf("energy ERROR records = %+v, want the failed clear once", errs)
	}
}

func TestARecordThatLeftItsStoreIsKeptAgainBeforeTheLowPowerWrite(t *testing.T) {
	// Arrange: the quiet state stands holding High Power Mode, and a thaw
	// guard's restore raced it: the store is empty and High Power Mode runs.
	h := closedHarness(t, energyModeHighPower).withLid(lidClosed)
	h.c.quiet = true
	h.c.energyKept = energyModeHighPower
	h.r.on(argLowPower, answer{})

	// Act
	h.c.tick(context.Background())

	// Assert
	if h.energy.kept() != energyModeHighPower || h.r.count(argLowPower) != 1 ||
		h.energy.savedAfter[0] > slices.Index(h.r.ran(), argLowPower) {
		t.Fatalf("record %q, calls %v: want High Power Mode kept again, then Low Power written", h.energy.kept(), h.r.ran())
	}
}

func TestGuardRestoreEnergy(t *testing.T) {
	cases := []struct {
		name        string
		rec         *EnergyRecord
		loadErr     error
		pmset       answer
		write       answer
		clrErr      error
		wantWrite   bool
		wantErr     bool
		wantKept    string
		wantMessage string // the ERROR record, empty for none
	}{
		{name: "nothing recorded restores nothing"},
		{name: "the recorded mode is restored and cleared", rec: &EnergyRecord{Mode: "2"},
			pmset: answer{out: pmsetIn(true, "1")}, wantWrite: true},
		{name: "the recorded mode in use is cleared with nothing written", rec: &EnergyRecord{Mode: "2"},
			pmset: answer{out: pmsetIn(true, "2")}},
		{name: "a mode the user chose since stands", rec: &EnergyRecord{Mode: "2"},
			pmset: answer{out: pmsetIn(true, "0")}},
		{name: "an unreadable record writes nothing", loadErr: errors.New("corrupt"), wantErr: true,
			wantMessage: "the thaw guard could not read the recorded energy mode; nothing was restored"},
		{name: "an unreadable mode keeps the record", rec: &EnergyRecord{Mode: "2"},
			pmset: answer{err: errors.New("no such file")}, wantErr: true, wantKept: "2",
			wantMessage: "the thaw guard could not read the energy mode in use; the record is kept for the next daemon"},
		{name: "a machine naming no mode keeps the record", rec: &EnergyRecord{Mode: "2"},
			pmset: answer{out: pmsetIn(true, "")}, wantErr: true, wantKept: "2",
			wantMessage: "the thaw guard read no energy mode in use; the record is kept for the next daemon"},
		{name: "a refused restore keeps the record", rec: &EnergyRecord{Mode: "2"},
			pmset: answer{out: pmsetIn(true, "1")}, write: answer{out: "sudo: a password is required", code: 1},
			wantWrite: true, wantErr: true, wantKept: "2",
			wantMessage: "the thaw guard could not restore the recorded energy mode; the record is kept for the next daemon"},
		{name: "a record that cannot be cleared is recorded", rec: &EnergyRecord{Mode: "2"},
			pmset: answer{out: pmsetIn(true, "1")}, clrErr: errors.New("permission denied"),
			wantWrite: true, wantErr: true, wantKept: "2",
			wantMessage: "the thaw guard could not clear the recorded energy mode"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			r := newFakeRunner(t)
			r.on(argPmsetRead, tc.pmset)
			r.on(argPowerMode("2"), tc.write)
			store := &fakeEnergyStore{r: r, rec: tc.rec, loadErr: tc.loadErr, clrErr: tc.clrErr}
			log := dlog.NewTestLogger()

			// Act
			host, err := NewHost(Config{Platform: "darwin", Tools: tools}, r, nil, "")
			if err != nil {
				t.Fatal(err)
			}
			err = GuardRestoreEnergy(context.Background(), host, store, log)

			// Assert
			if (err != nil) != tc.wantErr {
				t.Fatalf("GuardRestoreEnergy() = %v, want error %v", err, tc.wantErr)
			}
			if wrote := r.count(argPowerMode("2")) == 1; wrote != tc.wantWrite {
				t.Fatalf("calls = %v, want the restore written %v", r.ran(), tc.wantWrite)
			}
			if store.kept() != tc.wantKept {
				t.Fatalf("record = %q, want %q", store.kept(), tc.wantKept)
			}
			errs := dlog.FilterRecords(log.Records(), dlog.RecordQuery{Level: dlog.LevelError, Operation: opEnergy})
			if tc.wantMessage == "" && len(errs) != 0 || tc.wantMessage != "" && (len(errs) != 1 || errs[0].Message != tc.wantMessage) {
				t.Fatalf("energy ERROR records = %+v, want %q", errs, tc.wantMessage)
			}
		})
	}
}
